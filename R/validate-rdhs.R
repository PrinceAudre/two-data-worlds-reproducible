#!/usr/bin/env Rscript

## RDHS validation gate for the reproducibility workflow and Figure 1.
## REQUIRES authorised local Rwanda DHS 2019-20 IR microdata.
## No microdata are written by this script; only aggregate validation outputs are exported.
##
## Run from project root after renv::restore():
##   Rscript R/validate-rdhs.R
## Optional overrides:
##   RDHS_IR_RDS=/path/to/RWIR81DT.rds DERIVED_OUT=data/derived \
##     Rscript R/validate-rdhs.R
##
## Official known-answer anchors:
## - FR370 Table 3.9.1: women with any health insurance = 82.6%; weighted N = 14,634.
## - FR370 Table 9.2: 4+ ANC visits for most recent live birth in preceding 5 years = 47.2%; weighted N = 6,302.
## - FR370 Appendix A: 500 EAs (PSUs); 60 district-by-residence sampling strata.
## - DHS-7 recode: m14 = number of ANC visits; 0 = none; 98 = don't know; 99 = missing.
##   DHS tabulation includes don't know/missing in the denominator for the visit distribution.

suppressPackageStartupMessages({
  library(haven)
  library(survey)
})

fmt_msg <- function(fmt, ...) {
  args <- list(...)
  if (length(args)) sprintf(fmt, ...) else fmt
}
fail <- function(fmt, ...) stop(fmt_msg(fmt, ...), call. = FALSE)
pass <- function(fmt, ...) cat(sprintf("[PASS] %s\n", fmt_msg(fmt, ...)))
info <- function(fmt, ...) cat(sprintf("[INFO] %s\n", fmt_msg(fmt, ...)))
pct <- function(x) 100 * as.numeric(x)
ci_vec <- function(x) {
  z <- confint(x)
  if (is.null(dim(z))) as.numeric(z) else as.numeric(z[1, ])
}

rds <- Sys.getenv("RDHS_IR_RDS", unset = "data/raw/rdhs_cache/datasets/RWIR81DT.rds")
out_dir <- Sys.getenv("DERIVED_OUT", unset = "data/derived")
if (!file.exists(rds)) {
  fail("Authorised RDHS IR file not found: %s. Set RDHS_IR_RDS to the local RWIR81DT.rds path.", rds)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

women <- readRDS(rds)
required <- c("v005", "v021", "v022", "v023", "v106", "v208", "v481", "m14_1")
missing_vars <- setdiff(required, names(women))
if (length(missing_vars)) fail("RDHS IR file is missing required variable(s): %s", paste(missing_vars, collapse = ", "))
pass("Required RDHS variables are present")

women$wt <- as.numeric(women$v005) / 1e6
if (any(!is.finite(women$wt) | women$wt <= 0, na.rm = TRUE)) fail("Invalid v005-derived sampling weights detected")

## ---- B4a: survey-design structure ---------------------------------------------------------
n_psu <- length(unique(women$v021[!is.na(women$v021)]))
n_strata <- length(unique(women$v022[!is.na(women$v022)]))
psu_by_stratum <- tapply(women$v021, women$v022, function(x) length(unique(x[!is.na(x)])))
strata_min_psu <- min(psu_by_stratum, na.rm = TRUE)
v022_v023_equal <- all(as.numeric(women$v022) == as.numeric(women$v023), na.rm = TRUE)

info("Observed PSUs (v021): %d", n_psu)
info("Observed strata (v022): %d", n_strata)
info("Minimum PSUs per v022 stratum: %d", strata_min_psu)
info("v022 == v023 for all non-missing records: %s", v022_v023_equal)
if (!v022_v023_equal) fail("Expected v022 and v023 to agree for all non-missing records in this validated release input")

if (n_psu != 500) fail("Expected 500 sampled EAs from FR370 Appendix A; observed %d", n_psu)
if (n_strata != 60) fail("Expected 60 sampling strata from FR370 Appendix A; observed %d", n_strata)
if (strata_min_psu < 2) fail("At least one full-sample stratum has fewer than 2 observed PSUs")
pass("RDHS design structure matches the official 500-EA / 60-stratum design")

## ---- Derive every analysis variable BEFORE constructing the survey object -----------------
## This guarantees that subsetting/model formulae operate on variables stored inside the design.
v481_num <- suppressWarnings(as.numeric(women$v481))
women$insured <- ifelse(is.na(v481_num), NA_integer_, as.integer(v481_num == 1))

## FR370 Table 9.2 population: women 15-49 with a live birth in the preceding 5 years,
## most recent live birth. v208 > 0 identifies women with >=1 birth in the last 5 years.
## For the ANC-visit distribution, don't know/missing are retained in the denominator.
m14 <- suppressWarnings(as.numeric(women$m14_1))
women$eligible_anc <- !is.na(women$v208) & as.numeric(women$v208) > 0
women$anc4 <- ifelse(
  women$eligible_anc,
  as.integer(!is.na(m14) & m14 >= 4 & m14 <= 90),
  NA_integer_
)

edu_raw <- tolower(trimws(as.character(haven::as_factor(women$v106))))
edu_raw[grepl("more than secondary|higher", edu_raw)] <- "higher"
women$educ_panel <- factor(edu_raw, levels = c("no education", "primary", "secondary", "higher"))
women$analysis_rows <- women$eligible_anc & !is.na(women$educ_panel) & !is.na(women$anc4)

## Use the documented recent-DHS design variables: v021 PSU, v022 stratum, v005 weight.
## The full sample has >1 PSU per stratum, so this validation does not mask design problems
## with a lonely-PSU adjustment.
des <- svydesign(ids = ~v021, strata = ~v022, weights = ~wt, data = women, nest = TRUE)

## ---- B3: Panel A with symmetric confidence-interval construction --------------------------
ins_df <- women[!is.na(women$insured), , drop = FALSE]
if (!nrow(ins_df)) fail("No non-missing v481 observations")

srs_ins <- svydesign(ids = ~1, data = ins_df)
des_ins <- subset(des, !is.na(insured))

## Same interval family in both analyses: survey::svyciprop(method = 'logit').
srs_prop <- svyciprop(~insured, srs_ins, method = "logit")
dsg_prop <- svyciprop(~insured, des_ins, method = "logit")
srs_mean <- svymean(~insured, srs_ins)
dsg_mean <- svymean(~insured, des_ins)

srs_ci <- ci_vec(srs_prop)
dsg_ci <- ci_vec(dsg_prop)
srs_se <- as.numeric(SE(srs_mean))
dsg_se <- as.numeric(SE(dsg_mean))
se_ratio <- dsg_se / srs_se
variance_ratio <- se_ratio^2
effective_n <- nrow(ins_df) / variance_ratio

insurance_weighted_n <- sum(ins_df$wt)
insurance_official_pct <- 82.6
insurance_official_n <- 14634
insurance_observed_pct <- pct(coef(dsg_prop))

info("Insurance naive/SRS: %.2f%% [%.2f, %.2f], SE %.4f", pct(coef(srs_prop)), pct(srs_ci[1]), pct(srs_ci[2]), 100*srs_se)
info("Insurance design:    %.2f%% [%.2f, %.2f], SE %.4f", insurance_observed_pct, pct(dsg_ci[1]), pct(dsg_ci[2]), 100*dsg_se)
info("Panel A SE ratio %.3f; variance ratio %.3f; estimator-specific effective n %.1f", se_ratio, variance_ratio, effective_n)

if (abs(insurance_observed_pct - insurance_official_pct) > 0.15) {
  fail("Insurance benchmark failed: observed %.3f%% vs official %.1f%%", insurance_observed_pct, insurance_official_pct)
}
if (abs(round(insurance_weighted_n) - insurance_official_n) > 1) {
  fail("Insurance weighted-N benchmark failed: observed %.1f vs official %d", insurance_weighted_n, insurance_official_n)
}
pass("Health-insurance estimate reproduces FR370 Table 3.9.1 within rounding tolerance")

## ---- B4b: ANC4+ definition and official 47.2% known-answer test ---------------------------
anc_design <- subset(des, eligible_anc)
anc_prop <- svymean(~anc4, anc_design, na.rm = TRUE)
anc_pct <- pct(coef(anc_prop))
anc_weighted_n <- sum(women$wt[women$eligible_anc], na.rm = TRUE)
anc_unweighted_n <- sum(women$eligible_anc)
anc_official_pct <- 47.2
anc_official_n <- 6302

info("ANC4+ design estimate: %.3f%%; weighted N %.1f; unweighted eligible n %d", anc_pct, anc_weighted_n, anc_unweighted_n)
if (abs(anc_pct - anc_official_pct) > 0.15) {
  fail("ANC4+ benchmark failed: observed %.3f%% vs official %.1f%%", anc_pct, anc_official_pct)
}
if (abs(round(anc_weighted_n) - anc_official_n) > 1) {
  fail("ANC4+ weighted-N benchmark failed: observed %.1f vs official %d", anc_weighted_n, anc_official_n)
}
pass("ANC4+ reproduces FR370 Table 9.2 (47.2%) within rounding tolerance")

## ---- Panel B regeneration: education contrast, naive vs design-based -----------------------
sub <- women[women$analysis_rows, , drop = FALSE]
if (!nrow(sub)) fail("No records available for ANC4+ education model")

m_naive <- glm(anc4 ~ educ_panel, data = sub, family = binomial())
des_anc_model <- subset(des, analysis_rows)
m_svy <- svyglm(anc4 ~ educ_panel, design = des_anc_model, family = quasibinomial())

extract_or <- function(model, method_label, conf_fun) {
  b <- coef(model)
  ci <- conf_fun(model)
  keep <- names(b) != "(Intercept)"
  if (!all(names(b) %in% rownames(ci))) fail("Confidence-interval rows do not align with model coefficients")
  ci <- ci[names(b), , drop = FALSE]
  data.frame(
    method = method_label,
    term = names(b)[keep],
    OR = exp(b[keep]),
    lower = exp(ci[keep, 1]),
    upper = exp(ci[keep, 2]),
    row.names = NULL,
    check.names = FALSE
  )
}

or_naive <- extract_or(m_naive, "Naive (unweighted)", function(m) confint.default(m))
or_design <- extract_or(m_svy, "Design-based", function(m) confint(m))
panel_b <- rbind(or_naive, or_design)

## ---- Machine-readable aggregate outputs ----------------------------------------------------
panel_a <- data.frame(
  panel = "A",
  method = c("Naive SRS", "Design-based"),
  estimate = c(pct(coef(srs_prop)), pct(coef(dsg_prop))),
  lower = c(pct(srs_ci[1]), pct(dsg_ci[1])),
  upper = c(pct(srs_ci[2]), pct(dsg_ci[2])),
  se = c(100*srs_se, 100*dsg_se),
  stringsAsFactors = FALSE
)

panel_b$panel <- "B"
panel_b <- panel_b[, c("panel", "method", "term", "OR", "lower", "upper")]

summary_tbl <- data.frame(
  check = c(
    "PSUs (v021)",
    "Strata (v022)",
    "Insurance %",
    "Insurance weighted N",
    "ANC4+ %",
    "ANC4+ weighted N",
    "Panel A SE ratio",
    "Panel A variance ratio",
    "Panel A effective n"
  ),
  observed = c(
    n_psu,
    n_strata,
    insurance_observed_pct,
    insurance_weighted_n,
    anc_pct,
    anc_weighted_n,
    se_ratio,
    variance_ratio,
    effective_n
  ),
  official_or_target = c(500, 60, 82.6, 14634, 47.2, 6302, NA, NA, NA),
  stringsAsFactors = FALSE
)

write.csv(panel_a, file.path(out_dir, "figure1-panel-a-generated.csv"), row.names = FALSE)
write.csv(panel_b, file.path(out_dir, "figure1-panel-b-generated.csv"), row.names = FALSE)
write.csv(summary_tbl, file.path(out_dir, "section6-rdhs-validation-summary.csv"), row.names = FALSE)

cat("\n--- FIGURE 1 PANEL A (regenerated; same logit-CI family) ---\n")
print(transform(panel_a,
                estimate = round(estimate, 3), lower = round(lower, 3),
                upper = round(upper, 3), se = round(se, 4)))
cat("\n--- FIGURE 1 PANEL B (regenerated from official ANC4+ universe/coding) ---\n")
print(transform(panel_b,
                OR = round(OR, 3), lower = round(lower, 3), upper = round(upper, 3)))
cat("\n--- SECTION 6 SUMMARY ---\n")
print(summary_tbl)

cat(sprintf("\nAggregate outputs written to: %s\n", normalizePath(out_dir, winslash = "/", mustWork = TRUE)))
cat("SECTION 6 RDHS VALIDATION: PASS\n")
cat("Do not regenerate Figure 1 outputs until this validation passes on the authorised local file.\n")
