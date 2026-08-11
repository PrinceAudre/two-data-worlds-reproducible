## worked-examples.R — reproducible statistical worked examples accompanying the article.
## Run from project root with renv active.
## RDHS microdata remain local-only under DHS Program access terms; this script writes no microdata.

suppressPackageStartupMessages({
  library(haven)
  library(survey)
  library(DBI)
  library(duckdb)
})

r2 <- function(x) round(x, 2)
pct <- function(x) 100 * as.numeric(x)
ci_vec <- function(x) {
  z <- confint(x)
  if (is.null(dim(z))) as.numeric(z) else as.numeric(z[1, ])
}

rdhs_file <- Sys.getenv("RDHS_IR_RDS", unset = "data/raw/rdhs_cache/datasets/RWIR81DT.rds")
dhis2_file <- Sys.getenv("DHIS2_FILE", unset = "data/raw/dhis2_indicator.csv")
if (!file.exists(rdhs_file)) stop("Authorised RDHS IR file not found; set RDHS_IR_RDS.", call. = FALSE)
if (!file.exists(dhis2_file)) stop("Routine-data CSV not found; set DHIS2_FILE.", call. = FALSE)

women <- readRDS(rdhs_file)
required <- c("v005", "v012", "v021", "v022", "v023", "v024", "v025", "v106", "v208", "v481", "m14_1")
missing_vars <- setdiff(required, names(women))
if (length(missing_vars)) stop(sprintf("RDHS file missing variable(s): %s", paste(missing_vars, collapse = ", ")), call. = FALSE)

women$wt <- as.numeric(women$v005) / 1e6

## Derive all survey-analysis variables before constructing the survey object.
v481_num <- suppressWarnings(as.numeric(women$v481))
women$insured <- ifelse(is.na(v481_num), NA_integer_, as.integer(v481_num == 1))

## FR370 Table 9.2 universe: women with >=1 live birth in preceding 5 years,
## most recent live birth. For the ANC-visit distribution, DK/missing remain in
## the denominator and therefore do not count as 4+ visits.
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

## Full-sample DHS design. No lonely-PSU adjustment is used to mask design problems;
## the validated full sample contains at least two PSUs per stratum.
des <- svydesign(ids = ~v021, strata = ~v022, weights = ~wt, data = women, nest = TRUE)

cat("\n===== P1a — SURVEY DESCRIPTIVE: health-insurance coverage =====\n")
ins_df <- women[!is.na(women$insured), , drop = FALSE]
srs_ins <- svydesign(ids = ~1, data = ins_df)
des_ins <- subset(des, !is.na(insured))

## Same logit confidence-interval family for naive/SRS and design-based estimates.
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

cat(sprintf("  NAIVE/SRS:          %.2f%%  95%% CI [%.2f, %.2f]  SE %.4f\n",
            pct(coef(srs_prop)), pct(srs_ci[1]), pct(srs_ci[2]), 100*srs_se))
cat(sprintf("  DESIGN-BASED:       %.2f%%  95%% CI [%.2f, %.2f]  SE %.4f\n",
            pct(coef(dsg_prop)), pct(dsg_ci[1]), pct(dsg_ci[2]), 100*dsg_se))
cat(sprintf("  SE ratio %.3f; variance ratio %.3f; estimator-specific effective n %.1f of %d\n",
            se_ratio, variance_ratio, effective_n, nrow(ins_df)))

cat("\n===== P1b — ANC4+ ~ education: naive / design / multilevel =====\n")
anc_design <- subset(des, eligible_anc)
anc_prop <- svymean(~anc4, anc_design, na.rm = TRUE)
cat(sprintf("  official-universe ANC4+ design estimate: %.3f%%; weighted N %.1f; unweighted eligible n %d\n",
            pct(coef(anc_prop)), sum(women$wt[women$eligible_anc], na.rm = TRUE), sum(women$eligible_anc)))

sub <- women[women$analysis_rows, , drop = FALSE]
m_naive <- glm(anc4 ~ educ_panel, data = sub, family = binomial())
cat("\n  NAIVE unweighted logistic — OR [95% Wald CI]:\n")
print(r2(exp(cbind(OR = coef(m_naive), confint.default(m_naive)))))

des_anc_model <- subset(des, analysis_rows)
m_svy <- svyglm(anc4 ~ educ_panel, design = des_anc_model, family = quasibinomial())
cat("\n  DESIGN-BASED svyglm — OR [95% CI]:\n")
print(r2(exp(cbind(OR = coef(m_svy), confint(m_svy)))))

if (requireNamespace("lme4", quietly = TRUE)) {
  m_ml <- lme4::glmer(
    anc4 ~ educ_panel + (1 | v021),
    data = sub,
    family = binomial(),
    control = lme4::glmerControl(optimizer = "bobyqa")
  )
  cat("\n  MULTILEVEL random-intercept glmer (conditional) — OR [95% Wald CI]:\n")
  print(r2(exp(cbind(
    OR = lme4::fixef(m_ml),
    confint(m_ml, method = "Wald", parm = "beta_")
  ))))
  cat(sprintf("  cluster (v021) random-intercept SD = %.3f\n",
              attr(lme4::VarCorr(m_ml)$v021, "stddev")))
  cat("  NOTE: this multilevel model is unweighted and does not encode sampling strata;\n")
  cat("        it is a model-based conditional analysis, not the design-based estimator above.\n")
} else {
  cat("\n  [lme4 not installed -> multilevel example cannot be validated; install and lock lme4 before release.]\n")
}

cat("\n===== P2 — ROUTINE DATA QUALITY =====\n")
con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
qpath <- gsub("'", "''", normalizePath(dhis2_file, winslash = "/", mustWork = TRUE), fixed = TRUE)
DHIS2 <- sprintf("read_csv_auto('%s')", qpath)
print(dbGetQuery(con, sprintf("SELECT COUNT(*) AS district_months, ROUND(MIN(value),2) AS min_raw,
  ROUND(MAX(value),2) AS max_raw, ROUND(median(value),1) AS median_raw,
  SUM(CASE WHEN value>100 THEN 1 ELSE 0 END) AS flag_gt100,
  SUM(CASE WHEN value<5 THEN 1 ELSE 0 END) AS flag_lt5 FROM %s", DHIS2)))
cat("  flagged records for diagnostic review (raw values retained):\n")
print(dbGetQuery(con, sprintf("SELECT district, period, value,
  CASE WHEN value>100 THEN 'above 100%% plausibility flag'
       WHEN value<5 THEN 'low-value diagnostic flag' END AS flag_reason
  FROM %s WHERE value>100 OR value<5 ORDER BY value DESC", DHIS2)))
cat("  NOTE: P2 flags are not automatic exclusion rules. Investigate reporting completeness/timeliness,\n")
cat("        duplicate/service-count issues, numerator definition, denominator source/reference year,\n")
cat("        geography and catchment alignment before any correction.\n")
cat("  lowest observed values (diagnostic only; cause not inferred):\n")
print(dbGetQuery(con, sprintf("SELECT district, period, value FROM %s ORDER BY value LIMIT 4", DHIS2)))

cat("\n===== P5 — distribution shape (routine value; DHS age) =====\n")
print(dbGetQuery(con, sprintf("SELECT ROUND(AVG(value),1) AS mean, ROUND(median(value),1) AS median,
  ROUND(MIN(value),2) AS min, ROUND(MAX(value),2) AS max FROM %s", DHIS2)))
cat(sprintf("  RDHS age v012: mean %.1f vs median %.1f\n",
            mean(women$v012), median(women$v012)))


cat("\n===== P6 — effect size vs significance (simulation, seed 2026) =====\n")
set.seed(2026)
cat(sprintf("  rare 0.1%%->0.4%%: RR = %.0f (+%.0f%% relative) but RD = %.1f pp absolute\n",
            0.004/0.001, 100*(0.004/0.001 - 1), 100*(0.004 - 0.001)))
p0 <- 0.60; p1 <- 0.85
cat(sprintf("  common 60%%->85%%: RR = %.2f; OR = %.2f (OR lies farther from 1 than RR)\n",
            p1/p0, (p1/(1-p1))/(p0/(1-p0))))
pow <- function(nn, d, B = 3000) {
  mean(replicate(B, {
    x <- rep(0:1, each = nn/2)
    y <- rnorm(nn, d*x)
    coef(summary(lm(y ~ x)))[2, 4] < 0.05
  }))
}
cat(sprintf("  trivial effect d=0.1 -> power %.0f%% at n=40 vs %.0f%% at n=4000 (same effect)\n",
            100*pow(40, 0.1), 100*pow(4000, 0.1)))

cat("\n===== P8 — multiplicity (simulation, seed 8) =====\n")
set.seed(8)
screen <- function() {
  n <- 200; y <- rnorm(n); X <- matrix(rnorm(n*20), n, 20)
  pv <- apply(X, 2, function(x) coef(summary(lm(y ~ x)))[2, 4])
  c(hits = sum(pv < .05),
    bonf = as.integer(any(p.adjust(pv, "bonferroni") < .05)),
    bh = as.integer(any(p.adjust(pv, "BH") < .05)))
}
S <- replicate(2000, screen())
cat(sprintf("  20 NULL predictors, 2000 screens: mean %.2f raw hits (max %d); %.0f%% of screens had >=1\n",
            mean(S["hits", ]), max(S["hits", ]), 100*mean(S["hits", ] > 0)))
cat(sprintf("  after multiplicity correction, %% retaining >=1 hit: Bonferroni %.1f%%, BH %.1f%%\n",
            100*mean(S["bonf", ]), 100*mean(S["bh", ])))
cat(sprintf("  theory P(>=1 false positive): 20 independent null tests %.0f%%, 50 tests %.0f%%\n",
            100*(1 - .95^20), 100*(1 - .95^50)))

cat("\n===== P10 — confounding & collider (simulations; true X->Y effect = 0) =====\n")
set.seed(10)
n <- 2000; C <- rnorm(n); X <- rbinom(n, 1, plogis(C)); Y <- rnorm(n, 0.8*C + 0*X)
cr <- coef(summary(lm(Y ~ X)))[2, c(1, 4)]
aj <- coef(summary(lm(Y ~ X + C)))[2, c(1, 4)]
cat(sprintf("  confounder: crude X coef = %.2f (p = %.1e) -> adjusted for C = %.2f (p = %.2f)\n",
            cr[1], cr[2], aj[1], aj[2]))
set.seed(101)
Xc <- rbinom(n, 1, .5); Yc <- rnorm(n, 0*Xc); K <- rnorm(n, Xc + Yc)
cr2 <- coef(summary(lm(Yc ~ Xc)))[2, c(1, 4)]
co <- coef(summary(lm(Yc ~ Xc + K)))[2, c(1, 4)]
cat(sprintf("  collider: crude Xc coef = %.2f (p = %.2f) -> adjusted for K = %.2f (p = %.1e) [spurious]\n",
            cr2[1], cr2[2], co[1], co[2]))

cat("\n===== S3 — DHS design-variable check =====\n")
cat(sprintf("  v022==v023 for all non-missing records: %s\n",
            all(as.numeric(women$v022) == as.numeric(women$v023), na.rm = TRUE)))
cat(sprintf("  distinct v022 strata: %d; distinct region(v024) x residence(v025): %d\n",
            length(unique(women$v022[!is.na(women$v022)])),
            length(unique(paste(women$v024, women$v025)))))
cat("\n[done]\n")
