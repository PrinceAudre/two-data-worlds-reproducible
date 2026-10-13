#!/usr/bin/env Rscript

# Paper 2 RDHS empirical illustration of inferential consequences
# Version 1.0
# FROZEN BEFORE RDHS 2025 MICRODATA ACCESS: 2026-10-13
#
# Protocol specification:
# paper2/validation/RDHS_VALIDATION_SPEC_v1.0_FROZEN_2026-10-13.md
#
# Restricted microdata are read only from authorised local paths and are never
# written by this script. Only aggregate outputs are exported.
#
# Required packages: haven, survey
#
# Environment variables:
#   RDHS2019_IR=/authorised/path/to/RWIR81DT.dta
#   RDHS2025_IR=/authorised/path/to/RWIR91FL.dta
#   RDHS_ILLUSTRATION_OUT=paper2/rwanda_illustration/generated
#
# The script deliberately fails closed if required variables/design anchors do
# not match the prespecified definitions.

required_pkgs <- c("haven", "survey")
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_pkgs)) {
  stop(
    "Missing R package(s): ", paste(missing_pkgs, collapse = ", "),
    ". Install them before running the frozen validation.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(survey)
})

options(stringsAsFactors = FALSE)

fail <- function(...) stop(sprintf(...), call. = FALSE)
pass <- function(...) cat(sprintf("[PASS] %s\n", sprintf(...)))
info <- function(...) cat(sprintf("[INFO] %s\n", sprintf(...)))

read_authorised <- function(path) {
  if (!nzchar(path)) fail("An authorised microdata path was not supplied.")
  if (!file.exists(path)) fail("Authorised microdata file not found: %s", path)

  ext <- tolower(tools::file_ext(path))
  x <- switch(
    ext,
    rds = readRDS(path),
    dta = haven::read_dta(path),
    sav = haven::read_sav(path),
    zsav = haven::read_sav(path),
    fail("Unsupported microdata extension '.%s'. Use .rds, .dta, .sav or .zsav.", ext)
  )

  x <- as.data.frame(x)
  names(x) <- tolower(names(x))
  x
}

num <- function(x) suppressWarnings(as.numeric(x))

prepare_ir <- function(x, wave) {
  req <- c("v005", "v021", "v022", "v106", "v481")
  miss <- setdiff(req, names(x))
  if (length(miss)) {
    fail("%s IR file is missing required variable(s): %s",
         wave, paste(miss, collapse = ", "))
  }

  v481 <- num(x$v481)
  unexpected_insurance <- sort(unique(v481[!is.na(v481) & !(v481 %in% c(0, 1))]))
  if (length(unexpected_insurance)) {
    fail("%s v481 contains unexpected non-binary code(s): %s",
         wave, paste(unexpected_insurance, collapse = ", "))
  }

  v106 <- num(x$v106)
  unexpected_educ <- sort(unique(v106[!is.na(v106) & !(v106 %in% 0:3)]))
  if (length(unexpected_educ)) {
    fail("%s v106 contains unexpected code(s): %s",
         wave, paste(unexpected_educ, collapse = ", "))
  }

  x$wt <- num(x$v005) / 1e6
  x$psu <- num(x$v021)
  x$stratum <- num(x$v022)
  x$insured <- ifelse(is.na(v481), NA_integer_, as.integer(v481 == 1))
  x$educ <- factor(
    v106,
    levels = 0:3,
    labels = c("No education", "Primary", "Secondary", "Higher")
  )

  bad_weight <- !is.na(x$wt) & (!is.finite(x$wt) | x$wt <= 0)
  if (any(bad_weight)) fail("%s contains invalid non-positive/non-finite v005-derived weights.", wave)

  keep <- !is.na(x$insured) &
    !is.na(x$wt) & x$wt > 0 &
    !is.na(x$psu) &
    !is.na(x$stratum)

  d <- x[keep, , drop = FALSE]
  if (!nrow(d)) fail("%s has no records in the prespecified analytic universe.", wave)

  psu_by_stratum <- tapply(
    d$psu, d$stratum,
    function(z) length(unique(z[!is.na(z)]))
  )
  if (min(psu_by_stratum, na.rm = TRUE) < 2) {
    fail("%s has a full-sample stratum with fewer than 2 observed PSUs. Frozen specification requires review before analysis.", wave)
  }

  list(
    data = d,
    source_n = nrow(x),
    analysis_n = nrow(d),
    weighted_n = sum(d$wt),
    n_psu = length(unique(d$psu)),
    n_strata = length(unique(d$stratum)),
    min_psu_per_stratum = min(psu_by_stratum, na.rm = TRUE)
  )
}

make_designs <- function(d) {
  list(
    M1_unweighted_SRS = svydesign(ids = ~1, data = d),
    M2_weights_only = svydesign(ids = ~1, weights = ~wt, data = d),
    reference_full_design = svydesign(
      ids = ~psu,
      strata = ~stratum,
      weights = ~wt,
      data = d,
      nest = TRUE
    )
  )
}

ci_vec <- function(x) {
  z <- confint(x)
  if (is.null(dim(z))) as.numeric(z) else as.numeric(z[1, ])
}

prop_result <- function(design, method_name) {
  p <- svyciprop(~insured, design, method = "logit", na.rm = TRUE)
  m <- svymean(~insured, design, na.rm = TRUE)
  ci <- ci_vec(p)

  data.frame(
    method = method_name,
    estimate = as.numeric(coef(p)),
    se = as.numeric(SE(m)),
    ci_low = ci[1],
    ci_high = ci[2],
    ci_width = ci[2] - ci[1],
    stringsAsFactors = FALSE
  )
}

prevalence_analysis <- function(prep, wave) {
  d <- prep$data
  designs <- make_designs(d)

  out <- do.call(
    rbind,
    lapply(names(designs), function(nm) prop_result(designs[[nm]], nm))
  )
  out$wave <- wave
  out$raw_n <- nrow(d)
  out$weighted_n <- prep$weighted_n

  ref <- out[out$method == "reference_full_design", , drop = FALSE]
  out$point_diff_pp_vs_reference <- 100 * (out$estimate - ref$estimate)
  out$se_ratio_vs_reference <- out$se / ref$se
  out$variance_ratio_vs_reference <- (out$se / ref$se)^2
  out$ci_width_ratio_vs_reference <- out$ci_width / ref$ci_width

  out[, c(
    "wave", "method", "raw_n", "weighted_n",
    "estimate", "se", "ci_low", "ci_high", "ci_width",
    "point_diff_pp_vs_reference", "se_ratio_vs_reference",
    "variance_ratio_vs_reference", "ci_width_ratio_vs_reference"
  )]
}

extract_glm <- function(model, method_name) {
  b <- coef(model)
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  ci <- confint.default(model)
  keep <- names(b) != "(Intercept)"

  data.frame(
    method = method_name,
    term = names(b)[keep],
    log_or = unname(b[keep]),
    se_log_or = unname(se[keep]),
    ci_log_low = unname(ci[keep, 1]),
    ci_log_high = unname(ci[keep, 2]),
    OR = exp(unname(b[keep])),
    OR_low = exp(unname(ci[keep, 1])),
    OR_high = exp(unname(ci[keep, 2])),
    stringsAsFactors = FALSE
  )
}

extract_svyglm <- function(model, method_name) {
  b <- coef(model)
  se <- sqrt(diag(vcov(model)))
  ci <- confint(model)
  keep <- names(b) != "(Intercept)"
  ci <- ci[names(b), , drop = FALSE]

  data.frame(
    method = method_name,
    term = names(b)[keep],
    log_or = unname(b[keep]),
    se_log_or = unname(se[keep]),
    ci_log_low = unname(ci[keep, 1]),
    ci_log_high = unname(ci[keep, 2]),
    OR = exp(unname(b[keep])),
    OR_low = exp(unname(ci[keep, 1])),
    OR_high = exp(unname(ci[keep, 2])),
    stringsAsFactors = FALSE
  )
}

regression_analysis <- function(prep, wave) {
  d <- prep$data
  reg <- d[!is.na(d$educ), , drop = FALSE]
  if (!nrow(reg)) fail("%s has no regression-complete records.", wave)

  m1 <- glm(insured ~ educ, data = reg, family = binomial())

  d_w <- svydesign(ids = ~1, weights = ~wt, data = reg)
  m2 <- svyglm(insured ~ educ, design = d_w, family = quasibinomial())

  d_full <- svydesign(
    ids = ~psu,
    strata = ~stratum,
    weights = ~wt,
    data = reg,
    nest = TRUE
  )
  ref <- svyglm(insured ~ educ, design = d_full, family = quasibinomial())

  out <- rbind(
    extract_glm(m1, "M1_unweighted_logistic"),
    extract_svyglm(m2, "M2_weights_only_logistic"),
    extract_svyglm(ref, "reference_full_design_logistic")
  )
  out$wave <- wave
  out$raw_n <- nrow(reg)
  out$ci_log_width <- out$ci_log_high - out$ci_log_low

  refs <- out[out$method == "reference_full_design_logistic", , drop = FALSE]

  metrics <- do.call(rbind, lapply(unique(out$term), function(trm) {
    rr <- refs[refs$term == trm, , drop = FALSE]
    z <- out[out$term == trm, , drop = FALSE]

    data.frame(
      wave = wave,
      method = z$method,
      term = trm,
      raw_n = nrow(reg),
      log_or = z$log_or,
      OR = z$OR,
      OR_low = z$OR_low,
      OR_high = z$OR_high,
      se_log_or = z$se_log_or,
      log_or_diff_vs_reference = z$log_or - rr$log_or,
      se_ratio_vs_reference = z$se_log_or / rr$se_log_or,
      ci_width_ratio_vs_reference = z$ci_log_width / rr$ci_log_width,
      sign_change_vs_reference = sign(z$log_or) != sign(rr$log_or),
      ci_includes_1 = z$OR_low <= 1 & z$OR_high >= 1,
      reference_ci_includes_1 = rr$OR_low <= 1 & rr$OR_high >= 1,
      ci_crossing_status_differs =
        (z$OR_low <= 1 & z$OR_high >= 1) !=
        (rr$OR_low <= 1 & rr$OR_high >= 1),
      stringsAsFactors = FALSE
    )
  }))

  metrics
}

check_2019 <- function(prep, prev) {
  if (prep$n_psu != 500) {
    fail("RDHS 2019-20 gate failed: expected 500 PSUs; observed %d.", prep$n_psu)
  }
  if (prep$n_strata != 60) {
    fail("RDHS 2019-20 gate failed: expected 60 strata; observed %d.", prep$n_strata)
  }

  ref <- prev[prev$method == "reference_full_design", , drop = FALSE]
  obs_pct <- 100 * ref$estimate

  if (abs(obs_pct - 82.6) > 0.15) {
    fail("RDHS 2019-20 insurance benchmark failed: observed %.3f%% vs 82.6%%.", obs_pct)
  }
  if (abs(round(ref$weighted_n) - 14634) > 1) {
    fail("RDHS 2019-20 insurance weighted-N gate failed: observed %.1f vs 14634.", ref$weighted_n)
  }

  pass("RDHS 2019-20 design and insurance known-answer gates")
}

check_2025 <- function(prep) {
  if (prep$source_n != 14283) {
    fail("RDHS 2025 structural gate failed: expected 14,283 IR rows; observed %d.", prep$source_n)
  }
  if (prep$n_psu != 560) {
    fail("RDHS 2025 structural gate failed: expected 560 PSUs; observed %d.", prep$n_psu)
  }
  pass("RDHS 2025 public-metadata structural gates")
}

path19 <- Sys.getenv("RDHS2019_IR", unset = "")
path25 <- Sys.getenv("RDHS2025_IR", unset = "")
out_dir <- Sys.getenv(
  "RDHS_ILLUSTRATION_OUT",
  unset = "paper2/rwanda_illustration/generated"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

info("Loading authorised RDHS 2019-20 IR file")
x19 <- read_authorised(path19)
p19 <- prepare_ir(x19, "RDHS 2019-20")
v1_19 <- prevalence_analysis(p19, "RDHS 2019-20")
check_2019(p19, v1_19)
v2_19 <- regression_analysis(p19, "RDHS 2019-20")

info("Loading authorised RDHS 2025 IR file")
x25 <- read_authorised(path25)
p25 <- prepare_ir(x25, "RDHS 2025")
check_2025(p25)
v1_25 <- prevalence_analysis(p25, "RDHS 2025")
v2_25 <- regression_analysis(p25, "RDHS 2025")

v1 <- rbind(v1_19, v1_25)
v2 <- rbind(v2_19, v2_25)

checks <- data.frame(
  wave = c("RDHS 2019-20", "RDHS 2025"),
  source_n = c(p19$source_n, p25$source_n),
  analysis_n = c(p19$analysis_n, p25$analysis_n),
  weighted_n = c(p19$weighted_n, p25$weighted_n),
  n_psu = c(p19$n_psu, p25$n_psu),
  n_strata = c(p19$n_strata, p25$n_strata),
  min_psu_per_stratum = c(p19$min_psu_per_stratum, p25$min_psu_per_stratum),
  stringsAsFactors = FALSE
)

write.csv(v1, file.path(out_dir, "RDHS_illustration_V1_prevalence.csv"), row.names = FALSE)
write.csv(v2, file.path(out_dir, "RDHS_illustration_V2_regression.csv"), row.names = FALSE)
write.csv(checks, file.path(out_dir, "RDHS_illustration_design_checks.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(out_dir, "RDHS_illustration_sessionInfo.txt"))

cat("\n--- V1 NATIONAL HEALTH-INSURANCE PREVALENCE ---\n")
print(transform(
  v1,
  estimate = round(100 * estimate, 3),
  se = round(100 * se, 4),
  ci_low = round(100 * ci_low, 3),
  ci_high = round(100 * ci_high, 3)
))

cat("\n--- V2 EDUCATION AND HEALTH-INSURANCE ASSOCIATION ---\n")
print(transform(
  v2,
  OR = round(OR, 3),
  OR_low = round(OR_low, 3),
  OR_high = round(OR_high, 3),
  se_log_or = round(se_log_or, 4)
))

cat("\n--- DESIGN CHECKS ---\n")
print(checks)

cat("\nPAPER 2 RDHS EMPIRICAL ILLUSTRATION: PASS\n")
cat(sprintf("Aggregate outputs written to: %s\n",
            normalizePath(out_dir, winslash = "/", mustWork = TRUE)))
