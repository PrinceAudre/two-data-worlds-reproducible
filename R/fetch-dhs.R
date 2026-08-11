# Fetch DHS data via the rdhs package (the R <-> DHS bridge).
# Two capabilities:
#   A) DHS API — aggregated INDICATORS (STATcompiler). Works NOW, no login.
#   B) MICRODATA (recode files). Needs an APPROVED DHS account + project.
#
# COMPLIANCE: never commit DHS microdata or your rdhs config. `data/` and the rdhs
# config are git-ignored; DHS terms of use forbid redistribution.

library(rdhs)

# ── A) INDICATORS API (no login required) ────────────────────────────────────
# Which Rwanda DHS surveys exist?
rw_surveys <- dhs_surveys(countryIds = "RW", surveyType = "DHS")

# Find an indicator id (search the catalogue by keyword):
#   inds <- dhs_indicators()
#   inds[grepl("Antenatal", inds$Label, ignore.case = TRUE), c("IndicatorId", "Label")]
# Then pull its values for Rwanda (example id — replace with one from the line above):
#   anc_rw <- dhs_data(countryIds = "RW",
#                      indicatorIds = "RH_ANCN_W_N4P",   # TODO: confirm id
#                      surveyYearStart = 2010)

# ── B) MICRODATA download — Rwanda 2019-20 IR (authorised users only) ──
# Run ONCE in the RStudio console (interactive password prompt; rdhs caches an auth token,
# never the password, never in this repo):
#   set_rdhs_config(
#     email       = "YOUR_DHS_EMAIL",   # your registered email -> git-ignored rdhs.json
#     project     = "YOUR_APPROVED_DHS_PROJECT_NAME",
#     config_path = "rdhs.json",
#     cache_path  = "data/raw/rdhs_cache",
#     global      = FALSE               # keep config project-local (needed for a relative config_path)
#   )
# Rwanda 2019-20 DHS women's (IR) recode, Stata — exact file confirmed via the DHS API:
#   paths <- get_datasets("RWIR81DT.ZIP")   # 14,634 women x 5,117 variables
#   women <- readRDS(paths[[1]])
#
# DHS recode reminders:
#   * weight is  v005 / 1e6  (stored x1,000,000 — using it raw is wrong)
#   * cluster/PSU = v021 ; strata = v022 (or v023)
#   * coded categoricals (v024 region, v106 education) carry value labels -> haven::as_factor()
