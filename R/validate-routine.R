#!/usr/bin/env Rscript

## Routine-data validation gate; does not require DHS/RDHS microdata.
## Run from the repository root after renv::restore():
##   Rscript R/validate-routine.R
## Optional override:
##   DHIS2_FILE=/path/to/dhis2_indicator.csv Rscript R/validate-routine.R

fail <- function(...) stop(sprintf(...), call. = FALSE)
pass <- function(...) cat(sprintf("[PASS] %s\n", sprintf(...)))
info <- function(...) cat(sprintf("[INFO] %s\n", sprintf(...)))

required_pkgs <- c("DBI", "duckdb", "digest")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  fail("Missing package(s): %s. Run renv::restore() first.", paste(missing_pkgs, collapse = ", "))
}
pass("Required R packages are available: %s", paste(required_pkgs, collapse = ", "))

lockfile <- "renv.lock"
if (!file.exists(lockfile)) fail("renv.lock not found at repository root")
pass("renv.lock is present")

dhis2_file <- Sys.getenv("DHIS2_FILE", unset = "data/raw/dhis2_indicator.csv")
if (!file.exists(dhis2_file)) {
  fail("Routine-data file not found: %s. Set DHIS2_FILE to the local CSV path.", dhis2_file)
}
resolved_dhis2 <- normalizePath(dhis2_file, winslash = "/", mustWork = TRUE)
pass("Routine-data file found: %s", resolved_dhis2)

expected_sha256 <- "34412b3539ccdfae5e71dc56b007a315282d545722f75369a33abc45eec328ac"
observed_sha256 <- digest::digest(file = resolved_dhis2, algo = "sha256", serialize = FALSE)
if (!identical(tolower(observed_sha256), expected_sha256)) {
  fail("Routine-data SHA-256 mismatch: observed %s; expected %s", observed_sha256, expected_sha256)
}
pass("Routine-data SHA-256 matches the frozen publication input")

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
qpath <- gsub("'", "''", resolved_dhis2, fixed = TRUE)
source_sql <- sprintf("read_csv_auto('%s')", qpath)

cols <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s", source_sql))$column_name
required_cols <- c("district", "period", "value")
missing_cols <- setdiff(required_cols, cols)
if (length(missing_cols)) fail("Routine CSV is missing required column(s): %s", paste(missing_cols, collapse = ", "))
pass("Routine-data schema contains district, period and value")

stats <- DBI::dbGetQuery(con, sprintf(
  paste0(
    "SELECT COUNT(*) AS n_rows, ",
    "ROUND(MIN(value), 2) AS min_raw, ",
    "ROUND(MAX(value), 2) AS max_raw, ",
    "ROUND(median(value), 1) AS median_raw, ",
    "SUM(CASE WHEN value > 100 THEN 1 ELSE 0 END) AS flag_gt100, ",
    "SUM(CASE WHEN value < 5 THEN 1 ELSE 0 END) AS flag_lt5 ",
    "FROM %s"
  ), source_sql))
print(stats)
pass("Routine values were summarised without deleting flagged observations")

## Flagship publication-input provenance check. These counts come from the frozen
## district-month demonstration extract used by the article and guard against silently
## pointing the release workflow at a different CSV with the same column names.
expected_rows <- 148L
expected_gt100 <- 73L
if (as.integer(stats$n_rows[[1]]) != expected_rows) {
  fail("Flagship routine-data row-count mismatch: observed %d; expected %d",
       as.integer(stats$n_rows[[1]]), expected_rows)
}
if (as.integer(stats$flag_gt100[[1]]) != expected_gt100) {
  fail("Flagship >100%% flag-count mismatch: observed %d; expected %d",
       as.integer(stats$flag_gt100[[1]]), expected_gt100)
}
pass("Flagship routine-data provenance matches 148 district-months and 73 >100%% flags")

flagged <- DBI::dbGetQuery(con, sprintf(
  paste0(
    "SELECT district, period, value, ",
    "CASE WHEN value > 100 THEN 'above 100%% plausibility flag' ",
    "WHEN value < 5 THEN 'low-value diagnostic flag' END AS flag_reason ",
    "FROM %s WHERE value > 100 OR value < 5 ORDER BY value DESC"
  ), source_sql))
info("Flagged records retained for diagnostic review: %d", nrow(flagged))
if (nrow(flagged)) print(utils::head(flagged, 20))

cat("\nROUTINE-DATA VALIDATION: PASS\n")
cat("Flagged values remain diagnostic records; this gate performs no automatic exclusions or corrections.\n")
