# Paper 2: post hoc DHIS2 census robustness analysis
# Frozen primary DHIS2 probability sample: n=47 of N=89
# Additional census set: n=42
# No census sampling CI is calculated because all 89 eligible DHIS2 publications are enumerated.

options(stringsAsFactors = FALSE)

primary_path <- file.path("..", "r_reproduction_20261013", "Paper2_R_Analysis_Compact_127.csv")
additional_path <- "Paper2_DHIS2_Census_Additional42_2026-10-13.csv"

primary_all <- read.csv(primary_path, check.names = FALSE)
additional <- read.csv(additional_path, check.names = FALSE)

primary <- subset(primary_all, arm == "DHIS2")

stopifnot(nrow(primary) == 47L)
stopifnot(nrow(additional) == 42L)
stopifnot(length(unique(primary$study_id)) == 47L)
stopifnot(length(unique(additional$study_id)) == 42L)
stopifnot(length(intersect(primary$study_id, additional$study_id)) == 0L)
stopifnot(length(unique(c(primary$study_id, additional$study_id))) == 89L)

allowed <- c("A", "M", "I", "NA")
for (v in c("d1", "d2", "d3", "d4")) stopifnot(all(additional[[v]] %in% allowed))

derive_overall <- function(d1, d2, d3, d4) {
  z <- c(d1, d2, d3, d4)
  if (any(z == "M")) return("materially_misaligned")
  if (any(z == "I")) return("indeterminate_or_partial")
  "fully_aligned"
}
derived <- mapply(derive_overall, additional$d1, additional$d2, additional$d3, additional$d4)
stopifnot(all(derived == additional$overall_alignment_class))

# Primary design-weighted point estimates.
primary$publication_weight <- primary$stratum_N / primary$stratum_n
primary_est <- function(predicate) {
  100 * sum(primary$publication_weight * predicate) / sum(primary$publication_weight)
}

primary_overall <- data.frame(
  outcome = c("fully_aligned", "materially_misaligned", "indeterminate_or_partial"),
  primary_weighted_pct = c(
    primary_est(primary$overall_alignment_class == "fully_aligned"),
    primary_est(primary$overall_alignment_class == "materially_misaligned"),
    primary_est(primary$overall_alignment_class == "indeterminate_or_partial")
  )
)
primary_not_full <- 100 - primary_overall$primary_weighted_pct[primary_overall$outcome == "fully_aligned"]
primary_m_share <- 100 * primary_overall$primary_weighted_pct[primary_overall$outcome == "materially_misaligned"] / primary_not_full

# Harmonize domain names and enumerate all 89.
primary_census <- data.frame(
  study_id = primary$study_id,
  d1 = primary$d1_data_structure_class,
  d2 = primary$d2_inferential_goal_class,
  d3 = primary$d3_method_class,
  d4 = primary$d4_reporting_interpretation_class,
  overall_alignment_class = primary$overall_alignment_class,
  coding_provenance = "primary_frozen_n47"
)
additional_census <- additional[, c("study_id","d1","d2","d3","d4","overall_alignment_class","coding_provenance")]
census <- rbind(primary_census, additional_census)
stopifnot(nrow(census) == 89L)

pct <- function(x, level) 100 * mean(x == level)

census_overall <- data.frame(
  outcome = c("fully_aligned", "materially_misaligned", "indeterminate_or_partial"),
  census_n = c(
    sum(census$overall_alignment_class == "fully_aligned"),
    sum(census$overall_alignment_class == "materially_misaligned"),
    sum(census$overall_alignment_class == "indeterminate_or_partial")
  )
)
census_overall$census_pct <- 100 * census_overall$census_n / 89

comparison <- merge(primary_overall, census_overall, by = "outcome", sort = FALSE)
comparison$census_minus_primary_pp <- comparison$census_pct - comparison$primary_weighted_pct

census_not_full_n <- sum(census$overall_alignment_class != "fully_aligned")
census_not_full_pct <- 100 * census_not_full_n / 89
census_m_share <- 100 * sum(census$overall_alignment_class == "materially_misaligned") / census_not_full_n

composition <- data.frame(
  metric = c("not_fully_aligned_pct", "M_share_among_not_fully_aligned_pct"),
  primary_weighted_pct = c(primary_not_full, primary_m_share),
  census_pct = c(census_not_full_pct, census_m_share)
)
composition$census_minus_primary_pp <- composition$census_pct - composition$primary_weighted_pct

domain_rows <- list()
for (d in c("d1","d2","d3","d4")) {
  for (state in c("A","M","I","NA")) {
    domain_rows[[length(domain_rows)+1L]] <- data.frame(
      domain = toupper(d),
      state = state,
      census_n = sum(census[[d]] == state),
      census_pct = pct(census[[d]], state)
    )
  }
}
domain_table <- do.call(rbind, domain_rows)

provenance_table <- as.data.frame.matrix(table(additional$coding_provenance, additional$overall_alignment_class))
provenance_table$coding_provenance <- rownames(provenance_table)
rownames(provenance_table) <- NULL

write.csv(comparison, "Table_DHIS2_census_overall_comparison.csv", row.names = FALSE)
write.csv(composition, "Table_DHIS2_census_observability_composition.csv", row.names = FALSE)
write.csv(domain_table, "Table_DHIS2_census_domain_states.csv", row.names = FALSE)
write.csv(provenance_table, "Table_DHIS2_census_provenance_sensitivity.csv", row.names = FALSE)

cat(sprintf("Census N = %d\n", nrow(census)))
cat(sprintf("Fully aligned: %d/89 = %.3f%%\n",
            sum(census$overall_alignment_class == "fully_aligned"),
            pct(census$overall_alignment_class, "fully_aligned")))
cat(sprintf("Materially misaligned: %d/89 = %.3f%%\n",
            sum(census$overall_alignment_class == "materially_misaligned"),
            pct(census$overall_alignment_class, "materially_misaligned")))
cat(sprintf("Indeterminate/partial: %d/89 = %.3f%%\n",
            sum(census$overall_alignment_class == "indeterminate_or_partial"),
            pct(census$overall_alignment_class, "indeterminate_or_partial")))
cat(sprintf("Not fully aligned: %d/89 = %.3f%%\n", census_not_full_n, census_not_full_pct))
cat(sprintf("M share among not fully aligned: %.3f%%\n", census_m_share))
