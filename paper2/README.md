# Paper 2 reproducibility surface

This directory contains the release-safe materials for the Paper 2 meta-research analysis comparing methodological alignment in recent East African publications using designed household surveys (DHS/MICS) and routine DHIS2 data.

## Analysis status

The primary review estimands refer to the frozen identified eligible publication frame. Detailed methodological coding used a prespecified stratified probability sample of 127 publications: 80 DHS/MICS and 47 DHIS2. The main analysis distinguishes:

- fully aligned;
- positively documented material misalignment; and
- insufficient reporting to determine alignment.

The four domains D1-D4 are treated as a linked diagnostic chain from data-generating structure to inferential target, method, and reporting or interpretation. They are not analysed as independent repeated observations.

## Contents

- `code/paper2_reproduce_all_final_analysis.R`: complete R reproduction of the literature-review estimates, sensitivities, reliability statistics, and main review figures.
- `data/Paper2_R_Analysis_Compact_127.csv`: release-safe coded analysis dataset.
- `data/Paper2_R_Reliability_Clean_26.csv`: preserved pre-adjudication reliability classifications.
- `data/Paper2_R_Strata_Clean.csv`: frozen publication-sampling strata.
- `rwanda/`: code and aggregate outputs for the prespecified Rwanda empirical illustration of inferential consequences.
- `search/`: release-safe search and candidate-frame provenance.

## Reproduction

Run the main review analysis from a directory containing the three CSV inputs:

```bash
Rscript code/paper2_reproduce_all_final_analysis.R
```

The script was validated with R 4.6.0 and `survey` 4.5.

## Data access and licensing

This public repository intentionally excludes copyrighted article full texts, licensed bibliographic database exports, private operational audit material, and restricted DHS microdata. The Rwanda microdata must be obtained independently under the DHS Program terms. Aggregate Rwanda outputs and the analysis code are provided here.

## Primary versus robustness analyses

The frozen n=47 DHIS2 probability sample remains the primary analysis. A post hoc census robustness analysis of the remaining 42 eligible DHIS2 publications is being conducted under the unchanged D1-D4 rules. It will be reported separately and will not redefine the primary design.

## Version

Hardening branch initiated 2026-10-13. Final release files will be tagged at manuscript submission.
