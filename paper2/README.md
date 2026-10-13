# Paper 2: meta-research reproducibility surface

This directory contains release-safe derived data and R code for the manuscript on structure, estimand, method, and reporting in contemporary secondary analyses of DHS, MICS, and DHIS2 data in the East African Community.

## Included

- `data/Paper2_R_Analysis_Compact_127.csv`: derived coding data for the frozen probability sample of 127 publications.
- `data/Paper2_R_Reliability_Clean_26.csv`: pre-adjudication D1-D4 and overall classifications for the prespecified 26-publication reliability subset.
- `data/Paper2_R_Strata_Clean.csv`: finite-frame and sample counts by sampling stratum.
- `R/paper2_reproduce_all_final_analysis.R`: primary design-weighted analysis, finite-population uncertainty, domain estimates, observability composition, descriptive domain cross-tabs, sentinel analyses, sensitivity analyses, reliability, and figures.
- `data/Paper2_DHIS2_Census_Additional42_2026-10-13.csv`: the 42 eligible DHIS2 publications outside the frozen primary n=47 DHIS2 probability sample, used only for the post hoc census robustness analysis.
- `R/analyze_dhis2_census.R`: census robustness analysis with hard checks for disjointness and an N=89 union.
- `data/Table_DHIS2_census_*.csv`: derived census result tables.

## Reproduction boundary

This public surface does not redistribute Scopus exports, copyrighted article full texts, restricted DHS microdata, credentials, or private audit material. Review classifications and derived analytical data are provided so the reported statistical analyses can be inspected without redistributing protected source material.

The Rwanda empirical illustration uses access-controlled DHS microdata. Users must obtain their own authorised DHS access to reproduce microdata-level calculations.
