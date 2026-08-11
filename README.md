# Two Data Worlds: Reproducible Statistical Worked Examples

This repository contains reproducibility materials for statistical worked examples involving two common health-data environments: complex survey microdata, illustrated with the Rwanda Demographic and Health Survey (RDHS) 2019–20, and routine health information system data, illustrated with a frozen DHIS2 demonstration-data extract.

The repository contains analysis code, validation gates, non-disclosive aggregate outputs, and release figure files. Protected DHS microdata and the frozen raw routine-data CSV are not distributed here.

## Repository contents

- `R/fetch-dhs.R` - authorised DHS acquisition helper.
- `R/validate-rdhs.R` - RDHS design and benchmark validation gate.
- `R/validate-routine.R` - frozen routine-data provenance and diagnostic validation gate.
- `R/worked-examples.R` - statistical worked examples and deterministic simulations.
- `R/make-figure1.R` - Figure 1 release build from validated aggregate inputs.
- `data/derived/` - non-disclosive aggregate results.
- `figures/` - PDF, SVG, PNG, and factual alt-text aid for Figure 1.
- `renv.lock` - frozen R dependency environment.

## Software environment

The validated environment uses R 4.6.0 and `renv` for dependency management. From the repository root run:

```text
Rscript -e "renv::restore(prompt = FALSE)"
Rscript -e "renv::status()"
```

A correctly restored repository should report that the project is in a consistent state.

## Protected RDHS data

DHS microdata are access-controlled and are not included in this repository. Users wishing to regenerate the RDHS analyses must independently obtain authorised access to the Rwanda 2019–20 Individual Recode dataset corresponding to `RWIR81DT.ZIP` and prepare the authorised file locally as `RWIR81DT.rds`.

Set the environment variable `RDHS_IR_RDS` to that local authorised file. Raw-data locations and common microdata formats are excluded from Git.

## Routine DHIS2 example

The routine-data worked example uses a frozen extract derived from DHIS2 demonstration analytics. The extract contains 148 district-month observations and is used for statistical data-quality diagnostics, not for substantive claims about a national health programme.

The frozen raw CSV is not redistributed here while source-specific redistribution terms remain under review. Users possessing the validated input can set `DHIS2_FILE` to their local `dhis2_indicator.csv`.

Expected SHA-256:

```text
34412B3539CCDFAE5E71DC56B007A315282D545722F75369A33ABC45EEC328AC
```

`R/validate-routine.R` fails if the supplied source file does not match this frozen input.

## Reproduction workflow

1. Restore the environment with `renv::restore()`.
2. Set `DHIS2_FILE` and run `Rscript R/validate-routine.R`.
3. Set `RDHS_IR_RDS` and run `Rscript R/validate-rdhs.R`.
4. With both data sources available, run `Rscript R/worked-examples.R`.
5. Run `Rscript R/make-figure1.R` to rebuild Figure 1.

The routine-data validation expects 148 observations, 73 values above 100, and 3 low-value diagnostic flags. Flagged records remain in the data for diagnostic review and are not automatically deleted or corrected.

The RDHS validation expects 500 observed PSUs, 60 strata, at least 2 PSUs per stratum, health-insurance coverage of approximately 82.6%, and ANC4+ coverage of approximately 47.2%. Only aggregate analytical outputs are written to `data/derived/`; DHS microdata are not exported.

Figure 1 can be rebuilt directly from the aggregate CSV files distributed in `data/derived/`, so protected DHS microdata are not required merely to reproduce the released figure.

## Frozen aggregate outputs

```text
figure1-panel-a-generated.csv
E92055A931161DD6F8C44EFC15551DF40FFCFDE84CEA162EAE4FE59670439019

figure1-panel-b-generated.csv
AB11394A638089673D4D54D7F2E88C41756B055F72EE23B311B7C1313BFFA02A

section6-rdhs-validation-summary.csv
5C1A9258DBF442E8223970EAE5D47B03B323750909AF2DA50B84933A4E9F3305
```

## Reproducibility boundary

This public repository is deliberately separated from the private research workspace. It does not contain DHS microdata, private raw-data files, credentials, audit or pilot materials, manuscript or submission drafts, or machine-specific absolute file paths.

Its purpose is to provide code, environment metadata, validation logic, aggregate outputs, and figure-generation materials sufficient to inspect and reproduce the statistical workflow while respecting source-data access restrictions.

## Licence and reuse

Original source code under `R/` is released under the MIT License. The MIT grant does not extend to `data/derived/`, `figures/`, protected source data, or third-party materials. See `LICENSE` and `NOTICE` for the precise repository licensing and data-use boundary.
