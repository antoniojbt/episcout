[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides lower-level helper functions and specification-first workflows for cleaning, exploring and visualising epidemiological data. Use a declared data dictionary to make schema checks, missingness summaries, descriptive summaries, plots and optional reports repeatable.

## Install

Install the published 0.3.0 release from GitHub:

```r
install.packages("devtools")
devtools::install_github("AntonioJBT/episcout@0.3.0")
```

Install from `master` only when you deliberately need the current development version:

```r
devtools::install_github("AntonioJBT/episcout")
```

## Choose a workflow

- **Lower-level cleaning, statistics and plotting helpers:** begin with the [lower-level helper introduction](vignettes/introduction_episcout.Rmd). It contains a runnable neutral synthetic-data example.
- **New-data specification-first exploratory data analysis workflows:** start with `epi_eda_intake_run()` for a guided intake, or compose `epi_eda_run()` and `epi_eda_render_report()` from the [specification-first EDA guide](vignettes/specification-first-eda.Rmd). Create and review a semantic dictionary before running schema checks, summaries, plots or reports. Synthetic data support pipeline preparation and testing; they are not suitable for inference. episcout creates the requested outputs but does not decide whether they may be shared.
- **Longitudinal PostgreSQL pseudonymisation:** follow the [longitudinal pseudonymisation guide](vignettes/longitudinal-pseudonymisation.Rmd) before handling restricted data. Pseudonymised data remain restricted personal data; they are not anonymous or automatically disclosure-controlled.
- **Explicit geospatial mapping:** follow the [geospatial mapping guide](vignettes/geospatial-mapping-primer.Rmd) to declare coordinates and CRS, inspect geometry and create maps. Bounds and feature maps are value-bearing and may disclose location.

For an end-to-end learning example that combines disposable PostgreSQL tables, pseudonymisation and aggregate EDA delivery, use the [database-to-EDA-delivery walkthrough](inst/examples/db-to-report/README.md). Its synthetic data are for learning and testing, not inference; aggregate output is not automatically disclosure-controlled.

## Features

- `epi_clean_*`, `epi_stats_*`, `epi_plot_*` and `epi_utils_*` provide lower-level helpers for data preparation, descriptive work, plotting and utilities.
- `epi_eda_*` provides specification-first EDA for in-memory data and supported PostgreSQL sources.
- `epi_sec_*` provides auditable longitudinal pseudonymisation for related PostgreSQL tables.
- `epi_geo_*` provides explicit vector and coordinate mapping with optional `sf` support.

## Contributing

Read [AGENTS.md](AGENTS.md) for development and contribution instructions, [PROJECT_MAP.md](PROJECT_MAP.md) for implemented workflows and package locations, and use the canonical checks in `scripts/check-local.sh` and `scripts/check-cran.sh`. Report defects or propose changes through the [issue tracker](https://github.com/AntonioJBT/episcout/issues).
