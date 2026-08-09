[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides helper functions for cleaning, exploring and visualising large epidemiological datasets. It also supports specification-first exploratory data analysis workflows for epidemiological datasets, where a data dictionary drives schema checks, missingness summaries, descriptive summaries, plots and optional HTML reports.

## Features

* **Cleaning** - `epi_clean_*` functions tidy raw data and detect issues such as duplicates or inconsistent labels; `epi_clean_curp_audit()` performs value-free local CURP structural auditing and reviewed-field reconciliation without claiming registry validation.
* **Statistics** - `epi_stats_*` functions create summary tables and descriptive statistics in a single call.
* **Plotting** - `epi_plot_*` wrappers produce common graphs with *ggplot2* and *cowplot*.
* **Explicit geospatial mapping** - `epi_geo_*` functions convert declared coordinates, read local or explicitly bounded PostGIS geometry, describe and transform `sf` objects, create extensible static maps and stage safe GeoPackage output. Start with the [geospatial mapping guide](vignettes/geospatial-mapping-primer.Rmd).
* **Specification-first EDA** - `epi_eda_*` functions use a semantic data dictionary to run repeatable EDA on synthetic or real data, including coordinate-pair QA and explicitly requested bounded point maps.
* **PostgreSQL-backed EDA** - `epi_eda_postgres_source()` and the existing profilers run aggregate-only specification-first EDA against PostgreSQL 17 relations, while `epi_eda_db_run()` publishes a manifest-owned bundle.
* **Longitudinal pseudonymisation** - `epi_sec_*` functions audit and transactionally pseudonymise related PostgreSQL tables through a stable restricted identity registry. Start with the [longitudinal pseudonymisation guide](vignettes/longitudinal-pseudonymisation.Rmd).
* **PostgreSQL identifier universes** - `epi_sec_identity_universe_spec()` and `epi_sec_identity_universe_db()` audit one reviewed identifier namespace across multiple tables and can atomically publish a restricted canonical enrolment source without generating pseudonyms.
* **Utilities** - `epi_utils_*` helpers cover tasks like parallel processing and logging.

## Installation

<!--- 
You can install the released version of episcout from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("episcout")
```
--->

Install the released GitHub version:

``` r
install.packages("devtools")
devtools::install_github("AntonioJBT/episcout@0.3.0")
```

Use `devtools::install_github("AntonioJBT/episcout")` only when you
deliberately want the current development version from `master`.

## Development

See the [project map](PROJECT_MAP.md) for the package architecture, implemented workflows, source/test/documentation locations and planning lifecycle.

Use the repository development environment so local checks run with the same R tooling in Positron, Codex and shell sessions. Create it once with:

``` bash
mamba env create -f environment.yml
```

Update an existing environment with:

``` bash
mamba env update -n episcout -f environment.yml --prune
```

Run package checks through the repository wrapper, not bare `Rscript`:

``` bash
scripts/rscript_env_caller.R -e "cat(R.home())"
scripts/check-local.sh
scripts/check-cran.sh
```

For tracked GitHub work, run the read-only lifecycle check before starting, at pull-request handoff and during post-merge closeout:

``` bash
scripts/check-workflow-state.sh
```

GitHub roadmap issue [#249](https://github.com/antoniojbt/episcout/issues/249) is authoritative for the live sequence. The [future-work guide](future/README.md) defines specification states, closeout requirements and the synchronised repository records.

Set `EPISCOUT_RSCRIPT=/path/to/Rscript` if you need to use a different R binary.

CRAN does not require `renv`; it requires a source tarball from `R CMD build` that passes `R CMD check --as-cran` without errors, warnings or significant notes. Strong dependencies should be available from CRAN or Bioconductor, suggested packages should be used conditionally in examples and tests, and tests/examples should avoid internet requirements, unwanted filesystem writes and excessive runtime or parallelism. See the CRAN Repository Policy, CRAN submission checklist and Writing R Extensions for the current source of truth:

- <https://cran.r-project.org/web/packages/policies.html>
- <https://cran.r-project.org/web/packages/submission_checklist.html>
- <https://cran.r-project.org/doc/manuals/r-release/R-exts.html>

## Getting Started

There are four main ways to use episcout:

* Use lower-level helpers directly: `epi_clean_*`, `epi_stats_*`, `epi_plot_*` and `epi_utils_*`.
* Use the new-dataset workflow through `epi_eda_intake_run()`, or compose the lower-level specification-first functions directly.
* For related restricted PostgreSQL tables, follow the [audit-first longitudinal pseudonymisation guide](vignettes/longitudinal-pseudonymisation.Rmd) to create value-free linkage metadata, initialise a stable registry and inspect blockers before any write.
* For explicit vector data or declared coordinate pairs, follow the [geospatial mapping guide](vignettes/geospatial-mapping-primer.Rmd) to inspect CRS and geometry, create a static map and publish GeoPackage output safely.

For a complete runnable learning path, use the [database-to-EDA-bundle walkthrough](inst/examples/db-to-report/README.md). Its commented R script starts with a duplicated synthetic longitudinal CSV, creates disposable PostgreSQL relations, separates semantic and linkage policy metadata, pseudonymises the relations and finishes with a manifest-owned database EDA bundle.

Pseudonymised data remain restricted personal data. They are not anonymous or automatically disclosure-controlled. The guide explains database-administrator prerequisites, duplicate handling, recovery and the semantic handoff into EDA.

### Helper functions

This is a basic example of the lower-level helper API:

``` r
library(episcout)

# A data frame:
n <- 20
df <- data.frame(var_id = rep(1:(n / 2), each = 2),
                 var_to_rep = rep(c('Pre', 'Post'), n / 2),
                 x = rnorm(n),
                 y = rbinom(n, 1, 0.50),
                 z = rpois(n, 2)
                 )
# Print the first few rows and last few rows:
dim(df)
epi_head_and_tail(df, rows = 2, cols = 2)
epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)


# Get all duplicates:
check_dups <- epi_clean_get_dups(df, 'var_id', 1)
dim(check_dups)
check_dups

# Get summary descriptive statistics for numeric/integer column:
num_vec <- df$x
desc_stats <- epi_stats_numeric(num_vec)
class(desc_stats)
lapply(desc_stats, class)
desc_stats

# And many more functions for cleaning, stats and plotting that do things a bit faster or more conveniently and I couldn't easily find in other packages.
```

### Explicit geospatial mapping

The geospatial interface requires explicit coordinate columns, axis order and CRS. `sf` is an optional dependency, and GeoPackage is the created-file format.

``` r
locations <- data.frame(
  site = c("site_a", "site_b"),
  longitude = c(0.25, 1.25),
  latitude = c(0.25, 0.75),
  group = c("reference", "comparison")
)

converted <- epi_geo_from_coords(
  locations,
  x = "longitude",
  y = "latitude",
  crs = 4326
)

converted$audit
epi_geo_describe(converted$data)
epi_geo_map(converted$data, value = "group")
```

Conversion is all-or-nothing: missing, non-finite or EPSG:4326 out-of-range rows return aggregate blockers and no partial geometry. Bounds and feature maps remain value-bearing and may disclose location. The interface does not infer coordinates, repair geometry, add basemaps, perform spatial inference or publish feature-level locations through ordinary EDA bundles.

For PostGIS, construct `epi_geo_postgis_source()` from an open caller-owned RPostgres connection and exact schema/relation identifiers. `epi_geo_postgis_describe()` returns only catalogue and aggregate geometry QA from one read-only snapshot. `epi_geo_postgis_collect()` is the sole feature-materialisation path: it requires an explicit ordinary-column allow-list, accepts only a typed `sf` bounding box in the source CRS, and refuses rather than truncates a selection above `max_features`. episcout never accepts credentials or arbitrary SQL through this interface and never installs or modifies PostGIS.

### Specification-first EDA quickstart

If data arrive before a dictionary, generate a lean semantic dictionary from storage metadata:

``` r
received_data <- data.frame(
  age = c(42, 57, 61, NA),
  study_group = c("A", "B", "A", "B"),
  stringsAsFactors = FALSE
)

draft_spec <- epi_eda_spec_scaffold(received_data)
names(draft_spec)
write.csv(draft_spec, "data_dictionary_draft.csv", row.names = FALSE, na = "")
```

The dictionary has exactly `name`, `label`, `type`, `role`, `units`, `levels`, `min`, `max`, `missing_codes`, `required`, `group`, `description`, `geo_role`, `geo_pair` and `geo_crs`. It contains no observed-count evidence, candidate fields, approval states or privacy policy. Factor and logical levels are storage metadata; scientific roles, sentinels, bounds and geographic meaning are not inferred. Database inventory users use `epi_eda_dictionary_scaffold()`.

Start from a data dictionary with at least these columns:

``` csv
name,label,type,role,units,levels,min,max,missing_codes,required,group,description
age,Age at baseline,numeric,covariate,years,,18,110,,TRUE,demographics,Age in years
sex,Sex at birth,categorical,covariate,,"Female;Male;Unknown",,,Unknown,TRUE,demographics,Recorded sex
death,Death during follow-up,binary,outcome,,"0;1",0,1,,TRUE,outcomes,Outcome indicator
```

The optional `missing_codes` column accepts semicolon-separated sentinel values such as `Unknown;Refused`. These values are counted as missing in `epi_eda_profile_missing()` and excluded from observed EDA summaries and plots. Schema output reports presence in `status` and separately reports descriptive type compatibility in `type_status` and `type_reason`; it does not coerce data. The canonical summary contract covers numeric, integer, categorical, binary, text, date and datetime variables, with explicit variable coverage and documented skips.

Geo fields are never inferred. A declared pair contains exactly one numeric/integer `x` row and one `y` row with the same pair identifier and resolvable CRS. `epi_eda_profile_geo()` reports pair completeness, non-finite values and EPSG:4326 range failures for data frames or PostgreSQL sources. Roles are descriptive: identifier and coordinate variables receive their declared summaries and ordinary plots.

Mapping is opt-in on all runners. `maps = TRUE` creates one coordinate-derived point map per map-ready pair; each explicit `map_vars` variable adds one thematic map per pair. Date and datetime themes are unsupported. Failed QC, zero rows and sources above `max_map_points` record `not_created` without partial maps or truncation. Bundle workflows use deterministic `maps/` SVG paths.

After saving and reviewing the dictionary, load it with its matching data and inspect the preparation plan before changing anything. Apply is all-or-nothing: a missing required variable, unsafe conversion or unexpected level returns the original data and a complete blocking audit.

``` r
spec <- epi_eda_spec("metadata/data_dictionary.csv")
data <- read.csv("data/input.csv", stringsAsFactors = FALSE)

assessment <- epi_eda_prepare(data, spec, mode = "audit")
assessment$audit

prepared <- epi_eda_prepare(data, spec, mode = "apply")
prepared$metadata
prepared$data
```

Character numeric parsing is not implicit, categorical levels come from the declared specification, and local character datetimes require an explicit `timezone`. Empty or whitespace-only sentinels cannot be represented by the current semicolon-delimited `missing_codes` format. The preparation core is in memory and does not write row-level data.

Summarise a prepared cohort overall and by one declared categorical or binary variable, then create a traceable long-form Table 1:

``` r
stratified <- epi_eda_profile_stratified(prepared$data, spec, strata = "sex")
stratified$groups
stratified$numeric

table1 <- epi_eda_table1(stratified)
table1
```

Declared empty groups and levels remain visible, unexpected and missing strata are flagged, and numeric percentages retain their denominators. If missing strata are excluded, Overall describes only the included rows and metadata accounts for the omission. Table 1 contains no p-values or automatic role-based suppression.

For a guided new-dataset run, let intake generate, save and use the semantic dictionary:

``` r
intake_dir <- tempfile("episcout-intake-")
first_run <- epi_eda_intake_run(received_data, output_dir = intake_dir)
first_run$status
first_run$manifest
```

Supply an edited semantic dictionary when the declarations differ from storage or preparation is required:

``` r
semantic_spec <- epi_eda_spec("semantic_spec.csv")

audit_run <- epi_eda_intake_run(
  received_data,
  semantic_spec,
  output_dir = tempfile("episcout-audit-"),
  prepare = "audit"
)

final_run <- epi_eda_intake_run(
  received_data,
  semantic_spec,
  output_dir = tempfile("episcout-report-"),
  prepare = "apply",
  strata = "study_group"
)
```

Processing returns `blocked`, `audit_complete` or `complete`. The workflow writes `specification.csv`, audits, aggregate summaries, optional maps and reports, never raw or prepared rows. Core manifests contain only `artifact`, `type`, `path`, `status` and `checksum_md5`.

You can prepare the workflow before real data arrive by generating synthetic data from the same specification:

``` r
library(episcout)

spec <- epi_eda_spec("metadata/data_dictionary.csv")

results <- epi_eda_run(
  data = NULL,
  spec = spec,
  synthetic = TRUE,
  n = 100,
  seed = 1
)

names(results)
results$metadata
```

When real data are available, keep the same specification and change only the data source:

``` r
data <- read.csv("data/input.csv", stringsAsFactors = FALSE)
dir.create("outputs", showWarnings = FALSE)

prepared <- epi_eda_prepare(data, spec, mode = "apply")
stopifnot(prepared$metadata$overall_status == "prepared")

results <- epi_eda_run(
  data = prepared$data,
  spec = spec,
  output_dir = "outputs"
)
```

The workflow writes `summary_variables.csv`, `summary_numeric.csv`, `summary_categorical.csv`, `summary_text.csv`, `summary_temporal.csv` and `summary_skipped.csv`. The `variables` table accounts for every specification row, including unavailable counts and reasons for absent or incompatible variables. Numeric summaries distinguish observed infinities from finite analytical values, categorical summaries expose total-row and observed-value denominators, and temporal summaries state their range units. The active lower-level statistics path uses the same univariate calculation cores; `epi_stats_summary(data, output = "typed")` returns the corresponding typed components without requiring an EDA specification.

Numeric, text and temporal plots use compact 30-bin aggregate data. Text plots show Unicode character lengths rather than raw strings. Roles describe variables and do not authorise or suppress plots.

### PostgreSQL-backed specification-first EDA

PostgreSQL 17 or later can execute the same schema, missingness, six-component summary and compact-plot contract against one safely identified table or view. Create the connection separately with RPostgres, keep it open and idle, and pass schema and relation names as separate identifiers. Arbitrary SQL, dotted relation names, generic DBI connections and caller-managed transactions are not accepted.

``` r
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("PGHOST"),
  dbname = Sys.getenv("PGDATABASE"),
  user = Sys.getenv("PGUSER")
)

source <- epi_eda_postgres_source(
  con,
  schema = "eda_fixture",
  relation = "observations"
)

schema_profile <- epi_eda_check_schema(source, spec)
missing_profile <- epi_eda_profile_missing(source, spec)
summary_profile <- epi_eda_profile_summaries(source, spec)
plot_profile <- epi_eda_profile_plots(source, spec)

bundle <- epi_eda_db_run(
  source,
  spec,
  output_dir = "postgres-eda-bundle",
  plots = TRUE,
  max_plot_levels = 20L,
  maps = TRUE,
  map_vars = "sex",
  max_map_points = 10000L
)

DBI::dbDisconnect(con)
```

Each direct profiler owns one read-only repeatable-read transaction. `epi_eda_db_run()` uses one such snapshot for all database reads, including map QC, the row bound and any requested coordinate/theme collection. It collects only ready-pair coordinates and explicit themes, never truncates, ends the snapshot before rendering, and publishes through a sibling staging directory. Overwrite fingerprints source, specification, plot and map settings.

episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared. It does not control PostgreSQL, RPostgres, administrator, backup or server logging. Unsupported storage, especially `timestamp without time zone`, requires an explicit view cast; episcout does not infer local-time or DST meaning.

The PostgreSQL bundle does not render HTML. `epi_eda_render_report()` accepts an in-memory data frame and supports the same map options when `rmarkdown` and Pandoc are installed:

``` r
epi_eda_render_report(
  data = prepared$data,
  spec = spec,
  output_dir = "outputs",
  maps = TRUE,
  map_vars = "sex"
)
```

### Breaking EDA migration

This release rejects old core schemas immediately. Replace scaffold evidence/review fields by regenerating with `epi_eda_spec_scaffold()`; move `privacy_class`, `analytic_action` and `validation_status` from combined dictionaries into `epi_sec_linkage_spec(columns = ...)`; remove ordinary catalogue `validation_status`; pass an exact three-key `columns` selector to `epi_db_catalogue_profile()`; replace six-column sensitivity-bearing core manifests with the five-column manifest; and rebuild old three-component linkage objects with a `columns` component. See the [EDA vignette](vignettes/specification-first-eda.Rmd) for old-to-new examples.

To create a starter project scaffold:

``` r
epi_eda_create_project("my-eda-project")
```

Current EDA workflow limits: the synthetic data generator is for pipeline preparation and testing only, generated synthetic data are not suitable for inference, and PostgreSQL is the only server-backed EDA source. Arrow, DuckDB, data.table, SQLite, generic DBI and arbitrary lazy-query backends are not supported. Correlation, contingency and epidemiological outcome statistics remain separate from univariate EDA summaries.

## Contribute

- [Issue Tracker](https://github.com/AntonioJBT/episcout/issues)

- Pull requests welcome!


Support
-------

If you have any issues, pull requests, etc. please report them in the issue tracker. 

## News

- Version 0.1.4 Added `epi_plot_theme_imss` and colour palette helpers. New `epi_plot_add_var_labels` layer. Rewritten `epi_stats_*` summary functions.

- Version 0.1.3 Improved coverage tests, added a few wrappers, slightly improved documentation
  
- Version 0.1.2 Minor bug fixes and internal improvements

- Version 0.1.1 First release
