[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides helper functions for cleaning, exploring and visualising large epidemiological datasets. It also supports specification-first exploratory data analysis workflows for epidemiological datasets, where a data dictionary drives schema checks, missingness summaries, descriptive summaries, plots and optional HTML reports.

## Features

* **Cleaning** - `epi_clean_*` functions tidy raw data and detect issues such as duplicates or inconsistent labels.
* **Statistics** - `epi_stats_*` functions create summary tables and descriptive statistics in a single call.
* **Plotting** - `epi_plot_*` wrappers produce common graphs with *ggplot2* and *cowplot*.
* **Specification-first EDA** - `epi_eda_*` functions use a data dictionary to run repeatable EDA on synthetic or real data.
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

Set `EPISCOUT_RSCRIPT=/path/to/Rscript` if you need to use a different R binary.

CRAN does not require `renv`; it requires a source tarball from `R CMD build` that passes `R CMD check --as-cran` without errors, warnings or significant notes. Strong dependencies should be available from CRAN or Bioconductor, suggested packages should be used conditionally in examples and tests, and tests/examples should avoid internet requirements, unwanted filesystem writes and excessive runtime or parallelism. See the CRAN Repository Policy, CRAN submission checklist and Writing R Extensions for the current source of truth:

- <https://cran.r-project.org/web/packages/policies.html>
- <https://cran.r-project.org/web/packages/submission_checklist.html>
- <https://cran.r-project.org/doc/manuals/r-release/R-exts.html>

## Getting Started

There are three main ways to use episcout:

* Use lower-level helpers directly: `epi_clean_*`, `epi_stats_*`, `epi_plot_*` and `epi_utils_*`.
* Use the review-gated new-dataset workflow through `epi_eda_intake_run()`, or compose the lower-level specification-first functions directly.
* For related restricted PostgreSQL tables, follow the [audit-first longitudinal pseudonymisation guide](vignettes/longitudinal-pseudonymisation.Rmd) to create value-free linkage metadata, initialise a stable registry and inspect blockers before any write.

For a complete runnable learning path, use the [database-to-report walkthrough](inst/examples/db-to-report/README.md). Its commented R script starts with a duplicated synthetic longitudinal CSV, creates disposable PostgreSQL relations, pseudonymises them, runs database-backed EDA and finishes with plots, a Table 1 and an HTML report.

Pseudonymised data remain restricted personal data. They are not anonymous or automatically disclosure-controlled. The guide explains database-administrator prerequisites, duplicate handling, recovery and the reviewed handoff into EDA.

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

### Specification-first EDA quickstart

If data arrive before a dictionary, create a conservative draft from the in-memory data frame:

``` r
received_data <- data.frame(
  age = c(42, 57, 61, NA),
  study_group = c("A", "B", "A", "B"),
  stringsAsFactors = FALSE
)

draft_spec <- epi_eda_spec_scaffold(received_data)
draft_spec[, c("name", "type", "candidate_type", "n_missing", "review_status")]
write.csv(draft_spec, "data_dictionary_draft.csv", row.names = FALSE, na = "")
```

The draft records storage classes, missing counts, cardinality and conservative type candidates without including observed values as examples or candidate levels. It deliberately leaves roles, units, missing sentinels, validation bounds and requiredness unset. Review those fields, categorical declarations, privacy classification and factor metadata before loading the edited file with `epi_eda_spec()` or sharing it. Database inventory users should continue to use `epi_eda_dictionary_scaffold()`.

Start from a data dictionary with at least these columns:

``` csv
name,label,type,role,units,levels,min,max,missing_codes,required,group,description
age,Age at baseline,numeric,covariate,years,,18,110,,TRUE,demographics,Age in years
sex,Sex at birth,categorical,covariate,,"Female;Male;Unknown",,,Unknown,TRUE,demographics,Recorded sex
death,Death during follow-up,binary,outcome,,"0;1",0,1,,TRUE,outcomes,Outcome indicator
```

The optional `missing_codes` column accepts semicolon-separated sentinel values such as `Unknown;Refused`. These values are counted as missing in `epi_eda_profile_missing()` and excluded from observed EDA summaries and plots. Schema output reports presence in `status` and separately reports descriptive type compatibility in `type_status` and `type_reason`; it does not coerce data. The canonical summary contract covers numeric, integer, categorical, binary, text, date and datetime variables, with explicit variable coverage and documented skips.

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

Character numeric parsing is not implicit, categorical levels come from the reviewed specification, and local character datetimes require a reviewed `timezone`. Empty or whitespace-only sentinels cannot be represented by the current semicolon-delimited `missing_codes` format. The preparation core is in memory and does not write row-level data. It does not identify personal information or anonymise data; pseudonymisation remains a separate controlled step.

Summarise a prepared cohort overall and by one reviewed categorical or binary variable, then create a traceable long-form Table 1:

``` r
stratified <- epi_eda_profile_stratified(prepared$data, spec, strata = "sex")
stratified$groups
stratified$numeric

table1 <- epi_eda_table1(stratified)
table1
```

Declared empty groups and levels remain visible, unexpected and missing strata are flagged, and numeric percentages retain their denominators in the calculation result. If missing strata are excluded, Overall describes only the included rows and metadata accounts for the omission. Table 1 contains no p-values and performs no automatic small-cell suppression; it is not disclosure-controlled and must be reviewed before sharing.

For a guided new-dataset run, let the workflow create the first scaffold and stop for review:

``` r
intake_dir <- tempfile("episcout-intake-")
first_run <- epi_eda_intake_run(received_data, output_dir = intake_dir)
first_run$status
first_run$manifest
```

Edit `spec_scaffold.csv` outside the run, explicitly review every field and set each `review_status` to `reviewed`. Then audit the reviewed specification before applying it:

``` r
reviewed_spec <- epi_eda_spec("reviewed_spec.csv")

audit_run <- epi_eda_intake_run(
  received_data,
  reviewed_spec,
  output_dir = tempfile("episcout-audit-"),
  prepare = "audit"
)

final_run <- epi_eda_intake_run(
  received_data,
  reviewed_spec,
  output_dir = tempfile("episcout-report-"),
  prepare = "apply",
  strata = "study_group"
)
```

Expected gates return `review_required`, `blocked` or `audit_complete`; a reconciled analysis returns `complete`. The manifest distinguishes files that were and were not created. The workflow writes specification metadata, audits and aggregate summaries, never raw or prepared rows. Explicit `id`/`identifier` roles are excluded from returned and exported profiles, but variable names, supplied specification metadata and small groups may still be sensitive. The bundle is not de-identified or disclosure-controlled, and pseudonymisation remains a separate explicit workflow.

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

Numeric, text and temporal plots use compact 30-bin aggregate data. Text plots show Unicode character lengths rather than raw strings. Variables whose reviewed role is `id` or `identifier` retain aggregate missingness, are skipped from typed summaries and have named `NULL` plot entries.

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
  max_plot_levels = 20L
)

DBI::dbDisconnect(con)
```

Each direct profiler owns one read-only repeatable-read transaction. `epi_eda_db_run()` uses one such snapshot for all database reads, ends it before rendering or filesystem publication, and writes only registered aggregate artifacts through a sibling staging directory. Overwrite requires an exact unchanged prior database-EDA manifest plus matching source identity, specification fingerprint and plot options.

The bundle is aggregate-only, not anonymous or disclosure-controlled. Complete categorical frequencies, identifier QA, plots, variable and relation names, declared levels and missing sentinels may be sensitive. The normalized caller-authored specification is deliberately written for review, so its declared values are not covered by the raw-observation exclusion. Review every artifact before sharing. episcout does not control PostgreSQL, RPostgres, administrator, backup or server logging. Unsupported storage, especially `timestamp without time zone`, requires a caller-reviewed view cast; episcout does not infer local-time or DST meaning.

The aggregate PostgreSQL bundle is the end of the server-backed workflow. `epi_eda_render_report()` currently accepts an in-memory data frame, not a PostgreSQL source or aggregate bundle. Render its optional HTML report only for an approved in-memory dataset when `rmarkdown` and Pandoc are installed:

``` r
epi_eda_render_report(
  data = prepared$data,
  spec = spec,
  output_dir = "outputs"
)
```

To create a starter project scaffold:

``` r
epi_eda_create_project("my-eda-project")
```

Current EDA workflow limits: the synthetic data generator is for pipeline preparation and testing only, generated synthetic data are not suitable for inference or disclosure control, and PostgreSQL is the only server-backed EDA source. Arrow, DuckDB, data.table, SQLite, generic DBI and arbitrary lazy-query backends are not supported. Correlation, contingency and epidemiological outcome statistics remain separate from univariate EDA summaries.

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
