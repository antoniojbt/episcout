[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://app.codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides helper functions for cleaning, exploring and visualising large
epidemiological datasets. It also supports specification-first exploratory data
analysis workflows for epidemiological datasets, where a data dictionary drives
schema checks, missingness summaries, descriptive summaries, plots and optional
HTML reports.

## Features

* **Cleaning** - `epi_clean_*` functions tidy raw data and detect issues such as duplicates or inconsistent labels.
* **Statistics** - `epi_stats_*` functions create summary tables and descriptive statistics in a single call.
* **Plotting** - `epi_plot_*` wrappers produce common graphs with *ggplot2* and *cowplot*.
* **Specification-first EDA** - `epi_eda_*` functions use a data dictionary to run repeatable EDA on synthetic or real data.
* **Utilities** - `epi_utils_*` helpers cover tasks like parallel processing and logging.

## Installation

<!--- 
You can install the released version of episcout from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("episcout")
```
--->

Install from GitHub:
``` r
install.packages("devtools")
library(devtools)
install_github("AntonioJBT/episcout")
```

## Development

Use the repository development environment so local checks run with the same R
tooling in Positron, Codex and shell sessions. Create it once with:

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

Set `EPISCOUT_RSCRIPT=/path/to/Rscript` if you need to use a different R
binary.

CRAN does not require `renv`; it requires a source tarball from `R CMD build`
that passes `R CMD check --as-cran` without errors, warnings or significant
notes. Strong dependencies should be available from CRAN or Bioconductor,
suggested packages should be used conditionally in examples and tests, and
tests/examples should avoid internet requirements, unwanted filesystem writes
and excessive runtime or parallelism. See the CRAN Repository Policy, CRAN
submission checklist and Writing R Extensions for the current source of truth:

- <https://cran.r-project.org/web/packages/policies.html>
- <https://cran.r-project.org/web/packages/submission_checklist.html>
- <https://cran.r-project.org/doc/manuals/r-release/R-exts.html>

## Getting Started

There are two main ways to use episcout:

* Use lower-level helpers directly: `epi_clean_*`, `epi_stats_*`, `epi_plot_*`
  and `epi_utils_*`.
* Use the specification-first EDA workflow: `epi_eda_spec()`,
  `epi_eda_generate_synthetic_data()`, `epi_eda_run()` and
  `epi_eda_render_report()`.

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

Start from a data dictionary with at least these columns:

``` csv
name,label,type,role,units,levels,min,max,missing_codes,required,group,description
age,Age at baseline,numeric,covariate,years,,18,110,,TRUE,demographics,Age in years
sex,Sex at birth,categorical,covariate,,"Female;Male;Unknown",,,Unknown,TRUE,demographics,Recorded sex
death,Death during follow-up,binary,outcome,,"0;1",0,1,,TRUE,outcomes,Outcome indicator
```

The optional `missing_codes` column accepts semicolon-separated sentinel values
such as `Unknown;Refused`. These values are counted as missing in
`epi_eda_profile_missing()` and excluded from observed EDA summaries. In
categorical summaries, `p` uses all rows as the denominator and `p_observed`
uses only observed non-missing rows.

You can prepare the workflow before real data arrive by generating synthetic
data from the same specification:

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

When real data are available, keep the same specification and change only the
data source:

``` r
data <- read.csv("data/input.csv", stringsAsFactors = FALSE)
dir.create("outputs", showWarnings = FALSE)

results <- epi_eda_run(
  data = data,
  spec = spec,
  output_dir = "outputs"
)
```

Render the optional HTML report when `rmarkdown` is installed:

``` r
epi_eda_render_report(
  data = data,
  spec = spec,
  output_dir = "outputs"
)
```

To create a starter project scaffold:

``` r
epi_eda_create_project("my-eda-project")
```

Current EDA workflow limits: summaries and plots are deliberately basic, the
synthetic data generator is for pipeline preparation and testing only, generated
synthetic data are not suitable for inference or disclosure control, and the MVP
does not yet include Arrow, DuckDB or data.table large-data backends.

## Contribute

- [Issue Tracker](https://github.com/AntonioJBT/episcout/issues)

- Pull requests welcome!


Support
-------

If you have any issues, pull requests, etc. please report them in the issue tracker. 

## News

- Version 0.1.4
  Added `epi_plot_theme_imss` and colour palette helpers.
  New `epi_plot_add_var_labels` layer.
  Rewritten `epi_stats_*` summary functions.

- Version 0.1.3
  Improved coverage tests, added a few wrappers, slightly improved documentation
  
- Version 0.1.2
  Minor bug fixes and internal improvements

- Version 0.1.1
  First release
