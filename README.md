[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r-cmd-check.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides helper functions for cleaning, exploring and visualising large datasets. It wraps common preprocessing and descriptive tasks so you can focus on analysis. The package builds on the **tidyverse** and **data.table** ecosystems for fast and flexible data manipulation.

## Features

* **Cleaning** – `epi_clean_*` functions tidy raw data and detect issues such as duplicates or inconsistent labels.
* **Statistics** – `epi_stats_*` functions create summary tables and descriptive statistics in a single call.
* **Plotting** – `epi_plot_*` wrappers make it straightforward to produce common graphs with *ggplot2* and *cowplot*.
* **Utilities** – `epi_utils_*` helpers cover tasks like parallel processing and logging.

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
scripts/rscript -e "cat(R.home())"
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

Functions are grouped by purpose, e.g.:
epi_clean_* for data wrangling/cleanup.
epi_stats_* for generating descriptive statistics and contingency tables.
epi_plot_* for plotting (wrappers around ggplot2 and cowplot).
epi_utils_* for utilities such as parallel processing, logging, etc.
Miscellaneous helpers such as epi_read/epi_write.

## Example

This is a basic example of things you can do with episcout:

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
