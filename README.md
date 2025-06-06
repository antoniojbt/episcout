[![R](https://github.com/antoniojbt/episcout/actions/workflows/r.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://codecov.io/gh/AntonioJBT/episcout)

# episcout

episcout provides helper functions for cleaning, exploring and visualising large datasets. It wraps common preprocessing and descriptive tasks so you can focus on analysis. The package builds on the **tidyverse** and **data.table** ecosystems for fast and flexible data manipulation.

Continuous integration is handled with **GitHub Actions** which runs `R CMD check` across multiple operating systems and reports code coverage via Codecov.

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

- Version 0.1.2
  Minor bug fixes and internal improvements

- Version 0.1.1
  First release
