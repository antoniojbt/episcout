[![R](https://github.com/antoniojbt/episcout/actions/workflows/r.yml/badge.svg)](https://github.com/antoniojbt/episcout/actions/workflows/r.yml)
[![Travis build status](https://travis-ci.org/AntonioJBT/episcout.svg?branch=master)](https://travis-ci.org/AntonioJBT/episcout)
[![codecov](https://codecov.io/gh/AntonioJBT/episcout/branch/master/graph/badge.svg)](https://codecov.io/gh/AntonioJBT/episcout)

# episcout

Facilitates cleaning, exploring and visualising large-ish datasets (hundreds of thousands to millions of observations with tens to hundreds of variables).

These are mostly wrapper and convenience functions to pre-process (wrangle, explore, clean, etc.) data-sets. Assumes you're happy with tidyverse and the basics of data.table.

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

- Version 0.1.1
  First release

In development: 
- Version 0.1.2
