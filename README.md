[![Travis build status](https://travis-ci.org/AntonioJBT/episcout.svg?branch=master)](https://travis-ci.org/AntonioJBT/episcout)

<!---
[![Coverage Status](https://coveralls.io/repos/github/AntonioJBT/episcout/badge.svg?branch=master)](https://coveralls.io/github/AntonioJBT/episcout?branch=master)
--->

# episcout

*UNDER DEVELOPMENT*

Facilitates cleaning, exploring and visualising large-ish datasets (hundreds of thousands to millions of observations with tens to hundreds of variables).

These are mostly wrapper and convenience functions to pre-process (wrangle, explore, clean, etc.) data-sets. Assumes you're happy with tidyverse and the basics of data.table.

## Installation

You can install the released version of episcout from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("episcout")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# A data frame:
n <- 20
df <- data.frame(
	var_id = rep(1:(n / 2), each = 2),
  var_to_rep = rep(c('Pre', 'Post'), n / 2),
	x = rnorm(n),
	y = rbinom(n, 1, 0.50),
	z = rpois(n, 2)
	)
df

# Print the first few rows and last few rows:
dim(df)
head_and_tail(df, rows = 2, cols = 2)
head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)


# Get all duplicates:
check_dups <- get_all_dups(df, 'var_id', 1)
dim(check_dups)
check_dups

# Get summary descriptive statistics for numeric/integer column:
num_vec <- df$x
desc_stats <- epi_stats(num_vec)
class(desc_stats)
lapply(desc_stats, class)
desc_stats
```

## Contribute

- [Issue Tracker](https://github.com/AntonioJBT/episcout/issues)

- Pull requests welcome!


Support
-------

If you have any issues, pull requests, etc. please report them in the issue tracker. 


