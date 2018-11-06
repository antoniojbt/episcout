context("episcout cleaning and utility")

######################
library(episcout)
library(testthat)
library(dplyr)
library(tibble)
library(lubridate)
library(purrr)
library(stringr)
######################

# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/')

######################
# Test set df:
set.seed(12345)
n <- 20
df <- data.frame(
	var_id = rep(1:(n / 2), each = 2),
	var_to_rep = rep(c('Pre', 'Post'), n / 2),
	x = rnorm(n),
	y = rbinom(n, 1, 0.50),
	z = rpois(n, 2)
)
# df

# And save to disk with:
# epi_write(df, 'inst/extdata/df.tsv')
# df.tsv is then used for tests below
######################

######################
# Get all duplicates:
print("Function being tested: epi_clean_get_dups")

test_that("Test expected output after epi_clean_get_dups", {
	# Should contain all the df (because each row is duplicated
	# if looking at 'var_id' only)
	check_dups <- epi_clean_get_dups(df, 'var_id', 1)
	# str(dim(check_dups))
	# check_dups
	expect_output(str(dim(check_dups)), ' 20 5')
	expect_output(str(head(check_dups)), 'var_id    : int  1 1 2 2 3 3')
	# Should be empty:
	check_dups <- epi_clean_get_dups(df, 'var_id', 2)
	# str(dim(check_dups))
	# str(check_dups)
	expect_output(str(dim(check_dups)), ' 0 5')
	expect_output(str(check_dups), '0 obs. of  5 variables:')
	}
	)
######################

######################
print("Function being tested: epi_clean_compare_dup_rows")

test_that("Test expected output after epi_clean_compare_dup_rows", {
	# Check a few duplicated individuals:
	check_dups <- epi_clean_get_dups(df, 'var_id', 1)
	val_id <- '2' # TO DO: '1' matches '1' and '10' despite fixed = TRUE
	comp <- epi_clean_compare_dup_rows(check_dups, val_id, 'var_id', 1, 2)
	# comp
	# View(t(check_dups[comp$duplicate_indices, ]))
	# View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))
  expect_output(str(comp$differing_cols), ' 3 5')
  expect_output(str(comp$col_names), ' "x" "z"')
  expect_output(str(comp$duplicate_indices), ' 3 4')
  }
  )
######################

######################
print("Function being tested: epi_clean_compare_str")

test_that("Test expected output after epi_clean_compare_str", {
	# test data:
	letts <- paste(letters, collapse = ' ')
	other_letts <- toupper(paste(letters, collapse = ' '))
	df_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
										'str' = rep(x = substr(letts, 1, 5), 10),
										stringsAsFactors = FALSE)
	df2_comp <- data.frame ('sub' = rep(x = substr(letts, 1, 5), 10),
										'str' = rep(x = substr(other_letts, 6, 10), 10),
										stringsAsFactors = FALSE)
	df3 <- rbind(df_comp, df2_comp)
	col_1 <- 'sub'
	col_2 <- 'str'
	# df3[1, c(col_1, col_2)]
	# df3
  # Test output expectations:
	expect_output(str(epi_clean_compare_str(df3, 1, col_1, col_2)), ' TRUE')
	expect_output(str(epi_clean_compare_str(df3, 11, col_1, col_2)), ' FALSE')
  }
  )
######################

######################
print("Function being tested: epi_clean_cond_numeric")

test_that("Test expected output after epi_clean_cond_numeric", {
	get_cols <- df %>%
		# select_if(is.integer) %>%
		# select_if(is.numeric) %>%
		select_if(~ epi_clean_cond_numeric(.))
	expect_output(str(head(get_cols, 1)), 'x     : num 0.586')
	expect_output(str(tail(get_cols, 1)), 'z     : int 1')
	expect_output(str(tail(get_cols, 1)), 'var_id: int 10')
	expect_output(str(epi_clean_cond_numeric(df[[2]])), 'FALSE')
  expect_output(str(epi_clean_cond_numeric(df[, 'x'])), 'TRUE')
  }
  )
######################

######################
print("Function being tested: epi_clean_cond_chr_fct")

test_that("Test expected output after epi_clean_cond_chr_fct", {
	col_chr <- data.frame('chr1' = rep(c('A', 'B')),
												'chr2' = rep(c('C', 'D'))
												)
	df_cont_chr <- as.tibble(cbind(df, col_chr))
	get_cols <- df_cont_chr %>% select_if(~ epi_clean_cond_chr_fct(.))
  # Tests:
	expect_output(str(head(get_cols, 1)), 'Factor w/ 2 levels "Post","Pre"')
	expect_output(str(tail(get_cols, 1)), 'Factor w/ 2 levels "C","D"')
	expect_output(str(epi_clean_cond_chr_fct(df_cont_chr[[2]])), 'TRUE')
	expect_output(str(epi_clean_cond_chr_fct(df_cont_chr[, 'x'])), 'FALSE')
  }
  )
######################

######################
print("Function being tested: epi_clean_cond_date")

test_that("Test expected output after epi_clean_cond_date", {
	df_date <- df
	df_date$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
	get_cols <- df_date %>% select_if(~ epi_clean_cond_date(.))
	# Tests:
	expect_output(str(head(get_cols, 1)), 'date_col: Date, format:')
	expect_output(str(tail(get_cols, 1)), 'date_col: Date, format:')
	expect_output(str(epi_clean_cond_date(df_date[[2]])), 'FALSE')
	expect_output(str(epi_clean_cond_date(df_date[, 'date_col'])), 'TRUE')
  }
  )
######################

######################
print("Function being tested: epi_clean_count_classes")

test_that("epi_clean_count_classes", {
	df$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
	expect_output(str(epi_clean_count_classes(df)), ' 1 1 3 1')
	expect_output(str(head(epi_clean_count_classes(df))), '"Date" "factor" "integer" "numeric"')
  }
  )
######################

# TO DO: continue here
######################
print("Function being tested: epi_clean_class_to_factor")

test_that("epi_clean_class_to_factor", {
	df_factor <- df
	df_factor$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)#nrow(df_factor))
	# lapply(df_factor, class)
	# lapply(df_factor, function(x) length(unique(x)))
	# Check conditions:
	i <- 'date_col'
	cutoff_unique <- 10
	# if num. of unique values is less than cut-off:
	expect_output(str(
		length(unique(df_factor[[i]])) < cutoff_unique), # should be TRUE
		'TRUE')
	# and the class is not already a date:
	expect_output(str(
		class(df_factor[[i]]) != "Date"), # should be FALSE
		'FALSE')
	# Test column class changed, should return y and z as factors, date_col as Date:
	df_factor <- epi_clean_class_to_factor(df_factor, cutoff_unique = 10)
	get_classes <- lapply(df_factor, class)
	expect_output(str(head(get_classes, 1)), 'chr "integer"')
	expect_output(str(head(get_classes, 2)), 'chr "factor"')
	expect_output(str(head(get_classes, 3)), 'chr "numeric"')
	expect_output(str(head(get_classes, 4)), 'chr "factor"')
	expect_output(str(head(get_classes, 5)), 'chr "factor"')
	expect_output(str(head(get_classes, 6)), 'chr "Date"')
  }
  )
######################

######################
print("Function being tested: epi_clean_replace_value")

test_that("epi_clean_replace_value", {
	df_factor <- df
	df_factor$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
	# Convert to character first:
	df_factor$date_col <- as.character(df_factor$date_col)
	# lapply(df_factor, class)
	patterns <- c('2018', '2022')
	# match values starting with string
	pattern <- pattern <- sprintf('^%s', patterns[1])
	# Convert first pattern to NA
	# In a separate vector:
	# convert_NA <- epi_clean_replace_value(df_factor, 'date_col', pattern, NA)
	# convert_NA
	# df_factor$date_col
	# In place:
	df_factor[['date_col']] <- epi_clean_replace_value(df_factor,
																										 'date_col',
																										 pattern,
																										 NA)
	# df_factor$date_col
  expect_output(str(head(df_factor$date_col)), 'NA "2019-01-01" "2020-01-01"')
  expect_output(str(tail(df_factor$date_col)), '"2022-01-01" NA "2019-01-01"')
	}
  )
######################

