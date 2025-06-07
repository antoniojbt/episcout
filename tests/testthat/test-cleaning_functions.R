context("episcout cleaning and utility")

######################
library(episcout)
library(testthat)
library(dplyr)
library(tibble)
library(lubridate)
library(purrr)
library(stringi)
library(stringr)
library(magrittr)
library(data.table)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/')
######################

######################
# Test set df:
set.seed(12345)
n <- 20
df <- data.frame(var_id = rep(1:(n / 2), each = 2),
                 var_to_rep = rep(c("Pre", "Post"), n / 2),
                 x = rnorm(n),
                 y = rbinom(n, 1, 0.50),
                 z = rpois(n, 2)
                 )
# df

# And save to disk with:
# epi_write(df, 'tests/testthat/df.tsv')
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
  val_id <- '1'
  comp <- epi_clean_compare_dup_rows(check_dups, val_id, 'var_id', 1, 2)
  # comp
  # View(t(check_dups[comp$duplicate_indices, ]))
  # View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))
  expect_equal(comp$differing_cols, c(2, 3, 4, 5))
  expect_equal(comp$col_names, c('var_to_rep', 'x', 'y', 'z'))
  expect_equal(comp$duplicate_indices, c(1, 2))
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
    dplyr::select_if(~ epi_clean_cond_numeric(.))
  expect_true(all(c('x', 'y', 'z', 'var_id') %in% names(get_cols)))
  expect_false(epi_clean_cond_numeric(df[[2]]))
  expect_true(epi_clean_cond_numeric(df[, 'x']))
  }
  )
######################

######################
print("Function being tested: epi_clean_cond_chr_fct")

test_that("Test expected output after epi_clean_cond_chr_fct", {
  col_chr <- data.frame('chr1' = rep(c('A', 'B')),
                        'chr2' = rep(c('C', 'D'))
                        )
  df_cont_chr <- tibble::as_tibble(cbind(df, col_chr))
  get_cols <- df_cont_chr %>% dplyr::select_if(~ epi_clean_cond_chr_fct(.))
  # Tests:
  expect_true(is.character(df_cont_chr$var_to_rep))
  expect_true(is.character(get_cols$chr1))
  expect_true(epi_clean_cond_chr_fct(df_cont_chr[[2]]))
  expect_false(epi_clean_cond_chr_fct(df_cont_chr[, 'x']))
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
  expect_true('date_col' %in% names(get_cols))
  expect_false(epi_clean_cond_date(df_date[[2]]))
  expect_true(epi_clean_cond_date(df_date[, 'date_col']))
  }
  )
######################

######################
print("Function being tested: epi_clean_count_classes")

test_that("epi_clean_count_classes", {
  df$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
  class_counts <- epi_clean_count_classes(df)
  expect_equal(unname(class_counts["character"]), 1)
  expect_equal(unname(class_counts["Date"]), 1)
  expect_equal(unname(class_counts["integer"]), 3)
  expect_equal(unname(class_counts["numeric"]), 1)
  }
  )
######################

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

######################
print("Function being tested: epi_clean_add_rep_num")

test_that("epi_clean_add_rep_num", {
  var_id <- 'var_id'
  var_to_rep <- 'var_to_rep'
  reps <- epi_clean_add_rep_num(df, 'var_id', 'var_to_rep')
  # reps
  # Sanity check:
  expect_true(identical(as.character(reps[[var_id]]),
                        as.character(df[[var_id]])))
  # Bind:
  df2 <- tibble::as_tibble(cbind(df, 'rep_num' = reps$rep_num))
  # merge() adds all rows from both data frames as there are duplicates
  # so use cbind after making sure order is exact
  # epi_head_and_tail(df2, rows = 3, last_cols = TRUE)
  expect_equal(df2$rep_num[1:6], c(1,2,1,2,1,2))
  expect_equal(tail(df2$rep_num), c(1,2,1,2,1,2))
  }
  )
######################

######################
print("Function being tested: epi_clean_add_colname_suffix")

test_that("epi_clean_add_colname_suffix", {
  # names(df)
  # Add .0 as suffix to all column names starting from column 2 (skip id col):
  id_col <- 1
  new_colnames <- epi_clean_add_colname_suffix(df, id_col, '.0')
  # new_colnames
  # Rename them in my data frame:
  names(df)[-id_col] <- new_colnames
  # names(df)
  expect_output(str(names(df)), '"var_id" "var_to_rep.0" "x.0" "y.0"')
  }
  )
######################

######################
print("Function being tested: epi_clean_spread_repeated")

test_that("epi_clean_spread_repeated", {
  df_spread <- epi_clean_spread_repeated(df, 'var_to_rep', 1)
  # df_spread
  expect_output(str(df_spread[1]), '0.586 -0.109 0.606 0.63 -0.284')
  expect_output(str(df_spread[1]), '1 1 0 0 0 1 0 1 1 0')
  expect_output(str(df_spread[2]), '0.709 -0.453 -1.818 -0.276 -0.919')
  expect_output(str(df_spread[2]), '1 3 1 1 4 2 1 2 2 1')
  }
  )
######################

######################
print("Function being tested: epi_clean_merge_nested_dfs")

test_that("epi_clean_merge_nested_dfs", {
  # Create a nested list of dataframes using the repeated measurements variable:
  df_spread <- epi_clean_spread_repeated(df, 'var_to_rep', 1)
  # Returns a nested list
  # Run an example with epi_clean_merge_nested_dfs()
  # to create a single dataframe with repeated observations spread and
  # no duplicate IDs (create a wide instead of a long dataframe):
  nested_list_dfs <- purrr::flatten(list(df_spread, df_spread, df_spread))
  id_col <- 'var_id'
  # epi_list_head(nested_list_dfs, 2, 3)
  # epi_list_tail(nested_list_dfs, 2, 3)
  all_merged <- epi_clean_merge_nested_dfs(nested_list_dfs, id_col)
  # dim(all_merged)
  # as.tibble(all_merged)
  # names(all_merged)
  # str(all_merged)
  # epi_head_and_tail(all_merged, rows = 5, cols = 3)
  expect_output(str(all_merged), '10 obs. of  25 variables')
  expect_output(str(all_merged), '0.586 -0.109 0.606 0.63 -0.284') # x.Pre
  expect_output(str(all_merged), '0.709 -0.453 -1.818 -0.276 -0.919') # x.Post_4
  expect_output(str(all_merged), '1 3 1 1 4 2 1 2 2 1') # z.Post_6
  }
  )

test_that("epi_clean_merge_nested_dfs handles two dfs", {
  df_spread <- epi_clean_spread_repeated(df, 'var_to_rep', 1)
  nested_two <- list(df_spread[[1]], df_spread[[2]])
  id_col <- 'var_id'
  expect_output(
    res <- epi_clean_merge_nested_dfs(nested_two, id_col),
    'Only two data frames provided'
  )
  expect_true(is.data.frame(res))
})
######################


######################
print("Function being tested: epi_clean_transpose")

test_that("epi_clean_transpose", {
  df$id_col <- rownames(df)
  # df
  id_col <- 6
  df_t <- epi_clean_transpose(df, id_col)
  # class(df_t)
  # dim(df)
  # dim(df_t)
  # df_t
  # names(df_t)
  # df_t[, 1] # should contain the original column headers
  expect_output(str(class(df_t)), 'data.frame')
  expect_output(str(dim(df)), '20 6')
  expect_output(str(dim(df_t)), '5 21')
  expect_output(str(names(df_t)), '"V1" "1" "2" "3" "4" "5" "6" "7"')
  expect_output(str(df_t[, 1]), '"var_to_rep" "x" "y" "z" "id_col"')
  expect_output(str(epi_head_and_tail(df_t)), 'chr  "1" "Pre" "0.585528817843856" "1"')
  expect_output(str(epi_head_and_tail(df_t)), 'chr  "2" "Post" "-0.453497173462763" "1" ...')
  }
  )
######################

######################
print("Function being tested: epi_clean_make_names")

test_that("epi_clean_make_names", {
  string <- c('mean', '.j_j', '...', 'if',
              'while', 'TRUE', 'NULL', '_jj',
              '  j', '.2way'
              )
  valid_names <- epi_clean_make_names(string)
  # valid_names
  str_ref <- c('mean', 'X_j_j', 'X___', 'if_',
               'while_', 'TRUE_', 'NULL_', 'X_jj',
               'X__j', 'X_2way'
               )
  expect_identical(valid_names, str_ref)
  }
)
######################
