######################
context("episcout test functions not classed, ie not in clean, plot, etc.")
######################

######################
library(episcout)
library(testthat)
######################

# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/')

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
# epi_write(df, 'df.tsv')
# df.tsv is then used for tests below
######################

######################
# Set some variables used in more than one test:
input_file <- 'df.tsv' # has to be relative to tests/testthat/
test_df <- epi_read(input_file)
######################

######################
print('Function being tested: epi_read')

test_that("Test expected output after epi_read", {
  # str(dim(test_df))
  # str(head(test_df, 1))
  expect_output(str(dim(test_df)), '20 5')
  expect_output(str(dim(mtcars)), '32 11')
  expect_output(str(head(test_df, 1)), 'var_id    : int 1')
  expect_output(str(head(test_df, 1)), 'var_to_rep: chr "Pre"')
  expect_output(str(head(test_df, 1)), 'x         : num 0.586')
  expect_output(str(tail(test_df, 1)), 'x         : num 0.299')
  expect_output(str(tail(test_df, 1)), 'var_to_rep: chr "Post"')
  }
)
######################

######################
print("Function being tested: epi_write")

test_that("Test expected output after epi_write", {
  # epi_write(df, '') # write to stdout with data.table
  expect_output(epi_write(df, ''), 'var_id	var_to_rep	x	y	z')
  expect_output(epi_write(df, ''), '0.585528817843856')
  expect_output(epi_write(df, ''), '10	Post	0.298723699267293	0	1')
  }
)
######################

######################
print("Function being tested: epi_head_and_tail")

test_that("Test expected output after epi_head_and_tail", {
  # epi_head_and_tail(df, rows = 2, cols = 2)
  # epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)
  expect_output(epi_head_and_tail(df, rows = 2, cols = 2), '2       1       Post')
  expect_output(epi_head_and_tail(df, rows = 2, cols = 2), '20     10       Post')
  expect_output(epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE), '1  0.5855288 1 3')
  expect_output(epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE), '20 0.2987237 0 1')
  }
  )
######################

######################
print("Function being tested: epi_list_head and epi_list_tail")

test_that("Test expected output after epi_list_head and epi_list_tail", {
  # epi_list_head(as.list(df), 5, 4)
  # epi_list_tail(as.list(df), 5, 4)
  expect_output(epi_list_head(as.list(df), 5, 4), 'List has 5 elements in total.')
  expect_output(epi_list_head(as.list(df), 5, 4), 'Levels: Post Pre')
  expect_output(epi_list_tail(as.list(df), 5, 4), 'Last 5 rows of first 4 elements in list: ')
  expect_output(epi_list_tail(as.list(df), 5, 4), '0 1 0 0 0')
  }
)
######################

######################
print("Function being tested: epi_output_name")

test_that("Test expected output after epi_output_name", {
  str_output <- epi_output_name(input_name = input_file, suffix = '.last_dot_only')
  # str(str_output)
  expect_output(str(str_output), 'df.last_dot_only')
  expect_equal(str_output, 'df.last_dot_only')
  }
)
######################
