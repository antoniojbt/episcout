context("episcout cleaning and utility")

library(episcout)
library(testthat)

# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/episcout/tests/testthat/')

# Function being tested: epi_read
print('Function being tested: epi_read')

test_that("Test expected output after epi_read", {
	test_df <- epi_read('../../inst/extdata/df.tsv') # relative to tests/testthat/
	# str(dim(test_df))
	# str(head(test_df, 1))
	expect_output(str(dim(test_df)), '20 5')
	expect_output(str(dim(mtcars)), '32 11')
	expect_output(str(head(test_df, 1)), 'var_id    : int 1')
	expect_output(str(head(test_df, 1)), 'var_to_rep: chr "Pre"')
	expect_output(str(head(test_df, 1)), 'x         : num 0.435')
	expect_output(str(tail(test_df, 1)), 'x         : num -1.82')
	expect_output(str(tail(test_df, 1)), 'var_to_rep: chr "Post"')
	}
)

# # TO DO: add tests
# # "Function being tested: epi_write"
#
# test_that("Test expected writing", {
# 	epi_write(df, 'tests/simple_df_test_set.tsv')
# }
# )
#
# epi_head_and_tail(df, rows = 2, cols = 2)
# epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)
#
# epi_list_head(as.list(df), 5, 4)
# epi_list_tail(as.list(df), 5, 4)

# # Get all duplicates:
# check_dups <- epi_get_all_dups(df, 'var_id', 1)
# dim(check_dups)
# check_dups

