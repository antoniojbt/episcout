context("episcout cleaning and utility")
# library(episcout)
library(testthat)

# Function being tested: epi_write

# Errors:
test_that("Test expected output after epi_read", {
	test_df <- epi_read('inst/extdata/df.tsv')
	# dim(test_df)
	# head(test_df, 1)
	expect_output(dim(test_df), '[1] 20  5')
	expect_output(dim(mtcars), '[1] 32 11')
	expect_output(head(test_df, 1), 'var_id var_to_rep     x     y     z')
	expect_output(head(test_df, 1), 'var_id var_to_rep     x     y     z')
	expect_output(head(test_df, 1), '1      1 Pre        0.435     1     2')
	# str(mtcars)
	# expect_output(str(mtcars), "32 obs")
	}
)

# TO DO: add tests
"Function being tested: epi_writing"

test_that("Test expected writing", {
	epi_write(df, 'tests/simple_df_test_set.tsv')
}
)

epi_head_and_tail(df, rows = 2, cols = 2)
epi_head_and_tail(df, rows = 2, cols = 2, last_cols = TRUE)

epi_list_head(as.list(df), 5, 4)
epi_list_tail(as.list(df), 5, 4)

# # Get all duplicates:
# check_dups <- epi_get_all_dups(df, 'var_id', 1)
# dim(check_dups)
# check_dups

