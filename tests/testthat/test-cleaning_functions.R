context("episcout cleaning and utility")

######################
library(episcout)
library(testthat)
######################

# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")

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
# TO DO: continue here
# print("Function being tested: epi_XX_XX")
#
# test_that("Test expected output after epi_XX_XX", {
#   # output is silent if successful
#   # matches values, attributes, and type:
#   expect_identical(func_test, expected_result)
#   # matches values and attributes, adjust with tolerance parameter:
#   expect_equal(func_test, expected_result)
#   expect_equal(2 * 2, 4)
#   # matches values only:
#   expect_equivalent(funct_test, verbatim_screen_output)
#   # match (partial) string output:
#   expect_output(str(dim(an_object), expected_result_as_string))
#   # match a more complex output stored in a file:
#   expect_output_file(str(airquality), "airq.txt", update = TRUE) # create a file
#   expect_output_file(str(airquality), "airq.txt") # compare the contents
#   # test non-Exported functions (triple colon):
#   expect_equal(my_pkg:::func_not_exported(airquality$Ozone), 37)
#   }
#   )
######################
