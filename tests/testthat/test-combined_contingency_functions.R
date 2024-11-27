# test_combined_contingency_functions.R

library(testthat)
library(dplyr)
library(tidyr)

# Sample data for testing
set.seed(42)
test_data <- data.frame(
  State = sample(c("Active", "Inactive"), 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Something = sample(c("Yes", "No"), 100, replace = TRUE),
  Another = sample(c("Yes", "No", "Unknown"), 100, replace = TRUE),
  Type = sample(c("Type1", "Type2", "Type3"), 100, replace = TRUE),
  SingleValue = rep("Same", 100)
)

# Define expected variables
state_levels <- c("Active", "Inactive")
gender_levels <- c("Male", "Female")

# ===================================================================
# TESTS FOR epi_stats_contingency_df
# ===================================================================

# Verifies the function returns a data frame with correct column names and levels.

test_that("epi_stats_contingency_df produces correct structure and counts", {
  result <- epi_stats_contingency_df(test_data, x_var = "State", y_var = "Gender")

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)  # Two variables + Frequency column
  expect_equal(colnames(result), c("State", "Gender", "Freq"))
  expect_true(all(result$State %in% state_levels))
  expect_true(all(result$Gender %in% gender_levels))
})

# ===================================================================
# TESTS FOR epi_stats_contingency_tables
# ===================================================================

# Checks that the function produces a list of data frames with expected structure.

test_that("epi_stats_contingency_tables produces a list of data frames", {
  result <- epi_stats_contingency_tables(test_data, x_var = "State")

  expect_true(is.list(result))
  expect_true(all(sapply(result, is.data.frame)))
  expect_named(result, setdiff(colnames(test_data), "State"))
  expect_equal(colnames(result[["Gender"]]), c("State", "Gender", "Freq"))
})

# ===================================================================
# TESTS FOR epi_stats_rename_contingency_cols
# ===================================================================

# Ensures that dependent variable columns are renamed correctly in the contingency list.

test_that("epi_stats_rename_contingency_cols renames dependent variable columns", {
  contingency_list <- epi_stats_contingency_tables(test_data, x_var = "State")
  renamed_list <- epi_stats_rename_contingency_cols(contingency_list, test_data, x_var = "State")

  expect_true(is.list(renamed_list))
  expect_true(all(sapply(renamed_list, is.data.frame)))
  expect_equal(colnames(renamed_list[["Gender"]]), c("State", "Gender", "Freq"))
})

# ===================================================================
# TESTS FOR epi_stats_table
# ===================================================================

# Validates that the summary table has wide format, includes totals, and calculates percentages correctly.

test_that("epi_stats_table generates a wide-format summary", {
  result <- epi_stats_table(test_data, dep_var = "State", ind_vars = "Gender")

  expect_true(is.data.frame(result))
  expect_true(all(c("Active", "Inactive") %in% colnames(result)))
  expect_true(all(c("total", "perc_Active", "perc_Inactive") %in% colnames(result)))
  expect_equal(sum(result$total), 100)  # Total counts should match dataset
})

# ===================================================================
# TESTS FOR epi_stats_2x2_test
# ===================================================================

# Tests the statistical output structure (broom's tidy format).

test_that("epi_stats_2x2_test performs a statistical test", {
  result <- epi_stats_2x2_test(test_data, target_var = "State", other_var = "Gender")

  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 7)  # Typical broom::tidy output columns
  expect_true("p.value" %in% colnames(result))
})

# ===================================================================
# TESTS FOR epi_stats_2x2_cols
# ===================================================================

# Ensures valid columns (categorical with at least 2 unique values) are selected.

test_that("epi_stats_2x2_cols selects valid testable columns", {
  result <- epi_stats_2x2_cols(test_data)

  expect_true(is.character(result))
  expect_true("Gender" %in% result)  # Gender is categorical with >=2 unique values
  expect_false("SingleValue" %in% result)  # SingleValue has only 1 unique value
})

# ===================================================================
# TESTS FOR epi_stats_2x2_all
# ===================================================================

# Checks that tests are performed on all valid columns and invalid ones are excluded.

test_that("epi_stats_2x2_all performs tests on all valid columns", {
  result <- epi_stats_2x2_all(test_data, target_var = "State")

  expect_true(is.data.frame(result))
  expect_true(all(c("estimate", "p.value", "variable") %in% colnames(result)))
  expect_true("Gender" %in% result$variable)
  expect_false("SingleValue" %in% result$variable)  # SingleValue should not be included
})
