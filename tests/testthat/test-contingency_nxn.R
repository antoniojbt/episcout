# test_epi_stats_contingency_nxn

library(testthat)
library(tidyr)

# Sample data for testing
set.seed(42)
test_data <- data.frame(
  Outcome = sample(c("Yes", "No"), 100, replace = TRUE),
  Group = sample(c("A", "B", "C"), 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE)
)

# =====================================================
# TESTS FOR epi_stats_contingency_nxn
# =====================================================

# Single Independent Variable:
# Tests that the function handles a single independent variable correctly.
# Verifies totals, percentages, and column names.

test_that("epi_stats_contingency_nxn handles a single independent variable", {
  result <- epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = "Group")

  # Basic structure checks
  expect_true(is.data.frame(result))
  expect_true(all(c("Yes", "No", "total", "perc_Yes", "perc_No") %in% colnames(result)))

  # Row totals should match the total frequencies in the data
  total_counts <- table(test_data$Group)
  expect_equal(result$total, as.numeric(total_counts))

  # Percentages should sum to 100 for each row
  expect_equal(rowSums(result[, c("perc_Yes", "perc_No")]), rep(100, nrow(result)))
})

# Multiple Independent Variables:
# Ensures the function can handle multiple grouping variables.
# Checks that all combinations of independent variables are included.
# Input Validation:
# Ensures the function raises appropriate errors when variables are missing or invalid.
# Edge Cases:
# Handles scenarios with:
# No rows in the data.
# Only one level in the dependent variable.
# Verifies that the function returns valid outputs for these cases.
# Sorting:
# Confirms that rows are sorted by the percentage of the first level of the dependent variable.

test_that("epi_stats_contingency_nxn handles multiple independent variables", {
  result <- epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = c("Group", "Gender"))

  # Basic structure checks
  expect_true(is.data.frame(result))
  expect_true(all(c("Yes", "No", "total", "perc_Yes", "perc_No") %in% colnames(result)))

  # Check that all combinations of Group and Gender are represented
  combinations <- expand.grid(Group = unique(test_data$Group), Gender = unique(test_data$Gender))
  expect_true(all(paste(result$Group, result$Gender) %in% paste(combinations$Group, combinations$Gender)))

  # Percentages should sum to 100 for each row
  expect_equal(rowSums(result[, c("perc_Yes", "perc_No")]), rep(100, nrow(result)))
})

test_that("epi_stats_contingency_nxn validates input", {
  # Dependent variable not in data
  expect_error(
    epi_stats_contingency_nxn(test_data, dep_var = "Invalid", ind_vars = "Group"),
    "All specified variables must exist in the data frame"
  )

  # Independent variable not in data
  expect_error(
    epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = "Invalid"),
    "All specified variables must exist in the data frame"
  )

  # Non-existent column combination
  expect_error(
    epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = c("Group", "Invalid")),
    "All specified variables must exist in the data frame"
  )
})

test_that("epi_stats_contingency_nxn handles edge cases", {
  # Case with no rows
  empty_data <- test_data[0, ]
  result <- epi_stats_contingency_nxn(empty_data, dep_var = "Outcome", ind_vars = "Group")
  expect_true(nrow(result) == 0)

  # Case with only one level in the dependent variable
  single_level_data <- test_data
  single_level_data$Outcome <- "Yes"
  result <- epi_stats_contingency_nxn(single_level_data, dep_var = "Outcome", ind_vars = "Group")
  expect_true(is.data.frame(result))
  expect_true(all(result$No == 0))
  expect_true(all(result$perc_No == 0))
  expect_true(all(result$perc_Yes == 100))
})

test_that("epi_stats_contingency_nxn sorts by the first dependent variable level", {
  result <- epi_stats_contingency_nxn(test_data, dep_var = "Outcome", ind_vars = "Group")

  # Check if sorting is done by "perc_Yes" in descending order
  expect_equal(result$perc_Yes, sort(result$perc_Yes, decreasing = TRUE))
})
