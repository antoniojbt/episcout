context("episcout stats function tests")

######################
library(testthat)
library(data.table) # Ensure data.table is available for IDate
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")
######################

######################
# Set a test set:

# Descriptive stats for dates
# Range, min, max,etc
# Example data

# test_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01", "2021-07-15"))
# Set seed for reproducibility
set.seed(42)

# Define the start and end dates
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-12-31")

# Calculate the difference in days between start and end dates
days_between <- as.integer(end_date - start_date)
days_between

# Generate 300 random numbers within the range of days
random_days <- sample(0:days_between, 300, replace = TRUE)

# Add these random days to the start date to get random dates
random_dates <- start_date + random_days
random_dates

# Sort the dates
sorted_random_dates <- sort(random_dates)
sorted_random_dates

# Convert to character for display or further processing
sorted_random_dates_str <- as.character(sorted_random_dates)
sorted_random_dates_str

# Print the first 10 sorted random dates
sorted_random_dates_str[1:10]
str(sorted_random_dates_str)
sorted_random_dates_str <- as.Date(sorted_random_dates_str)
str(sorted_random_dates_str)

test_dates <- sorted_random_dates_str
######################

######################
# Test 1: Correct output for a simple Date input
print("Function being tested: epi_stats_dates")
test_that("Test with regular Date inputs", {
  # test_dates <- as.Date(c("2020-01-01", "2020-06-01", "2020-12-31"))
  result <- epi_stats_dates(test_dates)
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 11)
  expect_equal(
    result$Value[result$Statistic == "Min"],
    as.character(min(test_dates))
  )
  expect_equal(
    result$Value[result$Statistic == "Max"],
    as.character(max(test_dates))
  )
})

# Test 2: Correct handling of IDate input
test_that("Test with IDate inputs", {
  test_dates <- data.table::as.IDate(as.Date(c("2021-01-01", "2021-06-01", "2021-12-31")))
  result <- epi_stats_dates(test_dates)
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 11)
  expect_equal(
    result$Value[result$Statistic == "Min"],
    as.character(min(as.Date(test_dates)))
  )
  expect_equal(
    result$Value[result$Statistic == "Max"],
    as.character(max(as.Date(test_dates)))
  )
})

# Test 3: Function stops if non-date input is provided
test_that("Test with non-date input throws error", {
  non_date_input <- c(1, 2, 3)
  expect_error(epi_stats_dates(non_date_input))
})

# Test 4: Handling of NA values in date inputs
test_that("Test handling of NA values", {
  test_dates_with_na <- as.Date(c("2020-01-01", NA, "2020-12-31"))
  result <- epi_stats_dates(test_dates_with_na)
  expect_is(result, "data.frame")
  expect_equal(nrow(result), 11)
  # Expect NA not to affect the calculation of min, max, etc.
  expect_equal(
    result$Value[result$Statistic == "Min"],
    as.character(min(test_dates_with_na, na.rm = TRUE))
  )
})
######################

# Test 5: epi_stats_dates_multi summarises multiple date columns
test_that("epi_stats_dates_multi summarises multiple date columns", {
  df <- data.frame(
    date1 = as.Date(c("2020-01-01", "2020-01-02", NA)),
    date2 = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03"))
  )
  result <- epi_stats_dates_multi(df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(
    result$Min[result$Column == "date1"],
    as.character(min(df$date1, na.rm = TRUE))
  )
})

# Test 6: epi_stats_dates_freq computes differences and frequencies
test_that("epi_stats_dates_freq returns date differences and frequencies", {
  dates <- as.Date(c("2020-01-01", "2020-01-03", "2020-02-01"))
  result <- epi_stats_dates_freq(dates)
  expect_type(result, "list")
  expect_equal(result$date_differences, diff(sort(as.numeric(dates))))
  expect_equal(as.integer(result$frequencies["2020-01"]), 2)
  expect_equal(as.integer(result$frequencies["2020-02"]), 1)
})
######################
