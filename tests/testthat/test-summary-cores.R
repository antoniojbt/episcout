context("shared univariate statistics behaviour")

library(testthat)
library(episcout)

test_that("numeric wrappers exclude non-finite values from calculations", {
  expected_names <- names(epi_stats_numeric(c(1, 2, NA)))
  observed <- epi_stats_numeric(c(1, 2, Inf, -Inf, NA))

  expect_named(observed, expected_names)
  expect_equal(observed$mean, 1.5)
  expect_equal(observed$min, 1)
  expect_equal(observed$max, 2)
  expect_equal(observed$outlier_count, 0L)
  expect_equal(epi_stats_numeric(c(1, 2, 100), coef = 0)$outlier_count, 0L)
  expect_true(all(is.finite(unlist(observed[c("sum", "min", "mean", "max")]))))
  expect_error(epi_stats_numeric(c("1", "2")), "numeric")
})

test_that("constant and missing numeric inputs keep typed unavailable statistics", {
  constant <- epi_stats_numeric(c(2, 2, 2, 2))
  missing <- epi_stats_numeric(c(NA_real_, NaN))

  expect_true(is.na(constant$skewness))
  expect_true(is.na(constant$kurtosis))
  expect_true(is.na(constant$Shapiro_Wilk_p_value))
  expect_equal(missing$n_nonNA, 0L)
  expect_equal(missing$sum, 0)
  expect_true(is.na(missing$mean))
})

test_that("character and factor wrappers retain variable rows for zero rows", {
  chars <- epi_stats_chars(data.frame(x = character()))
  factors <- epi_stats_factors(data.frame(x = factor(character(), levels = c("A", "B"))))

  expect_equal(chars$Variable, "x")
  expect_equal(chars$n_missing, 0L)
  expect_true(is.na(chars$complete_rate))
  expect_true(is.na(chars$min_length))
  expect_equal(factors$Variable, "x")
  expect_equal(factors$n_missing, 0L)
  expect_true(is.na(factors$complete_rate))
  expect_equal(factors$n_unique, 0L)
  expect_equal(factors$top_counts, "")
})

test_that("factor tables retain declared unused levels and distinguish missing", {
  data <- data.frame(x = factor(c("A", NA), levels = c("A", "B", "NA")))
  observed <- epi_stats_fct_table(data)

  expect_equal(observed$level, c("A", "B", "NA", NA_character_))
  expect_equal(observed$count, c(1L, 0L, 0L, 1L))
})

test_that("date wrappers are stable for empty and all-missing inputs", {
  expected_statistics <- c("N", "N Missing", "N Unique", "Min", "25%", "Median", "75%", "Max", "IQR", "Most Common", "Range (Days)")
  empty <- expect_silent(epi_stats_dates(as.Date(character())))
  missing <- expect_silent(epi_stats_dates(as.Date(c(NA, NA))))
  datetime <- expect_silent(epi_stats_dates(as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 00:00:01"), tz = "UTC")))

  expect_equal(empty$Statistic, expected_statistics)
  expect_equal(empty$Value[1:3], c("0", "0", "0"))
  expect_true(all(is.na(empty$Value[4:11])))
  expect_equal(missing$Value[1:3], c("2", "2", "0"))
  expect_equal(datetime$Value[4], "2020-01-01T00:00:00Z")
  expect_equal(datetime$Value[8], "2020-01-01T00:00:01Z")

  multi <- epi_stats_dates_multi(data.frame(date = as.Date(character()), datetime = as.POSIXct(character())))
  expect_equal(multi$Column, c("date", "datetime"))
})

test_that("date wrappers retain IDate support when data.table is available", {
  skip_if_not_installed("data.table")
  values <- data.table::as.IDate(c("2020-01-01", "2020-01-03"))

  observed <- epi_stats_dates(values)

  expect_equal(observed$Value[4], "2020-01-01")
  expect_equal(observed$Value[8], "2020-01-03")
})
