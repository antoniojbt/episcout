context("versioned EDA summary contracts")

library(testthat)
library(episcout)

make_v2_fixture <- function() {
  data <- data.frame(
    check.names = FALSE,
    "numeric value" = c(1, 2, Inf, 999, NA),
    integer_value = c(1L, 2L, 3L, NA, 5L),
    category = factor(c("A", "B", "D", NA, "A"), levels = c("A", "B", "D")),
    binary = c("Yes", "No", "Yes", NA, "Yes"),
    text_value = c("abc", "", "  ", NA, "z"),
    date_value = c("2020-01-01", "2020-01-03", NA, "2020-01-05", "2020-01-07"),
    datetime_value = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-01 00:00:01", NA, "2020-01-01 00:00:03", "2020-01-01 00:00:04"),
      tz = "America/Mexico_City"
    )
  )
  spec <- data.frame(
    name = c("numeric value", "integer_value", "category", "binary", "text_value", "date_value", "datetime_value", "missing_variable"),
    label = c("Numeric", "Integer", "Category", "Binary", "Text", "Date", "Datetime", "Missing"),
    type = c("numeric", "integer", "categorical", "binary", "text", "date", "datetime", "text"),
    role = c("outcome", rep("covariate", 7)),
    levels = c("", "", "A;B;C", "No;Yes", "", "", "", ""),
    missing_codes = c("999", "", "", "", "", "", "", ""),
    stringsAsFactors = FALSE
  )
  list(data = data, spec = spec)
}

test_that("v1 remains the exact default summary contract", {
  fixture <- make_v2_fixture()
  default <- epi_eda_profile_summaries(fixture$data, fixture$spec[1:7, ])
  explicit <- epi_eda_profile_summaries(fixture$data, fixture$spec[1:7, ], summary_version = "v1")

  expect_named(default, c("numeric", "categorical"))
  expect_identical(default, explicit)
  expect_named(default$numeric, c("name", "n", "n_missing", "mean", "sd", "median", "min", "max"))
  expect_named(default$categorical, c("name", "level", "n", "p", "p_observed"))
})

test_that("v2 covers all specified types and documents skips", {
  fixture <- make_v2_fixture()
  observed <- epi_eda_profile_summaries(fixture$data, fixture$spec, summary_version = "v2")

  expect_named(observed, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_named(observed$variables, c("name", "label", "type", "role", "n", "n_missing", "n_observed", "n_unique", "n_infinite", "status", "reason"))
  expect_equal(observed$variables$name, fixture$spec$name)
  expect_equal(observed$variables$status, c(rep("summarised", 7), "skipped"))
  expect_match(observed$variables$reason[8], "not found|missing", ignore.case = TRUE)
  expect_equal(observed$skipped$name, "missing_variable")

  numeric_row <- observed$numeric[observed$numeric$name == "numeric value", ]
  expect_equal(numeric_row$n_finite, 2L)
  expect_equal(numeric_row$sum, 3)
  expect_equal(numeric_row$mean, 1.5)
  expect_equal(numeric_row$min, 1)
  expect_equal(numeric_row$max, 2)
  expect_equal(observed$variables$n_infinite[1], 1L)

  category <- observed$categorical[observed$categorical$name == "category", ]
  expect_equal(category$level, c("A", "B", "C", "D"))
  expect_equal(category$n, c(2L, 1L, 0L, 1L))
  expect_equal(category$is_declared, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(category$is_unexpected, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(category$p_total, c(0.4, 0.2, 0, 0.2))
  expect_equal(category$p_observed, c(0.5, 0.25, 0, 0.25))

  text <- observed$text[observed$text$name == "text_value", ]
  expect_equal(text$n_missing, 1L)
  expect_equal(text$n_empty, 1L)
  expect_equal(text$n_whitespace, 1L)
  expect_equal(text$min_length, 0L)
  expect_equal(text$max_length, 3L)

  date <- observed$temporal[observed$temporal$name == "date_value", ]
  expect_equal(date$min, "2020-01-01")
  expect_equal(date$max, "2020-01-07")
  expect_equal(date$range_value, 6)
  expect_equal(date$range_unit, "days")

  datetime <- observed$temporal[observed$temporal$name == "datetime_value", ]
  expect_equal(datetime$timezone, "America/Mexico_City")
  expect_match(datetime$min, "Z$")
  expect_equal(datetime$range_value, 4)
  expect_equal(datetime$range_unit, "seconds")
})

test_that("v2 returns typed empty components and rejects invalid versions", {
  data <- data.frame(x = numeric())
  spec <- data.frame(name = "x", label = "X", type = "numeric", role = "covariate")
  observed <- epi_eda_profile_summaries(data, spec, summary_version = "v2")

  expect_equal(observed$variables$n, 0L)
  expect_equal(observed$variables$n_observed, 0L)
  expect_equal(observed$numeric$n_finite, 0L)
  expect_equal(observed$numeric$sum, 0)
  expect_true(is.na(observed$numeric$mean))
  expect_equal(nrow(observed$skipped), 0L)
  expect_error(epi_eda_profile_summaries(data, spec, summary_version = "v3"), "summary_version|arg")
})

test_that("v2 keeps stable all-missing and zero-denominator contracts", {
  data <- data.frame(
    numeric_value = c(NA_real_, NaN),
    category = factor(c(NA, NA), levels = c("A", "B")),
    date_value = as.Date(c(NA, NA)),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("numeric_value", "category", "date_value"),
    label = c("Numeric", "Category", "Date"),
    type = c("numeric", "categorical", "date"),
    role = rep("covariate", 3)
  )
  observed <- epi_eda_profile_summaries(data, spec, summary_version = "v2")

  expect_equal(observed$numeric$n_finite, 0L)
  expect_equal(observed$numeric$sum, 0)
  expect_true(is.na(observed$numeric$shapiro_p))
  expect_equal(observed$categorical$level, c("A", "B"))
  expect_equal(observed$categorical$n, c(0L, 0L))
  expect_true(all(is.na(observed$categorical$p_observed)))
  expect_true(is.na(observed$temporal$min))
  expect_true(is.na(observed$temporal$range_value))
})

test_that("v2 parses ISO datetimes and supports POSIXlt", {
  data <- data.frame(
    iso = c("2020-01-01T00:00:00Z", "2020-01-01T00:00:02Z"),
    stringsAsFactors = FALSE
  )
  data$posixlt <- as.POSIXlt(c("2020-01-01 00:00:00", "2020-01-01 00:00:03"), tz = "UTC")
  spec <- data.frame(
    name = c("iso", "posixlt"),
    label = c("ISO", "POSIXlt"),
    type = c("datetime", "datetime"),
    role = c("covariate", "covariate")
  )
  observed <- epi_eda_profile_summaries(data, spec, summary_version = "v2")

  expect_equal(observed$temporal$min, c("2020-01-01T00:00:00Z", "2020-01-01T00:00:00Z"))
  expect_equal(observed$temporal$range_value, c(2, 3))
  expect_equal(observed$temporal$range_unit, c("seconds", "seconds"))
})

test_that("invalid temporal values and incompatible classes are explicit skips", {
  data <- data.frame(bad_date = c("2020-01-01", "not-a-date"), bad_number = c("1", "2"))
  spec <- data.frame(
    name = c("bad_date", "bad_number"),
    label = c("Bad date", "Bad number"),
    type = c("date", "numeric"),
    role = c("covariate", "covariate")
  )
  observed <- epi_eda_profile_summaries(data, spec, summary_version = "v2")

  expect_equal(observed$variables$status, c("skipped", "skipped"))
  expect_setequal(observed$skipped$name, c("bad_date", "bad_number"))
  expect_match(paste(observed$skipped$reason, collapse = " "), "invalid|incompatible", ignore.case = TRUE)
})

test_that("typed epi_stats_summary is additive and uses global sentinel codes", {
  data <- data.frame(
    numeric_value = c(1, 999, NA),
    category = factor(c("A", "B", NA), levels = c("A", "B", "C")),
    text_value = c("x", "", NA),
    date_value = as.Date(c("2020-01-01", NA, "2020-01-03")),
    stringsAsFactors = FALSE
  )
  current <- epi_stats_summary(data, class_type = "int_num", action = "exclude")
  typed <- epi_stats_summary(data, codes = 999, output = "typed")

  expect_s3_class(current, "tbl_df")
  expect_named(typed, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_equal(typed$variables$label, typed$variables$name)
  expect_true(all(is.na(typed$variables$role)))
  expect_equal(typed$numeric$n_finite, 1L)
  expect_equal(typed$numeric$mean, 1)
  expect_equal(typed$categorical$level, c("A", "B", "C"))
  expect_equal(typed$categorical$n, c(1L, 1L, 0L))
  expect_error(epi_stats_summary(data, output = "typed", action = "codes_only"), "exclude")
})

test_that("typed epi_stats_summary records unsupported columns", {
  data <- data.frame(x = 1:2)
  data$list_column <- I(list(1, 2))

  observed <- epi_stats_summary(data, output = "typed")

  expect_equal(observed$variables$status, c("summarised", "skipped"))
  expect_equal(observed$skipped$name, "list_column")
  expect_match(observed$skipped$reason, "unsupported", ignore.case = TRUE)
})

test_that("v2 runs write every deterministic summary table", {
  fixture <- make_v2_fixture()
  output_dir <- tempfile("eda-v2-")
  dir.create(output_dir)

  observed <- epi_eda_run(fixture$data, fixture$spec, output_dir = output_dir, summary_version = "v2")
  expected <- paste0(
    "summary_",
    c("variables", "numeric", "categorical", "text", "temporal", "skipped"),
    ".csv"
  )

  expect_true(all(file.exists(file.path(output_dir, expected))))
  expect_equal(names(utils::read.csv(file.path(output_dir, "summary_variables.csv"), check.names = FALSE)), names(observed$summaries$variables))
  expect_equal(names(utils::read.csv(file.path(output_dir, "summary_temporal.csv"), check.names = FALSE)), names(observed$summaries$temporal))
})
