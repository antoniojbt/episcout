context("fixture-backed EDA summary tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

make_expected_numeric_summary <- function(data, spec) {
  numeric_spec <- spec[spec$type %in% c("numeric", "integer"), , drop = FALSE]

  rows <- lapply(numeric_spec$name, function(name) {
    values <- data[[name]]
    data.frame(
      name = name,
      n = length(values),
      n_missing = sum(is.na(values)),
      mean = mean(values, na.rm = TRUE),
      sd = stats::sd(values, na.rm = TRUE),
      median = stats::median(values, na.rm = TRUE),
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

make_expected_categorical_summary <- function(data, spec) {
  categorical_spec <- spec[spec$type %in% c("categorical", "binary"), , drop = FALSE]

  rows <- lapply(categorical_spec$name, function(name) {
    values <- data[[name]]
    observed <- as.character(values[!is.na(values)])
    levels <- strsplit(categorical_spec$levels[categorical_spec$name == name], ";", fixed = TRUE)[[1]]
    counts <- vapply(levels, function(level) sum(observed == level), integer(1))

    data.frame(
      name = name,
      level = levels,
      n = as.integer(counts),
      p = as.numeric(counts) / length(values),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

test_that("epi_eda_profile_summaries returns numeric and categorical summary components", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_type(observed, "list")
  expect_named(observed, c("numeric", "categorical"))
  expect_s3_class(observed$numeric, "data.frame")
  expect_s3_class(observed$categorical, "data.frame")
})

test_that("epi_eda_profile_summaries numeric output matches independent fixture calculations", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- make_expected_numeric_summary(data, spec)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(
    as.data.frame(observed$numeric),
    expected,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("epi_eda_profile_summaries categorical output matches independent fixture calculations", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- make_expected_categorical_summary(data, spec)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(
    as.data.frame(observed$categorical),
    expected,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})
