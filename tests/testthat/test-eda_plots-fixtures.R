context("fixture-backed EDA plot tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
data_path <- file.path(fixture_dir, "blood_storage.csv")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

test_that("epi_eda_profile_plots returns one named plot object per specified variable", {
  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_plots(data, spec)

  expect_type(observed, "list")
  expect_named(observed, spec$name)
  expect_equal(length(observed), nrow(spec))
})

test_that("epi_eda_profile_plots returns ggplot objects without checking visual appearance", {
  skip_if_not_installed("ggplot2")

  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_plots(data, spec)

  expect_true(all(vapply(observed, inherits, logical(1), what = "ggplot")))
})

test_that("epi_eda_profile_plots dispatches numeric and categorical fixture variables", {
  skip_if_not_installed("ggplot2")

  data <- read.csv(data_path, check.names = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_plots(data, spec)

  expect_s3_class(observed$Age, "ggplot")
  expect_s3_class(observed$Units, "ggplot")
  expect_s3_class(observed$RBC.Age.Group, "ggplot")
  expect_s3_class(observed$Recurrence, "ggplot")
})

test_that("epi_eda_profile_plots excludes numeric and categorical sentinels from layers", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    measurement = c(1, 2, 999, 3, NA),
    treatment = c("control", "UNK", "case", "control", NA),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("measurement", "treatment"),
    label = c("Measurement", "Treatment"),
    type = c("numeric", "categorical"),
    role = c("outcome", "exposure"),
    missing_codes = c("999", "UNK"),
    stringsAsFactors = FALSE
  )

  plots <- epi_eda_profile_plots(data, spec)
  missing <- epi_eda_profile_missing(data, spec)
  numeric_layer <- ggplot2::ggplot_build(plots$measurement)$data[[1]]
  categorical_layer <- ggplot2::ggplot_build(plots$treatment)$data[[1]]

  expect_equal(missing$n_missing, c(2L, 2L))
  expect_equal(sum(numeric_layer$count), 3)
  expect_lt(max(numeric_layer$x[numeric_layer$count > 0]), 10)
  expect_equal(sort(categorical_layer$count), c(1, 2))
  expect_equal(sum(categorical_layer$count), 3)
})

test_that("epi_eda_profile_plots masks temporal sentinels before conversion", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    specimen_date = c("2024-01-01", "MISSING", "2024-01-03", NA),
    specimen_time = c("2024-01-01T08:00:00Z", "MISSING", "2024-01-03T10:30:00Z", NA),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("specimen_date", "specimen_time"),
    label = c("Specimen date", "Specimen time"),
    type = c("date", "datetime"),
    role = c("time", "time"),
    missing_codes = c("MISSING", "MISSING"),
    stringsAsFactors = FALSE
  )

  plots <- epi_eda_profile_plots(data, spec)
  missing <- epi_eda_profile_missing(data, spec)
  date_layer <- ggplot2::ggplot_build(plots$specimen_date)$data[[1]]
  datetime_layer <- ggplot2::ggplot_build(plots$specimen_time)$data[[1]]

  expect_equal(missing$n_missing, c(2L, 2L))
  expect_equal(sum(date_layer$count), 2)
  expect_equal(sum(datetime_layer$count), 2)
  expect_equal(length(plots$specimen_date$data$value), 2)
  expect_equal(length(plots$specimen_time$data$value), 2)
})

test_that("epi_eda_profile_plots rejects invalid non-missing temporal values", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(specimen_date = c("2024-01-01", "not-a-date"))
  spec <- data.frame(
    name = "specimen_date",
    label = "Specimen date",
    type = "date",
    role = "time",
    missing_codes = "MISSING",
    stringsAsFactors = FALSE
  )

  expect_error(
    epi_eda_profile_plots(data, spec),
    "invalid non-missing values"
  )
})

test_that("epi_eda_profile_plots preserves ISO-8601 timezone offsets", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    specimen_time = c(
      "2024-01-01T12:00:00-05:30",
      "2024-01-01T18:00:00Z",
      "2024-01-01T19:00:00",
      "2024-01-01 20:00:00"
    )
  )
  spec <- data.frame(
    name = "specimen_time",
    label = "Specimen time",
    type = "datetime",
    role = "time",
    stringsAsFactors = FALSE
  )

  plot <- epi_eda_profile_plots(data, spec)$specimen_time
  instants_utc <- format(plot$data$value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  expect_identical(
    instants_utc,
    c(
      "2024-01-01T17:30:00Z",
      "2024-01-01T18:00:00Z",
      "2024-01-01T19:00:00Z",
      "2024-01-01T20:00:00Z"
    )
  )
})
