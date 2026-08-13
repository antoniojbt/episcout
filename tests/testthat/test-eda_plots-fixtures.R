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
    database_type = "text", analysis_type = c("numeric", "categorical"),
    role = c("outcome", "exposure"),
    missing_codes = c("999", "UNK"),
    stringsAsFactors = FALSE
  )

  plots <- epi_eda_profile_plots(data, spec)
  missing <- epi_eda_profile_missing(data, spec)
  expect_equal(missing$n_missing, c(2L, 2L))
  expect_equal(sum(plots$measurement$data$count), 3)
  expect_lt(max(plots$measurement$data$midpoint[plots$measurement$data$count > 0]), 10)
  expect_equal(sort(plots$treatment$data$count), c(1, 2))
  expect_equal(sum(plots$treatment$data$count), 3)
  expect_false(any(c("UNK", "999") %in% unlist(lapply(plots, names))))
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
    database_type = "text", analysis_type = c("date", "datetime"),
    role = c("time", "time"),
    missing_codes = c("MISSING", "MISSING"),
    stringsAsFactors = FALSE
  )

  plots <- epi_eda_profile_plots(data, spec)
  missing <- epi_eda_profile_missing(data, spec)
  expect_equal(missing$n_missing, c(2L, 2L))
  expect_equal(sum(plots$specimen_date$data$count), 2)
  expect_equal(sum(plots$specimen_time$data$count), 2)
  expect_equal(nrow(plots$specimen_date$data), 30L)
  expect_equal(nrow(plots$specimen_time$data), 30L)
  expect_false("value" %in% names(plots$specimen_date$data))
  expect_false("value" %in% names(plots$specimen_time$data))
})

test_that("epi_eda_profile_plots rejects invalid non-missing temporal values", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(specimen_date = c("2024-01-01", "not-a-date"))
  spec <- data.frame(
    name = "specimen_date",
    label = "Specimen date",
    database_type = "text", analysis_type = "date",
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
    database_type = "text", analysis_type = "datetime",
    role = "time",
    stringsAsFactors = FALSE
  )

  plot <- epi_eda_profile_plots(data, spec)$specimen_time
  expect_equal(sum(plot$data$count), 4L)
  expect_identical(
    format(as.POSIXct(min(plot$data$lower), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "2024-01-01T17:30:00Z"
  )
  expect_identical(
    format(as.POSIXct(max(plot$data$upper), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "2024-01-01T20:00:00Z"
  )
})
