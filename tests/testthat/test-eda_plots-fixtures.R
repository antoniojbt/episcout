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
