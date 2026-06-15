context("fixture-backed EDA specification tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

test_that("blood_storage_spec.csv loads as an EDA specification", {
  raw_spec <- read.csv(spec_path, check.names = FALSE, stringsAsFactors = FALSE)

  spec <- epi_eda_spec(spec_path)

  expect_s3_class(spec, "data.frame")
  expect_equal(spec$name, raw_spec$name)
  expect_true(all(c("name", "label", "type", "role") %in% names(spec)))
})

test_that("blood_storage_spec.csv has required specification columns", {
  spec <- epi_eda_spec(spec_path)

  expect_true(all(c("name", "label", "type", "role") %in% names(spec)))
})

test_that("blood_storage specification variable names are unique", {
  spec <- epi_eda_spec(spec_path)

  expect_equal(anyDuplicated(spec$name), 0L)
})

test_that("invalid specification type fails clearly", {
  spec <- read.csv(spec_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec$type[1] <- "unsupported_type"

  expect_error(
    epi_eda_validate_spec(spec),
    regexp = "[Tt]ype|[Uu]nsupported|[Ii]nvalid"
  )
})

test_that("duplicate specification variable name fails clearly", {
  spec <- read.csv(spec_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec$name[2] <- spec$name[1]

  expect_error(
    epi_eda_validate_spec(spec),
    regexp = "[Dd]uplicate|[Uu]nique|[Nn]ame"
  )
})
