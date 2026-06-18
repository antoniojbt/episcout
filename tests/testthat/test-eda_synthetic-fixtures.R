context("fixture-backed EDA synthetic-data tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "blood_storage")
spec_path <- file.path(fixture_dir, "blood_storage_spec.csv")

spec <- epi_eda_spec(spec_path)

split_levels <- function(x) {
  strsplit(x, ";", fixed = TRUE)[[1]]
}

test_that("synthetic data has the same variable names as the specification", {
  synthetic <- epi_eda_generate_synthetic_data(spec, n = 25, seed = 1)

  expect_s3_class(synthetic, "data.frame")
  expect_identical(names(synthetic), spec$name)
})

test_that("synthetic data has the requested row count", {
  synthetic <- epi_eda_generate_synthetic_data(spec, n = 37, seed = 1)

  expect_equal(nrow(synthetic), 37L)
})

test_that("synthetic categorical and binary values respect specification levels", {
  synthetic <- epi_eda_generate_synthetic_data(spec, n = 100, seed = 1)

  categorical_spec <- spec[
    spec$type %in% c("categorical", "binary") &
      !is.na(spec$levels) & spec$levels != "",
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(categorical_spec))) {
    name <- categorical_spec$name[i]
    allowed_levels <- split_levels(categorical_spec$levels[i])
    observed_values <- unique(as.character(stats::na.omit(synthetic[[name]])))

    expect_true(
      all(observed_values %in% allowed_levels),
      info = paste(name, "contains values outside the specification levels")
    )
  }
})

test_that("synthetic numeric and integer values respect specification min and max", {
  synthetic <- epi_eda_generate_synthetic_data(spec, n = 100, seed = 1)

  ranged_spec <- spec[
    spec$type %in% c("numeric", "integer") &
      !is.na(spec$min) & spec$min != "" &
      !is.na(spec$max) & spec$max != "",
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(ranged_spec))) {
    name <- ranged_spec$name[i]
    min_value <- as.numeric(ranged_spec$min[i])
    max_value <- as.numeric(ranged_spec$max[i])
    observed_values <- stats::na.omit(synthetic[[name]])

    expect_true(
      all(observed_values >= min_value & observed_values <= max_value),
      info = paste(name, "contains values outside the specification range")
    )
  }
})

test_that("fixed seed gives identical synthetic output", {
  synthetic_a <- epi_eda_generate_synthetic_data(spec, n = 25, seed = 2024)
  synthetic_b <- epi_eda_generate_synthetic_data(spec, n = 25, seed = 2024)

  expect_identical(synthetic_a, synthetic_b)
})
