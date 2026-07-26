context("penguins_raw external fixture tests")

library(testthat)
library(episcout)

fixture_dir <- file.path("fixtures", "penguins_raw")
data_path <- file.path(fixture_dir, "penguins_raw.csv")
spec_path <- file.path(fixture_dir, "penguins_raw_spec.csv")
source_path <- file.path(fixture_dir, "SOURCE.md")
expected_schema_path <- file.path(fixture_dir, "expected_schema.csv")
expected_missing_path <- file.path(fixture_dir, "expected_missing.csv")
expected_numeric_path <- file.path(fixture_dir, "expected_summary_numeric.csv")
expected_categorical_path <- file.path(fixture_dir, "expected_summary_categorical.csv")
expected_plot_path <- file.path(fixture_dir, "expected_plot_inventory.csv")

required_fixture_files <- c(
  source_path,
  data_path,
  spec_path,
  expected_schema_path,
  expected_missing_path,
  expected_numeric_path,
  expected_categorical_path,
  expected_plot_path
)

expected_names <- c(
  "studyName", "Sample Number", "Species", "Region", "Island", "Stage",
  "Individual ID", "Clutch Completion", "Date Egg", "Culmen Length (mm)",
  "Culmen Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Sex",
  "Delta 15 N (o/oo)", "Delta 13 C (o/oo)", "Comments"
)

expected_types <- c(
  "categorical", "integer", "categorical", "categorical", "categorical",
  "categorical", "text", "binary", "date", "numeric", "numeric",
  "integer", "integer", "binary", "numeric", "numeric", "text"
)

expected_roles <- c(
  "metadata", "identifier", "covariate", "covariate", "covariate",
  "covariate", "identifier", "covariate", "covariate", "covariate",
  "covariate", "covariate", "covariate", "covariate", "covariate",
  "covariate", "metadata"
)

skip_if_penguins_missing <- function() {
  missing <- required_fixture_files[!file.exists(required_fixture_files)]
  skip_if(length(missing) > 0, "penguins_raw fixture has not been generated")
}

test_that("the complete penguins_raw fixture contract is committed", {
  expect_true(
    all(file.exists(required_fixture_files)),
    info = paste("Missing:", paste(required_fixture_files[!file.exists(required_fixture_files)], collapse = ", "))
  )
})

test_that("penguins_raw provenance is complete", {
  skip_if_penguins_missing()
  source <- paste(readLines(source_path, warn = FALSE), collapse = "\n")

  expect_match(source, "palmerpenguins")
  expect_match(source, "344")
  expect_match(source, "17")
  expect_match(source, "https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html", fixed = TRUE)
  expect_match(source, "10.1371/journal.pone.0090081", fixed = TRUE)
  expect_match(source, "CC0", fixed = TRUE)
  expect_match(
    source,
    "scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R",
    fixed = TRUE
  )
})

test_that("penguins_raw preserves the upstream dimensions and raw names", {
  skip_if_penguins_missing()
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)

  expect_equal(dim(data), c(344L, 17L))
  expect_identical(names(data), expected_names)
  expect_equal(sum(is.na(data)), 336L)
  expect_equal(sum(is.na(data$Sex)), 11L)
  expect_equal(sum(is.na(data$Comments)), 290L)
})

test_that("penguins_raw specification has the reviewed contracts", {
  skip_if_penguins_missing()
  spec <- epi_eda_spec(spec_path)

  expect_identical(spec$name, expected_names)
  expect_identical(spec$type, expected_types)
  expect_identical(spec$role, expected_roles)
  expect_true(all(spec$required))
  expect_true(all(is.na(spec$missing_codes) | trimws(spec$missing_codes) == ""))
})

test_that("penguins_raw schema matches its independent expectation", {
  skip_if_penguins_missing()
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_schema_path, check.names = FALSE, stringsAsFactors = FALSE)

  observed <- epi_eda_check_schema(data, spec)

  expect_equal(as.data.frame(observed), expected, ignore_attr = TRUE)
})

test_that("penguins_raw missingness matches its independent expectation", {
  skip_if_penguins_missing()
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_missing_path, check.names = FALSE, stringsAsFactors = FALSE)

  observed <- epi_eda_profile_missing(data, spec)

  expect_equal(as.data.frame(observed), expected, tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("penguins_raw summaries match their independent expectations", {
  skip_if_penguins_missing()
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected_numeric <- read.csv(expected_numeric_path, check.names = FALSE, stringsAsFactors = FALSE)
  expected_categorical <- read.csv(expected_categorical_path, check.names = FALSE, stringsAsFactors = FALSE)

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(
    as.data.frame(observed$numeric),
    expected_numeric,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
  expect_equal(
    as.data.frame(observed$categorical),
    expected_categorical,
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("penguins_raw v2 summaries cover every specified variable", {
  skip_if_penguins_missing()
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec <- epi_eda_spec(spec_path)

  observed <- epi_eda_profile_summaries(data, spec, summary_version = "v2")

  expect_equal(observed$variables$name, spec$name)
  expect_true(all(observed$variables$status == "summarised"))
  expect_equal(nrow(observed$skipped), 0L)
  expect_setequal(observed$text$name, c("Individual ID", "Comments"))
  expect_equal(observed$temporal$name, "Date Egg")
  expect_equal(observed$temporal$min, "2007-11-09")
  expect_equal(observed$temporal$max, "2009-12-01")
})

test_that("penguins_raw plot inventory proves specification-based dispatch", {
  skip_if_penguins_missing()
  skip_if_not_installed("ggplot2")
  data <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
  spec <- epi_eda_spec(spec_path)
  expected <- read.csv(expected_plot_path, check.names = FALSE, stringsAsFactors = FALSE)

  plots <- epi_eda_profile_plots(data, spec)
  observed <- data.frame(
    name = unname(names(plots)),
    type = spec$type,
    layer_geom = vapply(plots, function(plot) class(plot$layers[[1]]$geom)[1], character(1)),
    layer_stat = vapply(plots, function(plot) class(plot$layers[[1]]$stat)[1], character(1)),
    stringsAsFactors = FALSE
  )
  row.names(observed) <- NULL

  expect_equal(observed, expected, ignore_attr = TRUE)
})
