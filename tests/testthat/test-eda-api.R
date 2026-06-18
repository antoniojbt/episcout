context("EDA API naming tests")

library(testthat)
library(episcout)

find_package_root <- function() {
  candidates <- c(
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  )
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(candidate)
    }
  }
  NA_character_
}

read_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("EDA workflow exports use the epi_eda prefix", {
  exports <- getNamespaceExports("episcout")
  expected <- c(
    "epi_eda_spec",
    "epi_eda_validate_spec",
    "epi_eda_check_schema",
    "epi_eda_profile_missing",
    "epi_eda_profile_summaries",
    "epi_eda_profile_plots",
    "epi_eda_generate_synthetic_data",
    "epi_eda_run",
    "epi_eda_render_report",
    "epi_eda_create_project"
  )
  old <- c(
    "eda_spec",
    "validate_eda_spec",
    "check_schema",
    "profile_missing",
    "profile_summaries",
    "profile_plots",
    "generate_synthetic_data",
    "run_eda",
    "render_eda_report",
    "use_episcout_project"
  )

  expect_true(all(expected %in% exports))
  expect_false(any(old %in% exports))
})

test_that("README and EDA vignette document the epi_eda workflow", {
  root <- find_package_root()
  skip_if(is.na(root), "Package root not available.")

  readme <- read_if_exists(file.path(root, "README.md"))
  vignette <- read_if_exists(file.path(root, "vignettes", "specification-first-eda.Rmd"))
  skip_if(is.null(readme), "README.md not available.")
  skip_if(is.null(vignette), "specification-first-eda vignette not available.")

  expect_match(readme, "epi_eda_run")
  expect_match(readme, "epi_eda_render_report")
  expect_match(readme, "specification-first exploratory data\\s+analysis workflows")
  expect_match(vignette, "epi_eda_spec")
  expect_match(vignette, "epi_eda_run")
  expect_match(vignette, "epi_eda_render_report")
})
