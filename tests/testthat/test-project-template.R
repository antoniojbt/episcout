context("project template contract tests")

library(testthat)
library(episcout)

expected_project_template_entries <- c(
  "metadata/data_dictionary.csv",
  "config/eda.yml",
  "_targets.R",
  "reports/eda.qmd",
  "R/project-derivations.R",
  "outputs"
)

expect_project_template_entries <- function(root) {
  for (entry in expected_project_template_entries) {
    expect_true(
      file.exists(file.path(root, entry)),
      info = paste("Missing project template entry:", entry)
    )
  }
}

read_project_report_template <- function(root) {
  paste(
    readLines(file.path(root, "reports", "eda.qmd"), warn = FALSE),
    collapse = "\n"
  )
}

test_that("project template is bundled with the expected scaffold", {
  template_path <- system.file("project-template", package = "episcout")

  expect_true(nzchar(template_path))
  if (!nzchar(template_path)) {
    return(invisible())
  }

  expect_true(dir.exists(template_path))
  expect_project_template_entries(template_path)
  expect_match(read_project_report_template(template_path), "render_eda_report")
})

test_that("use_episcout_project creates the expected scaffold", {
  project_path <- tempfile("episcout-project-")

  returned_path <- use_episcout_project(project_path)

  expect_equal(
    normalizePath(returned_path, winslash = "/", mustWork = TRUE),
    normalizePath(project_path, winslash = "/", mustWork = TRUE)
  )
  expect_project_template_entries(project_path)
  expect_match(read_project_report_template(project_path), "render_eda_report")
})

test_that("use_episcout_project refuses to overwrite existing files by default", {
  project_path <- tempfile("episcout-project-existing-")
  dir.create(file.path(project_path, "metadata"), recursive = TRUE)
  existing_file <- file.path(project_path, "metadata", "data_dictionary.csv")
  writeLines("existing", existing_file)

  expect_error(
    use_episcout_project(project_path),
    regexp = "overwrite|exist"
  )
  expect_equal(readLines(existing_file, warn = FALSE), "existing")
})

test_that("use_episcout_project can overwrite existing files when requested", {
  project_path <- tempfile("episcout-project-overwrite-")
  dir.create(file.path(project_path, "metadata"), recursive = TRUE)
  existing_file <- file.path(project_path, "metadata", "data_dictionary.csv")
  writeLines("existing", existing_file)

  returned_path <- use_episcout_project(project_path, overwrite = TRUE)

  expect_equal(
    normalizePath(returned_path, winslash = "/", mustWork = TRUE),
    normalizePath(project_path, winslash = "/", mustWork = TRUE)
  )
  expect_project_template_entries(project_path)
  expect_false(identical(readLines(existing_file, warn = FALSE), "existing"))
})
