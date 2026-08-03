context("project template contract tests")

library(testthat)
library(episcout)

template_files <- c(
  "README.md",
  "metadata/data_dictionary.csv",
  "config/eda.yml",
  "_targets.R",
  "reports/eda.qmd",
  "R/project-derivations.R"
)

expect_template_files <- function(root) {
  for (entry in template_files) {
    expect_true(
      file.exists(file.path(root, entry)),
      info = paste("Missing project template entry:", entry)
    )
  }
}

expect_project_directories <- function(root) {
  for (entry in c("data", "outputs")) {
    expect_true(
      dir.exists(file.path(root, entry)),
      info = paste("Missing project template directory:", entry)
    )
    expect_false(file.exists(file.path(root, entry, ".gitkeep")))
  }
}

read_project_report_template <- function(root) {
  paste(
    readLines(file.path(root, "reports", "eda.qmd"), warn = FALSE),
    collapse = "\n"
  )
}

read_project_targets <- function(root) {
  paste(
    readLines(file.path(root, "_targets.R"), warn = FALSE),
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
  expect_template_files(template_path)
  expect_match(read_project_report_template(template_path), "epi_eda_render_report")
  expect_match(read_project_targets(template_path), "epi_eda_render_report")
  expect_no_match(read_project_report_template(template_path), "\\brender_eda_report\\b")
  expect_no_match(read_project_targets(template_path), "\\brender_eda_report\\b")
})

test_that("epi_eda_create_project creates the expected scaffold", {
  project_path <- tempfile("episcout-project-")

  returned_path <- epi_eda_create_project(project_path)

  expect_equal(
    normalizePath(returned_path, winslash = "/", mustWork = TRUE),
    normalizePath(project_path, winslash = "/", mustWork = TRUE)
  )
  expect_template_files(project_path)
  expect_project_directories(project_path)
  expect_match(read_project_report_template(project_path), "epi_eda_render_report")
  expect_match(read_project_targets(project_path), "epi_eda_render_report")
})

test_that("epi_eda_create_project refuses to overwrite existing files by default", {
  project_path <- tempfile("episcout-project-existing-")
  dir.create(file.path(project_path, "metadata"), recursive = TRUE)
  existing_file <- file.path(project_path, "metadata", "data_dictionary.csv")
  writeLines("existing", existing_file)

  expect_error(
    epi_eda_create_project(project_path),
    regexp = "overwrite|exist"
  )
  expect_equal(readLines(existing_file, warn = FALSE), "existing")
})

test_that("epi_eda_create_project can overwrite existing files when requested", {
  project_path <- tempfile("episcout-project-overwrite-")
  dir.create(file.path(project_path, "metadata"), recursive = TRUE)
  existing_file <- file.path(project_path, "metadata", "data_dictionary.csv")
  writeLines("existing", existing_file)

  returned_path <- epi_eda_create_project(project_path, overwrite = TRUE)

  expect_equal(
    normalizePath(returned_path, winslash = "/", mustWork = TRUE),
    normalizePath(project_path, winslash = "/", mustWork = TRUE)
  )
  expect_template_files(project_path)
  expect_project_directories(project_path)
  expect_false(identical(readLines(existing_file, warn = FALSE), "existing"))
})

test_that("epi_eda_create_project rejects files where scaffold directories belong", {
  project_path <- tempfile("episcout-project-directory-collision-")
  dir.create(project_path)
  writeLines("existing", file.path(project_path, "data"))

  expect_error(
    epi_eda_create_project(project_path),
    regexp = "cannot be used as directories"
  )
  expect_equal(readLines(file.path(project_path, "data"), warn = FALSE), "existing")
  expect_false(dir.exists(file.path(project_path, "outputs")))
})
