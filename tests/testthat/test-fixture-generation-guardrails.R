context("external fixture generation guardrails")

library(testthat)

find_fixture_generator <- function() {
  candidates <- c(
    file.path("data-raw", "test-fixtures", "make_external_fixtures.R"),
    file.path("..", "data-raw", "test-fixtures", "make_external_fixtures.R"),
    file.path("..", "..", "data-raw", "test-fixtures", "make_external_fixtures.R")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) NA_character_ else existing[[1]]
}

test_that("fixture generation does not call the package under test", {
  generator_path <- find_fixture_generator()
  skip_if(is.na(generator_path), "data-raw is not included in the built package")

  source <- paste(readLines(generator_path, warn = FALSE), collapse = "\n")
  forbidden_patterns <- c(
    "library\\s*\\(\\s*['\"]?episcout",
    "require\\s*\\(\\s*['\"]?episcout",
    "requireNamespace\\s*\\(\\s*['\"]episcout",
    "episcout\\s*::",
    "epi_eda_[[:alnum:]_]*\\s*\\("
  )

  for (pattern in forbidden_patterns) {
    expect_false(
      grepl(pattern, source, perl = TRUE),
      info = paste("Forbidden fixture-generator call matched:", pattern)
    )
  }
})
