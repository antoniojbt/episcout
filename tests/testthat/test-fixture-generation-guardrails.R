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

test_that("fixture regeneration pins sources and fails closed before replacement", {
  generator_path <- find_fixture_generator()
  skip_if(is.na(generator_path), "data-raw is not included in the built package")

  source <- paste(readLines(generator_path, warn = FALSE), collapse = "\n")
  required_evidence <- c(
    "medicaldata_0.2.0.tar.gz",
    "56dab0c6078e6f9a9f183427a4481c5497e5d107b795bf965cc7ce4ac4c39236",
    "palmerpenguins_0.1.1.tar.gz",
    "2a40d48ba6c7978fdf2a6daf647ccb39cd17590680138931d11194d3dd1a30b4",
    "e3a1c6b83de9ddae8380ef2a92ce995fe927c5a176c589039d8b6089dae812b9",
    "a634e85f0676c74c4cd73f94ff8cbf9ec12540d01797434cf1fd0ba8d9af663f",
    "verify_serialized_fixture",
    "write_fixture_checksums"
  )

  for (evidence in required_evidence) {
    expect_match(source, evidence, fixed = TRUE)
  }

  preflight_position <- regexpr(
    "verify_serialized_fixture\\(\\n  source_objects\\$penguins_raw",
    source,
    perl = TRUE
  )[[1]]
  replacement_position <- regexpr(
    "write_verified_csv\\(\\n  blood_storage",
    source,
    perl = TRUE
  )[[1]]
  expect_gt(preflight_position, 0L)
  expect_gt(replacement_position, preflight_position)
})
