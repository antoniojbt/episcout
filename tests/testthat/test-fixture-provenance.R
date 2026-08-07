context("external fixture provenance")

library(testthat)

fixture_sha256 <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(openssl::sha256(connection))
}

verify_fixture_family <- function(directory) {
  checksum_path <- file.path(directory, "CHECKSUMS.sha256")
  expect_true(file.exists(checksum_path))

  lines <- readLines(checksum_path, warn = FALSE)
  records <- strcapture(
    "^([0-9a-f]{64})  (.+)$",
    lines,
    proto = list(sha256 = character(), file = character())
  )
  expect_equal(nrow(records), length(lines))
  expect_false(anyNA(records))
  expect_false(anyDuplicated(records$file) > 0L)

  expected_files <- sort(list.files(directory))
  expected_files <- setdiff(expected_files, "CHECKSUMS.sha256")
  expect_identical(sort(records$file), expected_files)

  for (i in seq_len(nrow(records))) {
    path <- file.path(directory, records$file[[i]])
    expect_identical(fixture_sha256(path), records$sha256[[i]])
  }
}

test_that("fixture families pass their offline checksum manifests", {
  verify_fixture_family(file.path("fixtures", "blood_storage"))
  verify_fixture_family(file.path("fixtures", "penguins_raw"))
})

test_that("blood_storage records immutable source and redistribution evidence", {
  directory <- file.path("fixtures", "blood_storage")
  source <- paste(
    readLines(file.path(directory, "SOURCE.md"), warn = FALSE),
    collapse = "\n"
  )
  licence <- readLines(
    file.path(directory, "LICENSE.medicaldata"),
    warn = FALSE
  )

  expect_match(source, "medicaldata_0.2.0.tar.gz", fixed = TRUE)
  expect_match(source, "56dab0c6078e6f9a9f183427a4481c5497e5d107b795bf965cc7ce4ac4c39236", fixed = TRUE)
  expect_match(source, "e3a1c6b83de9ddae8380ef2a92ce995fe927c5a176c589039d8b6089dae812b9", fixed = TRUE)
  expect_match(source, "Licence: MIT", fixed = TRUE)
  expect_match(source, "No row, column or value is transformed or excluded.", fixed = TRUE)
  expect_identical(
    licence,
    c("YEAR: 2021", "COPYRIGHT HOLDER: medicaldata authors")
  )
})

test_that("penguins_raw records immutable source and CC0 evidence", {
  directory <- file.path("fixtures", "penguins_raw")
  source <- paste(
    readLines(file.path(directory, "SOURCE.md"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(source, "palmerpenguins_0.1.1.tar.gz", fixed = TRUE)
  expect_match(source, "2a40d48ba6c7978fdf2a6daf647ccb39cd17590680138931d11194d3dd1a30b4", fixed = TRUE)
  expect_match(source, "a634e85f0676c74c4cd73f94ff8cbf9ec12540d01797434cf1fd0ba8d9af663f", fixed = TRUE)
  expect_match(source, "Licence: CC0", fixed = TRUE)
  expect_match(source, "No row, column or value is transformed or excluded.", fixed = TRUE)
})
