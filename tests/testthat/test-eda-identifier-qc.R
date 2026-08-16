test_that("identifier QC specifications are explicit and do not carry values", {
  identifiers <- data.frame(
    name = c("primary_id", "secondary_id"),
    expected_length = c(18L, NA_integer_),
    pattern = c("^[A-Z0-9]+$", NA_character_),
    case_sensitive = c(TRUE, FALSE),
    provenance = c("declared_primary", "declared_secondary"),
    stringsAsFactors = FALSE
  )

  spec <- epi_eda_identifier_qc_spec(identifiers)

  expect_identical(spec, identifiers)
  expect_error(
    epi_eda_identifier_qc_spec(transform(identifiers, name = c("id", "id"))),
    "unique"
  )
  expect_error(
    epi_eda_identifier_qc_spec(transform(identifiers, expected_length = c(0L, NA))),
    "positive"
  )
})

test_that("identifier QC report rendering is aggregate-only and no-overwrite", {
  qc <- structure(
    list(
      metadata = data.frame(contract_version = "identifier-qc-1", stringsAsFactors = FALSE),
      identifier_audit = data.frame(
        name = "identifier", n_input = 4, n_nonblank = 3, n_distinct = 2,
        duplicate_excess = 1, stringsAsFactors = FALSE
      ),
      availability_audit = data.frame(pattern = "1", n_rows = 4, stringsAsFactors = FALSE)
    ),
    class = c("epi_eda_identifier_qc", "list")
  )
  root <- tempfile("identifier-qc-report-")
  dir.create(root)
  destination <- file.path(root, "review")

  expect_identical(epi_eda_render_identifier_qc_report(qc, destination), destination)
  expect_true(file.exists(file.path(destination, "index.html")))
  expect_true(file.exists(file.path(destination, "identifier_qc.csv")))
  expect_error(epi_eda_render_identifier_qc_report(qc, destination), "must not already exist")
})
