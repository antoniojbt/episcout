library(episcout)
library(testthat)

context("PostgreSQL identity-universe metadata")

make_identity_universe_sources <- function() {
  data.frame(
    source_schema = c("source_data", "source_data"),
    source_table = c("cohort_b", "cohort_a"),
    id_column = c("participant_code", "participant_code"),
    identity_namespace = c("participant_codes", "participant_codes"),
    provenance = c("reviewed_synthetic_fixture", "reviewed_synthetic_fixture"),
    validation_status = c("confirmed", "confirmed"),
    stringsAsFactors = FALSE
  )
}

test_that("identity-universe specification is value-free and deterministic", {
  sources <- make_identity_universe_sources()
  spec <- epi_sec_identity_universe_spec(sources, validity_regex = "^[A-Z]$")
  reversed <- epi_sec_identity_universe_spec(sources[2:1, ], validity_regex = "^[A-Z]$")

  expect_s3_class(spec, "epi_sec_identity_universe_spec")
  expect_named(
    spec,
    c(
      "sources", "normalization", "validity_regex", "contract_version",
      "fingerprint_sha256"
    )
  )
  expect_equal(spec$sources$source_table, c("cohort_a", "cohort_b"))
  expect_identical(spec$fingerprint_sha256, reversed$fingerprint_sha256)
  expect_match(spec$fingerprint_sha256, "^[a-f0-9]{64}$")
  expect_false(any(grepl("source_value|identifier_value", names(spec$sources))))
})

test_that("identity-universe specification requires reviewed compatible metadata", {
  sources <- make_identity_universe_sources()
  expect_error(
    epi_sec_identity_universe_spec(sources[1, ]),
    "at least two"
  )

  mixed <- sources
  mixed$identity_namespace[[2]] <- "other_codes"
  expect_error(
    epi_sec_identity_universe_spec(mixed),
    "one shared identity_namespace"
  )

  duplicate <- sources
  duplicate$source_table[[2]] <- duplicate$source_table[[1]]
  expect_error(
    epi_sec_identity_universe_spec(duplicate),
    "pairs must be unique"
  )

  unreviewed <- sources
  unreviewed$validation_status[[2]] <- "pending"
  expect_error(
    epi_sec_identity_universe_spec(unreviewed),
    "must be 'confirmed'"
  )

  with_values <- sources
  with_values$source_value <- c("A", "B")
  expect_error(
    epi_sec_identity_universe_spec(with_values),
    "must not contain row values"
  )

  expect_error(
    epi_sec_identity_universe_spec(sources, normalization = "lowercase"),
    "must be 'identity'"
  )
  expect_error(
    epi_sec_identity_universe_spec(sources, validity_regex = ""),
    "NULL or one non-empty"
  )
})

test_that("identity-universe specification reads metadata CSV files", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(make_identity_universe_sources(), path, row.names = FALSE)

  spec <- epi_sec_identity_universe_spec(path)

  expect_equal(nrow(spec$sources), 2L)
  expect_identical(spec$normalization, "identity")
  expect_null(spec$validity_regex)
})

test_that("identity-universe print method does not reveal relation metadata", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  printed <- capture.output(print(spec))

  expect_match(paste(printed, collapse = "\n"), "Confirmed sources: 2")
  expect_false(any(grepl("cohort|participant_code|source_data", printed)))
})

test_that("identity-universe issue semantics are stable and value-free", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  source_audit <- data.frame(
    source_schema = spec$sources$source_schema,
    source_table = spec$sources$source_table,
    id_column = spec$sources$id_column,
    identity_namespace = spec$sources$identity_namespace,
    provenance = spec$sources$provenance,
    n_input = c(5, 0),
    n_null = c(1, 0),
    n_blank = c(1, 0),
    n_invalid = c(1, 0),
    n_observed = c(2, 0),
    n_distinct = c(1, 0),
    n_duplicate_excess = c(1, 0),
    max_frequency = c(2, 0),
    status = c("blocked", "warning"),
    stringsAsFactors = FALSE
  )
  namespace_audit <- data.frame(n_collisions = 1)

  issues <- identity_universe_issues(
    spec, source_audit, namespace_audit
  )

  expect_equal(
    issues$issue_code,
    c(
      "null_identifier", "blank_identifier", "invalid_identifier",
      "duplicate_identifier", "empty_source", "normalization_collision"
    )
  )
  expect_equal(
    issues$severity,
    c("blocking", "blocking", "blocking", "warning", "warning", "blocking")
  )
  expect_false(any(grepl("opaque-value", unlist(issues), fixed = TRUE)))
})

test_that("identity-universe result print is aggregate only", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  audit <- list(
    source_audit = data.frame(source_table = c("a", "b")),
    namespace_audit = data.frame(n_distinct = 4),
    overlap_audit = data.frame(),
    issues = identity_universe_empty_issues()
  )
  result <- identity_universe_result("audit", FALSE, spec, audit)
  printed <- capture.output(print(result))

  expect_identical(result$status, "audit_complete")
  expect_false(any(grepl("cohort|participant_code|source_data", printed)))
  expect_match(paste(printed, collapse = "\n"), "distinct observed identifiers: 4")
})
