library(episcout)
library(testthat)

context("PostgreSQL identity-universe metadata")

make_identity_universe_sources <- function() {
  data.frame(
    source_schema = c("source_data", "source_data"),
    source_table = c("cohort_b", "cohort_a"),
    id_column = c("participant_code", "participant_code"),
    identity_namespace = c("participant_codes", "participant_codes"),
    provenance = c("synthetic_fixture_definition", "synthetic_fixture_definition"),
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
  expect_named(
    spec$sources,
    c(
      "source_schema", "source_table", "id_column", "identity_namespace",
      "provenance"
    )
  )
  expect_identical(spec$contract_version, "identity-universe-2")
  expect_identical(spec$fingerprint_sha256, reversed$fingerprint_sha256)
  expect_match(spec$fingerprint_sha256, "^[a-f0-9]{64}$")
  expect_false(any(grepl("source_value|identifier_value", names(spec$sources))))
})

test_that("legacy identity-universe status is ignored with one migration warning", {
  sources <- make_identity_universe_sources()
  legacy <- sources
  legacy$validation_status <- c("pending", NA_character_)
  observed <- new.env(parent = emptyenv())
  observed$warnings <- character()

  adapted <- withCallingHandlers(
    epi_sec_identity_universe_spec(legacy),
    warning = function(condition) {
      observed$warnings <- c(observed$warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  current <- epi_sec_identity_universe_spec(sources)

  expect_length(observed$warnings, 1L)
  expect_match(observed$warnings, "validation_status.*deprecated.*ignored")
  expect_identical(adapted, current)
  expect_false("validation_status" %in% names(adapted$sources))
})

test_that("identity-universe specification requires compatible technical metadata", {
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

test_that("saved and modified identity-universe specifications require regeneration", {
  sources <- make_identity_universe_sources()
  spec <- epi_sec_identity_universe_spec(sources)
  legacy_sources <- sources
  legacy_sources$validation_status <- "confirmed"
  saved_version_one <- structure(
    list(
      sources = legacy_sources,
      normalization = "identity",
      validity_regex = NULL,
      contract_version = "identity-universe-1",
      fingerprint_sha256 = "saved-version-one-fingerprint"
    ),
    class = c("epi_sec_identity_universe_spec", "list")
  )

  expect_error(
    universe_validate_spec(saved_version_one),
    "Regenerate it with epi_sec_identity_universe_spec"
  )

  modified <- spec
  modified$fingerprint_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    universe_validate_spec(modified),
    "Regenerate it with epi_sec_identity_universe_spec"
  )
  expect_identical(
    modified$fingerprint_sha256,
    paste(rep("0", 64L), collapse = "")
  )
})

test_that("identity-universe source structure and exact type boundaries remain errors", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  ordinary_relation <- function(con, schema, table) {
    list(exists = TRUE, relkind = "r")
  }
  text_columns <- function(con, schema, table) {
    data.frame(
      source_column = "participant_code",
      source_udt_name = "text",
      stringsAsFactors = FALSE
    )
  }

  expect_error(
    with_mocked_bindings(
      universe_context(NULL, spec),
      sec_relation_state = function(con, schema, table) {
        list(exists = TRUE, relkind = "v")
      },
      .package = "episcout"
    ),
    "ordinary PostgreSQL table"
  )
  expect_error(
    with_mocked_bindings(
      universe_context(NULL, spec),
      sec_relation_state = ordinary_relation,
      sec_source_columns = function(con, schema, table) {
        data.frame(
          source_column = "participant_code",
          source_udt_name = "numeric",
          stringsAsFactors = FALSE
        )
      },
      .package = "episcout"
    ),
    "text, integral or UUID"
  )
  expect_error(
    with_mocked_bindings(
      universe_context(NULL, spec),
      sec_relation_state = ordinary_relation,
      sec_source_columns = text_columns,
      sec_id_collation_deterministic = function(con, column) FALSE,
      .package = "episcout"
    ),
    "nondeterministic PostgreSQL collation"
  )
  expect_error(
    with_mocked_bindings(
      universe_context(NULL, spec),
      sec_relation_state = ordinary_relation,
      sec_source_columns = function(con, schema, table) {
        data.frame(
          source_column = "participant_code",
          source_udt_name = if (table == "cohort_a") "text" else "int8",
          stringsAsFactors = FALSE
        )
      },
      sec_id_collation_deterministic = function(con, column) TRUE,
      .package = "episcout"
    ),
    "one compatible identifier type family"
  )
})

test_that("identity-universe print method does not reveal relation metadata", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  printed <- capture.output(print(spec))

  expect_match(paste(printed, collapse = "\n"), "Sources: 2")
  expect_false(any(grepl("confirm|approv|authoris", printed, ignore.case = TRUE)))
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
    status = c("error", "warning"),
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
    c("error", "error", "error", "warning", "warning", "error")
  )
  expect_false(any(grepl("opaque-value", unlist(issues), fixed = TRUE)))
})

test_that("identity-universe result statuses are mode-specific and aggregate only", {
  spec <- epi_sec_identity_universe_spec(make_identity_universe_sources())
  audit <- list(
    source_audit = data.frame(source_table = c("a", "b")),
    namespace_audit = data.frame(n_distinct = 4),
    overlap_audit = data.frame(),
    issues = identity_universe_issue(
      "null_identifier", "error", "source", NULL, 1,
      "Identifier values are null.", "Correct null identifiers, then inspect again."
    )
  )
  result <- identity_universe_result("audit", FALSE, spec, audit)
  not_written <- identity_universe_result("materialise", FALSE, spec, audit)
  printed <- capture.output(print(result))

  expect_identical(result$status, "audit_complete")
  expect_false(result$metadata$writes[[1]])
  expect_identical(not_written$status, "not_written")
  expect_false(not_written$metadata$writes[[1]])
  expect_false(any(grepl("cohort|participant_code|source_data", printed)))
  expect_match(paste(printed, collapse = "\n"), "distinct observed identifiers: 4")
  expect_match(paste(printed, collapse = "\n"), "error issues: 1")
})
