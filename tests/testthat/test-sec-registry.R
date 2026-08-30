library(episcout)
library(testthat)

context("identity-registry technical contract")

registry_metadata_fixture <- function() {
  data.frame(
    registry_id = "registry-fixture",
    registry_version = 2L,
    token_prefix = "E",
    n_bytes = 24L,
    created_at = as.POSIXct("2026-01-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

test_that("registry results use neutral technical fields and statuses", {
  result <- sec_registry_result(
    status = "incompatible",
    mode = "audit",
    writes = FALSE,
    registry_schema = "registry_fixture",
    metadata = sec_empty_registry_metadata(),
    objects = sec_registry_object_frame("incompatible_structure"),
    next_action = "Inspect the registry structure."
  )

  expect_s3_class(result, "epi_sec_registry_result")
  expect_named(
    result,
    c(
      "status", "mode", "writes", "registry_schema", "metadata", "objects",
      "next_action"
    )
  )
  expect_identical(result$status, "incompatible")
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_false(grepl(
    "PUBLIC|restricted|approved|authori[sz]ed|blocked",
    printed,
    ignore.case = TRUE
  ))
})

test_that("registry inspection classifies structure without ownership or grants", {
  statements <- character()
  relations <- data.frame(
    table_name = sec_registry_tables(),
    relkind = rep("r", length(sec_registry_tables())),
    owned = FALSE,
    public_access = TRUE,
    stringsAsFactors = FALSE
  )

  observed <- with_mocked_bindings(
    with_mocked_bindings(
      sec_registry_inspect(NULL, "registry_fixture"),
      dbGetQuery = function(con, statement, params = NULL) {
        statements <<- c(statements, statement)
        if (grepl("registry_id, registry_version", statement, fixed = TRUE)) {
          return(registry_metadata_fixture())
        }
        relations
      },
      .package = "DBI"
    ),
    sec_registry_structure_ok = function(con, schema, version) TRUE,
    sec_quote_table = function(con, schema, table) {
      paste0('"', schema, '"."', table, '"')
    },
    .package = "episcout"
  )

  expect_identical(observed$state, "compatible")
  expect_true(all(observed$objects$status == "present"))
  expect_false(any(grepl(
    "relowner|has_(schema|table)_privilege|public_access",
    statements,
    ignore.case = TRUE,
    perl = TRUE
  )))
})

test_that("registry creation preserves objects without changing privileges", {
  statements <- character()

  created <- with_mocked_bindings(
    with_mocked_bindings(
      sec_registry_create(NULL, "registry_fixture", "E", 24L),
      dbExecute = function(con, statement, ...) {
        statements <<- c(statements, statement)
        1L
      },
      .package = "DBI"
    ),
    sec_quote_table = function(con, schema, table) {
      paste0('"', schema, '"."', table, '"')
    },
    sec_quote_identifier = function(con, value) paste0('"', value, '"'),
    sec_generate_tokens = function(n, n_bytes, prefix) "Rfixture",
    .package = "episcout"
  )

  expect_true(created)
  expect_equal(sum(grepl("^CREATE TABLE", statements)), 6L)
  expect_equal(sum(grepl("^INSERT INTO", statements)), 1L)
  expect_false(any(grepl(
    "has_(schema|table)_privilege|\\b(GRANT|REVOKE)\\b",
    statements,
    ignore.case = TRUE,
    perl = TRUE
  )))
})

test_that("registry token settings remain immutable", {
  metadata <- registry_metadata_fixture()

  expect_invisible(sec_registry_assert_settings(metadata, "E", 24L))
  expect_error(
    sec_registry_assert_settings(metadata, "X", 24L),
    "do not match the existing identity registry"
  )
  expect_error(
    sec_registry_assert_settings(metadata, "E", 32L),
    "do not match the existing identity registry"
  )
})

test_that("destructive replacement remains limited to owned safe tables", {
  destination_state <- function(owned, dependencies = FALSE) {
    with_mocked_bindings(
      sec_destination_state(NULL, "output_fixture", "destination_fixture"),
      dbGetQuery = function(con, statement, params) {
        data.frame(
          relkind = "r",
          relispartition = FALSE,
          owned = owned,
          has_dependencies = dependencies,
          stringsAsFactors = FALSE
        )
      },
      .package = "DBI"
    )
  }

  expect_true(destination_state(TRUE)$replaceable)
  expect_false(destination_state(FALSE)$replaceable)
  expect_false(destination_state(TRUE, dependencies = TRUE)$replaceable)
})

test_that("native permission detail remains behind a fixed database error", {
  denied <- expect_error(
    sec_database_boundary(
      stop(
        "ERROR: permission denied for table registry_fixture_secret",
        call. = FALSE
      ),
      "The database operation could not complete."
    ),
    "The database operation could not complete"
  )

  expect_false(grepl("permission denied", conditionMessage(denied), fixed = TRUE))
  expect_false(grepl("registry_fixture_secret", conditionMessage(denied), fixed = TRUE))
})
