library(episcout)
library(testthat)

context("live PostgreSQL identity-universe workflow")

identity_universe_connection <- function() {
  required <- c("PGHOST", "PGDATABASE", "PGUSER")
  configured <- Sys.getenv(required, unset = "")
  skip_if(
    any(!nzchar(configured)),
    paste("Set", paste(required, collapse = ", "), "to run live PostgreSQL tests.")
  )
  arguments <- list(
    drv = RPostgres::Postgres(),
    host = configured[["PGHOST"]],
    dbname = configured[["PGDATABASE"]],
    user = configured[["PGUSER"]]
  )
  port <- Sys.getenv("PGPORT", unset = "")
  password <- Sys.getenv("PGPASSWORD", unset = "")
  if (nzchar(port)) arguments$port <- as.integer(port)
  if (nzchar(password)) arguments$password <- password
  do.call(DBI::dbConnect, arguments)
}

identity_universe_suffix <- function() {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
}

identity_universe_quote <- function(connection, schema, table = NULL) {
  value <- if (is.null(table)) schema else DBI::Id(schema = schema, table = table)
  as.character(DBI::dbQuoteIdentifier(connection, value))
}

identity_universe_live_spec <- function(schema, regex = NULL, tables = c("source_a", "source_b")) {
  sources <- data.frame(
    source_schema = rep(schema, length(tables)),
    source_table = tables,
    id_column = rep("identifier", length(tables)),
    identity_namespace = rep("synthetic_codes", length(tables)),
    provenance = rep("reviewed_runtime_synthetic_fixture", length(tables)),
    validation_status = rep("confirmed", length(tables)),
    stringsAsFactors = FALSE
  )
  epi_sec_identity_universe_spec(sources, validity_regex = regex)
}

test_that("live identity-universe audit and materialisation reconcile without leakage", {
  skip_if_not(identical(Sys.getenv("EPISCOUT_TEST_POSTGRES", unset = ""), "1"))
  skip_if_not_installed("RPostgres")

  connection <- identity_universe_connection()
  lock_connection <- identity_universe_connection()

  suffix <- identity_universe_suffix()
  source_schema <- paste0("identity_source_", suffix)
  output_schema <- paste0("identity_output_", suffix)
  stopifnot(
    grepl("^identity_source_[a-f0-9]{16}$", source_schema),
    grepl("^identity_output_[a-f0-9]{16}$", output_schema)
  )
  on.exit(
    {
      if (DBI::dbIsValid(connection)) {
        DBI::dbExecute(
          connection,
          paste("DROP SCHEMA IF EXISTS", identity_universe_quote(connection, output_schema), "CASCADE")
        )
        DBI::dbExecute(
          connection,
          paste("DROP SCHEMA IF EXISTS", identity_universe_quote(connection, source_schema), "CASCADE")
        )
        DBI::dbDisconnect(connection)
      }
      if (DBI::dbIsValid(lock_connection)) DBI::dbDisconnect(lock_connection)
    },
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste("CREATE SCHEMA", identity_universe_quote(connection, source_schema))
  )
  DBI::dbExecute(
    connection,
    paste("CREATE SCHEMA", identity_universe_quote(connection, output_schema))
  )
  DBI::dbExecute(
    connection,
    paste("REVOKE ALL ON SCHEMA", identity_universe_quote(connection, output_schema), "FROM PUBLIC")
  )
  for (table in c("source_a", "source_b", "source_bad", "source_empty")) {
    DBI::dbExecute(
      connection,
      paste0(
        "CREATE TABLE ", identity_universe_quote(connection, source_schema, table),
        " (identifier text COLLATE \"C\")"
      )
    )
  }

  identifiers <- paste0("secret_", c("alpha", "beta", "gamma", "delta"), "_", suffix)
  insert_value <- function(table, value) {
    if (is.null(value)) {
      return(DBI::dbExecute(
        connection,
        paste("INSERT INTO", identity_universe_quote(connection, source_schema, table), "(identifier) VALUES (NULL)")
      ))
    }
    DBI::dbExecute(
      connection,
      paste("INSERT INTO", identity_universe_quote(connection, source_schema, table), "(identifier) VALUES ($1)"),
      params = list(value)
    )
  }
  for (value in identifiers[c(1, 2, 2, 3)]) insert_value("source_a", value)
  for (value in identifiers[c(2, 3, 4)]) insert_value("source_b", value)
  insert_value("source_bad", NULL)
  insert_value("source_bad", "   ")
  insert_value("source_bad", paste0("invalid_", suffix))

  regex <- paste0("^secret_[a-z]+_", suffix, "$")
  spec <- identity_universe_live_spec(source_schema, regex)
  before_rows <- DBI::dbGetQuery(
    connection,
    paste("SELECT COUNT(*)::integer AS n FROM", identity_universe_quote(connection, source_schema, "source_a"))
  )$n[[1]]

  audit <- epi_sec_identity_universe_db(connection, spec)

  expect_s3_class(audit, "epi_sec_identity_universe_result")
  expect_identical(audit$status, "audit_complete")
  expect_false(audit$metadata$writes[[1]])
  expect_equal(audit$source_audit$n_input, c(4, 3))
  expect_equal(audit$source_audit$n_distinct, c(3, 3))
  expect_equal(audit$source_audit$n_duplicate_excess, c(1, 0))
  expect_equal(audit$source_audit$max_frequency, c(2, 1))
  expect_equal(audit$namespace_audit$n_distinct, 4)
  expect_equal(audit$namespace_audit$n_single_source, 2)
  expect_equal(audit$namespace_audit$n_multi_source, 2)
  expect_equal(audit$namespace_audit$n_collisions, 0)
  expect_equal(audit$overlap_audit$n_intersection, 2)
  expect_equal(audit$overlap_audit$n_left_exclusive, 1)
  expect_equal(audit$overlap_audit$n_right_exclusive, 1)
  expect_equal(audit$overlap_audit$left_coverage, 2 / 3)
  expect_equal(audit$overlap_audit$right_coverage, 2 / 3)
  expect_equal(audit$issues$issue_code, "duplicate_identifier")
  expect_false(
    DBI::dbExistsTable(connection, DBI::Id(schema = output_schema, table = "identity_universe"))
  )
  expect_equal(
    DBI::dbGetQuery(
      connection,
      paste("SELECT COUNT(*)::integer AS n FROM", identity_universe_quote(connection, source_schema, "source_a"))
    )$n[[1]],
    before_rows
  )
  ordinary_text <- paste(unlist(audit), collapse = "\n")
  expect_false(any(vapply(identifiers, grepl, logical(1), x = ordinary_text, fixed = TRUE)))

  materialised <- epi_sec_identity_universe_db(
    connection,
    spec,
    mode = "materialise",
    output_schema = output_schema,
    output_table = "identity_universe"
  )

  expect_identical(materialised$status, "complete")
  expect_true(materialised$metadata$writes[[1]])
  output <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT identity_namespace, canonical_identifier, source_membership_count FROM",
      identity_universe_quote(connection, output_schema, "identity_universe"),
      "ORDER BY canonical_identifier"
    )
  )
  expect_equal(nrow(output), 4L)
  expect_equal(output$canonical_identifier, sort(identifiers))
  expected_membership <- c(1L, 2L, 2L, 1L)
  names(expected_membership) <- identifiers
  expect_equal(
    output$source_membership_count,
    unname(expected_membership[output$canonical_identifier])
  )
  expect_true(all(output$identity_namespace == "synthetic_codes"))
  constraints <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT COUNT(*)::integer AS n FROM pg_constraint c",
      "INNER JOIN pg_class t ON t.oid = c.conrelid",
      "INNER JOIN pg_namespace n ON n.oid = t.relnamespace",
      "WHERE n.nspname = $1 AND t.relname = $2 AND c.contype = 'u'"
    ),
    params = list(output_schema, "identity_universe")
  )
  expect_equal(constraints$n[[1]], 1L)
  expect_false(
    episcout:::sec_table_is_public(connection, output_schema, "identity_universe")
  )

  existing <- epi_sec_identity_universe_db(
    connection,
    spec,
    mode = "materialise",
    output_schema = output_schema,
    output_table = "identity_universe"
  )
  expect_identical(existing$status, "blocked")
  expect_equal(existing$issues$issue_code, c("duplicate_identifier", "destination_exists"))

  blocked_spec <- identity_universe_live_spec(
    source_schema, regex, c("source_bad", "source_empty")
  )
  blocked <- epi_sec_identity_universe_db(connection, blocked_spec)
  expect_identical(blocked$status, "blocked")
  expect_equal(
    blocked$issues$issue_code,
    c("null_identifier", "blank_identifier", "invalid_identifier", "empty_source")
  )
  expect_equal(blocked$issues$n_affected, c(1, 1, 1, 1))
  expect_equal(blocked$namespace_audit$n_distinct, 0)
  expect_equal(blocked$overlap_audit$n_intersection, 0)
  expect_true(is.na(blocked$overlap_audit$left_coverage))
  expect_true(is.na(blocked$overlap_audit$right_coverage))

  lock_table <- "locked_universe"
  lock_key <- paste0("identity-universe:", output_schema, ".", lock_table)
  DBI::dbGetQuery(
    lock_connection,
    "SELECT pg_advisory_lock(hashtextextended($1, 0))",
    params = list(lock_key)
  )
  locked <- epi_sec_identity_universe_db(
    connection,
    spec,
    mode = "materialise",
    output_schema = output_schema,
    output_table = lock_table,
    lock_timeout = 1
  )
  expect_identical(locked$status, "blocked")
  expect_true("lock_timeout" %in% locked$issues$issue_code)
  expect_false(DBI::dbExistsTable(connection, DBI::Id(schema = output_schema, table = lock_table)))
  DBI::dbGetQuery(
    lock_connection,
    "SELECT pg_advisory_unlock(hashtextextended($1, 0))",
    params = list(lock_key)
  )

  rollback_table <- "rollback_universe"
  original_db_execute <- DBI::dbExecute
  expect_error(
    with_mocked_bindings(
      epi_sec_identity_universe_db(
        connection,
        spec,
        mode = "materialise",
        output_schema = output_schema,
        output_table = rollback_table
      ),
      dbExecute = function(conn, statement, ...) {
        if (grepl("REVOKE ALL ON TABLE", statement, fixed = TRUE)) {
          stop("forced restricted-publication failure")
        }
        original_db_execute(conn, statement, ...)
      },
      .package = "DBI"
    ),
    "rolled back safely"
  )
  expect_false(
    DBI::dbExistsTable(connection, DBI::Id(schema = output_schema, table = rollback_table))
  )
})
