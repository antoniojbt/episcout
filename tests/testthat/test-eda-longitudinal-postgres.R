context("live PostgreSQL longitudinal population and key QC")

longitudinal_pg_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    testthat::skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
  }
  testthat::skip_if_not_installed("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

longitudinal_runtime_suffix <- function() {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
}

longitudinal_quote <- function(con, schema, relation = NULL) {
  identifier <- if (is.null(relation)) {
    schema
  } else {
    DBI::Id(schema = schema, table = relation)
  }
  as.character(DBI::dbQuoteIdentifier(con, identifier))
}

longitudinal_relation_state <- function(con, schema) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.relname, c.relkind FROM pg_catalog.pg_class AS c",
      "INNER JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 ORDER BY c.relname, c.relkind"
    ),
    params = list(schema)
  )
}

longitudinal_fixture_rows <- function(entity_values,
                                      key_values,
                                      entity_index,
                                      key_index,
                                      key_sequence) {
  entities <- vapply(entity_index, function(index) {
    if (is.na(index)) return(NA_character_)
    if (index == 0L) return("   ")
    entity_values[[index]]
  }, character(1))
  keys <- vapply(key_index, function(index) {
    if (is.na(index)) return(NA_character_)
    if (index == 0L) return("   ")
    key_values[[index]]
  }, character(1))
  data.frame(
    "entity id" = entities,
    "key part" = keys,
    "key sequence" = as.integer(key_sequence),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

longitudinal_create_table <- function(con, schema, relation, partitioned = FALSE) {
  relation_sql <- longitudinal_quote(con, schema, relation)
  partition_sql <- if (partitioned) {
    " PARTITION BY LIST (\"key sequence\")"
  } else {
    ""
  }
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", relation_sql,
    " (\"entity id\" text COLLATE \"C\", \"key part\" text COLLATE \"C\", \"key sequence\" integer)",
    partition_sql
  ))
  if (partitioned) {
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", longitudinal_quote(con, schema, paste0(relation, " partition")),
      " PARTITION OF ", relation_sql, " DEFAULT"
    ))
  }
  invisible(relation)
}

longitudinal_fixture <- function(con) {
  suffix <- longitudinal_runtime_suffix()
  schema <- paste0("longitudinal_", suffix)
  server <- paste0("longitudinal_server_", suffix)
  DBI::dbExecute(con, paste("CREATE SCHEMA", longitudinal_quote(con, schema)))

  entity_values <- paste0("runtime_entity_", seq_len(7L), "_", suffix)
  key_values <- paste0("runtime_key_", seq_len(5L), "_", suffix)
  period_one <- "period one"
  period_two <- "period two"
  period_three_base <- "period three base"
  period_four_base <- "period four base"
  longitudinal_create_table(con, schema, period_one)
  longitudinal_create_table(con, schema, period_two, partitioned = TRUE)
  longitudinal_create_table(con, schema, period_three_base)
  longitudinal_create_table(con, schema, period_four_base)

  rows <- list(
    longitudinal_fixture_rows(
      entity_values, key_values,
      c(1L, 1L, 2L, 3L, NA_integer_, 0L),
      c(1L, 1L, 2L, 3L, 4L, 4L), c(1L, 1L, 1L, 1L, 1L, 2L)
    ),
    longitudinal_fixture_rows(
      entity_values, key_values, c(1L, 2L, 4L, 5L),
      c(1L, 2L, NA_integer_, 0L), rep(1L, 4L)
    ),
    longitudinal_fixture_rows(
      entity_values, key_values, c(2L, 4L, 5L, 6L),
      c(1L, 1L, 2L, 3L), rep(1L, 4L)
    ),
    longitudinal_fixture_rows(
      entity_values, key_values, c(1L, 2L, 5L, 6L, 7L),
      seq_len(5L), rep(1L, 5L)
    )
  )
  for (index in seq_along(rows)) {
    relation <- c(
      period_one, period_two, period_three_base, period_four_base
    )[[index]]
    DBI::dbAppendTable(con, DBI::Id(schema = schema, table = relation), rows[[index]])
  }

  period_three <- "period three view"
  period_four <- "period four materialised"
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", longitudinal_quote(con, schema, period_three),
    " AS SELECT * FROM ", longitudinal_quote(con, schema, period_three_base)
  ))
  DBI::dbExecute(con, paste0(
    "CREATE MATERIALIZED VIEW ", longitudinal_quote(con, schema, period_four),
    " AS SELECT * FROM ", longitudinal_quote(con, schema, period_four_base)
  ))

  DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS file_fdw")
  DBI::dbExecute(con, paste0(
    "CREATE SERVER ", as.character(DBI::dbQuoteIdentifier(con, server)),
    " FOREIGN DATA WRAPPER file_fdw"
  ))
  foreign_relation <- "empty foreign period"
  DBI::dbExecute(con, paste0(
    "CREATE FOREIGN TABLE ", longitudinal_quote(con, schema, foreign_relation),
    " (\"entity id\" text COLLATE \"C\", \"key part\" text COLLATE \"C\", \"key sequence\" integer) SERVER ",
    as.character(DBI::dbQuoteIdentifier(con, server)),
    " OPTIONS (filename '/dev/null', format 'csv')"
  ))

  sources <- list(
    baseline = epi_eda_postgres_source(con, schema, period_one),
    follow_up_1 = epi_eda_postgres_source(con, schema, period_two),
    follow_up_2 = epi_eda_postgres_source(con, schema, period_three),
    follow_up_3 = epi_eda_postgres_source(con, schema, period_four)
  )
  list(
    schema = schema,
    server = server,
    sources = sources,
    foreign_source = epi_eda_postgres_source(con, schema, foreign_relation),
    entity_values = entity_values,
    key_values = key_values
  )
}

test_that("live ordered aggregates match hand-derived population and key evidence", {
  con <- longitudinal_pg_connection()
  fixture <- longitudinal_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0(
          "DROP SERVER IF EXISTS ",
          DBI::dbQuoteIdentifier(con, fixture$server), " CASCADE"
        ))
        DBI::dbExecute(con, paste0(
          "DROP SCHEMA IF EXISTS ",
          longitudinal_quote(con, fixture$schema), " CASCADE"
        ))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  before_relations <- longitudinal_relation_state(con, fixture$schema)

  observed <- epi_eda_longitudinal_qc(
    fixture$sources,
    entity_id = "entity id",
    record_key = c("key part", "key sequence")
  )

  expect_identical(class(observed), c("epi_eda_longitudinal_qc", "list"))
  expect_identical(
    names(observed),
    c(
      "metadata", "period_summary", "adjacent_membership",
      "pairwise_overlap", "history_summary", "issues"
    )
  )
  expect_identical(
    names(observed$metadata),
    c(
      "contract_version", "n_periods", "entity_id",
      "record_key_declared", "n_record_key_columns",
      "period_labels", "source_fingerprints",
      "source_set_fingerprint_sha256"
    )
  )
  expect_identical(observed$metadata$contract_version, "longitudinal-qc-1")
  expect_identical(observed$metadata$n_periods, 4L)
  expect_true(observed$metadata$record_key_declared)
  expect_identical(observed$metadata$n_record_key_columns, 2L)
  expect_identical(observed$metadata$period_labels[[1L]], names(fixture$sources))
  expect_length(observed$metadata$source_fingerprints[[1L]], 4L)

  period <- observed$period_summary
  expect_identical(
    names(period),
    c(
      "period_index", "period_label", "n_rows", "n_entity_null",
      "n_entity_blank", "n_entity_nonblank", "n_valid_entity_rows", "n_distinct_entities",
      "n_repeated_entity_rows", "n_repeated_entity_excess",
      "max_entity_frequency", "n_missing_record_key",
      "n_complete_record_key_rows", "n_distinct_record_keys",
      "n_duplicate_record_key_groups", "n_duplicate_record_key_rows",
      "n_duplicate_record_key_excess", "max_record_key_frequency"
    )
  )
  expect_identical(period$period_index, 1:4)
  expect_identical(period$period_label, names(fixture$sources))
  expect_identical(period$n_rows, c(6, 4, 4, 5))
  expect_identical(period$n_entity_null, c(1, 0, 0, 0))
  expect_identical(period$n_entity_blank, c(1, 0, 0, 0))
  expect_identical(period$n_entity_nonblank, c(4, 4, 4, 5))
  expect_identical(period$n_valid_entity_rows, c(4, 4, 4, 5))
  expect_identical(period$n_distinct_entities, c(3, 4, 4, 5))
  expect_identical(period$n_repeated_entity_rows, c(2, 0, 0, 0))
  expect_identical(period$n_repeated_entity_excess, c(1, 0, 0, 0))
  expect_identical(period$max_entity_frequency, c(2, 1, 1, 1))
  expect_identical(period$n_missing_record_key, c(0, 2, 0, 0))
  expect_identical(period$n_complete_record_key_rows, c(6, 2, 4, 5))
  expect_identical(period$n_distinct_record_keys, c(5, 2, 3, 5))
  expect_identical(period$n_duplicate_record_key_groups, c(1, 0, 1, 0))
  expect_identical(period$n_duplicate_record_key_rows, c(2, 0, 2, 0))
  expect_identical(period$n_duplicate_record_key_excess, c(1, 0, 1, 0))
  expect_identical(period$max_record_key_frequency, c(2, 1, 2, 1))
  expect_type(period$period_index, "integer")
  expect_true(all(vapply(period[3:18], is.double, logical(1))))

  adjacent <- observed$adjacent_membership
  expect_identical(
    names(adjacent),
    c(
      "from_period_index", "from_period_label", "to_period_index",
      "to_period_label", "n_from_entities", "n_to_entities", "n_union",
      "n_retained", "n_exited", "n_entered", "retention_numerator", "retention_denominator",
      "retention_proportion", "exit_denominator", "exit_proportion",
      "entry_numerator", "entry_denominator", "entry_proportion"
    )
  )
  expect_identical(adjacent$n_from_entities, c(3, 4, 4))
  expect_identical(adjacent$n_to_entities, c(4, 4, 5))
  expect_identical(adjacent$n_retained, c(2, 3, 3))
  expect_identical(adjacent$n_union, c(5, 5, 6))
  expect_identical(adjacent$retention_numerator, c(2, 3, 3))
  expect_identical(adjacent$n_exited, c(1, 1, 1))
  expect_identical(adjacent$n_entered, c(2, 1, 2))
  expect_identical(adjacent$entry_numerator, c(2, 1, 2))
  expect_equal(adjacent$retention_proportion, c(2 / 3, 3 / 4, 3 / 4))
  expect_equal(adjacent$exit_proportion, c(1 / 3, 1 / 4, 1 / 4))
  expect_equal(adjacent$entry_proportion, c(2 / 4, 1 / 4, 2 / 5))

  pairwise <- observed$pairwise_overlap
  expect_identical(
    cbind(pairwise$left_period_index, pairwise$right_period_index),
    rbind(c(1L, 2L), c(1L, 3L), c(1L, 4L), c(2L, 3L), c(2L, 4L), c(3L, 4L))
  )
  expect_identical(pairwise$n_overlap, c(2, 1, 2, 3, 3, 3))
  expect_identical(pairwise$n_union, c(5, 6, 6, 5, 6, 6))
  expect_identical(pairwise$n_left_only, c(1, 2, 1, 1, 1, 1))
  expect_identical(pairwise$n_right_only, c(2, 3, 3, 1, 2, 2))
  expect_true(all(vapply(pairwise[5:14], is.double, logical(1))))

  history <- observed$history_summary
  expect_identical(
    names(history),
    c(
      "first_period_index", "first_period_label", "last_period_index",
      "last_period_label", "periods_observed", "gap_periods", "has_gap",
      "n_entities", "proportion_denominator", "proportion"
    )
  )
  expect_identical(history$first_period_index, c(1L, 1L, 1L, 2L, 2L, 3L, 4L))
  expect_identical(history$last_period_index, c(1L, 4L, 4L, 3L, 4L, 4L, 4L))
  expect_identical(history$periods_observed, c(1L, 3L, 4L, 2L, 3L, 2L, 1L))
  expect_identical(history$gap_periods, c(0L, 1L, 0L, 0L, 0L, 0L, 0L))
  expect_identical(history$has_gap, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(history$n_entities, rep(1, 7L))
  expect_identical(history$proportion_denominator, rep(7, 7L))
  expect_equal(history$proportion, rep(1 / 7, 7L))

  expect_identical(
    observed$issues$issue_code,
    c(
      "invalid_entity_id", "duplicate_record_key", "missing_record_key",
      "duplicate_record_key"
    )
  )
  expect_identical(observed$issues$severity, rep("warning", 4L))
  expect_identical(observed$issues$n_affected, c(2, 1, 2, 1))
  expect_identical(observed$issues$variable, c("entity id", "key part,key sequence", "key part,key sequence", "key part,key sequence"))

  repeated <- epi_eda_longitudinal_qc(
    fixture$sources, "entity id", c("key part", "key sequence")
  )
  expect_identical(repeated, observed)
  expect_identical(
    longitudinal_relation_state(con, fixture$schema), before_relations
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))

  result_text <- paste(unlist(observed), collapse = "\n")
  print_text <- paste(capture.output(print(observed)), collapse = "\n")
  for (value in c(fixture$entity_values, fixture$key_values)) {
    expect_false(grepl(value, result_text, fixed = TRUE))
    expect_false(grepl(value, print_text, fixed = TRUE))
  }
})

test_that("live ordering, optional keys, empty periods and source kinds stay explicit", {
  con <- longitudinal_pg_connection()
  fixture <- longitudinal_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0(
          "DROP SERVER IF EXISTS ",
          DBI::dbQuoteIdentifier(con, fixture$server), " CASCADE"
        ))
        DBI::dbExecute(con, paste0(
          "DROP SCHEMA IF EXISTS ",
          longitudinal_quote(con, fixture$schema), " CASCADE"
        ))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  reversed <- epi_eda_longitudinal_qc(rev(fixture$sources), "entity id")
  expect_identical(reversed$period_summary$period_label, rev(names(fixture$sources)))
  expect_identical(reversed$adjacent_membership$n_retained, c(3, 3, 2))
  expect_identical(reversed$adjacent_membership$n_exited, c(2, 1, 2))
  expect_identical(reversed$adjacent_membership$n_entered, c(1, 1, 1))
  expect_true(all(is.na(reversed$period_summary$n_missing_record_key)))
  expect_false(any(reversed$issues$issue_code %in% c(
    "missing_record_key", "duplicate_record_key"
  )))

  simple_key <- epi_eda_longitudinal_qc(
    fixture$sources, "entity id", "key sequence"
  )
  expect_true(any(simple_key$issues$issue_code == "duplicate_record_key"))
  expect_true(all(simple_key$period_summary$n_distinct_record_keys >= 1))

  kinds <- c(
    vapply(fixture$sources, function(source) source$relation_kind, character(1)),
    fixture$foreign_source$relation_kind
  )
  expect_setequal(
    kinds,
    c("table", "partitioned table", "view", "materialized view", "foreign table")
  )
  empty <- epi_eda_longitudinal_qc(
    list(populated = fixture$sources[[1L]], empty = fixture$foreign_source),
    "entity id", c("key part", "key sequence")
  )
  expect_identical(empty$period_summary$n_rows, c(6, 0))
  expect_identical(tail(empty$issues$issue_code, 1L), "empty_period")
  expect_identical(empty$issues$n_affected[empty$issues$issue_code == "empty_period"], 0)
  expect_identical(nrow(empty$history_summary), 1L)
  expect_true(is.na(empty$adjacent_membership$entry_proportion[[1L]]))
  expect_true(is.na(empty$pairwise_overlap$right_overlap_proportion[[1L]]))
})

test_that("the longitudinal wrapper holds one snapshot and rolls back failures", {
  con <- longitudinal_pg_connection()
  writer <- longitudinal_pg_connection()
  fixture <- longitudinal_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0(
          "DROP SERVER IF EXISTS ",
          DBI::dbQuoteIdentifier(con, fixture$server), " CASCADE"
        ))
        DBI::dbExecute(con, paste0(
          "DROP SCHEMA IF EXISTS ",
          longitudinal_quote(con, fixture$schema), " CASCADE"
        ))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  transaction <- getFromNamespace("longitudinal_qc_transaction", "episcout")
  fetch <- getFromNamespace("eda_db_fetch", "episcout")
  table_sql <- longitudinal_quote(writer, fixture$schema, "period one")
  count_sql <- paste0("SELECT COUNT(*)::integer AS n FROM ", table_sql)
  concurrent_suffix <- longitudinal_runtime_suffix()
  concurrent_entity <- paste0("runtime_snapshot_entity_", concurrent_suffix)
  concurrent_key <- paste0("runtime_snapshot_key_", concurrent_suffix)

  observed <- transaction(fixture$sources, {
    before <- DBI::dbGetQuery(con, count_sql)$n[[1L]]
    DBI::dbExecute(writer, paste0(
      "INSERT INTO ", table_sql,
      " (\"entity id\", \"key part\", \"key sequence\") ",
      "VALUES ($1, $2, 999)"
    ), params = list(concurrent_entity, concurrent_key))
    after <- DBI::dbGetQuery(con, count_sql)$n[[1L]]
    c(before = before, after = after)
  })
  expect_identical(observed, c(before = 6L, after = 6L))
  expect_identical(DBI::dbGetQuery(con, count_sql)$n[[1L]], 7L)

  expect_error(
    transaction(fixture$sources, {
      fetch(
        con,
        "SELECT definitely_missing_column FROM definitely_missing_relation",
        query_kind = "forced_longitudinal_failure",
        limit = 1L
      )
    }),
    "forced_longitudinal_failure"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1::integer AS usable")$usable, 1L)
})

test_that("live source, connection and SQL failures are hard and value-free", {
  con <- longitudinal_pg_connection()
  fixture <- longitudinal_fixture(con)
  other_con <- longitudinal_pg_connection()
  on.exit(
    {
      if (DBI::dbIsValid(other_con)) DBI::dbDisconnect(other_con)
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0(
          "DROP SERVER IF EXISTS ",
          DBI::dbQuoteIdentifier(con, fixture$server), " CASCADE"
        ))
        DBI::dbExecute(con, paste0(
          "DROP SCHEMA IF EXISTS ",
          longitudinal_quote(con, fixture$schema), " CASCADE"
        ))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  other_source <- epi_eda_postgres_source(
    other_con,
    fixture$schema,
    fixture$sources[[2L]]$relation
  )
  expect_error(
    epi_eda_longitudinal_qc(
      list(one = fixture$sources[[1L]], two = other_source), "entity id"
    ),
    "share one"
  )

  modified <- fixture$sources[[1L]]
  modified$source_version <- "modified"
  expect_error(
    epi_eda_longitudinal_qc(
      list(one = modified, two = fixture$sources[[2L]]), "entity id"
    ),
    "unmodified"
  )

  drift_relation <- "catalogue drift"
  longitudinal_create_table(con, fixture$schema, drift_relation)
  drift <- epi_eda_postgres_source(con, fixture$schema, drift_relation)
  DBI::dbExecute(con, paste0(
    "ALTER TABLE ", longitudinal_quote(con, fixture$schema, drift_relation),
    " ADD COLUMN changed text"
  ))
  expect_error(
    epi_eda_longitudinal_qc(
      list(one = drift, two = fixture$sources[[2L]]), "entity id"
    ),
    "catalogue changed"
  )

  error_base <- "error base"
  error_view <- "error view"
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", longitudinal_quote(con, fixture$schema, error_base),
    " (divisor integer)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", longitudinal_quote(con, fixture$schema, error_base),
    " VALUES (0)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", longitudinal_quote(con, fixture$schema, error_view),
    " AS SELECT (1 / divisor)::text COLLATE \"C\" AS \"entity id\" FROM ",
    longitudinal_quote(con, fixture$schema, error_base)
  ))
  failing <- epi_eda_postgres_source(con, fixture$schema, error_view)
  failure <- tryCatch(
    epi_eda_longitudinal_qc(
      list(one = fixture$sources[[1L]], two = failing), "entity id"
    ),
    error = identity
  )
  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "query failed")
  expect_false(grepl("division", conditionMessage(failure), ignore.case = TRUE))
  for (value in fixture$entity_values) {
    expect_false(grepl(value, conditionMessage(failure), fixed = TRUE))
  }
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))
})
