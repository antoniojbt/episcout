context("live PostgreSQL thin longitudinal EDA")

longitudinal_eda_pg_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
  }
  skip_if_not_installed("RPostgres")
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

longitudinal_eda_pg_quote <- function(con, schema, relation = NULL) {
  identifier <- if (is.null(relation)) schema else DBI::Id(schema = schema, table = relation)
  as.character(DBI::dbQuoteIdentifier(con, identifier))
}

longitudinal_eda_pg_spec <- function() {
  data.frame(
    name = c("entity", "visit", "score", "state"),
    label = c("Entity", "Visit", "Score", "State"),
    database_type = c("text", "text", "numeric", "text"),
    analysis_type = c("text", "categorical", "numeric", "categorical"),
    role = c("identifier", "time", "measure", "measure"),
    levels = c("", "baseline;month_1;month_2", "", "A;B"),
    stringsAsFactors = FALSE
  )
}

longitudinal_eda_pg_panel <- function(con, rows, prefix = "le_panel") {
  suffix <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = "")
  schema <- paste0(prefix, "_", suffix)
  table <- "panel"
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", longitudinal_eda_pg_quote(con, schema)))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", longitudinal_eda_pg_quote(con, schema, table),
    " (entity text COLLATE \"C\", visit text COLLATE \"C\", score double precision, state text COLLATE \"C\")"
  ))
  DBI::dbAppendTable(con, DBI::Id(schema = schema, table = table), rows)
  list(
    schema = schema, table = table,
    source = epi_eda_postgres_source(con, schema, table), spec = longitudinal_eda_pg_spec()
  )
}

longitudinal_eda_pg_cleanup <- function(con, fixture) {
  if (DBI::dbIsValid(con)) {
    DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", longitudinal_eda_pg_quote(con, fixture$schema), " CASCADE"))
    DBI::dbDisconnect(con)
  }
}

test_that("PostgreSQL longitudinal EDA is aggregate-only and reuses canonical summaries", {
  con <- longitudinal_eda_pg_connection()
  suffix <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = "")
  schema <- paste0("le_", suffix)
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = "panel")
  ))
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql,
    " (entity text COLLATE \"C\", visit text COLLATE \"C\", score double precision, state text COLLATE \"C\")"
  ))
  canary <- paste0("privacy_canary_", suffix)
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES ",
    "($1, 'baseline', 1, 'A'), ($1, 'month_1', 3, 'B'), ",
    "($1, 'month_1', 4, 'B'), ('second', 'baseline', 2, 'A'), ",
    "('second', 'month_1', NULL, NULL), ('second', 'month_2', 'Infinity', 'B'), ",
    "(NULL, 'baseline', 99, 'B'), ('   ', NULL, 88, 'A')"
  ), params = list(canary))
  spec <- data.frame(
    name = c("entity", "visit", "score", "state"),
    label = c("Entity", "Visit", "Score", "State"),
    database_type = c("text", "text", "numeric", "text"),
    analysis_type = c("text", "categorical", "numeric", "categorical"),
    role = c("identifier", "time", "measure", "measure"),
    levels = c("", "baseline;month_1;month_2", "", "A;B"),
    stringsAsFactors = FALSE
  )
  source <- epi_eda_postgres_source(con, schema, "panel")
  observed <- epi_eda_longitudinal(
    source, spec, "entity", "visit", variables = c("score", "state")
  )
  summary_spec <- spec[spec$name %in% c("visit", "score", "state"), , drop = FALSE]
  expected_summaries <- epi_eda_profile_stratified(source, summary_spec, "visit")

  expect_s3_class(observed, "epi_eda_longitudinal")
  expect_identical(observed$summaries, expected_summaries)
  expect_identical(observed$structure$n_rows, 8)
  expect_identical(observed$structure$n_valid_entities, 2)
  expect_identical(observed$structure$n_observed_id_time_cells, 5)
  expect_identical(observed$structure$n_duplicate_cells, 1)
  expect_false(any(grepl(canary, capture.output(str(observed)), fixed = TRUE)))
  expect_true(DBI::dbIsValid(con))
  expect_false(isTRUE(DBI::dbGetInfo(con)$in_transaction))
})

test_that("PostgreSQL longitudinal EDA matches the complete hand-authored panel truth", {
  con <- longitudinal_eda_pg_connection()
  suffix <- paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = "")
  schema <- paste0("le_truth_", suffix)
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table <- DBI::Id(schema = schema, table = "panel")
  table_sql <- as.character(DBI::dbQuoteIdentifier(con, table))
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql,
    " (entity text COLLATE \"C\", visit text COLLATE \"C\", score double precision, state text COLLATE \"C\")"
  ))
  private <- paste0("longitudinal_private_", suffix)
  panel <- data.frame(
    entity = c("a", "a", "a", "a", "b", "b", "b", "c", "d", NA, "  "),
    visit = c("baseline", "month_1", "month_1", "month_2", "baseline", "month_1", "month_2", "baseline", "month_2", "baseline", NA),
    score = c(1, 2, 2, 4, 10, NA, Inf, 5, NaN, 99, 88),
    state = c("A", "B", "B", "B", "A", NA, "B", "A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  panel$entity[[1L]] <- private
  DBI::dbAppendTable(con, table, panel)
  spec <- data.frame(
    name = c("entity", "visit", "score", "state"),
    label = c("Entity", "Visit", "Score", "State"),
    database_type = c("text", "text", "numeric", "text"),
    analysis_type = c("text", "categorical", "numeric", "categorical"),
    role = c("identifier", "time", "measure", "measure"),
    levels = c("", "baseline;month_1;month_2", "", "A;B"),
    stringsAsFactors = FALSE
  )
  frame <- epi_eda_longitudinal(panel, spec, "entity", "visit", variables = c("score", "state"))
  database <- epi_eda_longitudinal(
    epi_eda_postgres_source(con, schema, "panel"), spec, "entity", "visit",
    variables = c("score", "state")
  )

  expect_identical(database$structure, frame$structure)
  expect_identical(database$followup, frame$followup)
  expect_identical(database$timepoints, frame$timepoints)
  expect_identical(database$missingness, frame$missingness)
  expect_equal(database$change, frame$change, tolerance = 1e-12)
  expect_identical(database$issues, frame$issues)
  expect_identical(database$missingness$entity_summary$n_valid_entities, c(5, 5))
  expect_identical(database$missingness$usable_measurement_distribution$n_entities, c(1, 2, 2, 0, 0, 3, 2, 0))
  expect_identical(database$missingness$interior_missing$n_entities_interior_missing, c(1, 1))
  expect_false(any(grepl(private, capture.output(str(database)), fixed = TRUE)))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("longitudinal numerical deltas have independently specified type-7 truth", {
  con <- longitudinal_eda_pg_connection()
  private <- paste0("private_delta_entity_", paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = ""))
  rows <- data.frame(
    entity = c(
      "e1", "e1", "e1", "e2", "e2", "e2", "e3", "e3", "e3",
      "e4", "e4", "e4", "e5", "e6", "e6", "e6", "e7", "e7", "e7"
    ),
    visit = c(
      rep(c("baseline", "month_1", "month_2"), 4L), "baseline",
      "baseline", "month_1", "month_2", "baseline", "month_1", "month_2"
    ),
    score = c(1, 2, 4, 10, 8, 10, 0, 4, 8, 5, NA, 7, 9, 1, 1, Inf, NaN, 4, 6),
    state = "A", stringsAsFactors = FALSE
  )
  rows <- rbind(rows, data.frame(
    entity = private, visit = "baseline", score = 42, state = "A",
    stringsAsFactors = FALSE
  ))
  fixture <- longitudinal_eda_pg_panel(con, rows, "le_delta_truth")
  on.exit(longitudinal_eda_pg_cleanup(con, fixture), add = TRUE)
  observed <- epi_eda_longitudinal(
    fixture$source, fixture$spec, "entity", "visit", variables = "score"
  )
  adjacent <- observed$change$adjacent
  first_last <- observed$change$first_to_last

  expect_identical(adjacent$n_present_both, c(6, 6))
  expect_identical(adjacent$n_eligible, c(4, 4))
  expect_identical(adjacent$n_excluded_missing, c(2, 1))
  expect_identical(adjacent$n_excluded_conflict, c(0, 0))
  expect_identical(adjacent$n_excluded_nonfinite, c(0, 1))
  expect_identical(adjacent$delta_n, c(4, 4))
  expect_equal(adjacent$mean, c(0.75, 2.5))
  expect_equal(adjacent$sd, c(2.5, 1))
  expect_equal(adjacent$min, c(-2, 2))
  expect_equal(adjacent$q1, c(-0.5, 2))
  expect_equal(adjacent$median, c(0.5, 2))
  expect_equal(adjacent$q3, c(1.75, 2.5))
  expect_equal(adjacent$max, c(4, 4))
  expect_equal(adjacent$iqr, c(2.25, 0.5))
  expect_identical(first_last$n_entities_with_presence, 8)
  expect_identical(first_last$n_excluded_single_timepoint, 2)
  expect_identical(first_last$n_present_both, 6)
  expect_identical(first_last$n_eligible, 4)
  expect_identical(first_last$n_excluded_missing, 1)
  expect_identical(first_last$n_excluded_conflict, 0)
  expect_identical(first_last$n_excluded_nonfinite, 1)
  expect_equal(first_last$mean, 3.25)
  expect_equal(first_last$sd, sqrt(139 / 12))
  expect_equal(first_last$min, 0)
  expect_equal(first_last$q1, 1.5)
  expect_equal(first_last$median, 2.5)
  expect_equal(first_last$q3, 4.25)
  expect_equal(first_last$max, 8)
  expect_equal(first_last$iqr, 2.75)
  expect_identical(
    first_last$n_eligible + first_last$n_excluded_missing +
      first_last$n_excluded_conflict + first_last$n_excluded_nonfinite,
    first_last$n_present_both
  )
  expect_false(grepl(private, paste(capture.output(str(observed)), collapse = "\n"), fixed = TRUE))
})

test_that("unexpected reviewed-time validation is value-free", {
  con <- longitudinal_eda_pg_connection()
  private <- paste0("private_unreviewed_time_", paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = ""))
  fixture <- longitudinal_eda_pg_panel(con, data.frame(
    entity = c("one", private), visit = c("baseline", private),
    score = c(1, 2), state = c("A", "B"), stringsAsFactors = FALSE
  ), "le_unreviewed")
  on.exit(longitudinal_eda_pg_cleanup(con, fixture), add = TRUE)
  failure <- tryCatch(
    epi_eda_longitudinal(fixture$source, fixture$spec, "entity", "visit", variables = "score"),
    error = identity
  )
  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "belong to the reviewed time levels")
  expect_false(grepl(private, conditionMessage(failure), fixed = TRUE))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("PostgreSQL longitudinal EDA keeps one snapshot and value-free SQL", {
  con <- longitudinal_eda_pg_connection()
  writer <- longitudinal_eda_pg_connection()
  private <- paste0("private_concurrent_", paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = ""))
  fixture <- longitudinal_eda_pg_panel(con, data.frame(
    entity = c("one", "one", "two"), visit = c("baseline", "month_1", "baseline"),
    score = c(1, 2, 3), state = "A", stringsAsFactors = FALSE
  ), "le_snapshot")
  on.exit({
    if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
    longitudinal_eda_pg_cleanup(con, fixture)
  }, add = TRUE)
  target <- longitudinal_eda_pg_quote(writer, fixture$schema, fixture$table)
  original_structure <- getFromNamespace("le_pg_structure", "episcout")
  original_fetch <- getFromNamespace("eda_db_fetch", "episcout")
  inserted <- FALSE
  statements <- character()
  observed <- with_mocked_bindings(
    epi_eda_longitudinal(fixture$source, fixture$spec, "entity", "visit", variables = "score"),
    le_pg_structure = function(context) {
      value <- original_structure(context)
      if (!inserted) {
        inserted <<- TRUE
        DBI::dbExecute(
          writer,
          paste0("INSERT INTO ", target, " (entity, visit, score, state) VALUES ($1, 'month_2', 99, 'A')"),
          params = list(private)
        )
      }
      value
    },
    eda_db_fetch = function(con, statement, ...) {
      statements <<- c(statements, statement)
      original_fetch(con, statement, ...)
    },
    .package = "episcout"
  )
  expect_identical(observed$structure$n_rows, 3)
  expect_false(any(grepl(private, statements, fixed = TRUE)))
  expect_false(grepl(private, paste(capture.output(str(observed)), collapse = "\n"), fixed = TRUE))
  later <- epi_eda_longitudinal(
    fixture$source, fixture$spec, "entity", "visit", variables = "score"
  )
  expect_identical(later$structure$n_rows, 4)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("changed sources and real database errors fail cleanly and roll back", {
  con <- longitudinal_eda_pg_connection()
  private <- paste0("private_database_error_", paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = ""))
  fixture <- longitudinal_eda_pg_panel(con, data.frame(
    entity = "one", visit = "baseline", score = 1, state = "A", stringsAsFactors = FALSE
  ), "le_failure")
  on.exit(longitudinal_eda_pg_cleanup(con, fixture), add = TRUE)
  changed <- fixture$source
  changed$source_version <- "tampered"
  expect_error(
    epi_eda_longitudinal(changed, fixture$spec, "entity", "visit", variables = "score"),
    "unmodified"
  )
  catalogue <- epi_eda_postgres_source(con, fixture$schema, fixture$table)
  DBI::dbExecute(con, paste0("ALTER TABLE ", longitudinal_eda_pg_quote(con, fixture$schema, fixture$table), " ADD COLUMN changed integer"))
  expect_error(
    epi_eda_longitudinal(catalogue, fixture$spec, "entity", "visit", variables = "score"),
    "catalogue"
  )
  function_name <- "raise_private_longitudinal_error"
  view_name <- "failing_panel"
  function_sql <- paste0(
    longitudinal_eda_pg_quote(con, fixture$schema), ".",
    as.character(DBI::dbQuoteIdentifier(con, function_name))
  )
  DBI::dbExecute(con, paste0(
    "CREATE FUNCTION ", function_sql,
    "() RETURNS double precision LANGUAGE plpgsql AS $body$ BEGIN RAISE EXCEPTION '",
    private, "'; END $body$"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", longitudinal_eda_pg_quote(con, fixture$schema, view_name),
    " AS SELECT entity, visit, ", function_sql, "() AS score, state FROM ",
    longitudinal_eda_pg_quote(con, fixture$schema, fixture$table)
  ))
  failing <- epi_eda_postgres_source(con, fixture$schema, view_name)
  failure <- tryCatch(
    epi_eda_longitudinal(failing, fixture$spec, "entity", "visit", variables = "score"),
    error = identity
  )
  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "restricted database logs")
  expect_false(grepl(private, conditionMessage(failure), fixed = TRUE))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("interior gaps, empty time points and issue ordering have literal parity", {
  con <- longitudinal_eda_pg_connection()
  gap_rows <- data.frame(
    entity = c("gap", "gap", "complete", "complete", "complete"),
    visit = c("baseline", "month_2", "baseline", "month_1", "month_2"),
    score = c(1, 3, 1, 2, 3), state = "A", stringsAsFactors = FALSE
  )
  fixture <- longitudinal_eda_pg_panel(con, gap_rows, "le_gap")
  on.exit(longitudinal_eda_pg_cleanup(con, fixture), add = TRUE)
  frame <- epi_eda_longitudinal(gap_rows, fixture$spec, "entity", "visit", variables = "score")
  database <- epi_eda_longitudinal(fixture$source, fixture$spec, "entity", "visit", variables = "score")
  expect_identical(frame$followup$gap_status, data.frame(
    has_gap = c(FALSE, TRUE), n_entities = c(1, 1), stringsAsFactors = FALSE
  ))
  expect_identical(database$followup$gap_status, frame$followup$gap_status)

  empty_rows <- gap_rows[gap_rows$visit != "month_1", , drop = FALSE]
  empty_fixture <- longitudinal_eda_pg_panel(con, empty_rows, "le_empty")
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", longitudinal_eda_pg_quote(con, empty_fixture$schema), " CASCADE"))
    }
  }, add = TRUE)
  empty_frame <- epi_eda_longitudinal(empty_rows, fixture$spec, "entity", "visit", variables = "score")
  empty_database <- epi_eda_longitudinal(empty_fixture$source, fixture$spec, "entity", "visit", variables = "score")
  expect_identical(empty_frame$issues$issue_code, "zero_observation_timepoint")
  expect_identical(empty_frame$issues$n_affected, 0)
  expect_identical(empty_database$issues, empty_frame$issues)

  conflict_rows <- data.frame(
    entity = rep("conflict", 4L),
    visit = c("baseline", "baseline", "month_1", "month_1"),
    score = c(1, 2, 3, 4), state = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  conflict_fixture <- longitudinal_eda_pg_panel(con, conflict_rows, "le_issues")
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste0("DROP SCHEMA IF EXISTS ", longitudinal_eda_pg_quote(con, conflict_fixture$schema), " CASCADE"))
    }
  }, add = TRUE)
  conflict_frame <- epi_eda_longitudinal(
    conflict_rows, fixture$spec, "entity", "visit", variables = c("score", "state")
  )
  conflict_database <- epi_eda_longitudinal(
    conflict_fixture$source, fixture$spec, "entity", "visit", variables = c("score", "state")
  )
  expect_identical(conflict_database$issues, conflict_frame$issues)
  expect_identical(
    conflict_frame$issues$issue_code,
    c("duplicate_id_time", rep("conflicting_variable_cell", 4L), "zero_observation_timepoint")
  )
  expect_identical(conflict_frame$issues$time_index, c(NA_integer_, 1L, 1L, 2L, 2L, 3L))
  expect_identical(conflict_frame$issues$variable_index, c(NA_integer_, 1L, 2L, 1L, 2L, NA_integer_))
  expect_identical(conflict_frame$issues$n_affected, c(2, 1, 1, 1, 1, 0))
})
