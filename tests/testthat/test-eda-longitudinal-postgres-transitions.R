context("live PostgreSQL longitudinal state transitions")

transition_live_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    testthat::skip(
      "Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests."
    )
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

transition_live_suffix <- function() {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
}

transition_live_quote <- function(con, schema, relation = NULL) {
  identifier <- if (is.null(relation)) {
    schema
  } else {
    DBI::Id(schema = schema, table = relation)
  }
  as.character(DBI::dbQuoteIdentifier(con, identifier))
}

transition_live_fixture <- function(con) {
  suffix <- transition_live_suffix()
  schema <- paste0("longitudinal_transitions_", suffix)
  relations <- c("transition period one", "transition period two", "transition period three")
  DBI::dbExecute(con, paste("CREATE SCHEMA", transition_live_quote(con, schema)))
  common <- paste(
    "entity_value text COLLATE \"C\", row_marker text COLLATE \"C\",",
    "state text COLLATE \"C\", open_state text COLLATE \"C\",",
    "binary_state boolean, measure double precision, invalid_missing integer,"
  )
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", transition_live_quote(con, schema, relations[[1L]]),
    " (", common,
    "absent_state text COLLATE \"C\", incompatible_state text COLLATE \"C\")"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", transition_live_quote(con, schema, relations[[2L]]),
    " (", common, "incompatible_state double precision)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", transition_live_quote(con, schema, relations[[3L]]),
    " (", common,
    "absent_state text COLLATE \"C\", incompatible_state text COLLATE \"C\")"
  ))
  entities <- paste0("private_entity_", seq_len(10L), "_", suffix)
  markers <- paste0("private_row_", seq_len(24L), "_", suffix)
  first <- data.frame(
    entity_value = c(
      entities[[1L]], entities[[1L]], entities[[1L]], entities[[2L]],
      entities[[3L]], entities[[3L]], entities[[4L]], entities[[5L]],
      entities[[8L]], entities[[9L]], entities[[10L]], NA, "  "
    ),
    row_marker = markers[1:13],
    state = c(
      "A", "A", NA, "B", "A", "B", NA, "A", "A", "B", "A", NA, "B"
    ),
    open_state = c(
      "A", "A", NA, "B", "A", "B", NA, "A", "A", "B", "A", NA, "B"
    ),
    binary_state = c(
      TRUE, TRUE, NA, FALSE, TRUE, FALSE, NA, TRUE, TRUE, FALSE,
      TRUE, NA, FALSE
    ),
    measure = seq_len(13L),
    invalid_missing = seq_len(13L),
    absent_state = rep("A", 13L),
    incompatible_state = rep("A", 13L),
    stringsAsFactors = FALSE
  )
  second <- data.frame(
    entity_value = c(
      entities[[1L]], entities[[2L]], entities[[2L]], entities[[3L]],
      entities[[4L]], entities[[6L]], entities[[8L]], entities[[9L]],
      entities[[10L]]
    ),
    row_marker = markers[14:22],
    state = c("B", "B", "B", NA, "B", "C", "A", "A", NA),
    open_state = c("A", "A", "A", NA, "B", "C", "A", "A", NA),
    binary_state = c(FALSE, FALSE, FALSE, NA, FALSE, TRUE, TRUE, TRUE, NA),
    measure = 14:22,
    invalid_missing = 14:22,
    incompatible_state = 14:22,
    stringsAsFactors = FALSE
  )
  third <- data.frame(
    entity_value = c(entities[[3L]], entities[[7L]]),
    row_marker = markers[23:24],
    state = c(NA, "A"),
    open_state = c(NA, "A"),
    binary_state = c(NA, TRUE),
    measure = 23:24,
    invalid_missing = 23:24,
    absent_state = c("A", "A"),
    incompatible_state = c("A", "A"),
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(
    con, DBI::Id(schema = schema, table = relations[[1L]]), first
  )
  DBI::dbAppendTable(
    con, DBI::Id(schema = schema, table = relations[[2L]]), second
  )
  DBI::dbAppendTable(
    con, DBI::Id(schema = schema, table = relations[[3L]]), third
  )
  spec <- data.frame(
    name = c(
      "entity_value", "state", "open_state", "binary_state", "measure",
      "invalid_missing", "absent_state", "incompatible_state"
    ),
    label = c(
      "Entity", "State", "Open state", "Binary state", "Measure",
      "Invalid missing", "Absent state", "Incompatible state"
    ),
    database_type = c(
      "text", "text", "text", "boolean", "numeric", "integer", "text", "text"
    ),
    analysis_type = c(
      "text", "categorical", "categorical", "binary", "numeric",
      "categorical", "categorical", "categorical"
    ),
    role = c("identifier", rep("measure", 7L)),
    levels = c("", "B;A;D;B", rep("", 6L)),
    missing_codes = c(rep("", 5L), "not-an-integer", "", ""),
    stringsAsFactors = FALSE
  )
  list(
    schema = schema,
    relations = relations,
    sources = list(
      baseline = epi_eda_postgres_source(con, schema, relations[[1L]]),
      follow_up = epi_eda_postgres_source(con, schema, relations[[2L]]),
      later = epi_eda_postgres_source(con, schema, relations[[3L]])
    ),
    spec = spec,
    entities = entities,
    markers = markers
  )
}

transition_live_cleanup <- function(con, fixture) {
  if (DBI::dbIsValid(con)) {
    DBI::dbExecute(con, paste0(
      "DROP SCHEMA IF EXISTS ",
      transition_live_quote(con, fixture$schema), " CASCADE"
    ))
    DBI::dbDisconnect(con)
  }
}

transition_live_row_counts <- function(con, fixture) {
  vapply(fixture$relations, function(relation) {
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*)::integer AS n FROM ",
        transition_live_quote(con, fixture$schema, relation)
      )
    )$n[[1L]]
  }, integer(1))
}

test_that("live transitions match hand-derived retained entity states", {
  con <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(transition_live_cleanup(con, fixture), add = TRUE)
  rows_before <- transition_live_row_counts(con, fixture)

  observed <- epi_eda_longitudinal_transitions(
    fixture$sources, "entity_value", fixture$spec, "state", max_levels = 4L
  )
  repeated <- epi_eda_longitudinal_transitions(
    fixture$sources, "entity_value", fixture$spec, "state", max_levels = 4L
  )

  expect_identical(observed, repeated)
  expect_identical(
    class(observed), c("epi_eda_longitudinal_transitions", "list")
  )
  expect_identical(names(observed), c(
    "metadata", "state_audit", "transition_summary", "transition_counts", "issues"
  ))
  expect_identical(
    observed$metadata$contract_version, "longitudinal-transitions-1"
  )
  expect_identical(observed$metadata$n_periods, 3L)
  expect_identical(observed$metadata$n_variables, 1L)
  expect_identical(observed$metadata$resolved_variables[[1L]], "state")
  expect_identical(observed$metadata$entity_id, "entity_value")
  expect_identical(observed$metadata$count_contract, "exact-base-r-double")
  expect_identical(observed$metadata$count_maximum, 9007199254740991)
  expect_identical(
    observed$metadata$snapshot_mode, "REPEATABLE READ READ ONLY"
  )

  audit <- observed$state_audit
  expect_identical(audit$n_valid_entities, c(8, 8, 2))
  expect_identical(audit$n_usable_state, c(6, 6, 1))
  expect_identical(audit$n_missing_state, c(1, 2, 1))
  expect_identical(audit$n_conflicting_state, c(1, 0, 0))
  expect_identical(audit$status, rep("available", 3L))
  expect_true(all(vapply(audit[5:8], is.double, logical(1))))

  summary <- observed$transition_summary
  expect_identical(summary$n_retained, c(7, 1))
  expect_identical(summary$n_eligible, c(4, 0))
  expect_identical(summary$n_excluded_missing, c(2, 1))
  expect_identical(summary$n_excluded_conflict, c(1, 0))
  expect_identical(summary$n_transition_cells, c(16L, 16L))
  expect_identical(summary$eligible_denominator, c(4, 0))
  expect_identical(summary$status, c("available", "unavailable"))
  expect_identical(summary$reason, c(NA_character_, "zero_eligible"))
  expect_identical(
    summary$n_retained,
    summary$n_eligible + summary$n_excluded_missing +
      summary$n_excluded_conflict
  )

  first_counts <- observed$transition_counts[
    observed$transition_counts$left_period_index == 1L, , drop = FALSE
  ]
  expect_identical(unique(first_counts$from_state), c("B", "A", "D", "C"))
  expect_identical(first_counts$n, c(
    1, 1, 0, 0,
    1, 1, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  ))
  expect_equal(sum(first_counts$proportion), 1)
  expect_identical(first_counts$from_is_declared, rep(c(
    TRUE, TRUE, TRUE, FALSE
  ), each = 4L))
  expect_identical(
    first_counts$from_is_unexpected, !first_counts$from_is_declared
  )
  later_counts <- observed$transition_counts[
    observed$transition_counts$left_period_index == 2L, , drop = FALSE
  ]
  expect_identical(later_counts$n, rep(0, 16L))
  expect_true(all(is.na(later_counts$proportion)))
  expect_identical(later_counts$reason, rep("zero_eligible", 16L))

  expect_identical(
    observed$issues$issue_code, c("conflicting_state", "zero_eligible")
  )
  expect_identical(observed$issues$n_affected, c(1, 1))
  expect_identical(observed$issues$period_index, c(1L, NA_integer_))
  expect_identical(observed$issues$left_period_index, c(NA_integer_, 2L))
  expect_identical(transition_live_row_counts(con, fixture), rows_before)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)

  text <- paste(capture.output(str(observed)), collapse = "\n")
  print_text <- paste(capture.output(print(observed)), collapse = "\n")
  for (private in c(fixture$entities, fixture$markers)) {
    expect_false(grepl(private, text, fixed = TRUE))
    expect_false(grepl(private, print_text, fixed = TRUE))
  }
})

test_that("live unavailability, privacy refusal and bounds are explicit", {
  con <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(transition_live_cleanup(con, fixture), add = TRUE)

  expect_error(
    epi_eda_longitudinal_transitions(
      fixture$sources, "entity_value", fixture$spec, "entity_value"
    ),
    "identifier"
  )
  observed <- epi_eda_longitudinal_transitions(
    fixture$sources,
    "entity_value",
    fixture$spec,
    c("measure", "invalid_missing", "absent_state", "incompatible_state"),
    max_levels = 4L
  )
  expect_identical(observed$issues$issue_code, c(
    "unsupported_analysis_type", "invalid_missing_contract",
    "unsupported_analysis_type", "invalid_missing_contract",
    "absent_variable", "incompatible_type",
    "unsupported_analysis_type", "invalid_missing_contract"
  ))
  expect_identical(observed$issues$severity, c(
    "warning", "error", "warning", "error", "error", "error", "warning", "error"
  ))
  expect_true(all(observed$issues$n_affected >= 0))
  unavailable <- observed$state_audit$reason[observed$state_audit$status == "unavailable"]
  expect_identical(unavailable, observed$issues$issue_code)
  expect_equal(nrow(observed$transition_counts), 0L)
  expect_identical(
    observed$transition_summary$n_transition_cells,
    rep(0L, nrow(observed$transition_summary))
  )
  expect_true(all(is.na(observed$transition_summary$n_eligible)))
  expect_true(all(is.na(
    observed$transition_summary$eligible_denominator
  )))

  expect_error(
    epi_eda_longitudinal_transitions(
      fixture$sources, "entity_value", fixture$spec, "state", max_levels = 2L
    ),
    "declared transition domain"
  )
  expect_error(
    epi_eda_longitudinal_transitions(
      fixture$sources, "entity_value", fixture$spec, "state", max_levels = 3L
    ),
    "adjacent union"
  )
  expect_error(
    epi_eda_longitudinal_transitions(
      fixture$sources, "entity_value", fixture$spec, "open_state", max_levels = 2L
    ),
    "adjacent union"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("literal 51-state period and adjacent union fail without an object", {
  con <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(transition_live_cleanup(con, fixture), add = TRUE)

  write_states <- function(relation, states) {
    DBI::dbWriteTable(
      con,
      DBI::Id(schema = fixture$schema, table = relation),
      data.frame(
        entity_value = paste0("bounded_entity_", seq_along(states)),
        state = states,
        stringsAsFactors = FALSE
      )
    )
    epi_eda_postgres_source(con, fixture$schema, relation)
  }
  levels_51 <- sprintf("level-%02d", seq_len(51L))
  observed_left <- write_states("observed 51 left", levels_51)
  observed_right <- write_states("observed 51 right", levels_51[[1L]])
  observed_error <- tryCatch(
    epi_eda_longitudinal_transitions(
      list(left = observed_left, right = observed_right),
      "entity_value", fixture$spec, "state", max_levels = 50L
    ),
    error = identity
  )
  expect_s3_class(observed_error, "error")
  expect_match(conditionMessage(observed_error), "exceeds max_levels")

  union_left <- write_states("union 25 left", levels_51[1:25])
  union_right <- write_states("union 26 right", levels_51[26:51])
  union_error <- tryCatch(
    epi_eda_longitudinal_transitions(
      list(left = union_left, right = union_right),
      "entity_value", fixture$spec, "state", max_levels = 50L
    ),
    error = identity
  )
  expect_s3_class(union_error, "error")
  expect_match(conditionMessage(union_error), "adjacent union")
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("empty periods and undeclared binary states remain explicit", {
  con <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(transition_live_cleanup(con, fixture), add = TRUE)
  empty_relation <- "empty transition period"
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = fixture$schema, table = empty_relation),
    data.frame(entity_value = character(), state = character())
  )
  empty_source <- epi_eda_postgres_source(
    con, fixture$schema, empty_relation
  )
  empty <- epi_eda_longitudinal_transitions(
    list(baseline = fixture$sources[[1L]], empty = empty_source),
    "entity_value", fixture$spec, "state", max_levels = 4L
  )
  expect_identical(empty$state_audit$n_valid_entities, c(8, 0))
  expect_identical(empty$state_audit$n_usable_state, c(6, 0))
  expect_identical(empty$state_audit$n_missing_state, c(1, 0))
  expect_identical(empty$state_audit$n_conflicting_state, c(1, 0))
  expect_identical(empty$transition_summary$n_retained, 0)
  expect_identical(empty$transition_summary$n_eligible, 0)
  expect_identical(empty$transition_summary$n_transition_cells, 9L)
  expect_identical(empty$transition_summary$reason, "zero_eligible")

  binary <- epi_eda_longitudinal_transitions(
    fixture$sources, "entity_value", fixture$spec, "binary_state",
    max_levels = 4L
  )
  expect_identical(
    unique(binary$transition_counts$from_state), c("FALSE", "TRUE")
  )
  expect_true(all(is.na(binary$transition_counts$from_is_declared)))
  expect_true(all(is.na(binary$transition_counts$to_is_declared)))
  expect_false(any(binary$transition_counts$from_is_unexpected))
  expect_false(any(binary$transition_counts$to_is_unexpected))
})

test_that("the complete transition API retains one concurrent snapshot", {
  con <- transition_live_connection()
  writer <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      transition_live_cleanup(con, fixture)
    },
    add = TRUE
  )
  original <- getFromNamespace("lt_population_counts", "episcout")
  inserted <- FALSE
  target <- transition_live_quote(
    writer, fixture$schema, fixture$relations[[2L]]
  )
  observed <- with_mocked_bindings(
    epi_eda_longitudinal_transitions(
      fixture$sources, "entity_value", fixture$spec, "state", max_levels = 4L
    ),
    lt_population_counts = function(context) {
      value <- original(context)
      if (!inserted) {
        inserted <<- TRUE
        DBI::dbExecute(
          writer,
          paste0(
            "INSERT INTO ", target,
            " (entity_value, row_marker, state) VALUES ($1, $2, 'D')"
          ),
          params = list(fixture$entities[[2L]], "concurrent_private_row")
        )
      }
      value
    },
    .package = "episcout"
  )
  expect_identical(observed$state_audit$n_conflicting_state, c(1, 0, 0))
  later <- epi_eda_longitudinal_transitions(
    fixture$sources, "entity_value", fixture$spec, "state", max_levels = 4L
  )
  expect_identical(later$state_audit$n_conflicting_state, c(1, 1, 0))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("transition query failures are sanitised and roll back", {
  con <- transition_live_connection()
  fixture <- transition_live_fixture(con)
  on.exit(transition_live_cleanup(con, fixture), add = TRUE)
  function_name <- "raise private transition error"
  view_name <- "transition failing view"
  marker <- paste0("private_database_error_", transition_live_suffix())
  function_sql <- paste0(
    transition_live_quote(con, fixture$schema), ".",
    as.character(DBI::dbQuoteIdentifier(con, function_name))
  )
  DBI::dbExecute(con, paste0(
    "CREATE FUNCTION ", function_sql,
    "() RETURNS text LANGUAGE plpgsql AS $body$ BEGIN RAISE EXCEPTION '",
    marker, "'; END $body$"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", transition_live_quote(con, fixture$schema, view_name),
    " AS SELECT entity_value, ", function_sql, "() AS state FROM ",
    transition_live_quote(con, fixture$schema, fixture$relations[[2L]])
  ))
  sources <- list(
    baseline = fixture$sources[[1L]],
    failing = epi_eda_postgres_source(con, fixture$schema, view_name)
  )
  observed <- tryCatch(
    epi_eda_longitudinal_transitions(
      sources, "entity_value", fixture$spec, "state", max_levels = 4L
    ),
    error = identity
  )
  expect_s3_class(observed, "error")
  expect_match(conditionMessage(observed), "restricted database logs")
  expect_false(grepl(marker, conditionMessage(observed), fixed = TRUE))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})
