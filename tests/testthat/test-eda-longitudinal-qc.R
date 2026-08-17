context("longitudinal PostgreSQL QC contract")

longitudinal_fake_source <- function(con, entity_type = "text", key_type = "text") {
  columns <- data.frame(
    name = c("entity", "key"),
    base_udt_name = c(entity_type, key_type),
    collation_deterministic = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      con = con,
      schema = "synthetic_schema",
      relation = "synthetic_relation",
      relation_kind = "table",
      columns = columns,
      source_version = "postgres-source-1"
    ),
    class = c("epi_eda_postgres_source", "list")
  )
}

test_that("ordered source and declaration validation is deterministic", {
  validate <- getFromNamespace("longitudinal_qc_inputs", "episcout")
  con <- new.env(parent = emptyenv())
  other_con <- new.env(parent = emptyenv())
  first <- longitudinal_fake_source(con)
  second <- longitudinal_fake_source(con)

  observed <- validate(
    list(later = second, earlier = first), "entity", "key"
  )
  expect_identical(observed$period_labels, c("later", "earlier"))
  expect_identical(observed$entity_id, "entity")
  expect_identical(observed$record_key, "key")

  expect_error(validate(list(first), "entity", NULL), "at least two")
  expect_error(validate(list(first, second), "entity", NULL), "named list")
  expect_error(
    validate(list(period = first, period = second), "entity", NULL),
    "unique"
  )
  expect_error(
    validate(list("bad\nlabel" = first, period = second), "entity", NULL),
    "control"
  )
  expect_error(
    validate(list(one = first, two = list()), "entity", NULL),
    "Every sources element"
  )
  expect_error(
    validate(
      list(one = first, two = longitudinal_fake_source(other_con)),
      "entity", NULL
    ),
    "share one"
  )
  expect_error(
    validate(list(one = first, two = second), "entity;DROP", NULL),
    "SQL fragments"
  )
  expect_error(
    validate(list(one = first, two = second), "entity", c("key", "key")),
    "unique"
  )
})

test_that("column contexts enforce the frozen entity and record-key types", {
  context_builder <- getFromNamespace("longitudinal_qc_context", "episcout")
  con <- new.env(parent = emptyenv())
  text_sources <- list(
    one = longitudinal_fake_source(con, "text", "text"),
    two = longitudinal_fake_source(con, "varchar", "text")
  )
  observed <- context_builder(
    text_sources, names(text_sources), "entity", "key"
  )
  expect_identical(observed$entity_family, "text")
  expect_identical(observed$record_key, "key")

  integral_sources <- list(
    one = longitudinal_fake_source(con, "int2", "uuid"),
    two = longitudinal_fake_source(con, "int8", "uuid")
  )
  expect_identical(
    context_builder(
      integral_sources, names(integral_sources), "entity", "key"
    )$entity_family,
    "integer"
  )

  mixed <- text_sources
  mixed[[2L]] <- longitudinal_fake_source(con, "uuid", "text")
  expect_error(
    context_builder(mixed, names(mixed), "entity", NULL),
    "one compatible"
  )
  unsupported <- text_sources
  unsupported[[1L]]$columns$base_udt_name[[2L]] <- "jsonb"
  unsupported[[2L]]$columns$base_udt_name[[2L]] <- "jsonb"
  expect_error(
    context_builder(
      unsupported, names(unsupported), "entity", "key"
    ),
    "supported PostgreSQL base type"
  )
  inconsistent_key <- text_sources
  inconsistent_key[[2L]]$columns$base_udt_name[[2L]] <- "uuid"
  expect_error(
    context_builder(
      inconsistent_key, names(inconsistent_key), "entity", "key"
    ),
    "one supported"
  )
  nondeterministic_entity <- text_sources
  nondeterministic_entity[[1L]]$columns$collation_deterministic[[1L]] <- FALSE
  expect_error(
    context_builder(
      nondeterministic_entity, names(nondeterministic_entity), "entity", NULL
    ),
    "deterministic"
  )
  nondeterministic_key <- text_sources
  nondeterministic_key[[2L]]$columns$collation_deterministic[[2L]] <- FALSE
  expect_error(
    context_builder(
      nondeterministic_key, names(nondeterministic_key), "entity", "key"
    ),
    "deterministic"
  )
})

test_that("longitudinal counts stop before imprecise double conversion", {
  checked_count <- getFromNamespace(
    "longitudinal_qc_checked_count", "episcout"
  )
  expect_identical(checked_count("0"), 0)
  expect_identical(checked_count("00012"), 12)
  expect_identical(checked_count("9007199254740991"), 9007199254740991)
  expect_type(checked_count("9007199254740991"), "double")
  expect_error(checked_count("9007199254740992"), "exact base-R double")
  expect_error(
    checked_count("9007199254740992", "longitudinal n_union"),
    "longitudinal n_union exceeds"
  )
  expect_error(checked_count("-1"), "exact non-negative decimal")
  expect_error(checked_count("1.5"), "exact non-negative decimal")
  expect_error(checked_count(NA_character_), "exact non-negative decimal")
  expect_error(checked_count(c("1", "2")), "exact non-negative decimal")
})

test_that("technical issues have one typed and deterministic contract", {
  make_issues <- getFromNamespace("longitudinal_qc_issues", "episcout")
  period_summary <- data.frame(
    period_label = c("empty", "affected", "clear"),
    n_rows = c(0, 5, 2),
    n_entity_null = c(0, 1, 0),
    n_entity_blank = c(0, 0, 0),
    n_missing_record_key = c(0, 2, 0),
    n_duplicate_record_key_excess = c(0, 1, 0),
    stringsAsFactors = FALSE
  )
  observed <- make_issues(period_summary, "persona", c("clave_a", "clave_b"))
  expect_identical(
    names(observed),
    c(
      "issue_code", "severity", "period_index", "period", "variable",
      "n_affected", "message"
    )
  )
  expect_identical(
    observed$issue_code,
    c(
      "empty_period", "invalid_entity_id", "missing_record_key",
      "duplicate_record_key"
    )
  )
  expect_identical(observed$severity, rep("warning", 4L))
  expect_identical(observed$period_index, c(1L, 2L, 2L, 2L))
  expect_identical(observed$n_affected, c(0, 1, 2, 1))
  expect_identical(observed$variable, c(NA_character_, "persona", "clave_a,clave_b", "clave_a,clave_b"))
  expect_type(observed$period_index, "integer")
  expect_type(observed$n_affected, "double")

  empty <- make_issues(transform(
    period_summary[3L, , drop = FALSE],
    n_missing_record_key = NA_real_,
    n_duplicate_record_key_excess = NA_real_
  ))
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(observed))
  expect_type(empty$period_index, "integer")
  expect_type(empty$n_affected, "double")
})

test_that("zero denominators and empty histories retain scalar types", {
  proportion <- getFromNamespace("longitudinal_qc_proportion", "episcout")
  history_limit <- getFromNamespace("longitudinal_history_limit", "episcout")
  empty_history <- getFromNamespace(
    "longitudinal_qc_empty_history", "episcout"
  )()
  expect_true(is.na(proportion(0, 0)))
  expect_identical(proportion(1, 2), 0.5)
  expect_identical(history_limit(2L), 4L)
  expect_identical(history_limit(4L), 20L)
  expect_identical(
    names(empty_history),
    c(
      "first_period_index", "first_period_label", "last_period_index",
      "last_period_label", "periods_observed", "gap_periods", "has_gap",
      "n_entities", "proportion_denominator", "proportion"
    )
  )
  expect_type(empty_history$first_period_index, "integer")
  expect_type(empty_history$periods_observed, "integer")
  expect_type(empty_history$n_entities, "double")
  expect_type(empty_history$proportion, "double")
})

test_that("period summaries retain exact columns when record keys are absent", {
  summarise <- getFromNamespace("longitudinal_qc_period_summary", "episcout")
  context <- list(
    sources = list(list(con = NULL), list(con = NULL)),
    period_labels = c("first", "second"),
    record_key = NULL
  )
  aggregate <- data.frame(
    n_rows = "2",
    n_entity_null = "0",
    n_entity_blank = "0",
    n_entity_nonblank = "2",
    n_valid_entity_rows = "2",
    n_distinct_entities = "1",
    n_repeated_entity_rows = "2",
    n_repeated_entity_excess = "1",
    max_entity_frequency = "2",
    stringsAsFactors = FALSE
  )
  observed <- with_mocked_bindings(
    summarise(context),
    longitudinal_qc_period_query = function(...) "aggregate query",
    eda_db_fetch = function(...) aggregate,
    .package = "episcout"
  )
  expect_identical(
    names(observed),
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
  expect_identical(observed$period_index, 1:2)
  expect_identical(observed$n_rows, c(2, 2))
  expect_true(all(vapply(observed[3:18], is.double, logical(1))))
  expect_true(all(vapply(observed[12:18], function(value) {
    all(is.na(value))
  }, logical(1))))
})

test_that("longitudinal transaction lifecycle failures remain value-free", {
  testthat::skip_if_not_installed("RSQLite")
  transaction <- getFromNamespace("longitudinal_qc_transaction", "episcout")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- structure(
    list(con = con), class = c("epi_eda_postgres_source", "list")
  )
  sources <- list(source, source)

  begin_error <- tryCatch(
    with_mocked_bindings(
      transaction(sources, 1L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_begin = function(...) stop("BEGIN_VALUE_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(begin_error), "could not begin")
  expect_false(grepl("CANARY", conditionMessage(begin_error), fixed = TRUE))

  commit_error <- tryCatch(
    with_mocked_bindings(
      transaction(sources, 1L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_statement = function(...) invisible(TRUE),
      eda_db_commit = function(...) stop("COMMIT_VALUE_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(commit_error), "could not commit safely")
  expect_false(grepl("CANARY", conditionMessage(commit_error), fixed = TRUE))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT 1 AS reusable")[["reusable"]], 1L
  )
})
