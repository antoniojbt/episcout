context("PostgreSQL stratified aggregate summaries")

stratified_postgres_connection <- function() {
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

stratified_postgres_fixture <- function(con) {
  schema <- paste0("epi_strat_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- paste0(
    schema_sql,
    ".",
    as.character(DBI::dbQuoteIdentifier(con, "stratified fixture"))
  )
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql, " (",
    "arm text, value double precision, status text, note text, visit date, ",
    "participant_id bigint, normality double precision, moment timestamptz, ",
    "payload integer[])"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES ",
    "('A', 1, 'yes', 'secret-a', DATE '2024-01-01', 1, 1, TIMESTAMPTZ '2024-01-01 00:00:00Z', ARRAY[1]),",
    "('A', 3, 'no', 'secret-b', DATE '2024-01-03', 2, 2, TIMESTAMPTZ '2024-01-01 00:00:02Z', ARRAY[2]),",
    "('B', 2, 'yes', 'secret-c', DATE '2024-01-02', 3, 3, TIMESTAMPTZ '2024-01-01 01:00:00+01', ARRAY[3]),",
    "('C', 'Infinity', 'other', 'secret-d', DATE '2024-01-04', 4, 4, TIMESTAMPTZ '2024-01-01 00:00:06Z', ARRAY[4]),",
    "('MISS', 999, 'NA', 'secret-e', NULL, 5, 8, NULL, ARRAY[5]),",
    "(NULL, NULL, NULL, NULL, NULL, 6, 16, NULL, ARRAY[6])"
  ))
  spec <- data.frame(
    name = c(
      "arm", "value", "status", "note", "visit", "participant_id",
      "normality", "moment", "absent"
    ),
    label = c(
      "Study arm", "Value", "Status", "Note", "Visit", "Participant",
      "Normality", "Moment", "Absent"
    ),
    type = c(
      "categorical", "numeric", "categorical", "text", "date", "integer",
      "numeric", "datetime", "text"
    ),
    role = c(
      "exposure", "measure", "measure", "measure", "measure", "identifier",
      "measure", "time", "measure"
    ),
    levels = c("B;A;D", "", "no;yes;unused", "", "", "", "", "", ""),
    missing_codes = c("MISS", "999", "", "", "", "", "", "", ""),
    required = c(rep(TRUE, 8), FALSE),
    stringsAsFactors = FALSE
  )
  frame <- data.frame(
    arm = c("A", "A", "B", "C", "MISS", NA),
    value = c(1, 3, 2, Inf, 999, NA_real_),
    status = c("yes", "no", "yes", "other", "NA", NA),
    note = c("secret-a", "secret-b", "secret-c", "secret-d", "secret-e", NA),
    visit = as.Date(c("2024-01-01", "2024-01-03", "2024-01-02", "2024-01-04", NA, NA)),
    participant_id = 1:6,
    normality = c(1, 2, 3, 4, 8, 16),
    moment = as.POSIXct(c(
      "2024-01-01 00:00:00", "2024-01-01 00:00:02",
      "2024-01-01 00:00:00", "2024-01-01 00:00:06", NA, NA
    ), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  list(
    schema = schema,
    relation = "stratified fixture",
    spec = spec,
    frame = frame
  )
}

stratified_postgres_cleanup <- function(con, fixture) {
  if (DBI::dbIsValid(con)) {
    DBI::dbExecute(
      con,
      paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE")
    )
    DBI::dbDisconnect(con)
  }
}

test_that("PostgreSQL stratification matches supported data-frame aggregates", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)

  observed <- epi_eda_profile_stratified(source, fixture$spec, "arm")
  expected <- epi_eda_profile_stratified(fixture$frame, fixture$spec, "arm")

  expect_s3_class(observed, "epi_eda_stratified")
  expect_named(observed, names(expected))
  expect_identical(observed$groups, expected$groups)
  expect_identical(
    observed$metadata[setdiff(names(expected$metadata), character())],
    expected$metadata
  )
  expect_identical(observed$metadata$source_contract, "postgres-source-1")
  expect_identical(
    observed$metadata$normality_contract,
    "not_calculated_no_analysis_value_collection"
  )
  expect_equal(observed$variables, expected$variables, tolerance = 1e-12)
  expect_equal(
    observed$numeric[setdiff(names(observed$numeric), "shapiro_p")],
    expected$numeric[setdiff(names(expected$numeric), "shapiro_p")],
    tolerance = 1e-10
  )
  expect_true(all(is.na(observed$numeric$shapiro_p)))
  expect_false(is.na(expected$numeric$shapiro_p[
    expected$numeric$name == "normality" & expected$numeric$is_overall
  ]))
  expect_equal(observed$categorical, expected$categorical, tolerance = 1e-12)
  expect_identical(observed$text, expected$text)
  expect_identical(observed$temporal, expected$temporal)
  pacific <- withr::with_envvar(
    c(TZ = "Pacific/Auckland"),
    epi_eda_profile_stratified(source, fixture$spec, "arm")
  )
  expect_identical(observed$temporal, pacific$temporal)
  expect_true(all(c("absent", "payload") %in% observed$skipped$name))
  expect_false(grepl(
    "secret-a",
    paste(capture.output(str(observed)), collapse = " "),
    fixed = TRUE
  ))

  table1 <- epi_eda_table1(observed)
  display <- epi_eda_categorical_display(observed, "column")
  expect_s3_class(table1, "data.frame")
  expect_s3_class(display, "data.frame")
  expect_true(all(!is.na(table1$denominator)))
  expect_true(all(display$denominator >= 0L))
})

test_that("PostgreSQL grouping preserves omitted, zero and unexpected strata", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)

  observed <- epi_eda_profile_stratified(
    source,
    fixture$spec,
    "arm",
    include_missing_stratum = FALSE
  )

  expect_identical(observed$groups$group_label, c("Overall", "B", "A", "D", "C"))
  expect_identical(observed$groups$n, c(4L, 1L, 2L, 0L, 1L))
  expect_identical(observed$metadata$n_input, 6L)
  expect_identical(observed$metadata$n_included, 4L)
  expect_identical(observed$metadata$n_omitted_missing_stratum, 2L)
  expect_false(any(observed$groups$is_missing_stratum))
  expect_identical(
    sum(observed$groups$n[!observed$groups$is_overall]),
    4L
  )
})

test_that("PostgreSQL stratification never fetches an analysis-value vector", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  queries <- character()
  original <- getFromNamespace("eda_db_fetch", "episcout")
  testthat::local_mocked_bindings(
    eda_db_fetch = function(con,
                            statement,
                            params = list(),
                            query_kind,
                            limit,
                            timing_env = NULL,
                            variable_index = NA_integer_,
                            name = NA_character_) {
      queries <<- c(queries, as.character(statement))
      original(
        con,
        statement,
        params,
        query_kind,
        limit,
        timing_env,
        variable_index,
        name
      )
    },
    .package = "episcout"
  )

  observed <- epi_eda_profile_stratified(source, fixture$spec, "arm")

  expect_s3_class(observed, "epi_eda_stratified")
  expect_gt(length(queries), 0L)
  expect_false(any(grepl(
    "SELECT value FROM v WHERE",
    queries,
    fixed = TRUE
  )))
  expect_true(any(grepl("GROUP BY", queries, fixed = TRUE)))
  expect_true(all(!grepl("secret-a", queries, fixed = TRUE)))
})

test_that("PostgreSQL zero-row and all-missing strata remain explicit", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  relation <- DBI::Id(schema = fixture$schema, table = fixture$relation)
  relation_sql <- as.character(DBI::dbQuoteIdentifier(con, relation))
  DBI::dbExecute(con, paste0("DELETE FROM ", relation_sql))
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)

  empty <- epi_eda_profile_stratified(source, fixture$spec, "arm")
  expect_identical(empty$groups$group_label, c("Overall", "B", "A", "D"))
  expect_true(all(empty$groups$n == 0L))
  expect_identical(empty$metadata$n_input, 0L)

  DBI::dbExecute(con, paste0("INSERT INTO ", relation_sql, " (arm) VALUES (NULL)"))
  all_missing <- epi_eda_profile_stratified(source, fixture$spec, "arm")
  omitted <- epi_eda_profile_stratified(
    source,
    fixture$spec,
    "arm",
    include_overall = FALSE,
    include_missing_stratum = FALSE
  )

  expect_identical(
    all_missing$groups$group_label,
    c("Overall", "B", "A", "D", "Missing")
  )
  expect_identical(all_missing$groups$n, c(1L, 0L, 0L, 0L, 1L))
  expect_identical(omitted$groups$group_label, c("B", "A", "D"))
  expect_true(all(omitted$groups$n == 0L))
  expect_identical(omitted$metadata$n_input, 1L)
  expect_identical(omitted$metadata$n_included, 0L)
  expect_identical(omitted$metadata$n_omitted_missing_stratum, 1L)
})

test_that("PostgreSQL stratification rejects unsafe boundaries", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)

  expect_error(
    epi_eda_profile_stratified(source, fixture$spec, "value"),
    "categorical or binary"
  )
  modified <- source
  modified$relation <- "changed"
  expect_error(
    epi_eda_profile_stratified(modified, fixture$spec, "arm"),
    "not found|unmodified"
  )
  DBI::dbBegin(con)
  expect_error(
    epi_eda_profile_stratified(source, fixture$spec, "arm"),
    "caller-managed transaction"
  )
  DBI::dbRollback(con)
})

test_that("PostgreSQL workflow bundles publish optional stratified aggregates", {
  con <- stratified_postgres_connection()
  fixture <- stratified_postgres_fixture(con)
  on.exit(stratified_postgres_cleanup(con, fixture), add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  output_dir <- tempfile("episcout-stratified-bundle-")
  default_dir <- tempfile("episcout-default-bundle-")
  on.exit(unlink(c(output_dir, default_dir), recursive = TRUE, force = TRUE), add = TRUE)

  default <- epi_eda_db_run(
    source,
    fixture$spec,
    default_dir,
    plots = FALSE
  )
  expect_false(any(c("stratified", "table1") %in% names(default)))
  expect_false(any(grepl("stratified|table1", default$manifest$artifact)))

  queries <- character()
  original <- getFromNamespace("eda_db_fetch", "episcout")
  testthat::local_mocked_bindings(
    eda_db_fetch = function(con,
                            statement,
                            params = list(),
                            query_kind,
                            limit,
                            timing_env = NULL,
                            variable_index = NA_integer_,
                            name = NA_character_) {
      queries <<- c(queries, as.character(statement))
      original(
        con,
        statement,
        params,
        query_kind,
        limit,
        timing_env,
        variable_index,
        name
      )
    },
    .package = "episcout"
  )
  observed <- epi_eda_db_run(
    source,
    fixture$spec,
    output_dir,
    plots = TRUE,
    layout = "delivery",
    strata = "arm",
    table1_basis = "column"
  )
  expect_named(observed, c(names(default), "stratified", "table1"))
  expect_identical(
    observed$table1,
    epi_eda_table1(observed$stratified, "column")
  )
  expect_true(all(c(
    "stratified_groups", "stratified_variables", "stratified_numeric",
    "stratified_categorical", "stratified_text", "stratified_temporal",
    "stratified_skipped", "stratified_metadata", "table1"
  ) %in% observed$manifest$artifact))
  expect_true(all(file.exists(file.path(
    output_dir,
    observed$manifest$path[observed$manifest$type %in% c(
      "stratified_aggregate", "table1"
    )]
  ))))

  read_bundle <- getFromNamespace("eda_db_read_bundle", "episcout")
  parsed <- read_bundle(output_dir)
  expect_true(all(c("stratified_metadata", "table1") %in% names(parsed$tables)))
  report <- paste(readLines(
    file.path(output_dir, "reports", "eda-report.html"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(report, "Stratified summaries", fixed = TRUE)
  expect_match(report, "Table 1", fixed = TRUE)
  expect_false(any(grepl(
    "SELECT value FROM v WHERE",
    queries,
    fixed = TRUE
  )))

  repeated <- epi_eda_db_run(
    source,
    fixture$spec,
    output_dir,
    overwrite = TRUE,
    plots = TRUE,
    layout = "delivery",
    strata = "arm",
    table1_basis = "column"
  )
  expect_identical(repeated$table1, observed$table1)
  expect_error(
    epi_eda_db_run(
      source,
      fixture$spec,
      output_dir,
      overwrite = TRUE,
      plots = TRUE,
      layout = "delivery",
      strata = "arm",
      include_missing_stratum = FALSE,
      table1_basis = "column"
    ),
    "do not match"
  )
  expect_error(
    epi_eda_db_run(
      source,
      fixture$spec,
      tempfile("episcout-stratified-map-rejection-"),
      plots = FALSE,
      maps = TRUE,
      strata = "arm"
    ),
    "cannot collect source-row map data"
  )
})
