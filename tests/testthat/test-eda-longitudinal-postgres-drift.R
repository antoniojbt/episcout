context("live PostgreSQL longitudinal variable drift")

drift_live_connection <- function() {
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

drift_live_suffix <- function() {
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(8L))), collapse = "")
}

drift_live_quote <- function(con, schema, relation = NULL) {
  identifier <- if (is.null(relation)) {
    schema
  } else {
    DBI::Id(schema = schema, table = relation)
  }
  as.character(DBI::dbQuoteIdentifier(con, identifier))
}

drift_live_fixture <- function(con) {
  suffix <- drift_live_suffix()
  schema <- paste0("longitudinal_drift_", suffix)
  first <- "drift period one"
  second <- "drift period two"
  DBI::dbExecute(con, paste("CREATE SCHEMA", drift_live_quote(con, schema)))
  first_sql <- drift_live_quote(con, schema, first)
  second_sql <- drift_live_quote(con, schema, second)
  common <- paste(
    "row_marker text COLLATE \"C\", measure double precision,",
    "reviewed_group text COLLATE \"C\", open_group text COLLATE \"C\",",
    "observed_date date, observed_at timestamptz, note text COLLATE \"C\",",
    "invalid_missing integer,"
  )
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", first_sql, " (", common,
    "incompatible integer, absent_in_followup integer)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", second_sql, " (", common,
    "incompatible text COLLATE \"C\")"
  ))
  markers <- paste0("private_row_marker_", seq_len(8L), "_", suffix)
  first_rows <- data.frame(
    row_marker = markers[1:4],
    measure = c(1, 2, 3, NA),
    reviewed_group = c("A", "A", "B", NA),
    open_group = c("A", "B", "A", NA),
    observed_date = as.Date(c("2020-01-01", "2020-01-03", "2020-01-05", NA)),
    observed_at = as.POSIXct(
      c("2020-01-01", "2020-01-02", "2020-01-03", NA),
      tz = "UTC"
    ),
    note = c("first", NA, "second", "third"),
    invalid_missing = c(1L, 2L, NA_integer_, 3L),
    incompatible = 1:4,
    absent_in_followup = c(10L, 20L, 30L, 40L),
    stringsAsFactors = FALSE
  )
  second_rows <- data.frame(
    row_marker = markers[5:8],
    measure = c(2, 4, 6, 8),
    reviewed_group = c("A", "C", "C", NA),
    open_group = c("A", "C", "A", NA),
    observed_date = as.Date(c(
      "2020-01-02", "2020-01-06", "2020-01-10", "2020-01-14"
    )),
    observed_at = as.POSIXct(
      c("2020-01-02", "2020-01-04", "2020-01-06", "2020-01-08"),
      tz = "UTC"
    ),
    note = c("fourth", "fifth", NA, "sixth"),
    invalid_missing = c(4L, 5L, 6L, NA_integer_),
    incompatible = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, DBI::Id(schema = schema, table = first), first_rows)
  DBI::dbAppendTable(con, DBI::Id(schema = schema, table = second), second_rows)
  spec <- data.frame(
    name = c(
      "measure", "reviewed_group", "open_group", "observed_date",
      "observed_at", "note", "invalid_missing", "absent_in_followup",
      "incompatible"
    ),
    label = c(
      "Measure", "Reviewed group", "Open group", "Observed date",
      "Observed at", "Note", "Invalid missing", "Absent later",
      "Incompatible later"
    ),
    database_type = c(
      "numeric", "text", "text", "date", "datetime", "text",
      "integer", "integer", "integer"
    ),
    analysis_type = c(
      "numeric", "categorical", "categorical", "date", "datetime",
      "text", "integer", "integer", "integer"
    ),
    role = c(
      "measure", "exposure", "exposure", "time", "time", "comment",
      "measure", "measure", "measure"
    ),
    levels = c("", "A;B;D", rep("", 7L)),
    missing_codes = c(rep("", 6L), "not-an-integer", "", ""),
    stringsAsFactors = FALSE
  )
  list(
    schema = schema,
    relations = c(first, second),
    sources = list(
      baseline = epi_eda_postgres_source(con, schema, first),
      follow_up = epi_eda_postgres_source(con, schema, second)
    ),
    spec = spec,
    markers = markers
  )
}

drift_live_cleanup <- function(con, fixture) {
  if (DBI::dbIsValid(con)) {
    DBI::dbExecute(con, paste0(
      "DROP SCHEMA IF EXISTS ",
      drift_live_quote(con, fixture$schema), " CASCADE"
    ))
    DBI::dbDisconnect(con)
  }
}

test_that("live drift matches hand-derived period and adjacent truth", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  variables <- c(
    "reviewed_group", "measure", "observed_date", "observed_at", "note",
    "invalid_missing", "absent_in_followup", "incompatible", "open_group"
  )
  rows_before <- vapply(
    fixture$relations,
    function(relation) {
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT count(*)::integer AS n FROM ",
          drift_live_quote(con, fixture$schema, relation)
        )
      )$n[[1L]]
    },
    integer(1)
  )

  observed <- epi_eda_longitudinal_drift(
    fixture$sources, fixture$spec, variables = variables, max_levels = 4L
  )

  expect_identical(
    class(observed), c("epi_eda_longitudinal_drift", "list")
  )
  expect_identical(
    names(observed),
    c(
      "metadata", "schema", "missingness", "missingness_adjacent",
      "numeric", "numeric_adjacent", "categorical",
      "categorical_adjacent", "temporal", "temporal_adjacent", "skipped"
    )
  )
  expect_identical(observed$metadata$contract_version, "longitudinal-drift-1")
  expect_identical(observed$metadata$n_periods, 2L)
  expect_identical(observed$metadata$n_variables, 9L)
  expect_identical(observed$metadata$resolved_variables[[1L]], variables)
  expect_identical(
    observed$metadata$snapshot_mode, "REPEATABLE READ READ ONLY"
  )
  expect_identical(observed$metadata$count_contract, "canonical-r-integer")
  expect_identical(observed$schema$period_index, rep(1:2, each = 9L))
  expect_identical(observed$schema$variable, rep(variables, 2L))
  expect_identical(
    observed$schema$type_status[observed$schema$variable == "incompatible"],
    c("compatible", "incompatible")
  )

  missing_measure <- observed$missingness[
    observed$missingness$variable == "measure", , drop = FALSE
  ]
  expect_identical(missing_measure$n, c(4L, 4L))
  expect_identical(missing_measure$n_missing, c(1L, 0L))
  expect_identical(missing_measure$n_observed, c(3L, 4L))
  expect_equal(missing_measure$p_missing, c(0.25, 0))
  missing_change <- observed$missingness_adjacent[
    observed$missingness_adjacent$variable == "measure", , drop = FALSE
  ]
  expect_identical(missing_change$absolute_change, -0.25)
  expect_identical(missing_change$relative_denominator, 0.25)
  expect_identical(missing_change$relative_change, -1)

  numeric <- observed$numeric[observed$numeric$variable == "measure", ]
  expect_identical(numeric$n_finite, c(3L, 4L))
  expect_equal(numeric$min, c(1, 2))
  expect_equal(numeric$q1, c(1.5, 3.5))
  expect_equal(numeric$mean, c(2, 5))
  expect_equal(numeric$median, c(2, 5))
  expect_equal(numeric$q3, c(2.5, 6.5))
  expect_equal(numeric$max, c(3, 8))
  expect_equal(numeric$iqr, c(1, 3))
  expect_equal(numeric$sd, c(1, sqrt(20 / 3)))
  numeric_change <- observed$numeric_adjacent[
    observed$numeric_adjacent$variable == "measure", , drop = FALSE
  ]
  expect_equal(numeric_change$mean_change, 3)
  expect_equal(numeric_change$iqr_change, 2)
  expect_false(any(c(
    "shapiro_p", "lower_fence", "upper_fence", "outlier_count",
    "skewness", "kurtosis"
  ) %in% names(observed$numeric)))

  baseline_group <- observed$categorical[
    observed$categorical$period_index == 1L &
      observed$categorical$variable == "reviewed_group",
    ,
    drop = FALSE
  ]
  expect_identical(baseline_group$level, c("A", "B", "D"))
  expect_identical(baseline_group$n, c(2L, 1L, 0L))
  expect_equal(baseline_group$p_total, c(0.5, 0.25, 0))
  expect_equal(baseline_group$p_observed, c(2 / 3, 1 / 3, 0))
  group_change <- observed$categorical_adjacent[
    observed$categorical_adjacent$variable == "reviewed_group", , drop = FALSE
  ]
  expect_identical(group_change$level, c("A", "B", "D", "C"))
  expect_identical(
    group_change$level_status,
    c("present_both", "removed", "absent_both", "introduced")
  )
  expect_equal(
    group_change$p_total_difference[group_change$level == "C"], 0.5
  )
  expect_equal(
    group_change$p_observed_difference[group_change$level == "C"], 2 / 3
  )

  date_rows <- observed$temporal[observed$temporal$variable == "observed_date", ]
  expect_identical(date_rows$unit, c("days", "days"))
  expect_identical(date_rows$min, c("2020-01-01", "2020-01-02"))
  expect_equal(date_rows$range_value, c(4, 12))
  date_change <- observed$temporal_adjacent[
    observed$temporal_adjacent$variable == "observed_date", , drop = FALSE
  ]
  expect_equal(date_change$min_shift, 1)
  expect_equal(date_change$max_shift, 9)
  expect_equal(date_change$range_change, 8)
  datetime_change <- observed$temporal_adjacent[
    observed$temporal_adjacent$variable == "observed_at", , drop = FALSE
  ]
  expect_identical(datetime_change$unit, "seconds")
  expect_equal(datetime_change$min_shift, 86400)

  expect_setequal(
    observed$skipped$code,
    c(
      "unsupported_analysis_type", "invalid_missing_contract",
      "absent_variable", "incompatible_type"
    )
  )
  expect_identical(
    observed$missingness$status[observed$missingness$variable == "note"],
    c("available", "available")
  )
  serialised <- paste(capture.output(str(observed)), collapse = "\n")
  for (marker in fixture$markers) {
    expect_false(grepl(marker, serialised, fixed = TRUE))
  }
  rows_after <- vapply(
    fixture$relations,
    function(relation) {
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT count(*)::integer AS n FROM ",
          drift_live_quote(con, fixture$schema, relation)
        )
      )$n[[1L]]
    },
    integer(1)
  )
  expect_identical(rows_after, rows_before)

  expect_error(
    epi_eda_longitudinal_drift(
      fixture$sources, fixture$spec,
      variables = "reviewed_group", max_levels = 2L
    ),
    "declared categorical domain"
  )
  expect_error(
    epi_eda_longitudinal_drift(
      fixture$sources, fixture$spec,
      variables = "reviewed_group", max_levels = 3L
    ),
    "period domain"
  )
  expect_error(
    epi_eda_longitudinal_drift(
      fixture$sources, fixture$spec,
      variables = "open_group", max_levels = 2L
    ),
    "adjacent union"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("period drift summaries reconcile with canonical PostgreSQL profiles", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  variables <- c(
    "measure", "reviewed_group", "observed_date", "observed_at"
  )
  selected <- fixture$spec[
    match(variables, fixture$spec$name), , drop = FALSE
  ]
  observed <- epi_eda_longitudinal_drift(
    fixture$sources, selected, variables = variables, max_levels = 4L
  )

  for (period_index in seq_along(fixture$sources)) {
    source <- fixture$sources[[period_index]]
    canonical_missing <- epi_eda_profile_missing(source, selected)
    drift_missing <- observed$missingness[
      observed$missingness$period_index == period_index, , drop = FALSE
    ]
    expect_identical(drift_missing$variable, canonical_missing$name)
    expect_identical(drift_missing$n, canonical_missing$n)
    expect_identical(drift_missing$n_missing, canonical_missing$n_missing)
    expect_equal(drift_missing$p_missing, canonical_missing$p_missing)

    canonical <- epi_eda_profile_summaries(source, selected)
    numeric <- observed$numeric[
      observed$numeric$period_index == period_index, , drop = FALSE
    ]
    canonical_numeric <- canonical$numeric[
      match(numeric$variable, canonical$numeric$name), , drop = FALSE
    ]
    numeric_values <- numeric[, c(
      "n_finite", "min", "q1", "mean", "median", "q3", "max", "iqr", "sd"
    )]
    canonical_numeric_values <- canonical_numeric[, c(
      "n_finite", "min", "q1", "mean", "median", "q3", "max", "iqr", "sd"
    )]
    rownames(numeric_values) <- NULL
    rownames(canonical_numeric_values) <- NULL
    expect_equal(numeric_values, canonical_numeric_values)

    categorical <- observed$categorical[
      observed$categorical$period_index == period_index, , drop = FALSE
    ]
    canonical_categorical <- canonical$categorical[
      canonical$categorical$name == "reviewed_group", , drop = FALSE
    ]
    categorical_values <- categorical[, c(
      "level", "n", "p_total", "p_observed", "is_declared", "is_unexpected"
    )]
    canonical_categorical_values <- canonical_categorical[, c(
      "level", "n", "p_total", "p_observed", "is_declared", "is_unexpected"
    )]
    rownames(categorical_values) <- NULL
    rownames(canonical_categorical_values) <- NULL
    expect_equal(categorical_values, canonical_categorical_values)

    temporal <- observed$temporal[
      observed$temporal$period_index == period_index, , drop = FALSE
    ]
    canonical_temporal <- canonical$temporal[
      match(temporal$variable, canonical$temporal$name), , drop = FALSE
    ]
    temporal_values <- temporal[, c(
      "n", "n_missing", "n_observed", "min", "max", "range_value"
    )]
    canonical_temporal_values <- canonical_temporal[, c(
      "n", "n_missing", "n_observed", "min", "max", "range_value"
    )]
    rownames(temporal_values) <- NULL
    rownames(canonical_temporal_values) <- NULL
    expect_equal(temporal_values, canonical_temporal_values)
    expect_identical(temporal$unit, canonical_temporal$range_unit)
  }
})

test_that("zero-row and all-missing periods remain explicit and typed", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  empty <- "drift empty period"
  all_missing <- "drift all missing period"
  source_table <- drift_live_quote(
    con, fixture$schema, fixture$relations[[1L]]
  )
  for (relation in c(empty, all_missing)) {
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE ", drift_live_quote(con, fixture$schema, relation),
        " (LIKE ", source_table, " INCLUDING ALL)"
      )
    )
  }
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ", drift_live_quote(con, fixture$schema, all_missing),
      " (row_marker) VALUES ('all_missing_one'), ('all_missing_two')"
    )
  )
  sources <- list(
    baseline = fixture$sources[[1L]],
    empty = epi_eda_postgres_source(con, fixture$schema, empty),
    all_missing = epi_eda_postgres_source(con, fixture$schema, all_missing)
  )
  observed <- epi_eda_longitudinal_drift(
    sources,
    fixture$spec,
    variables = c("measure", "open_group"),
    max_levels = 4L
  )
  empty_missing <- observed$missingness[
    observed$missingness$period == "empty", , drop = FALSE
  ]
  expect_identical(empty_missing$n, c(0L, 0L))
  expect_true(all(is.na(empty_missing$p_missing)))
  expect_identical(empty_missing$reason, rep("zero_denominator", 2L))
  all_missing_rows <- observed$missingness[
    observed$missingness$period == "all_missing", , drop = FALSE
  ]
  expect_identical(all_missing_rows$n_missing, c(2L, 2L))
  expect_equal(all_missing_rows$p_missing, c(1, 1))
  expect_true(any(
    observed$skipped$period == "all_missing" &
      observed$skipped$code == "zero_denominator"
  ))
  expect_false(anyNA(observed$categorical$level))
})

test_that("an observed 51st categorical level fails the bounded call", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  target <- drift_live_quote(
    con, fixture$schema, fixture$relations[[1L]]
  )
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ", target, " (row_marker, open_group) ",
      "SELECT 'bounded_' || value::text, 'level_' || lpad(value::text, 2, '0') ",
      "FROM generate_series(1, 51) AS value"
    )
  )
  expect_error(
    epi_eda_longitudinal_drift(
      fixture$sources,
      fixture$spec,
      variables = "open_group",
      max_levels = 50L
    ),
    "period domain exceeds max_levels"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("shared longitudinal transaction holds one drift snapshot", {
  con <- drift_live_connection()
  writer <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      drift_live_cleanup(con, fixture)
    },
    add = TRUE
  )
  transaction <- getFromNamespace("eda_longitudinal_transaction", "episcout")
  row_count <- getFromNamespace("eda_postgres_row_count", "episcout")
  target <- drift_live_quote(
    writer, fixture$schema, fixture$relations[[2L]]
  )
  observed <- transaction(
    fixture$sources,
    {
      before <- row_count(fixture$sources[[2L]])
      DBI::dbExecute(
        writer,
        paste0("INSERT INTO ", target, " (row_marker) VALUES ('concurrent')")
      )
      after <- row_count(fixture$sources[[2L]])
      c(before = before, after = after)
    },
    operation = "drift"
  )
  expect_identical(observed, c(before = 4L, after = 4L))
  expect_identical(row_count(fixture$sources[[2L]]), 5L)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("the complete drift API retains its snapshot during a concurrent write", {
  con <- drift_live_connection()
  writer <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      drift_live_cleanup(con, fixture)
    },
    add = TRUE
  )
  original <- getFromNamespace("eda_postgres_row_count", "episcout")
  inserted <- FALSE
  target <- drift_live_quote(
    writer, fixture$schema, fixture$relations[[2L]]
  )
  observed <- with_mocked_bindings(
    epi_eda_longitudinal_drift(
      fixture$sources,
      fixture$spec,
      variables = "measure"
    ),
    eda_postgres_row_count = function(source, timing_env = NULL) {
      value <- original(source, timing_env)
      if (!inserted) {
        inserted <<- TRUE
        DBI::dbExecute(
          writer,
          paste0("INSERT INTO ", target, " (row_marker) VALUES ('concurrent')")
        )
      }
      value
    },
    .package = "episcout"
  )
  expect_identical(observed$missingness$n, c(4L, 4L))
  expect_identical(original(fixture$sources[[2L]]), 5L)
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
})

test_that("a forced drift query failure rolls back without leaking row values", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  original <- getFromNamespace("eda_postgres_row_count", "episcout")
  calls <- 0L
  observed <- tryCatch(
    with_mocked_bindings(
      epi_eda_longitudinal_drift(
        fixture$sources,
        fixture$spec,
        variables = "measure"
      ),
      eda_postgres_row_count = function(source, timing_env = NULL) {
        calls <<- calls + 1L
        if (calls == 2L) stop("Controlled aggregate query failure.", call. = FALSE)
        original(source, timing_env)
      },
      .package = "episcout"
    ),
    error = identity
  )
  expect_s3_class(observed, "error")
  message <- conditionMessage(observed)
  for (marker in fixture$markers) {
    expect_false(grepl(marker, message, fixed = TRUE))
  }
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("a PostgreSQL error is sanitised and rolls back the drift call", {
  con <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(drift_live_cleanup(con, fixture), add = TRUE)
  function_name <- "raise private drift error"
  view_name <- "drift failing view"
  marker <- paste0("private_database_error_", drift_live_suffix())
  function_sql <- paste0(
    drift_live_quote(con, fixture$schema), ".",
    as.character(DBI::dbQuoteIdentifier(con, function_name))
  )
  DBI::dbExecute(
    con,
    paste0(
      "CREATE FUNCTION ", function_sql,
      "() RETURNS double precision LANGUAGE plpgsql AS $body$ ",
      "BEGIN RAISE EXCEPTION '", marker, "'; END $body$"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "CREATE VIEW ", drift_live_quote(con, fixture$schema, view_name),
      " AS SELECT ", function_sql, "() AS measure"
    )
  )
  sources <- list(
    baseline = fixture$sources[[1L]],
    failing = epi_eda_postgres_source(con, fixture$schema, view_name)
  )
  observed <- tryCatch(
    epi_eda_longitudinal_drift(
      sources, fixture$spec, variables = "measure"
    ),
    error = identity
  )
  expect_s3_class(observed, "error")
  expect_match(conditionMessage(observed), "restricted database logs")
  expect_false(grepl(marker, conditionMessage(observed), fixed = TRUE))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("the public drift API rejects changed and mixed-connection sources", {
  con <- drift_live_connection()
  other <- drift_live_connection()
  fixture <- drift_live_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(other)) DBI::dbDisconnect(other)
      drift_live_cleanup(con, fixture)
    },
    add = TRUE
  )
  DBI::dbExecute(
    con,
    paste0(
      "ALTER TABLE ",
      drift_live_quote(con, fixture$schema, fixture$relations[[1L]]),
      " ADD COLUMN catalogue_change integer"
    )
  )
  expect_error(
    epi_eda_longitudinal_drift(
      fixture$sources, fixture$spec, variables = "measure"
    ),
    "catalogue"
  )
  mixed <- fixture$sources
  mixed[[2L]] <- epi_eda_postgres_source(
    other, fixture$schema, fixture$relations[[2L]]
  )
  expect_error(
    epi_eda_longitudinal_drift(
      mixed, fixture$spec, variables = "measure"
    ),
    "share one"
  )
})
