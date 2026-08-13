context("PostgreSQL EDA aggregate parity")

postgres_eda_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostgreSQL integration tests.")
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

postgres_eda_fixture <- function(con) {
  schema <- paste0("epi_eda_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- paste0(schema_sql, ".", as.character(DBI::dbQuoteIdentifier(con, "odd table")))
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql, " (",
    "participant_id text, measurement double precision, whole_number bigint, ",
    "treatment text, flag boolean, note text, specimen_date date, ",
    "specimen_time timestamptz, local_time timestamp without time zone)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES ",
    "('x', 1, 1, 'A', true, 'alpha', DATE '2024-01-01', TIMESTAMPTZ '2024-01-01 00:00:00Z', TIMESTAMP '2024-01-01 00:00:00'),",
    "('x', 2, 2, 'B', false, '', DATE '2024-01-03', TIMESTAMPTZ '2024-01-02 00:00:00Z', TIMESTAMP '2024-01-02 00:00:00'),",
    "('y', 999, 2, 'A', true, '  ', NULL, NULL, NULL),",
    "(NULL, 'NaN', 3, 'Z', false, U&'\\00E9', DATE '2024-01-05', TIMESTAMPTZ '2024-01-03 00:00:00+02', TIMESTAMP '2024-01-03 00:00:00'),",
    "('z', 'Infinity', 9, 'MISSING', true, 'SECRET_CANARY', DATE '2024-01-07', TIMESTAMPTZ '2024-01-04 00:00:00Z', TIMESTAMP '2024-01-04 00:00:00'),",
    "('z', NULL, 11, NULL, NULL, NULL, DATE '2024-01-09', TIMESTAMPTZ '2024-01-05 00:00:00Z', TIMESTAMP '2024-01-05 00:00:00')"
  ))
  spec <- data.frame(
    name = c("participant_id", "measurement", "whole_number", "treatment", "flag", "note", "specimen_date", "specimen_time", "local_time"),
    label = c("Participant", "Measurement", "Whole number", "Treatment", "Flag", "Note", "Specimen date", "Specimen time", "Local time"),
    database_type = "text", analysis_type = c("text", "numeric", "integer", "categorical", "binary", "text", "date", "datetime", "datetime"),
    role = c("identifier", "measure", "measure", "exposure", "outcome", "comment", "time", "time", "time"),
    levels = c("", "", "", "A;B;C", "FALSE;TRUE", "", "", "", ""),
    missing_codes = c("", "999", "", "MISSING", "", "SECRET_CANARY", "2024-01-03", "", ""),
    required = TRUE,
    stringsAsFactors = FALSE
  )
  list(schema = schema, relation = "odd table", spec = spec)
}

expect_numeric_contract <- function(observed, expected) {
  integer_fields <- c(
    "n_finite", "n_below_lower", "n_above_upper", "outlier_count"
  )
  expect_identical(
    as.integer(observed[1, integer_fields, drop = TRUE]),
    as.integer(expected[integer_fields])
  )
  numeric_fields <- setdiff(names(expected), integer_fields)
  for (field in numeric_fields) {
    expected_value <- expected[[field]]
    observed_value <- observed[[field]][[1]]
    if (is.na(expected_value)) {
      expect_true(is.na(observed_value), info = field)
    } else {
      tolerance <- 1e-10 * max(1, abs(expected_value))
      expect_lte(abs(observed_value - expected_value), tolerance)
    }
  }
}

test_that("live PostgreSQL profiles reproduce independently stated aggregate expectations", {
  con <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)

  expect_identical(names(source), c("con", "schema", "relation", "relation_kind", "columns", "source_version"))
  expect_identical(class(source), c("epi_eda_postgres_source", "list"))
  missing <- epi_eda_profile_missing(source, fixture$spec)
  summaries <- epi_eda_profile_summaries(source, fixture$spec)
  schema <- epi_eda_check_schema(source, fixture$spec)
  plots <- epi_eda_profile_plots(source, fixture$spec)

  expect_identical(missing$name, fixture$spec$name)
  expect_identical(missing$n, rep(6L, 9L))
  expect_identical(
    missing$n_missing,
    c(1L, 3L, 0L, 2L, 1L, 2L, 2L, 1L, 1L)
  )
  expect_equal(missing$p_missing, missing$n_missing / 6, tolerance = 1e-12)
  numeric <- summaries$numeric[summaries$numeric$name == "measurement", ]
  expect_numeric_contract(numeric, c(
    n_finite = 2, sum = 3, min = 1, q1 = 1.25, mean = 1.5,
    median = 1.5, q3 = 1.75, max = 2, iqr = 0.5,
    sd = 0.707106781186548, variance = 0.5, sem = 0.5,
    cv = 0.471404520791032, skewness = NA, kurtosis = NA,
    shapiro_p = NA, lower_fence = 0.5, upper_fence = 2.5,
    n_below_lower = 0, n_above_upper = 0, outlier_count = 0,
    outlier_percentage = 0
  ))
  whole <- summaries$numeric[summaries$numeric$name == "whole_number", ]
  expect_numeric_contract(whole, c(
    n_finite = 6, sum = 28, min = 1, q1 = 2,
    mean = 4.66666666666667, median = 2.5, q3 = 7.5, max = 11,
    iqr = 5.5, sd = 4.22689799577263, variance = 17.8666666666667,
    sem = 1.7256238807393, cv = 0.905763856236992,
    skewness = 0.537503736374889, kurtosis = -1.82467327541398,
    shapiro_p = 0.0658661619960693, lower_fence = -6.25,
    upper_fence = 15.75, n_below_lower = 0, n_above_upper = 0,
    outlier_count = 0, outlier_percentage = 0
  ))
  asymmetric <- c(1, 2, 2, 3, 9, 11)
  expect_equal(whole$skewness, e1071::skewness(asymmetric, type = 3), tolerance = 1e-12)
  expect_equal(whole$kurtosis, e1071::kurtosis(asymmetric, type = 3), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    e1071::skewness(asymmetric, type = 3),
    e1071::skewness(asymmetric, type = 1)
  )))
  expect_equal(summaries$categorical$n[summaries$categorical$name == "treatment"], c(2L, 1L, 0L, 1L))
  expect_equal(summaries$categorical$level[summaries$categorical$name == "treatment"], c("A", "B", "C", "Z"))
  treatment <- summaries$categorical[summaries$categorical$name == "treatment", ]
  expect_equal(treatment$p_total, c(2, 1, 0, 1) / 6, tolerance = 1e-12)
  expect_equal(treatment$p_observed, c(2, 1, 0, 1) / 4, tolerance = 1e-12)
  expect_identical(treatment$is_declared, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(treatment$is_unexpected, c(FALSE, FALSE, FALSE, TRUE))
  flag <- summaries$categorical[summaries$categorical$name == "flag", ]
  expect_identical(flag$level, c("FALSE", "TRUE"))
  expect_identical(flag$n, c(2L, 3L))
  expect_equal(flag$p_total, c(2, 3) / 6, tolerance = 1e-12)
  expect_equal(flag$p_observed, c(2, 3) / 5, tolerance = 1e-12)
  expect_identical(flag$is_declared, c(TRUE, TRUE))
  expect_identical(flag$is_unexpected, c(FALSE, FALSE))
  text <- summaries$text[summaries$text$name == "note", ]
  rownames(text) <- NULL
  expect_identical(text, data.frame(
    name = "note", n = 6L, n_missing = 2L, n_observed = 4L,
    n_unique = 4L, n_empty = 1L, n_whitespace = 1L,
    min_length = 0L, max_length = 5L, stringsAsFactors = FALSE
  ))
  expect_identical(summaries$temporal, data.frame(
    name = c("specimen_date", "specimen_time"),
    source_class = c("Date", "POSIXct/POSIXt"),
    timezone = c(NA_character_, "UTC"),
    n = c(6L, 6L), n_missing = c(2L, 1L),
    n_observed = c(4L, 5L), n_unique = c(4L, 5L),
    min = c("2024-01-01", "2024-01-01T00:00:00Z"),
    q1 = c("2024-01-04", "2024-01-02T00:00:00Z"),
    median = c("2024-01-06", "2024-01-02T22:00:00Z"),
    q3 = c("2024-01-07", "2024-01-04T00:00:00Z"),
    max = c("2024-01-09", "2024-01-05T00:00:00Z"),
    range_value = c(8, 345600),
    range_unit = c("days", "seconds"),
    stringsAsFactors = FALSE
  ))
  participant_text <- summaries$text[
    summaries$text$name == "participant_id", ,
    drop = FALSE
  ]
  expect_identical(participant_text, data.frame(
    name = "participant_id", n = 6L, n_missing = 1L, n_observed = 5L,
    n_unique = 3L, n_empty = 0L, n_whitespace = 0L,
    min_length = 1L, max_length = 1L, stringsAsFactors = FALSE
  ))
  local_time_reason <- paste(
    "PostgreSQL timestamp without time zone has no declared instant or DST meaning;",
    "use a view cast to timestamp with time zone."
  )
  expect_identical(summaries$variables, data.frame(
    name = fixture$spec$name,
    label = fixture$spec$label,
    database_type = "text", analysis_type = fixture$spec$analysis_type,
    role = fixture$spec$role,
    required = rep(TRUE, 9L),
    n = rep(6L, 9L),
    n_missing = c(1L, 3L, 0L, 2L, 1L, 2L, 2L, 1L, 1L),
    n_observed = c(5L, 3L, 6L, 4L, 5L, 4L, 4L, 5L, 5L),
    n_unique = c(3L, 3L, 5L, 3L, 2L, 4L, 4L, 5L, 5L),
    n_infinite = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    status = c(rep("summarised", 8L), "skipped"),
    reason = c(rep(NA_character_, 8L), local_time_reason),
    stringsAsFactors = FALSE
  ))
  expect_identical(summaries$skipped, data.frame(
    name = "local_time",
    database_type = "text", analysis_type = "datetime",
    observed_class = "timestamp without time zone",
    reason = local_time_reason,
    stringsAsFactors = FALSE
  ))
  expect_identical(summaries$variables$status[summaries$variables$name == "participant_id"], "summarised")
  expect_true("participant_id" %in% summaries$text$name)
  expect_identical(schema$type_status[schema$name == "local_time"], "incompatible")
  expect_named(plots, fixture$spec$name)
  expect_s3_class(plots$participant_id, "ggplot")
  expect_s3_class(plots$measurement, "ggplot")
  expect_s3_class(plots$note, "ggplot")
  expect_null(plots$local_time)
  extra_spec <- rbind(fixture$spec, transform(fixture$spec[1, ], name = "absent_from_relation"))
  expect_error(epi_eda_profile_plots(source, extra_spec), "missing specified")
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))
})

test_that("PostgreSQL percentile_cont matches independently evaluated R type-7 edges", {
  con <- postgres_eda_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in 1:12) {
    observed <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS q1,",
        "percentile_cont(0.5) WITHIN GROUP (ORDER BY value) AS median,",
        "percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS q3",
        "FROM generate_series(1, $1::integer) AS value"
      ),
      params = list(n)
    )
    expected <- as.numeric(stats::quantile(seq_len(n), c(0.25, 0.5, 0.75), type = 7, names = FALSE))
    expect_equal(as.numeric(observed[1, ]), expected, tolerance = 1e-12)
  }
})

test_that("authorised relation kinds work and temporary/catalogue drift fail closed", {
  con <- postgres_eda_connection()
  schema <- paste0("epi_eda_kinds_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  server <- paste0("epi_eda_server_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  server_sql <- as.character(DBI::dbQuoteIdentifier(con, server))
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SERVER IF EXISTS ", server_sql, " CASCADE"))
        DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0("CREATE TABLE ", schema_sql, ".base_table (value integer)"))
  DBI::dbExecute(con, paste0("INSERT INTO ", schema_sql, ".base_table VALUES (1), (2)"))
  DBI::dbExecute(con, paste0("CREATE VIEW ", schema_sql, ".fixture_view AS SELECT * FROM ", schema_sql, ".base_table"))
  DBI::dbExecute(con, paste0("CREATE MATERIALIZED VIEW ", schema_sql, ".fixture_materialized AS SELECT * FROM ", schema_sql, ".base_table"))
  DBI::dbExecute(con, paste0("CREATE TABLE ", schema_sql, ".fixture_partitioned (value integer) PARTITION BY RANGE (value)"))
  DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS file_fdw")
  DBI::dbExecute(con, paste0("CREATE SERVER ", server_sql, " FOREIGN DATA WRAPPER file_fdw"))
  DBI::dbExecute(con, paste0(
    "CREATE FOREIGN TABLE ", schema_sql, ".fixture_foreign (value integer) SERVER ",
    server_sql, " OPTIONS (filename '/dev/null', format 'csv')"
  ))

  expect_identical(epi_eda_postgres_source(con, schema, "base_table")$relation_kind, "table")
  expect_identical(epi_eda_postgres_source(con, schema, "fixture_view")$relation_kind, "view")
  expect_identical(epi_eda_postgres_source(con, schema, "fixture_materialized")$relation_kind, "materialized view")
  expect_identical(epi_eda_postgres_source(con, schema, "fixture_partitioned")$relation_kind, "partitioned table")
  expect_identical(epi_eda_postgres_source(con, schema, "fixture_foreign")$relation_kind, "foreign table")

  DBI::dbExecute(con, "CREATE TEMPORARY TABLE temporary_eda (value integer)")
  temporary_schema <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT n.nspname FROM pg_class c",
      "INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE c.relname = 'temporary_eda' AND c.relpersistence = 't'"
    )
  )$nspname[[1]]
  expect_error(epi_eda_postgres_source(con, temporary_schema, "temporary_eda"), "temporary")

  source <- epi_eda_postgres_source(con, schema, "base_table")
  DBI::dbExecute(con, paste0("ALTER TABLE ", schema_sql, ".base_table ADD COLUMN changed text"))
  spec <- data.frame(name = "value", label = "Value", database_type = "text", analysis_type = "integer", role = "measure")
  expect_error(epi_eda_check_schema(source, spec), "catalogue changed")
})

test_that("live PostgreSQL storage matrix preserves zero-row schemas", {
  con <- postgres_eda_connection()
  schema <- paste0("epi_eda_matrix_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- paste0(schema_sql, ".", DBI::dbQuoteIdentifier(con, "type matrix"))
  enum_sql <- paste0(schema_sql, ".", DBI::dbQuoteIdentifier(con, "reviewed mood"))
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0("CREATE TYPE ", enum_sql, " AS ENUM ('A', 'B')"))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql, " (",
    "small_value smallint, integer_value integer, big_value bigint, ",
    "decimal_value numeric, real_value real, double_value double precision, ",
    "fixed_text character(3), varying_text character varying(10), plain_text text, ",
    "flag boolean, mood ", enum_sql, ", observed_date date, observed_at timestamptz, ",
    "local_stamp timestamp without time zone, payload jsonb)"
  ))
  spec <- data.frame(
    name = c(
      "small_value", "integer_value", "big_value", "decimal_value", "real_value",
      "double_value", "fixed_text", "varying_text", "plain_text", "flag", "mood",
      "observed_date", "observed_at", "local_stamp", "payload"
    ),
    label = c(
      "Small", "Integer", "Big", "Decimal", "Real", "Double", "Fixed", "Varying",
      "Plain", "Flag", "Mood", "Date", "Datetime", "Local datetime", "Payload"
    ),
    database_type = "text", analysis_type = c(
      "integer", "integer", "integer", "numeric", "numeric", "numeric", "text",
      "categorical", "text", "binary", "categorical", "date", "datetime", "datetime", "text"
    ),
    role = c(rep("measure", 9L), "outcome", "exposure", "time", "time", "time", "comment"),
    levels = c(rep("", 7L), "A;B", "", "FALSE;TRUE", "A;B", rep("", 4L)),
    stringsAsFactors = FALSE
  )
  source <- epi_eda_postgres_source(con, schema, "type matrix")
  schema_result <- epi_eda_check_schema(source, spec)
  missing <- epi_eda_profile_missing(source, spec)
  summaries <- epi_eda_profile_summaries(source, spec)

  expect_true(all(schema_result$type_status[seq_len(13L)] %in% c("compatible", "coercible")))
  expect_identical(schema_result$type_status[14:15], c("incompatible", "incompatible"))
  expect_true(all(missing$n == 0L))
  expect_true(all(missing$n_missing == 0L))
  expect_true(all(is.na(missing$p_missing)))
  expect_equal(nrow(summaries$numeric), 6L)
  expect_equal(nrow(summaries$categorical), 6L)
  expect_equal(nrow(summaries$text), 2L)
  expect_equal(nrow(summaries$temporal), 2L)
  expect_true(all(summaries$numeric$n == 0L))
  expect_true(all(is.na(summaries$numeric$mean)))
  expect_setequal(summaries$skipped$name, c("local_stamp", "payload"))
})

test_that("live PostgreSQL run publishes an exact aggregate-only owned bundle", {
  con <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  output_dir <- tempfile("postgres-eda-bundle-")
  run <- epi_eda_db_run(source, fixture$spec, output_dir, plots = FALSE)

  expect_s3_class(run, "epi_eda_db_run")
  expect_identical(names(run), c("status", "output_dir", "manifest", "source", "spec", "schema", "missing", "summaries", "identifier_qa", "geo", "plots", "plot_inventory", "maps", "map_inventory", "timings", "messages", "metadata"))
  expect_identical(run$status, "complete")
  expect_named(run$plots, character())
  expect_named(run$maps, character())
  expect_equal(nrow(run$map_inventory), 0L)
  expect_identical(names(run$manifest), c("artifact", "type", "path", "status", "checksum_md5"))
  expect_identical(sum(run$timings$query_kind == "row_count"), 1L)
  expect_equal(run$identifier_qa$n_distinct, 3L)
  expect_equal(run$identifier_qa$n_repeated_values, 2L)
  expect_equal(run$identifier_qa$duplicate_excess, 2L)
  expect_equal(run$identifier_qa$max_frequency, 2L)
  expect_true(all(run$manifest$path[run$manifest$status == "created"] %in% list.files(output_dir, recursive = TRUE)))
  bundle <- paste(vapply(list.files(output_dir, recursive = TRUE, full.names = TRUE), function(path) paste(readLines(path, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  expect_false(grepl("alpha|CANARY_HOST|SELECT |Infinity", bundle, ignore.case = TRUE))
  expect_error(epi_eda_db_run(source, fixture$spec, output_dir, plots = FALSE), "non-empty")
  overwritten <- epi_eda_db_run(source, fixture$spec, output_dir, overwrite = TRUE, plots = FALSE)
  expect_identical(overwritten$status, "complete")
  changed_spec <- fixture$spec
  changed_spec$label[[1]] <- "Changed reviewed label"
  expect_error(
    epi_eda_db_run(source, changed_spec, output_dir, overwrite = TRUE, plots = FALSE),
    "source, specification, plot options, or map options"
  )
  missing_path <- file.path(output_dir, "missing.csv")
  write("tamper", missing_path, append = TRUE)
  tampered <- readBin(missing_path, "raw", n = file.info(missing_path)$size)
  expect_error(
    epi_eda_db_run(source, fixture$spec, output_dir, overwrite = TRUE, plots = FALSE),
    "checksums"
  )
  expect_identical(readBin(missing_path, "raw", n = file.info(missing_path)$size), tampered)

  plot_dir <- tempfile("postgres-eda-plots-")
  plotted <- epi_eda_db_run(source, fixture$spec, plot_dir, plots = TRUE, max_plot_levels = 2L)
  expect_named(plotted$plots, fixture$spec$name)
  expect_s3_class(plotted$plots$participant_id, "ggplot")
  svg_paths <- file.path(plot_dir, plotted$plot_inventory$path[plotted$plot_inventory$status == "created"])
  expect_true(length(svg_paths) > 0L)
  expect_true(all(file.exists(svg_paths)))
  svg <- paste(vapply(svg_paths, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
  expect_match(svg, "<svg", fixed = TRUE)
  expect_false(grepl("alpha|SECRET_CANARY", svg, fixed = FALSE))
  expect_true(any(plotted$plot_inventory$plot_type == "quantile_box"))
})

test_that("live PostgreSQL database styling uses compact context and persistent provenance", {
  con <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  output_dir <- tempfile("postgres-eda-style-")
  seen <- new.env(parent = emptyenv())
  seen$contexts <- list()
  style <- function(plot, context) {
    seen$contexts[[length(seen$contexts) + 1L]] <- context
    plot + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  }
  styled <- epi_eda_db_run(
    source, fixture$spec, output_dir,
    plot_style = style, plot_style_id = "neutral-style-v1"
  )

  expect_identical(styled$metadata$plot_style_id, "neutral-style-v1")
  expect_true(length(seen$contexts) >= length(styled$plots))
  expect_true(all(vapply(seen$contexts, function(context) {
    identical(
      names(context),
      c("name", "label", "type", "plot_type", "n_total", "n_missing", "n_plotted", "n_excluded_non_finite")
    )
  }, logical(1L))))
  expect_false(any(vapply(seen$contexts, function(context) {
    any(c("data", "source", "con", "sql", "credentials") %in% names(context))
  }, logical(1L))))
  replaced <- epi_eda_db_run(
    source, fixture$spec, output_dir, overwrite = TRUE,
    plot_style = style, plot_style_id = "neutral-style-v1"
  )
  expect_identical(replaced$status, "complete")
  expect_error(
    epi_eda_db_run(
      source, fixture$spec, output_dir, overwrite = TRUE,
      plot_style = style, plot_style_id = "neutral-style-v2"
    ),
    "plot options"
  )
  disabled_dir <- tempfile("postgres-eda-style-disabled-")
  disabled <- epi_eda_db_run(
    source, fixture$spec, disabled_dir, plots = FALSE,
    plot_style = function(plot, context) stop("style must not run")
  )
  expect_identical(disabled$status, "complete")
  expect_false("plot_style_id" %in% names(disabled$metadata))
  failed_dir <- tempfile("postgres-eda-style-failed-")
  expect_error(
    epi_eda_db_run(
      source, fixture$spec, failed_dir,
      plot_style = function(plot, context) stop("style failure"),
      plot_style_id = "failing-style-v1"
    ),
    "plot_style failed"
  )
  expect_false(dir.exists(failed_dir))
})

test_that("live PostgreSQL delivery mode renders only after aggregate collection", {
  con <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0(
          "DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"
        ))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  flat_dir <- tempfile("postgres-eda-flat-compatible-")
  delivery_dir <- tempfile("postgres-eda-delivery-")

  flat <- epi_eda_db_run(source, fixture$spec, flat_dir, plots = FALSE)
  delivery <- epi_eda_db_run(
    source, fixture$spec, delivery_dir,
    plots = TRUE,
    layout = "delivery", quiet = TRUE
  )

  expect_identical(names(flat$metadata), names(delivery$metadata))
  expect_false("layout" %in% names(flat$metadata))
  expect_identical(flat$missing, delivery$missing)
  expect_identical(flat$summaries, delivery$summaries)
  expect_identical(flat$metadata$plot_data_contract[[1]], "compact-plot-data-1")
  expect_identical(delivery$metadata$plot_data_contract[[1]], "compact-plot-data-2")
  expect_true(file.exists(file.path(
    delivery_dir, "reports", "eda-report.html"
  )))
  expect_true(file.exists(file.path(delivery_dir, "README.md")))
  expect_true(file.exists(file.path(
    delivery_dir, "run_manifests", "manifest.csv"
  )))
  expect_true(file.exists(file.path(delivery_dir, "QA_QC", "missing.csv")))
  expect_true(any(delivery$manifest$type == "plot_data"))
  expect_false(any(grepl("coordinate|theme", delivery$manifest$path[
    delivery$manifest$type == "plot_data"
  ])))
  frequency_paths <- delivery$manifest$path[
    delivery$manifest$type == "plot_data" &
      grepl("-frequency\\.csv$", delivery$manifest$path)
  ]
  expect_identical(
    frequency_paths,
    c("plot_data/004-frequency.csv", "plot_data/005-frequency.csv")
  )
  treatment_companion <- utils::read.csv(
    file.path(delivery_dir, frequency_paths[[1]]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_named(
    treatment_companion,
    getFromNamespace("eda_frequency_companion_names", "episcout")()
  )
  expect_identical(treatment_companion$count, treatment_companion$numerator)
  expect_identical(
    treatment_companion$denominator,
    rep(4L, nrow(treatment_companion))
  )
  expect_equal(
    treatment_companion$proportion,
    treatment_companion$numerator / 4
  )
  report_html <- paste(readLines(
    file.path(delivery_dir, "reports", "eda-report.html"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(report_html, "Categorical percentage companions", fixed = TRUE)
  expect_match(report_html, "Treatment", fixed = TRUE)
  replaced <- epi_eda_db_run(
    source, fixture$spec, delivery_dir,
    overwrite = TRUE, plots = TRUE,
    layout = "delivery", quiet = TRUE
  )
  expect_true(file.exists(file.path(
    replaced$output_dir, "reports", "eda-report.html"
  )))
  expect_error(
    epi_eda_db_run(
      source, fixture$spec, delivery_dir,
      overwrite = TRUE, plots = TRUE,
      layout = "bundle"
    ),
    "layout does not match"
  )

  DBI::dbExecute(con, paste0(
    "DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"
  ))
  DBI::dbDisconnect(con)
  flat_report <- epi_eda_render_db_report(flat)
  expect_true(file.exists(flat_report))
  expect_false(DBI::dbIsValid(source$con))
})

test_that("live PostgreSQL calls reject caller transactions without disturbing them", {
  con <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        if (getFromNamespace("eda_pg_is_transacting", "episcout")(con)) try(DBI::dbRollback(con), silent = TRUE)
        DBI::dbExecute(con, paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  DBI::dbBegin(con)
  expect_error(epi_eda_profile_missing(source, fixture$spec), "caller-managed transaction")
  expect_true(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  DBI::dbRollback(con)
})

test_that("repeatable-read wrapper holds one snapshot and cleans up failures", {
  con <- postgres_eda_connection()
  writer <- postgres_eda_connection()
  fixture <- postgres_eda_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(writer)) DBI::dbDisconnect(writer)
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  transaction <- getFromNamespace("eda_postgres_transaction", "episcout")
  row_count <- getFromNamespace("eda_postgres_row_count", "episcout")
  table_sql <- paste0(DBI::dbQuoteIdentifier(writer, fixture$schema), ".", DBI::dbQuoteIdentifier(writer, fixture$relation))

  observed <- transaction(source, {
    before <- row_count(source)
    DBI::dbExecute(writer, paste0("INSERT INTO ", table_sql, " (participant_id) VALUES ('concurrent')"))
    after <- row_count(source)
    c(before = before, after = after)
  })
  expect_identical(observed, c(before = 6L, after = 6L))
  expect_equal(row_count(source), 7L)

  fetch <- getFromNamespace("eda_db_fetch", "episcout")
  expect_error(
    transaction(source, fetch(con, "SELECT definitely_missing_column FROM definitely_missing_relation", query_kind = "forced_failure", limit = 1L)),
    "forced_failure"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS usable")$usable, 1L)
})

test_that("live PostgreSQL notices are disclosed without server text", {
  con <- postgres_eda_connection()
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
  statement <- getFromNamespace("eda_db_statement", "episcout")
  observed_messages <- character()

  withCallingHandlers(
    statement(
      con,
      "DO $block$ BEGIN RAISE NOTICE 'NOTICE_CANARY'; END $block$",
      query_kind = "transaction_setup"
    ),
    message = function(condition) {
      observed_messages <<- c(observed_messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )

  expect_length(observed_messages, 1L)
  expect_match(observed_messages, "database message", fixed = TRUE)
  expect_false(any(grepl("CANARY", observed_messages, fixed = TRUE)))
})
