context("PostgreSQL EDA synthetic performance acceptance")

postgres_benchmark_connection <- function() {
  gates_enabled <- all(c(
    Sys.getenv("EPISCOUT_TEST_POSTGRES") == "1",
    Sys.getenv("EPISCOUT_BENCHMARK_POSTGRES") == "1"
  ))
  if (!gates_enabled) {
    skip("Set both PostgreSQL test gates for the synthetic performance acceptance test.")
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("RPostgres is required when the PostgreSQL benchmark gate is enabled.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required when the PostgreSQL benchmark gate is enabled.", call. = FALSE)
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("PGHOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "synthetic_records"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "")
  )
}

postgres_benchmark_spec <- function() {
  data.frame(
    name = c(
      "record_id", "measurement", "whole_number", "treatment",
      "flag", "note", "specimen_date", "specimen_time"
    ),
    label = c(
      "Record", "Measurement", "Whole number", "Treatment",
      "Flag", "Note", "Specimen date", "Specimen time"
    ),
    database_type = "text", analysis_type = c(
      "integer", "numeric", "integer", "categorical",
      "binary", "text", "date", "datetime"
    ),
    role = c(
      "identifier", "measure", "measure", "exposure",
      "outcome", "comment", "time", "time"
    ),
    levels = c(
      "", "", "", paste0(LETTERS[1:20], 0:19, collapse = ";"),
      "FALSE;TRUE", "", "", ""
    ),
    required = TRUE,
    stringsAsFactors = FALSE
  )
}

expect_benchmark_bundle <- function(run) {
  expect_s3_class(run, "epi_eda_db_run")
  expect_identical(run$status, "complete")
  expect_equal(run$metadata$n_rows, 1000000)
  expect_equal(run$metadata$n_spec_variables, 8L)

  created_plots <- run$plot_inventory$status == "created"
  expected_plots <- data.frame(
    name = c(
      "record_id", "record_id", "measurement", "measurement",
      "whole_number", "whole_number", "treatment", "flag", "note",
      "specimen_date", "specimen_time"
    ),
    plot_type = c(
      "histogram", "quantile_box", "histogram", "quantile_box",
      "histogram", "quantile_box", "frequency", "frequency", "text_length",
      "temporal", "temporal"
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(
    run$plot_inventory[created_plots, c("name", "plot_type")],
    expected_plots
  )
  plot_paths <- run$plot_inventory$path[created_plots]
  expect_setequal(run$manifest$path[run$manifest$type == "plot"], plot_paths)
  svg_paths <- file.path(run$output_dir, plot_paths)
  expect_true(all(file.info(svg_paths)$size > 0))
  expect_true(all(vapply(svg_paths, function(path) {
    any(grepl("<svg", readLines(path, warn = FALSE), fixed = TRUE))
  }, logical(1))))

  categorical <- run$summaries$categorical
  expect_equal(sum(categorical$name == "treatment"), 20L)
  expect_equal(sum(categorical$name == "flag"), 2L)
  published_categorical <- utils::read.csv(
    file.path(run$output_dir, "summary_categorical.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_equal(sum(published_categorical$name == "treatment"), 20L)
  expect_equal(sum(published_categorical$name == "flag"), 2L)

  checked <- run$manifest$status == "created" &
    run$manifest$artifact != "manifest"
  expect_identical(
    as.character(run$manifest$checksum_md5[checked]),
    unname(tools::md5sum(file.path(
      run$output_dir,
      run$manifest$path[checked]
    )))
  )

  categorical_rows <- run$timings[
    run$timings$query_kind == "categorical_frequency",
    c("name", "rows_returned"),
    drop = FALSE
  ]
  other_rows <- run$timings$rows_returned[
    run$timings$query_kind != "categorical_frequency"
  ]
  expect_identical(
    categorical_rows$rows_returned[categorical_rows$name == "treatment"],
    20L
  )
  expect_identical(
    categorical_rows$rows_returned[categorical_rows$name == "flag"],
    2L
  )
  expect_lte(max(other_rows), 30L)
}

test_that("one-million-row PostgreSQL EDA stays within its synthetic runtime gate", {
  con <- postgres_benchmark_connection()
  schema <- gsub(
    "[^A-Za-z0-9_]", "_",
    basename(tempfile(paste0("epi_eda_benchmark_", Sys.getpid(), "_")))
  )
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  relation_sql <- paste0(
    schema_sql, ".", as.character(DBI::dbQuoteIdentifier(con, "observations"))
  )
  output_dir <- tempfile("postgres-eda-benchmark-")
  schema_created <- FALSE
  on.exit(
    {
      unlink(output_dir, recursive = TRUE, force = TRUE)
      if (DBI::dbIsValid(con)) {
        if (schema_created) {
          try(
            DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE")),
            silent = TRUE
          )
        }
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  schema_created <- TRUE
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", relation_sql, " AS SELECT ",
    "g::bigint AS record_id, ",
    "CASE WHEN g % 97 = 0 THEN NULL ELSE ",
    "(g % 1000)::double precision + sin(g::double precision) END AS measurement, ",
    "(g % 100000)::bigint AS whole_number, ",
    "CASE WHEN g % 101 = 0 THEN NULL ELSE ",
    "chr(65 + (g % 20)::integer) || (g % 20)::text END AS treatment, ",
    "CASE WHEN g % 103 = 0 THEN NULL ELSE (g % 2 = 0) END AS flag, ",
    "CASE WHEN g % 107 = 0 THEN NULL ELSE ",
    "repeat(chr(97 + (g % 20)::integer), (g % 40)::integer) END AS note, ",
    "DATE '2010-01-01' + (g % 5000)::integer AS specimen_date, ",
    "TIMESTAMPTZ '2010-01-01 00:00:00Z' + ",
    "(g % 1000000) * INTERVAL '1 second' AS specimen_time ",
    "FROM generate_series(1, 1000000) AS g"
  ))
  DBI::dbExecute(con, paste0("ANALYZE ", relation_sql))

  source <- epi_eda_postgres_source(con, schema, "observations")
  spec <- postgres_benchmark_spec()
  warmup <- epi_eda_db_run(source, spec, output_dir, plots = TRUE)
  expect_benchmark_bundle(warmup)

  elapsed <- numeric(3L)
  runs <- vector("list", 3L)
  for (index in seq_len(3L)) {
    started <- proc.time()[["elapsed"]]
    runs[[index]] <- epi_eda_db_run(
      source,
      spec,
      output_dir,
      overwrite = TRUE,
      plots = TRUE
    )
    elapsed[[index]] <- proc.time()[["elapsed"]] - started
    expect_benchmark_bundle(runs[[index]])
  }

  median_elapsed <- stats::median(elapsed)
  message(
    "Synthetic PostgreSQL EDA measured seconds: ",
    paste(format(round(elapsed, 3), nsmall = 3), collapse = ", "),
    "; median: ", format(round(median_elapsed, 3), nsmall = 3)
  )
  expect_lt(median_elapsed, 120)
})
