context("declared coordinate-pair PostgreSQL EDA")

geo_eda_postgres_connection <- function() {
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

geo_eda_postgres_fixture <- function(con) {
  schema <- paste0("epi_geo_qa_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  relation <- "odd coords"
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = relation)
  ))
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql,
    " (\"x coordinate\" double precision, \"y coordinate\" double precision, value integer)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES ",
    "(-180, -90, 1), (180, 90, 2), (181, 0, 3), ",
    "(NULL, 2, 4), (4, NULL, 5), (NULL, NULL, 6), ",
    "('NaN', 3, 7), ('Infinity', 4, 8), (-999, 5, 9), ",
    "(179.123456789, 45.987654321, 10)"
  ))
  spec <- data.frame(
    name = c("x coordinate", "y coordinate", "value"),
    label = c("Reviewed x", "Reviewed y", "Value"),
    database_type = "text", analysis_type = c("numeric", "numeric", "integer"),
    role = c("", "", ""),
    missing_codes = c("-999", "-999", ""),
    geo_role = c("x", "y", ""),
    geo_pair = c("neutral pair", "neutral pair", ""),
    geo_crs = c("4326", "4326", ""),
    stringsAsFactors = FALSE
  )
  list(schema = schema, relation = relation, spec = spec, table_sql = table_sql)
}

test_that("PostgreSQL coordinate QA matches independently stated aggregates", {
  con <- geo_eda_postgres_connection()
  fixture <- geo_eda_postgres_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(
        con,
        paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE")
      )
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  observed <- epi_eda_profile_geo(source, fixture$spec)

  frame <- data.frame(
    check.names = FALSE,
    "x coordinate" = c(-180, 180, 181, NA, 4, NA, NaN, Inf, -999, 179.123456789),
    "y coordinate" = c(-90, 90, 0, 2, NA, NA, 3, 4, 5, 45.987654321),
    value = seq_len(10)
  )
  expect_identical(observed, epi_eda_profile_geo(frame, fixture$spec))

  expect_identical(observed$geo_pair, "neutral pair")
  expect_identical(observed$n, 10L)
  expect_identical(observed$complete_pairs, 4L)
  expect_identical(observed$missing_x, 2L)
  expect_identical(observed$missing_y, 1L)
  expect_identical(observed$both_missing, 1L)
  expect_identical(observed$non_finite, 2L)
  expect_identical(observed$range_failures, 1L)
  expect_false(observed$map_ready)
  expect_identical(
    observed$reason,
    "incomplete_pairs;non_finite_coordinates;declared_crs_range_failure"
  )
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))
})

test_that("PostgreSQL numeric storage families follow declared coordinate types", {
  con <- geo_eda_postgres_connection()
  schema <- paste0("epi_geo_types_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  relation <- "coordinate types"
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  table_sql <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = relation)
  ))
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste0("DROP SCHEMA ", schema_sql, " CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  DBI::dbExecute(con, paste0("CREATE SCHEMA ", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_sql, " (",
    "xi integer, yi bigint, xn numeric, yn decimal, ",
    "xr real, yr real, xd double precision, yd double precision)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", table_sql, " VALUES (1, 2, 3, 4, 5, 6, 7, 8)"
  ))
  names <- c("xi", "yi", "xn", "yn", "xr", "yr", "xd", "yd")
  spec <- data.frame(
    name = names,
    label = names,
    database_type = "text", analysis_type = c("integer", "integer", rep("numeric", 6L)),
    role = "",
    geo_role = rep(c("x", "y"), 4L),
    geo_pair = rep(c("integer", "numeric", "real", "double"), each = 2L),
    geo_crs = "3857",
    stringsAsFactors = FALSE
  )
  source <- epi_eda_postgres_source(con, schema, relation)
  observed <- epi_eda_profile_geo(source, spec)

  expect_identical(observed$geo_pair, c("integer", "numeric", "real", "double"))
  expect_true(all(observed$map_ready))
  expect_identical(observed$complete_pairs, rep(1L, 4L))
})

test_that("PostgreSQL bundle publishes one-row aggregate QA without PostGIS", {
  con <- geo_eda_postgres_connection()
  fixture <- geo_eda_postgres_fixture(con)
  output_dir <- tempfile("eda-geo-pg-bundle-")
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(
        con,
        paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE")
      )
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  run <- epi_eda_db_run(source, fixture$spec, output_dir, plots = FALSE)

  expect_identical(run$geo$n, 10L)
  expect_identical(
    run$manifest$status[run$manifest$artifact == "geo_qa"],
    "created"
  )
  expect_true(file.exists(file.path(output_dir, "geo_qa.csv")))
  timings <- run$timings[run$timings$query_kind == "geo_pair_qa", ]
  expect_identical(nrow(timings), 1L)
  expect_identical(timings$rows_returned, 1L)
  expect_identical(timings$bounded_limit, 1L)
  expect_identical(
    names(run$manifest),
    c("artifact", "type", "path", "status", "checksum_md5")
  )
  expect_length(run$maps, 0L)
  expect_equal(nrow(run$map_inventory), 0L)
  expect_false(any(run$timings$query_kind == "map_collection"))
})

test_that("PostgreSQL coordinate failures remain value-free and recover", {
  con <- geo_eda_postgres_connection()
  fixture <- geo_eda_postgres_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(
        con,
        paste0("DROP SCHEMA ", DBI::dbQuoteIdentifier(con, fixture$schema), " CASCADE")
      )
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, fixture$relation)
  broken <- fixture$spec
  broken$name[[1]] <- "absent coordinate"
  condition <- tryCatch(epi_eda_profile_geo(source, broken), error = identity)

  expect_s3_class(condition, "error")
  expect_false(grepl("181", conditionMessage(condition), fixed = TRUE))
  expect_false(getFromNamespace("eda_pg_is_transacting", "episcout")(con))
  expect_true(DBI::dbIsValid(con))
})
