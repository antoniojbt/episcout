context("integrated PostgreSQL point maps")

postgres_map_connection <- function() {
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

postgres_map_spec <- function() {
  data.frame(
    name = c("lon", "lat", "numeric_theme", "category_theme", "text_theme"),
    label = c("Longitude", "Latitude", "Numeric", "Category", "Text"),
    database_type = "text", analysis_type = c("numeric", "numeric", "integer", "categorical", "text"),
    role = c("coordinate", "coordinate", "measure", "group", "comment"),
    levels = c("", "", "", "A;B", ""),
    missing_codes = c("", "", "", "MISSING", "MISSING"),
    geo_role = c("x", "y", "", "", ""),
    geo_pair = c("site", "site", "", "", ""),
    geo_crs = c("4326", "4326", "", "", ""),
    stringsAsFactors = FALSE
  )
}

postgres_map_fixture <- function(con) {
  schema <- paste0("epi_eda_maps_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  points <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = "points")
  ))
  limits <- as.character(DBI::dbQuoteIdentifier(
    con, DBI::Id(schema = schema, table = "limits")
  ))
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))
  DBI::dbExecute(con, paste(
    "CREATE TABLE", points,
    "(lon double precision, lat double precision, numeric_theme integer,",
    "category_theme text, text_theme text, observation_canary text)"
  ))
  DBI::dbExecute(con, paste(
    "INSERT INTO", points, "VALUES",
    "(-10, -5, 1, 'A', 'one', 'CANARY-A'),",
    "(0, 0, 2, 'MISSING', 'MISSING', 'CANARY-B'),",
    "(10, 5, 3, 'B', 'two', 'CANARY-C')"
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE", limits, "AS SELECT id,",
    "-10.0 + (id - 1) * 20.0 / 10000.0 AS lon,",
    "-5.0 + (id - 1) * 10.0 / 10000.0 AS lat",
    "FROM generate_series(1, 10001) AS id"
  ))
  list(schema = schema, schema_sql = schema_sql, points = points, limits = limits)
}

test_that("PostgreSQL collects only requested map observations inside the snapshot", {
  con <- postgres_map_connection()
  fixture <- postgres_map_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  source <- epi_eda_postgres_source(con, fixture$schema, "points")
  spec <- postgres_map_spec()

  disabled_dir <- tempfile("eda-pg-maps-disabled-")
  disabled <- epi_eda_db_run(source, spec, disabled_dir, plots = FALSE)
  expect_length(disabled$maps, 0L)
  expect_equal(nrow(disabled$map_inventory), 0L)
  expect_false(any(disabled$timings$query_kind == "map_collection"))
  expect_false(dir.exists(file.path(disabled_dir, "maps")))

  original_fetch <- getFromNamespace("eda_db_fetch", "episcout")
  snapshot_states <- logical()
  local_mocked_bindings(
    eda_db_fetch = function(con, statement, params = list(), query_kind,
                            limit, timing_env = NULL,
                            variable_index = NA_integer_, name = NA_character_) {
      if (identical(query_kind, "map_collection")) {
        snapshot_states <<- c(
          snapshot_states,
          getFromNamespace("eda_pg_is_transacting", "episcout")(con)
        )
      }
      original_fetch(
        con, statement, params, query_kind, limit, timing_env,
        variable_index, name
      )
    },
    .package = "episcout"
  )
  output_dir <- tempfile("eda-pg-maps-created-")
  observed <- epi_eda_db_run(
    source,
    spec,
    output_dir,
    plots = FALSE,
    maps = TRUE,
    map_vars = c("numeric_theme", "category_theme", "text_theme")
  )

  expected_ids <- c(
    "map-p001-geometry", "map-p001-v003", "map-p001-v004", "map-p001-v005"
  )
  expect_identical(observed$map_inventory$map_id, expected_ids)
  expect_named(observed$maps, expected_ids)
  expect_true(all(observed$map_inventory$status == "created"))
  expect_true(all(observed$map_inventory$n_source_rows == 3L))
  expect_true(all(observed$map_inventory$n_mapped == 3L))
  expect_identical(snapshot_states, TRUE)
  collection <- observed$timings[observed$timings$query_kind == "map_collection", ]
  expect_identical(nrow(collection), 1L)
  expect_identical(collection$rows_returned, 3L)
  expect_identical(collection$bounded_limit, 10001L)
  mapped_data <- sf::st_drop_geometry(observed$maps[[1]]$data)
  expect_named(
    mapped_data,
    c("lon", "lat", "numeric_theme", "category_theme", "text_theme")
  )
  expect_false("observation_canary" %in% names(mapped_data))
  expect_equal(sum(is.na(observed$maps[["map-p001-v004"]]$data$category_theme)), 1L)
  expect_equal(sum(is.na(observed$maps[["map-p001-v005"]]$data$text_theme)), 1L)
  expect_true(all(file.exists(file.path(output_dir, observed$map_inventory$path))))
  expect_identical(
    observed$manifest$path[observed$manifest$type == "map"],
    observed$map_inventory$path
  )
  expect_true(all(nzchar(observed$manifest$checksum_md5[observed$manifest$type == "map"])))
})

test_that("PostgreSQL failed QC and point limits never collect or truncate observations", {
  con <- postgres_map_connection()
  fixture <- postgres_map_fixture(con)
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  spec <- postgres_map_spec()
  DBI::dbExecute(con, paste("UPDATE", fixture$points, "SET lon = NULL WHERE numeric_theme = 1"))
  failed_source <- epi_eda_postgres_source(con, fixture$schema, "points")
  failed <- epi_eda_db_run(
    failed_source, spec, tempfile("eda-pg-maps-failed-"),
    plots = FALSE, maps = TRUE, map_vars = "numeric_theme"
  )
  expect_length(failed$maps, 0L)
  expect_true(all(failed$map_inventory$status == "not_created"))
  expect_true(all(failed$map_inventory$reason == "incomplete_pairs"))
  expect_true(all(failed$map_inventory$n_mapped == 0L))
  expect_false(any(failed$timings$query_kind == "map_collection"))
  expect_true(all(failed$summaries$variables$status == "summarised"))

  limit_spec <- spec[spec$name %in% c("lon", "lat"), , drop = FALSE]
  limit_source <- epi_eda_postgres_source(con, fixture$schema, "limits")
  over <- epi_eda_db_run(
    limit_source, limit_spec, tempfile("eda-pg-maps-over-"),
    plots = FALSE, maps = TRUE, max_map_points = 10000L
  )
  expect_identical(over$map_inventory$status, "not_created")
  expect_identical(over$map_inventory$reason, "max_map_points_exceeded")
  expect_identical(over$map_inventory$n_source_rows, 10001L)
  expect_identical(over$map_inventory$n_mapped, 0L)
  expect_false(any(over$timings$query_kind == "map_collection"))

  DBI::dbExecute(con, paste("DELETE FROM", fixture$limits, "WHERE id = 10001"))
  exact_source <- epi_eda_postgres_source(con, fixture$schema, "limits")
  exact <- epi_eda_db_run(
    exact_source, limit_spec, tempfile("eda-pg-maps-exact-"),
    plots = FALSE, maps = TRUE, max_map_points = 10000L
  )
  expect_identical(exact$map_inventory$status, "created")
  expect_identical(exact$map_inventory$n_source_rows, 10000L)
  expect_identical(exact$map_inventory$n_mapped, 10000L)
  exact_collection <- exact$timings[exact$timings$query_kind == "map_collection", ]
  expect_identical(exact_collection$rows_returned, 10000L)
  expect_identical(exact_collection$bounded_limit, 10001L)
})
