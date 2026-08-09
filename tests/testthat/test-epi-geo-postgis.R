context("Read-only bounded PostGIS geospatial collection")

geo_postgis_test_source <- function(con) {
  columns <- data.frame(
    name = c("feature id", "label", "geom"),
    ordinal_position = 1:3,
    udt_name = c("int4", "text", "geometry"),
    base_udt_name = c("int4", "text", "geometry"),
    typtype = c("b", "b", "b"),
    formatted_type = c("integer", "text", "geometry(Point,4326)"),
    collation_schema = c(NA, "pg_catalog", NA),
    collation_name = c(NA, "default", NA),
    collation_deterministic = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  source <- structure(
    list(
      con = con,
      schema = "CANARY_SCHEMA",
      relation = "CANARY_RELATION",
      relation_kind = "table",
      geometry_column = "geom",
      storage = "geometry",
      declared_type = "POINT",
      declared_dimension = 2L,
      declared_srid = 4326L,
      columns = columns,
      source_version = "geo-postgis-source-1"
    ),
    relation_oid = "123",
    catalogue_fingerprint = "CANARY_FINGERPRINT",
    source_connection = con,
    class = c("epi_geo_postgis_source", "list")
  )
  attr(source, "source_fingerprint") <- getFromNamespace(
    "geo_pg_source_signature",
    "episcout"
  )(source)
  source
}

test_that("PostGIS geo public formals and redacted source methods are fixed", {
  expect_identical(
    names(formals(epi_geo_postgis_source)),
    c("con", "schema", "relation", "geometry_column")
  )
  expect_identical(names(formals(epi_geo_postgis_describe)), "source")
  expect_identical(
    names(formals(epi_geo_postgis_collect)),
    c("source", "columns", "bbox", "max_features")
  )

  source <- geo_postgis_test_source(structure(
    list(host = "CANARY_HOST", password = "CANARY_PASSWORD"),
    class = "secret_connection"
  ))
  output <- paste(c(capture.output(print(source)), capture.output(str(source))), collapse = "\n")
  expect_match(output, "relation kind: table", fixed = TRUE)
  expect_match(output, "declared SRID: 4326", fixed = TRUE)
  expect_false(grepl("CANARY|password|host", output, ignore.case = TRUE))
})

test_that("PostGIS source and collection input gates reject ambiguous values", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- geo_postgis_test_source(con)
  columns <- getFromNamespace("epi_geo_postgis_columns", "episcout")
  max_features <- getFromNamespace("epi_geo_postgis_max_features", "episcout")
  bbox <- getFromNamespace("epi_geo_postgis_bbox", "episcout")

  expect_identical(columns(source, c("label", "feature id")), c("label", "feature id"))
  expect_error(columns(source, c("label", "label")), "unique character")
  expect_error(columns(source, "geom"), "must not include")
  expect_error(columns(source, "absent"), "absent")
  expect_error(columns(source, "public.label"), "SQL fragments")
  expect_identical(max_features(1), 1L)
  expect_error(max_features(0), "positive whole number")
  expect_error(max_features(1.5), "positive whole number")

  reviewed <- sf::st_bbox(c(xmin = -1, ymin = -2, xmax = 3, ymax = 4), crs = 4326)
  expect_identical(bbox(reviewed, 4326L), c(-1, -2, 3, 4))
  expect_error(bbox(unclass(reviewed), 4326L), "sf bbox")
  expect_error(
    bbox(sf::st_bbox(c(xmin = 3, ymin = -2, xmax = -1, ymax = 4), crs = 4326), 4326L),
    "finite ordered"
  )
  expect_error(
    bbox(sf::st_bbox(c(xmin = -1, ymin = -2, xmax = 3, ymax = 4), crs = 3857), 4326L),
    "source CRS"
  )
})

test_that("PostGIS catalogue refuses a database without the extension", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  catalogue <- getFromNamespace("epi_geo_postgis_catalogue", "episcout")
  expect_error(
    with_mocked_bindings(
      catalogue(con, "public", "places"),
      eda_db_fetch = function(...) data.frame(extversion = character()),
      .package = "episcout"
    ),
    "does not have PostGIS enabled"
  )
})

test_that("aggregate description queries cannot return feature geometry or attributes", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- geo_postgis_test_source(con)
  describe_inside <- getFromNamespace("geo_pg_description_inside", "episcout")
  statements <- character()
  fetch <- function(con, statement, params = list(), query_kind, limit, ...) {
    statements <<- c(statements, statement)
    if (query_kind == "geo_postgis_types") return(data.frame(geometry_type = "POINT", n = "3"))
    if (query_kind == "geo_postgis_srids") return(data.frame(srid = 4326L, n = "3"))
    if (query_kind == "geo_postgis_dimensions") return(data.frame(dimension = 2L, n = "3"))
    if (query_kind == "geo_postgis_validity_counts") {
      return(data.frame(
        n_features = "4", n_non_null = "3", n_null = "1",
        n_empty = "0", n_valid = "2", n_invalid = "1"
      ))
    }
    if (query_kind == "geo_postgis_extent") {
      return(data.frame(xmin = -1, ymin = -2, xmax = 3, ymax = 4))
    }
    stop("unexpected query kind", call. = FALSE)
  }

  observed <- with_mocked_bindings(
    describe_inside(source),
    eda_db_fetch = fetch,
    .package = "episcout"
  )

  expect_s3_class(observed, "epi_geo_postgis_description")
  expect_identical(
    names(observed),
    c("source", "dataset", "geometry_types", "srids", "dimensions", "validity", "bounding_box", "messages")
  )
  expect_identical(as.numeric(observed$bounding_box), c(-1, -2, 3, 4))
  expect_identical(observed$validity$invalid, 1)
  inventory <- paste(statements, collapse = "\n")
  expect_false(grepl("ST_AsEWKB|ST_AsText|ST_AsBinary", inventory, ignore.case = TRUE))
  expect_false(grepl("feature id|label", inventory, fixed = TRUE))
  expect_true(all(grepl("^SELECT", trimws(statements))))
  expect_true(all(grepl("LIMIT 2$", statements[seq_len(3L)])))
})

test_that("observed PostGIS metadata fails closed", {
  resolve <- getFromNamespace("geo_pg_resolve_contract", "episcout")
  source <- geo_postgis_test_source(NULL)
  observed <- list(
    types = data.frame(geometry_type = "POINT", n = 1),
    srids = data.frame(srid = 4326L, n = 1),
    dimensions = data.frame(dimension = 2L, n = 1)
  )
  expect_identical(resolve(source, observed), list(type = "POINT", srid = 4326L, dimension = 2L))

  mixed_type <- observed
  mixed_type$types <- data.frame(geometry_type = c("POINT", "LINESTRING"), n = c(1, 1))
  expect_error(resolve(source, mixed_type), "mixed observed types")
  mixed_srid <- observed
  mixed_srid$srids <- data.frame(srid = c(4326L, 3857L), n = c(1, 1))
  expect_error(resolve(source, mixed_srid), "mixed observed SRIDs")
  z <- observed
  z$dimensions$dimension <- 3L
  expect_error(resolve(source, z), "Only XY")
  unsupported <- observed
  unsupported$types$geometry_type <- "GEOMETRYCOLLECTION"
  expect_error(resolve(source, unsupported), "unsupported observed type")
})

test_that("bounded collection quotes identifiers, binds extents and reconciles rows", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- geo_postgis_test_source(con)
  collect_inside <- getFromNamespace("epi_geo_postgis_collect_inside", "episcout")
  bbox <- sf::st_bbox(c(xmin = -1, ymin = -2, xmax = 3, ymax = 4), crs = 4326)
  point <- sf::st_as_binary(sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326), EWKB = TRUE)[[1L]]
  calls <- list()
  fetch <- function(con, statement, params = list(), query_kind, limit, ...) {
    calls[[query_kind]] <<- list(statement = statement, params = params, limit = limit)
    if (query_kind == "geo_postgis_types") return(data.frame(geometry_type = "POINT", n = "1"))
    if (query_kind == "geo_postgis_srids") return(data.frame(srid = 4326L, n = "1"))
    if (query_kind == "geo_postgis_dimensions") return(data.frame(dimension = 2L, n = "1"))
    if (query_kind == "geo_postgis_selection_count") return(data.frame(n = "1"))
    if (query_kind == "geo_postgis_feature_collection") {
      out <- data.frame("feature id" = 7L, label = "reviewed", check.names = FALSE)
      out[[".episcout_geometry_wkb"]] <- I(list(point))
      return(out)
    }
    stop("unexpected query kind", call. = FALSE)
  }

  observed <- with_mocked_bindings(
    collect_inside(source, c("feature id", "label"), bbox, 2L),
    eda_db_fetch = fetch,
    .package = "episcout"
  )
  expect_s3_class(observed, "sf")
  expect_identical(names(observed), c("feature id", "label", "geom"))
  expect_equal(as.numeric(sf::st_coordinates(observed)[1, ]), c(1, 2))
  expect_identical(calls$geo_postgis_selection_count$params, c(as.list(c(-1, -2, 3, 4)), list(4326L)))
  expect_identical(tail(calls$geo_postgis_feature_collection$params, 1L), list(3L))
  expect_match(calls$geo_postgis_feature_collection$statement, "`feature id`", fixed = TRUE)
  expect_match(calls$geo_postgis_feature_collection$statement, "ST_AsEWKB", fixed = TRUE)
  expect_match(calls$geo_postgis_feature_collection$statement, "LIMIT $6::bigint", fixed = TRUE)
})

test_that("bounded collection refuses over-limit and null geometry", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- geo_postgis_test_source(con)
  collect_inside <- getFromNamespace("epi_geo_postgis_collect_inside", "episcout")
  metadata <- function(query_kind) {
    if (query_kind == "geo_postgis_types") return(data.frame(geometry_type = "POINT", n = "1"))
    if (query_kind == "geo_postgis_srids") return(data.frame(srid = 4326L, n = "1"))
    if (query_kind == "geo_postgis_dimensions") return(data.frame(dimension = 2L, n = "1"))
    NULL
  }
  expect_error(
    with_mocked_bindings(
      collect_inside(source, character(), NULL, 1L),
      eda_db_fetch = function(con, statement, params = list(), query_kind, limit, ...) {
        result <- metadata(query_kind)
        if (!is.null(result)) {
          return(result)
        }
        if (query_kind == "geo_postgis_selection_count") {
          return(data.frame(n = "2"))
        }
        stop("feature query must not run", call. = FALSE)
      },
      .package = "episcout"
    ),
    "exceeds max_features"
  )

  null_wkb <- structure(list(NULL), class = "AsIs")
  expect_error(
    with_mocked_bindings(
      collect_inside(source, character(), NULL, 1L),
      eda_db_fetch = function(con, statement, params = list(), query_kind, limit, ...) {
        result <- metadata(query_kind)
        if (!is.null(result)) {
          return(result)
        }
        if (query_kind == "geo_postgis_selection_count") {
          return(data.frame(n = "1"))
        }
        out <- data.frame(row.names = 1L)
        out[[".episcout_geometry_wkb"]] <- null_wkb
        out
      },
      .package = "episcout"
    ),
    "null geometry"
  )
})

test_that("PostGIS transaction failures remain value-free and reusable", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- geo_postgis_test_source(con)
  transaction <- getFromNamespace("epi_geo_postgis_transaction", "episcout")
  begin_error <- tryCatch(
    with_mocked_bindings(
      transaction(source, 1L),
      geo_pg_validate_source = function(...) invisible(TRUE),
      eda_db_begin = function(...) stop("BEGIN_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(begin_error), "could not begin", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(begin_error), fixed = TRUE))

  commit_error <- tryCatch(
    with_mocked_bindings(
      transaction(source, 1L),
      geo_pg_validate_source = function(...) invisible(TRUE),
      eda_db_statement = function(...) invisible(TRUE),
      eda_db_commit = function(...) stop("COMMIT_CANARY", call. = FALSE),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(commit_error), "could not commit safely", fixed = TRUE)
  expect_false(grepl("CANARY", conditionMessage(commit_error), fixed = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

geo_postgis_connection <- function() {
  if (Sys.getenv("EPISCOUT_TEST_POSTGRES") != "1") {
    skip("Set EPISCOUT_TEST_POSTGRES=1 for disposable PostGIS integration tests.")
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

geo_postgis_fixture <- function(con) {
  extension <- DBI::dbGetQuery(con, "SELECT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'postgis') AS available")
  skip_if_not(isTRUE(extension$available[[1L]]), "The disposable PostgreSQL database must have PostGIS enabled.")
  schema <- paste0("epi_geo_", Sys.getpid(), "_", sample.int(1000000L, 1L))
  schema_sql <- as.character(DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, paste("CREATE SCHEMA", schema_sql))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", schema_sql, ".points (",
    '"feature id" integer, label text, geom geometry(Point,4326))'
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", schema_sql, ".points VALUES ",
    "(1, 'one', ST_GeomFromText('POINT(0 0)', 4326)), ",
    "(2, 'two', ST_GeomFromText('POINT(1 1)', 4326)), ",
    "(3, 'three', ST_GeomFromText('POINT(2 2)', 4326)), ",
    "(4, 'null', NULL), ",
    "(5, 'empty', ST_GeomFromText('POINT EMPTY', 4326))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", schema_sql,
    ".polygons (id integer, geom geometry(Polygon,4326))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", schema_sql, ".polygons VALUES ",
    "(1, ST_GeomFromText('POLYGON((0 0,2 2,0 2,2 0,0 0))', 4326))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", schema_sql,
    ".places (id integer, geog geography(Point,4326))"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", schema_sql,
    ".places VALUES (1, ST_GeogFromText('SRID=4326;POINT(1 2)'))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", schema_sql,
    ".point_view AS SELECT \"feature id\", geom FROM ", schema_sql, ".points WHERE geom IS NOT NULL"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE MATERIALIZED VIEW ", schema_sql,
    ".point_materialized AS SELECT \"feature id\", geom FROM ", schema_sql, ".points WHERE geom IS NOT NULL"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", schema_sql,
    ".mixed_types AS SELECT ST_GeomFromText('POINT(0 0)', 4326)::geometry AS geom ",
    "UNION ALL SELECT ST_GeomFromText('LINESTRING(0 0,1 1)', 4326)::geometry"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", schema_sql,
    ".mixed_srids AS SELECT ST_GeomFromText('POINT(0 0)', 4326)::geometry AS geom ",
    "UNION ALL SELECT ST_GeomFromText('POINT(0 0)', 3857)::geometry"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", schema_sql,
    ".z_points AS SELECT ST_GeomFromText('POINT Z(0 0 1)', 4326)::geometry AS geom"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", schema_sql,
    ".multiple_geometry (a geometry(Point,4326), b geometry(Point,4326))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", schema_sql,
    ".empty_points (id integer, geom geometry(Point,4326))"
  ))
  list(schema = schema, schema_sql = schema_sql)
}

test_that("live PostGIS description and collection satisfy the reviewed contract", {
  con <- geo_postgis_connection()
  fixture <- geo_postgis_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  source <- epi_geo_postgis_source(con, fixture$schema, "points")
  description <- epi_geo_postgis_describe(source)
  expect_identical(description$dataset$features, 5)
  expect_identical(description$dataset$non_null_geometry, 4)
  expect_identical(description$validity, data.frame(valid = 3, invalid = 0, empty = 1, null = 1))
  expect_identical(as.numeric(description$bounding_box), c(0, 0, 2, 2))
  expect_identical(description$geometry_types$geometry_type, "POINT")
  expect_identical(description$geometry_types$count, 4)

  empty <- epi_geo_postgis_describe(epi_geo_postgis_source(con, fixture$schema, "empty_points"))
  expect_identical(empty$dataset$features, 0)
  expect_true(all(is.na(as.numeric(empty$bounding_box))))
  empty_collect <- epi_geo_postgis_collect(
    epi_geo_postgis_source(con, fixture$schema, "empty_points"),
    columns = "id",
    max_features = 1L
  )
  expect_s3_class(empty_collect, "sf")
  expect_identical(nrow(empty_collect), 0L)
  expect_identical(as.character(sf::st_geometry_type(empty_collect)), character())

  reviewed_bbox <- sf::st_bbox(c(xmin = -1, ymin = -1, xmax = 1, ymax = 1), crs = 4326)
  collected <- epi_geo_postgis_collect(
    source,
    columns = c("feature id", "label"),
    bbox = reviewed_bbox,
    max_features = 2L
  )
  expect_s3_class(collected, "sf")
  expect_identical(collected[["feature id"]], 1:2)
  expect_identical(collected$label, c("one", "two"))
  expect_identical(as.numeric(sf::st_bbox(collected)), c(0, 0, 1, 1))
  expect_error(
    epi_geo_postgis_collect(source, bbox = reviewed_bbox, max_features = 1L),
    "exceeds max_features"
  )
  expect_error(
    epi_geo_postgis_collect(source, max_features = 5L),
    "null geometry"
  )
  expect_true(DBI::dbIsValid(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS reusable")$reusable, 1L)
})

test_that("live PostGIS supports approved relations and fails unsafe metadata", {
  con <- geo_postgis_connection()
  fixture <- geo_postgis_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )

  expect_identical(epi_geo_postgis_source(con, fixture$schema, "point_view")$relation_kind, "view")
  expect_identical(epi_geo_postgis_source(con, fixture$schema, "point_materialized")$relation_kind, "materialized view")
  geography <- epi_geo_postgis_source(con, fixture$schema, "places")
  expect_identical(geography$storage, "geography")
  expect_identical(epi_geo_postgis_describe(geography)$source$srid, 4326L)
  expect_s3_class(epi_geo_postgis_collect(geography, "id", max_features = 1L), "sf")
  expect_error(epi_geo_postgis_source(con, fixture$schema, "multiple_geometry"), "geometry_column is required")
  expect_s3_class(
    epi_geo_postgis_source(con, fixture$schema, "multiple_geometry", "b"),
    "epi_geo_postgis_source"
  )
  expect_error(
    epi_geo_postgis_describe(epi_geo_postgis_source(con, fixture$schema, "mixed_types")),
    "mixed observed types"
  )
  expect_error(
    epi_geo_postgis_describe(epi_geo_postgis_source(con, fixture$schema, "mixed_srids")),
    "mixed observed SRIDs"
  )
  expect_error(
    epi_geo_postgis_describe(epi_geo_postgis_source(con, fixture$schema, "z_points")),
    "Only XY"
  )
  invalid <- epi_geo_postgis_describe(epi_geo_postgis_source(con, fixture$schema, "polygons"))
  expect_identical(invalid$validity$invalid, 1)

  modified <- epi_geo_postgis_source(con, fixture$schema, "points")
  modified$declared_srid <- 3857L
  expect_error(epi_geo_postgis_describe(modified), "unmodified object")

  drift <- epi_geo_postgis_source(con, fixture$schema, "points")
  DBI::dbExecute(con, paste("ALTER TABLE", fixture$schema_sql, ".points ADD COLUMN changed integer"))
  expect_error(epi_geo_postgis_describe(drift), "catalogue changed")
  expect_true(DBI::dbIsValid(con))
})

test_that("live PostGIS rejects caller transactions and keeps ownership", {
  con <- geo_postgis_connection()
  fixture <- geo_postgis_fixture(con)
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        if (DBI::dbIsValid(con)) try(DBI::dbRollback(con), silent = TRUE)
        DBI::dbExecute(con, paste("DROP SCHEMA", fixture$schema_sql, "CASCADE"))
        DBI::dbDisconnect(con)
      }
    },
    add = TRUE
  )
  source <- epi_geo_postgis_source(con, fixture$schema, "points")
  DBI::dbBegin(con)
  expect_error(epi_geo_postgis_describe(source), "caller-managed transaction")
  DBI::dbRollback(con)
  expect_true(DBI::dbIsValid(con))

  DBI::dbDisconnect(con)
  expect_error(epi_geo_postgis_describe(source), "open RPostgres connection")
  con <- geo_postgis_connection()
})
