geo_pg_supported_types <- function() {
  c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
}

epi_geo_postgis_count <- function(value, field) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) {
    stop(field, " was not returned as exact decimal text.", call. = FALSE)
  }
  result <- suppressWarnings(as.numeric(value))
  if (!is.finite(result) || result > 2^53) {
    stop(field, " exceeds the exact R numeric range.", call. = FALSE)
  }
  result
}

epi_geo_postgis_catalogue <- function(con, schema, relation) {
  extension <- eda_db_fetch(
    con,
    paste(
      "SELECT e.extversion",
      "FROM pg_catalog.pg_extension AS e",
      "WHERE e.extname = $1"
    ),
    params = list("postgis"),
    query_kind = "geo_postgis_extension",
    limit = 1L
  )
  if (nrow(extension) != 1L) {
    stop("The current PostgreSQL database does not have PostGIS enabled.", call. = FALSE)
  }

  catalogue <- eda_postgres_catalogue(con, schema, relation)
  if (!catalogue$relation_kind %in% c("table", "partitioned table", "view", "materialized view")) {
    stop("The requested PostGIS object has an unsupported relation kind.", call. = FALSE)
  }
  spatial <- eda_db_fetch(
    con,
    paste(
      "SELECT a.attname AS name, bt.typname AS storage,",
      "postgis_typmod_type(a.atttypmod) AS declared_type,",
      "postgis_typmod_dims(a.atttypmod)::integer AS declared_dimension,",
      "postgis_typmod_srid(a.atttypmod)::integer AS declared_srid",
      "FROM pg_catalog.pg_attribute AS a",
      "INNER JOIN pg_catalog.pg_type AS t ON t.oid = a.atttypid",
      "INNER JOIN pg_catalog.pg_type AS bt ON bt.oid = CASE WHEN t.typtype = 'd' THEN t.typbasetype ELSE t.oid END",
      "WHERE a.attrelid = $1::oid AND a.attnum > 0 AND NOT a.attisdropped",
      "AND bt.typname IN ('geometry', 'geography')",
      "ORDER BY a.attnum"
    ),
    params = list(catalogue$relation_oid),
    query_kind = "geo_postgis_spatial_catalogue",
    limit = nrow(catalogue$columns)
  )
  list(
    relation_oid = catalogue$relation_oid,
    relation_kind = catalogue$relation_kind,
    columns = catalogue$columns,
    spatial = as.data.frame(spatial, stringsAsFactors = FALSE)
  )
}

epi_geo_postgis_fingerprint <- function(catalogue) {
  eda_postgres_fingerprint(list(
    relation_oid = catalogue$relation_oid,
    relation_kind = catalogue$relation_kind,
    columns = catalogue$columns,
    spatial = catalogue$spatial
  ))
}

geo_pg_source_signature <- function(source) {
  eda_postgres_fingerprint(list(
    schema = source$schema,
    relation = source$relation,
    relation_kind = source$relation_kind,
    geometry_column = source$geometry_column,
    storage = source$storage,
    declared_type = source$declared_type,
    declared_dimension = source$declared_dimension,
    declared_srid = source$declared_srid,
    columns = source$columns,
    source_version = source$source_version
  ))
}

#' Reference a reviewed PostGIS relation
#'
#' Validate one exact spatial relation on a caller-owned RPostgres connection
#' without collecting feature geometry or ordinary attribute values.
#'
#' @param con An open, idle connection created by RPostgres.
#' @param schema One exact PostgreSQL schema identifier. Dotted names and SQL
#'   fragments are not accepted.
#' @param relation One exact table, partitioned table, view or materialized view
#'   identifier in `schema`.
#' @param geometry_column `NULL` when the relation has exactly one PostGIS
#'   geometry/geography column, or its exact identifier when selection is
#'   required.
#'
#' @return An `epi_geo_postgis_source` object. Its print and structure methods
#'   redact the connection and relation identity.
#'
#' @details The current database must already have PostGIS enabled. The
#'   function never installs extensions, creates objects or accepts connection
#'   credentials or SQL. The connection remains caller-owned, open and idle.
#'
#' @export
epi_geo_postgis_source <- function(con, schema, relation, geometry_column = NULL) {
  epi_geo_require("sf")
  eda_pg_validate_connection(con, require_idle = TRUE)
  schema <- eda_postgres_identifier(schema, "schema")
  relation <- eda_postgres_identifier(relation, "relation")
  if (!is.null(geometry_column)) {
    geometry_column <- eda_postgres_identifier(geometry_column, "geometry_column")
  }
  catalogue <- epi_geo_postgis_catalogue(con, schema, relation)
  if (nrow(catalogue$spatial) == 0L) {
    stop("The requested relation has no PostGIS geometry or geography column.", call. = FALSE)
  }
  if (is.null(geometry_column)) {
    if (nrow(catalogue$spatial) != 1L) {
      stop("geometry_column is required when a relation has multiple spatial columns.", call. = FALSE)
    }
    selected <- 1L
  } else {
    selected <- match(geometry_column, catalogue$spatial$name)
    if (is.na(selected)) {
      stop("geometry_column must identify one PostGIS geometry or geography column.", call. = FALSE)
    }
  }
  spatial <- catalogue$spatial[selected, , drop = FALSE]
  declared_type <- toupper(as.character(spatial$declared_type[[1L]]))
  declared_dimension <- as.integer(spatial$declared_dimension[[1L]])
  declared_srid <- as.integer(spatial$declared_srid[[1L]])
  if (!is.na(declared_type) && declared_type != "GEOMETRY" &&
        !declared_type %in% geo_pg_supported_types()) {
    stop("The selected spatial column has an unsupported declared geometry type.", call. = FALSE)
  }
  if (!is.na(declared_dimension) && declared_dimension != 2L) {
    stop("Only XY PostGIS geometry is supported.", call. = FALSE)
  }
  source <- structure(
    list(
      con = con,
      schema = schema,
      relation = relation,
      relation_kind = catalogue$relation_kind,
      geometry_column = as.character(spatial$name[[1L]]),
      storage = as.character(spatial$storage[[1L]]),
      declared_type = declared_type,
      declared_dimension = declared_dimension,
      declared_srid = declared_srid,
      columns = catalogue$columns,
      source_version = "geo-postgis-source-1"
    ),
    relation_oid = catalogue$relation_oid,
    catalogue_fingerprint = epi_geo_postgis_fingerprint(catalogue),
    source_connection = con,
    class = c("epi_geo_postgis_source", "list")
  )
  attr(source, "source_fingerprint") <- geo_pg_source_signature(source)
  source
}

#' @export
print.epi_geo_postgis_source <- function(x, ...) {
  cat("<episcout PostGIS source>\n")
  cat("  relation kind: ", x$relation_kind, "\n", sep = "")
  cat("  storage: ", x$storage, "\n", sep = "")
  cat("  declared geometry: ", x$declared_type, "\n", sep = "")
  cat("  declared dimension: ", x$declared_dimension, "\n", sep = "")
  cat("  declared SRID: ", x$declared_srid, "\n", sep = "")
  invisible(x)
}

#' @export
str.epi_geo_postgis_source <- function(object, ...) {
  print(object)
  cat("  connection, relation identity and column identifiers: <redacted>\n")
  invisible(object)
}

geo_pg_validate_source <- function(source, require_idle = TRUE) {
  expected <- c(
    "con", "schema", "relation", "relation_kind", "geometry_column",
    "storage", "declared_type", "declared_dimension", "declared_srid",
    "columns", "source_version"
  )
  valid <- inherits(source, "epi_geo_postgis_source") &&
    identical(class(source), c("epi_geo_postgis_source", "list")) &&
    identical(names(source), expected) &&
    identical(source$source_version, "geo-postgis-source-1") &&
    is.data.frame(source$columns) &&
    !is.null(attr(source, "relation_oid")) &&
    !is.null(attr(source, "catalogue_fingerprint")) &&
    !is.null(attr(source, "source_connection")) &&
    !is.null(attr(source, "source_fingerprint")) &&
    identical(source$con, attr(source, "source_connection")) &&
    identical(geo_pg_source_signature(source), attr(source, "source_fingerprint"))
  if (!valid) {
    stop("source must be an unmodified object returned by epi_geo_postgis_source().", call. = FALSE)
  }
  eda_pg_validate_connection(source$con, require_idle = require_idle)
  schema <- eda_postgres_identifier(source$schema, "source schema")
  relation <- eda_postgres_identifier(source$relation, "source relation")
  current <- epi_geo_postgis_catalogue(source$con, schema, relation)
  selected <- current$spatial[current$spatial$name == source$geometry_column, , drop = FALSE]
  same <- identical(current$relation_oid, attr(source, "relation_oid")) &&
    identical(current$relation_kind, source$relation_kind) &&
    identical(nrow(selected), 1L) &&
    identical(epi_geo_postgis_fingerprint(current), attr(source, "catalogue_fingerprint"))
  if (!same) {
    stop("The PostGIS source catalogue changed after construction; construct a new source after review.", call. = FALSE)
  }
  invisible(current)
}

epi_geo_postgis_transaction <- function(source, code) {
  geo_pg_validate_source(source, require_idle = TRUE)
  con <- source$con
  eda_db_lifecycle_call(
    eda_db_begin(con),
    "The read-only PostGIS transaction could not begin; review restricted database logs."
  )
  finished <- FALSE
  on.exit(
    {
      if (!finished && DBI::dbIsValid(con)) {
        try(DBI::dbRollback(con), silent = TRUE)
      }
    },
    add = TRUE
  )
  eda_db_statement(
    con,
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY",
    query_kind = "geo_postgis_transaction_setup"
  )
  geo_pg_validate_source(source, require_idle = FALSE)
  value <- force(code)
  eda_db_lifecycle_call(
    eda_db_commit(con),
    "The read-only PostGIS transaction could not commit safely; review restricted database logs."
  )
  finished <- TRUE
  value
}

epi_geo_postgis_table_sql <- function(source) {
  as.character(DBI::dbQuoteIdentifier(
    source$con,
    DBI::Id(schema = source$schema, table = source$relation)
  ))
}

epi_geo_postgis_geometry_sql <- function(source) {
  quoted <- as.character(DBI::dbQuoteIdentifier(source$con, source$geometry_column))
  if (identical(source$storage, "geography")) {
    paste0("(", quoted, "::geometry)")
  } else {
    quoted
  }
}

epi_geo_postgis_grouped <- function(source, expression, alias, query_kind) {
  geometry <- epi_geo_postgis_geometry_sql(source)
  table <- epi_geo_postgis_table_sql(source)
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT ", expression, " AS ", alias, ", count(*)::text AS n ",
      "FROM ", table, " WHERE ", geometry, " IS NOT NULL ",
      "GROUP BY ", expression, " ORDER BY ", expression, " LIMIT 2"
    ),
    query_kind = query_kind,
    limit = 2L
  )
  observed$n <- vapply(observed$n, epi_geo_postgis_count, numeric(1), field = "PostGIS aggregate count")
  observed
}

geo_pg_observed_metadata <- function(source) {
  geometry <- epi_geo_postgis_geometry_sql(source)
  types <- epi_geo_postgis_grouped(
    source,
    paste0("upper(replace(ST_GeometryType(", geometry, "), 'ST_', ''))"),
    "geometry_type",
    "geo_postgis_types"
  )
  srids <- epi_geo_postgis_grouped(
    source,
    paste0("ST_SRID(", geometry, ")"),
    "srid",
    "geo_postgis_srids"
  )
  dimensions <- epi_geo_postgis_grouped(
    source,
    paste0("ST_NDims(", geometry, ")"),
    "dimension",
    "geo_postgis_dimensions"
  )
  types$geometry_type <- as.character(types$geometry_type)
  srids$srid <- as.integer(srids$srid)
  dimensions$dimension <- as.integer(dimensions$dimension)
  list(types = types, srids = srids, dimensions = dimensions)
}

geo_pg_resolve_contract <- function(source, observed) {
  if (nrow(observed$types) > 1L) {
    stop("The selected PostGIS geometry has mixed observed types.", call. = FALSE)
  }
  if (nrow(observed$types) == 1L &&
        !observed$types$geometry_type[[1L]] %in% geo_pg_supported_types()) {
    stop("The selected PostGIS geometry has an unsupported observed type.", call. = FALSE)
  }
  if (nrow(observed$srids) > 1L) {
    stop("The selected PostGIS geometry has mixed observed SRIDs.", call. = FALSE)
  }
  observed_srid <- if (nrow(observed$srids) == 1L) observed$srids$srid[[1L]] else NA_integer_
  srid <- if (!is.na(observed_srid)) observed_srid else source$declared_srid
  if (is.na(srid) || srid <= 0L) {
    stop("The selected PostGIS geometry has an unknown SRID.", call. = FALSE)
  }
  if (!is.na(source$declared_srid) && source$declared_srid > 0L &&
        !is.na(observed_srid) && observed_srid != source$declared_srid) {
    stop("The selected PostGIS geometry does not match its declared SRID.", call. = FALSE)
  }
  if (nrow(observed$dimensions) > 1L) {
    stop("The selected PostGIS geometry has mixed observed dimensions.", call. = FALSE)
  }
  observed_dimension <- if (nrow(observed$dimensions) == 1L) observed$dimensions$dimension[[1L]] else NA_integer_
  dimension <- if (!is.na(observed_dimension)) observed_dimension else source$declared_dimension
  if (is.na(dimension) || dimension != 2L) {
    stop("Only XY PostGIS geometry is supported.", call. = FALSE)
  }
  if (!is.na(source$declared_dimension) && !is.na(observed_dimension) &&
        observed_dimension != source$declared_dimension) {
    stop("The selected PostGIS geometry does not match its declared dimension.", call. = FALSE)
  }
  type <- if (nrow(observed$types) == 1L) observed$types$geometry_type[[1L]] else source$declared_type
  if (is.na(type) || identical(type, "GEOMETRY") || !type %in% geo_pg_supported_types()) {
    stop("The selected PostGIS geometry has an unknown or unsupported type.", call. = FALSE)
  }
  if (!is.na(source$declared_type) && source$declared_type != "GEOMETRY" &&
        type != source$declared_type) {
    stop("The selected PostGIS geometry does not match its declared type.", call. = FALSE)
  }
  list(type = type, srid = as.integer(srid), dimension = as.integer(dimension))
}

geo_pg_description_inside <- function(source) {
  table <- epi_geo_postgis_table_sql(source)
  geometry <- epi_geo_postgis_geometry_sql(source)
  observed <- geo_pg_observed_metadata(source)
  contract <- geo_pg_resolve_contract(source, observed)
  counts <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT count(*)::text AS n_features, ",
      "count(", geometry, ")::text AS n_non_null, ",
      "count(*) FILTER (WHERE ", geometry, " IS NULL)::text AS n_null, ",
      "count(*) FILTER (WHERE ", geometry, " IS NOT NULL AND ST_IsEmpty(", geometry, "))::text AS n_empty, ",
      "count(*) FILTER (WHERE ", geometry, " IS NOT NULL AND NOT ST_IsEmpty(", geometry, ") AND ST_IsValid(", geometry, "))::text AS n_valid, ",
      "count(*) FILTER (WHERE ", geometry, " IS NOT NULL AND NOT ST_IsEmpty(", geometry, ") AND NOT ST_IsValid(", geometry, "))::text AS n_invalid ",
      "FROM ", table
    ),
    query_kind = "geo_postgis_validity_counts",
    limit = 1L
  )
  count_names <- names(counts)
  counts <- vapply(counts, epi_geo_postgis_count, numeric(1), field = "PostGIS aggregate count")
  names(counts) <- count_names
  extent <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT ST_XMin(extent)::double precision AS xmin, ST_YMin(extent)::double precision AS ymin, ",
      "ST_XMax(extent)::double precision AS xmax, ST_YMax(extent)::double precision AS ymax ",
      "FROM (SELECT ST_Extent(", geometry, ") AS extent FROM ", table,
      " WHERE ", geometry, " IS NOT NULL AND NOT ST_IsEmpty(", geometry, ")) AS aggregate_extent"
    ),
    query_kind = "geo_postgis_extent",
    limit = 1L
  )
  bounds <- as.numeric(unlist(
    extent[1L, c("xmin", "ymin", "xmax", "ymax"), drop = FALSE],
    use.names = FALSE
  ))
  names(bounds) <- c("xmin", "ymin", "xmax", "ymax")
  bounding_box <- sf::st_bbox(bounds, crs = sf::st_crs(contract$srid))
  type_total <- sum(observed$types$n)
  geometry_types <- data.frame(
    geometry_type = observed$types$geometry_type,
    count = observed$types$n,
    proportion = if (type_total == 0) numeric(nrow(observed$types)) else observed$types$n / type_total,
    stringsAsFactors = FALSE
  )
  messages <- "Bounding boxes and rare spatial aggregates may disclose sensitive locations and require disclosure review."
  if (counts[["n_features"]] == 0) messages <- c(messages, "The relation contains no features.")
  if (counts[["n_null"]] > 0) messages <- c(messages, "The relation contains null geometry.")
  if (counts[["n_empty"]] > 0) messages <- c(messages, "The relation contains empty geometry.")
  if (counts[["n_invalid"]] > 0) messages <- c(messages, "The relation contains invalid geometry.")
  structure(
    list(
      source = data.frame(
        relation_kind = source$relation_kind,
        storage = source$storage,
        geometry_type = contract$type,
        dimension = contract$dimension,
        srid = contract$srid,
        bounding_box_classification = "sensitive_location_aggregate",
        stringsAsFactors = FALSE
      ),
      dataset = data.frame(
        features = counts[["n_features"]],
        non_null_geometry = counts[["n_non_null"]],
        null_geometry = counts[["n_null"]],
        empty_geometry = counts[["n_empty"]],
        stringsAsFactors = FALSE
      ),
      geometry_types = geometry_types,
      srids = data.frame(srid = observed$srids$srid, count = observed$srids$n),
      dimensions = data.frame(dimension = observed$dimensions$dimension, count = observed$dimensions$n),
      validity = data.frame(
        valid = counts[["n_valid"]],
        invalid = counts[["n_invalid"]],
        empty = counts[["n_empty"]],
        null = counts[["n_null"]],
        stringsAsFactors = FALSE
      ),
      bounding_box = bounding_box,
      messages = messages
    ),
    class = c("epi_geo_postgis_description", "list")
  )
}

#' Describe aggregate PostGIS geometry structure
#'
#' Return catalogue and aggregate geometry QA from one read-only
#' repeatable-read snapshot without collecting feature geometry or ordinary
#' attributes.
#'
#' @param source An unmodified [epi_geo_postgis_source()] object.
#'
#' @return An `epi_geo_postgis_description` list with fixed components
#'   `source`, `dataset`, `geometry_types`, `srids`, `dimensions`, `validity`,
#'   `bounding_box` and `messages`.
#'
#' @details The returned bounding box and rare aggregate combinations may
#'   reveal sensitive locations. They require disclosure review before being
#'   printed, logged or shared. The result contains no WKT, WKB, feature
#'   attributes, connection details, executable SQL or native database notices.
#'
#' @export
epi_geo_postgis_describe <- function(source) {
  epi_geo_require("sf")
  if (!inherits(source, "epi_geo_postgis_source")) {
    stop("source must be an epi_geo_postgis_source.", call. = FALSE)
  }
  epi_geo_postgis_transaction(source, geo_pg_description_inside(source))
}

epi_geo_postgis_columns <- function(source, columns) {
  if (!is.character(columns) || anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("columns must be a unique character allow-list of exact non-empty identifiers.", call. = FALSE)
  }
  if (any(vapply(columns, function(column) {
    tryCatch(
      {
        eda_postgres_identifier(column, "columns")
        FALSE
      },
      error = function(error) TRUE
    )
  }, logical(1)))) {
    stop("columns must contain only exact undotted identifiers without SQL fragments.", call. = FALSE)
  }
  spatial_names <- source$columns$name[source$columns$base_udt_name %in% c("geometry", "geography")]
  if (any(columns %in% spatial_names)) {
    stop("columns must not include a geometry or geography column.", call. = FALSE)
  }
  if (!all(columns %in% source$columns$name)) {
    stop("columns contains an identifier absent from the reviewed source catalogue.", call. = FALSE)
  }
  columns
}

epi_geo_postgis_max_features <- function(max_features) {
  valid <- is.numeric(max_features) && length(max_features) == 1L &&
    !is.na(max_features) && is.finite(max_features) && max_features == floor(max_features) &&
    max_features >= 1 && max_features < .Machine$integer.max
  if (!valid) {
    stop("max_features must be one positive whole number below the R integer limit.", call. = FALSE)
  }
  as.integer(max_features)
}

epi_geo_postgis_bbox <- function(bbox, srid) {
  if (is.null(bbox)) {
    return(NULL)
  }
  if (!inherits(bbox, "bbox") || !identical(names(bbox), c("xmin", "ymin", "xmax", "ymax"))) {
    stop("bbox must be NULL or an sf bbox with xmin, ymin, xmax and ymax.", call. = FALSE)
  }
  values <- as.numeric(bbox)
  if (any(!is.finite(values)) || values[[1L]] > values[[3L]] || values[[2L]] > values[[4L]]) {
    stop("bbox must contain finite ordered bounds.", call. = FALSE)
  }
  crs <- sf::st_crs(bbox)
  if (epi_geo_crs_is_missing(crs) || is.na(crs$epsg) || crs$epsg != srid) {
    stop("bbox must use the resolved source CRS.", call. = FALSE)
  }
  values
}

epi_geo_postgis_where <- function(source, bbox, srid) {
  if (is.null(bbox)) {
    return(list(sql = "", params = list()))
  }
  geometry <- epi_geo_postgis_geometry_sql(source)
  list(
    sql = paste0(
      " WHERE ST_Intersects(", geometry,
      ", ST_MakeEnvelope($1::double precision, $2::double precision, ",
      "$3::double precision, $4::double precision, $5::integer))"
    ),
    params = c(as.list(as.numeric(bbox)), list(as.integer(srid)))
  )
}

epi_geo_postgis_wkb_sfc <- function(value, srid, geometry_type) {
  wkb <- unclass(value)
  if (!is.list(wkb)) wkb <- as.list(wkb)
  missing <- vapply(wkb, function(item) is.null(item) || (length(item) == 1L && is.na(item)), logical(1))
  if (any(missing)) {
    stop("The reviewed selection contains null geometry, which cannot satisfy the Phase-A sf contract.", call. = FALSE)
  }
  if (length(wkb) == 0L) {
    empty <- sf::st_sfc(crs = sf::st_crs(srid))
    return(sf::st_cast(empty, geometry_type))
  }
  sf::st_as_sfc(
    structure(wkb, class = c("WKB", "list")),
    EWKB = TRUE,
    crs = sf::st_crs(srid)
  )
}

epi_geo_postgis_collect_inside <- function(source, columns, bbox, max_features) {
  observed <- geo_pg_observed_metadata(source)
  contract <- geo_pg_resolve_contract(source, observed)
  bbox <- epi_geo_postgis_bbox(bbox, contract$srid)
  where <- epi_geo_postgis_where(source, bbox, contract$srid)
  table <- epi_geo_postgis_table_sql(source)
  count <- eda_db_fetch(
    source$con,
    paste0("SELECT count(*)::text AS n FROM ", table, where$sql),
    params = where$params,
    query_kind = "geo_postgis_selection_count",
    limit = 1L
  )
  n <- epi_geo_postgis_count(count$n[[1L]], "PostGIS selection count")
  if (n > max_features) {
    stop("The reviewed PostGIS selection exceeds max_features; narrow the selection or raise the explicit bound after review.", call. = FALSE)
  }
  alias <- ".episcout_geometry_wkb"
  while (alias %in% columns) alias <- paste0(".", alias)
  quoted_columns <- vapply(
    columns,
    function(column) as.character(DBI::dbQuoteIdentifier(source$con, column)),
    character(1)
  )
  geometry <- epi_geo_postgis_geometry_sql(source)
  select <- c(quoted_columns, paste0(
    "ST_AsEWKB(", geometry, ") AS ",
    as.character(DBI::dbQuoteIdentifier(source$con, alias))
  ))
  limit_index <- length(where$params) + 1L
  rows <- eda_db_fetch(
    source$con,
    paste0(
      "SELECT ", paste(select, collapse = ", "), " FROM ", table, where$sql,
      " LIMIT $", limit_index, "::bigint"
    ),
    params = c(where$params, list(as.integer(max_features + 1L))),
    query_kind = "geo_postgis_feature_collection",
    limit = max_features + 1L
  )
  if (nrow(rows) != n) {
    stop("The bounded PostGIS selection did not reconcile inside its snapshot.", call. = FALSE)
  }
  geometry_values <- rows[[alias]]
  rows[[alias]] <- NULL
  sfc <- epi_geo_postgis_wkb_sfc(
    geometry_values,
    contract$srid,
    contract$type
  )
  rows[[source$geometry_column]] <- sfc
  result <- sf::st_as_sf(rows, sf_column_name = source$geometry_column)
  epi_geo_validate_sf(result)
  result
}

#' Collect an explicitly bounded PostGIS selection
#'
#' Materialise only a reviewed allow-list of ordinary columns plus geometry,
#' and only when the complete selection fits within a positive feature bound.
#'
#' @param source An unmodified [epi_geo_postgis_source()] object.
#' @param columns A unique character allow-list of exact non-geometry column
#'   identifiers. Defaults to geometry only.
#' @param bbox `NULL` for the complete relation, or an [sf::st_bbox()] object
#'   with finite ordered bounds in the resolved source CRS.
#' @param max_features One positive whole-number upper bound. The function
#'   refuses an over-limit selection and never truncates it.
#'
#' @return An `sf` object satisfying the Phase-A XY geometry and CRS contract.
#'
#' @details This is the only PostGIS path that collects feature geometry or
#'   attributes. Identifiers are quoted, bbox ordinates are bound as query
#'   values, and counting and collection occur in one read-only repeatable-read
#'   snapshot. Row order is retained as returned by PostgreSQL but is not a
#'   stable ordering contract. The caller owns and must close the connection.
#'
#' @export
epi_geo_postgis_collect <- function(source,
                                    columns = character(),
                                    bbox = NULL,
                                    max_features = 10000L) {
  epi_geo_require("sf")
  if (!inherits(source, "epi_geo_postgis_source")) {
    stop("source must be an epi_geo_postgis_source.", call. = FALSE)
  }
  columns <- epi_geo_postgis_columns(source, columns)
  max_features <- epi_geo_postgis_max_features(max_features)
  epi_geo_postgis_transaction(
    source,
    epi_geo_postgis_collect_inside(source, columns, bbox, max_features)
  )
}
