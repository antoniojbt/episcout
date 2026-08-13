#' Reference a PostgreSQL relation for specification-first EDA
#'
#' Validate a caller-owned RPostgres connection and one schema-qualified
#' relation without collecting observations. The returned object can be passed
#' to the five `epi_eda_*` profiling functions and [epi_eda_db_run()].
#'
#' @param con An open, idle connection created by RPostgres.
#' @param schema One PostgreSQL schema name. Dotted names and SQL fragments are
#'   not accepted.
#' @param relation One table or view name in `schema`.
#'
#' @return An `epi_eda_postgres_source` list with fixed components `con`,
#'   `schema`, `relation`, `relation_kind`, `columns`, and `source_version`.
#'   Printing and structure display redact the connection and relation names.
#'
#' @details PostgreSQL 17 or later is required. Ordinary tables, partitioned
#' tables, views, materialized views, and foreign tables are supported.
#' Temporary and other relation kinds are rejected. The connection remains
#' caller-owned and must remain open and idle while the source is used.
#'
#' @export
epi_eda_postgres_source <- function(con, schema, relation) {
  eda_pg_validate_connection(con, require_idle = TRUE)
  schema <- eda_postgres_identifier(schema, "schema")
  relation <- eda_postgres_identifier(relation, "relation")
  catalogue <- eda_postgres_catalogue(con, schema, relation)
  columns <- catalogue$columns
  catalogue_fingerprint <- eda_postgres_fingerprint(columns)
  attr(columns, "relation_oid") <- catalogue$relation_oid
  attr(columns, "catalogue_fingerprint") <- catalogue_fingerprint

  structure(
    list(
      con = con,
      schema = schema,
      relation = relation,
      relation_kind = catalogue$relation_kind,
      columns = columns,
      source_version = "postgres-source-1"
    ),
    class = c("epi_eda_postgres_source", "list")
  )
}

#' @export
print.epi_eda_postgres_source <- function(x, ...) {
  cat("<episcout PostgreSQL EDA source>\n")
  cat("  relation kind: ", x$relation_kind, "\n", sep = "")
  cat("  columns: ", nrow(x$columns), "\n", sep = "")
  invisible(x)
}

#' @export
str.epi_eda_postgres_source <- function(object, ...) {
  cat("<episcout PostgreSQL EDA source>\n")
  cat("  relation kind: ", object$relation_kind, "\n", sep = "")
  cat("  columns: ", nrow(object$columns), "\n", sep = "")
  cat("  connection and relation identity: <redacted>\n")
  invisible(object)
}

eda_pg_validate_connection <- function(con, require_idle = TRUE) {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("The RPostgres package is required for PostgreSQL-backed EDA.", call. = FALSE)
  }
  if (!inherits(con, "PqConnection") || !inherits(con, "DBIConnection") ||
        !DBI::dbIsValid(con)) {
    stop("con must be an open RPostgres connection.", call. = FALSE)
  }
  if (isTRUE(require_idle) && eda_pg_is_transacting(con)) {
    stop("PostgreSQL-backed EDA requires a connection that is not already inside a caller-managed transaction.", call. = FALSE)
  }
  server_version <- eda_db_fetch(
    con,
    "SELECT current_setting('server_version_num') AS server_version_num",
    query_kind = "server_version",
    limit = 1L
  )$server_version_num[[1]]
  if (is.na(server_version) || suppressWarnings(as.integer(server_version)) < 170000L) {
    stop("PostgreSQL-backed EDA requires PostgreSQL 17 or later.", call. = FALSE)
  }
  invisible(TRUE)
}

eda_pg_is_transacting <- function(con) {
  checker <- tryCatch(
    utils::getFromNamespace("connection_is_transacting", "RPostgres"),
    error = function(error) NULL
  )
  if (is.null(checker)) {
    stop("The installed RPostgres version cannot verify transaction state safely.", call. = FALSE)
  }
  isTRUE(checker(con@ptr))
}

eda_postgres_identifier <- function(value, name) {
  if (inherits(value, "SQL") || !is.character(value) || length(value) != 1L ||
        is.na(value) || !nzchar(trimws(value))) {
    stop(name, " must be one non-empty plain character identifier.", call. = FALSE)
  }
  if (grepl("[.;]|--|/\\*|\\*/", value) || grepl("[\r\n\t]", value)) {
    stop(name, " must be one undotted identifier and must not contain SQL fragments.", call. = FALSE)
  }
  value
}

eda_postgres_catalogue <- function(con, schema, relation, timing_env = NULL) {
  relation_row <- eda_db_fetch(
    con,
    paste(
      "SELECT c.oid::text AS relation_oid, c.relkind, c.relpersistence, c.relispartition,",
      "current_setting('server_version_num') AS server_version_num",
      "FROM pg_catalog.pg_class AS c",
      "INNER JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2"
    ),
    params = list(schema, relation),
    query_kind = "catalogue_relation",
    limit = 1L,
    timing_env = timing_env
  )
  if (nrow(relation_row) != 1L) {
    stop("The requested PostgreSQL relation was not found or was not uniquely resolved.", call. = FALSE)
  }
  kinds <- c(r = "table", p = "partitioned table", v = "view", m = "materialized view", f = "foreign table")
  relkind <- as.character(relation_row$relkind[[1]])
  if (!(relkind %in% names(kinds)) || !identical(as.character(relation_row$relpersistence[[1]]), "p")) {
    stop("The requested PostgreSQL object is temporary or has an unsupported relation kind.", call. = FALSE)
  }

  column_count <- eda_db_fetch(
    con,
    paste(
      "SELECT count(*)::text AS n",
      "FROM pg_catalog.pg_attribute AS a",
      "WHERE a.attrelid = $1::oid AND a.attnum > 0 AND NOT a.attisdropped"
    ),
    params = list(as.character(relation_row$relation_oid[[1]])),
    query_kind = "catalogue_column_count",
    limit = 1L,
    timing_env = timing_env
  )
  n_columns <- eda_checked_count(column_count$n[[1]], "PostgreSQL column count")
  columns <- eda_db_fetch(
    con,
    paste(
      "SELECT a.attname AS name, a.attnum::integer AS ordinal_position,",
      "t.typname AS udt_name, bt.typname AS base_udt_name, t.typtype,",
      "pg_catalog.format_type(a.atttypid, a.atttypmod) AS formatted_type,",
      "CASE WHEN a.attcollation = 0 THEN NULL ELSE cn.nspname END AS collation_schema,",
      "CASE WHEN a.attcollation = 0 THEN NULL ELSE coll.collname END AS collation_name,",
      "CASE WHEN a.attcollation = 0 THEN TRUE ELSE coll.collisdeterministic END AS collation_deterministic",
      "FROM pg_catalog.pg_attribute AS a",
      "INNER JOIN pg_catalog.pg_type AS t ON t.oid = a.atttypid",
      "INNER JOIN pg_catalog.pg_type AS bt ON bt.oid = CASE WHEN t.typtype = 'd' THEN t.typbasetype ELSE t.oid END",
      "LEFT JOIN pg_catalog.pg_collation AS coll ON coll.oid = a.attcollation",
      "LEFT JOIN pg_catalog.pg_namespace AS cn ON cn.oid = coll.collnamespace",
      "WHERE a.attrelid = $1::oid AND a.attnum > 0 AND NOT a.attisdropped",
      "ORDER BY a.attnum"
    ),
    params = list(as.character(relation_row$relation_oid[[1]])),
    query_kind = "catalogue_columns",
    limit = n_columns,
    timing_env = timing_env
  )
  if (nrow(columns) != n_columns) {
    stop("PostgreSQL catalogue column counts changed during validation.", call. = FALSE)
  }
  list(
    relation_oid = as.character(relation_row$relation_oid[[1]]),
    relation_kind = unname(kinds[[relkind]]),
    server_version_num = as.character(relation_row$server_version_num[[1]]),
    columns = as.data.frame(columns, stringsAsFactors = FALSE)
  )
}

eda_validate_postgres_source <- function(source, require_idle = TRUE, timing_env = NULL) {
  expected_names <- c("con", "schema", "relation", "relation_kind", "columns", "source_version")
  valid_shape <- inherits(source, "epi_eda_postgres_source") &&
    identical(class(source), c("epi_eda_postgres_source", "list")) &&
    identical(names(source), expected_names) &&
    identical(source$source_version, "postgres-source-1") &&
    is.data.frame(source$columns) &&
    !is.null(attr(source$columns, "relation_oid")) &&
    !is.null(attr(source$columns, "catalogue_fingerprint"))
  if (!valid_shape) {
    stop("source must be an unmodified object returned by epi_eda_postgres_source().", call. = FALSE)
  }
  eda_pg_validate_connection(source$con, require_idle = require_idle)
  schema <- eda_postgres_identifier(source$schema, "source schema")
  relation <- eda_postgres_identifier(source$relation, "source relation")
  current <- eda_postgres_catalogue(source$con, schema, relation, timing_env = timing_env)
  same <- identical(current$relation_oid, attr(source$columns, "relation_oid")) &&
    identical(current$relation_kind, source$relation_kind) &&
    identical(eda_postgres_fingerprint(current$columns), attr(source$columns, "catalogue_fingerprint"))
  if (!same) {
    stop("The PostgreSQL source catalogue changed after source construction; construct a new source after review.", call. = FALSE)
  }
  invisible(current)
}

eda_postgres_fingerprint <- function(value) {
  raw <- serialize(value, connection = NULL, ascii = TRUE, version = 2L)
  as.character(openssl::sha256(raw))
}

eda_db_begin <- function(con) {
  DBI::dbBegin(con)
}

eda_db_commit <- function(con) {
  DBI::dbCommit(con)
}

eda_db_lifecycle_call <- function(action, failure_message) {
  observed <- eda_db_observe_conditions(force(action))
  eda_db_signal_conditions(observed, "transaction lifecycle")
  if (inherits(observed$value, "error")) {
    stop(failure_message, call. = FALSE)
  }
  invisible(TRUE)
}

eda_postgres_transaction <- function(source, code, timing_env = NULL) {
  eda_validate_postgres_source(source, require_idle = TRUE, timing_env = timing_env)
  con <- source$con
  eda_db_lifecycle_call(
    eda_db_begin(con),
    "PostgreSQL EDA transaction could not begin; review restricted database logs."
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
    query_kind = "transaction_setup",
    timing_env = timing_env
  )
  eda_validate_postgres_source(source, require_idle = FALSE, timing_env = timing_env)
  value <- force(code)
  eda_db_lifecycle_call(
    eda_db_commit(con),
    "PostgreSQL EDA transaction could not commit safely; review restricted database logs."
  )
  finished <- TRUE
  value
}
