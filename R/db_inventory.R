#' Inventory PostgreSQL tables and columns
#'
#' Read table, column and constraint metadata from a PostgreSQL schema without reading row values. Optional row counts are disabled by default because exact counts may be expensive.
#'
#' @param con An open DBI connection created with RPostgres.
#' @param schema A single non-empty schema name.
#' @param tables Optional character vector of table names to include. `NULL` includes every table and view visible in `schema`.
#' @param row_counts One of `"none"`, `"estimate"` or `"exact"`.
#'
#' @return An `epi_db_inventory` list with `tables`, `columns` and `constraints` data frames.
#'
#' @details Inventory reads database metadata only unless `row_counts = "exact"`, which executes aggregate row counts. It never reads identifier values. For restricted longitudinal sources, use the inventory to create and review an [epi_eda_dictionary_scaffold()], then follow `vignette("longitudinal-pseudonymisation")`; do not export identifiable rows merely to prepare linkage metadata.
#'
#' @seealso [epi_eda_dictionary_scaffold()], [epi_sec_linkage_scaffold()], [epi_sec_pseudonymise_db()]
#' @export
epi_db_inventory <- function(con,
                             schema,
                             tables = NULL,
                             row_counts = c("none", "estimate", "exact")) {
  validate_postgres_connection(con)
  schema <- validate_inventory_schema(schema)
  tables <- validate_inventory_tables(tables)
  row_counts <- match.arg(row_counts)

  table_inventory <- postgres_table_inventory(con, schema)
  column_inventory <- postgres_column_inventory(con, schema)
  constraint_inventory <- postgres_constraint_inventory(con, schema)

  if (!is.null(tables)) {
    missing_tables <- setdiff(tables, table_inventory$source_table)
    if (length(missing_tables) > 0) {
      stop(
        "Requested tables were not found in schema '", schema, "': ",
        paste(missing_tables, collapse = ", "),
        call. = FALSE
      )
    }
    table_inventory <- table_inventory[table_inventory$source_table %in% tables, , drop = FALSE]
    column_inventory <- column_inventory[column_inventory$source_table %in% tables, , drop = FALSE]
    constraint_inventory <- constraint_inventory[constraint_inventory$source_table %in% tables, , drop = FALSE]
  }

  table_inventory$row_count <- inventory_row_counts(
    con = con,
    tables = table_inventory,
    mode = row_counts
  )

  table_inventory <- order_inventory_rows(table_inventory, c("source_schema", "source_table"))
  column_inventory <- order_inventory_rows(
    column_inventory,
    c("source_schema", "source_table", "source_ordinal", "source_column")
  )
  constraint_inventory <- order_inventory_rows(
    constraint_inventory,
    c("source_schema", "source_table", "constraint_name")
  )

  structure(
    list(
      tables = table_inventory,
      columns = column_inventory,
      constraints = constraint_inventory
    ),
    class = c("epi_db_inventory", "list")
  )
}

validate_postgres_connection <- function(con) {
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    stop("con must be an open DBI connection.", call. = FALSE)
  }

  info <- tryCatch(DBI::dbGetInfo(con), error = function(error) list())
  dbms_name <- if (is.null(info$dbms.name)) "" else as.character(info$dbms.name)
  is_postgres <- inherits(con, "PqConnection") || grepl("postgres", dbms_name, ignore.case = TRUE)
  if (!is_postgres) {
    stop("epi_db_inventory currently supports PostgreSQL connections only.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_inventory_schema <- function(schema) {
  if (!is.character(schema) || length(schema) != 1 || is.na(schema) || trimws(schema) == "") {
    stop("schema must be a single non-empty character value.", call. = FALSE)
  }
  schema
}

validate_inventory_tables <- function(tables) {
  if (is.null(tables)) {
    return(NULL)
  }
  if (!is.character(tables) || length(tables) == 0 || anyNA(tables) || any(trimws(tables) == "")) {
    stop("tables must be NULL or a non-empty character vector without missing values.", call. = FALSE)
  }
  if (anyDuplicated(tables)) {
    stop("tables must not contain duplicates.", call. = FALSE)
  }
  tables
}

postgres_table_inventory <- function(con, schema) {
  query <- paste(
    "SELECT",
    "  t.table_schema AS source_schema,",
    "  t.table_name AS source_table,",
    "  t.table_type,",
    "  pg_catalog.obj_description(c.oid, 'pg_class') AS table_comment",
    "FROM information_schema.tables AS t",
    "LEFT JOIN pg_catalog.pg_namespace AS n",
    "  ON n.nspname = t.table_schema",
    "LEFT JOIN pg_catalog.pg_class AS c",
    "  ON c.relnamespace = n.oid",
    " AND c.relname = t.table_name",
    "WHERE t.table_schema = $1",
    "ORDER BY t.table_schema, t.table_name"
  )
  as.data.frame(DBI::dbGetQuery(con, query, params = list(schema)), stringsAsFactors = FALSE)
}

postgres_column_inventory <- function(con, schema) {
  query <- paste(
    "SELECT",
    "  col.table_schema AS source_schema,",
    "  col.table_name AS source_table,",
    "  col.ordinal_position AS source_ordinal,",
    "  col.column_name AS source_column,",
    "  col.data_type AS source_data_type,",
    "  col.udt_name AS source_udt_name,",
    "  col.is_nullable AS source_is_nullable,",
    "  col.character_maximum_length AS source_character_maximum_length,",
    "  col.numeric_precision AS source_numeric_precision,",
    "  col.numeric_scale AS source_numeric_scale,",
    "  d.description AS source_column_comment",
    "FROM information_schema.columns AS col",
    "LEFT JOIN pg_catalog.pg_namespace AS n",
    "  ON n.nspname = col.table_schema",
    "LEFT JOIN pg_catalog.pg_class AS c",
    "  ON c.relnamespace = n.oid",
    " AND c.relname = col.table_name",
    "LEFT JOIN pg_catalog.pg_attribute AS a",
    "  ON a.attrelid = c.oid",
    " AND a.attname = col.column_name",
    "LEFT JOIN pg_catalog.pg_description AS d",
    "  ON d.objoid = c.oid",
    " AND d.objsubid = a.attnum",
    "WHERE col.table_schema = $1",
    "ORDER BY col.table_schema, col.table_name, col.ordinal_position"
  )
  as.data.frame(DBI::dbGetQuery(con, query, params = list(schema)), stringsAsFactors = FALSE)
}

postgres_constraint_inventory <- function(con, schema) {
  query <- paste(
    "SELECT",
    "  tc.table_schema AS source_schema,",
    "  tc.table_name AS source_table,",
    "  tc.constraint_name,",
    "  tc.constraint_type,",
    "  COALESCE(string_agg(kcu.column_name, ';' ORDER BY kcu.ordinal_position), '') AS source_columns",
    "FROM information_schema.table_constraints AS tc",
    "LEFT JOIN information_schema.key_column_usage AS kcu",
    "  ON kcu.constraint_catalog = tc.constraint_catalog",
    " AND kcu.constraint_schema = tc.constraint_schema",
    " AND kcu.constraint_name = tc.constraint_name",
    "WHERE tc.table_schema = $1",
    "GROUP BY tc.table_schema, tc.table_name, tc.constraint_name, tc.constraint_type",
    "ORDER BY tc.table_schema, tc.table_name, tc.constraint_name"
  )
  as.data.frame(DBI::dbGetQuery(con, query, params = list(schema)), stringsAsFactors = FALSE)
}

inventory_row_counts <- function(con, tables, mode) {
  if (nrow(tables) == 0) {
    return(numeric())
  }
  if (mode == "none") {
    return(rep(NA_real_, nrow(tables)))
  }
  if (mode == "estimate") {
    query <- paste(
      "SELECT c.reltuples::double precision AS row_count",
      "FROM pg_catalog.pg_class AS c",
      "JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2"
    )
    return(vapply(seq_len(nrow(tables)), function(index) {
      result <- DBI::dbGetQuery(
        con,
        query,
        params = list(tables$source_schema[[index]], tables$source_table[[index]])
      )
      if (nrow(result) == 0) NA_real_ else as.numeric(result$row_count[[1]])
    }, numeric(1)))
  }

  vapply(seq_len(nrow(tables)), function(index) {
    identifier <- DBI::Id(
      schema = tables$source_schema[[index]],
      table = tables$source_table[[index]]
    )
    quoted <- as.character(DBI::dbQuoteIdentifier(con, identifier))
    result <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS row_count FROM", quoted))
    as.numeric(result$row_count[[1]])
  }, numeric(1))
}

order_inventory_rows <- function(data, columns) {
  if (nrow(data) == 0) {
    row.names(data) <- NULL
    return(data)
  }
  ordering <- do.call(order, c(data[columns], list(na.last = TRUE)))
  data <- data[ordering, , drop = FALSE]
  row.names(data) <- NULL
  data
}
