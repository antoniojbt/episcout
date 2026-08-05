#' Initialise a PostgreSQL identity registry
#'
#' Audit or create the restricted tables used to assign stable pseudonym tokens across related PostgreSQL tables. The registry schema is one deliberately authorised linkage domain and must already exist.
#'
#' @param con An open PostgreSQL DBI connection created with RPostgres.
#' @param registry_schema A single existing PostgreSQL schema reserved for the identity registry.
#' @param token_prefix A non-empty prefix for generated entity tokens.
#' @param n_bytes Number of cryptographically random bytes per token; at least 16.
#' @param mode Either `"audit"` to inspect without writing or `"apply"` to create the registry tables transactionally.
#'
#' @return A redacted `epi_sec_registry_result` with scalar `status`, `mode`, `writes`, `registry_schema`, `schema_restricted` and `next_action`; `metadata` columns `registry_id`, `registry_version`, `token_prefix`, `n_bytes`, `created_at`; and `objects` columns `object`, `status`. Status is `blocked` when access or existing objects are incompatible, `initialisation_required` when audit finds a suitably restricted empty schema and `ready` when a compatible registry exists or has been created.
#'
#' @details Audit mode is the default and never writes. Before apply, the database administrator must revoke `PUBLIC` creation and usage on the registry schema. Apply creates `registry_metadata`, `namespaces`, `entities`, `aliases`, `runs` and `run_tables` in one transaction and defensively revokes `PUBLIC` access to every created table and the schema. The database administrator remains responsible for creating the schema, granting named roles, testing recovery and controlling database storage, backups and logging. Audit reports incompatible existing objects as `blocked`; apply treats them as an unsafe-state error. Repair or replace an incomplete registry under an administrator-approved recovery procedure rather than editing registry rows manually.
#'
#' The registry alias table contains source identifiers in plaintext and remains re-identifying. Pseudonymised data remain restricted personal data: they are not anonymous or automatically disclosure-controlled. See `vignette("longitudinal-pseudonymisation")` before initialising a registry.
#'
#' @export
#' @seealso [epi_sec_linkage_scaffold()], [epi_sec_linkage_spec()], [epi_sec_pseudonymise_db()], [epi_db_inventory()]
#' @family longitudinal pseudonymisation
epi_sec_identity_registry_init <- function(con,
                                           registry_schema,
                                           token_prefix = "E",
                                           n_bytes = 24,
                                           mode = c("audit", "apply")) {
  sec_database_boundary(
    {
      validate_postgres_connection(con)
      registry_schema <- sec_scalar_text(registry_schema, "registry_schema")
      token_prefix <- sec_scalar_text(token_prefix, "token_prefix")
      n_bytes <- sec_whole_number(n_bytes, "n_bytes", minimum = 16L)
      mode <- match.arg(mode)
      if (mode == "apply" && sec_connection_is_transacting(con)) {
        stop("mode = 'apply' requires a connection that is not already inside a caller-managed transaction.", call. = FALSE)
      }
      sec_require_schema(con, registry_schema, "registry_schema")

      schema_restricted <- !sec_schema_is_public(con, registry_schema)
      observed <- sec_registry_inspect(con, registry_schema)
      if (!schema_restricted) {
        if (mode == "apply") {
          stop(
            "registry_schema grants CREATE or USAGE to PUBLIC; ask the database administrator to revoke those privileges before applying.",
            call. = FALSE
          )
        }
        return(sec_registry_result(
          status = "blocked",
          mode = mode,
          writes = FALSE,
          registry_schema = registry_schema,
          schema_restricted = FALSE,
          metadata = observed$metadata,
          objects = observed$objects,
          next_action = "Ask the database administrator to revoke PUBLIC CREATE and USAGE on the registry schema, then audit again."
        ))
      }
      if (observed$state == "compatible") {
        sec_registry_assert_settings(observed$metadata, token_prefix, n_bytes)
        return(sec_registry_result(
          status = "ready",
          mode = mode,
          writes = FALSE,
          registry_schema = registry_schema,
          schema_restricted = TRUE,
          metadata = observed$metadata,
          objects = observed$objects,
          next_action = "Run epi_sec_pseudonymise_db() in audit mode."
        ))
      }
      if (observed$state == "incompatible") {
        if (mode == "audit") {
          return(sec_registry_result(
            status = "blocked",
            mode = mode,
            writes = FALSE,
            registry_schema = registry_schema,
            schema_restricted = TRUE,
            metadata = observed$metadata,
            objects = observed$objects,
            next_action = "Ask the database administrator to inspect the incompatible registry objects; do not edit registry rows manually."
          ))
        }
        stop(
          "registry_schema contains an incomplete or incompatible episcout registry; no objects were changed.",
          call. = FALSE
        )
      }
      if (mode == "audit") {
        return(sec_registry_result(
          status = "initialisation_required",
          mode = mode,
          writes = FALSE,
          registry_schema = registry_schema,
          schema_restricted = TRUE,
          metadata = sec_empty_registry_metadata(),
          objects = sec_registry_object_frame("planned"),
          next_action = "Review database access, then rerun with mode = 'apply'."
        ))
      }

      DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ")
        observed_inside <- sec_registry_inspect(con, registry_schema)
        if (observed_inside$state != "empty") {
          stop("registry_schema changed during initialisation; the transaction was rolled back.", call. = FALSE)
        }
        sec_registry_create(con, registry_schema, token_prefix, n_bytes)
      })

      created <- sec_registry_inspect(con, registry_schema)
      sec_registry_assert_settings(created$metadata, token_prefix, n_bytes)
      sec_registry_result(
        status = "ready",
        mode = mode,
        writes = TRUE,
        registry_schema = registry_schema,
        schema_restricted = TRUE,
        metadata = created$metadata,
        objects = created$objects,
        next_action = "Create and confirm a linkage specification, then run an audit."
      )
    },
    "Identity registry inspection or initialisation could not complete safely; ask the database administrator to inspect restricted database logs."
  )
}

#' @export
print.epi_sec_registry_result <- function(x, ...) {
  cat("episcout identity registry\n")
  cat("  status: ", x$status, "\n", sep = "")
  cat("  schema: ", x$registry_schema, "\n", sep = "")
  cat("  schema restricted from PUBLIC: ", if (isTRUE(x$schema_restricted)) "yes" else "no", "\n", sep = "")
  cat("  writes performed: ", if (isTRUE(x$writes)) "yes" else "no", "\n", sep = "")
  cat("  next: ", x$next_action, "\n", sep = "")
  invisible(x)
}

sec_registry_tables <- function() {
  c("registry_metadata", "namespaces", "entities", "aliases", "runs", "run_tables")
}

sec_registry_version <- function() 1L

sec_registry_inspect <- function(con, schema) {
  relations <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.relname AS table_name, c.relkind, pg_get_userbyid(c.relowner) = current_user AS owned,",
      "has_table_privilege('public', c.oid, 'SELECT') OR",
      "has_table_privilege('public', c.oid, 'INSERT') OR",
      "has_table_privilege('public', c.oid, 'UPDATE') OR",
      "has_table_privilege('public', c.oid, 'DELETE') OR",
      "has_table_privilege('public', c.oid, 'TRUNCATE') OR",
      "has_table_privilege('public', c.oid, 'REFERENCES') OR",
      "has_table_privilege('public', c.oid, 'TRIGGER') AS public_access",
      "FROM pg_class c INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 ORDER BY c.relname"
    ),
    params = list(schema)
  )
  found <- intersect(as.character(relations$table_name), sec_registry_tables())
  expected_relations <- relations[match(sec_registry_tables(), relations$table_name), , drop = FALSE]
  present <- !is.na(expected_relations$table_name)
  ordinary <- present & expected_relations$relkind == "r"
  owned <- present & expected_relations$owned
  restricted <- present & !expected_relations$public_access
  objects <- data.frame(
    object = sec_registry_tables(),
    status = ifelse(!present, "absent", ifelse(!ordinary, "wrong_kind", ifelse(!owned, "foreign_owner", ifelse(!restricted, "public_access", "present")))),
    stringsAsFactors = FALSE
  )
  if (length(found) == 0L) {
    return(list(state = "empty", metadata = sec_empty_registry_metadata(), objects = objects))
  }
  if (!setequal(found, sec_registry_tables()) || !all(ordinary) || !all(owned) || !all(restricted)) {
    return(list(state = "incompatible", metadata = sec_empty_registry_metadata(), objects = objects))
  }
  if (!sec_registry_structure_ok(con, schema)) {
    objects$status <- "incompatible_structure"
    return(list(state = "incompatible", metadata = sec_empty_registry_metadata(), objects = objects))
  }
  metadata <- tryCatch(
    DBI::dbGetQuery(con, paste("SELECT registry_id, registry_version, token_prefix, n_bytes, created_at FROM", sec_quote_table(con, schema, "registry_metadata"))),
    error = function(error) NULL
  )
  if (is.null(metadata) || nrow(metadata) != 1L || !identical(as.integer(metadata$registry_version[[1]]), sec_registry_version())) {
    return(list(state = "incompatible", metadata = sec_empty_registry_metadata(), objects = objects))
  }
  list(state = "compatible", metadata = metadata, objects = objects)
}

sec_registry_structure_ok <- function(con, schema) {
  expected_columns <- list(
    registry_metadata = c("registry_id:text:NO", "registry_version:int4:NO", "token_prefix:text:NO", "n_bytes:int4:NO", "created_at:timestamptz:NO"),
    namespaces = c("identity_namespace:text:NO", "type_family:text:NO", "created_at:timestamptz:NO"),
    entities = c("entity_token:text:NO", "created_at:timestamptz:NO"),
    aliases = c("identity_namespace:text:NO", "source_id:text:NO", "entity_token:text:NO", "created_at:timestamptz:NO"),
    runs = c("run_id:text:NO", "completed_at:timestamptz:NO", "configuration_hash:text:NO", "exact_duplicates:text:NO", "status:text:NO"),
    run_tables = c("run_id:text:NO", "source_schema:text:NO", "source_table:text:NO", "output_schema:text:NO", "output_table:text:NO", "n_input:int8:NO", "n_output:int8:NO", "n_exact_removed:int8:NO")
  )
  columns <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT table_name, column_name, udt_name, is_nullable, collation_name, column_default",
      "FROM information_schema.columns WHERE table_schema = $1",
      "ORDER BY table_name, ordinal_position"
    ),
    params = list(schema)
  )
  for (relation_name in names(expected_columns)) {
    observed <- columns[columns$table_name == relation_name, , drop = FALSE]
    signature <- paste(observed$column_name, observed$udt_name, observed$is_nullable, sep = ":")
    if (!identical(signature, expected_columns[[relation_name]])) {
      return(FALSE)
    }
  }
  exact_text <- (columns$table_name == "namespaces" & columns$column_name == "identity_namespace") |
    (columns$table_name == "aliases" & columns$column_name %in% c("identity_namespace", "source_id"))
  if (any(columns$collation_name[exact_text] != "C")) {
    return(FALSE)
  }

  constraints <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.relname AS table_name, k.contype, pg_get_constraintdef(k.oid) AS definition",
      "FROM pg_constraint k INNER JOIN pg_class c ON c.oid = k.conrelid",
      "INNER JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = $1"
    ),
    params = list(schema)
  )
  expected_constraints <- list(
    registry_metadata = c(
      "^PRIMARY KEY \\(registry_id\\)$",
      "^CHECK \\(\\(n_bytes >= 16\\)\\)$"
    ),
    namespaces = c(
      "^PRIMARY KEY \\(identity_namespace\\)$",
      "^CHECK \\(\\(type_family = ANY \\(ARRAY\\['text'::text, 'integer'::text, 'uuid'::text\\]\\)\\)\\)$"
    ),
    entities = "^PRIMARY KEY \\(entity_token\\)$",
    aliases = c(
      "^PRIMARY KEY \\(identity_namespace, source_id\\)$",
      "^FOREIGN KEY \\(identity_namespace\\) REFERENCES .+\\.namespaces\\(identity_namespace\\)$",
      "^FOREIGN KEY \\(entity_token\\) REFERENCES .+\\.entities\\(entity_token\\)$"
    ),
    runs = c(
      "^PRIMARY KEY \\(run_id\\)$",
      "^CHECK \\(\\(status = 'complete'::text\\)\\)$"
    ),
    run_tables = c(
      "^PRIMARY KEY \\(run_id, source_schema, source_table\\)$",
      "^FOREIGN KEY \\(run_id\\) REFERENCES .+\\.runs\\(run_id\\)$"
    )
  )
  for (relation_name in names(expected_constraints)) {
    observed <- constraints$definition[constraints$table_name == relation_name]
    expected <- expected_constraints[[relation_name]]
    if (length(observed) != length(expected)) {
      return(FALSE)
    }
    matched <- vapply(expected, function(pattern) sum(grepl(pattern, observed)) == 1L, logical(1))
    if (!all(matched)) {
      return(FALSE)
    }
  }

  defaults <- columns[!is.na(columns$column_default), c("table_name", "column_name", "column_default"), drop = FALSE]
  observed_defaults <- paste(defaults$table_name, defaults$column_name, defaults$column_default, sep = ":")
  expected_defaults <- c(
    "aliases:created_at:CURRENT_TIMESTAMP",
    "entities:created_at:CURRENT_TIMESTAMP",
    "namespaces:created_at:CURRENT_TIMESTAMP",
    "registry_metadata:created_at:CURRENT_TIMESTAMP",
    "runs:completed_at:CURRENT_TIMESTAMP"
  )
  if (!setequal(observed_defaults, expected_defaults)) {
    return(FALSE)
  }
  TRUE
}

sec_registry_create <- function(con, schema, token_prefix, n_bytes) {
  table_name <- function(name) sec_quote_table(con, schema, name)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("registry_metadata"), " (",
    "registry_id text PRIMARY KEY, registry_version integer NOT NULL, ",
    "token_prefix text NOT NULL, n_bytes integer NOT NULL CHECK (n_bytes >= 16), ",
    "created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("namespaces"), " (",
    "identity_namespace text COLLATE \"C\" PRIMARY KEY, type_family text NOT NULL ",
    "CHECK (type_family IN ('text', 'integer', 'uuid')), created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("entities"), " (",
    "entity_token text PRIMARY KEY, created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP)"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("aliases"), " (",
    "identity_namespace text COLLATE \"C\" NOT NULL REFERENCES ", table_name("namespaces"), " (identity_namespace), ",
    "source_id text COLLATE \"C\" NOT NULL, entity_token text NOT NULL REFERENCES ", table_name("entities"), " (entity_token), ",
    "created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY (identity_namespace, source_id))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("runs"), " (",
    "run_id text PRIMARY KEY, completed_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP, ",
    "configuration_hash text NOT NULL, exact_duplicates text NOT NULL, status text NOT NULL CHECK (status = 'complete'))"
  ))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", table_name("run_tables"), " (",
    "run_id text NOT NULL REFERENCES ", table_name("runs"), " (run_id), ",
    "source_schema text NOT NULL, source_table text NOT NULL, output_schema text NOT NULL, output_table text NOT NULL, ",
    "n_input bigint NOT NULL, n_output bigint NOT NULL, n_exact_removed bigint NOT NULL, ",
    "PRIMARY KEY (run_id, source_schema, source_table))"
  ))

  registry_id <- sec_generate_tokens(1L, 24L, "R")
  DBI::dbExecute(
    con,
    paste("INSERT INTO", table_name("registry_metadata"), "(registry_id, registry_version, token_prefix, n_bytes) VALUES ($1, $2, $3, $4)"),
    params = list(registry_id, sec_registry_version(), token_prefix, n_bytes)
  )
  quoted_tables <- paste(vapply(sec_registry_tables(), function(name) table_name(name), character(1)), collapse = ", ")
  DBI::dbExecute(con, paste("REVOKE ALL ON TABLE", quoted_tables, "FROM PUBLIC"))
  DBI::dbExecute(con, paste("REVOKE ALL ON SCHEMA", sec_quote_identifier(con, schema), "FROM PUBLIC"))
  invisible(TRUE)
}

sec_registry_assert_settings <- function(metadata, token_prefix, n_bytes) {
  if (!identical(as.character(metadata$token_prefix[[1]]), token_prefix) ||
        !identical(as.integer(metadata$n_bytes[[1]]), as.integer(n_bytes))) {
    stop("Requested token settings do not match the existing identity registry.", call. = FALSE)
  }
  invisible(TRUE)
}

sec_registry_result <- function(status, mode, writes, registry_schema, schema_restricted, metadata, objects, next_action) {
  structure(
    list(
      status = status,
      mode = mode,
      writes = writes,
      registry_schema = registry_schema,
      schema_restricted = schema_restricted,
      metadata = metadata,
      objects = objects,
      next_action = next_action
    ),
    class = c("epi_sec_registry_result", "list")
  )
}

sec_empty_registry_metadata <- function() {
  data.frame(
    registry_id = character(),
    registry_version = integer(),
    token_prefix = character(),
    n_bytes = integer(),
    created_at = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

sec_registry_object_frame <- function(status) {
  data.frame(object = sec_registry_tables(), status = status, stringsAsFactors = FALSE)
}

sec_quote_identifier <- function(con, value) {
  as.character(DBI::dbQuoteIdentifier(con, value))
}

sec_quote_table <- function(con, schema, table) {
  as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table)))
}

sec_scalar_text <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || trimws(value) == "") {
    stop(name, " must be a single non-empty character value.", call. = FALSE)
  }
  value
}

sec_whole_number <- function(value, name, minimum = 0L) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value != floor(value) || value < minimum) {
    stop(name, " must be a whole number greater than or equal to ", minimum, ".", call. = FALSE)
  }
  as.integer(value)
}

sec_require_schema <- function(con, schema, name) {
  observed <- DBI::dbGetQuery(
    con,
    "SELECT EXISTS (SELECT 1 FROM information_schema.schemata WHERE schema_name = $1) AS present",
    params = list(schema)
  )
  if (!isTRUE(observed$present[[1]])) {
    stop(name, " does not exist; create and secure it before continuing.", call. = FALSE)
  }
  invisible(TRUE)
}

sec_table_is_public <- function(con, schema, table) {
  observed <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT has_table_privilege('public', c.oid, 'SELECT') OR",
      "has_table_privilege('public', c.oid, 'INSERT') OR",
      "has_table_privilege('public', c.oid, 'UPDATE') OR",
      "has_table_privilege('public', c.oid, 'DELETE') OR",
      "has_table_privilege('public', c.oid, 'TRUNCATE') OR",
      "has_table_privilege('public', c.oid, 'REFERENCES') OR",
      "has_table_privilege('public', c.oid, 'TRIGGER') AS public_access",
      "FROM pg_class c INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2"
    ),
    params = list(schema, table)
  )
  nrow(observed) == 1L && isTRUE(observed$public_access[[1]])
}
