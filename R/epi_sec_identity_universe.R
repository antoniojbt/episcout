universe_source_columns <- function() {
  c(
    "source_schema", "source_table", "id_column", "identity_namespace",
    "provenance", "validation_status"
  )
}

#' Declare a reviewed PostgreSQL identifier universe
#'
#' Create a deterministic, value-free contract for auditing one identifier namespace across at least two PostgreSQL tables. The specification names relations and identifier columns but never contains identifier values.
#'
#' @param sources A data frame or CSV path with `source_schema`, `source_table`, `id_column`, `identity_namespace`, `provenance` and `validation_status`.
#' @param normalization Identifier normalisation rule. Version 1 supports only exact `"identity"` normalisation.
#' @param validity_regex `NULL` or one non-empty PostgreSQL regular expression applied to the textual representation of each non-null, non-blank identifier.
#'
#' @return An `epi_sec_identity_universe_spec` list containing normalised `sources`, `normalization`, `validity_regex`, `contract_version` and `fingerprint_sha256`. It contains metadata only.
#'
#' @details At least two unique ordinary-table declarations are required, all in one identity namespace. Every row must have non-empty provenance and `validation_status = "confirmed"`. This function validates portable metadata only; database relation, type and regular-expression checks occur in [epi_sec_identity_universe_db()]. The universe represents distinct observed identifiers under the reviewed rule, not confirmed people.
#'
#' @seealso [epi_sec_identity_universe_db()], [epi_sec_linkage_spec()], [epi_sec_identity_registry_init()]
#' @family longitudinal pseudonymisation
#' @export
epi_sec_identity_universe_spec <- function(sources,
                                           normalization = "identity",
                                           validity_regex = NULL) {
  sources <- read_linkage_csv_or_data(sources, "sources")
  validate_linkage_columns(sources, universe_source_columns(), "sources")
  if (nrow(sources) < 2L) {
    stop("sources must declare at least two PostgreSQL relations.", call. = FALSE)
  }
  sources <- sources[universe_source_columns()]
  sources <- normalise_linkage_char_cols(
    sources,
    universe_source_columns(),
    "sources"
  )
  for (column in c("source_schema", "source_table", "id_column")) {
    sources[[column]] <- vapply(
      sources[[column]],
      eda_postgres_identifier,
      character(1),
      name = paste("sources", column)
    )
  }
  if (any(sources$validation_status != "confirmed")) {
    stop("Every sources validation_status must be 'confirmed'.", call. = FALSE)
  }
  source_keys <- paste(sources$source_schema, sources$source_table, sep = "\r")
  if (anyDuplicated(source_keys)) {
    stop("sources source_schema and source_table pairs must be unique.", call. = FALSE)
  }
  if (length(unique(sources$identity_namespace)) != 1L) {
    stop("sources must declare exactly one shared identity_namespace in version 1.", call. = FALSE)
  }
  if (!is.character(normalization) || length(normalization) != 1L ||
        is.na(normalization) || normalization != "identity") {
    stop("normalization must be 'identity' in version 1.", call. = FALSE)
  }
  if (!is.null(validity_regex) &&
        (!is.character(validity_regex) || length(validity_regex) != 1L ||
           is.na(validity_regex) || trimws(validity_regex) == "")) {
    stop("validity_regex must be NULL or one non-empty character value.", call. = FALSE)
  }

  sources <- sources[order(source_keys, method = "radix"), , drop = FALSE]
  rownames(sources) <- NULL
  contract_version <- "identity-universe-1"
  contract <- list(
    sources = sources,
    normalization = normalization,
    validity_regex = validity_regex,
    contract_version = contract_version
  )
  contract$fingerprint_sha256 <- eda_postgres_fingerprint(contract)
  structure(contract, class = c("epi_sec_identity_universe_spec", "list"))
}

#' @export
print.epi_sec_identity_universe_spec <- function(x, ...) {
  cat("<epi_sec_identity_universe_spec>\n")
  cat("  Confirmed sources: ", nrow(x$sources), "\n", sep = "")
  cat("  Identity namespaces: 1\n")
  cat("  Normalisation: ", x$normalization, "\n", sep = "")
  cat("  Identifier values: not present\n")
  cat("  Next: run epi_sec_identity_universe_db() in audit mode.\n")
  invisible(x)
}

#' Audit or materialise a PostgreSQL identifier universe
#'
#' Audit one reviewed identifier namespace across multiple PostgreSQL tables or atomically publish its blocker-free canonical identifier set into a restricted schema. All ordinary results are aggregate and value-free; identifier values remain inside PostgreSQL.
#'
#' @param con An open, idle PostgreSQL DBI connection.
#' @param spec A confirmed object returned by [epi_sec_identity_universe_spec()].
#' @param mode `"audit"` runs in one read-only snapshot; `"materialise"` repeats all checks and writes one restricted table atomically.
#' @param output_schema Existing restricted destination schema. Required only for materialisation.
#' @param output_table New destination table name. Required only for materialisation.
#' @param existing Destination policy. Version 1 supports only `"error"` and never replaces a relation.
#' @param statement_timeout Maximum seconds for each PostgreSQL statement, from 1 to 3600.
#' @param lock_timeout Maximum seconds to wait for the destination advisory lock, from 1 to 3600.
#'
#' @return An `epi_sec_identity_universe_result` list with `status`, `metadata`, `source_audit`, `namespace_audit`, `overlap_audit` and value-free `issues`. Status is `audit_complete`, `blocked` or `complete`.
#'
#' @details Audit owns a `REPEATABLE READ READ ONLY` transaction and never creates a database object. Source audits report input, null, blank, invalid, observed and distinct counts, duplicate excess and maximum frequency. Namespace and pairwise tables reconcile the distinct union and overlap. Null, blank, invalid and normalisation-collision findings block; duplicates and empty sources warn.
#'
#' Materialisation requires a pre-existing output schema that grants neither `CREATE` nor `USAGE` to `PUBLIC`. It acquires bounded advisory-lock protection, repeats validation in one `REPEATABLE READ` transaction, refuses an existing destination and creates exactly `identity_namespace`, `canonical_identifier` and `source_membership_count` with a unique namespace/identifier constraint. It revokes all table privileges from `PUBLIC`, grants no analyst access, alters no source and rolls back completely on failure.
#'
#' The materialised universe is restricted and re-identifying. It records distinct observed identifiers, not confirmed people, and generates no pseudonyms. After separate authorisation and metadata review, it may be declared as an enrolment source in [epi_sec_linkage_spec()] and passed to the existing registry/pseudonymisation workflow.
#'
#' @seealso [epi_sec_identity_universe_spec()], [epi_sec_linkage_spec()], [epi_sec_identity_registry_init()], [epi_sec_pseudonymise_db()]
#' @family longitudinal pseudonymisation
#' @export
epi_sec_identity_universe_db <- function(con,
                                         spec,
                                         mode = c("audit", "materialise"),
                                         output_schema = NULL,
                                         output_table = NULL,
                                         existing = "error",
                                         statement_timeout = 60,
                                         lock_timeout = 30) {
  sec_database_boundary(
    {
      validate_postgres_connection(con)
      if (sec_connection_is_transacting(con)) {
        stop("The identity-universe workflow requires a connection outside a caller-managed transaction.", call. = FALSE)
      }
      if (!inherits(spec, "epi_sec_identity_universe_spec")) {
        stop("spec must be a confirmed epi_sec_identity_universe_spec object.", call. = FALSE)
      }
      universe_validate_spec(spec)
      mode <- match.arg(mode)
      if (!is.character(existing) || length(existing) != 1L || is.na(existing) || existing != "error") {
        stop("existing must be 'error' in version 1.", call. = FALSE)
      }
      statement_timeout <- identity_universe_timeout(statement_timeout, "statement_timeout")
      lock_timeout <- identity_universe_timeout(lock_timeout, "lock_timeout")

      if (mode == "audit") {
        if (!is.null(output_schema) || !is.null(output_table)) {
          stop("output_schema and output_table must be NULL in audit mode.", call. = FALSE)
        }
        return(identity_universe_transaction(con, read_only = TRUE, statement_timeout, {
          context <- universe_context(con, spec)
          audit <- universe_audit(con, context)
          identity_universe_result("audit", FALSE, spec, audit)
        }))
      }

      output_schema <- eda_postgres_identifier(output_schema, "output_schema")
      output_table <- eda_postgres_identifier(output_table, "output_table")

      preflight <- identity_universe_transaction(con, read_only = TRUE, statement_timeout, {
        universe_validate_destination(con, spec, output_schema, output_table)
        context <- universe_context(con, spec)
        audit <- universe_audit(con, context)
        universe_add_destination_issue(con, audit, output_schema, output_table)
      })
      if (identity_universe_is_blocked(preflight)) {
        return(identity_universe_result(
          "materialise", FALSE, spec, preflight, output_schema, output_table
        ))
      }

      lock_key <- paste0("identity-universe:", output_schema, ".", output_table)
      lock_guard <- new.env(parent = emptyenv())
      lock_guard$keys <- character()
      on.exit(sec_release_session_locks(con, lock_guard$keys), add = TRUE)
      if (!sec_acquire_session_locks(con, lock_key, lock_timeout, lock_guard)) {
        preflight$issues <- rbind(
          preflight$issues,
          identity_universe_issue(
            "lock_timeout", "blocking", "transaction", NULL, 0,
            "Another database operation held the destination lock beyond lock_timeout.",
            "Wait for the authorised operation to finish, verify its outcome and audit again."
          )
        )
        return(identity_universe_result(
          "materialise", FALSE, spec, preflight, output_schema, output_table
        ))
      }

      applied <- tryCatch(
        identity_universe_transaction(con, read_only = FALSE, statement_timeout, {
          sec_acquire_transaction_locks(con, lock_key)
          lock_guard$keys <- sec_release_session_locks(con, lock_guard$keys)
          if (length(lock_guard$keys) > 0L) {
            stop("Session advisory-lock protection could not transfer safely.", call. = FALSE)
          }
          universe_validate_destination(con, spec, output_schema, output_table)
          context <- universe_context(con, spec)
          inside <- universe_audit(con, context)
          inside <- universe_add_destination_issue(
            con, inside, output_schema, output_table
          )
          if (identity_universe_is_blocked(inside)) {
            identity_universe_stop_blocked(inside)
          }
          identity_universe_create(con, context, output_schema, output_table)
          inside
        }),
        epi_sec_identity_universe_blocked = function(error) error$audit,
        error = function(error) {
          stop(
            "PostgreSQL identity-universe materialisation was rolled back safely; ask the database administrator to inspect restricted database logs.",
            call. = FALSE
          )
        }
      )
      if (isTRUE(applied$rolled_back)) {
        return(identity_universe_result(
          "materialise", FALSE, spec, applied, output_schema, output_table
        ))
      }
      identity_universe_result(
        "materialise", TRUE, spec, applied, output_schema, output_table
      )
    },
    "PostgreSQL identity-universe work could not complete safely; ask the database administrator to inspect restricted database logs."
  )
}

#' @export
print.epi_sec_identity_universe_result <- function(x, ...) { # nolint: object_length_linter.
  cat("episcout PostgreSQL identifier universe\n")
  cat("  status: ", x$status, "\n", sep = "")
  cat("  sources: ", nrow(x$source_audit), "\n", sep = "")
  cat("  distinct observed identifiers: ", x$namespace_audit$n_distinct[[1]], "\n", sep = "")
  cat("  blocking issues: ", sum(x$issues$severity == "blocking"), "\n", sep = "")
  cat("  writes performed: ", if (isTRUE(x$metadata$writes[[1]])) "yes" else "no", "\n", sep = "")
  cat("  next: ", x$metadata$next_action[[1]], "\n", sep = "")
  invisible(x)
}

universe_validate_spec <- function(spec) {
  required <- c(
    "sources", "normalization", "validity_regex", "contract_version",
    "fingerprint_sha256"
  )
  if (!identical(names(spec), required) ||
        !identical(spec$contract_version, "identity-universe-1")) {
    stop("spec must be an unmodified object returned by epi_sec_identity_universe_spec().", call. = FALSE)
  }
  rebuilt <- epi_sec_identity_universe_spec(
    spec$sources,
    normalization = spec$normalization,
    validity_regex = spec$validity_regex
  )
  if (!identical(spec$fingerprint_sha256, rebuilt$fingerprint_sha256)) {
    stop("spec must be an unmodified object returned by epi_sec_identity_universe_spec().", call. = FALSE)
  }
  invisible(TRUE)
}

identity_universe_timeout <- function(value, name) {
  value <- sec_whole_number(value, name, minimum = 1L)
  if (value > 3600L) {
    stop(name, " must be no greater than 3600 seconds.", call. = FALSE)
  }
  value
}

identity_universe_transaction <- function(con, read_only, statement_timeout, code) {
  DBI::dbWithTransaction(con, {
    isolation <- "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ"
    if (read_only) isolation <- paste(isolation, "READ ONLY")
    DBI::dbExecute(con, isolation)
    DBI::dbExecute(
      con,
      paste0("SET LOCAL statement_timeout = '", statement_timeout * 1000L, "ms'")
    )
    force(code)
  })
}

universe_context <- function(con, spec) {
  if (!is.null(spec$validity_regex)) {
    DBI::dbGetQuery(
      con,
      paste0("SELECT ''::text ~ ", sec_quote_literal(con, spec$validity_regex), " AS valid")
    )
  }
  families <- character(nrow(spec$sources))
  for (index in seq_len(nrow(spec$sources))) {
    source <- spec$sources[index, , drop = FALSE]
    relation <- sec_relation_state(con, source$source_schema, source$source_table)
    if (!relation$exists || !identical(relation$relkind, "r")) {
      stop("Every declared identity-universe source must be an ordinary PostgreSQL table.", call. = FALSE)
    }
    columns <- sec_source_columns(con, source$source_schema, source$source_table)
    column_index <- match(source$id_column, columns$source_column)
    if (is.na(column_index)) {
      stop("Every declared identity-universe id_column must exist in its source table.", call. = FALSE)
    }
    family <- sec_identifier_family(columns$source_udt_name[[column_index]])
    if (is.na(family)) {
      stop("Identity-universe identifiers must use text, integral or UUID PostgreSQL types.", call. = FALSE)
    }
    if (family == "text" &&
          !sec_id_collation_deterministic(con, columns[column_index, , drop = FALSE])) {
      stop("A textual identity-universe identifier uses a nondeterministic PostgreSQL collation.", call. = FALSE)
    }
    families[[index]] <- family
  }
  if (length(unique(families)) != 1L) {
    stop("All sources in the identity namespace must use one compatible identifier type family.", call. = FALSE)
  }
  list(spec = spec, family = families[[1]], valid_union = universe_union_sql(con, spec))
}

universe_source_predicates <- function(con, source, validity_regex) {
  id <- sec_quote_identifier(con, source$id_column)
  non_null <- paste0(id, " IS NOT NULL")
  non_blank <- paste0("btrim(", id, "::text) <> ''")
  valid <- paste(non_null, non_blank, sep = " AND ")
  if (!is.null(validity_regex)) {
    valid <- paste0(
      valid, " AND (", id, "::text ~ ",
      sec_quote_literal(con, validity_regex), ")"
    )
  }
  list(id = id, non_null = non_null, non_blank = non_blank, valid = valid)
}

universe_union_sql <- function(con, spec) {
  paste(vapply(seq_len(nrow(spec$sources)), function(index) {
    source <- spec$sources[index, , drop = FALSE]
    predicates <- universe_source_predicates(
      con, source, spec$validity_regex
    )
    paste0(
      "SELECT ", index, "::integer AS source_index, ",
      sec_quote_literal(con, source$identity_namespace),
      "::text AS identity_namespace, (", predicates$id,
      "::text COLLATE \"C\") AS raw_identifier, (", predicates$id,
      "::text COLLATE \"C\") AS canonical_identifier FROM ",
      sec_quote_table(con, source$source_schema, source$source_table),
      " WHERE ", predicates$valid
    )
  }, character(1)), collapse = " UNION ALL ")
}

universe_source_audit <- function(con, spec) {
  rows <- lapply(seq_len(nrow(spec$sources)), function(index) {
    source <- spec$sources[index, , drop = FALSE]
    predicates <- universe_source_predicates(
      con, source, spec$validity_regex
    )
    invalid <- if (is.null(spec$validity_regex)) {
      "FALSE"
    } else {
      paste0(
        predicates$non_null, " AND ", predicates$non_blank,
        " AND NOT (", predicates$id, "::text ~ ",
        sec_quote_literal(con, spec$validity_regex), ")"
      )
    }
    table <- sec_quote_table(con, source$source_schema, source$source_table)
    query <- paste0(
      "WITH classified AS (SELECT ", predicates$id, " AS source_identifier, ",
      predicates$valid, " AS is_valid, ", predicates$id,
      " IS NULL AS is_null, (", predicates$non_null, " AND NOT (",
      predicates$non_blank, ")) AS is_blank, (", invalid,
      ") AS is_invalid FROM ", table, "), frequencies AS (SELECT COUNT(*)::bigint AS n ",
      "FROM classified WHERE is_valid GROUP BY source_identifier) SELECT ",
      "COUNT(*)::bigint AS n_input, COUNT(*) FILTER (WHERE is_null)::bigint AS n_null, ",
      "COUNT(*) FILTER (WHERE is_blank)::bigint AS n_blank, ",
      "COUNT(*) FILTER (WHERE is_invalid)::bigint AS n_invalid, ",
      "COUNT(*) FILTER (WHERE is_valid)::bigint AS n_observed, ",
      "COUNT(DISTINCT source_identifier) FILTER (WHERE is_valid)::bigint AS n_distinct, ",
      "(COUNT(*) FILTER (WHERE is_valid) - COUNT(DISTINCT source_identifier) FILTER (WHERE is_valid))::bigint AS n_duplicate_excess, ",
      "COALESCE((SELECT MAX(n) FROM frequencies), 0)::bigint AS max_frequency FROM classified"
    )
    aggregate <- DBI::dbGetQuery(con, query)
    counts <- lapply(aggregate, as.numeric)
    status <- if (counts$n_null > 0 || counts$n_blank > 0 || counts$n_invalid > 0) {
      "blocked"
    } else if (counts$n_duplicate_excess > 0 || counts$n_input == 0) {
      "warning"
    } else {
      "ready"
    }
    data.frame(
      source_schema = source$source_schema,
      source_table = source$source_table,
      id_column = source$id_column,
      identity_namespace = source$identity_namespace,
      provenance = source$provenance,
      n_input = counts$n_input,
      n_null = counts$n_null,
      n_blank = counts$n_blank,
      n_invalid = counts$n_invalid,
      n_observed = counts$n_observed,
      n_distinct = counts$n_distinct,
      n_duplicate_excess = counts$n_duplicate_excess,
      max_frequency = counts$max_frequency,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

universe_namespace_audit <- function(con, context, source_audit) {
  query <- paste0(
    "WITH all_valid AS (", context$valid_union,
    "), memberships AS (SELECT canonical_identifier, COUNT(DISTINCT source_index)::bigint AS n_sources ",
    "FROM all_valid GROUP BY canonical_identifier), collisions AS (SELECT canonical_identifier ",
    "FROM all_valid GROUP BY canonical_identifier HAVING COUNT(DISTINCT raw_identifier) > 1) SELECT ",
    "COUNT(*)::bigint AS n_distinct, COUNT(*) FILTER (WHERE n_sources = 1)::bigint AS n_single_source, ",
    "COUNT(*) FILTER (WHERE n_sources > 1)::bigint AS n_multi_source, ",
    "(SELECT COUNT(*)::bigint FROM collisions) AS n_collisions FROM memberships"
  )
  aggregate <- lapply(DBI::dbGetQuery(con, query), as.numeric)
  blocked <- aggregate$n_collisions > 0 || any(source_audit$status == "blocked")
  warning <- any(source_audit$status == "warning")
  data.frame(
    identity_namespace = context$spec$sources$identity_namespace[[1]],
    n_sources = nrow(context$spec$sources),
    n_input = sum(source_audit$n_input),
    n_observed = sum(source_audit$n_observed),
    n_distinct = aggregate$n_distinct,
    n_single_source = aggregate$n_single_source,
    n_multi_source = aggregate$n_multi_source,
    n_collisions = aggregate$n_collisions,
    status = if (blocked) "blocked" else if (warning) "warning" else "ready",
    stringsAsFactors = FALSE
  )
}

universe_overlap_audit <- function(con, context) {
  pairs <- utils::combn(seq_len(nrow(context$spec$sources)), 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(pair_index) {
    left <- pairs[1L, pair_index]
    right <- pairs[2L, pair_index]
    query <- paste0(
      "WITH all_valid AS (", context$valid_union,
      "), left_ids AS (SELECT DISTINCT canonical_identifier FROM all_valid WHERE source_index = ",
      left, "), right_ids AS (SELECT DISTINCT canonical_identifier FROM all_valid WHERE source_index = ",
      right, ") SELECT (SELECT COUNT(*)::bigint FROM left_ids) AS n_left_distinct, ",
      "(SELECT COUNT(*)::bigint FROM right_ids) AS n_right_distinct, ",
      "(SELECT COUNT(*)::bigint FROM left_ids INNER JOIN right_ids USING (canonical_identifier)) AS n_intersection"
    )
    aggregate <- lapply(DBI::dbGetQuery(con, query), as.numeric)
    left_source <- context$spec$sources[left, , drop = FALSE]
    right_source <- context$spec$sources[right, , drop = FALSE]
    data.frame(
      left_source_schema = left_source$source_schema,
      left_source_table = left_source$source_table,
      right_source_schema = right_source$source_schema,
      right_source_table = right_source$source_table,
      n_left_distinct = aggregate$n_left_distinct,
      n_right_distinct = aggregate$n_right_distinct,
      n_intersection = aggregate$n_intersection,
      n_left_exclusive = aggregate$n_left_distinct - aggregate$n_intersection,
      n_right_exclusive = aggregate$n_right_distinct - aggregate$n_intersection,
      left_coverage = if (aggregate$n_left_distinct == 0) NA_real_ else aggregate$n_intersection / aggregate$n_left_distinct,
      right_coverage = if (aggregate$n_right_distinct == 0) NA_real_ else aggregate$n_intersection / aggregate$n_right_distinct,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

identity_universe_empty_issues <- function() {
  data.frame(
    issue_code = character(), severity = character(), stage = character(),
    source_schema = character(), source_table = character(),
    source_column = character(), n_affected = numeric(), message = character(),
    recommended_action = character(), stringsAsFactors = FALSE
  )
}

identity_universe_issue <- function(code, severity, stage, source, n, message, action) {
  data.frame(
    issue_code = code,
    severity = severity,
    stage = stage,
    source_schema = if (is.null(source)) "" else source$source_schema[[1]],
    source_table = if (is.null(source)) "" else source$source_table[[1]],
    source_column = if (is.null(source)) "" else source$id_column[[1]],
    n_affected = as.numeric(n),
    message = message,
    recommended_action = action,
    stringsAsFactors = FALSE
  )
}

identity_universe_issues <- function(spec, source_audit, namespace_audit) {
  issues <- identity_universe_empty_issues()
  for (index in seq_len(nrow(source_audit))) {
    source <- spec$sources[index, , drop = FALSE]
    audit <- source_audit[index, , drop = FALSE]
    definitions <- list(
      list("null_identifier", "blocking", audit$n_null, "Identifier values are null.", "Correct null identifiers under the reviewed source-data rule, then audit again."),
      list("blank_identifier", "blocking", audit$n_blank, "Textual identifier representations are blank.", "Correct blank identifiers under the reviewed source-data rule, then audit again."),
      list("invalid_identifier", "blocking", audit$n_invalid, "Identifiers do not satisfy validity_regex.", "Review the regular expression or correct invalid identifiers, then audit again."),
      list("duplicate_identifier", "warning", audit$n_duplicate_excess, "A source contains repeated valid identifiers.", "Confirm that repeated source rows are expected; the universe retains one distinct identifier."),
      list("empty_source", "warning", as.numeric(audit$n_input == 0), "A declared source contains no rows.", "Confirm that the empty source is expected before materialisation.")
    )
    for (definition in definitions) {
      if (definition[[3]] > 0) {
        issues <- rbind(
          issues,
          identity_universe_issue(
            definition[[1]], definition[[2]], "source", source,
            definition[[3]], definition[[4]], definition[[5]]
          )
        )
      }
    }
  }
  if (namespace_audit$n_collisions[[1]] > 0) {
    issues <- rbind(
      issues,
      identity_universe_issue(
        "normalization_collision", "blocking", "namespace", NULL,
        namespace_audit$n_collisions[[1]],
        "Distinct source identifiers map to the same canonical identifier.",
        "Revise and reconfirm the normalisation contract; episcout will not select a collision member."
      )
    )
  }
  rownames(issues) <- NULL
  issues
}

universe_audit <- function(con, context) {
  source_audit <- universe_source_audit(con, context$spec)
  namespace_audit <- universe_namespace_audit(
    con, context, source_audit
  )
  overlap_audit <- universe_overlap_audit(con, context)
  list(
    rolled_back = FALSE,
    source_audit = source_audit,
    namespace_audit = namespace_audit,
    overlap_audit = overlap_audit,
    issues = identity_universe_issues(
      context$spec, source_audit, namespace_audit
    )
  )
}

identity_universe_is_blocked <- function(audit) {
  nrow(audit$issues) > 0L && any(audit$issues$severity == "blocking")
}

universe_validate_destination <- function(con, spec, schema, table) {
  sec_require_schema(con, schema, "output_schema")
  if (sec_schema_is_public(con, schema)) {
    stop("output_schema must not grant CREATE or USAGE to PUBLIC.", call. = FALSE)
  }
  same_as_source <- spec$sources$source_schema == schema &
    spec$sources$source_table == table
  if (any(same_as_source)) {
    stop("The identity-universe destination must be distinct from every source relation.", call. = FALSE)
  }
  invisible(TRUE)
}

universe_add_destination_issue <- function(con, audit, schema, table) {
  state <- sec_relation_state(con, schema, table)
  if (state$exists) {
    audit$issues <- rbind(
      audit$issues,
      identity_universe_issue(
        "destination_exists", "blocking", "output", NULL, 1,
        "The declared identity-universe destination already exists.",
        "Choose a new destination; version 1 never replaces an existing relation."
      )
    )
  }
  audit
}

identity_universe_create <- function(con, context, schema, table) {
  destination <- sec_quote_table(con, schema, table)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE ", destination, " (",
      "identity_namespace text NOT NULL, ",
      "canonical_identifier text COLLATE \"C\" NOT NULL, ",
      "source_membership_count integer NOT NULL CHECK (source_membership_count >= 1), ",
      "UNIQUE (identity_namespace, canonical_identifier))"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "WITH all_valid AS (", context$valid_union,
      ") INSERT INTO ", destination,
      " (identity_namespace, canonical_identifier, source_membership_count) ",
      "SELECT identity_namespace, canonical_identifier, COUNT(DISTINCT source_index)::integer ",
      "FROM all_valid GROUP BY identity_namespace, canonical_identifier"
    )
  )
  DBI::dbExecute(con, paste("REVOKE ALL ON TABLE", destination, "FROM PUBLIC"))
  invisible(TRUE)
}

identity_universe_stop_blocked <- function(audit) {
  audit$rolled_back <- TRUE
  condition <- structure(
    list(
      message = "Identity-universe materialisation was blocked and rolled back.",
      call = NULL,
      audit = audit
    ),
    class = c("epi_sec_identity_universe_blocked", "error", "condition")
  )
  stop(condition)
}

identity_universe_result <- function(mode,
                                     writes,
                                     spec,
                                     audit,
                                     output_schema = "",
                                     output_table = "") {
  blocked <- identity_universe_is_blocked(audit)
  status <- if (blocked) {
    "blocked"
  } else if (writes) {
    "complete"
  } else {
    "audit_complete"
  }
  next_action <- if (blocked) {
    "Resolve every blocking issue and begin again with audit mode."
  } else if (writes) {
    "Keep the restricted universe access-controlled; use it only after explicit enrolment review."
  } else if (mode == "audit") {
    "Review warnings and database access, then explicitly request materialisation."
  } else {
    "Review warnings and retry materialisation after the transient blocker is resolved."
  }
  structure(
    list(
      status = status,
      metadata = data.frame(
        mode = mode,
        writes = writes,
        contract_version = spec$contract_version,
        fingerprint_sha256 = spec$fingerprint_sha256,
        normalization = spec$normalization,
        validity_regex_declared = !is.null(spec$validity_regex),
        output_schema = output_schema,
        output_table = output_table,
        next_action = next_action,
        stringsAsFactors = FALSE
      ),
      source_audit = audit$source_audit,
      namespace_audit = audit$namespace_audit,
      overlap_audit = audit$overlap_audit,
      issues = audit$issues
    ),
    class = c("epi_sec_identity_universe_result", "list")
  )
}
