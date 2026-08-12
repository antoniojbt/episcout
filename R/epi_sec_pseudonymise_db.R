#' Pseudonymise related PostgreSQL tables through a stable identity registry
#'
#' Audit or transactionally create pseudonymised copies of related PostgreSQL tables. Exact linkage metadata, explicit output actions and a semantic multi-table dictionary control identity matching, retained columns and longitudinal duplicate checks.
#'
#' @param con An open PostgreSQL DBI connection created with RPostgres.
#' @param dictionary A technical and semantic multi-table dictionary accepted by [epi_eda_dictionary_validate()].
#' @param linkage An object returned by [epi_sec_linkage_spec()].
#' @param registry_schema Existing identity-registry schema initialised by [epi_sec_identity_registry_init()].
#' @param output_schema Existing schema for pseudonymised output tables.
#' @param catalogues Optional normalised semantic catalogue data frame used by
#'   `dictionary`, with `catalog_name`, `source_value`, `label`,
#'   `display_order`, `is_missing` and `provenance`.
#' @param mode `"audit"` performs no writes; `"apply"` repeats all checks and writes atomically.
#' @param token_column Name used for the pseudonym token in every output table.
#' @param exact_duplicates `"report"` retains identical projected rows; `"drop"` removes them explicitly.
#' @param existing `"error"` refuses existing destinations; `"replace"` replaces only declared ordinary tables without `CASCADE`.
#' @param sensitive_issues Deprecated compatibility alias for `include_issue_values`. `NULL` leaves the new argument unchanged; an explicit logical value warns and cannot conflict with an explicitly supplied new value.
#' @param lock_timeout Maximum seconds to wait for transaction-scoped PostgreSQL locks.
#' @param include_issue_values Logical; return identifier values for `invalid_identifier` and `unmatched_identifier` findings in an ordinary `issue_values` data frame. Other value families are not collected.
#'
#' @return An `epi_sec_pseudonymisation_result` list containing `status`; `metadata` columns `mode`, `writes`, `registry_schema`, `output_schema`, `next_action`; `identity_audit` columns `n_crosswalk_rows`, `n_unused_crosswalk_rows`, `n_crosswalk_conflicts`; `table_audit` columns `source_schema`, `source_table`, `destination_table`, `n_input`, `n_invalid_id`, `n_unmatched`, `n_missing_key`, `n_output`, `n_exact_removed`; `duplicate_audit` columns `source_schema`, `source_table`, `n_exact_excess`, `n_conflicting_keys`, `action`; `issues`; `output_dictionary`; `output_catalogues`; and `manifest` columns `source_schema`, `source_table`, `output_schema`, `output_table`, `status`, `output_type`. When `include_issue_values = TRUE`, ordinary `issue_values` columns are `issue_code`, source relation/column metadata and `source_value`. Status is `audit_complete`, `not_written` or `complete`.
#'
#' @details Audit mode is the default and performs no writes. Apply mode repeats the checks within one repeatable-read transaction, acquires bounded transaction-scoped advisory locks, and either commits the registry and output changes together or rolls them all back. `existing = "replace"` is deliberately opt-in and is limited to declared ordinary destination tables without `CASCADE`. Source tables are never altered.
#'
#' Registry, crosswalk and output operations use the connected role's configured PostgreSQL permissions. The function does not query or change schema or table privileges; authentication and permission denials are returned as sanitised technical database errors.
#'
#' A completed audit always returns `status = "audit_complete"`, whether or not technical issues are present. The value-free issue table contains `issue_code`, `severity`, `stage`, `source_schema`, `source_table`, `source_column`, `n_affected`, `message` and `recommended_action`; severity is `error` or `warning`. Apply returns `not_written` when error-severity findings or a lock timeout prevent commit. Malformed arguments, unsupported types and unsafe database or infrastructure state remain errors; apply errors roll back before returning a sanitised condition.
#'
#' The semantic dictionary must completely and currently cover every declared source table. The linkage `columns` component requires exactly the declared ID column to use `output_action = "pseudonymise"`; pseudonymised and dropped columns are excluded, while `retain` columns form the output. The generated token is semantic identifier metadata. `output_dictionary` and `output_catalogues` can pass directly to [epi_eda_dictionary_spec()].
#'
#' Identifier families are PostgreSQL `text`/`varchar`, integral types and `uuid`. Fixed-width character identifiers and nondeterministic text collations are rejected. Text matching preserves case, leading zeros and nonblank whitespace using deterministic byte-distinguishing comparisons; UUIDs follow PostgreSQL UUID identity. Matching never trims, case-folds, hashes or infers identity.
#'
#' Exact projected duplicates are retained under `exact_duplicates = "report"` and explicitly removed under `"drop"`. Equal declared record keys with different retained payloads produce an error-severity issue; no conflicting observation is selected, aggregated or overwritten. With no record key, only exact projected duplicates can be assessed.
#'
#' Pseudonymisation is reversible through the registry and is not anonymisation or automatic disclosure control. episcout does not decide whether an output may be used or disclosed and does not control PostgreSQL, driver, backup, administrator or server logging. See `vignette("longitudinal-pseudonymisation")` for the complete technical workflow and recovery guidance.
#'
#' @export
#' @seealso [epi_sec_linkage_scaffold()], [epi_sec_linkage_spec()], [epi_sec_identity_registry_init()], [epi_eda_dictionary_spec()]
#' @family longitudinal pseudonymisation
epi_sec_pseudonymise_db <- function(con,
                                    dictionary,
                                    linkage,
                                    registry_schema,
                                    output_schema,
                                    catalogues = NULL,
                                    mode = c("audit", "apply"),
                                    token_column = "entity_token",
                                    exact_duplicates = c("report", "drop"),
                                    existing = c("error", "replace"),
                                    sensitive_issues = NULL,
                                    lock_timeout = 30,
                                    include_issue_values = FALSE) {
  include_issue_values_supplied <- !missing(include_issue_values)
  diagnostics <- sec_diagnostic_options(
    sensitive_issues,
    include_issue_values,
    include_issue_values_supplied
  )
  include_issue_values <- diagnostics$include_issue_values
  legacy_issue_values_alias <- diagnostics$legacy_alias
  sec_database_boundary(
    {
      validate_postgres_connection(con)
      mode <- match.arg(mode)
      exact_duplicates <- match.arg(exact_duplicates)
      existing <- match.arg(existing)
      registry_schema <- sec_scalar_text(registry_schema, "registry_schema")
      output_schema <- sec_scalar_text(output_schema, "output_schema")
      token_column <- sec_scalar_text(token_column, "token_column")
      lock_timeout <- sec_whole_number(lock_timeout, "lock_timeout", minimum = 1L)
      if (!inherits(linkage, "epi_sec_linkage_spec") ||
        !identical(
          names(linkage),
          c("tables", "columns", "record_keys", "crosswalks")
        ) ||
        !linkage_has_exact_columns(linkage$tables, linkage_table_columns()) ||
        !linkage_has_exact_columns(linkage$columns, linkage_output_action_columns()) ||
        !linkage_has_exact_columns(linkage$record_keys, linkage_record_key_columns()) ||
        !linkage_has_exact_columns(linkage$crosswalks, linkage_crosswalk_columns())) {
        stop(
          "linkage must use the current four-component epi_sec_linkage_spec schema; regenerate saved or modified linkage objects.",
          call. = FALSE
        )
      }
      linkage <- epi_sec_linkage_spec(
        linkage$tables,
        linkage$columns,
        linkage$record_keys,
        linkage$crosswalks
      )
      if (mode == "apply" && sec_connection_is_transacting(con)) {
        stop("mode = 'apply' requires a connection that is not already inside a caller-managed transaction.", call. = FALSE)
      }
      validate_dictionary_shape(dictionary)
      validate_dictionary_values(dictionary)
      if (!is.null(catalogues)) {
        dictionary_without_catalogues <- dictionary
        dictionary_without_catalogues$catalog_name <- ""
        validate_catalogues(dictionary_without_catalogues, catalogues)
      }
      sec_require_schema(con, registry_schema, "registry_schema")
      sec_require_schema(con, output_schema, "output_schema")
      source_schemas <- unique(linkage$tables$source_schema)
      if (registry_schema == output_schema || registry_schema %in% source_schemas || output_schema %in% source_schemas) {
        stop("Source, registry and output schemas must be distinct.", call. = FALSE)
      }
      registry_observed <- sec_registry_inspect(con, registry_schema)
      if (registry_observed$state != "compatible") {
        stop("The identity registry is not initialised; run epi_sec_identity_registry_init() first.", call. = FALSE)
      }
      registry <- epi_sec_identity_registry_init(
        con,
        registry_schema,
        mode = "audit",
        token_prefix = as.character(registry_observed$metadata$token_prefix[[1]]),
        n_bytes = as.integer(registry_observed$metadata$n_bytes[[1]])
      )
      if (registry$status != "ready") {
        stop("The identity registry is not initialised; run epi_sec_identity_registry_init() first.", call. = FALSE)
      }

      context <- sec_pseudonym_context(
        con, dictionary, catalogues, linkage, registry_schema, output_schema,
        token_column, exact_duplicates, existing, include_issue_values,
        legacy_issue_values_alias
      )
      audit <- sec_pseudonym_audit(
        con,
        context,
        include_issue_values = include_issue_values
      )
      if (mode == "audit") {
        return(sec_pseudonym_result("audit_complete", mode, FALSE, context, audit))
      }
      if (sec_pseudonym_has_errors(audit)) {
        return(sec_pseudonym_result("not_written", mode, FALSE, context, audit))
      }

      lock_keys <- sec_lock_keys(context)
      lock_guard <- new.env(parent = emptyenv())
      lock_guard$keys <- character()
      on.exit(sec_release_session_locks(con, lock_guard$keys), add = TRUE)
      if (!sec_acquire_session_locks(con, lock_keys, lock_timeout, lock_guard)) {
        declaration <- context$linkage$tables[1, , drop = FALSE]
        audit$issues <- rbind(
          audit$issues,
          sec_issue(
            "lock_timeout", "error", "transaction", declaration, NA_character_, 0L,
            "Another database operation held a required registry or destination lock beyond lock_timeout.",
            "Wait for the other operation to finish, then retry."
          )
        )
        return(sec_pseudonym_result("not_written", mode, FALSE, context, audit))
      }

      applied <- tryCatch(
        DBI::dbWithTransaction(con, {
          DBI::dbExecute(con, "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ")
          sec_acquire_transaction_locks(con, lock_keys)
          lock_guard$keys <- sec_release_session_locks(con, lock_guard$keys)
          if (length(lock_guard$keys) > 0L) {
            stop("Session advisory locks could not be transferred safely; the transaction was rolled back.", call. = FALSE)
          }
          registry_inside <- sec_registry_inspect(con, registry_schema)
          if (registry_inside$state != "compatible") {
            stop("The identity registry changed after the initial audit; the transaction was rolled back.", call. = FALSE)
          }
          inside_context <- sec_pseudonym_context(
            con, dictionary, catalogues, linkage, registry_schema, output_schema,
            token_column, exact_duplicates, existing, include_issue_values,
            legacy_issue_values_alias
          )
          inside <- sec_pseudonym_audit(
            con,
            inside_context,
            include_issue_values = include_issue_values
          )
          if (sec_pseudonym_has_errors(inside)) {
            sec_stop_no_write(inside)
          }
          sec_apply_registry(con, inside_context)
          after_registry <- sec_pseudonym_audit(
            con,
            inside_context,
            include_issue_values = include_issue_values,
            registry_complete = TRUE
          )
          if (sec_pseudonym_has_errors(after_registry)) {
            sec_stop_no_write(after_registry)
          }
          sec_apply_outputs(con, inside_context, after_registry)
        }),
        epi_sec_no_write = function(error) error$audit,
        error = function(error) {
          stop("PostgreSQL pseudonymisation was rolled back safely; inspect PostgreSQL or driver logs.", call. = FALSE)
        }
      )
      if (is.list(applied) && identical(applied$rolled_back, TRUE)) {
        return(sec_pseudonym_result("not_written", mode, FALSE, context, applied))
      }
      sec_pseudonym_result("complete", mode, TRUE, context, applied)
    },
    "PostgreSQL pseudonymisation could not complete; inspect PostgreSQL or driver logs."
  )
}

sec_diagnostic_options <- function(sensitive_issues,
                                   include_issue_values,
                                   include_issue_values_supplied) {
  if (!is.logical(include_issue_values) ||
        length(include_issue_values) != 1L ||
        is.na(include_issue_values)) {
    stop("include_issue_values must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(sensitive_issues)) {
    return(list(
      include_issue_values = include_issue_values,
      legacy_alias = FALSE
    ))
  }
  if (!is.logical(sensitive_issues) ||
        length(sensitive_issues) != 1L ||
        is.na(sensitive_issues)) {
    stop("sensitive_issues must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  warning(
    "sensitive_issues is deprecated; use include_issue_values instead. Requested issue values are returned as ordinary data.",
    call. = FALSE
  )
  if (include_issue_values_supplied &&
        !identical(sensitive_issues, include_issue_values)) {
    stop(
      "sensitive_issues and include_issue_values must not conflict.",
      call. = FALSE
    )
  }
  list(
    include_issue_values = sensitive_issues,
    legacy_alias = isTRUE(sensitive_issues)
  )
}

#' @export
print.epi_sec_pseudonymisation_result <- function(x, ...) { # nolint: object_length_linter.
  cat("episcout longitudinal pseudonymisation\n")
  cat("  status: ", x$status, "\n", sep = "")
  cat("  tables: ", nrow(x$table_audit), "\n", sep = "")
  cat("  error issues: ", sum(x$issues$severity == "error"), "\n", sep = "")
  cat("  writes performed: ", if (isTRUE(x$metadata$writes[[1]])) "yes" else "no", "\n", sep = "")
  cat("  output schema: ", x$metadata$output_schema[[1]], "\n", sep = "")
  cat("  next: ", x$metadata$next_action[[1]], "\n", sep = "")
  invisible(x)
}

sec_pseudonym_context <- function(con, dictionary, catalogues, linkage, registry_schema, output_schema, token_column, exact_duplicates, existing, include_issue_values, legacy_issue_values_alias) {
  table_contexts <- lapply(seq_len(nrow(linkage$tables)), function(index) {
    declaration <- linkage$tables[index, , drop = FALSE]
    relation <- sec_relation_state(con, declaration$source_schema, declaration$source_table)
    if (!relation$exists || !identical(relation$relkind, "r")) {
      stop("Every declared source relation must be an ordinary PostgreSQL table.", call. = FALSE)
    }
    columns <- sec_source_columns(con, declaration$source_schema, declaration$source_table)
    if (nrow(columns) == 0L) {
      stop("A declared source table was not found: ", declaration$source_schema, ".", declaration$source_table, ".", call. = FALSE)
    }
    dictionary_rows <- dictionary[
      dictionary$source_schema == declaration$source_schema &
        dictionary$source_table == declaration$source_table &
        dictionary$drift_status != "removed", ,
      drop = FALSE
    ]
    if (!setequal(columns$source_column, dictionary_rows$source_column)) {
      stop(
        "The semantic dictionary does not exactly cover the current source columns.",
        call. = FALSE
      )
    }
    dictionary_rows <- dictionary_rows[match(columns$source_column, dictionary_rows$source_column), , drop = FALSE]
    action_rows <- linkage$columns[
      linkage$columns$source_schema == declaration$source_schema &
        linkage$columns$source_table == declaration$source_table, ,
      drop = FALSE
    ]
    action_match <- match(
      dictionary_rows$source_column, action_rows$source_column
    )
    if (nrow(action_rows) != nrow(dictionary_rows) || anyNA(action_match) ||
          !setequal(action_rows$source_column, dictionary_rows$source_column)) {
      stop(
        "The linkage output actions do not exactly cover the current semantic dictionary.",
        call. = FALSE
      )
    }
    action_rows <- action_rows[action_match, , drop = FALSE]
    sec_validate_current_rows(dictionary_rows)
    id_index <- match(declaration$id_column, columns$source_column)
    if (is.na(id_index)) {
      stop("The declared id_column was not found in source table ", declaration$source_schema, ".", declaration$source_table, ".", call. = FALSE)
    }
    id_family <- sec_identifier_family(columns$source_udt_name[[id_index]])
    if (is.na(id_family)) {
      stop("The declared identifier uses an unsupported PostgreSQL type in ", declaration$source_schema, ".", declaration$source_table, ".", call. = FALSE)
    }
    if (id_family == "text" && !sec_id_collation_deterministic(con, columns[id_index, , drop = FALSE])) {
      stop("A textual identifier uses a nondeterministic PostgreSQL collation and cannot provide exact identity matching.", call. = FALSE)
    }
    retained <- action_rows$output_action == "retain"
    retained_columns <- dictionary_rows$source_column[retained]
    if (token_column %in% retained_columns) {
      stop("token_column collides with a retained source column in ", declaration$source_schema, ".", declaration$source_table, ".", call. = FALSE)
    }
    keys <- linkage$record_keys[
      linkage$record_keys$source_schema == declaration$source_schema &
        linkage$record_keys$source_table == declaration$source_table, ,
      drop = FALSE
    ]
    if (nrow(keys) > 0L) keys <- keys[order(keys$key_order), , drop = FALSE]
    if (any(!(keys$key_column %in% retained_columns))) {
      stop("Every record-key column must be retained in the pseudonymised output.", call. = FALSE)
    }
    retained_meta <- columns[match(retained_columns, columns$source_column), , drop = FALSE]
    unsupported <- !(retained_meta$source_udt_name %in% sec_comparable_udt_names())
    if (any(unsupported)) {
      stop("A retained column uses a PostgreSQL type that cannot be compared safely for duplicate checks.", call. = FALSE)
    }
    list(
      declaration = declaration,
      columns = columns,
      dictionary = dictionary_rows,
      actions = action_rows,
      id_family = id_family,
      retained_columns = retained_columns,
      record_keys = keys
    )
  })
  names(table_contexts) <- vapply(table_contexts, function(item) paste(item$declaration$source_schema, item$declaration$source_table, sep = "."), character(1))
  sec_validate_crosswalks_db(con, linkage$crosswalks, table_contexts)
  sec_validate_ns_families(con, registry_schema, table_contexts, linkage$crosswalks)
  sec_validate_catalogues(
    dictionary, catalogues, linkage$tables, linkage$columns
  )
  list(
    dictionary = dictionary,
    catalogues = catalogues,
    linkage = linkage,
    registry_schema = registry_schema,
    output_schema = output_schema,
    token_column = token_column,
    exact_duplicates = exact_duplicates,
    existing = existing,
    include_issue_values = include_issue_values,
    legacy_issue_values_alias = legacy_issue_values_alias,
    tables = table_contexts
  )
}

sec_pseudonym_audit <- function(con, context, include_issue_values = FALSE, registry_complete = FALSE) {
  issues <- sec_empty_issues()
  issue_values <- sec_empty_issue_values()
  table_rows <- vector("list", length(context$tables))
  duplicate_rows <- vector("list", length(context$tables))
  mapping <- sec_mapping_ctes(con, context, registry_complete)

  crosswalk_audit <- sec_crosswalk_audit(con, context, mapping)
  issues <- rbind(issues, crosswalk_audit$issues)

  for (index in seq_along(context$tables)) {
    item <- context$tables[[index]]
    declaration <- item$declaration
    source <- sec_quote_table(con, declaration$source_schema, declaration$source_table)
    id <- sec_quote_identifier(con, declaration$id_column)
    invalid_id <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*)::bigint AS n FROM ", source, " WHERE ", id, " IS NULL OR btrim(", id, "::text) = ''")
    )$n[[1]]
    n_input <- DBI::dbGetQuery(con, paste("SELECT COUNT(*)::bigint AS n FROM", source))$n[[1]]
    unmatched_query <- sec_unmatched_query(con, context, item, mapping)
    n_unmatched <- DBI::dbGetQuery(con, unmatched_query)$n[[1]]
    key_missing <- sec_record_key_missing(con, item)
    destination <- sec_destination_state(con, context$output_schema, declaration$destination_table)

    if (invalid_id > 0) issues <- rbind(issues, sec_issue("invalid_identifier", "error", "identity", declaration, declaration$id_column, invalid_id, "Identifier values are missing or blank.", "Correct the source identifiers and retry."))
    if (n_unmatched > 0) issues <- rbind(issues, sec_issue("unmatched_identifier", "error", "identity", declaration, declaration$id_column, n_unmatched, "Identifiers cannot be resolved through the registry, enrolment source or declared crosswalk.", "Check the namespace or add a database-resident crosswalk, then retry."))
    if (key_missing > 0) issues <- rbind(issues, sec_issue("missing_record_key", "error", "duplicates", declaration, paste(item$record_keys$key_column, collapse = ","), key_missing, "Declared record-key values are missing.", "Correct the record keys or revise the linkage specification."))
    if (destination$exists && context$existing == "error") issues <- rbind(issues, sec_issue("destination_exists", "error", "output", declaration, NA_character_, 1L, "The declared destination table already exists.", "Choose a new destination or explicitly use existing = 'replace'."))
    if (destination$exists && !destination$replaceable) issues <- rbind(issues, sec_issue("unsafe_destination", "error", "output", declaration, NA_character_, 1L, "The destination is not an ordinary table owned by the current database role.", "Choose an owned ordinary destination table; dependencies and views are never replaced."))

    duplicate <- sec_duplicate_audit(con, context, item, mapping)
    if (duplicate$n_conflicting > 0) issues <- rbind(issues, sec_issue("conflicting_record_key", "error", "duplicates", declaration, NA_character_, duplicate$n_conflicting, "Equal record keys have different retained payloads.", "Resolve the conflicting records; episcout never selects or aggregates them."))
    if (nrow(item$record_keys) == 0L && !isTRUE(declaration$one_row_per_entity)) issues <- rbind(issues, sec_issue("record_key_not_declared", "warning", "duplicates", declaration, NA_character_, 0L, "No record key is declared, so only exact projected duplicates can be checked.", "Declare record-key columns when conflicting repeated observations must be detected."))

    if (include_issue_values && invalid_id > 0) {
      values <- DBI::dbGetQuery(
        con,
        paste0("SELECT ", id, "::text AS source_value FROM ", source, " WHERE ", id, " IS NULL OR btrim(", id, "::text) = ''")
      )
      issue_values <- rbind(issue_values, sec_issue_value_rows("invalid_identifier", declaration, declaration$id_column, values$source_value))
    }
    if (include_issue_values && n_unmatched > 0) {
      values <- DBI::dbGetQuery(con, sec_unmatched_query(con, context, item, mapping, return_values = TRUE))
      issue_values <- rbind(issue_values, sec_issue_value_rows("unmatched_identifier", declaration, declaration$id_column, values$source_value))
    }

    table_rows[[index]] <- data.frame(
      source_schema = declaration$source_schema,
      source_table = declaration$source_table,
      destination_table = declaration$destination_table,
      n_input = as.numeric(n_input),
      n_invalid_id = as.numeric(invalid_id),
      n_unmatched = as.numeric(n_unmatched),
      n_missing_key = as.numeric(key_missing),
      n_output = if (invalid_id == 0 && n_unmatched == 0 && key_missing == 0 && duplicate$n_conflicting == 0) {
        as.numeric(n_input) - if (context$exact_duplicates == "drop") as.numeric(duplicate$n_exact_excess) else 0
      } else {
        NA_real_
      },
      n_exact_removed = if (context$exact_duplicates == "drop") as.numeric(duplicate$n_exact_excess) else 0,
      stringsAsFactors = FALSE
    )
    duplicate_rows[[index]] <- data.frame(
      source_schema = declaration$source_schema,
      source_table = declaration$source_table,
      n_exact_excess = as.numeric(duplicate$n_exact_excess),
      n_conflicting_keys = as.numeric(duplicate$n_conflicting),
      action = context$exact_duplicates,
      stringsAsFactors = FALSE
    )
  }
  identity_audit <- data.frame(
    n_crosswalk_rows = crosswalk_audit$n_rows,
    n_unused_crosswalk_rows = crosswalk_audit$n_unused,
    n_crosswalk_conflicts = crosswalk_audit$n_conflicts,
    stringsAsFactors = FALSE
  )
  list(
    rolled_back = FALSE,
    identity_audit = identity_audit,
    table_audit = do.call(rbind, table_rows),
    duplicate_audit = do.call(rbind, duplicate_rows),
    issues = issues,
    issue_values = issue_values,
    output_dictionary = sec_output_dictionary(context),
    output_catalogues = sec_output_catalogues(context),
    manifest = sec_output_manifest(context, created = FALSE)
  )
}

sec_mapping_ctes <- function(con, context, registry_complete) {
  enrol_item <- context$tables[[which(vapply(context$tables, function(item) isTRUE(item$declaration$can_enrol), logical(1)))]]
  enrol <- enrol_item$declaration
  enrol_source <- sec_quote_table(con, enrol$source_schema, enrol$source_table)
  enrol_id <- sec_quote_identifier(con, enrol$id_column)
  crosswalk_sql <- sec_crosswalk_union(con, context$linkage$crosswalks)
  observed_sql <- paste(vapply(context$tables, function(item) {
    d <- item$declaration
    paste0(
      "SELECT ", sec_quote_literal(con, d$identity_namespace), "::text COLLATE \"C\" AS identity_namespace, ",
      "(", sec_quote_identifier(con, d$id_column), "::text COLLATE \"C\") AS source_id FROM ",
      sec_quote_table(con, d$source_schema, d$source_table), " WHERE ",
      sec_quote_identifier(con, d$id_column), " IS NOT NULL AND btrim(", sec_quote_identifier(con, d$id_column), "::text) <> ''"
    )
  }, character(1)), collapse = " UNION ")
  aliases <- sec_quote_table(con, context$registry_schema, "aliases")
  list(
    enrol_namespace = enrol$identity_namespace,
    prefix = paste0(
      "WITH enrol_ids AS (SELECT DISTINCT (", enrol_id, "::text COLLATE \"C\") AS source_id FROM ", enrol_source,
      " WHERE ", enrol_id, " IS NOT NULL AND btrim(", enrol_id, "::text) <> ''), ",
      "crosswalk_rows AS (", crosswalk_sql, "), ",
      "observed_ids AS (", observed_sql, "), ",
      "active_crosswalk AS (SELECT DISTINCT c.* FROM crosswalk_rows c INNER JOIN observed_ids o ON o.identity_namespace = c.alias_namespace AND o.source_id = c.alias_id), ",
      "resolved_crosswalk AS (SELECT c.alias_namespace, c.alias_id, c.canonical_namespace, c.canonical_id, ",
      "COALESCE(a.entity_token, CASE WHEN c.canonical_namespace = ", sec_quote_literal(con, enrol$identity_namespace),
      " AND e.source_id IS NOT NULL THEN 'NEW:' || c.canonical_namespace || ':' || c.canonical_id END) AS entity_key ",
      "FROM active_crosswalk c LEFT JOIN ", aliases, " a ON a.identity_namespace = c.canonical_namespace AND a.source_id = c.canonical_id ",
      "LEFT JOIN enrol_ids e ON c.canonical_namespace = ", sec_quote_literal(con, enrol$identity_namespace), " AND e.source_id = c.canonical_id), ",
      "prospective AS (SELECT o.identity_namespace, o.source_id, COALESCE(a.entity_token, r.entity_key, ",
      "CASE WHEN o.identity_namespace = ", sec_quote_literal(con, enrol$identity_namespace),
      " AND e.source_id IS NOT NULL THEN 'NEW:' || o.identity_namespace || ':' || o.source_id END) AS entity_key ",
      "FROM observed_ids o LEFT JOIN ", aliases, " a ON a.identity_namespace = o.identity_namespace AND a.source_id = o.source_id ",
      "LEFT JOIN resolved_crosswalk r ON r.alias_namespace = o.identity_namespace AND r.alias_id = o.source_id ",
      "LEFT JOIN enrol_ids e ON o.identity_namespace = ", sec_quote_literal(con, enrol$identity_namespace), " AND e.source_id = o.source_id) "
    )
  )
}

sec_unmatched_query <- function(con, context, item, mapping, return_values = FALSE) {
  d <- item$declaration
  source <- sec_quote_table(con, d$source_schema, d$source_table)
  id <- sec_quote_identifier(con, d$id_column)
  select <- if (return_values) paste0("SELECT ", id, "::text AS source_value") else "SELECT COUNT(*)::bigint AS n"
  paste0(
    mapping$prefix,
    select, " FROM ", source, " s LEFT JOIN prospective p ON p.identity_namespace = ",
    sec_quote_literal(con, d$identity_namespace), " AND p.source_id = (", id, "::text COLLATE \"C\") WHERE ",
    id, " IS NOT NULL AND btrim(", id, "::text) <> '' AND p.entity_key IS NULL"
  )
}

sec_duplicate_audit <- function(con, context, item, mapping) {
  d <- item$declaration
  source <- sec_quote_table(con, d$source_schema, d$source_table)
  id <- sec_quote_identifier(con, d$id_column)
  retained <- vapply(item$retained_columns, function(name) paste0("s.", sec_quote_identifier(con, name)), character(1))
  group_projection <- paste(c("p.entity_key", retained), collapse = ", ")
  resolved <- paste0(
    mapping$prefix,
    ", resolved_rows AS (SELECT p.entity_key, s.* FROM ", source,
    " s INNER JOIN prospective p ON p.identity_namespace = ", sec_quote_literal(con, d$identity_namespace),
    " AND p.source_id = (", id, "::text COLLATE \"C\")) "
  )
  exact_query <- paste0(
    resolved,
    "SELECT COALESCE(SUM(n - 1), 0)::bigint AS n FROM (SELECT COUNT(*)::bigint AS n FROM resolved_rows s JOIN prospective p ON FALSE GROUP BY ",
    group_projection, ") q"
  )
  # resolved_rows already includes entity_key; construct the query without a second mapping alias.
  exact_columns <- paste(c("entity_key", vapply(item$retained_columns, function(name) sec_quote_identifier(con, name), character(1))), collapse = ", ")
  exact_query <- paste0(resolved, "SELECT COALESCE(SUM(n - 1), 0)::bigint AS n FROM (SELECT COUNT(*)::bigint AS n FROM resolved_rows GROUP BY ", exact_columns, ") q")
  n_exact <- DBI::dbGetQuery(con, exact_query)$n[[1]]
  key_columns <- if (isTRUE(d$one_row_per_entity)) {
    "entity_key"
  } else if (nrow(item$record_keys) > 0L) {
    paste(c("entity_key", vapply(item$record_keys$key_column, function(name) sec_quote_identifier(con, name), character(1))), collapse = ", ")
  } else {
    NULL
  }
  n_conflicting <- 0
  if (!is.null(key_columns)) {
    row_columns <- paste(vapply(item$retained_columns, function(name) sec_quote_identifier(con, name), character(1)), collapse = ", ")
    conflict_query <- paste0(
      resolved,
      "SELECT COUNT(*)::bigint AS n FROM (SELECT ", key_columns,
      " FROM resolved_rows GROUP BY ", key_columns,
      " HAVING COUNT(DISTINCT ROW(", row_columns, ")) > 1) q"
    )
    n_conflicting <- DBI::dbGetQuery(con, conflict_query)$n[[1]]
  }
  list(n_exact_excess = n_exact, n_conflicting = n_conflicting)
}

sec_record_key_missing <- function(con, item) {
  if (nrow(item$record_keys) == 0L) {
    return(0)
  }
  source <- sec_quote_table(con, item$declaration$source_schema, item$declaration$source_table)
  clauses <- vapply(item$record_keys$key_column, function(name) paste0(sec_quote_identifier(con, name), " IS NULL"), character(1))
  DBI::dbGetQuery(con, paste0("SELECT COUNT(*)::bigint AS n FROM ", source, " WHERE ", paste(clauses, collapse = " OR ")))$n[[1]]
}

sec_crosswalk_audit <- function(con, context, mapping) {
  if (nrow(context$linkage$crosswalks) == 0L) {
    return(list(n_rows = 0, n_unused = 0, n_conflicts = 0, issues = sec_empty_issues()))
  }
  prefix <- mapping$prefix
  aliases <- sec_quote_table(con, context$registry_schema, "aliases")
  counts <- DBI::dbGetQuery(con, paste0(
    prefix,
    "SELECT (SELECT COUNT(*) FROM crosswalk_rows)::bigint AS n_rows, ",
    "(SELECT COUNT(*) FROM crosswalk_rows c LEFT JOIN observed_ids o ON o.identity_namespace = c.alias_namespace AND o.source_id = c.alias_id WHERE o.source_id IS NULL)::bigint AS n_unused, ",
    "(SELECT COUNT(*) FROM (SELECT alias_namespace, alias_id FROM crosswalk_rows GROUP BY alias_namespace, alias_id HAVING COUNT(DISTINCT ROW(canonical_namespace, canonical_id)) > 1) q)::bigint AS n_conflicts, ",
    "(SELECT COUNT(*) FROM resolved_crosswalk WHERE entity_key IS NULL)::bigint AS n_missing_targets, ",
    "(SELECT COUNT(*) FROM active_crosswalk c INNER JOIN active_crosswalk t ON t.alias_namespace = c.canonical_namespace AND t.alias_id = c.canonical_id)::bigint AS n_chains, ",
    "(SELECT COUNT(*) FROM crosswalk_rows WHERE alias_id IS NULL OR canonical_id IS NULL OR btrim(alias_id) = '' OR btrim(canonical_id) = '')::bigint AS n_invalid, ",
    "(SELECT COUNT(*) FROM resolved_crosswalk r INNER JOIN ", aliases,
    " a ON a.identity_namespace = r.alias_namespace AND a.source_id = r.alias_id WHERE a.entity_token <> r.entity_key)::bigint AS n_registry_conflicts"
  ))
  issues <- sec_empty_issues()
  declaration <- context$linkage$tables[which(context$linkage$tables$can_enrol), , drop = FALSE]
  if (counts$n_conflicts[[1]] > 0) issues <- rbind(issues, sec_issue("crosswalk_conflict", "error", "crosswalk", declaration, NA_character_, counts$n_conflicts[[1]], "A crosswalk alias has multiple canonical targets.", "Correct the database-resident crosswalk."))
  if (counts$n_missing_targets[[1]] > 0) issues <- rbind(issues, sec_issue("crosswalk_target_missing", "error", "crosswalk", declaration, NA_character_, counts$n_missing_targets[[1]], "Crosswalk targets are absent from the enrolment source and registry.", "Correct the canonical targets and retry."))
  if (counts$n_chains[[1]] > 0) issues <- rbind(issues, sec_issue("crosswalk_not_flat", "error", "crosswalk", declaration, NA_character_, counts$n_chains[[1]], "Crosswalk aliases form a chain or cycle.", "Flatten every mapping to one canonical enrolment or registry identifier."))
  if (counts$n_invalid[[1]] > 0) issues <- rbind(issues, sec_issue("invalid_crosswalk_identifier", "error", "crosswalk", declaration, NA_character_, counts$n_invalid[[1]], "Crosswalk identifiers are missing or blank.", "Correct the crosswalk relation and retry."))
  if (counts$n_registry_conflicts[[1]] > 0) issues <- rbind(issues, sec_issue("registry_alias_conflict", "error", "crosswalk", declaration, NA_character_, counts$n_registry_conflicts[[1]], "A crosswalk alias conflicts with its immutable registry assignment.", "Correct the crosswalk; existing registry aliases are never reassigned."))
  list(
    n_rows = as.numeric(counts$n_rows[[1]]),
    n_unused = as.numeric(counts$n_unused[[1]]),
    n_conflicts = as.numeric(counts$n_conflicts[[1]] + counts$n_missing_targets[[1]] + counts$n_chains[[1]] + counts$n_invalid[[1]] + counts$n_registry_conflicts[[1]]),
    issues = issues
  )
}

sec_apply_registry <- function(con, context) {
  mapping <- sec_mapping_ctes(con, context, registry_complete = FALSE)
  namespaces <- unique(data.frame(
    identity_namespace = vapply(context$tables, function(item) item$declaration$identity_namespace, character(1)),
    type_family = vapply(context$tables, function(item) item$id_family, character(1)),
    stringsAsFactors = FALSE
  ))
  namespace_table <- sec_quote_table(con, context$registry_schema, "namespaces")
  for (index in seq_len(nrow(namespaces))) {
    DBI::dbExecute(
      con,
      paste("INSERT INTO", namespace_table, "(identity_namespace, type_family) VALUES ($1, $2) ON CONFLICT (identity_namespace) DO NOTHING"),
      params = list(namespaces$identity_namespace[[index]], namespaces$type_family[[index]])
    )
  }

  DBI::dbExecute(con, paste0("CREATE TEMP TABLE episcout_active_crosswalk ON COMMIT DROP AS ", mapping$prefix, "SELECT * FROM active_crosswalk"))
  DBI::dbExecute(con, paste0("CREATE TEMP TABLE episcout_enrol_ids ON COMMIT DROP AS ", mapping$prefix, "SELECT * FROM enrol_ids"))
  enrol_namespace <- mapping$enrol_namespace
  aliases <- sec_quote_table(con, context$registry_schema, "aliases")
  entities <- sec_quote_table(con, context$registry_schema, "entities")
  new_count <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*)::integer AS n FROM (SELECT e.source_id FROM episcout_enrol_ids e ",
    "LEFT JOIN ", aliases, " a ON a.identity_namespace = ", sec_quote_literal(con, enrol_namespace), " AND a.source_id = e.source_id ",
    "LEFT JOIN episcout_active_crosswalk c ON c.alias_namespace = ", sec_quote_literal(con, enrol_namespace), " AND c.alias_id = e.source_id ",
    "WHERE a.source_id IS NULL AND c.alias_id IS NULL UNION SELECT c.canonical_id FROM episcout_active_crosswalk c ",
    "LEFT JOIN ", aliases, " a ON a.identity_namespace = c.canonical_namespace AND a.source_id = c.canonical_id ",
    "WHERE a.source_id IS NULL) q"
  ))$n[[1]]
  tokens <- sec_generate_registry_tokens(con, context$registry_schema, new_count)
  DBI::dbExecute(con, "CREATE TEMP TABLE episcout_tokens (sequence integer PRIMARY KEY, entity_token text NOT NULL) ON COMMIT DROP")
  if (length(tokens) > 0L) {
    DBI::dbAppendTable(con, "episcout_tokens", data.frame(sequence = seq_along(tokens), entity_token = tokens, stringsAsFactors = FALSE))
  }
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE episcout_new_canonical ON COMMIT DROP AS ",
    "SELECT identity_namespace, source_id, row_number() OVER (ORDER BY identity_namespace, source_id)::integer AS sequence FROM (",
    "SELECT ", sec_quote_literal(con, enrol_namespace), "::text AS identity_namespace, e.source_id FROM episcout_enrol_ids e ",
    "LEFT JOIN ", aliases, " a ON a.identity_namespace = ", sec_quote_literal(con, enrol_namespace), " AND a.source_id = e.source_id ",
    "LEFT JOIN episcout_active_crosswalk c ON c.alias_namespace = ", sec_quote_literal(con, enrol_namespace), " AND c.alias_id = e.source_id ",
    "WHERE a.source_id IS NULL AND c.alias_id IS NULL UNION ",
    "SELECT c.canonical_namespace, c.canonical_id FROM episcout_active_crosswalk c LEFT JOIN ", aliases,
    " a ON a.identity_namespace = c.canonical_namespace AND a.source_id = c.canonical_id WHERE a.source_id IS NULL) q"
  ))
  DBI::dbExecute(con, paste("INSERT INTO", entities, "(entity_token) SELECT t.entity_token FROM episcout_tokens t ORDER BY t.sequence"))
  DBI::dbExecute(con, paste(
    "INSERT INTO", aliases, "(identity_namespace, source_id, entity_token)",
    "SELECT c.identity_namespace, c.source_id, t.entity_token FROM episcout_new_canonical c INNER JOIN episcout_tokens t USING (sequence)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", aliases, " (identity_namespace, source_id, entity_token) ",
    "SELECT c.alias_namespace, c.alias_id, target.entity_token FROM episcout_active_crosswalk c INNER JOIN ", aliases,
    " target ON target.identity_namespace = c.canonical_namespace AND target.source_id = c.canonical_id ",
    "ON CONFLICT (identity_namespace, source_id) DO NOTHING"
  ))
  invisible(TRUE)
}

sec_apply_outputs <- function(con, context, audit) {
  run_id <- sec_generate_tokens(1L, 16L, "U")
  table_audit <- audit$table_audit
  manifest <- sec_output_manifest(context, created = TRUE)
  for (index in seq_along(context$tables)) {
    item <- context$tables[[index]]
    d <- item$declaration
    destination <- sec_quote_table(con, context$output_schema, d$destination_table)
    destination_state <- sec_destination_state(con, context$output_schema, d$destination_table)
    if (destination_state$exists) {
      DBI::dbExecute(con, paste("DROP TABLE", destination))
    }
    source <- sec_quote_table(con, d$source_schema, d$source_table)
    aliases <- sec_quote_table(con, context$registry_schema, "aliases")
    id <- sec_quote_identifier(con, d$id_column)
    fields <- character()
    for (row in seq_len(nrow(item$dictionary))) {
      column <- item$dictionary$source_column[[row]]
      action <- item$actions$output_action[[row]]
      if (column == d$id_column) {
        fields <- c(fields, paste0("a.entity_token AS ", sec_quote_identifier(con, context$token_column)))
      } else if (action == "retain") {
        fields <- c(fields, paste0("s.", sec_quote_identifier(con, column)))
      }
    }
    select_prefix <- if (context$exact_duplicates == "drop") "SELECT DISTINCT" else "SELECT"
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", destination, " AS ", select_prefix, " ", paste(fields, collapse = ", "),
      " FROM ", source, " s INNER JOIN ", aliases, " a ON a.identity_namespace = ",
      sec_quote_literal(con, d$identity_namespace), " AND a.source_id = (s.", id, "::text COLLATE \"C\")"
    ))
    not_null_columns <- c(
      context$token_column,
      item$columns$source_column[
        item$columns$source_column %in% item$retained_columns & item$columns$source_is_nullable == "NO"
      ]
    )
    for (column in unique(not_null_columns)) {
      DBI::dbExecute(
        con,
        paste("ALTER TABLE", destination, "ALTER COLUMN", sec_quote_identifier(con, column), "SET NOT NULL")
      )
    }
    n_output <- DBI::dbGetQuery(con, paste("SELECT COUNT(*)::bigint AS n FROM", destination))$n[[1]]
    expected_output <- table_audit$n_output[[index]]
    expected_removed <- table_audit$n_exact_removed[[index]]
    if (is.na(expected_output) || as.numeric(n_output) != expected_output || table_audit$n_input[[index]] - as.numeric(n_output) != expected_removed) {
      stop("Output row reconciliation failed; the transaction was rolled back.", call. = FALSE)
    }
  }
  configuration_hash <- sec_configuration_hash(context)
  DBI::dbExecute(
    con,
    paste("INSERT INTO", sec_quote_table(con, context$registry_schema, "runs"), "(run_id, configuration_hash, exact_duplicates, status) VALUES ($1, $2, $3, 'complete')"),
    params = list(run_id, configuration_hash, context$exact_duplicates)
  )
  for (index in seq_len(nrow(table_audit))) {
    DBI::dbExecute(
      con,
      paste("INSERT INTO", sec_quote_table(con, context$registry_schema, "run_tables"), "(run_id, source_schema, source_table, output_schema, output_table, n_input, n_output, n_exact_removed) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"),
      params = list(
        run_id,
        table_audit$source_schema[[index]],
        table_audit$source_table[[index]],
        context$output_schema,
        context$linkage$tables$destination_table[[index]],
        table_audit$n_input[[index]],
        table_audit$n_output[[index]],
        table_audit$n_exact_removed[[index]]
      )
    )
  }
  audit$table_audit <- table_audit
  audit$manifest <- manifest
  audit$output_dictionary <- sec_output_dictionary(context)
  audit$rolled_back <- FALSE
  audit
}

sec_generate_registry_tokens <- function(con, registry_schema, n) {
  if (n == 0L) {
    return(character())
  }
  metadata <- DBI::dbGetQuery(con, paste("SELECT token_prefix, n_bytes FROM", sec_quote_table(con, registry_schema, "registry_metadata")))
  entities <- sec_quote_table(con, registry_schema, "entities")
  for (attempt in seq_len(5L)) {
    tokens <- sec_generate_tokens(n, as.integer(metadata$n_bytes[[1]]), as.character(metadata$token_prefix[[1]]))
    collision <- any(vapply(tokens, function(token) {
      DBI::dbGetQuery(
        con,
        paste0("SELECT EXISTS (SELECT 1 FROM ", entities, " WHERE entity_token = $1) AS present"),
        params = list(token)
      )$present[[1]]
    }, logical(1)))
    if (!collision) {
      return(tokens)
    }
  }
  stop("Unique registry tokens could not be generated safely.", call. = FALSE)
}

sec_lock_keys <- function(context) {
  c(
    paste0("registry:", context$registry_schema),
    sort(unique(paste0("output:", context$output_schema, ".", context$linkage$tables$destination_table)), method = "radix")
  )
}

sec_acquire_session_locks <- function(con, keys, timeout, guard) {
  deadline <- proc.time()[["elapsed"]] + timeout
  for (key in keys) {
    repeat {
      locked <- DBI::dbGetQuery(
        con,
        "SELECT pg_try_advisory_lock(hashtextextended($1, 0)) AS acquired",
        params = list(key)
      )$acquired[[1]]
      if (isTRUE(locked)) {
        guard$keys <- c(guard$keys, key)
        break
      }
      if (proc.time()[["elapsed"]] >= deadline) {
        return(FALSE)
      }
      Sys.sleep(0.05)
    }
  }
  TRUE
}

sec_acquire_transaction_locks <- function(con, keys) {
  for (key in keys) {
    DBI::dbGetQuery(
      con,
      "SELECT pg_advisory_xact_lock(hashtextextended($1, 0))",
      params = list(key)
    )
  }
  invisible(TRUE)
}

sec_release_session_locks <- function(con, keys) {
  failed <- character()
  for (key in rev(keys)) {
    released <- tryCatch(
      {
        DBI::dbGetQuery(
          con,
          "SELECT pg_advisory_unlock(hashtextextended($1, 0))",
          params = list(key)
        )
        TRUE
      },
      error = function(error) FALSE
    )
    if (!released) {
      failed <- c(key, failed)
    }
  }
  invisible(failed)
}

sec_stop_no_write <- function(audit) {
  audit$rolled_back <- TRUE
  condition <- structure(
    list(message = "Pseudonymisation was not written and the transaction was rolled back.", call = NULL, audit = audit),
    class = c("epi_sec_no_write", "error", "condition")
  )
  stop(condition)
}

sec_pseudonym_has_errors <- function(audit) {
  nrow(audit$issues) > 0L && any(audit$issues$severity == "error")
}

sec_pseudonym_result <- function(status, mode, writes, context, audit) {
  next_action <- c(
    not_written = "Resolve every error issue before retrying mode = 'apply'.",
    audit_complete = "Inspect the result or run mode = 'apply'.",
    complete = "Use the pseudonymised tables and returned output dictionary."
  )[[status]]
  result <- list(
    status = status,
    metadata = data.frame(
      mode = mode,
      writes = writes,
      registry_schema = context$registry_schema,
      output_schema = context$output_schema,
      next_action = next_action,
      stringsAsFactors = FALSE
    ),
    identity_audit = audit$identity_audit,
    table_audit = audit$table_audit,
    duplicate_audit = audit$duplicate_audit,
    issues = audit$issues,
    output_dictionary = audit$output_dictionary,
    output_catalogues = audit$output_catalogues,
    manifest = audit$manifest
  )
  if (isTRUE(context$include_issue_values)) {
    result$issue_values <- audit$issue_values
  }
  if (isTRUE(context$legacy_issue_values_alias)) {
    result$sensitive_issues <- audit$issue_values
  }
  structure(
    result,
    class = c("epi_sec_pseudonymisation_result", "list")
  )
}

sec_empty_issues <- function() {
  data.frame(
    issue_code = character(), severity = character(), stage = character(),
    source_schema = character(), source_table = character(), source_column = character(),
    n_affected = numeric(), message = character(), recommended_action = character(),
    stringsAsFactors = FALSE
  )
}

sec_empty_issue_values <- function() {
  data.frame(
    issue_code = character(),
    source_schema = character(),
    source_table = character(),
    source_column = character(),
    source_value = character(),
    stringsAsFactors = FALSE
  )
}

sec_issue_value_rows <- function(code, declaration, column, values) {
  data.frame(
    issue_code = rep(code, length(values)),
    source_schema = rep(as.character(declaration$source_schema[[1]]), length(values)),
    source_table = rep(as.character(declaration$source_table[[1]]), length(values)),
    source_column = rep(as.character(column), length(values)),
    source_value = as.character(values),
    stringsAsFactors = FALSE
  )
}

sec_issue <- function(code, severity, stage, declaration, column, n, message, action) {
  data.frame(
    issue_code = code,
    severity = severity,
    stage = stage,
    source_schema = as.character(declaration$source_schema[[1]]),
    source_table = as.character(declaration$source_table[[1]]),
    source_column = if (length(column) == 0L || is.na(column)) "" else as.character(column),
    n_affected = as.numeric(n),
    message = message,
    recommended_action = action,
    stringsAsFactors = FALSE
  )
}

sec_source_columns <- function(con, schema, table) {
  observed <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT table_schema AS source_schema, table_name AS source_table, column_name AS source_column,",
      "ordinal_position AS source_ordinal, data_type AS source_data_type, udt_name AS source_udt_name,",
      "is_nullable AS source_is_nullable, character_maximum_length AS source_character_maximum_length,",
      "numeric_precision AS source_numeric_precision, numeric_scale AS source_numeric_scale,",
      "collation_schema AS source_collation_schema, collation_name AS source_collation_name",
      "FROM information_schema.columns WHERE table_schema = $1 AND table_name = $2 ORDER BY ordinal_position"
    ),
    params = list(schema, table)
  )
  observed$source_column_comment <- NA_character_
  observed
}

sec_relation_state <- function(con, schema, table) {
  observed <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.relkind FROM pg_class c",
      "INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2"
    ),
    params = list(schema, table)
  )
  if (nrow(observed) == 0L) {
    return(list(exists = FALSE, relkind = NA_character_))
  }
  list(exists = TRUE, relkind = as.character(observed$relkind[[1]]))
}

sec_id_collation_deterministic <- function(con, column) {
  if (is.na(column$source_collation_name[[1]]) || column$source_collation_name[[1]] == "") {
    return(TRUE)
  }
  observed <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.collisdeterministic FROM pg_collation c",
      "INNER JOIN pg_namespace n ON n.oid = c.collnamespace",
      "WHERE n.nspname = $1 AND c.collname = $2"
    ),
    params = list(column$source_collation_schema[[1]], column$source_collation_name[[1]])
  )
  nrow(observed) == 1L && isTRUE(observed$collisdeterministic[[1]])
}

sec_validate_current_rows <- function(rows) {
  if (any(rows$drift_status != "current")) {
    stop(
      "Every selected semantic dictionary row must have drift_status = 'current'.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

sec_validate_catalogues <- function(dictionary,
                                    catalogues,
                                    tables,
                                    columns) {
  selected <- merge(
    dictionary[dictionary$drift_status != "removed", , drop = FALSE],
    tables[c("source_schema", "source_table")],
    by = c("source_schema", "source_table")
  )
  actions <- merge(
    columns,
    tables[c("source_schema", "source_table")],
    by = c("source_schema", "source_table")
  )
  retained <- actions[
    actions$output_action == "retain",
    dictionary_key_columns(),
    drop = FALSE
  ]
  selected_key <- dictionary_key(selected)
  retained_key <- dictionary_key(retained)
  referenced <- unique(
    selected$catalog_name[
      selected_key %in% retained_key & selected$catalog_name != ""
    ]
  )
  if (length(referenced) == 0L) {
    return(invisible(TRUE))
  }
  invalid <- is.null(catalogues) ||
    any(!(referenced %in% catalogues$catalog_name))
  invalid_types <- selected_key %in% retained_key &
    selected$catalog_name != "" &
    !(selected$type %in% c("categorical", "binary"))
  invalid <- invalid || any(invalid_types)
  if (invalid) {
    stop(
      "A retained column references missing or incompatible semantic catalogue metadata.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

sec_identifier_family <- function(udt_name) {
  if (udt_name %in% c("text", "varchar")) {
    return("text")
  }
  if (udt_name %in% c("int2", "int4", "int8")) {
    return("integer")
  }
  if (identical(udt_name, "uuid")) {
    return("uuid")
  }
  NA_character_
}

sec_comparable_udt_names <- function() {
  c("bool", "int2", "int4", "int8", "float4", "float8", "numeric", "text", "varchar", "bpchar", "date", "timestamp", "timestamptz", "time", "timetz", "uuid")
}

sec_validate_ns_families <- function(con, registry_schema, table_contexts, crosswalks) {
  declarations <- data.frame(
    identity_namespace = vapply(table_contexts, function(item) item$declaration$identity_namespace, character(1)),
    type_family = vapply(table_contexts, function(item) item$id_family, character(1)),
    stringsAsFactors = FALSE
  )
  conflicting <- vapply(split(declarations$type_family, declarations$identity_namespace), function(values) length(unique(values)) > 1L, logical(1))
  if (any(conflicting)) stop("An identity namespace is reused with incompatible identifier type families.", call. = FALSE)
  existing <- DBI::dbGetQuery(con, paste("SELECT identity_namespace, type_family FROM", sec_quote_table(con, registry_schema, "namespaces")))
  matched <- match(declarations$identity_namespace, existing$identity_namespace)
  mismatch <- !is.na(matched) & declarations$type_family != existing$type_family[matched]
  if (any(mismatch)) stop("An identity namespace conflicts with the type family stored in the registry.", call. = FALSE)
  invisible(TRUE)
}

sec_validate_crosswalks_db <- function(con, crosswalks, table_contexts) {
  if (nrow(crosswalks) == 0L) {
    return(invisible(TRUE))
  }
  family_by_namespace <- stats::setNames(
    vapply(table_contexts, function(item) item$id_family, character(1)),
    vapply(table_contexts, function(item) item$declaration$identity_namespace, character(1))
  )
  for (index in seq_len(nrow(crosswalks))) {
    row <- crosswalks[index, , drop = FALSE]
    relation <- sec_relation_state(con, row$crosswalk_schema, row$crosswalk_table)
    if (!relation$exists || !identical(relation$relkind, "r")) {
      stop("Every declared crosswalk relation must be an ordinary PostgreSQL table.", call. = FALSE)
    }
    columns <- sec_source_columns(con, row$crosswalk_schema, row$crosswalk_table)
    if (nrow(columns) == 0L) stop("A declared crosswalk relation was not found.", call. = FALSE)
    alias_index <- match(row$alias_id_column, columns$source_column)
    canonical_index <- match(row$canonical_id_column, columns$source_column)
    if (is.na(alias_index) || is.na(canonical_index)) {
      stop("A declared crosswalk identifier column was not found.", call. = FALSE)
    }
    alias_family <- sec_identifier_family(columns$source_udt_name[[alias_index]])
    canonical_family <- sec_identifier_family(columns$source_udt_name[[canonical_index]])
    if (is.na(alias_family) || is.na(canonical_family) ||
          alias_family != family_by_namespace[[row$alias_namespace]] ||
          canonical_family != family_by_namespace[[row$canonical_namespace]]) {
      stop("Crosswalk identifier types do not match their declared namespace type families.", call. = FALSE)
    }
    if ((alias_family == "text" && !sec_id_collation_deterministic(con, columns[alias_index, , drop = FALSE])) ||
          (canonical_family == "text" && !sec_id_collation_deterministic(con, columns[canonical_index, , drop = FALSE]))) {
      stop("A textual crosswalk identifier uses a nondeterministic PostgreSQL collation.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

sec_crosswalk_union <- function(con, crosswalks) {
  if (nrow(crosswalks) == 0L) {
    return("SELECT NULL::text AS alias_namespace, NULL::text AS alias_id, NULL::text AS canonical_namespace, NULL::text AS canonical_id WHERE FALSE")
  }
  paste(vapply(seq_len(nrow(crosswalks)), function(index) {
    row <- crosswalks[index, , drop = FALSE]
    table <- sec_quote_table(con, row$crosswalk_schema, row$crosswalk_table)
    paste0(
      "SELECT ", sec_quote_literal(con, row$alias_namespace), "::text COLLATE \"C\" AS alias_namespace, ",
      "(", sec_quote_identifier(con, row$alias_id_column), "::text COLLATE \"C\") AS alias_id, ",
      sec_quote_literal(con, row$canonical_namespace), "::text COLLATE \"C\" AS canonical_namespace, ",
      "(", sec_quote_identifier(con, row$canonical_id_column), "::text COLLATE \"C\") AS canonical_id FROM ", table
    )
  }, character(1)), collapse = " UNION ALL ")
}

sec_quote_literal <- function(con, value) as.character(DBI::dbQuoteLiteral(con, value))

sec_connection_is_transacting <- function(con) {
  checker <- utils::getFromNamespace("connection_is_transacting", "RPostgres")
  isTRUE(checker(con@ptr))
}

sec_registry_requested_setting <- function(con, schema, column) {
  allowed <- c("token_prefix", "n_bytes")
  if (!(column %in% allowed)) stop("Unsupported registry setting.", call. = FALSE)
  observed <- DBI::dbGetQuery(con, paste("SELECT", column, "FROM", sec_quote_table(con, schema, "registry_metadata")))
  observed[[column]][[1]]
}

sec_destination_state <- function(con, schema, table) {
  observed <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT c.relkind, c.relispartition, pg_get_userbyid(c.relowner) = current_user AS owned,",
      "EXISTS (SELECT 1 FROM pg_depend d WHERE d.refobjid = c.oid AND d.objid <> c.oid AND d.deptype NOT IN ('i', 'a')) OR",
      "EXISTS (SELECT 1 FROM pg_depend d WHERE d.objid = c.oid AND d.deptype = 'e') AS has_dependencies",
      "FROM pg_class c INNER JOIN pg_namespace n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2"
    ),
    params = list(schema, table)
  )
  if (nrow(observed) == 0L) {
    return(list(exists = FALSE, replaceable = TRUE))
  }
  list(
    exists = TRUE,
    replaceable = identical(observed$relkind[[1]], "r") &&
      !isTRUE(observed$relispartition[[1]]) &&
      isTRUE(observed$owned[[1]]) &&
      !isTRUE(observed$has_dependencies[[1]])
  )
}

sec_output_dictionary <- function(context) {
  output <- lapply(context$tables, function(item) {
    d <- item$declaration
    retained <- item$actions$source_column[item$actions$output_action == "retain"]
    rows <- item$dictionary[
      item$dictionary$source_column == d$id_column |
        item$dictionary$source_column %in% retained, ,
      drop = FALSE
    ]
    id_index <- match(d$id_column, rows$source_column)
    rows$source_schema <- context$output_schema
    rows$source_table <- d$destination_table
    rows$source_ordinal <- seq_len(nrow(rows))
    rows$source_column_comment <- NA_character_
    rows$drift_status <- "current"
    rows$source_column[[id_index]] <- context$token_column
    rows$source_data_type[[id_index]] <- "text"
    rows$source_udt_name[[id_index]] <- "text"
    rows$source_is_nullable[[id_index]] <- "NO"
    rows$source_character_maximum_length[[id_index]] <- NA
    rows$source_numeric_precision[[id_index]] <- NA
    rows$source_numeric_scale[[id_index]] <- NA
    rows$label[[id_index]] <- "Pseudonym token"
    rows$type[[id_index]] <- "text"
    rows$role[[id_index]] <- "id"
    for (field in intersect(
      c(
        "units", "levels", "min", "max", "missing_codes", "group",
        "geo_role", "geo_pair", "geo_crs", "catalog_name"
      ),
      names(rows)
    )) {
      rows[[field]][[id_index]] <- ""
    }
    rows$required[[id_index]] <- TRUE
    rows$description[[id_index]] <- "Generated pseudonym token."
    rows$catalog_name[[id_index]] <- ""
    rows$provenance[[id_index]] <- "generated_pseudonymisation"
    rows
  })
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result[c(dictionary_source_columns(), dictionary_curated_columns(), "drift_status")]
}

sec_output_catalogues <- function(context) {
  if (is.null(context$catalogues)) {
    return(NULL)
  }
  referenced <- unique(sec_output_dictionary(context)$catalog_name)
  referenced <- referenced[referenced != ""]
  context$catalogues[context$catalogues$catalog_name %in% referenced, , drop = FALSE]
}

sec_output_manifest <- function(context, created) {
  data.frame(
    source_schema = context$linkage$tables$source_schema,
    source_table = context$linkage$tables$source_table,
    output_schema = context$output_schema,
    output_table = context$linkage$tables$destination_table,
    status = if (created) "created" else "planned",
    output_type = "pseudonymised_table",
    stringsAsFactors = FALSE
  )
}

sec_configuration_hash <- function(context) {
  fields <- c(
    context$registry_schema,
    context$output_schema,
    context$token_column,
    context$exact_duplicates,
    unlist(context$linkage$tables, use.names = FALSE),
    unlist(context$linkage$columns, use.names = FALSE),
    unlist(context$linkage$record_keys, use.names = FALSE),
    unlist(context$linkage$crosswalks, use.names = FALSE)
  )
  as.character(openssl::sha256(charToRaw(paste(fields, collapse = "\r"))))
}

sec_database_boundary <- function(expr, message) {
  tryCatch(
    withCallingHandlers(
      force(expr),
      warning = function(warning) {
        condition <- structure(
          list(message = message, call = NULL),
          class = c("epi_sec_database_condition", "error", "condition")
        )
        stop(condition)
      }
    ),
    error = function(error) {
      if (inherits(error, c("epi_sec_database_condition", "epi_sec_no_write"))) stop(error)
      native <- grepl(
        "ERROR:|Failed to (prepare|execute|fetch)|server closed|connection.*(closed|invalid)|invalid result",
        conditionMessage(error),
        ignore.case = TRUE
      )
      if (native) stop(message, call. = FALSE)
      stop(error)
    }
  )
}
