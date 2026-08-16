alias_pair_columns <- function() {
  c(
    "source_schema", "source_table", "alias_id_column", "canonical_id_column",
    "alias_namespace", "canonical_namespace", "provenance"
  )
}

#' Declare exact, database-resident identifier aliases
#'
#' Create a value-free contract for auditing exact co-observed identifier pairs
#' and, after a clean audit, materialising a crosswalk for
#' [epi_sec_linkage_spec()].  Episcout never infers aliases from similarity,
#' names, dates, or other record attributes.
#'
#' @param pairs A data frame or CSV path with exactly `source_schema`,
#'   `source_table`, `alias_id_column`, `canonical_id_column`,
#'   `alias_namespace`, `canonical_namespace` and `provenance`.
#' @param validity_regex `NULL` or one non-empty PostgreSQL regular expression
#'   applied independently to both textual identifier representations.
#'
#' @return An `epi_sec_alias_spec` containing metadata only.
#' @family longitudinal pseudonymisation
#' @export
epi_sec_alias_spec <- function(pairs, validity_regex = NULL) {
  pairs <- read_linkage_csv_or_data(pairs, "pairs")
  validate_linkage_columns(pairs, alias_pair_columns(), "pairs")
  if (nrow(pairs) < 1L) {
    stop("pairs must declare at least one PostgreSQL relation.", call. = FALSE)
  }
  pairs <- normalise_linkage_char_cols(pairs, alias_pair_columns(), "pairs")
  for (column in c(
    "source_schema", "source_table", "alias_id_column", "canonical_id_column"
  )) {
    pairs[[column]] <- vapply(
      pairs[[column]], eda_postgres_identifier, character(1),
      name = paste("pairs", column)
    )
  }
  if (any(pairs$alias_namespace == pairs$canonical_namespace)) {
    stop("alias_namespace and canonical_namespace must differ.", call. = FALSE)
  }
  source_keys <- paste(
    pairs$source_schema, pairs$source_table, pairs$alias_id_column,
    pairs$canonical_id_column, sep = "\r"
  )
  if (anyDuplicated(source_keys)) {
    stop("pairs must not repeat one source identifier-pair declaration.", call. = FALSE)
  }
  namespace_keys <- paste(pairs$alias_namespace, pairs$canonical_namespace, sep = "\r")
  if (length(unique(namespace_keys)) != 1L) {
    stop("version 1 requires one declared alias/canonical namespace pair.", call. = FALSE)
  }
  if (!is.null(validity_regex) &&
        (!is.character(validity_regex) || length(validity_regex) != 1L ||
           is.na(validity_regex) || trimws(validity_regex) == "")) {
    stop("validity_regex must be NULL or one non-empty character value.", call. = FALSE)
  }
  pairs <- pairs[order(source_keys, method = "radix"), , drop = FALSE]
  rownames(pairs) <- NULL
  contract <- list(
    pairs = pairs,
    validity_regex = validity_regex,
    contract_version = "exact-alias-1"
  )
  contract$fingerprint_sha256 <- eda_postgres_fingerprint(contract)
  structure(contract, class = c("epi_sec_alias_spec", "list"))
}

#' @export
print.epi_sec_alias_spec <- function(x, ...) {
  cat("<epi_sec_alias_spec>\n")
  cat("  Identifier-pair sources: ", nrow(x$pairs), "\n", sep = "")
  cat("  Identifier values: not present\n")
  cat("  Next: use epi_sec_alias_db() to audit or materialise.\n")
  invisible(x)
}

#' Audit or materialise exact identifier aliases
#'
#' Audit explicit identifier-pair declarations in one PostgreSQL snapshot, or
#' materialise a flat `alias_id`/`canonical_id` crosswalk after all ambiguity
#' checks pass. Ordinary result objects are aggregate and value-free.
#'
#' @param con An open, idle PostgreSQL DBI connection.
#' @param spec An unmodified object returned by [epi_sec_alias_spec()].
#' @param mode Either `"audit"` or `"materialise"`.
#' @param output_schema Existing destination schema for materialisation.
#' @param output_table New ordinary PostgreSQL destination table.
#' @param existing Destination policy; only `"error"` is supported.
#' @param statement_timeout Maximum seconds for each statement, from 1 to 3600.
#' @param lock_timeout Maximum seconds for the destination advisory lock, from
#'   1 to 3600.
#'
#' @return An `epi_sec_alias_result` with value-free source and mapping audits,
#'   issues, and crosswalk metadata suitable for [epi_sec_linkage_spec()].
#'
#' @details Source rows with a missing, blank, or regex-invalid declared
#' identifier are errors.  Alias-to-many-canonical and canonical-to-many-alias
#' mappings are errors; duplicate exact pairs are reported as warnings.  Apply
#' never replaces a destination, and rolls back fully if checks change between
#' preflight and write.  The resulting table contains no namespace columns:
#' namespaces remain caller-declared linkage metadata.
#' @family longitudinal pseudonymisation
#' @export
epi_sec_alias_db <- function(con,
                             spec,
                             mode = c("audit", "materialise"),
                             output_schema = NULL,
                             output_table = NULL,
                             existing = "error",
                             statement_timeout = 60,
                             lock_timeout = 30) {
  sec_database_boundary({
    validate_postgres_connection(con)
    if (sec_connection_is_transacting(con)) {
      stop("The alias workflow requires a connection outside a caller-managed transaction.", call. = FALSE)
    }
    alias_validate_spec(spec)
    mode <- match.arg(mode)
    if (!identical(existing, "error")) stop("existing must be 'error'.", call. = FALSE)
    statement_timeout <- alias_timeout(statement_timeout, "statement_timeout")
    lock_timeout <- alias_timeout(lock_timeout, "lock_timeout")
    if (mode == "audit") {
      if (!is.null(output_schema) || !is.null(output_table)) {
        stop("output_schema and output_table must be NULL in audit mode.", call. = FALSE)
      }
      return(alias_transaction(con, TRUE, statement_timeout, {
        alias_result("audit", FALSE, spec, alias_audit(con, alias_context(con, spec)))
      }))
    }
    output_schema <- eda_postgres_identifier(output_schema, "output_schema")
    output_table <- eda_postgres_identifier(output_table, "output_table")
    preflight <- alias_transaction(con, TRUE, statement_timeout, {
      context <- alias_context(con, spec)
      audit <- alias_audit(con, context)
      alias_destination_issue(con, audit, output_schema, output_table)
    })
    if (alias_has_errors(preflight)) {
      return(alias_result("materialise", FALSE, spec, preflight, output_schema, output_table))
    }
    lock_key <- paste0("exact-alias:", output_schema, ".", output_table)
    guard <- new.env(parent = emptyenv())
    guard$keys <- character()
    on.exit(sec_release_session_locks(con, guard$keys), add = TRUE)
    if (!sec_acquire_session_locks(con, lock_key, lock_timeout, guard)) {
      preflight$issues <- rbind(preflight$issues, alias_issue(
        "lock_timeout", "error", "transaction", NULL, 0,
        "Another database operation held the destination lock beyond lock_timeout.",
        "Wait for the other operation to finish, verify its outcome and retry."
      ))
      return(alias_result("materialise", FALSE, spec, preflight, output_schema, output_table))
    }
    applied <- tryCatch(alias_transaction(con, FALSE, statement_timeout, {
      sec_acquire_transaction_locks(con, lock_key)
      guard$keys <- sec_release_session_locks(con, guard$keys)
      if (length(guard$keys) > 0L) stop("Session advisory-lock protection could not transfer safely.", call. = FALSE)
      context <- alias_context(con, spec)
      audit <- alias_destination_issue(con, alias_audit(con, context), output_schema, output_table)
      if (alias_has_errors(audit)) alias_stop_write(audit)
      alias_create(con, context, output_schema, output_table)
      audit
    }), epi_sec_alias_not_written = function(error) error$audit,
    error = function(error) stop("PostgreSQL alias materialisation was rolled back safely; ask the database administrator to inspect database logs.", call. = FALSE))
    alias_result("materialise", !isTRUE(applied$rolled_back), spec, applied, output_schema, output_table)
  }, "PostgreSQL exact-alias work could not complete safely; ask the database administrator to inspect database logs.")
}

#' @export
print.epi_sec_alias_result <- function(x, ...) {
  cat("episcout PostgreSQL exact aliases\n")
  cat("  status: ", x$status, "\n", sep = "")
  cat("  mappings: ", x$mapping_audit$n_distinct_pairs[[1]], "\n", sep = "")
  cat("  error issues: ", sum(x$issues$severity == "error"), "\n", sep = "")
  invisible(x)
}

alias_validate_spec <- function(spec) {
  required <- c("pairs", "validity_regex", "contract_version", "fingerprint_sha256")
  message <- "spec must be an unmodified exact-alias-1 object; regenerate it with epi_sec_alias_spec()."
  if (!identical(class(spec), c("epi_sec_alias_spec", "list")) ||
        !identical(names(spec), required) || !identical(spec$contract_version, "exact-alias-1") ||
        !is.data.frame(spec$pairs) || !identical(names(spec$pairs), alias_pair_columns())) stop(message, call. = FALSE)
  rebuilt <- tryCatch(epi_sec_alias_spec(spec$pairs, spec$validity_regex), error = function(error) NULL)
  if (is.null(rebuilt) || !identical(spec, rebuilt)) stop(message, call. = FALSE)
  invisible(TRUE)
}

alias_timeout <- function(value, name) {
  value <- sec_whole_number(value, name, minimum = 1L)
  if (value > 3600L) stop(name, " must be no greater than 3600 seconds.", call. = FALSE)
  value
}

alias_transaction <- function(con, read_only, statement_timeout, code) {
  DBI::dbWithTransaction(con, {
    isolation <- "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ"
    if (read_only) isolation <- paste(isolation, "READ ONLY")
    DBI::dbExecute(con, isolation)
    DBI::dbExecute(con, paste0("SET LOCAL statement_timeout = '", statement_timeout * 1000L, "ms'"))
    force(code)
  })
}

alias_context <- function(con, spec) {
  if (!is.null(spec$validity_regex)) DBI::dbGetQuery(con, paste0("SELECT ''::text ~ ", sec_quote_literal(con, spec$validity_regex), " AS valid"))
  for (index in seq_len(nrow(spec$pairs))) {
    pair <- spec$pairs[index, , drop = FALSE]
    state <- sec_relation_state(con, pair$source_schema, pair$source_table)
    if (!state$exists || !identical(state$relkind, "r")) stop("Every alias source must be an ordinary PostgreSQL table.", call. = FALSE)
    columns <- sec_source_columns(con, pair$source_schema, pair$source_table)
    positions <- match(c(pair$alias_id_column, pair$canonical_id_column), columns$source_column)
    if (anyNA(positions)) stop("Every declared alias identifier column must exist in its source table.", call. = FALSE)
    families <- vapply(positions, function(position) sec_identifier_family(columns$source_udt_name[[position]]), character(1))
    if (anyNA(families)) stop("Alias identifiers must use text, integral or UUID PostgreSQL types.", call. = FALSE)
    for (position in positions[families == "text"]) if (!sec_id_collation_deterministic(con, columns[position, , drop = FALSE])) stop("A textual alias identifier uses a nondeterministic PostgreSQL collation.", call. = FALSE)
  }
  list(spec = spec, valid_union = alias_union_sql(con, spec))
}

alias_predicates <- function(con, pair, column, regex) {
  id <- sec_quote_identifier(con, pair[[column]][[1]])
  non_null <- paste0(id, " IS NOT NULL")
  non_blank <- paste0("btrim(", id, "::text) <> ''")
  valid <- paste(non_null, non_blank, sep = " AND ")
  if (!is.null(regex)) valid <- paste0(valid, " AND (", id, "::text ~ ", sec_quote_literal(con, regex), ")")
  list(id = id, non_null = non_null, non_blank = non_blank, valid = valid)
}

alias_union_sql <- function(con, spec) {
  paste(vapply(seq_len(nrow(spec$pairs)), function(index) {
    pair <- spec$pairs[index, , drop = FALSE]
    alias <- alias_predicates(con, pair, "alias_id_column", spec$validity_regex)
    canonical <- alias_predicates(con, pair, "canonical_id_column", spec$validity_regex)
    paste0("SELECT ", index, "::integer AS source_index, (", alias$id, "::text COLLATE \"C\") AS alias_id, (", canonical$id, "::text COLLATE \"C\") AS canonical_id FROM ", sec_quote_table(con, pair$source_schema, pair$source_table), " WHERE ", alias$valid, " AND ", canonical$valid)
  }, character(1)), collapse = " UNION ALL ")
}

alias_source_audit <- function(con, spec) {
  rows <- lapply(seq_len(nrow(spec$pairs)), function(index) {
    pair <- spec$pairs[index, , drop = FALSE]
    alias <- alias_predicates(con, pair, "alias_id_column", spec$validity_regex)
    canonical <- alias_predicates(con, pair, "canonical_id_column", spec$validity_regex)
    table <- sec_quote_table(con, pair$source_schema, pair$source_table)
    invalid <- function(predicates) if (is.null(spec$validity_regex)) "FALSE" else paste0(predicates$non_null, " AND ", predicates$non_blank, " AND NOT (", predicates$id, "::text ~ ", sec_quote_literal(con, spec$validity_regex), ")")
    query <- paste0("SELECT COUNT(*)::bigint AS n_input, COUNT(*) FILTER (WHERE ", alias$valid, " AND ", canonical$valid, ")::bigint AS n_both_valid, COUNT(*) FILTER (WHERE ", alias$valid, " AND NOT (", canonical$valid, "))::bigint AS n_alias_only, COUNT(*) FILTER (WHERE NOT (", alias$valid, ") AND ", canonical$valid, ")::bigint AS n_canonical_only, COUNT(*) FILTER (WHERE NOT (", alias$valid, ") AND NOT (", canonical$valid, "))::bigint AS n_neither_valid, COUNT(*) FILTER (WHERE ", alias$id, " IS NULL)::bigint AS n_alias_null, COUNT(*) FILTER (WHERE ", canonical$id, " IS NULL)::bigint AS n_canonical_null, COUNT(*) FILTER (WHERE ", alias$non_null, " AND NOT (", alias$non_blank, "))::bigint AS n_alias_blank, COUNT(*) FILTER (WHERE ", canonical$non_null, " AND NOT (", canonical$non_blank, "))::bigint AS n_canonical_blank, COUNT(*) FILTER (WHERE ", invalid(alias), ")::bigint AS n_alias_invalid, COUNT(*) FILTER (WHERE ", invalid(canonical), ")::bigint AS n_canonical_invalid FROM ", table)
    counts <- lapply(DBI::dbGetQuery(con, query), as.numeric)
    data.frame(pair, as.data.frame(counts, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

alias_mapping_audit <- function(con, context) {
  query <- paste0("WITH pairs AS (", context$valid_union, "), aliases AS (SELECT alias_id, COUNT(DISTINCT canonical_id)::bigint AS n_targets FROM pairs GROUP BY alias_id), canonicals AS (SELECT canonical_id, COUNT(DISTINCT alias_id)::bigint AS n_aliases FROM pairs GROUP BY canonical_id) SELECT COUNT(*)::bigint AS n_pair_rows, COUNT(DISTINCT alias_id)::bigint AS n_distinct_alias, COUNT(DISTINCT canonical_id)::bigint AS n_distinct_canonical, COUNT(DISTINCT ROW(alias_id, canonical_id))::bigint AS n_distinct_pairs, (COUNT(*) - COUNT(DISTINCT ROW(alias_id, canonical_id)))::bigint AS n_duplicate_pairs, (SELECT COUNT(*)::bigint FROM aliases WHERE n_targets > 1) AS n_alias_conflicts, (SELECT COUNT(*)::bigint FROM canonicals WHERE n_aliases > 1) AS n_canonical_conflicts FROM pairs")
  counts <- lapply(DBI::dbGetQuery(con, query), as.numeric)
  data.frame(as.data.frame(counts, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
}

alias_empty_issues <- function() data.frame(issue_code = character(), severity = character(), stage = character(), source_schema = character(), source_table = character(), source_column = character(), n_affected = numeric(), message = character(), recommended_action = character(), stringsAsFactors = FALSE)

alias_issue <- function(code, severity, stage, pair, n, message, action) data.frame(issue_code = code, severity = severity, stage = stage, source_schema = if (is.null(pair)) "" else pair$source_schema[[1]], source_table = if (is.null(pair)) "" else pair$source_table[[1]], source_column = "", n_affected = as.numeric(n), message = message, recommended_action = action, stringsAsFactors = FALSE)

alias_issues <- function(spec, source_audit, mapping_audit) {
  issues <- alias_empty_issues()
  for (index in seq_len(nrow(source_audit))) {
    row <- source_audit[index, , drop = FALSE]
    definitions <- list(list("alias_missing_or_invalid", row$n_alias_null + row$n_alias_blank + row$n_alias_invalid, "Alias identifiers are null, blank or invalid."), list("canonical_missing_or_invalid", row$n_canonical_null + row$n_canonical_blank + row$n_canonical_invalid, "Canonical identifiers are null, blank or invalid."), list("incomplete_identifier_pair", row$n_alias_only + row$n_canonical_only, "A source row has only one valid declared identifier."))
    for (item in definitions) if (item[[2]] > 0) issues <- rbind(issues, alias_issue(item[[1]], "error", "source", row, item[[2]], item[[3]], "Correct the explicitly declared identifier relationship, then audit again."))
  }
  if (mapping_audit$n_alias_conflicts[[1]] > 0) issues <- rbind(issues, alias_issue("alias_to_many", "error", "mapping", NULL, mapping_audit$n_alias_conflicts[[1]], "An alias identifier maps to multiple canonical identifiers.", "Correct the source relationship; no resolution policy is implied."))
  if (mapping_audit$n_canonical_conflicts[[1]] > 0) issues <- rbind(issues, alias_issue("canonical_to_many", "error", "mapping", NULL, mapping_audit$n_canonical_conflicts[[1]], "A canonical identifier maps to multiple aliases.", "Correct the source relationship; no resolution policy is implied."))
  if (mapping_audit$n_duplicate_pairs[[1]] > 0) issues <- rbind(issues, alias_issue("duplicate_exact_pair", "warning", "mapping", NULL, mapping_audit$n_duplicate_pairs[[1]], "An exact identifier pair is repeated.", "Determine whether repeated source observations are expected; materialisation retains one pair."))
  rownames(issues) <- NULL
  issues
}

alias_audit <- function(con, context) {
  source_audit <- alias_source_audit(con, context$spec)
  mapping_audit <- alias_mapping_audit(con, context)
  list(rolled_back = FALSE, source_audit = source_audit, mapping_audit = mapping_audit, issues = alias_issues(context$spec, source_audit, mapping_audit))
}

alias_has_errors <- function(audit) nrow(audit$issues) > 0L && any(audit$issues$severity == "error")

alias_destination_issue <- function(con, audit, schema, table) {
  sec_require_schema(con, schema, "output_schema")
  if (sec_relation_state(con, schema, table)$exists) audit$issues <- rbind(audit$issues, alias_issue("destination_exists", "error", "output", NULL, 1, "The declared alias destination already exists.", "Choose a new destination; the function never replaces an existing relation."))
  audit
}

alias_create <- function(con, context, schema, table) {
  destination <- sec_quote_table(con, schema, table)
  DBI::dbExecute(con, paste0("CREATE TABLE ", destination, " (alias_id text COLLATE \"C\" NOT NULL, canonical_id text COLLATE \"C\" NOT NULL, UNIQUE (alias_id), UNIQUE (canonical_id))"))
  DBI::dbExecute(con, paste0("WITH pairs AS (", context$valid_union, ") INSERT INTO ", destination, " (alias_id, canonical_id) SELECT DISTINCT alias_id, canonical_id FROM pairs"))
  invisible(TRUE)
}

alias_stop_write <- function(audit) {
  audit$rolled_back <- TRUE
  stop(structure(
    list(
      message = "Exact-alias materialisation was not written and rolled back.",
      call = NULL,
      audit = audit
    ),
    class = c("epi_sec_alias_not_written", "error", "condition")
  ))
}

alias_result <- function(mode, writes, spec, audit, output_schema = "", output_table = "") {
  status <- if (writes) "complete" else if (mode == "audit") "audit_complete" else "not_written"
  structure(list(status = status, metadata = data.frame(mode = mode, writes = writes, contract_version = spec$contract_version, fingerprint_sha256 = spec$fingerprint_sha256, output_schema = output_schema, output_table = output_table, alias_namespace = spec$pairs$alias_namespace[[1]], canonical_namespace = spec$pairs$canonical_namespace[[1]], crosswalk_alias_column = "alias_id", crosswalk_canonical_column = "canonical_id", stringsAsFactors = FALSE), source_audit = audit$source_audit, mapping_audit = audit$mapping_audit, issues = audit$issues), class = c("epi_sec_alias_result", "list"))
}
