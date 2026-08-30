#' Import reviewed assignments into an identity registry
#'
#' Audit or transactionally import a PostgreSQL relation containing existing identifier-to-token assignments. Identifier and token values remain in PostgreSQL.
#'
#' @param con An open PostgreSQL DBI connection created with RPostgres.
#' @param registry_schema A compatible version-2 identity-registry schema.
#' @param source_schema,source_table Existing ordinary PostgreSQL source relation.
#' @param identifier_column,token_column Source columns containing identifiers and preserved tokens.
#' @param identity_namespace Non-empty registry namespace for the identifiers.
#' @param normalization `"identity"`, `"trim"` or `"trim_upper"`.
#' @param validity_regex `NULL` or a non-empty PostgreSQL regular expression applied after preparation.
#' @param invalid_policy `"fail"` or `"retain_and_flag"`; import records no flag, but the latter permits reviewed regex mismatches.
#' @param mode `"audit"` performs no writes; `"apply"` repeats the audit and imports atomically.
#'
#' @return An `epi_sec_registry_import_result` with status, mode, writes, aggregate counts and value-free issues.
#'
#' @details Import never changes an existing alias assignment. Blank identifiers or tokens, preparation collisions, duplicate source mappings and registry conflicts prevent every write. Multiple identifiers may intentionally share one token. The function does not grant privileges, select roles, copy values into R or manage backups.
#'
#' @export
#' @family longitudinal pseudonymisation
# nolint start: object_length_linter.
epi_sec_identity_registry_import <- function(con,
                                             registry_schema,
                                             source_schema,
                                             source_table,
                                             identifier_column,
                                             token_column,
                                             identity_namespace,
                                             normalization = c("identity", "trim", "trim_upper"),
                                             validity_regex = NULL,
                                             invalid_policy = c("fail", "retain_and_flag"),
                                             mode = c("audit", "apply")) {
  # nolint end
  sec_database_boundary(
    {
      validate_postgres_connection(con)
      registry_schema <- sec_scalar_text(registry_schema, "registry_schema")
      source_schema <- eda_postgres_identifier(source_schema, "source_schema")
      source_table <- eda_postgres_identifier(source_table, "source_table")
      identifier_column <- eda_postgres_identifier(identifier_column, "identifier_column")
      token_column <- eda_postgres_identifier(token_column, "token_column")
      identity_namespace <- sec_scalar_text(identity_namespace, "identity_namespace")
      normalization <- match.arg(normalization)
      invalid_policy <- match.arg(invalid_policy)
      mode <- match.arg(mode)
      if (!is.null(validity_regex) &&
            (!is.character(validity_regex) || length(validity_regex) != 1L ||
               is.na(validity_regex) || trimws(validity_regex) == "")) {
        stop("validity_regex must be NULL or one non-empty character value.", call. = FALSE)
      }
      if (mode == "apply" && sec_connection_is_transacting(con)) {
        stop("mode = 'apply' requires a connection outside a caller-managed transaction.", call. = FALSE)
      }
      observed <- sec_registry_inspect(con, registry_schema)
      if (observed$state != "compatible") {
        stop("registry_schema must contain a compatible version-2 identity registry.", call. = FALSE)
      }
      relation <- sec_relation_state(con, source_schema, source_table)
      if (!relation$exists || relation$relkind != "r") {
        stop("The import source must be an ordinary PostgreSQL table.", call. = FALSE)
      }
      columns <- sec_source_columns(con, source_schema, source_table)
      id_index <- match(identifier_column, columns$source_column)
      token_index <- match(token_column, columns$source_column)
      if (is.na(id_index) || is.na(token_index)) {
        stop("identifier_column and token_column must exist in the import source.", call. = FALSE)
      }
      family <- sec_identifier_family(columns$source_udt_name[[id_index]])
      if (is.na(family)) stop("identifier_column uses an unsupported PostgreSQL type.", call. = FALSE)
      rule <- data.frame(
        normalization = normalization,
        validity_regex = if (is.null(validity_regex)) NA_character_ else validity_regex,
        stringsAsFactors = FALSE
      )
      preparation_hash <- sec_preparation_hash(rule)
      audit <- sec_registry_import_audit(
        con, registry_schema, source_schema, source_table,
        identifier_column, token_column, identity_namespace,
        rule, invalid_policy, family, preparation_hash
      )
      if (mode == "audit") return(sec_registry_import_result("audit_complete", mode, FALSE, audit))
      if (any(audit$issues$severity == "error")) return(sec_registry_import_result("not_written", mode, FALSE, audit))

      applied <- tryCatch(
        DBI::dbWithTransaction(con, {
          DBI::dbExecute(con, "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ")
          inside <- sec_registry_import_audit(
            con, registry_schema, source_schema, source_table,
            identifier_column, token_column, identity_namespace,
            rule, invalid_policy, family, preparation_hash
          )
          if (any(inside$issues$severity == "error")) sec_stop_no_write(inside)
          sec_registry_import_apply(
            con, registry_schema, source_schema, source_table,
            identifier_column, token_column, identity_namespace,
            rule, family, preparation_hash
          )
          inside
        }),
        epi_sec_no_write = function(error) error$audit
      )
      if (is.list(applied) && identical(applied$rolled_back, TRUE)) {
        return(sec_registry_import_result("not_written", mode, FALSE, applied))
      }
      sec_registry_import_result("complete", mode, TRUE, applied)
    },
    "Identity-registry import could not complete; inspect PostgreSQL or driver logs."
  )
}

sec_registry_import_audit <- function(con, registry_schema, source_schema, source_table,
                                      identifier_column, token_column, identity_namespace,
                                      rule, invalid_policy, family, preparation_hash) {
  source <- sec_quote_table(con, source_schema, source_table)
  id <- sec_quote_identifier(con, identifier_column)
  token <- sec_quote_identifier(con, token_column)
  prepared <- sec_identifier_expression(con, rule, id)
  regex_invalid <- if (is.na(rule$validity_regex[[1]])) "FALSE" else paste0("NOT (", prepared, " ~ ", sec_quote_literal(con, rule$validity_regex[[1]]), ")")
  aliases <- sec_quote_table(con, registry_schema, "aliases")
  counts <- DBI::dbGetQuery(con, paste0(
    "WITH prepared AS (SELECT ", id, "::text COLLATE \"C\" AS raw_id, ", prepared,
    " AS source_id, ", token, "::text AS entity_token FROM ", source, "), ",
    "summary AS (SELECT COUNT(*)::bigint AS n_input, ",
    "COUNT(*) FILTER (WHERE source_id IS NULL OR btrim(source_id) = '' OR entity_token IS NULL OR btrim(entity_token) = '')::bigint AS n_invalid, ",
    "COUNT(*) FILTER (WHERE source_id IS NOT NULL AND btrim(source_id) <> '' AND ", regex_invalid, ")::bigint AS n_regex_invalid FROM prepared), ",
    "collisions AS (SELECT COUNT(*)::bigint AS n FROM (SELECT source_id FROM prepared WHERE source_id IS NOT NULL GROUP BY source_id HAVING COUNT(DISTINCT raw_id) > 1) q), ",
    "duplicates AS (SELECT COUNT(*)::bigint AS n FROM (SELECT source_id FROM prepared GROUP BY source_id HAVING COUNT(DISTINCT entity_token) > 1) q), ",
    "conflicts AS (SELECT COUNT(*)::bigint AS n FROM prepared p INNER JOIN ", aliases,
    " a ON a.identity_namespace = ", sec_quote_literal(con, identity_namespace),
    " AND a.source_id = p.source_id WHERE a.entity_token <> p.entity_token) ",
    "SELECT summary.*, collisions.n AS n_collisions, duplicates.n AS n_duplicate_mappings, conflicts.n AS n_registry_conflicts FROM summary, collisions, duplicates, conflicts"
  ))
  issues <- sec_empty_issues()
  declaration <- data.frame(source_schema = source_schema, source_table = source_table, stringsAsFactors = FALSE)
  add <- function(code, severity, n, message, action) {
    sec_issue(code, severity, "registry_import", declaration, identifier_column, n, message, action)
  }
  if (counts$n_invalid[[1]] > 0) issues <- rbind(issues, add("invalid_import_assignment", "error", counts$n_invalid[[1]], "Import identifiers and tokens must be nonblank.", "Correct the reviewed import relation."))
  if (counts$n_regex_invalid[[1]] > 0) issues <- rbind(issues, add("identifier_regex_mismatch", if (invalid_policy == "fail") "error" else "warning", counts$n_regex_invalid[[1]], "Prepared import identifiers do not satisfy validity_regex.", "Correct or explicitly retain the reviewed identifiers."))
  if (counts$n_collisions[[1]] > 0) issues <- rbind(issues, add("normalization_collision", "error", counts$n_collisions[[1]], "Preparation maps distinct import identifiers to one value.", "Correct the import or preparation rule."))
  if (counts$n_duplicate_mappings[[1]] > 0) issues <- rbind(issues, add("duplicate_import_mapping", "error", counts$n_duplicate_mappings[[1]], "One prepared identifier maps to multiple tokens.", "Resolve the duplicate mapping."))
  if (counts$n_registry_conflicts[[1]] > 0) issues <- rbind(issues, add("registry_alias_conflict", "error", counts$n_registry_conflicts[[1]], "An imported assignment conflicts with an immutable registry alias.", "Use the existing assignment or a separate registry."))
  existing <- DBI::dbGetQuery(con, paste("SELECT identity_namespace, type_family, preparation_hash FROM", sec_quote_table(con, registry_schema, "namespaces")))
  match_index <- match(identity_namespace, existing$identity_namespace)
  if (!is.na(match_index) && (existing$type_family[[match_index]] != family || existing$preparation_hash[[match_index]] != preparation_hash)) {
    issues <- rbind(issues, add("namespace_contract_conflict", "error", 1L, "The import contract conflicts with the existing namespace.", "Use the existing preparation or a new namespace/registry."))
  }
  list(rolled_back = FALSE, counts = counts, issues = issues)
}

sec_registry_import_apply <- function(con, registry_schema, source_schema, source_table,
                                      identifier_column, token_column, identity_namespace,
                                      rule, family, preparation_hash) {
  namespace_table <- sec_quote_table(con, registry_schema, "namespaces")
  DBI::dbExecute(
    con,
    paste("INSERT INTO", namespace_table, "(identity_namespace, type_family, normalization, validity_regex, preparation_hash) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (identity_namespace) DO NOTHING"),
    params = list(identity_namespace, family, rule$normalization[[1]], rule$validity_regex[[1]], preparation_hash)
  )
  source <- sec_quote_table(con, source_schema, source_table)
  id <- sec_quote_identifier(con, identifier_column)
  token <- sec_quote_identifier(con, token_column)
  prepared <- sec_identifier_expression(con, rule, id)
  entities <- sec_quote_table(con, registry_schema, "entities")
  aliases <- sec_quote_table(con, registry_schema, "aliases")
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", entities, " (entity_token) SELECT DISTINCT ", token,
    "::text FROM ", source, " ON CONFLICT (entity_token) DO NOTHING"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", aliases, " (identity_namespace, source_id, entity_token) SELECT DISTINCT ",
    sec_quote_literal(con, identity_namespace), ", ", prepared, ", ", token,
    "::text FROM ", source, " ON CONFLICT (identity_namespace, source_id) DO NOTHING"
  ))
  invisible(TRUE)
}

sec_registry_import_result <- function(status, mode, writes, audit) {
  structure(
    list(status = status, mode = mode, writes = writes, counts = audit$counts, issues = audit$issues),
    class = c("epi_sec_registry_import_result", "list")
  )
}
