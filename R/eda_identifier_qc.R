identifier_qc_columns <- function() {
  c("name", "expected_length", "pattern", "case_sensitive", "provenance")
}

#' Declare generic identifier-quality checks
#'
#' Validate portable, value-free identifier-QC metadata for
#' [epi_eda_identifier_qc()].  Identifier text is observed exactly as stored;
#' this contract never trims, changes case, pads, or recodes identifiers.
#'
#' @param identifiers A data frame or CSV path with exactly `name`,
#'   `expected_length`, `pattern`, `case_sensitive`, and `provenance`.
#'
#' @return A normalised identifier-QC declaration.
#' @family EDA
#' @export
epi_eda_identifier_qc_spec <- function(identifiers) {
  identifiers <- read_linkage_csv_or_data(identifiers, "identifiers")
  validate_linkage_columns(identifiers, identifier_qc_columns(), "identifiers")
  if (nrow(identifiers) < 1L) stop("identifiers must contain at least one row.", call. = FALSE)
  identifiers <- identifiers[identifier_qc_columns()]
  identifiers$name <- vapply(identifiers$name, eda_postgres_identifier, character(1), name = "identifiers name")
  if (anyDuplicated(identifiers$name)) stop("identifiers name values must be unique.", call. = FALSE)
  lengths <- suppressWarnings(as.integer(identifiers$expected_length))
  bad_length <- !is.na(identifiers$expected_length) &
    (is.na(lengths) | lengths < 1L | as.character(lengths) != as.character(identifiers$expected_length))
  if (any(bad_length)) stop("expected_length must be NA or a positive whole number.", call. = FALSE)
  identifiers$expected_length <- lengths
  identifiers$pattern <- as.character(identifiers$pattern)
  identifiers$pattern[is.na(identifiers$pattern) | trimws(identifiers$pattern) == ""] <- NA_character_
  if (!is.logical(identifiers$case_sensitive) || anyNA(identifiers$case_sensitive)) stop("case_sensitive must contain only TRUE or FALSE.", call. = FALSE)
  identifiers$provenance <- trimws(as.character(identifiers$provenance))
  if (any(is.na(identifiers$provenance) | identifiers$provenance == "")) stop("provenance must contain non-empty values.", call. = FALSE)
  rownames(identifiers) <- NULL
  identifiers
}

#' Audit identifiers in a PostgreSQL analytical relation
#'
#' Produce aggregate, value-free identifier diagnostics in a repeatable-read,
#' read-only PostgreSQL snapshot.  This is a pre-pseudonymisation QC primitive,
#' distinct from identity resolution and pseudonymisation.
#'
#' @param source An [epi_eda_postgres_source()].
#' @param identifiers A declaration returned by [epi_eda_identifier_qc_spec()],
#'   or the corresponding data frame/CSV path.
#'
#' @return An `epi_eda_identifier_qc` object with metadata, per-identifier
#'   aggregate diagnostics, availability patterns, and no identifier values.
#' @family EDA
#' @export
epi_eda_identifier_qc <- function(source, identifiers) {
  if (!inherits(source, "epi_eda_postgres_source")) stop("source must be an epi_eda_postgres_source.", call. = FALSE)
  spec <- if (is.data.frame(identifiers) || is.character(identifiers)) epi_eda_identifier_qc_spec(identifiers) else identifiers
  identifier_qc_validate_spec(spec)
  missing <- setdiff(spec$name, source$columns$name)
  if (length(missing) > 0L) stop("Every declared identifier must exist in source.", call. = FALSE)
  eda_postgres_transaction(source, {
    rows <- lapply(seq_len(nrow(spec)), function(index) identifier_qc_one(source, spec[index, , drop = FALSE]))
    audit <- do.call(rbind, rows)
    rownames(audit) <- NULL
    availability <- identifier_qc_availability(source, spec)
    structure(
      list(
        metadata = data.frame(
          contract_version = "identifier-qc-1",
          source_fingerprint_sha256 = eda_pg_source_fingerprint(source),
          specification_fingerprint_sha256 = eda_postgres_fingerprint(spec),
          n_identifiers = nrow(spec),
          stringsAsFactors = FALSE
        ),
        identifier_audit = audit,
        availability_audit = availability
      ),
      class = c("epi_eda_identifier_qc", "list")
    )
  })
}

#' @export
print.epi_eda_identifier_qc <- function(x, ...) {
  cat("<epi_eda_identifier_qc>\n")
  cat("  Identifiers: ", nrow(x$identifier_audit), "\n", sep = "")
  cat("  Identifier values: not returned\n")
  invisible(x)
}

#' Render an aggregate identifier-QC review bundle
#'
#' Write a new, simple HTML/CSV bundle from an [epi_eda_identifier_qc()] result.
#' The destination must not exist.  The renderer writes only aggregate audits
#' and never reconnects to PostgreSQL or queries identifiers.
#'
#' @param qc An object returned by [epi_eda_identifier_qc()].
#' @param output_dir A new local directory.
#'
#' @return The output directory invisibly.
#' @family EDA
#' @export
epi_eda_render_identifier_qc_report <- function(qc, output_dir) { # nolint: object_length_linter.
  if (!inherits(qc, "epi_eda_identifier_qc")) stop("qc must be an epi_eda_identifier_qc object.", call. = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (dir.exists(output_dir) || file.exists(output_dir)) stop("output_dir must not already exist.", call. = FALSE)
  parent <- dirname(output_dir)
  if (!dir.exists(parent)) stop("output_dir parent must already exist.", call. = FALSE)
  staging <- tempfile(pattern = ".identifier-qc-", tmpdir = parent)
  dir.create(staging)
  published <- FALSE
  on.exit(if (!published && dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
  utils::write.csv(qc$identifier_audit, file.path(staging, "identifier_qc.csv"), row.names = FALSE, na = "")
  utils::write.csv(qc$availability_audit, file.path(staging, "availability_qc.csv"), row.names = FALSE, na = "")
  utils::write.csv(qc$metadata, file.path(staging, "manifest.csv"), row.names = FALSE, na = "")
  summary_rows <- paste0("<tr><td>", identifier_qc_html_escape(qc$identifier_audit$name), "</td><td>", qc$identifier_audit$n_input, "</td><td>", qc$identifier_audit$n_nonblank, "</td><td>", qc$identifier_audit$n_distinct, "</td><td>", qc$identifier_audit$duplicate_excess, "</td></tr>", collapse = "")
  html <- paste0("<!doctype html><html><head><meta charset='utf-8'><title>Identifier QC</title></head><body><h1>Identifier QC</h1><p>Aggregate-only review bundle. Source identifiers are not included.</p><table><thead><tr><th>Identifier</th><th>Rows</th><th>Nonblank</th><th>Distinct</th><th>Duplicate excess</th></tr></thead><tbody>", summary_rows, "</tbody></table><p>See identifier_qc.csv, availability_qc.csv and manifest.csv.</p></body></html>")
  writeLines(html, file.path(staging, "index.html"), useBytes = TRUE)
  if (!file.rename(staging, output_dir)) stop("Identifier-QC report could not be published atomically.", call. = FALSE)
  published <- TRUE
  invisible(output_dir)
}

identifier_qc_validate_spec <- function(spec) {
  expected <- identifier_qc_columns()
  if (!is.data.frame(spec) || !identical(names(spec), expected) || !identical(spec, epi_eda_identifier_qc_spec(spec))) stop("identifiers must be an unmodified identifier-QC specification.", call. = FALSE)
  invisible(TRUE)
}

identifier_qc_one <- function(source, row) {
  id <- eda_postgres_column_sql(source, row$name[[1]])
  table <- eda_postgres_table_sql(source)
  pattern <- row$pattern[[1]]
  pattern_sql <- if (is.na(pattern)) "FALSE" else paste0("NOT (value ~ ", sec_quote_literal(source$con, pattern), ")")
  expected_sql <- if (is.na(row$expected_length[[1]])) "FALSE" else paste0("char_length(value) <> ", row$expected_length[[1]])
  query <- paste0("WITH values AS (SELECT ", id, "::text COLLATE \"C\" AS value FROM ", table, "), nonblank AS (SELECT value FROM values WHERE value IS NOT NULL AND btrim(value) <> ''), frequencies AS (SELECT value, COUNT(*)::bigint AS n FROM nonblank GROUP BY value), case_groups AS (SELECT lower(value) AS folded, COUNT(DISTINCT value)::bigint AS variants FROM nonblank GROUP BY lower(value)) SELECT COUNT(*)::bigint AS n_input, COUNT(*) FILTER (WHERE value IS NULL)::bigint AS n_null, COUNT(*) FILTER (WHERE value IS NOT NULL AND btrim(value) = '')::bigint AS n_blank, COUNT(*) FILTER (WHERE value IS NOT NULL AND btrim(value) <> '')::bigint AS n_nonblank, COUNT(DISTINCT value) FILTER (WHERE value IS NOT NULL AND btrim(value) <> '')::bigint AS n_distinct, (SELECT COALESCE(SUM(n - 1), 0)::bigint FROM frequencies WHERE n > 1) AS duplicate_excess, (SELECT COALESCE(MAX(n), 0)::bigint FROM frequencies) AS max_frequency, COUNT(*) FILTER (WHERE value ~ '^[[:space:]]')::bigint AS n_leading_whitespace, COUNT(*) FILTER (WHERE value ~ '[[:space:]]$')::bigint AS n_trailing_whitespace, COUNT(*) FILTER (WHERE btrim(value) ~ '[[:space:]]')::bigint AS n_internal_whitespace, COALESCE(MIN(char_length(value)) FILTER (WHERE value IS NOT NULL AND btrim(value) <> ''), 0)::bigint AS min_length, COALESCE(MAX(char_length(value)) FILTER (WHERE value IS NOT NULL AND btrim(value) <> ''), 0)::bigint AS max_length, COUNT(*) FILTER (WHERE value IS NOT NULL AND btrim(value) <> '' AND ", expected_sql, ")::bigint AS n_expected_length_violations, COUNT(*) FILTER (WHERE value IS NOT NULL AND btrim(value) <> '' AND ", pattern_sql, ")::bigint AS n_pattern_violations, (SELECT COUNT(*)::bigint FROM case_groups WHERE variants > 1) AS n_case_variations FROM values")
  observed <- DBI::dbGetQuery(source$con, query)
  numeric <- lapply(observed, function(value) as.numeric(value[[1]]))
  data.frame(name = row$name[[1]], expected_length = row$expected_length[[1]], pattern_declared = !is.na(pattern), case_sensitive = row$case_sensitive[[1]], provenance = row$provenance[[1]], as.data.frame(numeric, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
}

identifier_qc_availability <- function(source, spec) {
  if (nrow(spec) == 1L) return(data.frame(pattern = "only_identifier", n_rows = as.numeric(DBI::dbGetQuery(source$con, paste0("SELECT COUNT(*)::bigint AS n FROM ", eda_postgres_table_sql(source)))$n[[1]]), stringsAsFactors = FALSE))
  expressions <- vapply(spec$name, function(name) paste0("CASE WHEN ", eda_postgres_column_sql(source, name), " IS NOT NULL AND btrim(", eda_postgres_column_sql(source, name), "::text) <> '' THEN '1' ELSE '0' END"), character(1))
  query <- paste0("SELECT concat_ws('_', ", paste(expressions, collapse = ", "), ") AS pattern, COUNT(*)::bigint AS n_rows FROM ", eda_postgres_table_sql(source), " GROUP BY 1 ORDER BY 1")
  result <- DBI::dbGetQuery(source$con, query)
  result$pattern <- as.character(result$pattern)
  result$n_rows <- as.numeric(result$n_rows)
  result
}

identifier_qc_html_escape <- function(value) {
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  gsub("\"", "&quot;", value, fixed = TRUE)
}
