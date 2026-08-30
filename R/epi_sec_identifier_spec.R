#' Define identifier preparation for PostgreSQL pseudonymisation
#'
#' Create a value-free contract describing how each linked identifier is prepared and validated before registry matching.
#'
#' @param linkage An object returned by [epi_sec_linkage_spec()].
#' @param rules `NULL` for exact identity preparation and fail-on-invalid behaviour, or a data frame with exactly `source_schema`, `source_table`, `normalization`, `validity_regex`, `invalid_policy` and `validity_column`.
#'
#' @return An `epi_sec_identifier_spec` containing sorted metadata-only `rules`, `contract_version` and `fingerprint_sha256`.
#'
#' @details Every linkage source table has exactly one rule. `normalization` is `identity`, `trim` or `trim_upper`. `validity_regex` is `NA` when no regular expression applies. `invalid_policy` is `fail` or `retain_and_flag`; the latter requires a non-empty `validity_column`, while `fail` requires `NA`. Omitting `rules` preserves released byte-exact matching behaviour.
#'
#' @export
#' @family longitudinal pseudonymisation
epi_sec_identifier_spec <- function(linkage, rules = NULL) {
  if (!inherits(linkage, "epi_sec_linkage_spec") ||
        !identical(names(linkage), c("tables", "columns", "record_keys", "crosswalks"))) {
    stop("linkage must be a current epi_sec_linkage_spec object.", call. = FALSE)
  }
  keys <- linkage$tables[c("source_schema", "source_table")]
  if (is.null(rules)) {
    rules <- transform(
      keys,
      normalization = "identity",
      validity_regex = NA_character_,
      invalid_policy = "fail",
      validity_column = NA_character_
    )
  }
  expected <- c(
    "source_schema", "source_table", "normalization", "validity_regex",
    "invalid_policy", "validity_column"
  )
  if (!is.data.frame(rules) || !identical(names(rules), expected)) {
    stop("rules must have exactly the documented six columns.", call. = FALSE)
  }
  rules <- as.data.frame(rules, stringsAsFactors = FALSE)
  if (!all(vapply(rules, is.character, logical(1)))) {
    stop("Every rules column must be character.", call. = FALSE)
  }
  key <- paste(rules$source_schema, rules$source_table, sep = "\r")
  expected_key <- paste(keys$source_schema, keys$source_table, sep = "\r")
  if (anyDuplicated(key) || !setequal(key, expected_key)) {
    stop("rules must cover every linkage source table exactly once.", call. = FALSE)
  }
  if (anyNA(rules$source_schema) || anyNA(rules$source_table) ||
        any(trimws(rules$source_schema) == "") || any(trimws(rules$source_table) == "")) {
    stop("rules source relations must be non-empty.", call. = FALSE)
  }
  if (anyNA(rules$normalization) ||
        any(!(rules$normalization %in% c("identity", "trim", "trim_upper")))) {
    stop("normalization must be identity, trim or trim_upper.", call. = FALSE)
  }
  regex_present <- !is.na(rules$validity_regex)
  if (any(regex_present & trimws(rules$validity_regex) == "")) {
    stop("validity_regex must be NA or non-empty.", call. = FALSE)
  }
  if (anyNA(rules$invalid_policy) ||
        any(!(rules$invalid_policy %in% c("fail", "retain_and_flag")))) {
    stop("invalid_policy must be fail or retain_and_flag.", call. = FALSE)
  }
  flag <- rules$invalid_policy == "retain_and_flag"
  if (any(flag & (is.na(rules$validity_column) | trimws(rules$validity_column) == "")) ||
        any(!flag & !is.na(rules$validity_column))) {
    stop("validity_column is required only for retain_and_flag.", call. = FALSE)
  }
  for (index in which(flag)) {
    rules$validity_column[[index]] <- eda_postgres_identifier(
      rules$validity_column[[index]], "rules validity_column"
    )
  }
  rules <- rules[match(expected_key, key), , drop = FALSE]
  rownames(rules) <- NULL
  contract <- list(
    rules = rules,
    contract_version = "pseudonym-identifiers-1"
  )
  contract$fingerprint_sha256 <- eda_postgres_fingerprint(contract)
  structure(contract, class = c("epi_sec_identifier_spec", "list"))
}

#' @export
print.epi_sec_identifier_spec <- function(x, ...) {
  cat("<epi_sec_identifier_spec>\n")
  cat("  Source tables: ", nrow(x$rules), "\n", sep = "")
  cat("  Identifier values: not present\n")
  invisible(x)
}

sec_identifier_validate <- function(identifiers, linkage) {
  if (!inherits(identifiers, "epi_sec_identifier_spec") ||
        !identical(names(identifiers), c("rules", "contract_version", "fingerprint_sha256")) ||
        !identical(identifiers$contract_version, "pseudonym-identifiers-1")) {
    stop("identifiers must be an unmodified epi_sec_identifier_spec object.", call. = FALSE)
  }
  rebuilt <- tryCatch(
    epi_sec_identifier_spec(linkage, identifiers$rules),
    error = function(error) NULL
  )
  if (is.null(rebuilt) || !identical(identifiers, rebuilt)) {
    stop("identifiers must be an unmodified epi_sec_identifier_spec object.", call. = FALSE)
  }
  invisible(TRUE)
}

sec_identifier_rule <- function(identifiers, declaration) {
  identifiers$rules[
    identifiers$rules$source_schema == declaration$source_schema &
      identifiers$rules$source_table == declaration$source_table, ,
    drop = FALSE
  ]
}

sec_identifier_expression <- function(con, rule, expression) {
  text <- paste0("(", expression, "::text COLLATE \"C\")")
  switch(
    rule$normalization[[1]],
    identity = text,
    trim = paste0("btrim(", text, ") COLLATE \"C\""),
    trim_upper = paste0("upper(btrim(", text, ")) COLLATE \"C\"")
  )
}
