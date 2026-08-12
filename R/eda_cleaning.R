# Approved cleaning rules are an explicit executable contract separate from the
# descriptive EDA dictionary and pending QC proposals. This module validates
# that contract, applies only missing-code, bound and allowed-value operations,
# and publishes complete file or PostgreSQL outputs after aggregate
# reconciliation. It never returns source names, rule values or destination
# identity in its audit and display interfaces.

clean_rule_columns <- function() {
  c(
    "variable_key", "rule_state", "declared_type", "valid_min",
    "valid_max", "allowed_values", "missing_codes", "approval_id"
  )
}

#' Validate analyst-approved cleaning rules
#'
#' Create the executable rule object consumed by
#' [epi_eda_apply_cleaning_rules()]. This schema is separate from descriptive
#' EDA dictionaries and the pending result returned by
#' [epi_eda_qc_proposals()].
#'
#' @param rules A non-empty data frame containing exactly `variable_key`,
#'   `rule_state`, `declared_type`, `valid_min`, `valid_max`, `allowed_values`,
#'   `missing_codes`, and `approval_id` in that order. See Details.
#'
#' @return An `epi_eda_approved_rules` data frame in canonical opaque-key order.
#'
#' @details `rule_state` must be `"approved"`. Supported declared types are
#'   `numeric`, `integer`, `categorical`, and `binary`. Numeric and integer rules
#'   may contain finite `valid_min`/`valid_max` bounds and approved semicolon-
#'   delimited `missing_codes`; categorical and binary rules may contain exact
#'   semicolon-delimited `allowed_values` and `missing_codes`. Every row must
#'   define at least one operation. A populated binary allowed set contains
#'   exactly two values. Allowed and missing sets must not overlap.
#'
#'   `variable_key` must match `^var_[a-z0-9]{16,64}$`. `approval_id` is a
#'   caller-managed opaque reference matching
#'   `^approval_[a-z0-9]{16,64}$`; episcout validates the declared state and
#'   structure but does not authenticate the approving analyst. Pending
#'   proposal objects, descriptive dictionary bounds, unsupported types,
#'   contradictory bounds, non-finite numeric instructions, duplicate keys,
#'   empty list tokens, and extra or reordered fields are rejected.
#'
#' @export
epi_eda_approved_rules <- function(rules) {
  if (!is.data.frame(rules)) {
    stop("rules must be a data frame using the approved-rule schema.", call. = FALSE)
  }
  if (!identical(names(rules), clean_rule_columns())) {
    stop(
      "rules must contain exactly the approved-rule fields in the required order.",
      call. = FALSE
    )
  }
  rules <- as.data.frame(rules, stringsAsFactors = FALSE)
  if (nrow(rules) == 0L) {
    stop("rules must contain at least one approved rule.", call. = FALSE)
  }
  character_fields <- c(
    "variable_key", "rule_state", "declared_type", "allowed_values",
    "missing_codes", "approval_id"
  )
  if (!all(vapply(rules[character_fields], is.character, logical(1)))) {
    stop("Approved-rule character fields must use character vectors.", call. = FALSE)
  }
  if (!is.numeric(rules$valid_min) || !is.numeric(rules$valid_max)) {
    stop("Approved-rule bounds must use numeric vectors with typed NA when absent.", call. = FALSE)
  }
  if (anyNA(rules[character_fields])) {
    stop("Approved-rule character fields must not be missing.", call. = FALSE)
  }
  valid_keys <- validUTF8(rules$variable_key)
  valid_keys[valid_keys] <- grepl(
    "^var_[a-z0-9]{16,64}$",
    rules$variable_key[valid_keys]
  )
  if (!all(valid_keys) || anyDuplicated(rules$variable_key)) {
    stop("Approved rules require unique caller-managed opaque variable keys.", call. = FALSE)
  }
  if (!all(rules$rule_state == "approved")) {
    stop("Every executable rule must have rule_state equal to approved.", call. = FALSE)
  }
  supported <- c("numeric", "integer", "categorical", "binary")
  if (!all(rules$declared_type %in% supported)) {
    stop("Approved rules contain an unsupported declared type.", call. = FALSE)
  }
  valid_approvals <- validUTF8(rules$approval_id)
  valid_approvals[valid_approvals] <- grepl(
    "^approval_[a-z0-9]{16,64}$",
    rules$approval_id[valid_approvals]
  )
  if (!all(valid_approvals)) {
    stop("Approved rules require opaque approval identifiers matching the required pattern.", call. = FALSE)
  }

  normalised_allowed <- character(nrow(rules))
  normalised_missing <- character(nrow(rules))
  for (index in seq_len(nrow(rules))) {
    type <- rules$declared_type[[index]]
    allowed <- clean_rule_tokens(rules$allowed_values[[index]])
    missing <- clean_rule_tokens(rules$missing_codes[[index]])
    lower <- as.numeric(rules$valid_min[[index]])
    upper <- as.numeric(rules$valid_max[[index]])
    if (is.nan(lower) || is.nan(upper)) {
      stop("Approved-rule bounds must use typed NA when absent.", call. = FALSE)
    }

    if (type %in% c("numeric", "integer")) {
      if (length(allowed) > 0L) {
        stop("Numeric and integer approved rules must not contain allowed_values.", call. = FALSE)
      }
      invalid_lower <- !is.na(lower) && !is.finite(lower)
      invalid_upper <- !is.na(upper) && !is.finite(upper)
      if (invalid_lower || invalid_upper) {
        stop("Approved numeric bounds must be finite when present.", call. = FALSE)
      }
      if (!is.na(lower) && !is.na(upper) && lower > upper) {
        stop("Approved numeric bounds are contradictory.", call. = FALSE)
      }
      if (type == "integer") {
        present_bounds <- c(lower, upper)[!is.na(c(lower, upper))]
        whole_bounds <- present_bounds == trunc(present_bounds)
        exact_bounds <- abs(present_bounds) <= 9007199254740991
        if (!all(whole_bounds) || !all(exact_bounds)) {
          stop("Approved integer bounds must be exactly represented whole numbers.", call. = FALSE)
        }
      }
      missing <- clean_numeric_rule_tokens(missing, type)
    } else {
      if (!is.na(lower) || !is.na(upper)) {
        stop("Categorical and binary approved rules must not contain numeric bounds.", call. = FALSE)
      }
      if (type == "binary" && length(allowed) > 0L && length(allowed) != 2L) {
        stop("A populated binary allowed_values field must contain exactly two values.", call. = FALSE)
      }
      if (length(intersect(allowed, missing)) > 0L) {
        stop("Approved allowed values and missing codes are contradictory.", call. = FALSE)
      }
    }
    has_operation <- !is.na(lower) || !is.na(upper) ||
      length(allowed) > 0L || length(missing) > 0L
    if (!has_operation) {
      stop("Every approved rule must contain at least one executable operation.", call. = FALSE)
    }
    normalised_allowed[[index]] <- paste(allowed, collapse = ";")
    normalised_missing[[index]] <- paste(missing, collapse = ";")
  }

  rules$valid_min <- as.numeric(rules$valid_min)
  rules$valid_max <- as.numeric(rules$valid_max)
  rules$allowed_values <- normalised_allowed
  rules$missing_codes <- normalised_missing
  rules <- rules[order(rules$variable_key, method = "radix"), , drop = FALSE]
  row.names(rules) <- NULL
  class(rules) <- c("epi_eda_approved_rules", "data.frame")
  rules
}

clean_rule_tokens <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("Approved-rule list fields must be non-missing character values.", call. = FALSE)
  }
  if (!validUTF8(value)) {
    stop("Approved-rule list fields must use valid UTF-8 text.", call. = FALSE)
  }
  if (trimws(value) == "") {
    return(character())
  }
  tokens <- strsplit(value, ";", fixed = TRUE)[[1L]]
  tokens <- trimws(tokens)
  if (any(tokens == "") || anyDuplicated(tokens) || !all(validUTF8(tokens))) {
    stop("Approved-rule list fields contain malformed or duplicate values.", call. = FALSE)
  }
  sort(enc2utf8(tokens), method = "radix")
}

clean_numeric_rule_tokens <- function(tokens, type) {
  if (length(tokens) == 0L) {
    return(tokens)
  }
  values <- suppressWarnings(as.numeric(tokens))
  if (any(!is.finite(values))) {
    stop("Approved numeric missing codes must be finite numeric values.", call. = FALSE)
  }
  if (type == "integer") {
    syntax_ok <- grepl("^[+-]?[0-9]+$", tokens)
    whole_values <- values == trunc(values)
    exact_values <- abs(values) <= 9007199254740991
    if (!all(syntax_ok) || !all(whole_values) || !all(exact_values)) {
      stop("Approved integer missing codes must be exactly represented whole numbers.", call. = FALSE)
    }
    canonical <- sprintf("%.0f", values)
  } else {
    values[values == 0] <- 0
    canonical <- sprintf("%.17g", values)
  }
  if (anyDuplicated(canonical)) {
    stop("Approved numeric missing codes contain duplicate values.", call. = FALSE)
  }
  sort(canonical, method = "radix")
}

#' @export
print.epi_eda_approved_rules <- function(x, ...) {
  cat("<episcout approved cleaning rules>\n")
  cat("  approved rules: ", nrow(x), "\n", sep = "")
  cat("  rule values and approval references: <not displayed>\n")
  invisible(x)
}

#' @export
str.epi_eda_approved_rules <- function(object, ...) {
  cat("<episcout approved cleaning rules>\n")
  cat("  approved rules: ", nrow(object), "\n", sep = "")
  cat("  variable keys, rule values and approval references: <not displayed>\n")
  invisible(object)
}

#' Apply approved cleaning rules and materialise a complete processed output
#'
#' Apply separately approved bounds, allowed values, and missing-value codes to
#' a data frame or PostgreSQL source without modifying the source. Validation
#' and aggregate reconciliation complete before the function reports success.
#'
#' @param data A data frame or an [epi_eda_postgres_source()].
#' @param rules An unmodified object returned by [epi_eda_approved_rules()].
#' @param variable_keys A caller-owned data frame containing exactly `name` and
#'   `variable_key`. Every approved key must resolve exactly once. Additional
#'   unruled mappings are permitted.
#' @param output_path For a data-frame source, an optional new destination file.
#' @param output_format When `output_path` is supplied, exactly `"csv"` or
#'   `"rds"`. The format is never inferred. RData and Parquet are unsupported.
#' @param destination_schema For a PostgreSQL source, the existing schema in
#'   which to create the new destination table.
#' @param destination_table For a PostgreSQL source, the new table name.
#'
#' @return An `epi_eda_cleaning_result` list with `data` and `audit`. For a
#'   data-frame source, `data` is the complete processed data frame, including
#'   when it was also exported. For PostgreSQL, `data` is `NULL` because rows
#'   remain server-side. `audit` contains one aggregate `summary` row and an
#'   opaque-keyed `variables` table with missing counts before and after,
#'   observed-to-missing transition counts, dimensions, a deterministic rule
#'   hash, and reconciliation flags.
#'
#' @details Existing destinations are never replaced. File output is written to
#'   a same-directory staging file, reconciled, and published with a no-replace
#'   operation. PostgreSQL validates and creates one new table with server-side
#'   transformations in a repeatable-read transaction; any validation,
#'   transformation, reconciliation, or commit failure rolls the operation
#'   back. PostgreSQL relations have no physical row-order contract, but the
#'   projection does not filter or explicitly reorder rows.
#'
#'   Numeric values outside approved inclusive bounds become typed missing.
#'   Categorical and binary values outside a populated allowed set become typed
#'   missing. Approved missing codes become typed missing. Existing missing
#'   values remain missing. Data-frame row count, row order, row names, column
#'   order and vector storage are preserved. The source object or relation is
#'   never changed.
#'
#'   The returned audit and custom display methods omit source names, paths,
#'   relation identities, rule values, approval references, and row-level data.
#'   Aggregate counts and hashes can still be sensitive in context and require
#'   caller review before sharing.
#'
#' @export
epi_eda_apply_cleaning_rules <- function(data,
                                         rules,
                                         variable_keys,
                                         output_path = NULL,
                                         output_format = NULL,
                                         destination_schema = NULL,
                                         destination_table = NULL) {
  rules <- clean_revalidate_rules(rules)
  source_names <- clean_validate_variable_keys(variable_keys, rules$variable_key)
  rule_hash <- eda_postgres_fingerprint(clean_plain_rules(rules))

  if (inherits(data, "epi_eda_postgres_source")) {
    clean_validate_pg_arguments(
      output_path,
      output_format,
      destination_schema,
      destination_table
    )
    return(clean_apply_postgres(
      data,
      rules,
      source_names,
      rule_hash,
      destination_schema,
      destination_table
    ))
  }

  clean_validate_data_frame(data)
  publication <- clean_validate_file_arguments(
    data,
    output_path,
    output_format,
    destination_schema,
    destination_table
  )
  clean_apply_data_frame(
    data,
    rules,
    source_names,
    rule_hash,
    publication
  )
}

clean_plain_rules <- function(rules) {
  out <- rules
  class(out) <- "data.frame"
  out
}

clean_validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "data must be a data frame or an epi_eda_postgres_source.",
      call. = FALSE
    )
  }
  data_names <- names(data)
  valid_name_values <- !is.null(data_names) &&
    all(!is.na(data_names) & validUTF8(data_names))
  if (valid_name_values) {
    valid_name_values <- all(trimws(data_names) != "")
  }
  valid_names <- valid_name_values && !anyDuplicated(data_names)
  if (!valid_names) {
    stop("data must have unique non-missing column names.", call. = FALSE)
  }
  clean_checked_count(nrow(data))
  invisible(TRUE)
}

clean_checked_count <- function(value) {
  valid <- length(value) == 1L && !is.na(value) && is.numeric(value) &&
    is.finite(value) && value >= 0 && value == floor(value) &&
    value <= .Machine$integer.max
  if (!valid) {
    stop("Cleaning counts must fit the canonical R integer range.", call. = FALSE)
  }
  as.integer(value)
}

clean_revalidate_rules <- function(rules) {
  valid_class <- inherits(rules, "epi_eda_approved_rules") &&
    identical(class(rules), c("epi_eda_approved_rules", "data.frame"))
  if (!valid_class) {
    stop("rules must be an unmodified object returned by epi_eda_approved_rules().", call. = FALSE)
  }
  rebuilt <- tryCatch(
    epi_eda_approved_rules(clean_plain_rules(rules)),
    error = function(error) NULL
  )
  if (is.null(rebuilt) || !identical(rules, rebuilt)) {
    stop("rules must be an unmodified object returned by epi_eda_approved_rules().", call. = FALSE)
  }
  rules
}

clean_validate_variable_keys <- function(variable_keys, rule_keys) {
  valid_shape <- is.data.frame(variable_keys) &&
    identical(names(variable_keys), c("name", "variable_key")) &&
    is.character(variable_keys$name) &&
    is.character(variable_keys$variable_key)
  if (!valid_shape) {
    stop("variable_keys must contain exactly character name and variable_key fields.", call. = FALSE)
  }
  valid_names <- !is.na(variable_keys$name) & validUTF8(variable_keys$name)
  valid_names[valid_names] <- trimws(variable_keys$name[valid_names]) != ""
  valid_keys <- !is.na(variable_keys$variable_key) &
    validUTF8(variable_keys$variable_key)
  valid_keys[valid_keys] <- grepl(
    "^var_[a-z0-9]{16,64}$",
    variable_keys$variable_key[valid_keys]
  )
  unique_names <- !anyDuplicated(variable_keys$name)
  unique_keys <- !anyDuplicated(variable_keys$variable_key)
  if (!all(valid_names) || !all(valid_keys) || !unique_names || !unique_keys) {
    stop("variable_keys must contain unique names and opaque variable keys.", call. = FALSE)
  }
  matched <- match(rule_keys, variable_keys$variable_key)
  if (anyNA(matched)) {
    stop("variable_keys must resolve every approved rule exactly once.", call. = FALSE)
  }
  variable_keys$name[matched]
}

clean_validate_file_arguments <- function(data,
                                          output_path,
                                          output_format,
                                          destination_schema,
                                          destination_table) {
  if (!is.null(destination_schema) || !is.null(destination_table)) {
    stop("PostgreSQL destination arguments are not supported for a data-frame source.", call. = FALSE)
  }
  if (is.null(output_path) && is.null(output_format)) {
    return(list(kind = "memory", path = NULL))
  }
  if (is.null(output_path) || is.null(output_format)) {
    stop("output_path and output_format must be supplied together.", call. = FALSE)
  }
  valid_path <- is.character(output_path) && length(output_path) == 1L &&
    !is.na(output_path) && trimws(output_path) != ""
  if (!valid_path) {
    stop("output_path must be one non-empty character path.", call. = FALSE)
  }
  valid_format <- is.character(output_format) && length(output_format) == 1L &&
    !is.na(output_format) && output_format %in% c("csv", "rds")
  if (!valid_format) {
    stop("output_format must be exactly csv or rds.", call. = FALSE)
  }
  symlink_target <- Sys.readlink(output_path)
  is_symlink <- !is.na(symlink_target) && nzchar(symlink_target)
  if (file.exists(output_path) || dir.exists(output_path) || is_symlink) {
    stop("The destination file already exists and will not be replaced.", call. = FALSE)
  }
  if (!dir.exists(dirname(output_path))) {
    stop("The destination directory must already exist.", call. = FALSE)
  }
  if (output_format == "csv") {
    scalar <- vapply(
      data,
      function(value) is.atomic(value) && is.null(dim(value)),
      logical(1)
    )
    if (!all(scalar)) {
      stop("CSV output requires scalar atomic data-frame columns.", call. = FALSE)
    }
  }
  list(kind = output_format, path = output_path)
}

clean_validate_pg_arguments <- function(output_path,
                                        output_format,
                                        destination_schema,
                                        destination_table) {
  if (!is.null(output_path) || !is.null(output_format)) {
    stop("File output arguments are not supported for a PostgreSQL source.", call. = FALSE)
  }
  if (is.null(destination_schema) || is.null(destination_table)) {
    stop("PostgreSQL destination_schema and destination_table are both required.", call. = FALSE)
  }
  invisible(TRUE)
}

clean_rule_values <- function(rule, field) {
  clean_rule_tokens(as.character(rule[[field]][[1L]]))
}

clean_rule_missing_values <- function(rule) {
  values <- clean_rule_values(rule, "missing_codes")
  if (rule$declared_type[[1L]] %in% c("numeric", "integer")) {
    return(suppressWarnings(as.numeric(values)))
  }
  values
}

clean_memory_supported <- function(values, type, missing_codes) {
  classes <- class(values)
  base_class <- identical(classes, "integer") || identical(classes, "numeric")
  if (type == "numeric") {
    return(base_class)
  }
  if (type == "integer") {
    if (identical(classes, "integer")) {
      return(TRUE)
    }
    if (!identical(classes, "numeric")) {
      return(FALSE)
    }
    standard <- is.na(values)
    coded <- !standard & values %in% missing_codes
    observed <- values[!standard & !coded]
    return(all(is.finite(observed)) && all(observed == trunc(observed)))
  }
  allowed_classes <- list(
    "character", "logical", "integer", "factor", c("ordered", "factor")
  )
  any(vapply(allowed_classes, identical, logical(1), classes))
}

clean_memory_plans <- function(data, rules, source_names) {
  plans <- vector("list", nrow(rules))
  for (index in seq_len(nrow(rules))) {
    name <- source_names[[index]]
    if (!name %in% names(data)) {
      stop("Every approved rule must resolve to a present source variable.", call. = FALSE)
    }
    rule <- rules[index, , drop = FALSE]
    missing_codes <- clean_rule_missing_values(rule)
    if (!clean_memory_supported(data[[name]], rule$declared_type[[1L]], missing_codes)) {
      stop("An approved rule is incompatible with source storage.", call. = FALSE)
    }
    plans[[index]] <- list(
      name = name,
      rule = rule,
      allowed = clean_rule_values(rule, "allowed_values"),
      missing = missing_codes
    )
  }
  plans
}

clean_memory_transition <- function(values, plan) {
  standard <- is.na(values)
  type <- plan$rule$declared_type[[1L]]
  if (type %in% c("numeric", "integer")) {
    invalid <- values %in% plan$missing
    lower <- plan$rule$valid_min[[1L]]
    upper <- plan$rule$valid_max[[1L]]
    if (!is.na(lower)) {
      invalid <- invalid | values < lower
    }
    if (!is.na(upper)) {
      invalid <- invalid | values > upper
    }
  } else {
    text <- as.character(values)
    invalid <- text %in% plan$missing
    if (length(plan$allowed) > 0L) {
      invalid <- invalid | !(text %in% plan$allowed)
    }
  }
  invalid[is.na(invalid)] <- FALSE
  !standard & invalid
}

clean_assign_missing <- function(values, transition) {
  if (!any(transition)) {
    return(values)
  }
  if (is.factor(values)) {
    values[transition] <- NA
  } else if (is.character(values)) {
    values[transition] <- NA_character_
  } else if (is.integer(values)) {
    values[transition] <- NA_integer_
  } else if (is.double(values)) {
    values[transition] <- NA_real_
  } else if (is.logical(values)) {
    values[transition] <- NA
  } else {
    stop("An approved rule is incompatible with source storage.", call. = FALSE)
  }
  values
}

clean_apply_data_frame <- function(data,
                                   rules,
                                   source_names,
                                   rule_hash,
                                   publication) {
  plans <- clean_memory_plans(data, rules, source_names)
  processed <- as.data.frame(data, stringsAsFactors = FALSE)
  variable_rows <- vector("list", length(plans))

  for (index in seq_along(plans)) {
    plan <- plans[[index]]
    values <- processed[[plan$name]]
    standard <- is.na(values)
    transition <- clean_memory_transition(values, plan)
    updated <- clean_assign_missing(values, transition)
    processed[[plan$name]] <- updated
    variable_rows[[index]] <- clean_variable_audit_row(
      rules$variable_key[[index]],
      length(values),
      sum(standard),
      sum(is.na(updated)),
      sum(transition)
    )
  }

  variables <- clean_bind_variable_audit(variable_rows)
  clean_require_reconciliation(
    variables,
    nrow(data),
    ncol(data),
    nrow(processed),
    ncol(processed)
  )
  publication_dimensions <- c(nrow(processed), ncol(processed))
  if (!identical(publication$kind, "memory")) {
    publication_dimensions <- clean_publish_file(
      processed,
      publication$path,
      publication$kind
    )
  }
  summary <- clean_audit_summary(
    rule_hash,
    publication$kind,
    nrow(data),
    ncol(data),
    publication_dimensions[[1L]],
    publication_dimensions[[2L]],
    variables,
    publication_reconciled = TRUE
  )
  clean_result(processed, summary, variables)
}

clean_variable_audit_row <- function(key,
                                     n,
                                     missing_before,
                                     missing_after,
                                     transitioned) {
  n <- clean_checked_count(n)
  missing_before <- clean_checked_count(missing_before)
  missing_after <- clean_checked_count(missing_after)
  transitioned <- clean_checked_count(transitioned)
  reconciled <- missing_after == missing_before + transitioned &&
    missing_after <= n
  data.frame(
    variable_key = as.character(key),
    n = n,
    n_missing_before = missing_before,
    n_missing_after = missing_after,
    n_transitioned_to_missing = transitioned,
    reconciled = reconciled,
    stringsAsFactors = FALSE
  )
}

clean_bind_variable_audit <- function(rows) {
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

clean_require_reconciliation <- function(variables,
                                         source_rows,
                                         source_columns,
                                         destination_rows,
                                         destination_columns) {
  dimensions <- source_rows == destination_rows &&
    source_columns == destination_columns
  if (!isTRUE(dimensions) || !all(variables$reconciled)) {
    stop("Cleaning dimensions or missingness transitions failed reconciliation.", call. = FALSE)
  }
  invisible(TRUE)
}

clean_total_count <- function(value) {
  clean_checked_count(sum(as.double(value)))
}

clean_audit_summary <- function(rule_hash,
                                publication,
                                source_rows,
                                source_columns,
                                destination_rows,
                                destination_columns,
                                variables,
                                publication_reconciled) {
  data.frame(
    rule_set_sha256 = as.character(rule_hash),
    publication = as.character(publication),
    source_rows = clean_checked_count(source_rows),
    source_columns = clean_checked_count(source_columns),
    destination_rows = clean_checked_count(destination_rows),
    destination_columns = clean_checked_count(destination_columns),
    n_missing_before = clean_total_count(variables$n_missing_before),
    n_missing_after = clean_total_count(variables$n_missing_after),
    n_transitioned_to_missing = clean_total_count(
      variables$n_transitioned_to_missing
    ),
    dimensions_reconciled = source_rows == destination_rows &&
      source_columns == destination_columns,
    transitions_reconciled = all(variables$reconciled),
    publication_reconciled = isTRUE(publication_reconciled),
    stringsAsFactors = FALSE
  )
}

clean_result <- function(data, summary, variables) {
  structure(
    list(data = data, audit = list(summary = summary, variables = variables)),
    class = c("epi_eda_cleaning_result", "list")
  )
}

#' @export
print.epi_eda_cleaning_result <- function(x, ...) {
  cat("<episcout approved cleaning result>\n")
  cat("  publication: ", x$audit$summary$publication[[1L]], "\n", sep = "")
  cat("  rows: ", x$audit$summary$destination_rows[[1L]], "\n", sep = "")
  cat("  approved rules: ", nrow(x$audit$variables), "\n", sep = "")
  cat("  source, destination and rule values: <not displayed>\n")
  invisible(x)
}

#' @export
str.epi_eda_cleaning_result <- function(object, ...) {
  cat("<episcout approved cleaning result>\n")
  cat("  publication: ", object$audit$summary$publication[[1L]], "\n", sep = "")
  dimensions <- paste0(
    object$audit$summary$destination_rows[[1L]],
    " rows x ",
    object$audit$summary$destination_columns[[1L]],
    " columns"
  )
  cat("  dimensions: ", dimensions, "\n", sep = "")
  cat("  approved rules: ", nrow(object$audit$variables), "\n", sep = "")
  cat("  source, destination, keys and rule values: <not displayed>\n")
  invisible(object)
}

clean_file_action <- function(action, failure_message) {
  observed <- tryCatch(
    withCallingHandlers(
      list(ok = TRUE, value = force(action)),
      warning = function(condition) stop("file operation warning", call. = FALSE)
    ),
    error = function(error) list(ok = FALSE, value = NULL)
  )
  if (!isTRUE(observed$ok)) {
    stop(failure_message, call. = FALSE)
  }
  observed$value
}

clean_write_staged_file <- function(data, path, format) {
  if (format == "csv") {
    clean_file_action(
      utils::write.csv(data, path, row.names = FALSE, na = ""),
      "The processed CSV could not be written to staging."
    )
  } else {
    clean_file_action(
      saveRDS(data, path, version = 2L),
      "The processed RDS could not be written to staging."
    )
  }
  if (!file.exists(path)) {
    stop("The processed output was not created in staging.", call. = FALSE)
  }
  invisible(TRUE)
}

clean_staged_dimensions <- function(data, path, format) {
  if (format == "rds") {
    observed <- clean_file_action(
      readRDS(path),
      "The staged RDS could not be reconciled."
    )
    if (!identical(observed, data)) {
      stop("The staged RDS did not reconcile with the processed data.", call. = FALSE)
    }
  } else {
    observed <- clean_file_action(
      utils::read.csv(
        path,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        na.strings = character()
      ),
      "The staged CSV could not be reconciled."
    )
  }
  c(nrow(observed), ncol(observed))
}

clean_publish_file <- function(data, path, format) {
  stage <- tempfile(
    pattern = ".episcout-cleaning-stage-",
    tmpdir = dirname(path),
    fileext = paste0(".", format)
  )
  on.exit(if (file.exists(stage)) unlink(stage, force = TRUE), add = TRUE)
  clean_write_staged_file(data, stage, format)
  staged_dimensions <- clean_staged_dimensions(data, stage, format)
  if (!identical(as.integer(staged_dimensions), c(nrow(data), ncol(data)))) {
    stop("The staged processed output dimensions did not reconcile.", call. = FALSE)
  }
  linked <- clean_file_action(
    file.link(stage, path),
    "The processed output could not be published without replacement."
  )
  if (!isTRUE(linked)) {
    stop("The processed output could not be published without replacement.", call. = FALSE)
  }
  published <- tryCatch(
    clean_staged_dimensions(data, path, format),
    error = function(error) NULL
  )
  reconciled <- !is.null(published) &&
    identical(as.integer(published), c(nrow(data), ncol(data)))
  if (!reconciled) {
    removed <- unlink(path, force = FALSE) == 0L
    if (!removed) {
      stop("Publication reconciliation failed and the new destination could not be removed safely.", call. = FALSE)
    }
    stop("The published processed output failed reconciliation and was removed.", call. = FALSE)
  }
  as.integer(published)
}

clean_pg_quote_table <- function(con, schema, table) {
  as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = schema, table = table)
  ))
}

clean_pg_standard_missing <- function(source, column) {
  column_sql <- eda_postgres_column_sql(source, column$name[[1L]])
  if (eda_postgres_storage_family(column) == "numeric") {
    return(paste0("(", column_sql, " IS NULL OR ", column_sql, "::text = 'NaN')"))
  }
  paste0("(", column_sql, " IS NULL)")
}

clean_pg_compatible <- function(column, type) {
  if (is.null(column) || identical(as.character(column$typtype[[1L]]), "d")) {
    return(FALSE)
  }
  family <- eda_postgres_storage_family(column)
  deterministic <- isTRUE(column$collation_deterministic[[1L]])
  if (type == "numeric") {
    return(family %in% c("integer", "numeric"))
  }
  if (type == "integer") {
    return(family == "integer")
  }
  family %in% c("text", "enum", "boolean", "integer") &&
    (!family %in% c("text", "enum") || deterministic)
}

clean_pg_numeric_cast <- function(column) {
  exact <- eda_postgres_storage_family(column) == "integer" ||
    identical(as.character(column$base_udt_name[[1L]]), "numeric")
  if (exact) "numeric" else "double precision"
}

clean_pg_add_parameter <- function(predicates,
                                   params,
                                   value,
                                   cast,
                                   expression,
                                   operator,
                                   offset) {
  params[[length(params) + 1L]] <- value
  placeholder <- paste0("$", offset + length(params), "::", cast)
  list(
    predicates = c(predicates, paste(expression, operator, placeholder)),
    params = params
  )
}

clean_pg_rule_plan <- function(source, rule, name, offset = 0L) {
  column <- eda_postgres_column(source, name)
  type <- rule$declared_type[[1L]]
  if (!clean_pg_compatible(column, type)) {
    stop("An approved rule is incompatible with PostgreSQL source storage.", call. = FALSE)
  }
  column_sql <- eda_postgres_column_sql(source, name)
  standard <- clean_pg_standard_missing(source, column)
  allowed <- clean_rule_values(rule, "allowed_values")
  missing <- clean_rule_missing_values(rule)
  predicates <- character()
  params <- list()

  if (length(missing) > 0L) {
    if (type %in% c("numeric", "integer")) {
      cast <- clean_pg_numeric_cast(column)
      for (value in missing) {
        updated <- clean_pg_add_parameter(
          predicates,
          params,
          value,
          cast,
          column_sql,
          "=",
          offset
        )
        predicates <- updated$predicates
        params <- updated$params
      }
    } else {
      expression <- eda_postgres_value_expression(source, column, type)
      for (value in missing) {
        updated <- clean_pg_add_parameter(
          predicates,
          params,
          value,
          "text",
          expression,
          "=",
          offset
        )
        predicates <- updated$predicates
        params <- updated$params
      }
    }
  }
  if (type %in% c("numeric", "integer")) {
    cast <- clean_pg_numeric_cast(column)
    if (!is.na(rule$valid_min[[1L]])) {
      updated <- clean_pg_add_parameter(
        predicates,
        params,
        rule$valid_min[[1L]],
        cast,
        column_sql,
        "<",
        offset
      )
      predicates <- updated$predicates
      params <- updated$params
    }
    if (!is.na(rule$valid_max[[1L]])) {
      updated <- clean_pg_add_parameter(
        predicates,
        params,
        rule$valid_max[[1L]],
        cast,
        column_sql,
        ">",
        offset
      )
      predicates <- updated$predicates
      params <- updated$params
    }
  } else if (length(allowed) > 0L) {
    expression <- eda_postgres_value_expression(source, column, type)
    placeholders <- character(length(allowed))
    for (index in seq_along(allowed)) {
      params[[length(params) + 1L]] <- allowed[[index]]
      placeholders[[index]] <- paste0("$", offset + length(params), "::text")
    }
    predicates[[length(predicates) + 1L]] <- paste0(
      "NOT (", expression, " IN (", paste(placeholders, collapse = ", "), "))"
    )
  }
  transition <- paste0(
    "(NOT ", standard, " AND (", paste(predicates, collapse = " OR "), "))"
  )
  list(
    name = name,
    key = rule$variable_key[[1L]],
    column_sql = column_sql,
    standard = standard,
    transition = transition,
    params = params
  )
}

clean_pg_fetch <- function(con, statement, params = list(), kind) {
  eda_db_fetch(
    con,
    statement,
    params = params,
    query_kind = kind,
    limit = 1L
  )
}

clean_pg_execute <- function(con, statement, params = list(), kind) {
  observed <- eda_db_observe_conditions({
    if (length(params) == 0L) {
      DBI::dbExecute(con, statement)
    } else {
      DBI::dbExecute(con, statement, params = params)
    }
  })
  eda_db_signal_conditions(observed, "cleaning operation")
  if (inherits(observed$value, "error")) {
    stop(
      "PostgreSQL cleaning failed during ", kind,
      "; review restricted database logs.",
      call. = FALSE
    )
  }
  invisible(observed$value)
}

clean_pg_destination_state <- function(con, schema, table) {
  state <- clean_pg_fetch(
    con,
    paste(
      "SELECT",
      "EXISTS (SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = $1) AS schema_exists,",
      "EXISTS (SELECT 1 FROM pg_catalog.pg_class AS c",
      "INNER JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace",
      "WHERE n.nspname = $1 AND c.relname = $2) AS relation_exists"
    ),
    params = list(schema, table),
    kind = "cleaning_destination_state"
  )
  valid <- identical(names(state), c("schema_exists", "relation_exists")) &&
    nrow(state) == 1L && is.logical(state$schema_exists) &&
    is.logical(state$relation_exists)
  if (!valid) {
    stop("PostgreSQL destination validation returned an invalid scalar schema.", call. = FALSE)
  }
  state
}

clean_pg_source_audit <- function(source, plan) {
  observed <- clean_pg_fetch(
    source$con,
    paste0(
      "SELECT count(*) FILTER (WHERE ", plan$standard,
      ")::text AS n_missing_before, count(*) FILTER (WHERE ",
      plan$transition, ")::text AS n_transitioned FROM ",
      eda_postgres_table_sql(source)
    ),
    params = plan$params,
    kind = "cleaning_source_audit"
  )
  if (!identical(names(observed), c("n_missing_before", "n_transitioned"))) {
    stop("PostgreSQL cleaning audit returned an invalid scalar schema.", call. = FALSE)
  }
  c(
    n_missing_before = eda_checked_count(
      observed$n_missing_before[[1L]],
      "PostgreSQL cleaning count"
    ),
    n_transitioned = eda_checked_count(
      observed$n_transitioned[[1L]],
      "PostgreSQL cleaning count"
    )
  )
}

clean_pg_destination_missing <- function(con, table_sql, standard) {
  observed <- clean_pg_fetch(
    con,
    paste0(
      "SELECT count(*) FILTER (WHERE ", standard,
      ")::text AS n_missing_after FROM ", table_sql
    ),
    kind = "cleaning_destination_audit"
  )
  if (!identical(names(observed), "n_missing_after")) {
    stop("PostgreSQL cleaning audit returned an invalid scalar schema.", call. = FALSE)
  }
  eda_checked_count(
    observed$n_missing_after[[1L]],
    "PostgreSQL cleaning count"
  )
}

clean_pg_destination_catalogue <- function(con, schema, table) {
  eda_postgres_catalogue(con, schema, table)
}

clean_pg_combined_plans <- function(source, rules, source_names) {
  plans <- vector("list", nrow(rules))
  offset <- 0L
  for (index in seq_len(nrow(rules))) {
    plans[[index]] <- clean_pg_rule_plan(
      source,
      rules[index, , drop = FALSE],
      source_names[[index]],
      offset
    )
    offset <- offset + length(plans[[index]]$params)
  }
  plans
}

clean_pg_create_statement <- function(source, destination_sql, plans) {
  plan_names <- vapply(plans, `[[`, character(1), "name")
  fields <- vapply(source$columns$name, function(name) {
    column_sql <- eda_postgres_column_sql(source, name)
    index <- match(name, plan_names)
    if (is.na(index)) {
      return(column_sql)
    }
    paste0(
      "CASE WHEN ", plans[[index]]$transition,
      " THEN NULL ELSE ", column_sql, " END AS ", column_sql
    )
  }, character(1))
  paste0(
    "CREATE TABLE ", destination_sql, " AS SELECT ",
    paste(fields, collapse = ", "), " FROM ",
    eda_postgres_table_sql(source)
  )
}

clean_apply_postgres <- function(source,
                                 rules,
                                 source_names,
                                 rule_hash,
                                 destination_schema,
                                 destination_table) {
  eda_validate_postgres_source(source, require_idle = TRUE)
  destination_schema <- eda_postgres_identifier(
    destination_schema,
    "destination_schema"
  )
  destination_table <- eda_postgres_identifier(
    destination_table,
    "destination_table"
  )
  system_schema <- destination_schema %in% c("pg_catalog", "information_schema") ||
    grepl("^pg_(temp|toast)", destination_schema)
  if (system_schema) {
    stop("The PostgreSQL destination must use a caller-owned permanent schema.", call. = FALSE)
  }
  same_destination <- identical(destination_schema, source$schema) &&
    identical(destination_table, source$relation)
  if (same_destination) {
    stop("The PostgreSQL destination must differ from the source relation.", call. = FALSE)
  }

  con <- source$con
  eda_db_lifecycle_call(
    eda_db_begin(con),
    "PostgreSQL cleaning transaction could not begin; review restricted database logs."
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
  clean_pg_execute(
    con,
    "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ",
    kind = "transaction setup"
  )
  eda_validate_postgres_source(source, require_idle = FALSE)
  destination_state <- clean_pg_destination_state(
    con,
    destination_schema,
    destination_table
  )
  if (!isTRUE(destination_state$schema_exists[[1L]])) {
    stop("The PostgreSQL destination schema does not exist.", call. = FALSE)
  }
  if (isTRUE(destination_state$relation_exists[[1L]])) {
    stop("The PostgreSQL destination already exists and will not be replaced.", call. = FALSE)
  }

  local_plans <- lapply(seq_len(nrow(rules)), function(index) {
    clean_pg_rule_plan(
      source,
      rules[index, , drop = FALSE],
      source_names[[index]],
      0L
    )
  })
  source_rows <- eda_postgres_row_count(source)
  before <- lapply(
    local_plans,
    function(plan) clean_pg_source_audit(source, plan)
  )
  combined_plans <- clean_pg_combined_plans(source, rules, source_names)
  destination_sql <- clean_pg_quote_table(
    con,
    destination_schema,
    destination_table
  )
  create_statement <- clean_pg_create_statement(
    source,
    destination_sql,
    combined_plans
  )
  create_params <- unlist(
    lapply(combined_plans, `[[`, "params"),
    recursive = FALSE,
    use.names = FALSE
  )
  clean_pg_execute(
    con,
    create_statement,
    params = create_params,
    kind = "destination creation"
  )

  destination_catalogue <- clean_pg_destination_catalogue(
    con,
    destination_schema,
    destination_table
  )
  destination_rows <- clean_pg_fetch(
    con,
    paste0("SELECT count(*)::text AS n FROM ", destination_sql),
    kind = "cleaning_destination_dimensions"
  )
  destination_rows <- eda_checked_count(
    destination_rows$n[[1L]],
    "PostgreSQL cleaning destination row count"
  )
  destination_columns <- nrow(destination_catalogue$columns)
  same_columns <- identical(
    as.character(destination_catalogue$columns$name),
    as.character(source$columns$name)
  )
  if (!same_columns) {
    stop("PostgreSQL cleaning destination columns failed reconciliation.", call. = FALSE)
  }

  variable_rows <- vector("list", length(local_plans))
  for (index in seq_along(local_plans)) {
    missing_after <- clean_pg_destination_missing(
      con,
      destination_sql,
      local_plans[[index]]$standard
    )
    variable_rows[[index]] <- clean_variable_audit_row(
      rules$variable_key[[index]],
      source_rows,
      before[[index]][["n_missing_before"]],
      missing_after,
      before[[index]][["n_transitioned"]]
    )
  }
  variables <- clean_bind_variable_audit(variable_rows)
  clean_require_reconciliation(
    variables,
    source_rows,
    nrow(source$columns),
    destination_rows,
    destination_columns
  )
  summary <- clean_audit_summary(
    rule_hash,
    "postgresql",
    source_rows,
    nrow(source$columns),
    destination_rows,
    destination_columns,
    variables,
    publication_reconciled = TRUE
  )
  eda_db_lifecycle_call(
    eda_db_commit(con),
    "PostgreSQL cleaning transaction could not commit safely; review restricted database logs."
  )
  finished <- TRUE
  clean_result(NULL, summary, variables)
}
