#' Prepare data using a reviewed EDA specification
#'
#' Audit or apply deterministic, specification-guided missing-value, type and
#' level preparation. Audit mode never changes the returned data. Apply mode is
#' all-or-nothing: any blocking finding returns the original data and no
#' after-schema.
#'
#' @param data A data frame to assess or prepare.
#' @param spec An EDA specification data frame or CSV path accepted by
#'   [epi_eda_spec()]. If scaffold evidence includes `review_status`, every row
#'   must be `reviewed` before apply mode can proceed.
#' @param mode Either `"audit"` (the default) or `"apply"`.
#' @param unexpected_levels Either `"error"` (the default) or `"append"` for
#'   categorical variables. Binary variables never append a third level.
#' @param extra_variables Either `"keep"` (the default), `"error"`, or
#'   `"drop"`.
#'
#' @return An `epi_eda_preparation` list with fixed components `data`, `audit`,
#'   `schema_before`, `schema_after`, and `metadata`. Audits contain names,
#'   classes and counts but no observed values. `schema_after` is non-`NULL`
#'   only after a successful apply.
#'
#' @details Character numeric parsing is deliberately unsupported because no
#' locale or decimal-mark contract is present. Character dates and datetimes
#' use strict ISO forms. Offset or `Z` datetimes are normalised to UTC; local
#' datetimes require a valid reviewed `timezone` field supported by `clock`'s
#' packaged IANA timezone database. Ambiguous, nonexistent, unsupported or
#' otherwise unclassifiable local wall times block preparation without exposing
#' observed values. Optional `min` and `max` fields are descriptive metadata,
#' not recoding rules.
#'
#' The semicolon-delimited v1 `missing_codes` format cannot represent empty,
#' whitespace-only, or semicolon-containing sentinels. No files are written.
#'
#' @export
epi_eda_prepare <- function(data,
                            spec,
                            mode = c("audit", "apply"),
                            unexpected_levels = c("error", "append"),
                            extra_variables = c("keep", "error", "drop")) {
  mode <- match.arg(mode)
  unexpected_levels <- match.arg(unexpected_levels)
  extra_variables <- match.arg(extra_variables)
  prepare_validate_data(data)
  spec <- epi_eda_spec(spec)
  prepare_validate_names(spec$name, "specification")

  schema_before <- epi_eda_check_schema(data, spec)
  plan <- prepare_build_plan(data, spec, unexpected_levels, extra_variables)
  audit <- plan$audit
  blocking <- any(audit$status == "blocking")

  if (mode == "audit") {
    return(prepare_result(
      data, audit, schema_before, NULL,
      prepare_metadata(mode, "audited", data, NULL, audit)
    ))
  }

  if (blocking) {
    return(prepare_result(
      data, audit, schema_before, NULL,
      prepare_metadata(mode, "blocked", data, NULL, audit)
    ))
  }

  prepared <- prepare_apply_plan(data, spec, plan, extra_variables)
  if (nrow(prepared) != nrow(data) || !identical(row.names(prepared), row.names(data))) {
    stop("Internal preparation invariant failed before a complete result was returned.", call. = FALSE)
  }
  audit$status[audit$status == "planned"] <- "applied"
  schema_after <- epi_eda_check_schema(prepared, spec)
  present_spec <- schema_after$expected_present & schema_after$observed_present
  if (any(schema_after$type_status[present_spec] != "compatible")) {
    stop("Internal preparation schema reconciliation failed.", call. = FALSE)
  }

  prepare_result(
    prepared, audit, schema_before, schema_after,
    prepare_metadata(mode, "prepared", data, prepared, audit)
  )
}

prepare_result <- function(data, audit, schema_before, schema_after, metadata) {
  structure(
    list(
      data = data,
      audit = audit,
      schema_before = schema_before,
      schema_after = schema_after,
      metadata = metadata
    ),
    class = c("epi_eda_preparation", "list")
  )
}

prepare_validate_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.", call. = FALSE)
  }
  prepare_validate_names(names(data), "data")
  invisible(TRUE)
}

prepare_validate_names <- function(names, source) {
  empty <- is.na(names) | trimws(names) == ""
  if (any(empty)) {
    stop("Variable names in ", source, " must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(names)) {
    stop("Duplicate variable names in ", source, " are not supported.", call. = FALSE)
  }
  if (any(startsWith(names, ".dataset."))) {
    stop("Variable names using the reserved .dataset. audit prefix are not supported.", call. = FALSE)
  }
  invisible(TRUE)
}

prepare_build_plan <- function(data, spec, unexpected_levels, extra_variables) {
  rows <- prepare_dataset_rows(data, spec)
  column_plans <- vector("list", nrow(spec))
  names(column_plans) <- spec$name

  for (index in seq_len(nrow(spec))) {
    variable <- prepare_variable_plan(data, spec, index, unexpected_levels)
    rows <- c(rows, variable$rows)
    column_plans[[index]] <- variable
  }

  extra_names <- setdiff(names(data), spec$name)
  for (name in extra_names) {
    rows[[length(rows) + 1L]] <- prepare_extra_row(data, name, extra_variables)
  }

  list(
    audit = do.call(rbind, rows),
    columns = column_plans,
    extra_names = extra_names
  )
}

prepare_dataset_rows <- function(data, spec) {
  n <- nrow(data)
  duplicates <- as.integer(sum(duplicated(data)))
  zero_shape <- n == 0L || ncol(data) == 0L
  rows <- list(
    prepare_audit_row(
      ".dataset.dimensions", "dataset", action = "report_dimensions",
      status = "unchanged", n_total = n,
      reason = "Source dimensions were recorded without changing data."
    ),
    prepare_audit_row(
      ".dataset.duplicate_rows", "dataset", action = "report_duplicate_rows",
      status = if (duplicates > 0L) "warning" else "unchanged",
      n_total = n, n_affected = duplicates,
      reason = if (duplicates > 0L) {
        "Repeated rows after their first occurrence were counted and retained."
      } else {
        "No repeated rows after their first occurrence were found."
      }
    ),
    prepare_audit_row(
      ".dataset.zero_shape", "dataset", action = "report_zero_shape",
      status = if (zero_shape) "warning" else "unchanged",
      n_total = n, n_affected = as.integer(zero_shape),
      reason = if (zero_shape) {
        "The source has zero rows or zero columns; no observations were invented."
      } else {
        "The source has at least one row and one column."
      }
    ),
    prepare_audit_row(
      ".dataset.column_names", "dataset", action = "validate_column_names",
      status = "unchanged", n_total = n,
      reason = "Column names are unique, non-empty and outside the reserved audit namespace."
    )
  )

  if ("review_status" %in% names(spec)) {
    reviewed <- !is.na(spec$review_status) & as.character(spec$review_status) == "reviewed"
    n_unreviewed <- as.integer(sum(!reviewed))
    rows[[length(rows) + 1L]] <- prepare_audit_row(
      ".dataset.spec_review", "dataset", action = "verify_spec_review",
      status = if (n_unreviewed > 0L) "blocking" else "unchanged",
      n_total = nrow(spec), n_affected = n_unreviewed,
      reason = if (n_unreviewed > 0L) {
        "Scaffold evidence contains specification rows that are not explicitly reviewed."
      } else {
        "Every scaffold evidence row is explicitly reviewed."
      }
    )
  }
  rows
}

prepare_variable_plan <- function(data, spec, index, unexpected_levels) {
  row <- spec[index, , drop = FALSE]
  name <- row$name[[1]]
  type <- row$type[[1]]
  required <- if ("required" %in% names(row)) row$required[[1]] else NA
  present <- name %in% names(data)

  if (!present) {
    status <- if (isTRUE(required)) "blocking" else "skipped"
    rows <- list(prepare_audit_row(
      name, "presence", type, action = "assess_presence", status = status,
      n_total = nrow(data),
      reason = if (isTRUE(required)) {
        "A required specification variable is absent."
      } else {
        "An optional or unasserted specification variable is absent and was not created."
      }
    ))
    timezone <- prepare_timezone_value(row)
    if (type != "datetime" && nzchar(timezone)) {
      rows[[length(rows) + 1L]] <- prepare_audit_row(
        name, "type", type, action = "ignore_timezone", status = "warning",
        n_total = nrow(data),
        reason = "Timezone metadata was ignored because the declared type is not datetime."
      )
    }
    return(list(
      rows = rows,
      present = FALSE,
      levels = character()
    ))
  }

  values <- data[[name]]
  observed_class <- prepare_class(values)
  presence <- prepare_audit_row(
    name, "presence", type, observed_class, observed_class,
    "assess_presence", "unchanged", nrow(data),
    reason = "The specification variable is present."
  )

  if (!prepare_is_vector_column(values)) {
    skipped_missing <- prepare_audit_row(
      name, "missingness", type, observed_class, NA_character_,
      "assess_missingness", "skipped", nrow(data),
      reason = "Missingness preparation was skipped for unsupported or nested storage."
    )
    blocked_type <- prepare_audit_row(
      name, "type", type, observed_class, NA_character_,
      "convert_declared_type", "blocking", nrow(data),
      n_invalid = nrow(data), n_affected = nrow(data),
      reason = "Unsupported or nested storage cannot be converted under the declared type."
    )
    rows <- list(presence, skipped_missing, blocked_type)
    timezone <- prepare_timezone_value(row)
    if (type != "datetime" && nzchar(timezone)) {
      rows[[length(rows) + 1L]] <- prepare_audit_row(
        name, "type", type, observed_class, NA_character_,
        "ignore_timezone", "warning", nrow(data),
        reason = "Timezone metadata was ignored because the declared type is not datetime."
      )
    }
    levels_out <- character()
    if (type %in% c("categorical", "binary")) {
      level_plan <- prepare_levels_plan(
        values, rep(FALSE, length(values)), row, unexpected_levels, "blocking"
      )
      levels_out <- level_plan$levels
      rows[[length(rows) + 1L]] <- prepare_audit_row(
        name, "levels", type, observed_class, level_plan$class_after,
        level_plan$action, level_plan$status, nrow(data),
        n_invalid = 0L, n_unexpected = level_plan$n_unexpected,
        n_affected = level_plan$n_affected, n_changed = level_plan$n_changed,
        reason = level_plan$reason
      )
    }
    return(list(rows = rows, present = TRUE, levels = levels_out))
  }

  standard <- is.na(values)
  codes <- eda_missing_codes(spec, name)
  sentinel <- summary_missing_mask(values, codes) & !standard
  remaining <- !(standard | sentinel)
  n_standard <- as.integer(sum(standard))
  n_sentinel <- as.integer(sum(sentinel))

  missing_row <- prepare_audit_row(
    name, "missingness", type, observed_class, observed_class,
    if (n_sentinel > 0L) "replace_sentinel_missing" else "retain_missingness",
    if (n_sentinel > 0L) "planned" else "unchanged", nrow(data),
    n_standard, n_sentinel, 0L, 0L, n_sentinel, n_sentinel,
    if (n_sentinel > 0L) {
      "Declared sentinel observations will be replaced with typed missing values."
    } else {
      "No non-standard declared sentinel observations require replacement."
    }
  )

  type_plan <- prepare_type_plan(values, remaining, row)
  type_row <- prepare_audit_row(
    name, "type", type, observed_class, type_plan$class_after,
    type_plan$action, type_plan$status, nrow(data), n_standard, n_sentinel,
    type_plan$n_invalid, 0L, type_plan$n_affected, type_plan$n_changed,
    type_plan$reason
  )
  rows <- list(presence, missing_row, type_row)
  levels_out <- character()

  timezone <- prepare_timezone_value(row)
  if (type != "datetime" && nzchar(timezone)) {
    rows[[length(rows) + 1L]] <- prepare_audit_row(
      name, "type", type, observed_class, type_plan$class_after,
      "ignore_timezone", "warning", nrow(data), n_standard, n_sentinel,
      reason = "Timezone metadata was ignored because the declared type is not datetime."
    )
  }

  if (type %in% c("categorical", "binary")) {
    level_plan <- prepare_levels_plan(values, remaining, row, unexpected_levels, type_plan$status)
    levels_out <- level_plan$levels
    rows[[length(rows) + 1L]] <- prepare_audit_row(
      name, "levels", type, observed_class, level_plan$class_after,
      level_plan$action, level_plan$status, nrow(data), n_standard, n_sentinel,
      0L, level_plan$n_unexpected, level_plan$n_affected,
      level_plan$n_changed, level_plan$reason
    )
  }

  list(rows = rows, present = TRUE, levels = levels_out)
}

prepare_is_vector_column <- function(values) {
  is.null(dim(values)) && (is.atomic(values) || is.factor(values) || inherits(values, "POSIXlt"))
}

prepare_type_plan <- function(values, remaining, row) {
  type <- row$type[[1]]
  n <- length(values)
  n_remaining <- as.integer(sum(remaining))
  plain_numeric <- (is.double(values) || is.integer(values)) &&
    !inherits(values, c("Date", "POSIXt", "IDate")) && !is.object(values)

  if (type == "numeric") {
    if (plain_numeric) {
      change <- is.integer(values)
      return(prepare_type_result(
        if (change) "convert_integer_to_numeric" else "retain_numeric",
        if (change) "planned" else "unchanged", "numeric", 0L,
        if (change) n else 0L, if (change) n else 0L,
        if (change) "Integer storage will be converted to numeric without parsing." else "Numeric storage is already compatible."
      ))
    }
    return(prepare_type_result("convert_declared_type", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage cannot be converted to numeric without an unapproved parsing rule."))
  }

  if (type == "integer") {
    if (is.integer(values) && !inherits(values, "IDate") && !is.object(values)) {
      return(prepare_type_result("retain_integer", "unchanged", "integer", 0L, 0L, 0L, "Integer storage is already compatible."))
    }
    if (is.double(values) && !inherits(values, c("Date", "POSIXt")) && !is.object(values)) {
      observed <- values[remaining]
      invalid <- !is.finite(observed) | observed != trunc(observed) |
        observed < -.Machine$integer.max | observed > .Machine$integer.max
      n_invalid <- as.integer(sum(invalid))
      return(prepare_type_result(
        "convert_numeric_to_integer",
        if (n_invalid > 0L) "blocking" else "planned",
        if (n_invalid > 0L) NA_character_ else "integer", n_invalid,
        n_invalid, if (n_invalid > 0L) 0L else n,
        if (n_invalid > 0L) {
          "Numeric storage contains non-finite, non-whole or out-of-range observations."
        } else {
          "Finite whole numeric observations will be converted within the R integer range."
        }
      ))
    }
    return(prepare_type_result("convert_declared_type", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage cannot be converted to integer without an unapproved parsing rule."))
  }

  if (type %in% c("categorical", "binary")) {
    compatible <- is.factor(values) || is.character(values) || is.logical(values) ||
      ((is.integer(values) || is.double(values)) && !is.object(values))
    if (!compatible) {
      return(prepare_type_result("convert_to_factor", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage is not a supported categorical representation."))
    }
    logical_binary <- type == "binary" && is.logical(values) &&
      length(prepare_declared_levels(row)$levels) == 0L
    if (logical_binary) {
      return(prepare_type_result(
        "retain_logical_binary", "unchanged", "logical", 0L, 0L, 0L,
        "Logical binary storage uses the fixed FALSE and TRUE interpretation."
      ))
    }
    change <- !is.factor(values)
    prepare_type_result(
      "convert_to_factor", if (change) "planned" else "unchanged", "factor", 0L,
      if (change) n else 0L, if (change) n else 0L,
      if (change) "Supported atomic observations will be matched exactly to declared factor levels." else "Factor storage is supported; declared level metadata is assessed separately."
    )
  } else if (type == "text") {
    if (is.character(values) && !is.object(values)) {
      prepare_type_result("retain_text", "unchanged", "character", 0L, 0L, 0L, "Character storage is already compatible.")
    } else if (is.factor(values)) {
      prepare_type_result("convert_factor_to_text", "planned", "character", 0L, n, n, "Factor labels will be converted to character text.")
    } else {
      prepare_type_result("convert_declared_type", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage cannot be converted to text without implicit stringification.")
    }
  } else if (type == "date") {
    if (inherits(values, c("Date", "IDate"))) {
      prepare_type_result("retain_date", "unchanged", prepare_class(values), 0L, 0L, 0L, "Date storage is already compatible.")
    } else if (is.character(values)) {
      observed <- values[remaining]
      invalid <- !prepare_valid_iso_dates(observed)
      n_invalid <- as.integer(sum(invalid))
      prepare_type_result(
        "parse_iso_date", if (n_invalid > 0L) "blocking" else "planned",
        if (n_invalid > 0L) NA_character_ else "Date", n_invalid, n_invalid,
        if (n_invalid > 0L) 0L else n,
        if (n_invalid > 0L) "Character storage contains invalid strict ISO dates." else "Strict ISO date text will be converted to Date."
      )
    } else {
      prepare_type_result("parse_iso_date", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage is not a supported date representation.")
    }
  } else if (type == "datetime") {
    if (inherits(values, c("POSIXct", "POSIXlt"))) {
      prepare_type_result("retain_datetime", "unchanged", prepare_class(values), 0L, 0L, 0L, "Datetime storage and source timezone metadata are already compatible.")
    } else if (is.character(values)) {
      temporal <- prepare_datetime_plan(values[remaining], prepare_timezone_value(row))
      prepare_type_result(
        "parse_iso_datetime", if (temporal$n_invalid > 0L) "blocking" else "planned",
        if (temporal$n_invalid > 0L) NA_character_ else "POSIXct/POSIXt",
        temporal$n_invalid, temporal$n_invalid,
        if (temporal$n_invalid > 0L) 0L else n,
        temporal$reason
      )
    } else {
      prepare_type_result("parse_iso_datetime", "blocking", NA_character_, n_remaining, n_remaining, 0L, "Observed storage is not a supported datetime representation.")
    }
  } else {
    prepare_type_result("convert_declared_type", "blocking", NA_character_, n_remaining, n_remaining, 0L, "The declared type has no preparation implementation.")
  }
}

prepare_type_result <- function(action, status, class_after, n_invalid, n_affected, n_changed, reason) {
  list(
    action = action,
    status = status,
    class_after = class_after,
    n_invalid = as.integer(n_invalid),
    n_affected = as.integer(n_affected),
    n_changed = as.integer(n_changed),
    reason = reason
  )
}

prepare_levels_plan <- function(values, remaining, row, unexpected_levels, type_status) {
  type <- row$type[[1]]
  declared <- prepare_declared_levels(row)
  if (!declared$safe) {
    return(prepare_level_result("validate_declared_levels", "blocking", NA_character_, character(), 0L, 0L, 0L, "Declared level metadata is empty, duplicated or unsafe for the semicolon contract."))
  }
  levels <- declared$levels
  if (type == "categorical" && length(levels) == 0L) {
    return(prepare_level_result("validate_declared_levels", "blocking", NA_character_, levels, 0L, 0L, 0L, "Categorical preparation requires at least one declared level."))
  }
  logical_default <- type == "binary" && is.logical(values) && length(levels) == 0L
  if (logical_default) {
    levels <- c("FALSE", "TRUE")
  }
  if (type == "binary" && length(levels) != 2L) {
    return(prepare_level_result("validate_binary_levels", "blocking", NA_character_, levels, 0L, 0L, 0L, "Binary preparation requires exactly two distinct declared levels."))
  }
  if (type == "binary" && is.logical(values) && !identical(levels, c("FALSE", "TRUE"))) {
    return(prepare_level_result("validate_binary_levels", "blocking", NA_character_, levels, 0L, 0L, 0L, "Logical binary storage requires the fixed FALSE and TRUE level declaration."))
  }
  if (identical(type_status, "blocking")) {
    return(prepare_level_result("assess_unexpected_levels", "skipped", NA_character_, levels, 0L, 0L, 0L, "Level assessment was skipped because source storage is unsupported."))
  }

  observed <- as.character(values[remaining])
  unexpected <- setdiff(unique(observed), levels)
  n_unexpected <- as.integer(sum(observed %in% unexpected))
  if (n_unexpected == 0L) {
    if (logical_default) {
      return(prepare_level_result(
        "retain_logical_levels", "unchanged", "logical", levels,
        0L, 0L, 0L,
        "Logical binary observations use the fixed FALSE and TRUE interpretation."
      ))
    }
    same_factor <- is.factor(values) && identical(base::levels(values), levels) && !is.ordered(values)
    return(prepare_level_result(
      "declare_levels", if (same_factor) "unchanged" else "planned", "factor",
      levels, 0L, if (same_factor) 0L else length(values), if (same_factor) 0L else length(values),
      if (same_factor) "Observed factor levels already match the reviewed declaration." else "Reviewed factor levels will be applied in declared order."
    ))
  }
  if (type == "binary" || unexpected_levels == "error") {
    return(prepare_level_result(
      "reject_unexpected_levels", "blocking", NA_character_, levels,
      n_unexpected, n_unexpected, 0L,
      if (type == "binary") "Unexpected binary observations would violate the two-level declaration." else "Unexpected categorical observations require source or specification review."
    ))
  }

  appended <- sort(unique(unexpected), method = "radix")
  raw_appended_safe <- all(nzchar(appended)) && all(trimws(appended) == appended) && !any(grepl(";", appended, fixed = TRUE))
  if (!raw_appended_safe) {
    return(prepare_level_result("append_unexpected_levels", "blocking", NA_character_, levels, n_unexpected, n_unexpected, 0L, "Unexpected categorical metadata cannot be represented safely by the semicolon contract."))
  }
  prepare_level_result(
    "append_unexpected_levels", "warning", "factor", c(levels, appended),
    n_unexpected, n_unexpected, n_unexpected,
    "Unexpected categorical levels will be appended deterministically; prepared factor metadata diverge from the unchanged specification."
  )
}

prepare_level_result <- function(action, status, class_after, levels, n_unexpected, n_affected, n_changed, reason) {
  list(
    action = action,
    status = status,
    class_after = class_after,
    levels = levels,
    n_unexpected = as.integer(n_unexpected),
    n_affected = as.integer(n_affected),
    n_changed = as.integer(n_changed),
    reason = reason
  )
}

prepare_declared_levels <- function(row) {
  if (!("levels" %in% names(row)) || length(row$levels) == 0L ||
        is.na(row$levels[[1]]) || !nzchar(as.character(row$levels[[1]]))) {
    return(list(levels = character(), safe = TRUE))
  }
  raw_levels <- as.character(row$levels[[1]])
  pieces <- strsplit(raw_levels, ";", fixed = TRUE)[[1]]
  empty_boundary <- startsWith(raw_levels, ";") || endsWith(raw_levels, ";") ||
    grepl(";;", raw_levels, fixed = TRUE)
  safe <- !empty_boundary && all(nzchar(pieces)) &&
    all(trimws(pieces) == pieces) && !anyDuplicated(pieces)
  list(levels = pieces, safe = safe)
}

prepare_timezone_value <- function(row) {
  if (!("timezone" %in% names(row)) || length(row$timezone) == 0L || is.na(row$timezone[[1]])) {
    return("")
  }
  as.character(row$timezone[[1]])
}

prepare_valid_iso_dates <- function(values) {
  if (length(values) == 0L) {
    return(logical())
  }
  shape <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values)
  parsed <- suppressWarnings(as.Date(values, format = "%Y-%m-%d"))
  shape & !is.na(parsed) & format(parsed, "%Y-%m-%d") == values
}

prepare_datetime_plan <- function(values, timezone) {
  if (length(values) == 0L) {
    return(list(n_invalid = 0L, reason = "No observed datetime text requires parsing."))
  }
  shape <- prepare_datetime_shapes(values)
  valid_components <- prepare_valid_dt_components(values)
  invalid <- !(shape$offset | shape$z | shape$local) | !valid_components
  offset_rows <- shape$offset | shape$z
  if (any(offset_rows)) {
    parsed <- prepare_parse_offset_dt(values[offset_rows])
    invalid[offset_rows] <- invalid[offset_rows] | is.na(parsed)
  }
  if (any(shape$local)) {
    valid_timezone <- prepare_timezone_supported(timezone)
    if (!valid_timezone) {
      invalid[shape$local] <- TRUE
    } else {
      parsed <- prepare_parse_local_dt(values[shape$local], timezone)
      invalid[shape$local] <- invalid[shape$local] | is.na(parsed)
    }
  }
  n_invalid <- as.integer(sum(invalid))
  list(
    n_invalid = n_invalid,
    reason = if (n_invalid > 0L) {
      paste(
        "Character storage contains invalid, unzoned, ambiguous, nonexistent or unsupported strict ISO datetimes;",
        "provide an explicit Z or numeric offset or correct the reviewed IANA timezone."
      )
    } else {
      "Strict ISO datetime text will be converted and normalised to UTC."
    }
  )
}

prepare_timezone_supported <- function(timezone) {
  if (!nzchar(timezone)) {
    return(FALSE)
  }
  tryCatch(
    timezone %in% clock::tzdb_names(),
    error = function(...) FALSE
  )
}

prepare_valid_dt_components <- function(values) {
  normalized <- as.character(values)
  substr(normalized, 11L, 11L) <- "T"
  date_valid <- prepare_valid_iso_dates(substr(normalized, 1L, 10L))
  hour <- suppressWarnings(as.integer(substr(normalized, 12L, 13L)))
  minute <- suppressWarnings(as.integer(substr(normalized, 15L, 16L)))
  seconds <- suppressWarnings(as.numeric(sub(
    "^.{17}([0-9]{2}([.][0-9]+)?).*$", "\\1", normalized
  )))
  shape <- prepare_datetime_shapes(values)
  offset_valid <- rep(TRUE, length(values))
  if (any(shape$offset)) {
    suffix <- sub("^.*([+-][0-9]{2}:?[0-9]{2})$", "\\1", values[shape$offset])
    compact <- gsub(":", "", suffix, fixed = TRUE)
    offset_hour <- suppressWarnings(as.integer(substr(compact, 2L, 3L)))
    offset_minute <- suppressWarnings(as.integer(substr(compact, 4L, 5L)))
    offset_valid[shape$offset] <- !is.na(offset_hour) & offset_hour <= 14L &
      !is.na(offset_minute) & offset_minute <= 59L &
      (offset_hour < 14L | offset_minute == 0L)
  }
  date_valid & !is.na(hour) & hour >= 0L & hour <= 23L &
    !is.na(minute) & minute >= 0L & minute <= 59L &
    !is.na(seconds) & seconds >= 0 & seconds < 60 & offset_valid
}

prepare_parse_offset_dt <- function(values) {
  values <- as.character(values)
  local <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", values)
  pseudo <- suppressWarnings(as.POSIXct(
    strptime(local, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    tz = "UTC"
  ))
  offset_seconds <- rep(0, length(values))
  offset_rows <- grepl("[+-][0-9]{2}:?[0-9]{2}$", values)
  if (any(offset_rows)) {
    suffix <- sub("^.*([+-][0-9]{2}:?[0-9]{2})$", "\\1", values[offset_rows])
    compact <- gsub(":", "", suffix, fixed = TRUE)
    direction <- ifelse(substr(compact, 1L, 1L) == "-", -1, 1)
    hours <- as.integer(substr(compact, 2L, 3L))
    minutes <- as.integer(substr(compact, 4L, 5L))
    offset_seconds[offset_rows] <- direction * (hours * 3600 + minutes * 60)
  }
  out <- as.POSIXct(
    as.numeric(pseudo) - offset_seconds,
    origin = "1970-01-01", tz = "UTC"
  )
  attr(out, "tzone") <- "UTC"
  out
}

prepare_datetime_shapes <- function(values) {
  date <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  time <- "[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?"
  list(
    offset = grepl(paste0("^", date, "T", time, "[+-][0-9]{2}:?[0-9]{2}$"), values),
    z = grepl(paste0("^", date, "T", time, "Z$"), values),
    local = grepl(paste0("^", date, "[T ]", time, "$"), values)
  )
}

prepare_parse_local_dt <- function(values, timezone) {
  values <- as.character(values)
  out <- as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
  if (length(values) == 0L) {
    return(out)
  }
  normalized <- values
  substr(normalized, 11L, 11L) <- "T"
  suffix <- ifelse(nchar(normalized) > 19L, substr(normalized, 20L, nchar(normalized)), "")
  fractions <- suppressWarnings(as.numeric(paste0("0", suffix)))
  tryCatch(
    {
      naive <- suppressWarnings(clock::naive_time_parse(
        substr(normalized, 1L, 19L),
        format = "%Y-%m-%dT%H:%M:%S",
        precision = "second"
      ))
      info <- clock::naive_time_info(naive, zone = timezone)
      unique <- !is.na(info$type) & info$type == "unique" & !is.na(fractions)
      if (any(unique)) {
        converted <- clock::as_date_time(
          naive[unique],
          zone = timezone,
          nonexistent = "NA",
          ambiguous = "NA"
        )
        out[unique] <- as.numeric(converted) + fractions[unique]
      }
      attr(out, "tzone") <- "UTC"
      out
    },
    error = function(...) out
  )
}

prepare_parse_datetime <- function(values, timezone) {
  out <- as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = "UTC")
  missing <- is.na(values)
  if (all(missing)) {
    return(out)
  }
  shape <- prepare_datetime_shapes(values)
  offset_rows <- !missing & (shape$offset | shape$z)
  out[offset_rows] <- prepare_parse_offset_dt(values[offset_rows])
  local_rows <- !missing & shape$local
  if (any(local_rows)) {
    out[local_rows] <- prepare_parse_local_dt(values[local_rows], timezone)
  }
  attr(out, "tzone") <- "UTC"
  out
}

prepare_extra_row <- function(data, name, policy) {
  values <- data[[name]]
  unsupported <- !prepare_is_vector_column(values)
  status <- switch(
    policy,
    keep = if (unsupported) "warning" else "unchanged",
    error = "blocking",
    drop = "planned"
  )
  action <- paste0(policy, "_extra")
  reason <- switch(
    policy,
    keep = if (unsupported) {
      "An unsupported extra variable will be retained unchanged under the explicit keep policy."
    } else {
      "An extra variable will be retained unchanged under the explicit keep policy."
    },
    error = "An extra variable blocks preparation under the explicit error policy.",
    drop = "An extra variable will be removed under the explicit drop policy."
  )
  prepare_audit_row(
    name, "presence", observed_class_before = prepare_class(values),
    observed_class_after = if (policy == "drop") NA_character_ else prepare_class(values),
    action = action, status = status, n_total = nrow(data),
    n_affected = if (policy == "keep") 0L else nrow(data),
    n_changed = if (policy == "drop") nrow(data) else 0L,
    reason = reason
  )
}

prepare_apply_plan <- function(data, spec, plan, extra_variables) {
  output <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  row.names(output) <- row.names(data)
  for (index in seq_len(nrow(spec))) {
    name <- spec$name[[index]]
    column_plan <- plan$columns[[index]]
    if (!isTRUE(column_plan$present)) {
      next
    }
    output[[name]] <- prepare_apply_column(
      data[[name]], spec[index, , drop = FALSE], column_plan$levels,
      eda_missing_codes(spec, name)
    )
  }
  present_spec <- spec$name[spec$name %in% names(data)]
  retained_extra <- if (extra_variables == "keep") plan$extra_names else character()
  output <- output[c(present_spec, retained_extra)]
  as.data.frame(output, stringsAsFactors = FALSE, optional = TRUE)
}

prepare_apply_column <- function(values, row, levels, missing_codes) {
  type <- row$type[[1]]
  standard <- is.na(values)
  sentinel <- summary_missing_mask(values, missing_codes) & !standard
  if (type == "numeric") {
    out <- as.numeric(values)
    out[sentinel] <- NA_real_
    return(out)
  }
  if (type == "integer") {
    input <- values
    input[standard | sentinel] <- NA_real_
    out <- as.integer(input)
    return(out)
  }
  if (type %in% c("categorical", "binary")) {
    if (type == "binary" && is.logical(values) &&
          length(prepare_declared_levels(row)$levels) == 0L) {
      out <- values
      out[sentinel] <- NA
      return(out)
    }
    out <- as.character(values)
    out[standard | sentinel] <- NA_character_
    return(factor(out, levels = levels))
  }
  if (type == "text") {
    out <- as.character(values)
    out[standard | sentinel] <- NA_character_
    return(out)
  }
  if (type == "date") {
    if (inherits(values, c("Date", "IDate"))) {
      out <- values
      out[sentinel] <- NA
      return(out)
    }
    input <- as.character(values)
    input[standard | sentinel] <- NA_character_
    out <- as.Date(input, format = "%Y-%m-%d")
    return(out)
  }
  if (inherits(values, c("POSIXct", "POSIXlt"))) {
    out <- values
    out[sentinel] <- NA
    return(out)
  }
  input <- as.character(values)
  input[standard | sentinel] <- NA_character_
  prepare_parse_datetime(input, prepare_timezone_value(row))
}

prepare_audit_row <- function(name,
                              stage,
                              declared_type = NA_character_,
                              observed_class_before = NA_character_,
                              observed_class_after = NA_character_,
                              action,
                              status,
                              n_total = NA_integer_,
                              n_standard_missing = NA_integer_,
                              n_sentinel_missing = NA_integer_,
                              n_invalid = NA_integer_,
                              n_unexpected = NA_integer_,
                              n_affected = 0L,
                              n_changed = 0L,
                              reason) {
  data.frame(
    name = as.character(name),
    stage = as.character(stage),
    declared_type = as.character(declared_type),
    observed_class_before = as.character(observed_class_before),
    observed_class_after = as.character(observed_class_after),
    action = as.character(action),
    status = as.character(status),
    n_total = as.integer(n_total),
    n_standard_missing = as.integer(n_standard_missing),
    n_sentinel_missing = as.integer(n_sentinel_missing),
    n_invalid = as.integer(n_invalid),
    n_unexpected = as.integer(n_unexpected),
    n_affected = as.integer(n_affected),
    n_changed = as.integer(n_changed),
    reason = as.character(reason),
    stringsAsFactors = FALSE
  )
}

prepare_metadata <- function(mode, overall_status, before, after, audit) {
  counts <- table(factor(
    audit$status,
    levels = c("unchanged", "planned", "applied", "skipped", "warning", "blocking")
  ))
  data.frame(
    mode = mode,
    overall_status = overall_status,
    n_rows_before = as.integer(nrow(before)),
    n_columns_before = as.integer(ncol(before)),
    n_rows_after = if (is.null(after)) NA_integer_ else as.integer(nrow(after)),
    n_columns_after = if (is.null(after)) NA_integer_ else as.integer(ncol(after)),
    n_unchanged = as.integer(counts[["unchanged"]]),
    n_planned = as.integer(counts[["planned"]]),
    n_applied = as.integer(counts[["applied"]]),
    n_skipped = as.integer(counts[["skipped"]]),
    n_warning = as.integer(counts[["warning"]]),
    n_blocking = as.integer(counts[["blocking"]]),
    stringsAsFactors = FALSE
  )
}

prepare_class <- function(values) {
  paste(class(values), collapse = "/")
}
