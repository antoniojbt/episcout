#' Generate reviewable QC evidence and pending cleaning-rule proposals
#'
#' Profile aggregate quality-control evidence without changing observations or the reviewed EDA specification. Deterministic proposal fields are prompts for analyst review only; they are neither approved nor executable rules.
#'
#' @param data A data frame or an [epi_eda_postgres_source()] containing the variables declared by `spec`.
#' @param spec An EDA specification data frame or CSV path accepted by [epi_eda_spec()].
#' @param variable_keys A data frame containing exactly `name` and `variable_key`. Names must cover the normalised specification exactly once. Keys are caller-created, persisted opaque identifiers matching `^var_[a-z0-9]{16,64}$` and must be unique.
#'
#' @return An `epi_eda_qc_proposals` list containing exactly `evidence` and `proposals`. `evidence` has one row per specification row and the columns `variable_key`, `evidence_state`, `declared_type`, `profile_status`, `evidence_code`, `n`, `n_missing`, `n_observed`, `n_unique`, `n_infinite`, `n_finite`, `observed_min`, `observed_max`, `tukey_lower_fence`, `tukey_upper_fence`, `n_below_tukey`, and `n_above_tukey`. `proposals` has zero or one pending row per variable and the columns `variable_key`, `proposal_state`, `candidate_type`, `units_review_required`, `candidate_units`, `candidate_screening_min`, `candidate_screening_max`, `screening_basis`, `candidate_allowed_levels`, `candidate_missing_codes`, and `rationale_codes`.
#'
#' @details Source names are used only to look up caller-managed keys and are not returned. Observed extrema and Tukey 1.5-IQR fences are descriptive signals from the profiled snapshot, not scientific validity limits. An integral numeric or integer variable with exactly both zero and one observed may receive a pending binary candidate, but its reviewed type and values are never changed. Units and missing-value codes are never inferred.
#'
#'   The reviewed specification remains authoritative for `analysis_type`, `units`, `levels`, `min`, `max`, and `missing_codes`. This function does not approve or apply rules, clean data, return a modified specification, or write files or database objects. Aggregate results can still be sensitive in context and require caller review before saving or sharing.
#'
#' @export
epi_eda_qc_proposals <- function(data, spec, variable_keys) {
  spec <- qc_normalise_spec(spec)
  keys <- qc_validate_variable_keys(variable_keys, spec)

  if (inherits(data, "epi_eda_postgres_source")) {
    evidence <- eda_postgres_transaction(
      data,
      eda_pg_qc_evidence_inside(data, spec, keys)
    )
  } else {
    qc_validate_data_frame(data)
    evidence <- qc_data_frame_evidence(data, spec, keys)
  }

  qc_result(evidence, qc_pending_proposals(evidence, spec))
}

#' @export
print.epi_eda_qc_proposals <- function(x, ...) {
  cat("<episcout reviewable QC proposals>\n")
  cat("  evidence rows: ", nrow(x$evidence), "\n", sep = "")
  cat("  pending proposals: ", nrow(x$proposals), "\n", sep = "")
  invisible(x)
}

#' @export
str.epi_eda_qc_proposals <- function(object, ...) {
  cat("<episcout reviewable QC proposals>\n")
  cat("  evidence rows: ", nrow(object$evidence), "\n", sep = "")
  cat("  pending proposals: ", nrow(object$proposals), "\n", sep = "")
  cat("  source names and key crosswalk: <not returned>\n")
  invisible(object)
}

qc_normalise_spec <- function(spec) {
  tryCatch(
    epi_eda_spec(spec),
    error = function(error) {
      stop(
        "spec must be a valid EDA specification accepted by epi_eda_spec().",
        call. = FALSE
      )
    }
  )
}

qc_validate_variable_keys <- function(variable_keys, spec) {
  if (!is.data.frame(variable_keys)) {
    stop("variable_keys must be a data frame.", call. = FALSE)
  }
  if (!identical(names(variable_keys), c("name", "variable_key"))) {
    stop(
      "variable_keys must contain exactly name and variable_key in that order.",
      call. = FALSE
    )
  }
  if (!is.character(variable_keys$name)) {
    stop("variable_keys$name must be character.", call. = FALSE)
  }
  if (!is.character(variable_keys$variable_key)) {
    stop("variable_keys$variable_key must be character.", call. = FALSE)
  }
  if (any(is.na(variable_keys$name) | trimws(variable_keys$name) == "")) {
    stop("variable_keys$name must contain non-missing lookup names.", call. = FALSE)
  }
  if (anyDuplicated(variable_keys$name)) {
    stop("variable_keys$name must be unique.", call. = FALSE)
  }
  valid_keys <- !is.na(variable_keys$variable_key) &
    grepl("^var_[a-z0-9]{16,64}$", variable_keys$variable_key)
  if (!all(valid_keys)) {
    stop(
      "variable_keys$variable_key must contain caller-managed opaque identifiers matching the required pattern.",
      call. = FALSE
    )
  }
  if (anyDuplicated(variable_keys$variable_key)) {
    stop("variable_keys$variable_key must be unique.", call. = FALSE)
  }
  if (!setequal(variable_keys$name, spec$name) || nrow(variable_keys) != nrow(spec)) {
    stop(
      "variable_keys$name must cover the normalised specification exactly once.",
      call. = FALSE
    )
  }

  variable_keys$variable_key[match(spec$name, variable_keys$name)]
}

qc_validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "data must be a data frame or an epi_eda_postgres_source.",
      call. = FALSE
    )
  }
  valid_names <- !is.null(names(data)) &&
    !any(is.na(names(data)) | trimws(names(data)) == "") &&
    !anyDuplicated(names(data))
  if (!valid_names) {
    stop("data must have unique non-missing column names.", call. = FALSE)
  }
  qc_checked_count(nrow(data))
  invisible(TRUE)
}

qc_checked_count <- function(value) {
  valid <- length(value) == 1L && !is.na(value) && is.numeric(value) &&
    is.finite(value) && value >= 0 && value == floor(value) &&
    value <= .Machine$integer.max
  if (!valid) {
    stop("QC counts must fit the canonical R integer range.", call. = FALSE)
  }
  as.integer(value)
}

qc_identifier_role <- function(role) {
  trimws(tolower(as.character(role))) %in% c("id", "identifier")
}

qc_storage_supported <- function(values) {
  classes <- class(values)
  if (!is.null(dim(values)) || is.raw(values) || is.complex(values)) {
    return(FALSE)
  }
  if (inherits(values, "POSIXlt")) {
    return(identical(classes, c("POSIXlt", "POSIXt")))
  }
  if (is.list(values)) {
    return(FALSE)
  }
  allowed_classes <- list(
    "character", "logical", "integer", "numeric", "factor",
    c("ordered", "factor"), "Date", c("IDate", "Date"),
    c("POSIXct", "POSIXt")
  )
  any(vapply(allowed_classes, identical, logical(1), classes))
}

qc_data_frame_evidence <- function(data, spec, keys) {
  rows <- lapply(seq_len(nrow(spec)), function(index) {
    row <- spec[index, , drop = FALSE]
    if (qc_identifier_role(row$role[[1]])) {
      return(qc_unprofiled_evidence(keys[[index]], row$analysis_type[[1]], "declared_identifier"))
    }

    name <- row$name[[1]]
    if (!name %in% names(data)) {
      return(qc_unprofiled_evidence(keys[[index]], row$analysis_type[[1]], "missing_variable"))
    }
    values <- data[[name]]
    if (!qc_storage_supported(values)) {
      return(qc_unprofiled_evidence(keys[[index]], row$analysis_type[[1]], "unsupported_storage"))
    }
    levels <- if ("levels" %in% names(row)) eda_spec_levels(row$levels) else character()
    compatibility <- eda_type_compatibility(
      values,
      row$analysis_type[[1]],
      levels,
      eda_missing_codes(spec, name)
    )
    if (!compatibility$status %in% c("compatible", "coercible")) {
      return(qc_unprofiled_evidence(keys[[index]], row$analysis_type[[1]], "incompatible_storage"))
    }

    qc_df_profiled_evidence(
      values,
      row$analysis_type[[1]],
      eda_missing_codes(spec, name),
      keys[[index]]
    )
  })
  qc_bind_evidence(rows)
}

qc_df_profiled_evidence <- function(values, declared_type, missing_codes, key) {
  n <- qc_checked_count(length(values))
  missing <- summary_missing_mask(values, missing_codes)
  observed <- values[!missing]
  n_missing <- qc_checked_count(sum(missing))
  n_observed <- qc_checked_count(length(observed))
  n_unique <- qc_checked_count(length(unique(observed)))

  if (declared_type %in% c("numeric", "integer")) {
    numeric <- summary_numeric_core(values, missing_codes)
    row <- qc_profiled_evidence(
      variable_key = key,
      declared_type = declared_type,
      n = n,
      n_missing = n_missing,
      n_observed = n_observed,
      n_unique = n_unique,
      n_infinite = numeric$n_infinite[[1]],
      n_finite = numeric$n_finite[[1]],
      observed_min = numeric$min[[1]],
      observed_max = numeric$max[[1]],
      tukey_lower_fence = numeric$lower_fence[[1]],
      tukey_upper_fence = numeric$upper_fence[[1]],
      n_below_tukey = numeric$n_below_lower[[1]],
      n_above_tukey = numeric$n_above_upper[[1]]
    )
  } else {
    row <- qc_profiled_evidence(
      variable_key = key,
      declared_type = declared_type,
      n = n,
      n_missing = n_missing,
      n_observed = n_observed,
      n_unique = n_unique
    )
  }
  qc_validate_evidence_counts(row)
  row
}

qc_profiled_evidence <- function(variable_key,
                                 declared_type,
                                 n,
                                 n_missing,
                                 n_observed,
                                 n_unique,
                                 n_infinite = NA_integer_,
                                 n_finite = NA_integer_,
                                 observed_min = NA_real_,
                                 observed_max = NA_real_,
                                 tukey_lower_fence = NA_real_,
                                 tukey_upper_fence = NA_real_,
                                 n_below_tukey = NA_integer_,
                                 n_above_tukey = NA_integer_) {
  evidence_code <- if (n == 0L) {
    "zero_rows"
  } else if (n_observed == 0L) {
    "all_missing"
  } else {
    "profiled"
  }
  data.frame(
    variable_key = as.character(variable_key),
    evidence_state = "descriptive",
    declared_type = as.character(declared_type),
    profile_status = "profiled",
    evidence_code = evidence_code,
    n = as.integer(n),
    n_missing = as.integer(n_missing),
    n_observed = as.integer(n_observed),
    n_unique = as.integer(n_unique),
    n_infinite = as.integer(n_infinite),
    n_finite = as.integer(n_finite),
    observed_min = as.numeric(observed_min),
    observed_max = as.numeric(observed_max),
    tukey_lower_fence = as.numeric(tukey_lower_fence),
    tukey_upper_fence = as.numeric(tukey_upper_fence),
    n_below_tukey = as.integer(n_below_tukey),
    n_above_tukey = as.integer(n_above_tukey),
    stringsAsFactors = FALSE
  )
}

qc_unprofiled_evidence <- function(variable_key, declared_type, evidence_code) {
  data.frame(
    variable_key = as.character(variable_key),
    evidence_state = "descriptive",
    declared_type = as.character(declared_type),
    profile_status = "not_profiled",
    evidence_code = as.character(evidence_code),
    n = NA_integer_,
    n_missing = NA_integer_,
    n_observed = NA_integer_,
    n_unique = NA_integer_,
    n_infinite = NA_integer_,
    n_finite = NA_integer_,
    observed_min = NA_real_,
    observed_max = NA_real_,
    tukey_lower_fence = NA_real_,
    tukey_upper_fence = NA_real_,
    n_below_tukey = NA_integer_,
    n_above_tukey = NA_integer_,
    stringsAsFactors = FALSE
  )
}

qc_empty_evidence <- function() {
  data.frame(
    variable_key = character(),
    evidence_state = character(),
    declared_type = character(),
    profile_status = character(),
    evidence_code = character(),
    n = integer(),
    n_missing = integer(),
    n_observed = integer(),
    n_unique = integer(),
    n_infinite = integer(),
    n_finite = integer(),
    observed_min = numeric(),
    observed_max = numeric(),
    tukey_lower_fence = numeric(),
    tukey_upper_fence = numeric(),
    n_below_tukey = integer(),
    n_above_tukey = integer(),
    stringsAsFactors = FALSE
  )
}

qc_bind_evidence <- function(rows) {
  if (length(rows) == 0L) {
    return(qc_empty_evidence())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

qc_validate_evidence_counts <- function(row) {
  valid <- as.double(row$n_missing[[1]]) + row$n_observed[[1]] == row$n[[1]] &&
    row$n_unique[[1]] <= row$n_observed[[1]]
  if (row$declared_type[[1]] %in% c("numeric", "integer")) {
    valid <- valid &&
      as.double(row$n_finite[[1]]) + row$n_infinite[[1]] == row$n_observed[[1]]
  }
  if (!isTRUE(valid)) {
    stop("QC aggregate counts failed reconciliation.", call. = FALSE)
  }
  invisible(TRUE)
}

qc_pending_proposals <- function(evidence, spec) {
  rows <- lapply(seq_len(nrow(spec)), function(index) {
    if (qc_identifier_role(spec$role[[index]])) {
      return(NULL)
    }
    row <- evidence[index, , drop = FALSE]
    type <- spec$analysis_type[[index]]
    units <- if ("units" %in% names(spec)) spec$units[[index]] else ""
    units_prompt <- type %in% c("numeric", "integer", "date", "datetime") &&
      (is.na(units) || trimws(as.character(units)) == "")
    binary_prompt <- type %in% c("numeric", "integer") &&
      row$profile_status[[1]] == "profiled" &&
      row$n_observed[[1]] > 0L &&
      row$n_unique[[1]] == 2L &&
      row$n_infinite[[1]] == 0L &&
      identical(row$observed_min[[1]], 0) &&
      identical(row$observed_max[[1]], 1)
    screening_prompt <- type %in% c("numeric", "integer") &&
      row$profile_status[[1]] == "profiled" &&
      !binary_prompt &&
      is.finite(row$tukey_lower_fence[[1]]) &&
      is.finite(row$tukey_upper_fence[[1]]) &&
      as.double(row$n_below_tukey[[1]]) + row$n_above_tukey[[1]] > 0
    non_finite_prompt <- type %in% c("numeric", "integer") &&
      row$profile_status[[1]] == "profiled" &&
      row$n_infinite[[1]] > 0L

    rationale <- c(
      if (units_prompt) "units_not_declared",
      if (binary_prompt) "observed_integral_zero_one",
      if (screening_prompt) "finite_values_beyond_tukey",
      if (non_finite_prompt) "non_finite_values_present"
    )
    if (length(rationale) == 0L) {
      return(NULL)
    }
    data.frame(
      variable_key = row$variable_key,
      proposal_state = "pending",
      candidate_type = if (binary_prompt) "binary" else "",
      units_review_required = units_prompt,
      candidate_units = "",
      candidate_screening_min = if (screening_prompt) row$tukey_lower_fence else NA_real_,
      candidate_screening_max = if (screening_prompt) row$tukey_upper_fence else NA_real_,
      screening_basis = if (screening_prompt) "tukey_1_5_iqr" else "",
      candidate_allowed_levels = if (binary_prompt) "0;1" else "",
      candidate_missing_codes = "",
      rationale_codes = paste(rationale, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(qc_empty_proposals())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

qc_empty_proposals <- function() {
  data.frame(
    variable_key = character(),
    proposal_state = character(),
    candidate_type = character(),
    units_review_required = logical(),
    candidate_units = character(),
    candidate_screening_min = numeric(),
    candidate_screening_max = numeric(),
    screening_basis = character(),
    candidate_allowed_levels = character(),
    candidate_missing_codes = character(),
    rationale_codes = character(),
    stringsAsFactors = FALSE
  )
}

qc_result <- function(evidence, proposals) {
  structure(
    list(evidence = evidence, proposals = proposals),
    class = c("epi_eda_qc_proposals", "list")
  )
}
