#' Compare reviewed variables across ordered PostgreSQL periods
#'
#' Produce descriptive schema, missingness, and distribution-drift evidence
#' for reviewed EDA variables across explicitly ordered completed periods.
#' Source rows and row identifiers remain inside PostgreSQL.
#'
#' @param sources A uniquely named list of at least two unmodified
#'   [epi_eda_postgres_source()] objects sharing one caller-owned connection.
#'   List order defines period order.
#' @param spec A validated EDA specification data frame as returned by
#'   [epi_eda_spec()].
#' @param variables `NULL` to use every specification row in specification
#'   order, or a unique non-empty character vector of specification variable
#'   names. Explicit order is retained.
#' @param max_levels Positive whole-number hard bound for each declared
#'   categorical domain, each observed period domain, and each adjacent union.
#'
#' @return An `epi_eda_longitudinal_drift` list with fixed components
#'   `metadata`, `schema`, `missingness`, `missingness_adjacent`, `numeric`,
#'   `numeric_adjacent`, `categorical`, `categorical_adjacent`, `temporal`,
#'   `temporal_adjacent`, and `skipped`.
#'
#' @details All source validation and calculations occur in one `REPEATABLE
#' READ READ ONLY` transaction. Counts use the canonical R integer contract and
#' fail before exceeding `.Machine$integer.max`. Categorical retrieval is
#' limited to `max_levels + 1`; any declared, period, or adjacent domain above
#' the bound fails the whole call. No source rows, identifiers, hypothesis
#' tests, Shapiro-Wilk values, outlier rules, anomaly scores, or inferred
#' interpretations are returned or calculated.
#'
#' Missingness uses standard PostgreSQL missing values plus reviewed
#' semicolon-delimited `missing_codes`. Numeric location and spread retain the
#' canonical finite-value, type-7 quantile, and sample-SD rules. Categorical
#' proportions name both total-row and observed-value denominators. Dates use
#' days and timestamps with time zone use UTC seconds. Text participates in
#' schema and missingness continuity but has no distribution-drift summary.
#'
#' Adjacent changes are signed right-minus-left changes on the original scale;
#' they are not magnitudes or thresholds. Missingness relative change is
#' available only when its explicit left-period proportion denominator is
#' positive. Unavailable distribution evidence is recorded with stable
#' `code` values: `absent_variable`, `incompatible_type`,
#' `invalid_missing_contract`, `unsupported_analysis_type`, or
#' `zero_denominator`.
#'
#' Any invalid input, modified source, connection, catalogue, query,
#' transaction, count-range, categorical-bound, or reconciliation failure is a
#' hard error and returns no partial object. The operation creates no database
#' object and leaves the caller-owned connection open and idle.
#'
#' @section Result schema:
#' `metadata` records the contract version; period, full-specification and selected-variable counts; ordered period labels, source fingerprints and resolved variables as list columns; source-set, full-specification and selected-specification SHA-256 fingerprints; `max_levels`; the count contract and maximum; and the snapshot mode.
#'
#' Every period table starts with integer `period_index`, `period`, integer
#' `variable_index`, and `variable`. `schema` then has `analysis_type`,
#' `expected_database_type`, `observed_type`, `observed_present`,
#' `type_status`, and `type_reason`. `missingness` has integer `n`,
#' `n_missing`, and `n_observed`, `p_missing`, availability `status`, and
#' machine-readable `reason`.
#'
#' `missingness_adjacent` names both periods and retains their left and right
#' counts and missing proportions, signed `absolute_change`, `relative_change`,
#' and its explicit `relative_denominator`. `numeric` has integer `n`,
#' `n_missing`, `n_observed`, `n_infinite`, and `n_finite`; `min`, `q1`,
#' `mean`, `median`, `q3`, `max`, `iqr`, and `sd`; and availability fields.
#' `numeric_adjacent` retains left and right `n_finite` and the left value,
#' right value, and signed `*_change` for each authorised numeric metric.
#'
#' `categorical` has `level`, integer `n`, `p_total`, `p_observed`,
#' `is_declared`, `is_unexpected`, and availability fields.
#' `categorical_adjacent` names both denominator pairs, left and right counts
#' and proportions, signed proportion differences, declaration flags, the
#' positive-count `level_status`, and availability fields. `temporal` has
#' integer `n`, `n_missing`, and `n_observed`; formatted `min` and `max`;
#' numeric `range_value`; `unit`; and availability fields.
#' `temporal_adjacent` retains both formatted endpoints, numeric endpoint
#' shifts, both range values, signed range change, the unit, and availability
#' fields.
#'
#' `skipped` starts with the period/variable keys and adds `component`, `code`,
#' and a value-free explanatory `message`. Typed zero-row tables retain these
#' schemas.
#'
#' @family EDA
#' @export
epi_eda_longitudinal_drift <- function(sources,
                                       spec,
                                       variables = NULL,
                                       max_levels = 50L) {
  inputs <- ld_inputs(sources, spec, variables, max_levels)
  eda_longitudinal_transaction( # nolint: object_usage_linter
    inputs$sources,
    {
      context <- ld_context(inputs)
      result <- ld_profiles(context)
      structure(
        list(
          metadata = ld_metadata(context),
          schema = ld_schema(context),
          missingness = result$missingness,
          missingness_adjacent = ld_missing_adjacent(
            result$missingness, context
          ),
          numeric = result$numeric,
          numeric_adjacent = ld_numeric_adjacent(
            result, context
          ),
          categorical = result$categorical,
          categorical_adjacent = ld_categorical_adjacent(
            result, context
          ),
          temporal = result$temporal,
          temporal_adjacent = ld_temporal_adjacent(
            result, context
          ),
          skipped = result$skipped
        ),
        class = c("epi_eda_longitudinal_drift", "list")
      )
    },
    operation = "drift"
  )
}

#' @export
print.epi_eda_longitudinal_drift <- function(x, ...) {
  cat("<epi_eda_longitudinal_drift>\n")
  cat("  Periods: ", x$metadata$n_periods[[1L]], "\n", sep = "")
  cat("  Reviewed variables: ", x$metadata$n_variables[[1L]], "\n", sep = "")
  cat("  Unavailable distribution rows: ", nrow(x$skipped), "\n", sep = "")
  cat("  Source rows and identifiers: not returned\n")
  invisible(x)
}

ld_inputs <- function(sources,
                      spec,
                      variables,
                      max_levels) {
  source_inputs <- eda_longitudinal_source_inputs(sources) # nolint: object_usage_linter
  if (!is.data.frame(spec)) {
    stop("spec must be a validated EDA specification data frame.", call. = FALSE)
  }
  spec <- epi_eda_spec(spec)
  max_levels <- ld_max_levels(max_levels)
  if (is.null(variables)) {
    variables <- spec$name
  } else {
    valid <- is.character(variables) && length(variables) >= 1L &&
      !anyNA(variables) && !anyDuplicated(variables) &&
      all(nzchar(trimws(variables)))
    if (!valid) {
      stop(
        "variables must be NULL or a unique non-empty character vector.",
        call. = FALSE
      )
    }
    outside <- setdiff(variables, spec$name)
    if (length(outside) > 0L) {
      stop("variables cannot select outside the EDA specification.", call. = FALSE)
    }
  }
  selected <- spec[match(variables, spec$name), , drop = FALSE]
  rownames(selected) <- NULL
  declared_counts <- vapply(seq_len(nrow(selected)), function(index) {
    if (!(selected$analysis_type[[index]] %in% c("categorical", "binary")) ||
          !("levels" %in% names(selected))) {
      return(0L)
    }
    length(unique(eda_spec_levels(selected$levels[[index]])))
  }, integer(1))
  if (any(declared_counts > max_levels)) {
    stop("A declared categorical domain exceeds max_levels.", call. = FALSE)
  }
  list(
    sources = source_inputs$sources,
    period_labels = source_inputs$period_labels,
    spec = spec,
    selected = selected,
    variables = unname(variables),
    max_levels = max_levels
  )
}

ld_max_levels <- function(max_levels) {
  valid <- is.numeric(max_levels) && length(max_levels) == 1L &&
    !is.na(max_levels) && is.finite(max_levels) && max_levels >= 1 &&
    max_levels == floor(max_levels) &&
    max_levels < .Machine$integer.max
  if (!valid) {
    stop("max_levels must be a positive whole number below the R integer maximum.", call. = FALSE)
  }
  as.integer(max_levels)
}

ld_context <- function(inputs) {
  cells <- vector("list", length(inputs$sources) * nrow(inputs$selected))
  cell_index <- 0L
  for (period_index in seq_along(inputs$sources)) {
    source <- inputs$sources[[period_index]]
    for (variable_index in seq_len(nrow(inputs$selected))) {
      cell_index <- cell_index + 1L
      row <- inputs$selected[variable_index, , drop = FALSE]
      name <- as.character(row$name[[1L]])
      type <- as.character(row$analysis_type[[1L]])
      column <- eda_postgres_column(source, name)
      levels <- ld_declared_levels(row)
      compatibility <- eda_pg_type_compatibility(column, type, levels)
      contract <- if (is.null(column)) {
        NULL
      } else {
        eda_postgres_missing_contract(
          source, column, type, eda_missing_codes(inputs$spec, name)
        )
      }
      base <- ld_base_unavailable(
        column, compatibility, contract
      )
      cells[[cell_index]] <- list(
        period_index = as.integer(period_index),
        variable_index = as.integer(variable_index),
        source = source,
        spec_row = row,
        name = name,
        type = type,
        levels = levels,
        column = column,
        compatibility = compatibility,
        contract = contract,
        unavailable = base
      )
    }
  }
  inputs$cells <- cells
  inputs
}

ld_declared_levels <- function(spec_row) {
  if (!("levels" %in% names(spec_row))) return(character())
  unique(eda_spec_levels(spec_row$levels))
}

ld_base_unavailable <- function(column,
                                compatibility,
                                contract) {
  if (is.null(column)) {
    return(ld_unavailable(
      "absent_variable", "The reviewed variable is absent in this period."
    ))
  }
  if (identical(compatibility$status, "incompatible")) {
    return(ld_unavailable(
      "incompatible_type", compatibility$reason
    ))
  }
  if (!isTRUE(contract$valid)) {
    return(ld_unavailable(
      "invalid_missing_contract", contract$reason
    ))
  }
  NULL
}

ld_unavailable <- function(code, reason) {
  list(code = as.character(code), reason = as.character(reason))
}

ld_metadata <- function(context) {
  source_fingerprints <- unname(vapply(
    context$sources, eda_pg_source_fingerprint, character(1)
  ))
  specification_fingerprint <- eda_postgres_fingerprint(context$spec)
  selected_fingerprint <- eda_postgres_fingerprint(context$selected)
  source_contract <- list(
    contract_version = "longitudinal-drift-1",
    period_labels = context$period_labels,
    source_fingerprints = source_fingerprints,
    specification_fingerprint = specification_fingerprint,
    selected_specification_fingerprint = selected_fingerprint,
    resolved_variables = context$variables,
    max_levels = context$max_levels,
    count_contract = "canonical-r-integer",
    snapshot_mode = "REPEATABLE READ READ ONLY"
  )
  data.frame(
    contract_version = "longitudinal-drift-1",
    n_periods = as.integer(length(context$sources)),
    n_spec_variables = as.integer(nrow(context$spec)),
    n_variables = as.integer(nrow(context$selected)),
    period_labels = I(list(unname(context$period_labels))),
    source_fingerprints = I(list(source_fingerprints)),
    source_set_fingerprint_sha256 = eda_postgres_fingerprint(source_contract),
    specification_fingerprint_sha256 = specification_fingerprint,
    selected_specification_fingerprint_sha256 = selected_fingerprint,
    resolved_variables = I(list(unname(context$variables))),
    max_levels = as.integer(context$max_levels),
    count_contract = "canonical-r-integer",
    count_maximum = as.numeric(.Machine$integer.max),
    snapshot_mode = "REPEATABLE READ READ ONLY",
    stringsAsFactors = FALSE
  )
}

ld_schema <- function(context) {
  canonical <- lapply(
    context$sources,
    eda_postgres_schema_inside,
    spec = context$selected
  )
  rows <- lapply(context$cells, function(cell) {
    evidence <- canonical[[cell$period_index]][cell$variable_index, , drop = FALSE]
    data.frame(
      period_index = cell$period_index,
      period = context$period_labels[[cell$period_index]],
      variable_index = cell$variable_index,
      variable = cell$name,
      analysis_type = cell$type,
      expected_database_type = as.character(cell$spec_row$database_type[[1L]]),
      observed_type = evidence$observed_type,
      observed_present = evidence$observed_present,
      type_status = evidence$type_status,
      type_reason = evidence$type_reason,
      stringsAsFactors = FALSE
    )
  })
  ld_bind(rows, ld_empty_schema())
}

ld_profiles <- function(context) {
  row_counts <- vapply(
    context$sources, eda_postgres_row_count, integer(1)
  )
  missing_cache <- lapply(seq_along(context$sources), function(period_index) {
    eda_postgres_missing_inside(
      context$sources[[period_index]],
      context$selected,
      n_total = row_counts[[period_index]]
    )
  })
  categorical_cache <- ld_categorical_cache(context)
  missingness <- list()
  numeric <- list()
  categorical <- list()
  temporal <- list()
  skipped <- list()
  states <- vector("list", length(context$cells))
  cell_data <- vector("list", length(context$cells))

  for (cell_index in seq_along(context$cells)) {
    cell <- context$cells[[cell_index]]
    n_total <- row_counts[[cell$period_index]]
    missing <- ld_missing_one(
      cell,
      context,
      n_total,
      missing_cache[[cell$period_index]][cell$variable_index, , drop = FALSE]
    )
    missingness[[length(missingness) + 1L]] <- missing$row
    distribution <- ld_distribution_one(
      cell, context, n_total, missing, categorical_cache[[cell_index]]
    )
    states[[cell_index]] <- distribution$state
    cell_data[[cell_index]] <- distribution$data
    if (!is.null(distribution$row)) {
      component <- distribution$component
      if (component == "numeric") {
        numeric[[length(numeric) + 1L]] <- distribution$row
      } else if (component == "categorical") {
        categorical[[length(categorical) + 1L]] <- distribution$row
      } else if (component == "temporal") {
        temporal[[length(temporal) + 1L]] <- distribution$row
      }
    }
    if (!is.null(distribution$skipped)) {
      skipped[[length(skipped) + 1L]] <- distribution$skipped
    }
  }

  list(
    row_counts = row_counts,
    missingness = ld_bind(
      missingness, ld_empty_missingness()
    ),
    numeric = ld_bind(
      numeric, ld_empty_numeric()
    ),
    categorical = ld_bind(
      categorical, ld_empty_categorical()
    ),
    temporal = ld_bind(
      temporal, ld_empty_temporal()
    ),
    skipped = ld_bind(
      skipped, ld_empty_skipped()
    ),
    states = states,
    cell_data = cell_data
  )
}

ld_missing_one <- function(cell, context, n_total, canonical) {
  key <- ld_period_key(cell, context)
  if (!is.null(cell$unavailable)) {
    return(list(
      row = data.frame(
        key,
        n = as.integer(n_total),
        n_missing = NA_integer_,
        n_observed = NA_integer_,
        p_missing = NA_real_,
        status = "unavailable",
        reason = cell$unavailable$code,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      n_missing = NA_integer_,
      n_observed = NA_integer_,
      status = "unavailable",
      unavailable = cell$unavailable
    ))
  }
  n_missing <- as.integer(canonical$n_missing[[1L]])
  n_observed <- as.integer(n_total - n_missing)
  if (n_missing > n_total || n_observed < 0L) {
    stop(
      "PostgreSQL longitudinal drift missingness did not reconcile.",
      call. = FALSE
    )
  }
  unavailable <- if (n_total == 0L) {
    ld_unavailable(
      "zero_denominator", "The period has no rows for a missing proportion."
    )
  } else {
    NULL
  }
  list(
    row = data.frame(
      key,
      n = as.integer(n_total),
      n_missing = as.integer(n_missing),
      n_observed = n_observed,
      p_missing = if (n_total == 0L) NA_real_ else n_missing / n_total,
      status = if (is.null(unavailable)) "available" else "unavailable",
      reason = if (is.null(unavailable)) NA_character_ else unavailable$code,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    n_missing = as.integer(n_missing),
    n_observed = n_observed,
    status = if (is.null(unavailable)) "available" else "unavailable",
    unavailable = unavailable
  )
}

ld_period_key <- function(cell, context) {
  data.frame(
    period_index = cell$period_index,
    period = context$period_labels[[cell$period_index]],
    variable_index = cell$variable_index,
    variable = cell$name,
    stringsAsFactors = FALSE
  )
}

ld_distribution_one <- function(cell,
                                context,
                                n_total,
                                missing,
                                categorical_cache) {
  component <- ld_component(cell$type)
  unavailable <- cell$unavailable
  if (is.null(unavailable) && cell$type == "text") {
    unavailable <- ld_unavailable(
      "unsupported_analysis_type",
      "Text distribution drift is not supported; schema and missingness remain available."
    )
  }
  if (!is.null(unavailable)) {
    return(ld_skipped_distribution(
      cell, context, component, unavailable
    ))
  }
  if (cell$type == "integer" && !eda_postgres_integer_exact(
    cell$source, cell$column, cell$contract, cell$variable_index, NULL
  )) {
    unavailable <- ld_unavailable(
      "incompatible_type",
      "PostgreSQL bigint values exceed the exact R double integer range."
    )
    return(ld_skipped_distribution(
      cell, context, component, unavailable
    ))
  }

  result <- if (component == "numeric") {
    ld_numeric_one(cell, context, n_total)
  } else if (component == "categorical") {
    ld_categorical_one(
      cell, context, n_total, missing$n_observed, categorical_cache
    )
  } else {
    ld_temporal_one(cell, context, n_total)
  }
  ld_reconcile_distribution(
    result$counts, n_total, missing, component
  )
  zero <- if (component == "numeric") {
    result$counts$n_finite == 0L
  } else {
    result$counts$n_observed == 0L
  }
  state_unavailable <- if (zero) {
    ld_unavailable(
      "zero_denominator",
      "The period has no eligible values for a distribution summary."
    )
  } else {
    NULL
  }
  result$row$status <- rep(
    if (zero) "unavailable" else "available",
    nrow(result$row)
  )
  result$row$reason <- rep(
    if (zero) "zero_denominator" else NA_character_,
    nrow(result$row)
  )
  skipped <- if (zero) {
    ld_skip_row(
      cell, context, component, state_unavailable
    )
  } else {
    NULL
  }
  list(
    component = component,
    row = result$row,
    skipped = skipped,
    state = list(
      component = component,
      status = if (zero) "unavailable" else "available",
      unavailable = state_unavailable
    ),
    data = result$data
  )
}

ld_component <- function(type) {
  if (type %in% c("numeric", "integer")) return("numeric")
  if (type %in% c("categorical", "binary")) return("categorical")
  if (type %in% c("date", "datetime")) return("temporal")
  "text"
}

ld_skipped_distribution <- function(cell,
                                    context,
                                    component,
                                    unavailable) {
  list(
    component = component,
    row = NULL,
    skipped = ld_skip_row(
      cell, context, component, unavailable
    ),
    state = list(
      component = component,
      status = "unavailable",
      unavailable = unavailable
    ),
    data = NULL
  )
}

ld_skip_row <- function(cell,
                        context,
                        component,
                        unavailable) {
  data.frame(
    ld_period_key(cell, context),
    component = component,
    code = unavailable$code,
    message = unavailable$reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

ld_reconcile_distribution <- function(counts,
                                      n_total,
                                      missing,
                                      component) {
  valid <- identical(as.integer(counts$n_missing), missing$n_missing) &&
    identical(as.integer(counts$n_observed), missing$n_observed) &&
    counts$n_missing + counts$n_observed == n_total
  if (!is.null(counts$n_unique)) {
    valid <- valid && counts$n_unique <= counts$n_observed
  }
  if (component == "numeric") {
    valid <- valid &&
      counts$n_infinite + counts$n_finite == counts$n_observed
  }
  if (!isTRUE(valid)) {
    stop(
      "PostgreSQL longitudinal drift aggregate counts did not reconcile.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ld_categorical_cache <- function(context) {
  cache <- vector("list", length(context$cells))
  for (cell_index in seq_along(context$cells)) {
    cell <- context$cells[[cell_index]]
    if (!(cell$type %in% c("categorical", "binary")) ||
          !is.null(cell$unavailable)) {
      next
    }
    n_total <- eda_postgres_row_count(cell$source)
    canonical <- eda_pg_categorical_summary( # nolint: object_usage_linter
      cell$source,
      cell$column,
      cell$contract,
      cell$spec_row,
      cell$variable_index,
      n_total,
      NULL,
      max_levels = context$max_levels
    )
    cache[[cell_index]] <- canonical
  }
  cache
}

ld_ordered_domain <- function(declared, observed) {
  unexpected <- sort(setdiff(observed, declared), method = "radix")
  if (length(declared) > 0L) {
    c(declared, unexpected)
  } else {
    sort(unique(observed), method = "radix")
  }
}

ld_categorical_one <- function(cell,
                               context,
                               n_total,
                               n_observed,
                               cache) {
  if (is.null(cache)) {
    stop(
      "PostgreSQL longitudinal drift categorical preflight was incomplete.",
      call. = FALSE
    )
  }
  canonical <- cache$data
  if (!identical(as.integer(cache$counts$n_observed), n_observed)) {
    stop(
      "PostgreSQL longitudinal drift categorical counts did not reconcile.",
      call. = FALSE
    )
  }
  row <- data.frame(
    ld_period_key(cell, context)[rep(1L, nrow(canonical)), , drop = FALSE],
    level = canonical$level,
    n = as.integer(canonical$n),
    p_total = canonical$p_total,
    p_observed = canonical$p_observed,
    is_declared = canonical$is_declared,
    is_unexpected = canonical$is_unexpected,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  internal <- row
  attr(internal, "n_total") <- as.integer(n_total)
  attr(internal, "n_observed") <- as.integer(n_observed)
  list(
    row = row,
    counts = list(
      n_missing = as.integer(n_total - n_observed),
      n_observed = as.integer(n_observed),
      n_unique = as.integer(cache$counts$n_unique)
    ),
    data = internal
  )
}

ld_temporal_one <- function(cell, context, n_total) {
  observed <- eda_postgres_temporal_summary(
    cell$source,
    cell$column,
    cell$contract,
    cell$type,
    cell$variable_index,
    NULL
  )
  data <- observed$data
  row <- data.frame(
    ld_period_key(cell, context),
    n = as.integer(data$n),
    n_missing = as.integer(data$n_missing),
    n_observed = as.integer(data$n_observed),
    min = data$min,
    max = data$max,
    range_value = as.numeric(data$range_value),
    unit = data$range_unit,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(
    row = row,
    counts = c(observed$counts, list(n = as.integer(n_total))),
    data = list(
      row = row,
      values = observed$numeric_values
    )
  )
}

ld_missing_adjacent <- function(missingness, context) {
  rows <- list()
  for (from_period in seq_len(length(context$sources) - 1L)) {
    to_period <- from_period + 1L
    for (variable_index in seq_len(nrow(context$selected))) {
      left <- missingness[
        missingness$period_index == from_period &
          missingness$variable_index == variable_index,
        ,
        drop = FALSE
      ]
      right <- missingness[
        missingness$period_index == to_period &
          missingness$variable_index == variable_index,
        ,
        drop = FALSE
      ]
      if (nrow(left) != 1L || nrow(right) != 1L) {
        stop(
          "PostgreSQL longitudinal drift missingness adjacency was incomplete.",
          call. = FALSE
        )
      }
      available <- identical(left$status[[1L]], "available") &&
        identical(right$status[[1L]], "available")
      absolute_change <- if (available) {
        right$p_missing[[1L]] - left$p_missing[[1L]]
      } else {
        NA_real_
      }
      relative_denominator <- if (
        available && left$p_missing[[1L]] > 0
      ) {
        left$p_missing[[1L]]
      } else {
        NA_real_
      }
      relative_available <- available && !is.na(relative_denominator)
      rows[[length(rows) + 1L]] <- data.frame(
        left_period_index = as.integer(from_period),
        left_period = context$period_labels[[from_period]],
        right_period_index = as.integer(to_period),
        right_period = context$period_labels[[to_period]],
        variable_index = as.integer(variable_index),
        variable = context$variables[[variable_index]],
        n_left = as.integer(left$n[[1L]]),
        n_missing_left = as.integer(left$n_missing[[1L]]),
        n_observed_left = as.integer(left$n_observed[[1L]]),
        p_missing_left = as.numeric(left$p_missing[[1L]]),
        n_right = as.integer(right$n[[1L]]),
        n_missing_right = as.integer(right$n_missing[[1L]]),
        n_observed_right = as.integer(right$n_observed[[1L]]),
        p_missing_right = as.numeric(right$p_missing[[1L]]),
        absolute_change = absolute_change,
        relative_change = if (relative_available) {
          absolute_change / relative_denominator
        } else {
          NA_real_
        },
        relative_denominator = relative_denominator,
        status = if (available) "available" else "unavailable",
        reason = if (available) {
          NA_character_
        } else {
          ld_pair_reason(
            left$reason[[1L]], right$reason[[1L]]
          )
        },
        stringsAsFactors = FALSE
      )
    }
  }
  ld_bind(
    rows, ld_empty_missingness_adjacent()
  )
}

ld_pair_reason <- function(left, right) {
  reasons <- c(left, right)
  reasons <- reasons[!is.na(reasons)]
  if (length(reasons) == 0L) NA_character_ else reasons[[1L]]
}

ld_numeric_adjacent <- function(result, context) {
  rows <- list()
  metrics <- c("min", "q1", "mean", "median", "q3", "max", "iqr", "sd")
  numeric_variables <- which(
    context$selected$analysis_type %in% c("numeric", "integer")
  )
  for (from_period in seq_len(length(context$sources) - 1L)) {
    to_period <- from_period + 1L
    for (variable_index in numeric_variables) {
      left_index <- ld_cell_index(
        from_period, variable_index, context
      )
      right_index <- ld_cell_index(
        to_period, variable_index, context
      )
      left <- result$cell_data[[left_index]]
      right <- result$cell_data[[right_index]]
      state_left <- result$states[[left_index]]
      state_right <- result$states[[right_index]]
      available <- identical(state_left$status, "available") &&
        identical(state_right$status, "available")
      row <- data.frame(
        left_period_index = as.integer(from_period),
        left_period = context$period_labels[[from_period]],
        right_period_index = as.integer(to_period),
        right_period = context$period_labels[[to_period]],
        variable_index = as.integer(variable_index),
        variable = context$variables[[variable_index]],
        left_n_finite = ld_numeric_count(left, "n_finite"),
        right_n_finite = ld_numeric_count(right, "n_finite"),
        stringsAsFactors = FALSE
      )
      for (metric in metrics) {
        left_value <- ld_numeric_value(left, metric)
        right_value <- ld_numeric_value(right, metric)
        row[[paste0("left_", metric)]] <- left_value
        row[[paste0("right_", metric)]] <- right_value
        row[[paste0(metric, "_change")]] <-
          right_value - left_value
      }
      row$status <- if (available) "available" else "unavailable"
      row$reason <- if (available) {
        NA_character_
      } else {
        ld_state_reason(state_left, state_right)
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  ld_bind(rows, ld_empty_numeric_adjacent())
}

ld_numeric_count <- function(data, metric) {
  if (is.null(data) || !(metric %in% names(data))) return(NA_integer_)
  as.integer(data[[metric]][[1L]])
}

ld_numeric_value <- function(data, metric) {
  if (is.null(data) || !(metric %in% names(data))) return(NA_real_)
  as.numeric(data[[metric]][[1L]])
}

ld_state_reason <- function(left, right) {
  candidates <- list(left$unavailable, right$unavailable)
  for (candidate in candidates) {
    if (!is.null(candidate)) return(candidate$code)
  }
  NA_character_
}

ld_cell_index <- function(period_index,
                          variable_index,
                          context) {
  as.integer(
    (period_index - 1L) * nrow(context$selected) + variable_index
  )
}

ld_categorical_adjacent <- function(result, context) {
  rows <- list()
  categorical_variables <- which(
    context$selected$analysis_type %in% c("categorical", "binary")
  )
  for (from_period in seq_len(length(context$sources) - 1L)) {
    to_period <- from_period + 1L
    for (variable_index in categorical_variables) {
      left_index <- ld_cell_index(
        from_period, variable_index, context
      )
      right_index <- ld_cell_index(
        to_period, variable_index, context
      )
      left <- result$cell_data[[left_index]]
      right <- result$cell_data[[right_index]]
      state_left <- result$states[[left_index]]
      state_right <- result$states[[right_index]]
      cell <- context$cells[[left_index]]
      left_levels <- if (is.null(left)) character() else left$level
      right_levels <- if (is.null(right)) character() else right$level
      domain <- ld_ordered_domain(
        cell$levels, union(left_levels, right_levels)
      )
      if (length(domain) > context$max_levels) {
        stop(
          "A PostgreSQL categorical adjacent union exceeds max_levels.",
          call. = FALSE
        )
      }
      adjacent <- ld_categorical_pair_rows(
        domain,
        left,
        right,
        state_left,
        state_right,
        from_period,
        to_period,
        variable_index,
        context
      )
      rows[[length(rows) + 1L]] <- adjacent
    }
  }
  ld_bind(
    rows, ld_empty_categorical_adjacent()
  )
}

ld_categorical_pair_rows <- function(domain,
                                     left,
                                     right,
                                     state_left,
                                     state_right,
                                     from_period,
                                     to_period,
                                     variable_index,
                                     context) {
  available <- identical(state_left$status, "available") &&
    identical(state_right$status, "available")
  declared <- ld_declared_levels(
    context$selected[variable_index, , drop = FALSE]
  )
  rows <- lapply(domain, function(level) {
    left_values <- ld_level_values(left, level)
    right_values <- ld_level_values(right, level)
    level_status <- ld_level_status(
      left_values$n, right_values$n
    )
    data.frame(
      left_period_index = as.integer(from_period),
      left_period = context$period_labels[[from_period]],
      right_period_index = as.integer(to_period),
      right_period = context$period_labels[[to_period]],
      variable_index = as.integer(variable_index),
      variable = context$variables[[variable_index]],
      level = level,
      left_n = left_values$n,
      right_n = right_values$n,
      left_total_denominator = left_values$n_total,
      right_total_denominator = right_values$n_total,
      left_p_total = left_values$p_total,
      right_p_total = right_values$p_total,
      p_total_difference = right_values$p_total - left_values$p_total,
      left_observed_denominator = left_values$n_observed,
      right_observed_denominator = right_values$n_observed,
      left_p_observed = left_values$p_observed,
      right_p_observed = right_values$p_observed,
      p_observed_difference =
        right_values$p_observed - left_values$p_observed,
      is_declared = if (length(declared) > 0L && !is.na(level)) {
        level %in% declared
      } else {
        NA
      },
      is_unexpected = if (length(declared) > 0L && !is.na(level)) {
        !(level %in% declared)
      } else if (!is.na(level)) {
        FALSE
      } else {
        NA
      },
      level_status = level_status,
      status = if (available) "available" else "unavailable",
      reason = if (available) {
        NA_character_
      } else {
        ld_state_reason(state_left, state_right)
      },
      stringsAsFactors = FALSE
    )
  })
  ld_bind(
    rows, ld_empty_categorical_adjacent()
  )
}

ld_level_values <- function(data, level) {
  unavailable <- list(
    n = NA_integer_, n_total = NA_integer_, n_observed = NA_integer_,
    p_total = NA_real_, p_observed = NA_real_
  )
  if (is.null(data) || is.na(level)) return(unavailable)
  n_total <- attr(data, "n_total", exact = TRUE)
  n_observed <- attr(data, "n_observed", exact = TRUE)
  if (is.null(n_total) || is.null(n_observed)) return(unavailable)
  match_index <- match(level, data$level)
  if (is.na(match_index)) {
    return(list(
      n = 0L,
      n_total = as.integer(n_total),
      n_observed = as.integer(n_observed),
      p_total = summary_safe_proportion(0L, n_total),
      p_observed = summary_safe_proportion(0L, n_observed)
    ))
  }
  list(
    n = as.integer(data$n[[match_index]]),
    n_total = as.integer(n_total),
    n_observed = as.integer(n_observed),
    p_total = as.numeric(data$p_total[[match_index]]),
    p_observed = as.numeric(data$p_observed[[match_index]])
  )
}

ld_level_status <- function(left_n, right_n) {
  if (is.na(left_n) || is.na(right_n)) return(NA_character_)
  if (left_n > 0L && right_n > 0L) return("present_both")
  if (left_n > 0L) return("removed")
  if (right_n > 0L) return("introduced")
  "absent_both"
}

ld_temporal_adjacent <- function(result, context) {
  rows <- list()
  metrics <- c("min", "max")
  temporal_variables <- which(
    context$selected$analysis_type %in% c("date", "datetime")
  )
  for (from_period in seq_len(length(context$sources) - 1L)) {
    to_period <- from_period + 1L
    for (variable_index in temporal_variables) {
      left_index <- ld_cell_index(
        from_period, variable_index, context
      )
      right_index <- ld_cell_index(
        to_period, variable_index, context
      )
      left <- result$cell_data[[left_index]]
      right <- result$cell_data[[right_index]]
      state_left <- result$states[[left_index]]
      state_right <- result$states[[right_index]]
      available <- identical(state_left$status, "available") &&
        identical(state_right$status, "available")
      row <- data.frame(
        left_period_index = as.integer(from_period),
        left_period = context$period_labels[[from_period]],
        right_period_index = as.integer(to_period),
        right_period = context$period_labels[[to_period]],
        variable_index = as.integer(variable_index),
        variable = context$variables[[variable_index]],
        stringsAsFactors = FALSE
      )
      for (metric in metrics) {
        row[[paste0("left_", metric)]] <-
          ld_temporal_label(left, metric)
        row[[paste0("right_", metric)]] <-
          ld_temporal_label(right, metric)
        row[[paste0(metric, "_shift")]] <-
          ld_temporal_number(right, metric) -
          ld_temporal_number(left, metric)
      }
      row$left_range_value <- ld_temporal_number(
        left, "range_value"
      )
      row$right_range_value <- ld_temporal_number(
        right, "range_value"
      )
      row$range_change <-
        row$right_range_value - row$left_range_value
      row$unit <- if (
        context$selected$analysis_type[[variable_index]] == "date"
      ) {
        "days"
      } else {
        "seconds"
      }
      row$status <- if (available) "available" else "unavailable"
      row$reason <- if (available) {
        NA_character_
      } else {
        ld_state_reason(state_left, state_right)
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  ld_bind(rows, ld_empty_temporal_adjacent())
}

ld_temporal_label <- function(data, metric) {
  if (is.null(data) || is.null(data$row) || !(metric %in% names(data$row))) {
    return(NA_character_)
  }
  as.character(data$row[[metric]][[1L]])
}

ld_temporal_number <- function(data, metric) {
  if (is.null(data) || is.null(data$values) ||
        !(metric %in% names(data$values))) {
    return(NA_real_)
  }
  as.numeric(data$values[[metric]])
}

ld_bind <- function(rows, empty) {
  if (length(rows) == 0L) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

ld_empty_schema <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(), analysis_type = character(),
    expected_database_type = character(), observed_type = character(),
    observed_present = logical(),
    type_status = character(), type_reason = character(),
    stringsAsFactors = FALSE
  )
}

ld_empty_missingness <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    n = integer(), n_missing = integer(), n_observed = integer(),
    p_missing = numeric(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

ld_empty_missingness_adjacent <- function() {
  data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    n_left = integer(), n_missing_left = integer(),
    n_observed_left = integer(), p_missing_left = numeric(),
    n_right = integer(), n_missing_right = integer(), n_observed_right = integer(),
    p_missing_right = numeric(), absolute_change = numeric(),
    relative_change = numeric(), relative_denominator = numeric(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

ld_empty_numeric <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    n = integer(), n_missing = integer(), n_observed = integer(),
    n_infinite = integer(), n_finite = integer(),
    min = numeric(), q1 = numeric(), mean = numeric(), median = numeric(),
    q3 = numeric(), max = numeric(), iqr = numeric(), sd = numeric(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

ld_empty_numeric_adjacent <- function() {
  out <- data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    left_n_finite = integer(), right_n_finite = integer(),
    stringsAsFactors = FALSE
  )
  for (metric in c("min", "q1", "mean", "median", "q3", "max", "iqr", "sd")) {
    out[[paste0("left_", metric)]] <- numeric()
    out[[paste0("right_", metric)]] <- numeric()
    out[[paste0(metric, "_change")]] <- numeric()
  }
  out$status <- character()
  out$reason <- character()
  out
}

ld_empty_categorical <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    level = character(), n = integer(), p_total = numeric(), p_observed = numeric(),
    is_declared = logical(), is_unexpected = logical(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

ld_empty_categorical_adjacent <- function() {
  data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    level = character(), left_n = integer(), right_n = integer(),
    left_total_denominator = integer(), right_total_denominator = integer(),
    left_p_total = numeric(), right_p_total = numeric(),
    p_total_difference = numeric(), left_observed_denominator = integer(),
    right_observed_denominator = integer(), left_p_observed = numeric(),
    right_p_observed = numeric(), p_observed_difference = numeric(),
    is_declared = logical(), is_unexpected = logical(),
    level_status = character(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

ld_empty_temporal <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    n = integer(), n_missing = integer(), n_observed = integer(),
    min = character(), max = character(),
    range_value = numeric(), unit = character(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

ld_empty_temporal_adjacent <- function() {
  out <- data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    stringsAsFactors = FALSE
  )
  for (metric in c("min", "max")) {
    out[[paste0("left_", metric)]] <- character()
    out[[paste0("right_", metric)]] <- character()
    out[[paste0(metric, "_shift")]] <- numeric()
  }
  out$left_range_value <- numeric()
  out$right_range_value <- numeric()
  out$range_change <- numeric()
  out$unit <- character()
  out$status <- character()
  out$reason <- character()
  out
}

ld_empty_skipped <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    component = character(), code = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

# Reuse the canonical PostgreSQL numeric aggregate rather than maintaining a
# longitudinal copy of its finite-value and type-7 quantile definitions.
ld_numeric_one <- function(cell, context, n_total) {
  canonical <- eda_postgres_numeric_summary(
    cell$source, cell$column, cell$contract, cell$variable_index, NULL,
    allow_value_vector = FALSE
  )
  data <- canonical$data
  row <- data.frame(
    ld_period_key(cell, context), n = as.integer(n_total),
    n_missing = as.integer(canonical$counts$n_missing),
    n_observed = as.integer(canonical$counts$n_observed),
    n_infinite = as.integer(canonical$counts$n_infinite),
    n_finite = as.integer(data$n_finite), min = data$min, q1 = data$q1,
    mean = data$mean, median = data$median, q3 = data$q3, max = data$max,
    iqr = data$iqr, sd = data$sd, stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(row = row, counts = c(canonical$counts, list(n = as.integer(n_total))), data = row)
}
