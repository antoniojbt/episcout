#' Profile explicit-pair Spearman associations
#'
#' Calculate descriptive Spearman coefficients for an explicit ordered set of
#' reviewed numeric or integer variable pairs. Each pair is reranked
#' independently after pairwise exclusion of standard missing values, declared
#' missing codes and non-finite values.
#'
#' @param data A data frame containing observed or prepared data, or an
#'   [epi_eda_postgres_source()].
#' @param spec An EDA specification accepted by [epi_eda_spec()].
#' @param pairs An ordered two-column character data frame. Columns are
#'   interpreted as the left and right variable names; their column names are
#'   ignored. Zero-row typed input returns a typed empty result.
#'
#' @return A data frame with `left`, `left_label`, `right`, `right_label`,
#'   eligible `n`, Spearman `rho`, `status` and `reason`, in caller order.
#'
#' @details Identifier-role, absent, unsupported, self and duplicate unordered
#'   pairs are rejected. Unavailable rows distinguish insufficient eligible
#'   observations from a constant left, right or both variables. No p-values,
#'   confidence intervals, thresholds or interpretations are calculated.
#'
#'   PostgreSQL inputs are calculated inside one read-only repeatable-read
#'   transaction. Queries return one aggregate row per pair and never collect
#'   an analysis-value vector.
#'
#' @export
epi_eda_profile_spearman <- function(data, spec, pairs) {
  spec <- epi_eda_spec(spec)
  pairs <- association_spearman_pairs(pairs, spec)
  if (inherits(data, "epi_eda_postgres_source")) {
    return(association_pg_spearman(data, spec, pairs))
  }
  stratified_validate_data(data)
  association_validate_present(names(data), pairs)
  if (nrow(pairs) == 0L) {
    return(association_empty_spearman())
  }
  rows <- lapply(seq_len(nrow(pairs)), function(index) {
    left <- pairs$left[[index]]
    right <- pairs$right[[index]]
    left_values <- association_numeric_values(
      data[[left]], eda_missing_codes(spec, left), left
    )
    right_values <- association_numeric_values(
      data[[right]], eda_missing_codes(spec, right), right
    )
    eligible <- !left_values$missing & !right_values$missing &
      is.finite(left_values$values) & is.finite(right_values$values)
    association_spearman_row(
      pairs[index, , drop = FALSE],
      spec,
      left_values$values[eligible],
      right_values$values[eligible]
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

association_spearman_pairs <- function(pairs, spec) {
  valid <- is.data.frame(pairs) && ncol(pairs) == 2L &&
    all(vapply(pairs, is.character, logical(1)))
  if (!valid) {
    stop("pairs must be an ordered two-column character data frame.", call. = FALSE)
  }
  pairs <- data.frame(
    left = as.character(pairs[[1L]]),
    right = as.character(pairs[[2L]]),
    stringsAsFactors = FALSE
  )
  invalid_names <- is.na(pairs$left) | is.na(pairs$right) |
    !nzchar(trimws(pairs$left)) | !nzchar(trimws(pairs$right))
  if (any(invalid_names)) {
    stop("pairs variable names must be non-missing and non-blank.", call. = FALSE)
  }
  if (any(pairs$left == pairs$right)) {
    stop("pairs cannot contain a self pair.", call. = FALSE)
  }
  if (nrow(pairs) > 0L) {
    canonical <- t(vapply(seq_len(nrow(pairs)), function(index) {
      sort(c(pairs$left[[index]], pairs$right[[index]]), method = "radix")
    }, character(2)))
    if (anyDuplicated(as.data.frame(canonical, stringsAsFactors = FALSE))) {
      stop("pairs cannot contain duplicate unordered variable pairs.", call. = FALSE)
    }
  }
  requested <- unique(c(pairs$left, pairs$right))
  absent <- setdiff(requested, spec$name)
  if (length(absent) > 0L) {
    stop("Every pairs variable must be represented in the EDA specification.", call. = FALSE)
  }
  selected <- spec[match(requested, spec$name), , drop = FALSE]
  if (any(!selected$analysis_type %in% c("numeric", "integer"))) {
    stop("Every pairs variable must be declared numeric or integer.", call. = FALSE)
  }
  private <- trimws(tolower(as.character(selected$role))) %in% c("id", "identifier")
  if (any(private)) {
    stop("pairs cannot include an identifier-role variable.", call. = FALSE)
  }
  pairs
}

association_validate_present <- function(names, pairs) {
  requested <- unique(c(pairs$left, pairs$right))
  if (any(!requested %in% names)) {
    stop("Every pairs variable must be present in data.", call. = FALSE)
  }
  invisible(TRUE)
}

association_numeric_values <- function(values, missing_codes, name) {
  atomic <- is.atomic(values) && is.null(dim(values)) &&
    !inherits(values, c("Date", "POSIXt"))
  if (!atomic) {
    stop("Variable ", name, " requires numeric vector storage.", call. = FALSE)
  }
  missing <- summary_missing_mask(values, missing_codes)
  if (!is.numeric(values)) {
    if (!all(missing)) {
      stop("Variable ", name, " requires numeric vector storage.", call. = FALSE)
    }
    values <- rep(NA_real_, length(values))
  }
  list(values = as.numeric(values), missing = missing)
}

association_spearman_row <- function(pair, spec, left_values, right_values) {
  n <- as.numeric(length(left_values))
  left_constant <- n >= 2 && length(unique(left_values)) < 2L
  right_constant <- n >= 2 && length(unique(right_values)) < 2L
  availability <- association_spearman_state(
    n, left_constant, right_constant
  )
  if (availability$status == "available") {
    left_rank <- rank(left_values, ties.method = "average")
    right_rank <- rank(right_values, ties.method = "average")
    rho <- as.numeric(stats::cor(left_rank, right_rank))
  } else {
    rho <- NA_real_
  }
  association_spearman_result(
    pair, spec, n, rho, availability$status, availability$reason
  )
}

association_spearman_state <- function(n,
                                       left_constant,
                                       right_constant) {
  if (n < 2) {
    status <- "unavailable"
    reason <- "Fewer than two eligible observations."
  } else if (left_constant && right_constant) {
    status <- "unavailable"
    reason <- "Both variables are constant among eligible observations."
  } else if (left_constant) {
    status <- "unavailable"
    reason <- "The left variable is constant among eligible observations."
  } else if (right_constant) {
    status <- "unavailable"
    reason <- "The right variable is constant among eligible observations."
  } else {
    status <- "available"
    reason <- NA_character_
  }
  list(status = status, reason = reason)
}

association_spearman_result <- function(pair, spec, n, rho, status, reason) {
  left <- pair$left[[1L]]
  right <- pair$right[[1L]]
  left_row <- spec[match(left, spec$name), , drop = FALSE]
  right_row <- spec[match(right, spec$name), , drop = FALSE]
  data.frame(
    left = left,
    left_label = stratified_label(left_row$label[[1L]], left),
    right = right,
    right_label = stratified_label(right_row$label[[1L]], right),
    n = as.numeric(n),
    rho = as.numeric(rho),
    status = as.character(status),
    reason = as.character(reason),
    stringsAsFactors = FALSE
  )
}

association_empty_spearman <- function() {
  data.frame(
    left = character(), left_label = character(),
    right = character(), right_label = character(),
    n = numeric(), rho = numeric(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

association_pg_spearman <- function(source, spec, pairs) {
  eda_validate_postgres_source(source, require_idle = TRUE)
  association_validate_present(source$columns$name, pairs)
  contracts <- association_pg_contracts(source, spec, pairs)
  if (nrow(pairs) == 0L) {
    return(association_empty_spearman())
  }
  eda_postgres_transaction(source, {
    rows <- lapply(seq_len(nrow(pairs)), function(index) {
      association_pg_spearman_pair(
        source, spec, pairs[index, , drop = FALSE], contracts, index
      )
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  })
}

association_pg_contracts <- function(source, spec, pairs) {
  requested <- unique(c(pairs$left, pairs$right))
  out <- stats::setNames(vector("list", length(requested)), requested)
  for (name in requested) {
    row <- spec[match(name, spec$name), , drop = FALSE]
    column <- eda_postgres_column(source, name)
    compatibility <- eda_pg_type_compatibility(
      column, row$analysis_type[[1L]]
    )
    if (!compatibility$status %in% c("compatible", "coercible")) {
      stop("Every pairs variable requires compatible PostgreSQL numeric storage.", call. = FALSE)
    }
    missing <- eda_postgres_missing_contract(
      source, column, row$analysis_type[[1L]], eda_missing_codes(spec, name)
    )
    if (!missing$valid) {
      stop("A pairs missing-value contract is incompatible with PostgreSQL storage.", call. = FALSE)
    }
    out[[name]] <- list(
      column = column,
      value_sql = eda_postgres_column_sql(source, name),
      missing = missing,
      numeric_family = eda_postgres_storage_family(column) == "numeric"
    )
  }
  out
}

association_pg_spearman_pair <- function(source, spec, pair, contracts, index) {
  left <- contracts[[pair$left[[1L]]]]
  right <- contracts[[pair$right[[1L]]]]
  right_missing <- eda_postgres_missing_contract(
    source,
    right$column,
    spec$analysis_type[match(pair$right[[1L]], spec$name)],
    eda_missing_codes(spec, pair$right[[1L]]),
    offset = length(left$missing$params)
  )
  finite <- function(contract) {
    if (!contract$numeric_family) return("TRUE")
    paste0(
      contract$value_sql,
      "::text NOT IN ('Infinity', '-Infinity', 'NaN')"
    )
  }
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH eligible AS (SELECT ", left$value_sql, " AS left_value, ",
      right$value_sql, " AS right_value FROM ", eda_postgres_table_sql(source),
      " WHERE NOT (", left$missing$sql, ") AND NOT (", right_missing$sql,
      ") AND ", finite(left), " AND ", finite(right), "), ",
      "ranked AS (SELECT ",
      "rank() OVER (ORDER BY left_value)::double precision + ",
      "(count(*) OVER (PARTITION BY left_value)::double precision - 1) / 2 AS left_rank, ",
      "rank() OVER (ORDER BY right_value)::double precision + ",
      "(count(*) OVER (PARTITION BY right_value)::double precision - 1) / 2 AS right_rank, ",
      "left_value, right_value FROM eligible) ",
      "SELECT count(*)::text AS n, ",
      "count(DISTINCT left_value)::text AS n_left_levels, ",
      "count(DISTINCT right_value)::text AS n_right_levels, ",
      "corr(left_rank, right_rank) AS rho FROM ranked"
    ),
    params = c(left$missing$params, right_missing$params),
    query_kind = "association_spearman",
    limit = 1L,
    variable_index = as.integer(index),
    name = paste(pair$left[[1L]], pair$right[[1L]], sep = ":")
  )
  expected <- c("n", "n_left_levels", "n_right_levels", "rho")
  if (nrow(observed) != 1L || !identical(names(observed), expected)) {
    stop("PostgreSQL Spearman aggregates have an invalid schema.", call. = FALSE)
  }
  n <- association_exact_count(observed$n[[1L]], "PostgreSQL Spearman count")
  left_levels <- association_exact_count(
    observed$n_left_levels[[1L]], "PostgreSQL Spearman left level count"
  )
  right_levels <- association_exact_count(
    observed$n_right_levels[[1L]], "PostgreSQL Spearman right level count"
  )
  availability <- association_spearman_state(
    n, left_levels < 2, right_levels < 2
  )
  row <- association_spearman_result(
    pair, spec, n, NA_real_, availability$status, availability$reason
  )
  if (identical(row$status[[1L]], "available")) {
    row$rho <- if (is.na(observed$rho[[1L]])) {
      NA_real_
    } else {
      as.numeric(observed$rho[[1L]])
    }
    if (!is.finite(row$rho[[1L]])) {
      stop("PostgreSQL Spearman coefficient is invalid.", call. = FALSE)
    }
  }
  row
}

association_exact_count <- function(value, field) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) {
    stop(field, " was not returned as exact non-negative decimal text.", call. = FALSE)
  }
  normalised <- sub("^0+", "", value)
  if (!nzchar(normalised)) normalised <- "0"
  maximum <- "9007199254740991"
  too_large <- nchar(normalised) > nchar(maximum) ||
    (nchar(normalised) == nchar(maximum) && normalised > maximum)
  if (too_large) {
    stop(field, " exceeds the exact base-R double count range.", call. = FALSE)
  }
  numeric_value <- suppressWarnings(as.numeric(normalised))
  if (!is.finite(numeric_value)) {
    stop(field, " could not be converted to an exact base-R double.", call. = FALSE)
  }
  numeric_value
}
