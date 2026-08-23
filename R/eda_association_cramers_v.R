#' Calculate descriptive Cramer's V from aggregate counts
#'
#' Calculate Cramer's V for one or more explicit variable pairs from long-form
#' aggregate contingency counts. The calculation removes zero-total margins
#' from the active dimensions and uses Pearson expectations without continuity
#' or small-sample bias correction.
#'
#' @param counts A data frame containing character columns `left`, `right`,
#'   `row_level` and `column_level`, plus numeric non-negative whole-number
#'   `n`. Pair order is the order of first occurrence. Missing combinations of
#'   supplied row and column levels are treated as zero-count cells; explicit
#'   zero rows retain otherwise inactive domain evidence.
#'
#' @return A data frame with `left`, `right`, total `n`, `active_rows`,
#'   `active_columns`, `cramers_v`, `status` and `reason`. Zero total or fewer
#'   than two active rows or columns returns typed unavailable evidence.
#'
#' @details Counts and their pair totals must be exactly representable as base-R
#'   doubles from zero through `2^53 - 1`. Each explicit pair/row/column cell
#'   must occur at most once. This function consumes aggregates only and
#'   calculates no p-value, threshold, strength label or interpretation.
#'
#'   The categorical component from [epi_eda_profile_stratified()] can be
#'   adapted without another source query: use its `group_value`, `name`,
#'   `level` and `n` fields as `row_level`, `right`, `column_level` and `n`, and
#'   repeat the stratifier name from result metadata as `left`. Exclude Overall
#'   and missing-level rows when constructing that ordinary contingency table.
#'
#' @export
epi_eda_cramers_v <- function(counts) {
  counts <- association_cramers_counts(counts)
  if (nrow(counts) == 0L) {
    return(association_empty_cramers_v())
  }
  pairs <- unique(counts[c("left", "right")])
  rows <- lapply(seq_len(nrow(pairs)), function(index) {
    pair <- pairs[index, , drop = FALSE]
    selected <- counts[
      counts$left == pair$left[[1L]] & counts$right == pair$right[[1L]],
      ,
      drop = FALSE
    ]
    association_cramers_pair(pair, selected)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

association_cramers_counts <- function(counts) {
  required <- c("left", "right", "row_level", "column_level", "n")
  if (!is.data.frame(counts) || any(!required %in% names(counts))) {
    stop(
      "counts must contain left, right, row_level, column_level and n.",
      call. = FALSE
    )
  }
  counts <- counts[required]
  character_fields <- required[1:4]
  if (any(!vapply(counts[character_fields], is.character, logical(1)))) {
    stop("Contingency identities and levels must be character columns.", call. = FALSE)
  }
  if (anyNA(counts[character_fields])) {
    stop("Contingency identities and levels cannot be missing.", call. = FALSE)
  }
  if (any(!nzchar(trimws(counts$left))) || any(!nzchar(trimws(counts$right)))) {
    stop("Contingency pair identities cannot be blank.", call. = FALSE)
  }
  valid_counts <- is.numeric(counts$n) && !anyNA(counts$n) &&
    all(is.finite(counts$n)) && all(counts$n >= 0) &&
    all(counts$n == floor(counts$n)) && all(counts$n <= 9007199254740991)
  if (!valid_counts) {
    stop(
      "Contingency counts must be safe non-negative whole numbers.",
      call. = FALSE
    )
  }
  counts$n <- as.numeric(counts$n)
  if (anyDuplicated(counts[required[1:4]])) {
    stop("Contingency cells must be unique within each pair.", call. = FALSE)
  }
  counts
}

association_cramers_pair <- function(pair, counts) {
  row_levels <- sort(unique(counts$row_level), method = "radix")
  column_levels <- sort(unique(counts$column_level), method = "radix")
  cells <- matrix(
    0,
    nrow = length(row_levels),
    ncol = length(column_levels),
    dimnames = list(row_levels, column_levels)
  )
  positions <- cbind(
    match(counts$row_level, row_levels),
    match(counts$column_level, column_levels)
  )
  cells[positions] <- counts$n
  n <- sum(cells)
  if (!is.finite(n) || n > 9007199254740991 || n != floor(n)) {
    stop(
      "A contingency pair total exceeds the exact base-R double count range.",
      call. = FALSE
    )
  }
  active_row <- rowSums(cells) > 0
  active_column <- colSums(cells) > 0
  active_rows <- as.integer(sum(active_row))
  active_columns <- as.integer(sum(active_column))
  if (n == 0) {
    status <- "unavailable"
    reason <- "The contingency table has zero total count."
    coefficient <- NA_real_
  } else if (active_rows < 2L && active_columns < 2L) {
    status <- "unavailable"
    reason <- "The contingency table has fewer than two active rows and columns."
    coefficient <- NA_real_
  } else if (active_rows < 2L) {
    status <- "unavailable"
    reason <- "The contingency table has fewer than two active rows."
    coefficient <- NA_real_
  } else if (active_columns < 2L) {
    status <- "unavailable"
    reason <- "The contingency table has fewer than two active columns."
    coefficient <- NA_real_
  } else {
    active <- cells[active_row, active_column, drop = FALSE]
    row_totals <- rowSums(active)
    column_totals <- colSums(active)
    expected <- outer(row_totals, column_totals) / n
    chi_square <- sum((active - expected)^2 / expected)
    coefficient <- sqrt(
      chi_square / (n * min(active_rows - 1L, active_columns - 1L))
    )
    status <- "available"
    reason <- NA_character_
  }
  data.frame(
    left = pair$left[[1L]],
    right = pair$right[[1L]],
    n = as.numeric(n),
    active_rows = active_rows,
    active_columns = active_columns,
    cramers_v = as.numeric(coefficient),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

association_empty_cramers_v <- function() {
  data.frame(
    left = character(), right = character(), n = numeric(),
    active_rows = integer(), active_columns = integer(),
    cramers_v = numeric(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}
