#' Generate synthetic data from an EDA specification
#'
#' Generate simple deterministic synthetic data from a validated specification-first EDA data dictionary. Synthetic data are intended for pipeline preparation and testing only, not for inference.
#'
#' @param spec A data frame containing an EDA specification, or a path accepted by [epi_eda_spec()].
#' @param n Number of rows to generate.
#' @param seed Optional random seed. When supplied, repeated calls with the same specification, row count and seed return identical data.
#'
#' @return A data frame with one column per specification variable and `n` rows.
#'
#' @export
epi_eda_generate_synthetic_data <- function(spec, n = 100, seed = NULL) { # nolint: object_length_linter
  spec <- if (is.data.frame(spec)) {
    epi_eda_validate_spec(spec)
  } else {
    epi_eda_spec(spec)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n != floor(n)) {
    stop("n must be a non-negative whole number.", call. = FALSE)
  }
  n <- as.integer(n)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      stop("seed must be NULL or a single numeric value.", call. = FALSE)
    }
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv) # nolint: object_name_linter
      }
    }, add = TRUE)
    set.seed(seed)
  }

  columns <- stats::setNames(
    lapply(seq_len(nrow(spec)), function(i) generate_synthetic_column(spec[i, , drop = FALSE], n)),
    spec$name
  )

  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Inject exact missing and blank values into an EDA fixture
#'
#' Deterministically replace eligible values in a data frame with exact
#' caller-declared counts of R missing values and literal blank strings. This
#' helper is intended for neutral test and pipeline-preparation fixtures. It
#' does not infer missing-value semantics, validate fixture provenance or
#' establish privacy, representativeness or scientific validity.
#' Existing R missing values and literal blanks are preserved and excluded from
#' eligible positions; reported counts describe newly injected values.
#'
#' @param data A data frame with unique, non-empty column names.
#' @param missing A named numeric vector giving the exact number of R missing
#'   values to inject per declared variable.
#' @param blanks Optional named numeric vector giving the exact number of
#'   literal `""` values to inject per declared character variable.
#' @param seed Optional non-negative whole-number seed. When supplied, repeated
#'   calls with the same data and declarations select identical positions and
#'   restore the caller's random-number state.
#'
#' @return A named list containing the modified `data`, per-variable realised
#'   `counts`, and compact reproducibility `metadata`.
#'
#' @export
epi_eda_inject_missingness <- function(data, missing, blanks = NULL, seed = NULL) {
  validate_injection_data(data)
  missing <- validate_injection_counts(missing, "missing", names(data))
  blanks <- validate_injection_counts(blanks, "blanks", names(data), allow_null = TRUE)
  seed <- validate_injection_seed(seed)
  validate_injection_targets(data, missing, blanks)

  declared <- names(data)[names(data) %in% union(names(missing), names(blanks))]
  realised <- data.frame(
    variable = declared,
    n_missing = as.integer(missing[declared]),
    n_blank = as.integer(blanks[declared]),
    stringsAsFactors = FALSE
  )
  realised$n_missing[is.na(realised$n_missing)] <- 0L
  realised$n_blank[is.na(realised$n_blank)] <- 0L

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv) # nolint: object_name_linter
      }
    }, add = TRUE)
    set.seed(seed)
  }

  result <- data
  for (i in seq_along(declared)) {
    variable <- declared[[i]]
    n_missing <- realised$n_missing[[i]]
    n_blank <- realised$n_blank[[i]]
    total <- n_missing + n_blank
    if (total == 0L) {
      next
    }

    values <- result[[variable]]
    eligible <- !is.na(values)
    if (is.character(values)) {
      eligible <- eligible & values != ""
    }
    positions <- which(eligible)
    selected <- positions[sample.int(length(positions), total, replace = FALSE)]
    if (n_missing > 0L) {
      result[[variable]][selected[seq_len(n_missing)]] <- NA
    }
    if (n_blank > 0L) {
      blank_index <- seq.int(n_missing + 1L, total)
      result[[variable]][selected[blank_index]] <- ""
    }
  }

  list(
    data = result,
    counts = realised,
    metadata = data.frame(
      seed = if (is.null(seed)) NA_real_ else seed,
      n_rows = as.integer(nrow(data)),
      n_declared_variables = as.integer(length(declared)),
      stringsAsFactors = FALSE
    )
  )
}

validate_injection_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  data_names <- names(data)
  if (anyNA(data_names) || any(!nzchar(data_names)) || anyDuplicated(data_names)) {
    stop("data must have unique, non-empty column names.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_injection_counts <- function(counts, argument, data_names, allow_null = FALSE) {
  if (is.null(counts) && allow_null) {
    return(stats::setNames(numeric(), character()))
  }
  if (!is.numeric(counts) || is.object(counts) || anyNA(counts) ||
        any(!is.finite(counts)) || any(counts < 0) || any(counts != floor(counts))) {
    stop(argument, " must contain exact non-negative whole-number counts.", call. = FALSE)
  }
  count_names <- names(counts)
  invalid_names <- length(counts) > 0L && (
    is.null(count_names) || anyNA(count_names) || any(!nzchar(count_names))
  )
  if (invalid_names) {
    stop(argument, " must be named by variable.", call. = FALSE)
  }
  if (anyDuplicated(count_names)) {
    stop(argument, " must not contain duplicate variable declarations.", call. = FALSE)
  }
  if (any(!(count_names %in% data_names))) {
    stop(argument, " contains an unknown variable.", call. = FALSE)
  }
  stats::setNames(as.numeric(counts), count_names)
}

validate_injection_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || is.object(seed) || length(seed) != 1L ||
        is.na(seed) || !is.finite(seed) || seed < 0 || seed != floor(seed) ||
        seed > .Machine$integer.max) {
    stop("seed must be NULL or a non-negative whole number within the R integer range.", call. = FALSE)
  }
  as.numeric(seed)
}

validate_injection_targets <- function(data, missing, blanks) {
  blank_variables <- names(blanks)[blanks > 0]
  if (any(!vapply(data[blank_variables], is.character, logical(1)))) {
    stop("blanks can be injected only into character variables.", call. = FALSE)
  }

  declared <- union(names(missing), names(blanks))
  for (variable in declared) {
    values <- data[[variable]]
    eligible <- !is.na(values)
    if (is.character(values)) {
      eligible <- eligible & values != ""
    }
    n_missing <- if (variable %in% names(missing)) missing[[variable]] else 0
    n_blank <- if (variable %in% names(blanks)) blanks[[variable]] else 0
    if (n_missing + n_blank > sum(eligible)) {
      stop("Requested injections exceed the eligible values for a variable.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

generate_synthetic_column <- function(row, n) {
  type <- row$analysis_type[[1]]

  switch(
    type,
    numeric = generate_synthetic_numeric(row, n),
    integer = generate_synthetic_integer(row, n),
    categorical = generate_synthetic_categorical(row, n),
    binary = generate_synthetic_binary(row, n),
    date = generate_synthetic_date(row, n),
    datetime = generate_synthetic_datetime(row, n),
    text = generate_synthetic_text(row, n),
    stop("Unsupported EDA specification type: ", type, call. = FALSE)
  )
}

generate_synthetic_numeric <- function(row, n) {
  bounds <- synthetic_numeric_bounds(row, default_min = 0, default_max = 100)
  stats::runif(n, min = bounds[["min"]], max = bounds[["max"]])
}

generate_synthetic_integer <- function(row, n) {
  bounds <- synthetic_numeric_bounds(row, default_min = 0, default_max = 100)
  lower_bound <- ceiling(bounds[["min"]])
  upper_bound <- floor(bounds[["max"]])

  if (lower_bound > upper_bound) {
    stop(
      "Synthetic integer variable '", row$name[[1]],
      "' bounds contain no integer values.",
      call. = FALSE
    )
  }

  candidates <- seq.int(lower_bound, upper_bound)
  if (n == 0) {
    return(candidates[integer()])
  }

  candidates[sample.int(length(candidates), n, replace = TRUE)]
}

generate_synthetic_categorical <- function(row, n) {
  levels <- synthetic_levels(row)
  if (length(levels) == 0) {
    levels <- c("Level 1", "Level 2", "Level 3")
  }
  sample(levels, n, replace = TRUE)
}

generate_synthetic_binary <- function(row, n) {
  levels <- synthetic_levels(row)
  if (length(levels) == 0) {
    levels <- c("0", "1")
  }
  sample(levels, n, replace = TRUE)
}

generate_synthetic_date <- function(row, n) {
  bounds <- synthetic_date_bounds(row)
  offsets <- sample(seq.int(0, as.integer(bounds[["max"]] - bounds[["min"]])), n, replace = TRUE)
  bounds[["min"]] + offsets
}

generate_synthetic_datetime <- function(row, n) {
  bounds <- synthetic_datetime_bounds(row)
  offsets <- stats::runif(n, min = 0, max = as.numeric(difftime(bounds[["max"]], bounds[["min"]], units = "secs")))
  bounds[["min"]] + offsets
}

generate_synthetic_text <- function(row, n) {
  if (n == 0) {
    return(character())
  }
  paste0(row$name[[1]], "_synthetic_", seq_len(n))
}

synthetic_levels <- function(row) {
  if (!("levels" %in% names(row)) || is.na(row$levels[[1]]) || trimws(row$levels[[1]]) == "") {
    return(character())
  }
  trimws(strsplit(as.character(row$levels[[1]]), ";", fixed = TRUE)[[1]])
}

synthetic_numeric_bounds <- function(row, default_min, default_max) {
  min_value <- if ("min" %in% names(row)) suppressWarnings(as.numeric(row$min[[1]])) else NA_real_
  max_value <- if ("max" %in% names(row)) suppressWarnings(as.numeric(row$max[[1]])) else NA_real_
  if (is.na(min_value)) min_value <- default_min
  if (is.na(max_value)) max_value <- default_max
  if (min_value > max_value) {
    stop("Synthetic numeric bounds are invalid for ", row$name[[1]], ".", call. = FALSE)
  }
  c(min = min_value, max = max_value)
}

synthetic_date_bounds <- function(row) {
  min_value <- synthetic_parse_date_bound(row, "min", as.Date("2000-01-01"))
  max_value <- synthetic_parse_date_bound(row, "max", as.Date("2000-12-31"))
  if (min_value > max_value) {
    stop("Synthetic date bounds are invalid for ", row$name[[1]], ".", call. = FALSE)
  }
  c(min = min_value, max = max_value)
}

synthetic_datetime_bounds <- function(row) {
  min_value <- synthetic_parse_datetime_bound(row, "min", as.POSIXct("2000-01-01 00:00:00", tz = "UTC"))
  max_value <- synthetic_parse_datetime_bound(row, "max", as.POSIXct("2000-12-31 23:59:59", tz = "UTC"))
  if (min_value > max_value) {
    stop("Synthetic datetime bounds are invalid for ", row$name[[1]], ".", call. = FALSE)
  }
  c(min = min_value, max = max_value)
}

synthetic_parse_date_bound <- function(row, column, default) {
  if (!(column %in% names(row)) || is.na(row[[column]][[1]]) || trimws(row[[column]][[1]]) == "") {
    return(default)
  }
  value <- as.Date(row[[column]][[1]])
  if (is.na(value)) default else value
}

synthetic_parse_datetime_bound <- function(row, column, default) {
  if (!(column %in% names(row)) || is.na(row[[column]][[1]]) || trimws(row[[column]][[1]]) == "") {
    return(default)
  }
  value <- as.POSIXct(row[[column]][[1]], tz = "UTC")
  if (is.na(value)) default else value
}
