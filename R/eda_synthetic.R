#' Generate synthetic data from an EDA specification
#'
#' Generate simple deterministic synthetic data from a validated
#' specification-first EDA data dictionary. Synthetic data are intended for
#' pipeline preparation and testing only, not for inference or disclosure
#' control.
#'
#' @param spec A data frame containing an EDA specification, or a path accepted
#'   by [epi_eda_spec()].
#' @param n Number of rows to generate.
#' @param seed Optional random seed. When supplied, repeated calls with the same
#'   specification, row count and seed return identical data.
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

generate_synthetic_column <- function(row, n) {
  type <- row$type[[1]]

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
  sample(seq.int(ceiling(bounds[["min"]]), floor(bounds[["max"]])), n, replace = TRUE)
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
