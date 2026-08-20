# Declarative contracts for bounded two-source probabilistic record linkage.
# This module validates caller choices only; it performs no source comparison.

linkage_scalar_choice <- function(value, choices, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !value %in% choices) {
    stop(
      name, " must be exactly one of: ", paste(choices, collapse = ", "), ".",
      call. = FALSE
    )
  }
  value
}

linkage_nonempty_text <- function(value, name, unique_values = FALSE) {
  if (!is.character(value) || anyNA(value) ||
        any(!nzchar(trimws(value, which = "both")))) {
    stop(name, " must contain non-missing, non-blank text.", call. = FALSE)
  }
  if (unique_values && anyDuplicated(value)) {
    stop(name, " must contain unique values.", call. = FALSE)
  }
  value
}

linkage_exact_names <- function(value, expected, name) {
  if (!identical(names(value), expected)) {
    stop(
      name, " must have exactly these fields in order: ",
      paste(expected, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(value)
}

linkage_whole_number <- function(value, name, minimum = 0, maximum = Inf) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value != floor(value) || value < minimum ||
        value > maximum) {
    stop(
      name, " must be one finite whole number from ", minimum, " through ",
      maximum, ".",
      call. = FALSE
    )
  }
  value
}

#' Declare a text normalisation profile for record linkage
#'
#' Creates an explicit profile used to derive comparison representations without
#' changing source values. The profile is generic: no particle or name component
#' is removed or interpreted unless the caller declares it.
#'
#' @param unicode Unicode normalisation form, either `"NFC"` or `"NFKC"`.
#' @param case Either `"keep"` or `"fold"`.
#' @param diacritics Either `"keep"` or `"strip"`.
#' @param punctuation One of `"keep"`, `"space"` or `"drop"`.
#' @param whitespace One of `"keep"`, `"trim"` or `"collapse"`.
#' @param token_order Either `"preserve"` or `"sort"`.
#' @param drop_tokens Explicit tokens to remove after applying the other profile
#'   operations. The default removes nothing.
#'
#' @return An `epi_linkage_text_profile` object.
#' @export
epi_linkage_text_profile <- function(unicode = "NFC",
                                     case = "fold",
                                     diacritics = "keep",
                                     punctuation = "space",
                                     whitespace = "collapse",
                                     token_order = "preserve",
                                     drop_tokens = character()) {
  unicode <- linkage_scalar_choice(unicode, c("NFC", "NFKC"), "unicode")
  case <- linkage_scalar_choice(case, c("keep", "fold"), "case")
  diacritics <- linkage_scalar_choice(
    diacritics, c("keep", "strip"), "diacritics"
  )
  punctuation <- linkage_scalar_choice(
    punctuation, c("keep", "space", "drop"), "punctuation"
  )
  whitespace <- linkage_scalar_choice(
    whitespace, c("keep", "trim", "collapse"), "whitespace"
  )
  token_order <- linkage_scalar_choice(
    token_order, c("preserve", "sort"), "token_order"
  )
  if (!is.character(drop_tokens) || anyNA(drop_tokens) ||
        any(!nzchar(trimws(drop_tokens, which = "both"))) ||
        anyDuplicated(drop_tokens)) {
    stop(
      "drop_tokens must be a unique character vector without missing or blank values.",
      call. = FALSE
    )
  }

  structure(
    list(
      unicode = unicode,
      case = case,
      diacritics = diacritics,
      punctuation = punctuation,
      whitespace = whitespace,
      token_order = token_order,
      drop_tokens = drop_tokens
    ),
    class = c("epi_linkage_text_profile", "list")
  )
}

linkage_validate_profiles <- function(profiles) {
  if (is.list(profiles) && length(profiles) == 0L) {
    return(list(
      identity = structure(list(), class = "epi_linkage_identity_profile")
    ))
  }
  if (!is.list(profiles) || is.null(names(profiles)) ||
        any(!nzchar(names(profiles))) || anyDuplicated(names(profiles))) {
    stop("profiles must be a uniquely named list.", call. = FALSE)
  }
  if ("identity" %in% names(profiles)) {
    stop("identity is a reserved built-in profile and must not be replaced.", call. = FALSE)
  }
  valid <- vapply(
    profiles,
    inherits,
    logical(1L),
    what = "epi_linkage_text_profile"
  )
  if (any(!valid)) {
    stop("Every profiles entry must be an epi_linkage_text_profile.", call. = FALSE)
  }
  c(list(identity = structure(list(), class = "epi_linkage_identity_profile")), profiles)
}

linkage_validate_blocks <- function(blocks, profile_names) {
  if (!is.data.frame(blocks)) {
    stop("blocks must be a data frame.", call. = FALSE)
  }
  expected <- c("pass", "x_field", "y_field", "profile")
  linkage_exact_names(blocks, expected, "blocks")
  if (nrow(blocks) == 0L) {
    stop("blocks must declare at least one bounded candidate-generation key.", call. = FALSE)
  }
  if (!is.numeric(blocks$pass) || anyNA(blocks$pass) ||
        any(!is.finite(blocks$pass)) || any(blocks$pass < 1) ||
        any(blocks$pass != floor(blocks$pass))) {
    stop("blocks$pass must contain positive whole numbers.", call. = FALSE)
  }
  linkage_nonempty_text(blocks$x_field, "blocks$x_field")
  linkage_nonempty_text(blocks$y_field, "blocks$y_field")
  linkage_nonempty_text(blocks$profile, "blocks$profile")
  if (any(!blocks$profile %in% profile_names)) {
    stop("Every blocks profile must be declared.", call. = FALSE)
  }
  normalised <- data.frame(
    pass = as.integer(blocks$pass),
    x_field = blocks$x_field,
    y_field = blocks$y_field,
    profile = blocks$profile,
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(normalised)) {
    stop("blocks must not contain duplicate declarations.", call. = FALSE)
  }
  normalised
}

linkage_validate_comparisons <- function(comparisons, profile_names) {
  if (!is.data.frame(comparisons)) {
    stop("comparisons must be a data frame.", call. = FALSE)
  }
  expected <- c(
    "comparison", "x_field", "y_field", "profile", "method", "parameter"
  )
  linkage_exact_names(comparisons, expected, "comparisons")
  if (nrow(comparisons) == 0L) {
    stop("comparisons must declare at least one field comparison.", call. = FALSE)
  }
  linkage_nonempty_text(
    comparisons$comparison, "comparisons$comparison", unique_values = TRUE
  )
  linkage_nonempty_text(comparisons$x_field, "comparisons$x_field")
  linkage_nonempty_text(comparisons$y_field, "comparisons$y_field")
  linkage_nonempty_text(comparisons$profile, "comparisons$profile")
  linkage_nonempty_text(comparisons$method, "comparisons$method")
  if (any(!comparisons$profile %in% profile_names)) {
    stop("Every comparisons profile must be declared.", call. = FALSE)
  }
  methods <- c(
    "exact", "jaro_winkler", "token_jaccard", "numeric_tolerance",
    "date_tolerance"
  )
  if (any(!comparisons$method %in% methods)) {
    stop(
      "comparisons$method must use a supported comparison method.",
      call. = FALSE
    )
  }
  if (!is.numeric(comparisons$parameter) || anyNA(comparisons$parameter) ||
        any(!is.finite(comparisons$parameter))) {
    stop("comparisons$parameter must contain finite numbers.", call. = FALSE)
  }
  string_method <- comparisons$method %in% c("jaro_winkler", "token_jaccard")
  tolerance_method <- comparisons$method %in% c(
    "numeric_tolerance", "date_tolerance"
  )
  invalid <- (comparisons$method == "exact" & comparisons$parameter != 1) |
    (string_method &
       (comparisons$parameter < 0 | comparisons$parameter > 1)) |
    (tolerance_method & comparisons$parameter < 0) |
    (comparisons$method == "date_tolerance" &
       comparisons$parameter != floor(comparisons$parameter))
  if (any(invalid)) {
    stop(
      "comparisons$parameter is invalid for its declared method.",
      call. = FALSE
    )
  }
  data.frame(
    comparison = comparisons$comparison,
    x_field = comparisons$x_field,
    y_field = comparisons$y_field,
    profile = comparisons$profile,
    method = comparisons$method,
    parameter = as.numeric(comparisons$parameter),
    stringsAsFactors = FALSE
  )
}

linkage_validate_model <- function(model, comparison_names) {
  if (is.null(model)) return(NULL)
  if (!is.list(model)) {
    stop("model must be NULL or a list.", call. = FALSE)
  }
  linkage_exact_names(model, c("parameters", "match_prevalence"), "model")
  parameters <- model$parameters
  if (!is.data.frame(parameters)) {
    stop("model$parameters must be a data frame.", call. = FALSE)
  }
  expected <- c("comparison", "m_probability", "u_probability")
  linkage_exact_names(parameters, expected, "model$parameters")
  linkage_nonempty_text(
    parameters$comparison, "model$parameters$comparison", unique_values = TRUE
  )
  if (!identical(parameters$comparison, comparison_names)) {
    stop(
      "model$parameters must cover every comparison once in declared order.",
      call. = FALSE
    )
  }
  if (!is.numeric(parameters$m_probability) ||
        !is.numeric(parameters$u_probability) ||
        anyNA(parameters$m_probability) || anyNA(parameters$u_probability) ||
        any(!is.finite(parameters$m_probability)) ||
        any(!is.finite(parameters$u_probability)) ||
        any(parameters$u_probability <= 0) ||
        any(parameters$m_probability >= 1) ||
        any(parameters$u_probability >= parameters$m_probability)) {
    stop(
      "Model probabilities must satisfy 0 < u_probability < m_probability < 1.",
      call. = FALSE
    )
  }
  prevalence <- model$match_prevalence
  if (!is.numeric(prevalence) || length(prevalence) != 1L ||
        is.na(prevalence) || !is.finite(prevalence) || prevalence <= 0 ||
        prevalence >= 1) {
    stop("model$match_prevalence must be strictly between zero and one.", call. = FALSE)
  }
  list(
    parameters = data.frame(
      comparison = parameters$comparison,
      m_probability = as.numeric(parameters$m_probability),
      u_probability = as.numeric(parameters$u_probability),
      stringsAsFactors = FALSE
    ),
    match_prevalence = as.numeric(prevalence)
  )
}

linkage_validate_thresholds <- function(thresholds, model) {
  if (is.null(thresholds)) return(NULL)
  if (is.null(model)) {
    stop("thresholds require a declared model.", call. = FALSE)
  }
  if (!is.list(thresholds)) {
    stop("thresholds must be NULL or a list.", call. = FALSE)
  }
  expected <- c("metric", "nonmatch_max", "match_min")
  linkage_exact_names(thresholds, expected, "thresholds")
  linkage_scalar_choice(
    thresholds$metric, "model_posterior", "thresholds$metric"
  )
  values <- unlist(thresholds[c("nonmatch_max", "match_min")], use.names = FALSE)
  if (!is.numeric(values) || anyNA(values) || any(!is.finite(values)) ||
        any(values < 0) || any(values > 1) || values[[1L]] >= values[[2L]]) {
    stop(
      "thresholds must satisfy 0 <= nonmatch_max < match_min <= 1.",
      call. = FALSE
    )
  }
  list(
    metric = "model_posterior",
    nonmatch_max = as.numeric(values[[1L]]),
    match_min = as.numeric(values[[2L]])
  )
}

#' Declare a bounded probabilistic record-linkage workflow
#'
#' Defines source record-key columns, derived normalisation profiles, exact
#' blocking passes, field comparisons, a hard candidate cap and optional
#' Fellegi-Sunter model/decision parameters. No role or threshold is inferred.
#'
#' @param x_id,y_id Exact record-key column names in the two source data frames.
#' @param profiles A uniquely named list created with
#'   [epi_linkage_text_profile()]. Built-in profile `"identity"` is always
#'   available and must not be supplied.
#' @param blocks Data frame with columns `pass`, `x_field`, `y_field`, `profile`.
#' @param comparisons Data frame with columns `comparison`, `x_field`,
#'   `y_field`, `profile`, `method`, `parameter`.
#' @param max_candidates Explicit hard candidate cap, from 1 through 10,000,000.
#' @param model Optional list with `parameters` and `match_prevalence`; see
#'   Details.
#' @param thresholds Optional list with `metric`, `nonmatch_max` and `match_min`.
#'
#' @details Rows in one blocking `pass` are joined with AND; distinct passes are
#' combined with OR. Candidate overflow fails rather than truncates. Supported
#' comparison methods are `exact`, `jaro_winkler`, `token_jaccard`,
#' `numeric_tolerance` and `date_tolerance`.
#'
#' `model$parameters` must contain `comparison`, `m_probability` and
#' `u_probability` in comparison order, with `0 < u < m < 1`. A model posterior
#' is not an empirical calibration. The package supplies no model values or
#' decision thresholds.
#'
#' @return An `epi_linkage_spec` object.
#' @export
epi_linkage_spec <- function(x_id,
                             y_id,
                             profiles = list(),
                             blocks,
                             comparisons,
                             max_candidates,
                             model = NULL,
                             thresholds = NULL) {
  linkage_nonempty_text(x_id, "x_id", unique_values = TRUE)
  linkage_nonempty_text(y_id, "y_id", unique_values = TRUE)
  if (length(x_id) != 1L || length(y_id) != 1L) {
    stop("x_id and y_id must each be one column name.", call. = FALSE)
  }
  profiles <- linkage_validate_profiles(profiles)
  blocks <- linkage_validate_blocks(blocks, names(profiles))
  comparisons <- linkage_validate_comparisons(comparisons, names(profiles))
  max_candidates <- linkage_whole_number(
    max_candidates, "max_candidates", minimum = 1, maximum = 10000000
  )
  model <- linkage_validate_model(model, comparisons$comparison)
  thresholds <- linkage_validate_thresholds(thresholds, model)

  structure(
    list(
      x_id = x_id,
      y_id = y_id,
      profiles = profiles,
      blocks = blocks,
      comparisons = comparisons,
      max_candidates = as.numeric(max_candidates),
      model = model,
      thresholds = thresholds,
      contract_version = "probabilistic-linkage-1"
    ),
    class = c("epi_linkage_spec", "list")
  )
}

#' @export
print.epi_linkage_text_profile <- function(x, ...) {
  cat("<epi_linkage_text_profile>\n")
  cat("  explicit derived-text profile\n")
  invisible(x)
}

#' @export
summary.epi_linkage_text_profile <- function(object, ...) {
  data.frame(
    contract = "explicit_derived_text_profile",
    n_explicit_drop_tokens = as.numeric(length(object$drop_tokens)),
    stringsAsFactors = FALSE
  )
}

#' @export
str.epi_linkage_text_profile <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
print.epi_linkage_spec <- function(x, ...) {
  cat("<epi_linkage_spec>\n")
  cat("  blocking passes: ", length(unique(x$blocks$pass)), "\n", sep = "")
  cat("  comparisons: ", nrow(x$comparisons), "\n", sep = "")
  cat("  hard candidate cap: ", x$max_candidates, "\n", sep = "")
  cat("  model declared: ", !is.null(x$model), "\n", sep = "")
  cat("  thresholds declared: ", !is.null(x$thresholds), "\n", sep = "")
  invisible(x)
}

#' @export
summary.epi_linkage_spec <- function(object, ...) {
  data.frame(
    contract_version = object$contract_version,
    n_blocking_passes = length(unique(object$blocks$pass)),
    n_comparisons = nrow(object$comparisons),
    max_candidates = object$max_candidates,
    model_declared = !is.null(object$model),
    thresholds_declared = !is.null(object$thresholds),
    stringsAsFactors = FALSE
  )
}

#' @export
str.epi_linkage_spec <- function(object, ...) {
  print(object)
  invisible(object)
}
