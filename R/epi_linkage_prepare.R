# Source-copy preparation, bounded exact blocking and field comparison.
# Returned objects retain only declared computational values and redact routine
# print, summary and structure output.

linkage_value_name <- function(profile, field) {
  paste(profile, field, sep = "\035")
}

linkage_normalize_text <- function(value, profile) {
  result <- as.character(value)
  result <- if (identical(profile$unicode, "NFKC")) {
    stringi::stri_trans_nfkc(result)
  } else {
    stringi::stri_trans_nfc(result)
  }
  if (identical(profile$case, "fold")) {
    result <- stringi::stri_trans_casefold(result)
  }
  if (identical(profile$diacritics, "strip")) {
    result <- stringi::stri_trans_general(
      result, "NFD; [:Nonspacing Mark:] Remove; NFC"
    )
  }
  if (identical(profile$punctuation, "space")) {
    result <- stringi::stri_replace_all_regex(result, "\\p{P}+", " ")
  } else if (identical(profile$punctuation, "drop")) {
    result <- stringi::stri_replace_all_regex(result, "\\p{P}+", "")
  }
  if (identical(profile$whitespace, "trim")) {
    result <- stringi::stri_trim_both(result)
  } else if (identical(profile$whitespace, "collapse")) {
    result <- stringi::stri_replace_all_regex(result, "\\s+", " ")
    result <- stringi::stri_trim_both(result)
  }

  token_action <- length(profile$drop_tokens) > 0L ||
    identical(profile$token_order, "sort")
  if (token_action) {
    drop_tokens <- profile$drop_tokens
    if (length(drop_tokens) > 0L) {
      drop_profile <- profile
      drop_profile$drop_tokens <- character()
      drop_profile$token_order <- "preserve"
      drop_tokens <- linkage_normalize_text(drop_tokens, drop_profile)
    }
    result <- vapply(result, function(item) {
      if (is.na(item)) return(NA_character_)
      tokens <- stringi::stri_split_regex(item, "\\s+", omit_empty = TRUE)[[1L]]
      if (length(drop_tokens) > 0L) {
        tokens <- tokens[!tokens %in% drop_tokens]
      }
      if (identical(profile$token_order, "sort")) tokens <- sort(tokens)
      paste(tokens, collapse = " ")
    }, character(1L), USE.NAMES = FALSE)
  }
  blank <- !is.na(result) & !nzchar(stringi::stri_trim_both(result))
  result[blank] <- NA_character_
  result
}

linkage_value_family <- function(value) {
  if (inherits(value, "Date")) return("date")
  if (is.character(value)) return("character")
  if (is.integer(value) || (is.numeric(value) && identical(class(value), "numeric"))) {
    return("numeric")
  }
  if (is.logical(value)) return("logical")
  "unsupported"
}

linkage_prepare_value <- function(value, profile) {
  if (inherits(profile, "epi_linkage_text_profile")) {
    if (!is.character(value) && !is.factor(value)) {
      stop(
        "Text profiles require character or factor linkage fields.",
        call. = FALSE
      )
    }
    return(linkage_normalize_text(value, profile))
  }
  if (is.factor(value)) value <- as.character(value)
  family <- linkage_value_family(value)
  if (identical(family, "unsupported")) {
    stop("A declared linkage field has an unsupported class.", call. = FALSE)
  }
  if (identical(family, "numeric") && any(!is.finite(value[!is.na(value)]))) {
    stop("Numeric linkage fields must not contain NaN or infinity.", call. = FALSE)
  }
  if (identical(family, "character")) {
    blank <- !is.na(value) & !nzchar(stringi::stri_trim_both(value))
    value[blank] <- NA_character_
  }
  value
}

linkage_record_ids <- function(data, id) {
  if (!id %in% names(data)) {
    stop("A declared record-key column is absent.", call. = FALSE)
  }
  value <- data[[id]]
  if (!is.atomic(value) || is.list(value) || is.matrix(value) ||
        inherits(value, c("POSIXct", "POSIXlt"))) {
    stop("Record-key columns must be supported atomic vectors.", call. = FALSE)
  }
  if (is.factor(value)) value <- as.character(value)
  if (anyNA(value)) {
    stop("Record-key columns must not contain missing values.", call. = FALSE)
  }
  if (is.character(value) && any(!nzchar(stringi::stri_trim_both(value)))) {
    stop("Record-key columns must not contain blank values.", call. = FALSE)
  }
  if (is.numeric(value) && any(!is.finite(value))) {
    stop("Numeric record keys must be finite.", call. = FALSE)
  }
  if (anyDuplicated(value)) {
    stop("Record-key columns must be unique within each source.", call. = FALSE)
  }
  value
}

linkage_declarations_for_side <- function(spec, side) {
  field <- paste0(side, "_field")
  unique(rbind(
    data.frame(
      field = spec$blocks[[field]],
      profile = spec$blocks$profile,
      stringsAsFactors = FALSE
    ),
    data.frame(
      field = spec$comparisons[[field]],
      profile = spec$comparisons$profile,
      stringsAsFactors = FALSE
    )
  ))
}

linkage_prepare_side <- function(data, side, spec) {
  if (!is.data.frame(data) || is.matrix(data)) {
    stop("x and y must be ordinary data frames.", call. = FALSE)
  }
  id <- spec[[paste0(side, "_id")]]
  ids <- linkage_record_ids(data, id)
  declarations <- linkage_declarations_for_side(spec, side)
  absent <- setdiff(declarations$field, names(data))
  if (length(absent) > 0L) {
    stop("A declared linkage field is absent from its source.", call. = FALSE)
  }
  values <- vector("list", nrow(declarations))
  names(values) <- vapply(
    seq_len(nrow(declarations)),
    function(index) {
      linkage_value_name(
        declarations$profile[[index]], declarations$field[[index]]
      )
    },
    character(1L)
  )
  for (index in seq_len(nrow(declarations))) {
    values[[index]] <- linkage_prepare_value(
      data[[declarations$field[[index]]]],
      spec$profiles[[declarations$profile[[index]]]]
    )
  }
  list(ids = ids, values = values, n = nrow(data))
}

linkage_get_value <- function(prepared, side, profile, field) {
  prepared[[side]]$values[[linkage_value_name(profile, field)]]
}

linkage_validate_types <- function(prepared) {
  spec <- prepared$spec
  for (index in seq_len(nrow(spec$blocks))) {
    row <- spec$blocks[index, ]
    x <- linkage_get_value(prepared, "x", row$profile, row$x_field)
    y <- linkage_get_value(prepared, "y", row$profile, row$y_field)
    x_family <- linkage_value_family(x)
    y_family <- linkage_value_family(y)
    if (!identical(x_family, y_family) &&
          !(x_family == "numeric" && y_family == "numeric")) {
      stop("Paired blocking fields must have compatible derived types.", call. = FALSE)
    }
  }
  for (index in seq_len(nrow(spec$comparisons))) {
    row <- spec$comparisons[index, ]
    x <- linkage_get_value(prepared, "x", row$profile, row$x_field)
    y <- linkage_get_value(prepared, "y", row$profile, row$y_field)
    families <- c(linkage_value_family(x), linkage_value_family(y))
    valid <- switch(
      row$method,
      exact = identical(families[[1L]], families[[2L]]),
      jaro_winkler = all(families == "character"),
      token_jaccard = all(families == "character"),
      numeric_tolerance = all(families == "numeric"),
      date_tolerance = all(families == "date"),
      FALSE
    )
    if (!valid) {
      stop("A comparison method has incompatible derived field types.", call. = FALSE)
    }
  }
  invisible(prepared)
}

#' Prepare derived representations for probabilistic record linkage
#'
#' Validates both sources and creates declared comparison representations in a
#' separate object. The input data frames are never modified.
#'
#' @param x,y Two ordinary data frames.
#' @param spec An [epi_linkage_spec()] object.
#'
#' @return An `epi_linkage_prepared` object. Its print and summary methods are
#'   aggregate-only; the object contains sensitive derived values needed by later
#'   linkage steps.
#' @export
epi_linkage_prepare <- function(x, y, spec) {
  if (!inherits(spec, "epi_linkage_spec")) {
    stop("spec must be an epi_linkage_spec.", call. = FALSE)
  }
  prepared <- structure(
    list(
      x = linkage_prepare_side(x, "x", spec),
      y = linkage_prepare_side(y, "y", spec),
      spec = spec,
      metadata = data.frame(
        contract_version = "probabilistic-linkage-foundation-1",
        n_x = as.numeric(nrow(x)),
        n_y = as.numeric(nrow(y)),
        stringsAsFactors = FALSE
      )
    ),
    class = c("epi_linkage_prepared", "list")
  )
  linkage_validate_types(prepared)
  prepared
}

linkage_empty_pairs <- function() {
  data.frame(x_index = integer(), y_index = integer())
}

linkage_block_tables <- function(prepared, pass_rows) {
  key_names <- sprintf(".block_%03d", seq_len(nrow(pass_rows)))
  x <- data.frame(x_index = seq_len(prepared$x$n))
  y <- data.frame(y_index = seq_len(prepared$y$n))
  for (index in seq_len(nrow(pass_rows))) {
    row <- pass_rows[index, ]
    x[[key_names[[index]]]] <- linkage_get_value(
      prepared, "x", row$profile, row$x_field
    )
    y[[key_names[[index]]]] <- linkage_get_value(
      prepared, "y", row$profile, row$y_field
    )
  }
  x <- x[stats::complete.cases(x[key_names]), , drop = FALSE]
  y <- y[stats::complete.cases(y[key_names]), , drop = FALSE]
  list(x = x, y = y, keys = key_names)
}

linkage_block_count <- function(tables) {
  if (nrow(tables$x) == 0L || nrow(tables$y) == 0L) return(0)
  x_unique <- unique(tables$x[tables$keys])
  y_unique <- unique(tables$y[tables$keys])
  x_unique$.x_group <- seq_len(nrow(x_unique))
  y_unique$.y_group <- seq_len(nrow(y_unique))
  x_map <- merge(tables$x, x_unique, by = tables$keys, sort = FALSE)
  y_map <- merge(tables$y, y_unique, by = tables$keys, sort = FALSE)
  x_counts <- tabulate(x_map$.x_group, nbins = nrow(x_unique))
  y_counts <- tabulate(y_map$.y_group, nbins = nrow(y_unique))
  common <- merge(x_unique, y_unique, by = tables$keys, sort = FALSE)
  if (nrow(common) == 0L) return(0)
  sum(as.numeric(x_counts[common$.x_group]) * y_counts[common$.y_group])
}

linkage_block_pairs <- function(tables) {
  if (nrow(tables$x) == 0L || nrow(tables$y) == 0L) {
    return(linkage_empty_pairs())
  }
  pairs <- merge(tables$x, tables$y, by = tables$keys, sort = FALSE)
  if (nrow(pairs) == 0L) return(linkage_empty_pairs())
  pairs <- pairs[c("x_index", "y_index")]
  pairs <- pairs[order(pairs$x_index, pairs$y_index), , drop = FALSE]
  rownames(pairs) <- NULL
  pairs
}

#' Generate bounded candidate pairs from prepared linkage sources
#'
#' Applies exact AND-within/OR-across blocking passes. Missing block components
#' never match. The declared cap is a hard error boundary and candidates are
#' never truncated.
#'
#' @param prepared An [epi_linkage_prepare()] result.
#'
#' @return An `epi_linkage_candidates` object containing pair indices and exact
#'   aggregate candidate diagnostics.
#' @export
epi_linkage_candidates <- function(prepared) {
  if (!inherits(prepared, "epi_linkage_prepared")) {
    stop("prepared must be an epi_linkage_prepared object.", call. = FALSE)
  }
  n_possible <- as.numeric(prepared$x$n) * as.numeric(prepared$y$n)
  if (!is.finite(n_possible) || n_possible > 2^53 - 1) {
    stop("The possible-pair count exceeds the exact supported range.", call. = FALSE)
  }
  pairs <- linkage_empty_pairs()
  diagnostics <- vector("list", length(unique(prepared$spec$blocks$pass)))
  passes <- unique(prepared$spec$blocks$pass)
  for (pass_index in seq_along(passes)) {
    pass <- passes[[pass_index]]
    rows <- prepared$spec$blocks[prepared$spec$blocks$pass == pass, , drop = FALSE]
    tables <- linkage_block_tables(prepared, rows)
    pass_count <- linkage_block_count(tables)
    if (pass_count > prepared$spec$max_candidates) {
      stop("A blocking pass exceeds max_candidates; no pairs were returned.", call. = FALSE)
    }
    pass_pairs <- linkage_block_pairs(tables)
    if (!identical(as.numeric(nrow(pass_pairs)), as.numeric(pass_count))) {
      stop("Candidate-generation reconciliation failed.", call. = FALSE)
    }
    prior_keys <- paste(pairs$x_index, pairs$y_index, sep = ":")
    pass_keys <- paste(pass_pairs$x_index, pass_pairs$y_index, sep = ":")
    is_new <- !pass_keys %in% prior_keys
    new_pairs <- pass_pairs[is_new, , drop = FALSE]
    if (nrow(pairs) + nrow(new_pairs) > prepared$spec$max_candidates) {
      stop("The candidate union exceeds max_candidates; no pairs were returned.", call. = FALSE)
    }
    pairs <- rbind(pairs, new_pairs)
    diagnostics[[pass_index]] <- data.frame(
      pass = as.integer(pass),
      n_candidates_before_union = as.numeric(nrow(pass_pairs)),
      n_new_candidates = as.numeric(nrow(new_pairs)),
      n_duplicate_candidates = as.numeric(nrow(pass_pairs) - nrow(new_pairs)),
      stringsAsFactors = FALSE
    )
  }
  pairs <- pairs[order(pairs$x_index, pairs$y_index), , drop = FALSE]
  rownames(pairs) <- NULL
  n_candidates <- as.numeric(nrow(pairs))
  reduction_n <- n_possible - n_candidates
  reduction_ratio <- if (n_possible == 0) NA_real_ else reduction_n / n_possible
  structure(
    list(
      pairs = pairs,
      diagnostics = list(
        overall = data.frame(
          n_x = as.numeric(prepared$x$n),
          n_y = as.numeric(prepared$y$n),
          n_possible = n_possible,
          n_candidates = n_candidates,
          reduction_n = reduction_n,
          reduction_ratio = reduction_ratio,
          stringsAsFactors = FALSE
        ),
        passes = do.call(rbind, diagnostics)
      ),
      prepared = prepared
    ),
    class = c("epi_linkage_candidates", "list")
  )
}

linkage_token_jaccard <- function(x, y) {
  vapply(seq_along(x), function(index) {
    x_tokens <- unique(stringi::stri_split_regex(
      x[[index]], "\\s+", omit_empty = TRUE
    )[[1L]])
    y_tokens <- unique(stringi::stri_split_regex(
      y[[index]], "\\s+", omit_empty = TRUE
    )[[1L]])
    union_tokens <- union(x_tokens, y_tokens)
    if (length(union_tokens) == 0L) return(NA_real_)
    length(intersect(x_tokens, y_tokens)) / length(union_tokens)
  }, numeric(1L))
}

linkage_compare_values <- function(x, y, method, parameter) {
  missing <- is.na(x) | is.na(y)
  similarity <- rep(NA_real_, length(x))
  observed <- which(!missing)
  if (length(observed) > 0L) {
    similarity[observed] <- switch(
      method,
      exact = as.numeric(x[observed] == y[observed]),
      jaro_winkler = stringdist::stringsim(
        x[observed], y[observed], method = "jw"
      ),
      token_jaccard = linkage_token_jaccard(x[observed], y[observed]),
      numeric_tolerance = as.numeric(
        abs(x[observed] - y[observed]) <= parameter
      ),
      date_tolerance = as.numeric(
        abs(as.numeric(x[observed] - y[observed])) <= parameter
      )
    )
  }
  threshold <- if (method %in% c("jaro_winkler", "token_jaccard")) {
    parameter
  } else {
    1
  }
  state <- rep("missing", length(x))
  state[!missing & similarity >= threshold] <- "agree"
  state[!missing & similarity < threshold] <- "disagree"
  list(similarity = similarity, state = state)
}

linkage_empty_evidence <- function(include_values = FALSE) {
  result <- data.frame(
    x_index = integer(),
    y_index = integer(),
    comparison = character(),
    similarity = numeric(),
    comparison_state = character(),
    stringsAsFactors = FALSE
  )
  if (include_values) {
    result$x_id <- character()
    result$y_id <- character()
    result$x_value <- character()
    result$y_value <- character()
  }
  result
}

#' Compare declared fields for bounded linkage candidates
#'
#' Produces field-level similarity and explicit agreement, disagreement or
#' missing states. Routine output excludes record keys and values.
#'
#' @param candidates An [epi_linkage_candidates()] result.
#' @param include_values Whether to add declared record keys and derived field
#'   values for authorised manual review. These values remain sensitive.
#'
#' @return An `epi_linkage_comparisons` object.
#' @export
epi_linkage_compare <- function(candidates, include_values = FALSE) {
  if (!inherits(candidates, "epi_linkage_candidates")) {
    stop("candidates must be an epi_linkage_candidates object.", call. = FALSE)
  }
  if (!is.logical(include_values) || length(include_values) != 1L ||
        is.na(include_values)) {
    stop("include_values must be TRUE or FALSE.", call. = FALSE)
  }
  prepared <- candidates$prepared
  spec <- prepared$spec
  pairs <- candidates$pairs
  if (nrow(pairs) == 0L) {
    evidence <- linkage_empty_evidence(include_values)
  } else {
    rows <- vector("list", nrow(spec$comparisons))
    for (index in seq_len(nrow(spec$comparisons))) {
      declaration <- spec$comparisons[index, ]
      x_all <- linkage_get_value(
        prepared, "x", declaration$profile, declaration$x_field
      )
      y_all <- linkage_get_value(
        prepared, "y", declaration$profile, declaration$y_field
      )
      x <- x_all[pairs$x_index]
      y <- y_all[pairs$y_index]
      compared <- linkage_compare_values(
        x, y, declaration$method, declaration$parameter
      )
      row <- data.frame(
        x_index = pairs$x_index,
        y_index = pairs$y_index,
        comparison = rep(declaration$comparison, nrow(pairs)),
        similarity = compared$similarity,
        comparison_state = compared$state,
        stringsAsFactors = FALSE
      )
      if (include_values) {
        row$x_id <- as.character(prepared$x$ids[pairs$x_index])
        row$y_id <- as.character(prepared$y$ids[pairs$y_index])
        row$x_value <- as.character(x)
        row$y_value <- as.character(y)
      }
      rows[[index]] <- row
    }
    evidence <- do.call(rbind, rows)
    rownames(evidence) <- NULL
  }
  structure(
    list(
      metadata = data.frame(
        contract_version = "probabilistic-linkage-foundation-1",
        include_values = include_values,
        n_comparisons = as.numeric(nrow(spec$comparisons)),
        stringsAsFactors = FALSE
      ),
      candidate_diagnostics = candidates$diagnostics,
      evidence = evidence,
      spec = spec
    ),
    class = c("epi_linkage_comparisons", "list")
  )
}

#' @export
print.epi_linkage_prepared <- function(x, ...) {
  cat("<epi_linkage_prepared>\n")
  cat("  source rows: ", x$x$n, " + ", x$y$n, "\n", sep = "")
  cat("  derived values retained internally; source values not printed\n")
  invisible(x)
}

#' @export
summary.epi_linkage_prepared <- function(object, ...) {
  object$metadata
}

#' @export
str.epi_linkage_prepared <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
print.epi_linkage_candidates <- function(x, ...) {
  overall <- x$diagnostics$overall[1, ]
  cat("<epi_linkage_candidates>\n")
  cat("  possible pairs: ", overall$n_possible, "\n", sep = "")
  cat("  candidates: ", overall$n_candidates, "\n", sep = "")
  cat("  reduction ratio: ", overall$reduction_ratio, "\n", sep = "")
  invisible(x)
}

#' @export
summary.epi_linkage_candidates <- function(object, ...) {
  object$diagnostics$overall
}

#' @export
str.epi_linkage_candidates <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
print.epi_linkage_comparisons <- function(x, ...) {
  overall <- x$candidate_diagnostics$overall[1, ]
  counts <- table(factor(
    x$evidence$comparison_state,
    levels = c("agree", "disagree", "missing")
  ))
  cat("<epi_linkage_comparisons>\n")
  cat("  candidates: ", overall$n_candidates, "\n", sep = "")
  cat("  field evidence rows: ", nrow(x$evidence), "\n", sep = "")
  cat(
    "  states: agree=", counts[[1L]], ", disagree=", counts[[2L]],
    ", missing=", counts[[3L]], "\n", sep = ""
  )
  cat("  value-bearing evidence: ", x$metadata$include_values, "\n", sep = "")
  invisible(x)
}

#' @export
summary.epi_linkage_comparisons <- function(object, ...) {
  states <- table(factor(
    object$evidence$comparison_state,
    levels = c("agree", "disagree", "missing")
  ))
  data.frame(
    n_candidates = object$candidate_diagnostics$overall$n_candidates,
    n_field_evidence = as.numeric(nrow(object$evidence)),
    n_agree = as.numeric(states[[1L]]),
    n_disagree = as.numeric(states[[2L]]),
    n_missing = as.numeric(states[[3L]]),
    include_values = object$metadata$include_values,
    stringsAsFactors = FALSE
  )
}

#' @export
str.epi_linkage_comparisons <- function(object, ...) {
  print(object)
  invisible(object)
}
