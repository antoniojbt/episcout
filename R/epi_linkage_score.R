# Declared Fellegi-Sunter scoring, three-way classification and complete-truth
# validation. This module composes foundation objects and performs no writes.

linkage_empty_pair_scores <- function() {
  data.frame(
    x_index = integer(),
    y_index = integer(),
    n_agree = numeric(),
    n_disagree = numeric(),
    n_missing = numeric(),
    linkage_weight = numeric(),
    model_posterior = numeric(),
    stringsAsFactors = FALSE
  )
}

linkage_weight_contributions <- function(evidence, parameters) {
  parameter_index <- match(evidence$comparison, parameters$comparison)
  if (anyNA(parameter_index)) {
    stop("Field evidence does not match the declared model.", call. = FALSE)
  }
  m <- parameters$m_probability[parameter_index]
  u <- parameters$u_probability[parameter_index]
  contribution <- rep(0, nrow(evidence))
  agree <- evidence$comparison_state == "agree"
  disagree <- evidence$comparison_state == "disagree"
  missing <- evidence$comparison_state == "missing"
  if (any(!(agree | disagree | missing))) {
    stop("Field evidence contains an unsupported comparison state.", call. = FALSE)
  }
  contribution[agree] <- log2(m[agree] / u[agree])
  contribution[disagree] <- log2((1 - m[disagree]) / (1 - u[disagree]))
  contribution
}

linkage_aggregate_scores <- function(evidence, prevalence) {
  if (nrow(evidence) == 0L) return(linkage_empty_pair_scores())
  pair_keys <- paste(evidence$x_index, evidence$y_index, sep = ":")
  groups <- split(seq_len(nrow(evidence)), pair_keys)
  rows <- lapply(groups, function(indices) {
    states <- evidence$comparison_state[indices]
    weight <- sum(evidence$weight_contribution[indices])
    data.frame(
      x_index = evidence$x_index[indices[[1L]]],
      y_index = evidence$y_index[indices[[1L]]],
      n_agree = as.numeric(sum(states == "agree")),
      n_disagree = as.numeric(sum(states == "disagree")),
      n_missing = as.numeric(sum(states == "missing")),
      linkage_weight = weight,
      model_posterior = stats::plogis(stats::qlogis(prevalence) + weight * log(2)),
      stringsAsFactors = FALSE
    )
  })
  scores <- do.call(rbind, rows)
  scores <- scores[order(scores$x_index, scores$y_index), , drop = FALSE]
  rownames(scores) <- NULL
  scores
}

#' Score probabilistic linkage comparisons with a declared Fellegi-Sunter model
#'
#' Converts explicit agreement states into field log-likelihood contributions,
#' total linkage weights and model posteriors using only caller-supplied model
#' parameters. Missing comparisons contribute zero under the documented
#' ignorable pairwise-missingness assumption.
#'
#' @param comparisons An [epi_linkage_compare()] result whose specification
#'   contains a complete model declaration.
#'
#' @return An `epi_linkage_scores` object. `model_posterior` is a model result,
#'   not an empirical calibration.
#' @export
epi_linkage_score <- function(comparisons) {
  if (!inherits(comparisons, "epi_linkage_comparisons")) {
    stop("comparisons must be an epi_linkage_comparisons object.", call. = FALSE)
  }
  model <- comparisons$spec$model
  if (is.null(model)) {
    stop("Scoring requires caller-supplied Fellegi-Sunter model parameters.", call. = FALSE)
  }
  evidence <- comparisons$evidence
  evidence$weight_contribution <- linkage_weight_contributions(
    evidence, model$parameters
  )
  scores <- linkage_aggregate_scores(evidence, model$match_prevalence)
  expected_rows <- comparisons$candidate_diagnostics$overall$n_candidates
  if (!identical(as.numeric(nrow(scores)), as.numeric(expected_rows))) {
    stop("Pair-score reconciliation failed.", call. = FALSE)
  }
  structure(
    list(
      metadata = data.frame(
        contract_version = "probabilistic-linkage-score-1",
        include_values = comparisons$metadata$include_values,
        probability_semantics = paste0(
          "fellegi_sunter_model_posterior_",
          "not_empirically_calibrated"
        ),
        model_assumptions = paste(
          "conditional field independence;",
          "ignorable pairwise missing comparisons"
        ),
        stringsAsFactors = FALSE
      ),
      candidate_diagnostics = comparisons$candidate_diagnostics,
      pair_scores = scores,
      field_evidence = evidence,
      spec = comparisons$spec
    ),
    class = c("epi_linkage_scores", "list")
  )
}

linkage_empty_decisions <- function() {
  data.frame(
    x_index = integer(),
    y_index = integer(),
    decision = character(),
    stringsAsFactors = FALSE
  )
}

#' Classify linkage scores with caller-supplied thresholds
#'
#' Applies exact lower and upper model-posterior boundaries. Values at or below
#' `nonmatch_max` are `"non_match"`; values at or above `match_min` are
#' `"match"`; values between the boundaries are `"review"`.
#'
#' @param scores An [epi_linkage_score()] result whose specification contains
#'   caller-supplied thresholds.
#'
#' @return An `epi_linkage_result` with fixed metadata, diagnostics, scores,
#'   evidence and decisions. It never writes a crosswalk or registry.
#' @export
epi_linkage_classify <- function(scores) {
  if (!inherits(scores, "epi_linkage_scores")) {
    stop("scores must be an epi_linkage_scores object.", call. = FALSE)
  }
  thresholds <- scores$spec$thresholds
  if (is.null(thresholds)) {
    stop("Classification requires caller-supplied decision thresholds.", call. = FALSE)
  }
  if (nrow(scores$pair_scores) == 0L) {
    decisions <- linkage_empty_decisions()
  } else {
    posterior <- scores$pair_scores$model_posterior
    decision <- rep("review", length(posterior))
    decision[posterior <= thresholds$nonmatch_max] <- "non_match"
    decision[posterior >= thresholds$match_min] <- "match"
    decisions <- data.frame(
      x_index = scores$pair_scores$x_index,
      y_index = scores$pair_scores$y_index,
      decision = decision,
      stringsAsFactors = FALSE
    )
  }
  metadata <- scores$metadata
  metadata$decision_metric <- thresholds$metric
  metadata$nonmatch_max <- thresholds$nonmatch_max
  metadata$match_min <- thresholds$match_min
  structure(
    list(
      metadata = metadata,
      candidate_diagnostics = scores$candidate_diagnostics,
      pair_scores = scores$pair_scores,
      field_evidence = scores$field_evidence,
      decisions = decisions
    ),
    class = c("epi_linkage_result", "list")
  )
}

#' Run the declared in-memory probabilistic linkage workflow
#'
#' Thinly composes preparation, bounded candidate generation, field comparison,
#' Fellegi-Sunter scoring and caller-threshold classification. It adds no
#' alternate calculations and performs no persistence.
#'
#' @param x,y Two ordinary data frames.
#' @param spec A complete [epi_linkage_spec()] with model and threshold
#'   declarations.
#' @param include_values Whether reviewed output should include declared record
#'   keys and derived comparison values. These remain sensitive.
#'
#' @return An `epi_linkage_result`.
#' @export
epi_linkage_run <- function(x, y, spec, include_values = FALSE) {
  if (!inherits(spec, "epi_linkage_spec")) {
    stop("spec must be an epi_linkage_spec.", call. = FALSE)
  }
  if (is.null(spec$model) || is.null(spec$thresholds)) {
    stop("A complete model and caller-supplied thresholds are required.", call. = FALSE)
  }
  prepared <- epi_linkage_prepare(x, y, spec)
  candidates <- epi_linkage_candidates(prepared)
  comparisons <- epi_linkage_compare(candidates, include_values = include_values)
  epi_linkage_classify(epi_linkage_score(comparisons))
}

linkage_ratio <- function(numerator, denominator) {
  if (denominator == 0) return(NA_real_)
  numerator / denominator
}

linkage_validate_truth <- function(truth, n_x, n_y, n_possible) {
  if (!is.data.frame(truth)) {
    stop("truth must be a complete data frame.", call. = FALSE)
  }
  linkage_exact_names(truth, c("x_index", "y_index", "is_match"), "truth")
  valid_index <- function(value, maximum) {
    is.numeric(value) && !anyNA(value) && all(is.finite(value)) &&
      all(value == floor(value)) && all(value >= 1) && all(value <= maximum)
  }
  if (!valid_index(truth$x_index, n_x) || !valid_index(truth$y_index, n_y)) {
    stop("truth pair indices must be complete and within the two sources.", call. = FALSE)
  }
  if (!is.logical(truth$is_match) || anyNA(truth$is_match)) {
    stop("truth$is_match must contain non-missing logical values.", call. = FALSE)
  }
  keys <- paste(truth$x_index, truth$y_index, sep = ":")
  if (anyDuplicated(keys) || !identical(as.numeric(nrow(truth)), n_possible)) {
    stop("truth must contain every Cartesian pair exactly once.", call. = FALSE)
  }
  data.frame(
    x_index = as.integer(truth$x_index),
    y_index = as.integer(truth$y_index),
    is_match = truth$is_match,
    stringsAsFactors = FALSE
  )
}

#' Validate linkage candidates and decisions against complete truth
#'
#' Evaluates blocking separately from final decisions. V1 requires one truth row
#' for every Cartesian pair so that non-candidate pairs are known rather than
#' silently assumed to be non-matches.
#'
#' @param result An [epi_linkage_classify()] or [epi_linkage_run()] result.
#' @param truth A complete data frame with exact columns `x_index`, `y_index`,
#'   `is_match` and one unique row for every possible pair.
#'
#' @return An aggregate-only `epi_linkage_validation` object containing explicit
#'   numerator, denominator and measure columns. No truth or source values are
#'   retained.
#' @export
epi_linkage_validate <- function(result, truth) {
  if (!inherits(result, "epi_linkage_result")) {
    stop("result must be an epi_linkage_result.", call. = FALSE)
  }
  overall <- result$candidate_diagnostics$overall[1, ]
  truth <- linkage_validate_truth(
    truth, overall$n_x, overall$n_y, overall$n_possible
  )
  truth_keys <- paste(truth$x_index, truth$y_index, sep = ":")
  decision_keys <- paste(
    result$decisions$x_index, result$decisions$y_index, sep = ":"
  )
  if (anyDuplicated(decision_keys) || any(!decision_keys %in% truth_keys)) {
    stop("Candidate decisions do not reconcile with complete truth.", call. = FALSE)
  }
  decision <- rep("non_match", nrow(truth))
  candidate_position <- match(decision_keys, truth_keys)
  decision[candidate_position] <- result$decisions$decision
  candidate <- truth_keys %in% decision_keys

  n_true <- as.numeric(sum(truth$is_match))
  n_true_candidate <- as.numeric(sum(truth$is_match & candidate))
  classified_match <- decision == "match"
  n_classified_match <- as.numeric(sum(classified_match))
  n_true_classified_match <- as.numeric(sum(truth$is_match & classified_match))
  n_false_match <- as.numeric(sum(!truth$is_match & classified_match))
  n_missed <- as.numeric(sum(truth$is_match & decision == "non_match"))
  n_true_review <- as.numeric(sum(truth$is_match & decision == "review"))
  n_review <- as.numeric(sum(decision == "review"))
  n_candidates <- as.numeric(overall$n_candidates)
  n_possible <- as.numeric(overall$n_possible)

  metrics <- data.frame(
    n_possible_pairs = n_possible,
    n_candidates = n_candidates,
    candidate_recall_numerator = n_true_candidate,
    candidate_recall_denominator = n_true,
    candidate_recall = linkage_ratio(n_true_candidate, n_true),
    precision_numerator = n_true_classified_match,
    precision_denominator = n_classified_match,
    precision = linkage_ratio(n_true_classified_match, n_classified_match),
    recall_numerator = n_true_classified_match,
    recall_denominator = n_true,
    recall = linkage_ratio(n_true_classified_match, n_true),
    false_match_n = n_false_match,
    false_match_denominator = n_classified_match,
    false_match_rate = linkage_ratio(n_false_match, n_classified_match),
    missed_match_n = n_missed,
    missed_match_denominator = n_true,
    missed_match_rate = linkage_ratio(n_missed, n_true),
    true_match_review_n = n_true_review,
    review_n = n_review,
    review_candidate_denominator = n_candidates,
    review_candidate_proportion = linkage_ratio(n_review, n_candidates),
    review_possible_denominator = n_possible,
    review_possible_proportion = linkage_ratio(n_review, n_possible),
    stringsAsFactors = FALSE
  )
  if (n_true_classified_match + n_missed + n_true_review != n_true) {
    stop("Validation true-match reconciliation failed.", call. = FALSE)
  }
  if (n_true_classified_match + n_false_match != n_classified_match) {
    stop("Validation classified-match reconciliation failed.", call. = FALSE)
  }
  structure(
    list(
      metadata = data.frame(
        contract_version = "probabilistic-linkage-validation-1",
        truth_scope = "complete_cartesian",
        stringsAsFactors = FALSE
      ),
      metrics = metrics
    ),
    class = c("epi_linkage_validation", "list")
  )
}

#' @export
print.epi_linkage_scores <- function(x, ...) {
  cat("<epi_linkage_scores>\n")
  cat("  candidates scored: ", nrow(x$pair_scores), "\n", sep = "")
  cat("  posterior semantics: model-based, not empirically calibrated\n")
  invisible(x)
}

#' @export
summary.epi_linkage_scores <- function(object, ...) {
  data.frame(
    n_candidates = as.numeric(nrow(object$pair_scores)),
    n_field_evidence = as.numeric(nrow(object$field_evidence)),
    n_missing_field_evidence = as.numeric(sum(
      object$field_evidence$comparison_state == "missing"
    )),
    empirically_calibrated = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @export
str.epi_linkage_scores <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
print.epi_linkage_result <- function(x, ...) {
  counts <- table(factor(
    x$decisions$decision,
    levels = c("match", "review", "non_match")
  ))
  cat("<epi_linkage_result>\n")
  cat("  candidates: ", nrow(x$decisions), "\n", sep = "")
  cat(
    "  decisions: match=", counts[[1L]], ", review=", counts[[2L]],
    ", non_match=", counts[[3L]], "\n", sep = ""
  )
  cat("  no crosswalk or registry was written\n")
  invisible(x)
}

#' @export
summary.epi_linkage_result <- function(object, ...) {
  counts <- table(factor(
    object$decisions$decision,
    levels = c("match", "review", "non_match")
  ))
  data.frame(
    n_candidates = as.numeric(nrow(object$decisions)),
    n_match = as.numeric(counts[[1L]]),
    n_review = as.numeric(counts[[2L]]),
    n_non_match = as.numeric(counts[[3L]]),
    empirically_calibrated = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @export
str.epi_linkage_result <- function(object, ...) {
  print(object)
  invisible(object)
}

#' @export
print.epi_linkage_validation <- function(x, ...) {
  metrics <- x$metrics[1, ]
  cat("<epi_linkage_validation>\n")
  cat("  truth scope: complete Cartesian pairs\n")
  cat("  candidate recall: ", metrics$candidate_recall, "\n", sep = "")
  cat("  precision: ", metrics$precision, "\n", sep = "")
  cat("  recall: ", metrics$recall, "\n", sep = "")
  cat(
    "  review proportion among candidates: ",
    metrics$review_candidate_proportion, "\n", sep = ""
  )
  invisible(x)
}

#' @export
summary.epi_linkage_validation <- function(object, ...) {
  object$metrics
}

#' @export
str.epi_linkage_validation <- function(object, ...) {
  print(object)
  invisible(object)
}
