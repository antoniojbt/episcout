library(episcout)
library(testthat)

context("probabilistic linkage scoring and validation")

test_that("Fellegi-Sunter weights and posteriors match hand calculations", {
  sources <- linkage_foundation_sources()
  spec <- linkage_scoring_spec()
  comparisons <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, spec)
  ))
  scores <- epi_linkage_score(comparisons)

  expect_s3_class(scores, "epi_linkage_scores")
  expect_named(
    scores$pair_scores,
    c("x_index", "y_index", "n_agree", "n_disagree", "n_missing",
      "linkage_weight", "model_posterior")
  )
  expect_true("weight_contribution" %in% names(scores$field_evidence))
  expect_match(
    scores$metadata$probability_semantics,
    "not_empirically_calibrated"
  )

  homonym <- scores$pair_scores[
    scores$pair_scores$x_index == 3L & scores$pair_scores$y_index == 4L,
  ]
  expected_weight <- sum(c(
    log2(0.8 / 0.1),
    log2(0.85 / 0.2),
    log2((1 - 0.99) / (1 - 0.02)),
    log2((1 - 0.9) / (1 - 0.2)),
    log2(0.95 / 0.5),
    log2(0.9 / 0.25)
  ))
  prior_odds <- 0.05 / 0.95
  expected_odds <- prior_odds * 2^expected_weight
  expected_posterior <- expected_odds / (1 + expected_odds)

  expect_equal(homonym$n_agree, 4)
  expect_equal(homonym$n_disagree, 2)
  expect_equal(homonym$n_missing, 0)
  expect_equal(homonym$linkage_weight, expected_weight, tolerance = 1e-12)
  expect_equal(homonym$model_posterior, expected_posterior, tolerance = 1e-12)
  expect_output(print(scores), "not empirically calibrated")
  expect_false(summary(scores)$empirically_calibrated)
})

test_that("missing field evidence contributes zero and preserves the prior", {
  x <- data.frame(id = "x1", block = "A", name = NA_character_)
  y <- data.frame(id = "y1", block = "A", name = NA_character_)
  spec <- epi_linkage_spec(
    x_id = "id",
    y_id = "id",
    blocks = data.frame(
      pass = 1L, x_field = "block", y_field = "block",
      profile = "identity", stringsAsFactors = FALSE
    ),
    comparisons = data.frame(
      comparison = "name", x_field = "name", y_field = "name",
      profile = "identity", method = "exact", parameter = 1,
      stringsAsFactors = FALSE
    ),
    max_candidates = 1,
    model = list(
      parameters = data.frame(
        comparison = "name", m_probability = 0.9, u_probability = 0.1
      ),
      match_prevalence = 0.2
    ),
    thresholds = list(
      metric = "model_posterior", nonmatch_max = 0.1, match_min = 0.8
    )
  )
  result <- epi_linkage_run(x, y, spec)

  expect_equal(result$field_evidence$weight_contribution, 0)
  expect_equal(result$pair_scores$linkage_weight, 0)
  expect_equal(result$pair_scores$model_posterior, 0.2)
  expect_identical(result$decisions$decision, "review")
})

test_that("caller thresholds create exact match review and non-match regions", {
  sources <- linkage_foundation_sources()
  spec <- linkage_scoring_spec()
  scores <- epi_linkage_score(epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, spec)
  )))
  scores$pair_scores <- scores$pair_scores[1:3, ]
  scores$pair_scores$model_posterior <- c(0.05, 0.5, 0.9)

  result <- epi_linkage_classify(scores)
  expect_identical(result$decisions$decision, c("non_match", "review", "match"))

  complete <- epi_linkage_run(sources$x, sources$y, spec)
  expect_named(
    complete,
    c("metadata", "candidate_diagnostics", "pair_scores", "field_evidence",
      "decisions")
  )
  expect_identical(
    complete$decisions$decision,
    c("match", "match", "match", "non_match", "review")
  )
  expect_output(print(complete), "no crosswalk or registry was written")
  expect_identical(
    unname(as.numeric(summary(complete)[1, 1:4])),
    c(5, 3, 1, 1)
  )
})

test_that("complete truth separates blocking recall and classifier performance", {
  sources <- linkage_foundation_sources()
  result <- epi_linkage_run(sources$x, sources$y, linkage_scoring_spec())
  validation <- epi_linkage_validate(result, linkage_complete_truth())
  metrics <- validation$metrics

  expect_s3_class(validation, "epi_linkage_validation")
  expect_equal(metrics$n_possible_pairs, 20)
  expect_equal(metrics$n_candidates, 5)
  expect_equal(metrics$candidate_recall_numerator, 4)
  expect_equal(metrics$candidate_recall_denominator, 4)
  expect_equal(metrics$candidate_recall, 1)
  expect_equal(metrics$precision_numerator, 3)
  expect_equal(metrics$precision_denominator, 3)
  expect_equal(metrics$precision, 1)
  expect_equal(metrics$recall_numerator, 3)
  expect_equal(metrics$recall_denominator, 4)
  expect_equal(metrics$recall, 0.75)
  expect_equal(metrics$false_match_n, 0)
  expect_equal(metrics$missed_match_n, 0)
  expect_equal(metrics$true_match_review_n, 1)
  expect_equal(metrics$review_n, 1)
  expect_equal(metrics$review_candidate_proportion, 0.2)
  expect_equal(metrics$review_possible_proportion, 0.05)
  expect_output(print(validation), "candidate recall: 1")
  expect_identical(summary(validation), metrics)
})

test_that("blocking misses remain visible in candidate recall and missed matches", {
  sources <- linkage_foundation_sources()
  birth_block <- data.frame(
    pass = 1L,
    x_field = "birth_date",
    y_field = "dob",
    profile = "identity",
    stringsAsFactors = FALSE
  )
  result <- epi_linkage_run(
    sources$x, sources$y, linkage_scoring_spec(blocks = birth_block)
  )
  metrics <- epi_linkage_validate(result, linkage_complete_truth())$metrics

  expect_equal(metrics$n_candidates, 2)
  expect_equal(metrics$candidate_recall_numerator, 2)
  expect_equal(metrics$candidate_recall_denominator, 4)
  expect_equal(metrics$candidate_recall, 0.5)
  expect_equal(metrics$recall, 0.5)
  expect_equal(metrics$missed_match_n, 2)
  expect_equal(metrics$missed_match_rate, 0.5)
  expect_equal(metrics$true_match_review_n, 0)
})

test_that("false matches and zero denominators are explicit", {
  sources <- linkage_foundation_sources()
  permissive <- linkage_scoring_spec(nonmatch_max = 0.005, match_min = 0.01)
  metrics <- epi_linkage_validate(
    epi_linkage_run(sources$x, sources$y, permissive),
    linkage_complete_truth()
  )$metrics

  expect_equal(metrics$precision_numerator, 4)
  expect_equal(metrics$precision_denominator, 5)
  expect_equal(metrics$precision, 0.8)
  expect_equal(metrics$false_match_n, 1)
  expect_equal(metrics$false_match_rate, 0.2)
  expect_equal(metrics$recall, 1)

  strict <- linkage_scoring_spec(nonmatch_max = 0.999, match_min = 1)
  no_classified_match <- epi_linkage_validate(
    epi_linkage_run(sources$x, sources$y, strict),
    linkage_complete_truth()
  )$metrics
  expect_equal(no_classified_match$precision_denominator, 0)
  expect_true(is.na(no_classified_match$precision))
  expect_true(is.na(no_classified_match$false_match_rate))

  no_match_truth <- linkage_complete_truth()
  no_match_truth$is_match <- FALSE
  no_true_match <- epi_linkage_validate(
    epi_linkage_run(sources$x, sources$y, strict), no_match_truth
  )$metrics
  expect_true(is.na(no_true_match$candidate_recall))
  expect_true(is.na(no_true_match$recall))
  expect_true(is.na(no_true_match$missed_match_rate))
})

test_that("truth must be complete, unique, typed and in range", {
  sources <- linkage_foundation_sources()
  result <- epi_linkage_run(sources$x, sources$y, linkage_scoring_spec())
  truth <- linkage_complete_truth()

  expect_error(epi_linkage_validate(result, truth[-1, ]), "every Cartesian pair")
  duplicate <- truth
  duplicate[2, ] <- duplicate[1, ]
  expect_error(epi_linkage_validate(result, duplicate), "every Cartesian pair")
  missing <- truth
  missing$is_match[[1L]] <- NA
  expect_error(epi_linkage_validate(result, missing), "non-missing logical")
  outside <- truth
  outside$x_index[[1L]] <- 5L
  expect_error(epi_linkage_validate(result, outside), "within the two sources")
  wrong_schema <- truth[c("y_index", "x_index", "is_match")]
  expect_error(epi_linkage_validate(result, wrong_schema), "exactly these fields")
})

test_that("zero-row sources retain typed scoring and validation output", {
  sources <- linkage_foundation_sources()
  result <- epi_linkage_run(
    sources$x[0, ], sources$y[0, ], linkage_scoring_spec()
  )
  truth <- data.frame(
    x_index = integer(), y_index = integer(), is_match = logical()
  )
  validation <- epi_linkage_validate(result, truth)

  expect_equal(nrow(result$pair_scores), 0L)
  expect_equal(nrow(result$field_evidence), 0L)
  expect_equal(nrow(result$decisions), 0L)
  expect_type(result$pair_scores$model_posterior, "double")
  expect_equal(validation$metrics$n_possible_pairs, 0)
  expect_true(is.na(validation$metrics$review_candidate_proportion))
  expect_true(is.na(validation$metrics$review_possible_proportion))
})

test_that("end-to-end output is deterministic and routine methods redact values", {
  sources <- linkage_foundation_sources()
  sources$x$full_name[[1L]] <- "SENSITIVE-SCORING-CANARY"
  spec <- linkage_scoring_spec()
  first <- epi_linkage_run(sources$x, sources$y, spec, include_values = TRUE)
  second <- epi_linkage_run(sources$x, sources$y, spec, include_values = TRUE)

  expect_identical(first, second)
  routine <- c(
    capture.output(print(first)), capture.output(summary(first)),
    capture.output(str(first))
  )
  expect_false(any(grepl("SENSITIVE-SCORING-CANARY", routine, fixed = TRUE)))
  validation <- epi_linkage_validate(first, linkage_complete_truth())
  expect_false(any(grepl(
    "SENSITIVE-SCORING-CANARY", capture.output(str(validation)), fixed = TRUE
  )))
  expect_false(any(vapply(validation, function(value) {
    any(grepl("SENSITIVE-SCORING-CANARY", capture.output(value), fixed = TRUE))
  }, logical(1L))))
})

test_that("source order changes indices but not record-key decisions", {
  sources <- linkage_foundation_sources()
  x_original <- sources$x
  y_original <- sources$y
  first <- epi_linkage_run(
    sources$x, sources$y, linkage_scoring_spec(), include_values = TRUE
  )
  reordered <- epi_linkage_run(
    sources$x[c(4, 2, 1, 3), ],
    sources$y[c(5, 3, 1, 4, 2), ],
    linkage_scoring_spec(),
    include_values = TRUE
  )
  decision_by_key <- function(result) {
    keys <- unique(result$field_evidence[c(
      "x_index", "y_index", "x_id", "y_id"
    )])
    keyed <- merge(
      keys, result$decisions,
      by = c("x_index", "y_index"),
      sort = FALSE
    )
    keyed <- keyed[order(keyed$x_id, keyed$y_id), c("x_id", "y_id", "decision")]
    rownames(keyed) <- NULL
    keyed
  }

  expect_identical(decision_by_key(first), decision_by_key(reordered))
  expect_identical(sources$x, x_original)
  expect_identical(sources$y, y_original)
})

test_that("scoring and classification require explicit complete declarations", {
  sources <- linkage_foundation_sources()
  foundation <- linkage_foundation_spec()
  comparisons <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, foundation)
  ))
  expect_error(epi_linkage_score(comparisons), "caller-supplied")
  expect_error(
    epi_linkage_run(sources$x, sources$y, foundation),
    "complete model"
  )

  spec <- linkage_scoring_spec()
  scores <- epi_linkage_score(epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, spec)
  )))
  scores$spec$thresholds <- NULL
  expect_error(epi_linkage_classify(scores), "caller-supplied")
  expect_error(epi_linkage_validate(scores, linkage_complete_truth()), "result")
})

test_that("model and threshold declarations fail closed", {
  base <- linkage_foundation_spec()
  valid_parameters <- data.frame(
    comparison = base$comparisons$comparison,
    m_probability = rep(0.9, nrow(base$comparisons)),
    u_probability = rep(0.1, nrow(base$comparisons)),
    stringsAsFactors = FALSE
  )
  declare <- function(parameters = valid_parameters,
                      prevalence = 0.05,
                      nonmatch_max = 0.1,
                      match_min = 0.9) {
    epi_linkage_spec(
      x_id = base$x_id,
      y_id = base$y_id,
      profiles = base$profiles[names(base$profiles) != "identity"],
      blocks = base$blocks,
      comparisons = base$comparisons,
      max_candidates = base$max_candidates,
      model = list(
        parameters = parameters,
        match_prevalence = prevalence
      ),
      thresholds = list(
        metric = "model_posterior",
        nonmatch_max = nonmatch_max,
        match_min = match_min
      )
    )
  }

  expect_s3_class(declare(), "epi_linkage_spec")
  expect_error(declare(valid_parameters[-1, ]), "every comparison")
  extra <- rbind(valid_parameters, valid_parameters[1, ])
  extra$comparison[[nrow(extra)]] <- "extra"
  expect_error(declare(extra), "every comparison")
  invalid_probability <- valid_parameters
  invalid_probability$m_probability[[1L]] <- 1
  expect_error(declare(invalid_probability), "0 < u_probability")
  invalid_probability <- valid_parameters
  invalid_probability$u_probability[[1L]] <- 0
  expect_error(declare(invalid_probability), "0 < u_probability")
  invalid_probability <- valid_parameters
  invalid_probability$m_probability[[1L]] <- NA_real_
  expect_error(declare(invalid_probability), "Model probabilities")
  expect_error(declare(prevalence = 0), "strictly between")
  expect_error(declare(prevalence = 1), "strictly between")
  expect_error(declare(prevalence = NA_real_), "strictly between")
  expect_error(declare(nonmatch_max = 0.9, match_min = 0.9), "nonmatch_max")
  expect_error(declare(nonmatch_max = 0.95, match_min = 0.9), "nonmatch_max")
  expect_error(declare(nonmatch_max = -0.1), "nonmatch_max")
  expect_error(declare(match_min = 1.1), "nonmatch_max")
  expect_error(declare(match_min = NA_real_), "nonmatch_max")
})
