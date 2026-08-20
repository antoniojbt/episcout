library(episcout)
library(testthat)

context("probabilistic linkage foundation")

test_that("text profiles are explicit and validated", {
  profile <- epi_linkage_text_profile(
    unicode = "NFKC",
    case = "fold",
    diacritics = "strip",
    punctuation = "space",
    whitespace = "collapse",
    token_order = "sort",
    drop_tokens = c("SENSITIVE-TOKEN-ONE", "SENSITIVE-TOKEN-TWO")
  )

  expect_s3_class(profile, "epi_linkage_text_profile")
  expect_identical(
    profile$drop_tokens,
    c("SENSITIVE-TOKEN-ONE", "SENSITIVE-TOKEN-TWO")
  )
  expect_error(epi_linkage_text_profile(unicode = "ASCII"), "unicode")
  expect_error(epi_linkage_text_profile(drop_tokens = c("de", "de")), "unique")
  expect_error(epi_linkage_text_profile(drop_tokens = NA_character_), "missing")
  expect_error(epi_linkage_text_profile(drop_tokens = " "), "blank")
  expect_output(print(profile), "explicit derived-text profile")
  expect_equal(summary(profile)$n_explicit_drop_tokens, 2)
  expect_false(any(grepl(
    "drop_tokens", capture.output(print(profile)), fixed = TRUE
  )))
  expect_false(any(grepl(
    "SENSITIVE-TOKEN", capture.output(str(profile)), fixed = TRUE
  )))
})

test_that("specification validates declarations without inferred defaults", {
  spec <- linkage_foundation_spec()

  expect_s3_class(spec, "epi_linkage_spec")
  expect_identical(names(spec$profiles), c("identity", "latin", "latin_sorted"))
  expect_identical(spec$blocks$pass, c(1L, 1L, 2L))
  expect_false(summary(spec)$model_declared)
  expect_output(print(spec), "blocking passes: 2")

  blocks <- spec$blocks[0, ]
  expect_error(
    epi_linkage_spec("record_id", "source_key", blocks = blocks,
                     comparisons = spec$comparisons, max_candidates = 10),
    "at least one"
  )
  invalid <- spec$comparisons
  invalid$method[[1L]] <- "soundex"
  expect_error(
    epi_linkage_spec("record_id", "source_key",
                     profiles = spec$profiles[names(spec$profiles) != "identity"],
                     blocks = spec$blocks,
                     comparisons = invalid, max_candidates = 10),
    "supported"
  )
  expect_error(
    epi_linkage_spec("record_id", "source_key",
                     profiles = spec$profiles[names(spec$profiles) != "identity"],
                     blocks = spec$blocks,
                     comparisons = spec$comparisons, max_candidates = Inf),
    "finite whole number"
  )
})

test_that("preparation preserves sources and derives declared text only", {
  sources <- linkage_foundation_sources()
  original_x <- sources$x
  original_y <- sources$y

  prepared <- epi_linkage_prepare(sources$x, sources$y, linkage_foundation_spec())
  candidates <- epi_linkage_candidates(prepared)
  evidence <- epi_linkage_compare(candidates, include_values = TRUE)$evidence

  expect_s3_class(prepared, "epi_linkage_prepared")
  expect_identical(sources$x, original_x)
  expect_identical(sources$y, original_y)
  jose <- evidence[
    evidence$x_id == "x1" & evidence$y_id == "y1" &
      evidence$comparison == "name_tokens",
  ]
  expect_identical(jose$x_value, "garcia hernandez jose luis")
  expect_identical(jose$y_value, "garcia hernandez jose luis")
  maria <- evidence[
    evidence$x_id == "x2" & evidence$y_id == "y2" &
      evidence$comparison == "name_tokens",
  ]
  expect_identical(maria$x_value, "carmen cruz de del la maria")
  expect_identical(maria$y_value, "carmen cruz de del la maria")
  expect_output(print(prepared), "source values not printed")
  expect_named(summary(prepared), c("contract_version", "n_x", "n_y"))
})

test_that("tokens are retained or dropped only through explicit profiles", {
  sources <- linkage_foundation_sources()
  retained <- linkage_foundation_spec()
  dropped <- linkage_foundation_spec()
  dropped$profiles$latin_sorted <- epi_linkage_text_profile(
    diacritics = "strip",
    token_order = "sort",
    drop_tokens = c("de", "del", "la")
  )

  retained_values <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, retained)
  ), include_values = TRUE)$evidence
  dropped_values <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, dropped)
  ), include_values = TRUE)$evidence
  select_maria <- function(value) {
    value[
      value$x_id == "x2" & value$y_id == "y2" &
        value$comparison == "name_tokens",
    ]
  }

  expect_identical(
    select_maria(retained_values)$x_value,
    "carmen cruz de del la maria"
  )
  expect_identical(select_maria(dropped_values)$x_value, "carmen cruz maria")
  expect_identical(select_maria(dropped_values)$y_value, "carmen cruz maria")
})

test_that("blocking passes are bounded, deterministic and reconciled", {
  sources <- linkage_foundation_sources()
  candidates <- epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, linkage_foundation_spec())
  )

  expect_s3_class(candidates, "epi_linkage_candidates")
  expect_identical(
    candidates$pairs,
    data.frame(
      x_index = c(1L, 2L, 3L, 3L, 4L),
      y_index = c(1L, 2L, 3L, 4L, 5L)
    )
  )
  expect_identical(
    unname(as.numeric(candidates$diagnostics$overall[1, 1:5])),
    c(4, 5, 20, 5, 15)
  )
  expect_equal(candidates$diagnostics$overall$reduction_ratio, 0.75)
  expect_identical(
    candidates$diagnostics$passes$n_candidates_before_union,
    c(5, 2)
  )
  expect_identical(candidates$diagnostics$passes$n_new_candidates, c(5, 0))
  expect_identical(
    candidates$diagnostics$passes$n_duplicate_candidates,
    c(0, 2)
  )
  expect_output(print(candidates), "candidates: 5")
  expect_named(
    summary(candidates),
    c("n_x", "n_y", "n_possible", "n_candidates", "reduction_n",
      "reduction_ratio")
  )

  expect_error(
    epi_linkage_candidates(
      epi_linkage_prepare(sources$x, sources$y, linkage_foundation_spec(4))
    ),
    "exceeds max_candidates"
  )
})

test_that("field comparisons distinguish similarity, disagreement and missingness", {
  sources <- linkage_foundation_sources()
  comparison <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, linkage_foundation_spec())
  ))
  evidence <- comparison$evidence

  expect_s3_class(comparison, "epi_linkage_comparisons")
  expect_named(
    evidence,
    c("x_index", "y_index", "comparison", "similarity", "comparison_state")
  )
  expect_equal(nrow(evidence), 30L)
  expect_false(any(c("x_id", "y_id", "x_value", "y_value") %in% names(evidence)))

  both_missing <- evidence[
    evidence$x_index == 4L & evidence$y_index == 5L &
      evidence$comparison == "birth_date",
  ]
  expect_identical(both_missing$comparison_state, "missing")
  expect_true(is.na(both_missing$similarity))

  typo <- evidence[
    evidence$x_index == 3L & evidence$y_index == 3L &
      evidence$comparison == "name_tokens",
  ]
  expect_equal(typo$similarity, 0.5)
  expect_identical(typo$comparison_state, "disagree")

  homonym_date <- evidence[
    evidence$x_index == 3L & evidence$y_index == 4L &
      evidence$comparison == "birth_date",
  ]
  expect_identical(homonym_date$similarity, 0)
  expect_identical(homonym_date$comparison_state, "disagree")

  sources$x$full_name[[1L]] <- NA_character_
  one_side_missing <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(sources$x, sources$y, linkage_foundation_spec())
  ))$evidence
  one_side_missing <- one_side_missing[
    one_side_missing$x_index == 1L & one_side_missing$y_index == 1L &
      one_side_missing$comparison == "name_jw",
  ]
  expect_identical(one_side_missing$comparison_state, "missing")
  expect_true(is.na(one_side_missing$similarity))

  expect_output(print(comparison), "value-bearing evidence: FALSE")
  expect_equal(summary(comparison)$n_field_evidence, 30)
})

test_that("zero-row and missing-block inputs retain typed safe output", {
  spec <- linkage_foundation_spec()
  sources <- linkage_foundation_sources()
  empty_x <- sources$x[0, ]
  empty_y <- sources$y[0, ]

  zero <- epi_linkage_compare(epi_linkage_candidates(
    epi_linkage_prepare(empty_x, empty_y, spec)
  ))
  expect_equal(nrow(zero$evidence), 0L)
  expect_type(zero$evidence$x_index, "integer")
  expect_equal(zero$candidate_diagnostics$overall$n_possible, 0)
  expect_true(is.na(zero$candidate_diagnostics$overall$reduction_ratio))

  sources$x$geography[[1L]] <- NA_character_
  sources$x$birth_date[[1L]] <- as.Date(NA)
  candidates <- epi_linkage_candidates(epi_linkage_prepare(
    sources$x, sources$y, spec
  ))
  expect_false(any(candidates$pairs$x_index == 1L))
})

test_that("unsupported types and source defects fail without values", {
  sources <- linkage_foundation_sources()
  spec <- linkage_foundation_spec()

  sources$x$record_id[[2L]] <- sources$x$record_id[[1L]]
  expect_error(epi_linkage_prepare(sources$x, sources$y, spec), "unique")
  sources <- linkage_foundation_sources()
  sources$x$age[[1L]] <- Inf
  expect_error(epi_linkage_prepare(sources$x, sources$y, spec), "NaN or infinity")
  sources <- linkage_foundation_sources()
  sources$x$full_name <- I(as.list(sources$x$full_name))
  expect_error(epi_linkage_prepare(sources$x, sources$y, spec), "character or factor")

  canary <- "SENSITIVE-CANARY-NAME"
  sources <- linkage_foundation_sources()
  sources$x$record_id[[2L]] <- sources$x$record_id[[1L]]
  sources$x$full_name[[1L]] <- canary
  message <- tryCatch(
    epi_linkage_prepare(sources$x, sources$y, spec),
    error = conditionMessage
  )
  expect_false(grepl(canary, message, fixed = TRUE))

  sources <- linkage_foundation_sources()
  sources$x$full_name[[1L]] <- canary
  prepared <- epi_linkage_prepare(sources$x, sources$y, spec)
  candidates <- epi_linkage_candidates(prepared)
  comparison <- epi_linkage_compare(candidates, include_values = TRUE)
  routine_output <- c(
    capture.output(print(prepared)), capture.output(str(prepared)),
    capture.output(print(candidates)), capture.output(str(candidates)),
    capture.output(print(comparison)), capture.output(str(comparison))
  )
  expect_false(any(grepl(canary, routine_output, fixed = TRUE)))
})

test_that("model and thresholds are declarations rather than package defaults", {
  base <- linkage_foundation_spec()
  parameters <- data.frame(
    comparison = base$comparisons$comparison,
    m_probability = rep(0.9, nrow(base$comparisons)),
    u_probability = rep(0.1, nrow(base$comparisons)),
    stringsAsFactors = FALSE
  )
  model <- list(parameters = parameters, match_prevalence = 0.05)
  thresholds <- list(
    metric = "model_posterior", nonmatch_max = 0.2, match_min = 0.8
  )
  declared <- epi_linkage_spec(
    base$x_id, base$y_id,
    profiles = base$profiles[names(base$profiles) != "identity"],
    blocks = base$blocks,
    comparisons = base$comparisons,
    max_candidates = base$max_candidates,
    model = model,
    thresholds = thresholds
  )

  expect_true(summary(declared)$model_declared)
  expect_true(summary(declared)$thresholds_declared)
  parameters$u_probability[[1L]] <- 0.95
  expect_error(
    epi_linkage_spec(
      base$x_id, base$y_id,
      profiles = base$profiles[names(base$profiles) != "identity"],
      blocks = base$blocks, comparisons = base$comparisons,
      max_candidates = 100,
      model = list(parameters = parameters, match_prevalence = 0.05)
    ),
    "0 < u_probability"
  )
  expect_error(
    epi_linkage_spec(
      base$x_id, base$y_id,
      profiles = base$profiles[names(base$profiles) != "identity"],
      blocks = base$blocks, comparisons = base$comparisons,
      max_candidates = 100,
      thresholds = thresholds
    ),
    "require a declared model"
  )
})
