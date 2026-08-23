context("aggregate-count Cramer's V contract")

association_counts <- function(left,
                               right,
                               row_level,
                               column_level,
                               n) {
  data.frame(
    left = left,
    right = right,
    row_level = row_level,
    column_level = column_level,
    n = n,
    stringsAsFactors = FALSE
  )
}

test_that("hand-derived 2 by 2 Cramer's V truth is exact", {
  counts <- association_counts(
    "group", "outcome",
    rep(c("r1", "r2"), each = 2L),
    rep(c("c1", "c2"), 2L),
    c(30, 10, 10, 50)
  )

  observed <- epi_eda_cramers_v(counts)

  expect_named(observed, c(
    "left", "right", "n", "active_rows", "active_columns",
    "cramers_v", "status", "reason"
  ))
  expect_identical(observed$left, "group")
  expect_identical(observed$right, "outcome")
  expect_identical(observed$n, 100)
  expect_identical(observed$active_rows, 2L)
  expect_identical(observed$active_columns, 2L)
  expect_equal(observed$cramers_v, 0.5833333333333334, tolerance = 1e-15)
  expect_identical(observed$status, "available")
  expect_true(is.na(observed$reason))
})

test_that("hand-derived 3 by 2 Cramer's V truth is exact", {
  counts <- association_counts(
    "group", "outcome",
    rep(c("r1", "r2", "r3"), each = 2L),
    rep(c("c1", "c2"), 3L),
    c(20, 10, 0, 10, 20, 10)
  )

  observed <- epi_eda_cramers_v(counts)

  expect_identical(observed$n, 70)
  expect_identical(observed$active_rows, 3L)
  expect_identical(observed$active_columns, 2L)
  expect_equal(observed$cramers_v, 0.4714045207910317, tolerance = 1e-15)
})

test_that("explicit inactive margins and row permutations do not change V", {
  counts <- association_counts(
    "group", "outcome",
    c("r1", "r1", "r2", "r2", "r3", "r1"),
    c("c1", "c2", "c1", "c2", "c1", "c3"),
    c(30, 10, 10, 50, 0, 0)
  )

  ordinary <- epi_eda_cramers_v(counts)
  permuted <- epi_eda_cramers_v(counts[rev(seq_len(nrow(counts))), , drop = FALSE])

  expect_identical(ordinary$n, 100)
  expect_identical(ordinary$active_rows, 2L)
  expect_identical(ordinary$active_columns, 2L)
  expect_equal(ordinary$cramers_v, 0.5833333333333334, tolerance = 1e-15)
  expect_identical(permuted, ordinary)
})

test_that("stratified categorical aggregates require no second source calculation", {
  data <- data.frame(
    group = rep(c("r1", "r2"), c(40L, 60L)),
    outcome = c(
      rep("c1", 30L), rep("c2", 10L),
      rep("c1", 10L), rep("c2", 50L)
    ),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("group", "outcome"),
    label = c("Group", "Outcome"),
    database_type = "text",
    analysis_type = "categorical",
    role = "measure",
    levels = c("r1;r2", "c1;c2"),
    stringsAsFactors = FALSE
  )
  stratified <- epi_eda_profile_stratified(
    data,
    spec,
    "group",
    include_overall = FALSE,
    include_missing_stratum = FALSE,
    max_levels = 2
  )
  categorical <- stratified$categorical[
    stratified$categorical$name == "outcome" &
      !stratified$categorical$is_missing_level,
    ,
    drop = FALSE
  ]
  counts <- association_counts(
    stratified$metadata$strata,
    categorical$name,
    categorical$group_value,
    categorical$level,
    categorical$n
  )
  rm(data)

  observed <- epi_eda_cramers_v(counts)

  expect_identical(observed$n, 100)
  expect_equal(observed$cramers_v, 0.5833333333333334, tolerance = 1e-15)
})

test_that("pair output order follows first occurrence", {
  second <- association_counts(
    "z", "b", rep(c("r1", "r2"), each = 2L),
    rep(c("c1", "c2"), 2L), c(1, 0, 0, 1)
  )
  first <- association_counts(
    "a", "c", rep(c("r1", "r2"), each = 2L),
    rep(c("c1", "c2"), 2L), c(1, 1, 1, 1)
  )

  observed <- epi_eda_cramers_v(rbind(second, first))

  expect_identical(observed$left, c("z", "a"))
  expect_identical(observed$right, c("b", "c"))
  expect_equal(observed$cramers_v, c(1, 0))
})

test_that("zero and one-dimensional tables return typed unavailable evidence", {
  zero <- association_counts(
    "x", "y", rep(c("r1", "r2"), each = 2L),
    rep(c("c1", "c2"), 2L), rep(0, 4L)
  )
  one_row <- association_counts(
    "x", "y", c("r1", "r1", "r2"), c("c1", "c2", "c1"), c(1, 1, 0)
  )
  one_column <- association_counts(
    "x", "y", c("r1", "r2", "r1"), c("c1", "c1", "c2"), c(1, 1, 0)
  )
  one_cell <- association_counts("x", "y", "r1", "c1", 1)

  observed <- lapply(
    list(zero, one_row, one_column, one_cell),
    epi_eda_cramers_v
  )

  expect_identical(
    vapply(observed, `[[`, character(1), "status"),
    rep("unavailable", 4L)
  )
  expect_true(all(vapply(observed, function(value) is.na(value$cramers_v), logical(1))))
  expect_identical(vapply(observed, `[[`, character(1), "reason"), c(
    "The contingency table has zero total count.",
    "The contingency table has fewer than two active rows.",
    "The contingency table has fewer than two active columns.",
    "The contingency table has fewer than two active rows and columns."
  ))
  expect_true(all(vapply(observed, function(value) is.double(value$n), logical(1))))
  expect_true(all(vapply(observed, function(value) is.integer(value$active_rows), logical(1))))
  expect_true(all(vapply(observed, function(value) is.integer(value$active_columns), logical(1))))
})

test_that("typed empty counts return the exact typed result", {
  counts <- association_counts(
    character(), character(), character(), character(), numeric()
  )

  observed <- epi_eda_cramers_v(counts)

  expect_identical(observed, data.frame(
    left = character(), right = character(), n = numeric(),
    active_rows = integer(), active_columns = integer(),
    cramers_v = numeric(), status = character(), reason = character(),
    stringsAsFactors = FALSE
  ))
})

test_that("invalid aggregate cells fail before calculation", {
  valid <- association_counts("x", "y", "r1", "c1", 1)

  expect_error(epi_eda_cramers_v(data.frame()), "must contain")
  expect_error(
    epi_eda_cramers_v(transform(valid, left = factor(left))),
    "character columns"
  )
  expect_error(
    epi_eda_cramers_v(transform(valid, row_level = NA_character_)),
    "cannot be missing"
  )
  expect_error(epi_eda_cramers_v(transform(valid, left = "")), "cannot be blank")
  expect_error(epi_eda_cramers_v(transform(valid, n = NA_real_)), "safe non-negative")
  expect_error(epi_eda_cramers_v(transform(valid, n = -1)), "safe non-negative")
  expect_error(epi_eda_cramers_v(transform(valid, n = 0.5)), "safe non-negative")
  expect_error(
    epi_eda_cramers_v(transform(valid, n = 9007199254740992)),
    "safe non-negative"
  )
  expect_error(epi_eda_cramers_v(rbind(valid, valid)), "unique")

  unsafe_total <- rbind(
    transform(valid, row_level = "r1", n = 4503599627370496),
    transform(valid, row_level = "r2", n = 4503599627370496)
  )
  expect_error(epi_eda_cramers_v(unsafe_total), "pair total exceeds")
})
