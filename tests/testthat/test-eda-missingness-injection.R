context("deterministic EDA fixture missingness injection")

library(testthat)
library(episcout)

neutral_fixture <- function() {
  data.frame(
    measure = 11:16,
    group = letters[1:6],
    date = as.Date("2020-01-01") + 0:5,
    row.names = paste0("row_", 1:6),
    stringsAsFactors = FALSE
  )
}

test_that("exact seeded injections are deterministic and non-overlapping", {
  data <- neutral_fixture()
  observed_a <- epi_eda_inject_missingness(
    data,
    missing = c(measure = 2, group = 2),
    blanks = c(group = 2),
    seed = 17
  )
  observed_b <- epi_eda_inject_missingness(
    data,
    missing = c(measure = 2, group = 2),
    blanks = c(group = 2),
    seed = 17
  )

  expect_named(observed_a, c("data", "counts", "metadata"))
  expect_identical(observed_a, observed_b)
  expect_identical(names(observed_a$data), names(data))
  expect_identical(row.names(observed_a$data), row.names(data))
  expect_identical(observed_a$data$date, data$date)
  expect_identical(
    observed_a$counts,
    data.frame(
      variable = c("measure", "group"),
      n_missing = c(2L, 2L),
      n_blank = c(0L, 2L),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(sum(is.na(observed_a$data$measure)), 2L)
  expect_identical(sum(is.na(observed_a$data$group)), 2L)
  expect_identical(sum(observed_a$data$group == "", na.rm = TRUE), 2L)
  expect_length(
    intersect(
      which(is.na(observed_a$data$group)),
      which(observed_a$data$group == "")
    ),
    0L
  )

  changed_measure <- which(is.na(observed_a$data$measure))
  unchanged_measure <- setdiff(seq_len(nrow(data)), changed_measure)
  expect_identical(observed_a$data$measure[unchanged_measure], data$measure[unchanged_measure])
  changed_group <- which(is.na(observed_a$data$group) | observed_a$data$group == "")
  unchanged_group <- setdiff(seq_len(nrow(data)), changed_group)
  expect_identical(observed_a$data$group[unchanged_group], data$group[unchanged_group])
})

test_that("different seeds change eligible positions while preserving counts", {
  data <- neutral_fixture()
  observed_a <- epi_eda_inject_missingness(data, c(measure = 2), seed = 1)
  observed_b <- epi_eda_inject_missingness(data, c(measure = 2), seed = 2)

  expect_false(identical(observed_a$data, observed_b$data))
  expect_identical(sum(is.na(observed_a$data$measure)), 2L)
  expect_identical(sum(is.na(observed_b$data$measure)), 2L)
})

test_that("injection preserves existing missing and blank values", {
  data <- neutral_fixture()
  data$measure[[1]] <- NA_integer_
  data$group[[2]] <- ""
  data$group[[3]] <- NA_character_

  observed <- epi_eda_inject_missingness(
    data,
    missing = c(measure = 2, group = 1),
    blanks = c(group = 1),
    seed = 9
  )

  expect_true(is.na(observed$data$measure[[1]]))
  expect_identical(observed$data$group[[2]], "")
  expect_true(is.na(observed$data$group[[3]]))
  expect_identical(sum(is.na(observed$data$measure)), 3L)
  expect_identical(sum(is.na(observed$data$group)), 2L)
  expect_identical(sum(observed$data$group == "", na.rm = TRUE), 2L)
})

test_that("explicit seeds restore the caller random-number state", {
  data <- neutral_fixture()
  set.seed(321)
  before <- .Random.seed
  epi_eda_inject_missingness(data, c(measure = 1), seed = 22)
  expect_identical(.Random.seed, before)

  rm(".Random.seed", envir = .GlobalEnv)
  epi_eda_inject_missingness(data, c(measure = 1), seed = 22)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("zero counts and zero-row data are deterministic", {
  data <- neutral_fixture()[integer(), , drop = FALSE]
  observed <- epi_eda_inject_missingness(
    data,
    missing = c(measure = 0),
    blanks = c(group = 0),
    seed = 1
  )

  expect_identical(observed$data, data)
  expect_identical(observed$counts$n_missing, c(0L, 0L))
  expect_identical(observed$counts$n_blank, c(0L, 0L))
  expect_identical(observed$metadata$n_rows, 0L)

  expect_error(
    epi_eda_inject_missingness(data, c(measure = 1), seed = 1),
    "exceed the eligible values"
  )
})

test_that("invalid declarations and seeds fail without a result", {
  data <- neutral_fixture()

  expect_error(epi_eda_inject_missingness(1:3, c(measure = 1)), "data frame")
  expect_error(
    epi_eda_inject_missingness(setNames(data, c("", "group", "date")), c(group = 1)),
    "unique, non-empty"
  )
  expect_error(epi_eda_inject_missingness(data, 1), "named by variable")
  expect_error(
    epi_eda_inject_missingness(data, c(measure = 1, measure = 1)),
    "duplicate"
  )
  expect_error(epi_eda_inject_missingness(data, c(other = 1)), "unknown variable")
  expect_error(epi_eda_inject_missingness(data, c(measure = -1)), "non-negative")
  expect_error(epi_eda_inject_missingness(data, c(measure = 1.5)), "whole-number")
  expect_error(
    epi_eda_inject_missingness(data, c(measure = 0), blanks = c(measure = 1)),
    "only into character"
  )
  expect_error(epi_eda_inject_missingness(data, c(measure = 7)), "eligible values")
  expect_error(epi_eda_inject_missingness(data, c(measure = 1), seed = Inf), "seed")
  expect_error(epi_eda_inject_missingness(data, c(measure = 1), seed = 1.5), "seed")
})
