# tests/testthat/test-epi_stats_factors.R

library(testthat)
# If the function isn’t yet in a package, uncomment the next line:
# source("R/epi_stats_factors.R")

context("epi_stats_factors")

test_that("summarizes a single unordered factor correctly", {
    df <- tibble::tibble(
        f = factor(c("x", "y", "x", NA, "z", "y", "x"))
    )
    res <- epi_stats_factors(df)

    expect_equal(nrow(res), 1L)
    expect_equal(res$Variable, "f")
    expect_equal(res$n_missing, 1L)
    expect_equal(res$complete_rate, 6/7)
    expect_false(res$ordered)
    expect_equal(res$n_unique, 3L)
    expect_equal(res$top_counts, "x (3), y (2), z (1)")
})

test_that("summarizes a single ordered factor correctly", {
    df <- tibble::tibble(
        f = ordered(c("low", "medium", "high", "low", NA))
    )
    res <- epi_stats_factors(df)

    expect_equal(nrow(res), 1L)
    expect_equal(res$Variable, "f")
    expect_equal(res$n_missing, 1L)
    expect_equal(res$complete_rate, 4/5)
    expect_true(res$ordered)
    expect_equal(res$n_unique, 3L)
    # 'low' appears twice, then 'high' and 'medium' once each
    expect_equal(res$top_counts, "low (2), high (1), medium (1)")
})

test_that("handles factors with fewer than three levels", {
    df <- tibble::tibble(
        f = factor(c("a", "a", NA))
    )
    res <- epi_stats_factors(df)

    expect_equal(nrow(res), 1L)
    expect_equal(res$Variable, "f")
    expect_equal(res$n_unique, 1L)
    expect_equal(res$top_counts, "a (2)")
})

test_that("summarizes multiple factor columns independently", {
    df <- tibble::tibble(
        a = factor(c("u", "v", "u")),
        b = ordered(c("alpha", "beta", NA))
    )
    res <- epi_stats_factors(df)

    expect_setequal(res$Variable, c("a", "b"))

    row_a <- dplyr::filter(res, Variable == "a")
    expect_equal(row_a$n_missing, 0L)
    expect_false(row_a$ordered)
    expect_equal(row_a$n_unique, 2L)
    expect_true(grepl("^u \\(2\\)", row_a$top_counts))

    row_b <- dplyr::filter(res, Variable == "b")
    expect_equal(row_b$n_missing, 1L)
    expect_true(row_b$ordered)
})
