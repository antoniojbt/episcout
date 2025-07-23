# tests/testthat/test-epi_stats_numeric.R

library(testthat)
# If your function isn’t in a package yet, uncomment the next line:
# source("R/epi_stats_numeric.R")

context("epi_stats_numeric")

test_that("basic summary for a simple vector without NAs", {
    v <- c(1, 2, 3, 4)
    res <- epi_stats_numeric(v)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 1L)

    # Counts
    expect_equal(res$n,         4L)
    expect_equal(res$n_nonNA,   4L)
    expect_equal(res$NA_count,  0L)
    expect_equal(res$NA_percentage, 0)

    # Location & dispersion
    expect_equal(res$sum,       sum(v))
    expect_equal(res$min,       min(v))
    expect_equal(res$quantile_25, quantile(v, .25, names = FALSE))
    expect_equal(res$mean,      mean(v))
    expect_equal(res$median,    median(v))
    expect_equal(res$quantile_75, quantile(v, .75, names = FALSE))
    expect_equal(res$max,       max(v))

    # IQR, SD, variance, CV, sem
    expect_equal(res$IQR,       IQR(v))
    expect_equal(res$SD,        sd(v))
    expect_equal(res$variance,  var(v))
    expect_equal(res$CV,        sd(v) / mean(v))
    expect_equal(res$sem,       sd(v) / sqrt(length(v)))

    # Outlier fences & counts (no outliers here)
    q1 <- quantile(v, .25, names = FALSE)
    q3 <- quantile(v, .75, names = FALSE)
    expect_equal(res$lower_fence, q1 - 1.5 * IQR(v))
    expect_equal(res$upper_fence, q3 + 1.5 * IQR(v))
    expect_equal(res$n_below_lower, 0L)
    expect_equal(res$n_above_upper, 0L)
    expect_equal(res$outlier_count, 0L)
    expect_equal(res$outlier_percentage, 0)

    # Shapiro–Wilk p–value should be numeric for n=4
    expect_true(is.numeric(res$Shapiro_Wilk_p_value))
    expect_true(!is.na(res$Shapiro_Wilk_p_value))
})

test_that("NAs are counted and percentages correct", {
    v <- c(NA, 1, 2, NA)
    res <- epi_stats_numeric(v)

    expect_equal(res$n,         4L)
    expect_equal(res$n_nonNA,   2L)
    expect_equal(res$NA_count,  2L)
    expect_equal(res$NA_percentage, 50)
})

test_that("outlier detection works", {
    v <- c(1, 2, 3, 100)  # 100 is a Tukey outlier
    res <- epi_stats_numeric(v, coef = 1.5)

    # Only one value above the upper fence
    expect_equal(res$outlier_count, 1L)
    expect_equal(res$n_above_upper, 1L)
    expect_equal(res$n_below_lower, 0L)
    expect_equal(res$outlier_percentage, 1 / res$n_nonNA * 100)
})

test_that("Shapiro–Wilk skipped for too few or too many observations", {
    # Too few (n_nonNA = 3)
    small <- c(1, 2, NA, 3)
    res_small <- epi_stats_numeric(small)
    expect_true(is.na(res_small$Shapiro_Wilk_p_value))

    # Too many (n_nonNA > 4999)
    large <- rep(1, 5000)
