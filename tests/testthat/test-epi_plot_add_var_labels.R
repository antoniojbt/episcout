# tests/testthat/test-epi_plot_add_var_labels.R

library(testthat)
library(ggplot2)
# If your function isn’t in a package yet, uncomment the next line:
# source("R/epi_plot_add_var_labels.R")

context("epi_plot_add_var_labels")

# Dummy lookup table
var_lookup <- c(
    var1 = "Description One (var1)",
    var2 = "Description Two (var2)"
)

test_that("sets both y and x labels when present in lookup", {
    p <- ggplot() +
        epi_plot_add_var_labels(var_y = "var1", var_x = "var2", var_lookup = var_lookup)

    expect_equal(p$labels$y, "Description One (var1)")
    expect_equal(p$labels$x, "Description Two (var2)")
})

test_that("only sets y when x is missing or not in lookup", {
    # x not provided
    p1 <- ggplot() +
        epi_plot_add_var_labels(var_y = "var1", var_lookup = var_lookup)
    expect_equal(p1$labels$y, "Description One (var1)")
    expect_null(p1$labels$x)

    # x not in lookup
    p2 <- ggplot() +
        epi_plot_add_var_labels(var_y = "var1", var_x = "unknown", var_lookup = var_lookup)
    expect_equal(p2$labels$y, "Description One (var1)")
    expect_null(p2$labels$x)
})

test_that("only sets x when y is missing or not in lookup", {
    p <- ggplot() +
        epi_plot_add_var_labels(var_x = "var2", var_lookup = var_lookup)
    expect_equal(p$labels$x, "Description Two (var2)")
    expect_null(p$labels$y)
})

test_that("sets no labels when neither var_y nor var_x are in lookup", {
    p <- ggplot() +
        epi_plot_add_var_labels(var_y = "foo", var_x = "bar", var_lookup = var_lookup)
    expect_null(p$labels$y)
    expect_null(p$labels$x)
})
