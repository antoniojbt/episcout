print("Function being tested: epi_utils_multicore in plotting context")

# Skip parallel plot tests if multicore support is unavailable or
# when CRAN limits cores via _R_CHECK_LIMIT_CORES_. This prevents
# false failures on systems unable to run multicore futures.
skip_if(!future::supportsMulticore() || nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")))
skip_if(parallel::detectCores() < 2, "Not enough cores")

  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("iterators")
  skip_if_not_installed("parallel")
  skip_if_not_installed("withr")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("cowplot")

  library(episcout)
  library(ggplot2)
  library(future)
  library(parallel)
  library(doFuture)
  library(foreach)
  library(iterators)

# Ensure that workers are cleaned up immediately after each test

test_that("epi_utils_multicore uses multisession plan", {  
  original_plan <- future::plan()
  withr::defer(future::plan(original_plan), teardown_env())
  epi_utils_multicore(
    num_cores = 2,
    future_plan = "multisession",
    verbose = FALSE
  )
  withr::defer(future::plan("sequential"), teardown_env())
  plan_now <- future::plan()
  if (inherits(plan_now, "multisession")) {
    expect_identical(future::nbrOfWorkers(), 2L)
  } else {
    expect_true(inherits(plan_now, "sequential"))
  }
  future_v %<-% {
    1 + 2
  }
  expect_identical(future_v, 3)
})

# Data for tests
mt <- mtcars

test_that("epi_plot_parallel generates plots using multiple workers", {
  plots <- epi_plot_parallel(mt,
    vars_to_plot = c("mpg", "disp"),
    num_cores = 2,
    future_plan = "multicore"
  )
  expect_equal(length(plots), 2)
  expect_equal(sort(names(plots)), sort(c("mpg", "disp")))
  expect_gt(attr(plots, "workers"), 1)
})

test_that("epi_plot_save_parallel saves plots in parallel", {
  plots <- list(mpg = epi_plot_hist(mt, "mpg"), disp = epi_plot_hist(mt, "disp"))
  tmp_prefix <- file.path(tempdir(), "plots_parallel")
  files <- epi_plot_save_parallel(
    plots,
    file_prefix = tmp_prefix,
    plot_type = "png",
    plot_step = 1,
    num_cores = 2,
    future_plan = "multicore"
  )
  expect_equal(length(files), 2)
  expect_true(all(file.exists(files)))
  expect_gt(attr(files, "workers"), 1)
})