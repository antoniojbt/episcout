context("episcout utility function tests")

######################
library(episcout)
library(testthat)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")
######################

######################
# Set a test set:
# Test set df:
# set.seed(12345)
# n <- 20
# df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#                  var_to_rep = rep(c("Pre", "Post"), n / 2),
#                               x = rnorm(n),
#                               y = rbinom(n, 1, 0.50),
#                               z = rpois(n, 2)
#                  )
# df

# Set variables used in more than one test:
# input_file <- "inst/extdata/df.tsv"
######################

######################
print("Function being tested: epi_utils_multicore")

test_that("epi_utils_multicore sequential", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("iterators")
  skip_if_not_installed("parallel")
  library(future)
  library(parallel)
  library(doFuture)
  library(foreach)
  library(iterators)
  plan0 <- future::plan()
  on.exit(future::plan(plan0), add = TRUE)
  epi_utils_multicore(
    num_cores = 1,
    future_plan = "sequential",
    verbose = FALSE
  )
  expect_true(inherits(future::plan(), "sequential"))
  expect_identical(future::nbrOfWorkers(), 1L)
  future_v %<-% {
    1 + 2
  }
  expect_identical(future_v, 3)
})

test_that("epi_utils_multicore multi", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("iterators")
  skip_if_not_installed("parallel")
  skip_if(parallel::detectCores() < 2, "Not enough cores")
  library(future)
  library(parallel)
  library(doFuture)
  library(foreach)
  library(iterators)
  plan0 <- future::plan()
  on.exit(future::plan(plan0), add = TRUE)
  epi_utils_multicore(
    num_cores = 2,
    future_plan = "multisession",
    verbose = FALSE
  )
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
######################

print("Function being tested: epi_utils_log")

test_that("epi_utils_log writes log to file", {
  tmp_prefix <- tempfile("log_file")
  out <- capture.output(epi_utils_log(tmp_prefix))
  log_file <- paste0(tmp_prefix, "_log.txt")
  expect_true(any(grepl("Session information saved in", out)))
  expect_true(file.exists(log_file))
  first_line <- readLines(log_file, n = 1)
  expect_true(grepl("R version", first_line))
  unlink(log_file)
})

test_that("epi_utils_log uses default name with no arguments", {
  skip_if_not_installed("withr")
  withr::with_tempdir({
    file <- paste0("session_", Sys.Date(), "_log.txt")
    expect_invisible(epi_utils_log())
    expect_true(file.exists(file))
    expect_true(any(grepl("R version", readLines(file))))
    unlink(file)
  })
})

test_that("epi_utils_log ignores numeric input", {
  skip_if_not_installed("withr")
  withr::with_tempdir({
    file <- paste0("session_", Sys.Date(), "_log.txt")
    expect_invisible(epi_utils_log(123))
    expect_true(file.exists(file))
    expect_true(any(grepl("R version", readLines(file))))
    unlink(file)
  })
})

print("Function being tested: epi_utils_session")

test_that("epi_utils_session saves selected objects", {
  tmp_prefix <- tempfile("session_file")
  a <- 1:3
  b <- "test"
  out <- capture.output(epi_utils_session(tmp_prefix, objects_to_save = c("a", "b")))
  session_file <- paste0(tmp_prefix, ".RData")
  expect_true(any(grepl("Saved objects as", out)))
  expect_true(file.exists(session_file))
  new_env <- new.env()
  load(session_file, envir = new_env)
  expect_identical(new_env$a, a)
  expect_identical(new_env$b, b)
  unlink(session_file)
})

######################
