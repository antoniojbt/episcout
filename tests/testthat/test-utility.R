context("episcout utility function tests")

######################
library(episcout)
library(testthat)
library(future)
library(parallel)
library(doFuture)
library(foreach)
library(iterators)
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
  epi_utils_multicore(num_cores = 1,
                      future_plan = 'sequential')
  core_s <- capture.output(epi_utils_multicore(num_cores = 1,
                                               future_plan = 'sequential')
                           )
  core_s
  # TO DO: fix these, pass in test() but not in check()
  # expect_output(str(core_s[12]), 'sequential')
  future_v %<-% {1 + 2}
  future_v
  expect_identical(future_v, 3)
  future:::ClusterRegistry("stop")
}
)

test_that("epi_utils_multicore multi", {
  epi_utils_multicore(num_cores = 2,
                      future_plan = 'multisession')
  core_m <- capture.output(epi_utils_multicore(num_cores = 2,
                                               future_plan = 'multisession')
                           )
  core_m
  # TO DO: fix these, pass in test() but not in check()
  # expect_output(str(core_m[12]), 'multisession')
  # expect_output(str(core_m[13], nchar.max = 300), 'workers = 2')
  future_v %<-% {1 + 2}
  future_v
  expect_identical(future_v, 3)
  future:::ClusterRegistry("stop")
  }
  )
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
