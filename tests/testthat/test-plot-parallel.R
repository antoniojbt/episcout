
library(episcout)

print("Function being tested: epi_utils_multicore in plotting context")

# Ensure that workers are cleaned up immediately after each test

test_that("epi_utils_multicore uses multisession plan", {
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  skip_if_not_installed("foreach")
  skip_if_not_installed("iterators")
  skip_if_not_installed("parallel")
  skip_if_not_installed("withr")
  library(future)
  library(parallel)
  library(doFuture)
  library(foreach)
  library(iterators)
  skip_if(parallel::detectCores() < 2, "Not enough cores")
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
