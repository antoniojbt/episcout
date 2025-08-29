# Skip parallel plot tests if multicore support is unavailable or
# when CRAN limits cores via _R_CHECK_LIMIT_CORES_. This prevents
# false failures on systems unable to run multicore futures.
skip_if(!future::supportsMulticore() || nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")))

test_that("placeholder parallel plotting test", {
  expect_true(TRUE)
})
