test_that("check_suggests errors for missing packages", {
  expect_error(episcout:::check_suggests("fakepkgdoesnotexist"))
})

test_that("check_suggests returns invisibly when packages are installed", {
  expect_invisible(episcout:::check_suggests("stats"))
})
