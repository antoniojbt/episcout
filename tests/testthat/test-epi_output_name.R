context("epi_output_name")

test_that("NA and empty inputs produce 'NA.tsv'", {
  expect_equal(epi_output_name(NA_character_), "NA.tsv")
  expect_equal(epi_output_name(""), "NA.tsv")
})

test_that("custom suffix and dot handling", {
  expect_equal(epi_output_name("nofile", suffix = ".csv"), "nofile.csv")
  expect_equal(epi_output_name("a.b.c", suffix = ".txt"), "a.b.txt")
})
