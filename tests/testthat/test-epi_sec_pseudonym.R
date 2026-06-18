context("secure pseudonymisation bridge helper")

library(episcout)
library(testthat)

test_that("epi_sec_pseudonym returns bridge table in input order", {
  participant_id <- c("study_003", "study_001", "study_002")
  bridge <- epi_sec_pseudonym(participant_id, prefix = "MX", n_bytes = 16)

  expect_true(tibble::is_tibble(bridge))
  expect_named(bridge, c("participant_id", "token_id"))
  expect_equal(nrow(bridge), length(participant_id))
  expect_equal(bridge$participant_id, participant_id)
})

test_that("epi_sec_pseudonym generates unique prefixed hex tokens", {
  participant_id <- sprintf("study_%03d", 1:100)
  bridge <- epi_sec_pseudonym(participant_id, prefix = "MXSTUDY", n_bytes = 16)

  expect_equal(length(unique(bridge$token_id)), length(participant_id))
  expect_true(all(grepl("^MXSTUDY_[0-9a-f]{32}$", bridge$token_id)))
})

test_that("epi_sec_pseudonym uses 192-bit tokens by default", {
  bridge <- epi_sec_pseudonym(c("a", "b"), prefix = "P")

  expect_true(all(grepl("^P_[0-9a-f]{48}$", bridge$token_id)))
})

test_that("epi_sec_pseudonym accepts numeric and factor participant IDs", {
  numeric_bridge <- epi_sec_pseudonym(1:3, n_bytes = 16)
  factor_bridge <- epi_sec_pseudonym(factor(c("a", "b", "c")), n_bytes = 16)

  expect_equal(numeric_bridge$participant_id, 1:3)
  expect_true(is.factor(factor_bridge$participant_id))
  expect_equal(as.character(factor_bridge$participant_id), c("a", "b", "c"))
})

test_that("epi_sec_pseudonym errors on invalid participant IDs", {
  expect_error(
    epi_sec_pseudonym(c("a", "a"), n_bytes = 16),
    "participant_id values must be unique"
  )
  expect_error(
    epi_sec_pseudonym(c("a", NA), n_bytes = 16),
    "participant_id must not contain missing values"
  )
  expect_error(
    epi_sec_pseudonym(list("a", "b"), n_bytes = 16),
    "participant_id must be a character, numeric or factor vector"
  )
})

test_that("epi_sec_pseudonym errors on invalid token options", {
  expect_error(
    epi_sec_pseudonym(c("a", "b"), n_bytes = 15),
    "n_bytes must be a whole number greater than or equal to 16"
  )
  expect_error(
    epi_sec_pseudonym(c("a", "b"), prefix = NA_character_, n_bytes = 16),
    "prefix must be a single non-missing character value"
  )
})

test_that("epi_sec_pseudonym optionally writes bridge CSV", {
  bridge_path <- tempfile(fileext = ".csv")
  bridge <- epi_sec_pseudonym(
    c("study_1", "study_2"),
    prefix = "TEST",
    n_bytes = 16,
    bridge_path = bridge_path
  )
  written <- utils::read.csv(bridge_path, stringsAsFactors = FALSE)

  expect_true(file.exists(bridge_path))
  expect_equal(written, as.data.frame(bridge))
})

test_that("epi_sec_pseudonym refuses overwrite unless requested", {
  bridge_path <- tempfile(fileext = ".csv")
  writeLines("existing", bridge_path)

  expect_error(
    epi_sec_pseudonym(c("study_1", "study_2"), bridge_path = bridge_path),
    "bridge_path already exists"
  )

  bridge <- epi_sec_pseudonym(
    c("study_1", "study_2"),
    n_bytes = 16,
    bridge_path = bridge_path,
    overwrite = TRUE
  )
  written <- utils::read.csv(bridge_path, stringsAsFactors = FALSE)

  expect_equal(written, as.data.frame(bridge))
})
