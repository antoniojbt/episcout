context("data-frame EDA specification scaffold")

library(testthat)
library(episcout)

scaffold_columns <- c(
  "name", "label", "database_type", "analysis_type", "role", "units", "levels", "min", "max",
  "missing_codes", "required", "group", "description", "geo_role",
  "geo_pair", "geo_crs"
)

test_that("scaffold has the lean public contract", {
  expect_named(formals(epi_eda_spec_scaffold), "data")
  data <- data.frame(
    integer_value = c(1L, NA_integer_),
    numeric_value = c(1, Inf),
    logical_value = c(TRUE, FALSE),
    category = factor(c("A", "B"), levels = c("B", "A", "unused")),
    date_value = as.Date(c("2020-01-01", NA)),
    datetime_value = as.POSIXct(c("2020-01-01", NA), tz = "UTC"),
    text_value = c("PRIVATE_ONE", "PRIVATE_TWO"),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_spec_scaffold(data)

  expect_named(observed, scaffold_columns)
  expect_identical(observed$name, names(data))
  expect_identical(
    observed$analysis_type,
    c("integer", "numeric", "binary", "categorical", "date", "datetime", "text")
  )
  expect_identical(observed$levels, c("", "", "FALSE;TRUE", "B;A;unused", "", "", ""))
  expect_true(all(is.na(observed$required)))
  blank <- setdiff(scaffold_columns, c("name", "label", "database_type", "analysis_type", "levels", "required"))
  expect_true(all(vapply(observed[blank], function(x) all(x == ""), logical(1))))
  expect_false(any(grepl("PRIVATE_", unlist(observed), fixed = TRUE)))
})

test_that("scaffold keeps stable typed empty contracts", {
  zero_rows <- epi_eda_spec_scaffold(data.frame(
    integer_value = integer(),
    text_value = character(),
    date_value = as.Date(character()),
    stringsAsFactors = FALSE
  ))
  zero_columns <- epi_eda_spec_scaffold(data.frame())

  expect_named(zero_rows, scaffold_columns)
  expect_identical(zero_rows$analysis_type, c("integer", "text", "date"))
  expect_named(zero_columns, scaffold_columns)
  expect_equal(nrow(zero_columns), 0L)
  expect_type(zero_columns$required, "logical")
  expect_true(all(vapply(
    zero_columns[setdiff(scaffold_columns, "required")],
    is.character,
    logical(1)
  )))
})

test_that("scaffold uses storage types without value-derived candidates", {
  data <- data.frame(
    whole_double = c(1, 2, 3),
    iso_date_text = c("2024-01-01", "2024-01-02", "2024-01-03"),
    low_cardinality_text = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
  observed <- epi_eda_spec_scaffold(data)

  expect_identical(observed$analysis_type, c("numeric", "text", "text"))
  expect_false(any(c(
    "observed_class", "n", "n_missing", "n_observed", "n_unique",
    "candidate_type", "candidate_levels", "review_status", "review_reason"
  ) %in% names(observed)))
})

test_that("scaffold validates names and unsupported storage value-free", {
  expect_error(epi_eda_spec_scaffold(1:3), "data frame")
  invalid_names <- data.frame(a = 1, b = 2, check.names = FALSE)
  names(invalid_names) <- c("", "duplicate")
  expect_error(epi_eda_spec_scaffold(invalid_names), "non-empty")
  names(invalid_names) <- c("duplicate", "duplicate")
  expect_error(epi_eda_spec_scaffold(invalid_names), "[Dd]uplicate")

  data <- data.frame(safe = 1:2)
  data$nested_secret <- I(list("PRIVATE_VALUE_ONE", "PRIVATE_VALUE_TWO"))
  error <- tryCatch(epi_eda_spec_scaffold(data), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "nested_secret")
  expect_false(grepl("PRIVATE_VALUE", conditionMessage(error), fixed = TRUE))
})

test_that("factor metadata must round-trip through the semantic contract", {
  unsafe <- factor(c("A", "B"), levels = c("A", "sensitive;code"))
  error <- tryCatch(
    epi_eda_spec_scaffold(data.frame(group = unsafe)),
    error = identity
  )
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "group")
  expect_match(conditionMessage(error), "encod|round-trip", ignore.case = TRUE)
  expect_false(grepl("sensitive;code", conditionMessage(error), fixed = TRUE))
})

test_that("lean scaffolds round-trip without changing literal NA text", {
  data <- data.frame(
    check.names = FALSE,
    "NA" = c(1, 2),
    "group code" = factor(c("NA", "A"), levels = c("NA", "A"))
  )
  scaffold <- epi_eda_spec_scaffold(data)
  scaffold$description[[1]] <- "NA"
  path <- tempfile(fileext = ".csv")
  utils::write.csv(scaffold, path, row.names = FALSE, na = "")

  observed <- epi_eda_spec(path)
  expect_identical(observed, scaffold)
  expect_identical(observed$name, names(data))
  expect_identical(observed$description[[1]], "NA")
  expect_identical(observed$levels[[2]], "NA;A")
})

test_that("removed scaffold schemas fail with migration guidance", {
  old <- epi_eda_spec_scaffold(data.frame(value = 1:2))
  old$review_status <- "reviewed"
  expect_error(epi_eda_spec(old), "removed evidence/review scaffold.*Regenerate")

  old <- epi_eda_spec_scaffold(data.frame(value = 1:2))
  old$n <- 2L
  expect_error(epi_eda_spec(old), "removed evidence/review scaffold.*Regenerate")
})

test_that("scaffold is deterministic, non-mutating and side-effect free", {
  data <- data.frame(value = c(1L, NA_integer_, 2L))
  original <- data
  before <- list.files(tempdir(), all.files = TRUE)

  first <- epi_eda_spec_scaffold(data)
  second <- epi_eda_spec_scaffold(data)

  expect_identical(first, second)
  expect_identical(data, original)
  expect_setequal(list.files(tempdir(), all.files = TRUE), before)
})
