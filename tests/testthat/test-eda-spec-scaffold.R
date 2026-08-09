context("data-frame EDA specification scaffold")

library(testthat)
library(episcout)

scaffold_columns <- c(
  "name", "label", "type", "role", "units", "levels", "min", "max",
  "missing_codes", "required", "group", "description", "geo_role",
  "geo_pair", "geo_crs", "observed_class",
  "n", "n_missing", "n_observed", "n_unique", "candidate_type",
  "candidate_levels", "review_status", "review_reason"
)

test_that("scaffold public formals are stable", {
  expect_named(formals(epi_eda_spec_scaffold), c("data", "max_candidate_levels"))
  expect_identical(formals(epi_eda_spec_scaffold)$max_candidate_levels, 20L)
})

test_that("scaffold records conservative structural evidence in source order", {
  data <- data.frame(
    check.names = FALSE,
    integer_value = c(1L, 2L, NA_integer_, 2L),
    numeric_value = c(1, 2, Inf, NA_real_),
    whole_numeric = c(1, 2, NA_real_, 2),
    logical_value = c(TRUE, FALSE, NA, TRUE),
    category = factor(
      c("A", "B", NA, "A"),
      levels = c("B", "A", "NA", "unused")
    ),
    date_value = as.Date(c("2020-01-01", "2020-01-02", NA, "2020-01-04")),
    datetime_value = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-01 00:00:01", NA, "2020-01-01 00:00:03"),
      tz = "UTC"
    ),
    iso_date = c("2021-02-01", "2021-02-02", NA, "2021-02-04"),
    iso_datetime = c("2021-02-01T00:00:00Z", "2021-02-02T12:30:00+00:00", NA, "2021-02-04T00:00:00Z"),
    site_code = c("PRIVATE_SITE_ALPHA", "PRIVATE_SITE_BETA", NA, "PRIVATE_SITE_ALPHA"),
    free_text = c("one", "two", "three", "four"),
    all_missing = rep(NA_character_, 4)
  )

  observed <- epi_eda_spec_scaffold(data, max_candidate_levels = 2L)

  expect_s3_class(observed, "data.frame")
  expect_named(observed, scaffold_columns)
  expect_equal(vapply(observed, typeof, character(1)), c(
    name = "character", label = "character", type = "character",
    role = "character", units = "character", levels = "character",
    min = "character", max = "character", missing_codes = "character",
    required = "logical", group = "character", description = "character",
    geo_role = "character", geo_pair = "character", geo_crs = "character",
    observed_class = "character", n = "integer", n_missing = "integer",
    n_observed = "integer", n_unique = "integer",
    candidate_type = "character", candidate_levels = "character",
    review_status = "character", review_reason = "character"
  ))
  expect_equal(observed$name, names(data))
  expect_equal(observed$label, names(data))
  expect_equal(observed$type, c(
    "integer", "numeric", "numeric", "binary", "categorical", "date",
    "datetime", "text", "text", "text", "text", "text"
  ))
  expect_equal(observed$candidate_type, c(
    "binary", "", "integer", "", "", "", "", "date", "datetime",
    "binary", "", ""
  ))
  expect_equal(observed$levels[observed$name == "logical_value"], "FALSE;TRUE")
  expect_equal(observed$levels[observed$name == "category"], "B;A;NA;unused")
  expect_true(all(observed$candidate_levels == ""))
  expect_true(all(observed$review_status == "review_required"))
  expect_equal(observed$n, rep(4L, ncol(data)))
  expect_equal(observed$n_missing, c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 4L))
  expect_equal(observed$n_observed, observed$n - observed$n_missing)
  expect_equal(observed$n_unique, c(2L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 2L, 4L, 0L))
  expect_true(all(is.na(observed$required)))
  expect_true(all(observed$role == ""))
  expect_true(all(observed$units == ""))
  expect_true(all(observed$min == ""))
  expect_true(all(observed$max == ""))
  expect_true(all(observed$missing_codes == ""))
  expect_true(all(observed$group == ""))
  expect_true(all(observed$description == ""))
  expect_true(all(observed$geo_role == ""))
  expect_true(all(observed$geo_pair == ""))
  expect_true(all(observed$geo_crs == ""))
  expect_false(any(grepl("PRIVATE_SITE_", unlist(observed), fixed = TRUE)))
})

test_that("whole-number and character candidates require observed evidence", {
  data <- data.frame(
    whole = c(1, 2, 3),
    fractional = c(1, 2.5, 3),
    infinite = c(1, 2, Inf),
    valid_date = c("2020-02-28", "2020-02-29", "2020-03-01"),
    invalid_date = c("2020-02-28", "2020-02-30", "2020-03-01"),
    mixed_date = c("2020-02-28", "other", "2020-03-01"),
    out_of_range_whole = c(1e20, 2e20, NA_real_),
    all_missing_numeric = rep(NA_real_, 3),
    all_missing_character = rep(NA_character_, 3),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_spec_scaffold(data, max_candidate_levels = 2L)

  expect_equal(observed$candidate_type, c("integer", "", "", "date", "", "", "", "", ""))
})

test_that("ordered factors, POSIXlt and IDate retain supported storage types", {
  data <- data.frame(
    severity = ordered(
      c("low", "high"),
      levels = c("low", "medium", "high")
    )
  )
  data[["local_time"]] <- as.POSIXlt(
    c("2020-01-01 00:00:00", "2020-01-02 00:00:00"),
    tz = "UTC"
  )
  if (requireNamespace("data.table", quietly = TRUE)) {
    data[["idate"]] <- data.table::as.IDate(c("2020-01-01", "2020-01-02"))
  }

  observed <- epi_eda_spec_scaffold(data)

  expect_equal(observed$type[observed$name == "severity"], "categorical")
  expect_equal(observed$observed_class[observed$name == "severity"], "ordered/factor")
  expect_equal(observed$levels[observed$name == "severity"], "low;medium;high")
  expect_equal(observed$type[observed$name == "local_time"], "datetime")
  expect_equal(observed$observed_class[observed$name == "local_time"], "POSIXlt/POSIXt")
  expect_match(observed$review_reason[observed$name == "local_time"], "timezone metadata is UTC")
  if ("idate" %in% observed$name) {
    expect_equal(observed$type[observed$name == "idate"], "date")
    expect_equal(observed$observed_class[observed$name == "idate"], "IDate/Date")
  }
})

test_that("ordinary missingness does not invent sentinel codes", {
  data <- data.frame(
    numeric_code = c(999, NA_real_, NaN, 1),
    text_code = c("Unknown", "", NA, "Observed"),
    constant_integer = c(7L, 7L, NA_integer_, 7L),
    constant_character = c("constant", "constant", NA, "constant"),
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_spec_scaffold(data, max_candidate_levels = 1L)

  expect_equal(observed$n_missing, c(2L, 1L, 1L, 1L))
  expect_equal(observed$n_unique, c(2L, 3L, 1L, 1L))
  expect_true(all(observed$missing_codes == ""))
  expect_equal(observed$candidate_type, c("integer", "", "categorical", "categorical"))
})

test_that("strict temporal candidates reject malformed calendar and time values", {
  cases <- list(
    valid_date = c("2024-02-29", "2000-01-01", NA),
    invalid_date = c("2023-02-29", "2024-01-01", NA),
    mixed = c("2024-01-01", "2024-01-01T00:00:00Z", NA),
    valid_datetime = c("2024-01-01T00:00:00Z", "2024-01-02T01:02:03.5-05:00", NA),
    space_datetime = c("2024-01-01 00:00:00", "2024-01-02 00:00:00", NA),
    invalid_hour = c("2024-01-01T25:00:00Z", "2024-01-02T00:00:00Z", NA)
  )
  expected <- c("date", "", "", "datetime", "", "")

  observed <- vapply(cases, function(values) {
    epi_eda_spec_scaffold(
      data.frame(value = values, stringsAsFactors = FALSE),
      max_candidate_levels = 1L
    )$candidate_type
  }, character(1))

  expect_equal(unname(observed), expected)

  local_datetime <- epi_eda_spec_scaffold(data.frame(
    value = c("2024-01-01T00:00:00", "2024-01-02T00:00:00"),
    stringsAsFactors = FALSE
  ))
  expect_match(local_datetime$review_reason, "timezone semantics require review")
})

test_that("scaffold has stable typed zero-row and zero-column contracts", {
  zero_rows <- data.frame(
    integer_value = integer(),
    text_value = character(),
    date_value = as.Date(character()),
    stringsAsFactors = FALSE
  )

  observed_rows <- epi_eda_spec_scaffold(zero_rows)
  observed_columns <- epi_eda_spec_scaffold(data.frame())

  expect_named(observed_rows, scaffold_columns)
  expect_equal(observed_rows$n, rep(0L, 3))
  expect_equal(observed_rows$n_missing, rep(0L, 3))
  expect_equal(observed_rows$n_observed, rep(0L, 3))
  expect_equal(observed_rows$n_unique, rep(0L, 3))
  expect_equal(observed_rows$candidate_type, rep("", 3))
  expect_named(observed_columns, scaffold_columns)
  expect_equal(nrow(observed_columns), 0L)
  expect_type(observed_columns$required, "logical")
  expect_type(observed_columns$n, "integer")
  expect_type(observed_columns$n_missing, "integer")
  expect_type(observed_columns$n_observed, "integer")
  expect_type(observed_columns$n_unique, "integer")
  expect_true(all(vapply(
    observed_columns[setdiff(scaffold_columns, c("required", "n", "n_missing", "n_observed", "n_unique"))],
    is.character,
    logical(1)
  )))
})

test_that("scaffold validates its inputs before returning output", {
  expect_error(epi_eda_spec_scaffold(1:3), "data frame")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), 0), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), 1.5), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), NA_integer_), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), c(1L, 2L)), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), Inf), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), "2"), "positive whole number")
  expect_error(epi_eda_spec_scaffold(data.frame(x = 1), TRUE), "positive whole number")
  expect_s3_class(epi_eda_spec_scaffold(data.frame(x = 1), 1e20), "data.frame")

  invalid_names <- data.frame(a = 1, b = 2, check.names = FALSE)
  names(invalid_names) <- c("", "duplicate")
  expect_error(epi_eda_spec_scaffold(invalid_names), "non-empty")

  whitespace_name <- data.frame(a = 1, check.names = FALSE)
  names(whitespace_name) <- "  "
  expect_error(epi_eda_spec_scaffold(whitespace_name), "non-empty")

  duplicated_names <- data.frame(a = 1, b = 2, check.names = FALSE)
  names(duplicated_names) <- c("duplicate", "duplicate")
  expect_error(epi_eda_spec_scaffold(duplicated_names), "[Dd]uplicate")
})

test_that("scaffold rejects unsupported and decorated columns without exposing values", {
  data <- data.frame(safe = 1:2)
  data$nested_secret <- I(list("PRIVATE_VALUE_ONE", "PRIVATE_VALUE_TWO"))
  data$elapsed <- structure(c(1, 2), class = "difftime", units = "secs")
  data$matrix_value <- I(matrix(1:4, nrow = 2))
  data$raw_value <- as.raw(c(1, 2))
  data$complex_value <- c(1 + 1i, 2 + 2i)
  data$labelled_value <- structure(
    c(1, 2),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )

  error <- tryCatch(
    epi_eda_spec_scaffold(data),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "nested_secret")
  expect_match(conditionMessage(error), "elapsed")
  expect_match(conditionMessage(error), "matrix_value")
  expect_match(conditionMessage(error), "raw_value")
  expect_match(conditionMessage(error), "complex_value")
  expect_match(conditionMessage(error), "labelled_value")
  expect_false(grepl("PRIVATE_VALUE", conditionMessage(error), fixed = TRUE))
})

test_that("scaffold refuses factor metadata that cannot round-trip safely", {
  unsafe <- list(
    delimiter = factor(c("A", "B"), levels = c("A", "sensitive;code")),
    whitespace = factor(c("A", "B"), levels = c("A", " sensitive code")),
    empty = factor(c("A", "A"), levels = c("A", ""))
  )

  for (case in names(unsafe)) {
    data <- data.frame(group = unsafe[[case]])
    error <- tryCatch(epi_eda_spec_scaffold(data), error = identity)

    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "group", info = case)
    expect_match(conditionMessage(error), "encod|round-trip", ignore.case = TRUE, info = case)
    expect_false(grepl("sensitive;code", conditionMessage(error), fixed = TRUE), info = case)
  }
})

test_that("scaffold preserves non-syntactic names and CSV-safe reviewed specifications", {
  data <- data.frame(
    check.names = FALSE,
    "numeric value" = c(1, 2, NA_real_),
    "group code" = c("A", "B", "A"),
    "exposição" = c("low", "high", "low")
  )
  scaffold <- epi_eda_spec_scaffold(data)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(scaffold, path, row.names = FALSE, na = "")

  round_trip <- epi_eda_spec(path)

  expect_identical(round_trip, scaffold)
  expect_equal(round_trip$name, names(data))
  expect_equal(round_trip$type, c("numeric", "text", "text"))
  expect_true(all(is.na(round_trip$required)))
  expect_equal(names(round_trip), scaffold_columns)

  reviewed <- round_trip
  reviewed$role <- "covariate"
  reviewed$type[reviewed$name == "group code"] <- "categorical"
  reviewed$levels[reviewed$name == "group code"] <- "A;B"
  reviewed$type[reviewed$name == "exposição"] <- "categorical"
  reviewed$levels[reviewed$name == "exposição"] <- "low;high"
  result <- epi_eda_run(data, reviewed)

  expect_named(result$summaries, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_equal(result$summaries$variables$name, names(data))
  expect_equal(result$summaries$categorical$level, c("A", "B", "low", "high"))
  expect_equal(result$summaries$categorical$n, c(2L, 1L, 2L, 1L))

  literal_na <- epi_eda_spec_scaffold(data.frame(
    category = factor(c("NA", "NA"), levels = "NA")
  ))
  literal_path <- tempfile(fileext = ".csv")
  utils::write.csv(literal_na, literal_path, row.names = FALSE, na = "")
  literal_round_trip <- epi_eda_spec(literal_path)

  expect_equal(literal_na$levels, "NA;")
  expect_equal(strsplit(literal_round_trip$levels, ";", fixed = TRUE)[[1]][[1]], "NA")
})

test_that("ordinary specification CSV types and custom extra columns remain compatible", {
  fixture_path <- file.path("fixtures", "blood_storage", "blood_storage_spec.csv")
  raw <- utils::read.csv(fixture_path, check.names = FALSE, stringsAsFactors = FALSE)
  observed <- epi_eda_spec(fixture_path)

  expect_identical(observed$min, raw$min)
  expect_identical(observed$max, raw$max)

  custom <- data.frame(
    name = "value",
    label = "Value",
    type = "numeric",
    role = "covariate",
    n = "not applicable",
    stringsAsFactors = FALSE
  )
  expect_identical(epi_eda_spec(custom)$n, "not applicable")
})

test_that("scaffold CSV round-trip preserves literal NA text", {
  data <- data.frame(value = c(1, 2), check.names = FALSE)
  names(data) <- "NA"
  scaffold <- epi_eda_spec_scaffold(data)
  scaffold$description <- "NA"

  default_path <- tempfile(fileext = ".csv")
  utils::write.csv(scaffold, default_path, row.names = FALSE)
  blank_path <- tempfile(fileext = ".csv")
  utils::write.csv(scaffold, blank_path, row.names = FALSE, na = "")

  expect_identical(epi_eda_spec(default_path), scaffold)
  expect_identical(epi_eda_spec(blank_path), scaffold)
  expect_equal(epi_eda_spec(default_path)$name, "NA")
  expect_equal(epi_eda_spec(default_path)$description, "NA")
  expect_true(is.na(epi_eda_spec(default_path)$required))
})

test_that("scaffold evidence counts must be valid and reconcile", {
  scaffold <- epi_eda_spec_scaffold(data.frame(value = c(1, 2, NA_real_)))

  negative <- scaffold
  negative$n_missing <- -1L
  expect_error(epi_eda_spec(negative), "Invalid n_missing")

  overflow <- scaffold
  overflow$n <- .Machine$integer.max + 1
  expect_error(epi_eda_spec(overflow), "Invalid n")

  inconsistent <- scaffold
  inconsistent$n_observed <- 1L
  expect_error(epi_eda_spec(inconsistent), "reconcile")

  impossible_unique <- scaffold
  impossible_unique$n_unique <- 3L
  expect_error(epi_eda_spec(impossible_unique), "must not exceed")
})

test_that("scaffold is deterministic, non-mutating and side-effect free", {
  data <- data.frame(value = c(1L, NA_integer_, 2L))
  original <- data
  before <- list.files(tempdir(), all.files = TRUE)

  expect_silent(first <- epi_eda_spec_scaffold(data))
  second <- epi_eda_spec_scaffold(data)

  expect_identical(first, second)
  expect_identical(data, original)
  expect_setequal(list.files(tempdir(), all.files = TRUE), before)
})
