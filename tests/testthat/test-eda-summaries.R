context("canonical EDA summary contract")

library(testthat)
library(episcout)

make_canonical_fixture <- function() {
  data <- data.frame(
    check.names = FALSE,
    "numeric value" = c(1, 2, Inf, 999, NA),
    integer_value = c(1L, 2L, 3L, NA, 5L),
    category = factor(c("A", "B", "D", NA, "A"), levels = c("A", "B", "D", "unused")),
    binary = c("Yes", "No", "Yes", NA, "Yes"),
    text_value = c("abc", "", "  ", NA, "z"),
    date_value = c("2020-01-01", "2020-01-03", NA, "2020-01-05", "2020-01-07"),
    datetime_value = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-01 00:00:01", NA, "2020-01-01 00:00:03", "2020-01-01 00:00:04"),
      tz = "America/Mexico_City"
    )
  )
  spec <- data.frame(
    name = c("numeric value", "integer_value", "category", "binary", "text_value", "date_value", "datetime_value", "missing_variable"),
    label = c("Numeric", "Integer", "Category", "Binary", "Text", "Date", "Datetime", "Missing"),
    type = c("numeric", "integer", "categorical", "binary", "text", "date", "datetime", "text"),
    role = c("outcome", rep("covariate", 7)),
    levels = c("", "", "A;B;C", "No;Yes", "", "", "", ""),
    missing_codes = c("999", "", "", "", "", "", "", ""),
    required = c(rep(TRUE, 7), FALSE),
    stringsAsFactors = FALSE
  )
  list(data = data, spec = spec)
}

test_that("public EDA summary interfaces expose one canonical contract", {
  expect_named(formals(epi_eda_profile_summaries), c("data", "spec"))
  expect_named(
    formals(epi_eda_run),
    c(
      "data", "spec", "output_dir", "synthetic", "n", "seed",
      "maps", "map_vars", "max_map_points"
    )
  )
  expect_named(
    formals(epi_eda_render_report),
    c(
      "data", "spec", "output_dir", "synthetic", "n", "seed", "quiet",
      "maps", "map_vars", "max_map_points"
    )
  )

  fixture <- make_canonical_fixture()
  observed <- epi_eda_profile_summaries(fixture$data, fixture$spec)

  expect_named(observed, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_named(observed$variables, c(
    "name", "label", "type", "role", "required", "n", "n_missing",
    "n_observed", "n_unique", "n_infinite", "status", "reason"
  ))
})

test_that("canonical summaries account for every specification variable", {
  fixture <- make_canonical_fixture()
  observed <- epi_eda_profile_summaries(fixture$data, fixture$spec)

  expect_equal(observed$variables$name, fixture$spec$name)
  expect_equal(observed$variables$required, fixture$spec$required)
  expect_equal(observed$variables$status, c(rep("summarised", 7), "skipped"))
  expect_true(is.na(observed$variables$n[8]))
  expect_true(is.na(observed$variables$n_missing[8]))
  expect_match(observed$variables$reason[8], "optional", ignore.case = TRUE)
  expect_equal(observed$skipped$name, "missing_variable")

  successful <- c(
    observed$numeric$name,
    unique(observed$categorical$name),
    observed$text$name,
    observed$temporal$name
  )
  expect_setequal(successful, fixture$spec$name[1:7])
  expect_length(intersect(successful, observed$skipped$name), 0L)
})

test_that("numeric summaries use finite values and hand-derived expectations", {
  data <- data.frame(value = c(1, 2, 10, Inf, 999, NA_real_))
  spec <- data.frame(
    name = "value",
    label = "Value",
    type = "numeric",
    role = "outcome",
    missing_codes = "999",
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_summaries(data, spec)
  audit <- observed$variables
  numeric <- observed$numeric

  expect_equal(audit$n, 6L)
  expect_equal(audit$n_missing, 2L)
  expect_equal(audit$n_observed, 4L)
  expect_equal(audit$n_infinite, 1L)
  expect_equal(numeric$n_finite, 3L)
  expect_equal(numeric$sum, 13)
  expect_equal(numeric$min, 1)
  expect_equal(numeric$q1, 1.5)
  expect_equal(numeric$mean, 13 / 3)
  expect_equal(numeric$median, 2)
  expect_equal(numeric$q3, 6)
  expect_equal(numeric$max, 10)
  expect_equal(numeric$iqr, 4.5)
  expect_equal(numeric$variance, 73 / 3)
  expect_equal(numeric$sd, sqrt(73 / 3))
  expect_equal(numeric$lower_fence, -5.25)
  expect_equal(numeric$upper_fence, 12.75)
  expect_equal(numeric$outlier_count, 0L)
  expect_equal(numeric$outlier_percentage, 0)
})

test_that("numeric summaries do not invent zero totals", {
  data <- data.frame(
    empty = numeric(),
    stringsAsFactors = FALSE
  )
  empty_spec <- data.frame(name = "empty", label = "Empty", type = "numeric", role = "covariate")
  empty <- epi_eda_profile_summaries(data, empty_spec)

  expect_equal(empty$variables$n, 0L)
  expect_equal(empty$numeric$n_finite, 0L)
  expect_true(is.na(empty$numeric$sum))

  cases <- list(
    all_missing = c(NA_real_, NaN),
    sentinel_only = c(999, 999),
    infinite_only = c(Inf, -Inf)
  )
  for (name in names(cases)) {
    case_data <- data.frame(value = cases[[name]])
    case_spec <- data.frame(
      name = "value",
      label = "Value",
      type = "numeric",
      role = "covariate",
      missing_codes = "999"
    )
    observed <- epi_eda_profile_summaries(case_data, case_spec)
    expect_equal(observed$numeric$n_finite, 0L, info = name)
    expect_true(is.na(observed$numeric$sum), info = name)
    expect_true(is.na(observed$numeric$mean), info = name)
  }
})

test_that("categorical summaries reconcile declarations and denominators", {
  data <- data.frame(status = factor(
    c("A", "B", "D", NA, "UNK", "NA", "A"),
    levels = c("A", "B", "C", "D", "NA", "unused", "UNK")
  ))
  spec <- data.frame(
    name = "status",
    label = "Status",
    type = "categorical",
    role = "covariate",
    levels = "A;B;C",
    missing_codes = "UNK",
    stringsAsFactors = FALSE
  )

  observed <- epi_eda_profile_summaries(data, spec)$categorical

  expect_equal(observed$level, c("A", "B", "C", "D", "NA"))
  expect_equal(observed$n, c(2L, 1L, 0L, 1L, 1L))
  expect_equal(observed$p_total, c(2 / 7, 1 / 7, 0, 1 / 7, 1 / 7))
  expect_equal(observed$p_observed, c(2 / 5, 1 / 5, 0, 1 / 5, 1 / 5))
  expect_equal(observed$is_declared, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(observed$is_unexpected, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_false("unused" %in% observed$level)
})

test_that("factor metadata does not become an undeclared result row", {
  data <- data.frame(status = factor(c("A", "B"), levels = c("A", "B", "metadata_only")))
  spec <- data.frame(name = "status", label = "Status", type = "categorical", role = "covariate")

  observed <- epi_eda_profile_summaries(data, spec)$categorical

  expect_equal(observed$level, c("A", "B"))
  expect_true(all(is.na(observed$is_declared)))
  expect_false(any(observed$is_unexpected))
})

test_that("text and temporal summaries preserve documented states and units", {
  fixture <- make_canonical_fixture()
  observed <- epi_eda_profile_summaries(fixture$data, fixture$spec)

  text <- observed$text[observed$text$name == "text_value", ]
  expect_equal(text$n_missing, 1L)
  expect_equal(text$n_empty, 1L)
  expect_equal(text$n_whitespace, 1L)
  expect_equal(text$min_length, 0L)
  expect_equal(text$max_length, 3L)

  date <- observed$temporal[observed$temporal$name == "date_value", ]
  expect_equal(date$min, "2020-01-01")
  expect_equal(date$q1, "2020-01-02")
  expect_equal(date$median, "2020-01-04")
  expect_equal(date$q3, "2020-01-05")
  expect_equal(date$max, "2020-01-07")
  expect_equal(date$range_value, 6)
  expect_equal(date$range_unit, "days")

  datetime <- observed$temporal[observed$temporal$name == "datetime_value", ]
  expect_equal(datetime$timezone, "America/Mexico_City")
  expect_match(datetime$min, "Z$")
  expect_equal(datetime$range_value, 4)
  expect_equal(datetime$range_unit, "seconds")
})

test_that("invalid temporal values and incompatible classes are explicit skips", {
  data <- data.frame(bad_date = c("2020-01-01", "not-a-date"), bad_number = c("1", "2"))
  spec <- data.frame(
    name = c("bad_date", "bad_number"),
    label = c("Bad date", "Bad number"),
    type = c("date", "numeric"),
    role = c("covariate", "covariate")
  )

  observed <- epi_eda_profile_summaries(data, spec)

  expect_equal(observed$variables$status, c("skipped", "skipped"))
  expect_setequal(observed$skipped$name, c("bad_date", "bad_number"))
  expect_match(paste(observed$skipped$reason, collapse = " "), "invalid|incompatible", ignore.case = TRUE)
})

test_that("typed epi_stats_summary uses the canonical builder", {
  data <- data.frame(
    numeric_value = c(1, 999, NA),
    category = factor(c("A", "B", NA), levels = c("A", "B", "C")),
    stringsAsFactors = FALSE
  )

  current <- epi_stats_summary(data, class_type = "int_num", action = "exclude")
  typed <- epi_stats_summary(data, codes = 999, output = "typed")

  expect_s3_class(current, "tbl_df")
  expect_named(typed, c("variables", "numeric", "categorical", "text", "temporal", "skipped"))
  expect_true(all(is.na(typed$variables$required)))
  expect_equal(typed$numeric$n_finite, 1L)
  expect_equal(typed$numeric$mean, 1)
  expect_equal(typed$categorical$level, c("A", "B", "C"))
  expect_equal(typed$categorical$n, c(1L, 1L, 0L))
  expect_error(epi_stats_summary(data, output = "typed", action = "codes_only"), "exclude")
})

test_that("typed epi_stats_summary records unsupported columns", {
  data <- data.frame(x = 1:2)
  data$list_column <- I(list(1, 2))

  observed <- epi_stats_summary(data, output = "typed")

  expect_equal(observed$variables$status, c("summarised", "skipped"))
  expect_equal(observed$skipped$name, "list_column")
  expect_match(observed$skipped$reason, "unsupported", ignore.case = TRUE)
})

test_that("canonical runs write exactly six deterministic summary tables", {
  fixture <- make_canonical_fixture()
  output_dir <- tempfile("eda-canonical-")
  dir.create(output_dir)

  observed <- epi_eda_run(fixture$data, fixture$spec, output_dir = output_dir)
  expected <- paste0(
    "summary_",
    c("variables", "numeric", "categorical", "text", "temporal", "skipped"),
    ".csv"
  )
  summary_files <- list.files(output_dir, pattern = "^summary_.*[.]csv$")

  expect_setequal(summary_files, expected)
  expect_equal(names(utils::read.csv(file.path(output_dir, "summary_variables.csv"), check.names = FALSE)), names(observed$summaries$variables))
  expect_equal(names(utils::read.csv(file.path(output_dir, "summary_temporal.csv"), check.names = FALSE)), names(observed$summaries$temporal))
})
