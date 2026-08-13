context("stratified EDA summaries")

library(testthat)
library(episcout)

make_stratified_fixture <- function() {
  data <- data.frame(
    arm = c("A", "A", "B", "C", "MISS", NA),
    value = c(1, 3, 2, Inf, 999, NA_real_),
    status = c("yes", "no", "yes", "other", "NA", NA),
    note = c("secret-a", "secret-b", "secret-c", "secret-d", "secret-e", NA),
    visit = as.Date(c("2024-01-01", "2024-01-03", "2024-01-02", "2024-01-04", NA, NA)),
    participant_id = 1:6,
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("arm", "value", "status", "note", "visit", "participant_id", "absent"),
    label = c("Study arm", "Value", "Status", "Note", "Visit", "Participant", "Absent"),
    database_type = "text", analysis_type = c("categorical", "numeric", "categorical", "text", "date", "integer", "text"),
    role = c("exposure", "measure", "measure", "measure", "measure", "identifier", "measure"),
    levels = c("B;A;D", "", "no;yes;unused", "", "", "", ""),
    missing_codes = c("MISS", "999", "", "", "", "", ""),
    required = c(rep(TRUE, 6), FALSE),
    stringsAsFactors = FALSE
  )
  list(data = data, spec = spec)
}

test_that("stratified contract preserves group order, empty groups and reconciliation", {
  fixture <- make_stratified_fixture()
  original_data <- fixture$data
  original_spec <- fixture$spec
  observed <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")

  expect_named(formals(epi_eda_profile_stratified), c(
    "data", "spec", "strata", "include_overall", "include_missing_stratum"
  ))
  expect_s3_class(observed, "epi_eda_stratified")
  expect_named(observed, c(
    "groups", "variables", "numeric", "categorical", "text",
    "temporal", "skipped", "metadata"
  ))
  expect_identical(observed$groups$group_label, c("Overall", "B", "A", "D", "C", "Missing"))
  expect_identical(observed$groups$n, c(6L, 1L, 2L, 0L, 1L, 2L))
  expect_identical(observed$groups$is_unexpected_stratum, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(sum(observed$groups$n[!observed$groups$is_overall]), nrow(fixture$data))
  expect_identical(fixture$data, original_data)
  expect_identical(fixture$spec, original_spec)

  prefix <- c(
    "group_id", "group_order", "group_value", "group_label", "is_overall",
    "is_missing_stratum", "is_unexpected_stratum"
  )
  expect_named(observed$groups, c(prefix, "is_declared_stratum", "n"))
  expect_named(observed$variables, c(
    prefix, "name", "label", "type", "role", "required", "n", "n_missing",
    "n_observed", "n_unique", "n_infinite", "status", "reason"
  ))
  expect_named(observed$numeric, c(
    prefix, "name", "type", "n", "n_missing", "n_observed", "n_infinite",
    "n_finite", "sum", "min", "q1", "mean", "median", "q3", "max", "iqr",
    "sd", "variance", "sem", "cv", "skewness", "kurtosis", "shapiro_p",
    "lower_fence", "upper_fence", "n_below_lower", "n_above_upper",
    "outlier_count", "outlier_percentage"
  ))
  expect_named(observed$categorical, c(
    prefix, "name", "type", "level", "n", "n_total", "n_observed", "p_total",
    "p_observed", "is_declared", "is_unexpected", "is_missing_level"
  ))
  expect_named(observed$text, c(
    prefix, "name", "n", "n_missing", "n_observed", "n_unique", "n_empty",
    "n_whitespace", "min_length", "max_length"
  ))
  expect_named(observed$temporal, c(
    prefix, "name", "source_class", "timezone", "n", "n_missing", "n_observed",
    "n_unique", "min", "q1", "median", "q3", "max", "range_value", "range_unit"
  ))
  expect_named(observed$skipped, c(prefix, "name", "type", "observed_class", "reason"))
  expect_named(observed$metadata, c(
    "strata", "strata_label", "include_overall", "include_missing_stratum",
    "n_input", "n_included", "n_omitted_missing_stratum", "n_strata",
    "summary_contract", "stratified_contract"
  ))
  expect_type(observed$groups$group_order, "integer")
  expect_type(observed$variables$status, "character")
  expect_type(observed$categorical$is_missing_level, "logical")
})

test_that("group numeric results reuse canonical values and expose denominators", {
  fixture <- make_stratified_fixture()
  observed <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  overall <- observed$numeric[observed$numeric$is_overall & observed$numeric$name == "value", ]
  group_a <- observed$numeric[observed$numeric$group_label == "A" & observed$numeric$name == "value", ]
  canonical <- epi_eda_profile_summaries(fixture$data, fixture$spec)

  expect_identical(group_a$n, 2L)
  expect_identical(group_a$n_finite, 2L)
  expect_equal(group_a$mean, 2)
  expect_equal(group_a$sd, sqrt(2))
  expect_identical(overall$n, canonical$variables$n[canonical$variables$name == "value"])
  expect_identical(overall$n_missing, canonical$variables$n_missing[canonical$variables$name == "value"])
  expect_equal(overall$mean, canonical$numeric$mean[canonical$numeric$name == "value"])
  expect_identical(overall$n_infinite, 1L)
})

test_that("categorical cells include zero, unexpected and explicit missing levels", {
  fixture <- make_stratified_fixture()
  observed <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  status_a <- observed$categorical[
    observed$categorical$group_label == "A" & observed$categorical$name == "status",
  ]

  expect_identical(status_a$level, c("no", "yes", "unused", "NA", "other", NA_character_))
  expect_identical(status_a$n, c(1L, 1L, 0L, 0L, 0L, 0L))
  expect_identical(status_a$n_total, rep(2L, 6))
  expect_identical(status_a$n_observed, rep(2L, 6))
  expect_equal(status_a$p_observed[1:5], c(.5, .5, 0, 0, 0))
  expect_true(status_a$is_unexpected[!is.na(status_a$level) & status_a$level == "other"])
  expect_true(status_a$is_unexpected[!is.na(status_a$level) & status_a$level == "NA"])
  expect_true(status_a$is_missing_level[[6]])
})

test_that("missing-stratum exclusion changes Overall population explicitly", {
  fixture <- make_stratified_fixture()
  observed <- epi_eda_profile_stratified(
    fixture$data, fixture$spec, "arm", include_missing_stratum = FALSE
  )
  overall <- observed$groups[observed$groups$is_overall, ]

  expect_identical(observed$metadata$n_input, 6L)
  expect_identical(observed$metadata$n_included, 4L)
  expect_identical(observed$metadata$n_omitted_missing_stratum, 2L)
  expect_identical(overall$n, 4L)
  expect_false(any(observed$groups$is_missing_stratum))
  expect_identical(sum(observed$groups$n[!observed$groups$is_overall]), 4L)
})

test_that("text is aggregate-only and skipped variables remain explicit", {
  fixture <- make_stratified_fixture()
  fixture$data$payload <- I(lapply(seq_len(nrow(fixture$data)), function(x) list(secret = x)))
  observed <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  rendered <- paste(capture.output(str(observed)), collapse = " ")

  expect_true(all(c("n_empty", "n_whitespace", "min_length", "max_length") %in% names(observed$text)))
  expect_false(grepl("secret-a", rendered, fixed = TRUE))
  expect_true(all(c("absent", "payload") %in% observed$skipped$name))
  expect_false("participant_id" %in% observed$skipped$name)
  expect_true("participant_id" %in% observed$numeric$name)
  expect_match(paste(observed$skipped$reason, collapse = " "), "not found|not declared", ignore.case = TRUE)
})

test_that("zero rows and invalid strata have deliberate behavior", {
  fixture <- make_stratified_fixture()
  empty <- fixture$data[0, ]
  observed <- epi_eda_profile_stratified(empty, fixture$spec, "arm")
  expect_identical(observed$groups$group_label, c("Overall", "B", "A", "D"))
  expect_true(all(observed$groups$n == 0L))

  expect_error(epi_eda_profile_stratified(fixture$data, fixture$spec, c("arm", "status")), "single")
  expect_error(epi_eda_profile_stratified(fixture$data, fixture$spec, "value"), "categorical|binary")
  expect_error(epi_eda_profile_stratified(fixture$data, fixture$spec, "unknown"), "present|specification")
  expect_error(epi_eda_profile_stratified(fixture$data, fixture$spec, "arm", include_overall = NA), "TRUE or FALSE")

  duplicate_levels <- fixture$spec
  duplicate_levels$levels[duplicate_levels$name == "arm"] <- "A;A;B"
  expect_error(epi_eda_profile_stratified(fixture$data, duplicate_levels, "arm"), "unique")

  duplicate_names <- fixture$data
  names(duplicate_names)[2] <- "arm"
  expect_error(epi_eda_profile_stratified(duplicate_names, fixture$spec, "arm"), "Duplicate")

  extra <- fixture$spec
  extra$custom_state <- "unreviewed"
  expect_s3_class(
    epi_eda_profile_stratified(fixture$data, extra, "arm"),
    "epi_eda_stratified"
  )
})

test_that("all-missing strata and local character datetimes remain explicit", {
  data <- data.frame(arm = NA_character_, when = "2024-01-01T12:00:00")
  spec <- data.frame(
    name = c("arm", "when"), label = c("Arm", ""),
    database_type = "text", analysis_type = c("categorical", "datetime"), role = "measure",
    levels = c("A", ""), missing_codes = "", stringsAsFactors = FALSE
  )
  observed <- epi_eda_profile_stratified(
    data, spec, "arm", include_overall = FALSE, include_missing_stratum = FALSE
  )

  expect_identical(observed$groups$group_label, "A")
  expect_identical(observed$groups$n, 0L)
  expect_identical(observed$metadata$n_omitted_missing_stratum, 1L)
  expect_true("when" %in% observed$skipped$name)
  expect_match(observed$skipped$reason[observed$skipped$name == "when"], "prepare")
})

test_that("integer, binary, datetime and numeric edge semantics remain canonical", {
  data <- data.frame(
    arm = c("A", "A", "B", "B"),
    integer_value = 1:4,
    binary_value = factor(c("yes", "no", "yes", "yes"), levels = c("no", "yes")),
    edge = c(NA_real_, NaN, Inf, -Inf),
    constant = rep(5, 4),
    all_missing = rep(NA_real_, 4),
    when = as.POSIXct(c(
      "2024-01-01 00:00:00", "2024-01-01 00:00:02",
      "2024-01-01 00:00:04", "2024-01-01 00:00:06"
    ), tz = "UTC")
  )
  spec <- data.frame(
    name = names(data), label = names(data),
    database_type = "text", analysis_type = c("categorical", "integer", "binary", "numeric", "numeric", "numeric", "datetime"),
    role = "measure", levels = c("A;B", "", "no;yes", "", "", "", ""),
    missing_codes = "", stringsAsFactors = FALSE
  )
  utc <- withr::with_envvar(
    c(TZ = "UTC"),
    epi_eda_profile_stratified(data, spec, "arm")
  )
  pacific <- withr::with_envvar(
    c(TZ = "Pacific/Auckland"),
    epi_eda_profile_stratified(data, spec, "arm")
  )

  integer_a <- utc$numeric[utc$numeric$name == "integer_value" & utc$numeric$group_label == "A", ]
  expect_identical(integer_a$type, "integer")
  expect_equal(integer_a$mean, 1.5)
  expect_identical(
    utc$categorical$n[
      utc$categorical$name == "binary_value" & utc$categorical$group_label == "B" &
        !is.na(utc$categorical$level) & utc$categorical$level == "yes"
    ],
    2L
  )
  edge <- utc$numeric[utc$numeric$name == "edge" & utc$numeric$is_overall, ]
  expect_identical(edge$n_missing, 2L)
  expect_identical(edge$n_infinite, 2L)
  expect_identical(edge$n_finite, 0L)
  expect_true(is.na(edge$mean))
  expect_equal(utc$numeric$sd[utc$numeric$name == "constant" & utc$numeric$is_overall], 0)
  expect_true(is.na(utc$numeric$mean[utc$numeric$name == "all_missing" & utc$numeric$is_overall]))
  expect_identical(utc$temporal$timezone[utc$temporal$name == "when"], rep("UTC", 3))
  expect_identical(utc$temporal, pacific$temporal)
  expect_type(utc$numeric$n, "integer")
  expect_type(utc$categorical$p_observed, "double")
})
