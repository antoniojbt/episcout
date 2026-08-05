context("specification-guided EDA preparation")

library(testthat)
library(episcout)

preparation_spec <- function(name, type, levels = "", missing_codes = "",
                             required = TRUE, timezone = "") {
  data.frame(
    name = name,
    label = name,
    type = type,
    role = "measure",
    levels = levels,
    missing_codes = missing_codes,
    required = required,
    timezone = timezone,
    stringsAsFactors = FALSE
  )
}

audit_row <- function(result, name, stage) {
  result$audit[result$audit$name == name & result$audit$stage == stage, , drop = FALSE]
}

test_that("public preparation contract and audit mode are stable and non-mutating", {
  data <- data.frame(value = c(1, 999, NA_real_), extra = letters[1:3])
  original <- data
  spec <- preparation_spec("value", "numeric", missing_codes = "999")

  expect_named(formals(epi_eda_prepare), c(
    "data", "spec", "mode", "unexpected_levels", "extra_variables"
  ))
  expect_identical(formals(epi_eda_prepare)$mode, quote(c("audit", "apply")))
  expect_identical(formals(epi_eda_prepare)$unexpected_levels, quote(c("error", "append")))
  expect_identical(formals(epi_eda_prepare)$extra_variables, quote(c("keep", "error", "drop")))
  observed <- epi_eda_prepare(data, spec)

  expect_s3_class(observed, "epi_eda_preparation")
  expect_named(observed, c("data", "audit", "schema_before", "schema_after", "metadata"))
  expect_identical(observed$data, original)
  expect_identical(data, original)
  expect_null(observed$schema_after)
  expect_identical(observed$schema_before, epi_eda_check_schema(data, spec))
  expect_named(observed$audit, c(
    "name", "stage", "declared_type", "observed_class_before",
    "observed_class_after", "action", "status", "n_total",
    "n_standard_missing", "n_sentinel_missing", "n_invalid",
    "n_unexpected", "n_affected", "n_changed", "reason"
  ))
  expect_identical(observed$metadata$overall_status, "audited")
  expect_named(observed$metadata, c(
    "mode", "overall_status", "n_rows_before", "n_columns_before",
    "n_rows_after", "n_columns_after", "n_unchanged", "n_planned",
    "n_applied", "n_skipped", "n_warning", "n_blocking"
  ))
  expect_identical(audit_row(observed, "value", "missingness")$n_standard_missing, 1L)
  expect_identical(audit_row(observed, "value", "missingness")$n_sentinel_missing, 1L)
  expect_identical(audit_row(observed, "extra", "presence")$action, "keep_extra")
})

test_that("apply prepares supported types, sentinels and column order", {
  data <- data.frame(
    count = c(1, -999, 2),
    amount = c(1L, 2L, NA_integer_),
    category = c("b", "MISSING", "a"),
    binary = c(TRUE, FALSE, NA),
    text = factor(c("first", "unknown", "second")),
    date = c("2024-01-02", "missing-date", "2024-02-29"),
    datetime = c("2024-01-01T12:00:00Z", "missing-time", "2024-01-01T13:00:00+01:00"),
    extra = 1:3,
    stringsAsFactors = FALSE
  )
  spec <- rbind(
    preparation_spec("amount", "numeric"),
    preparation_spec("count", "integer", missing_codes = "-999"),
    preparation_spec("category", "categorical", "b;a;unused", "MISSING"),
    preparation_spec("binary", "binary"),
    preparation_spec("text", "text", missing_codes = "unknown"),
    preparation_spec("date", "date", missing_codes = "missing-date"),
    preparation_spec("datetime", "datetime", missing_codes = "missing-time")
  )
  original <- data

  observed <- epi_eda_prepare(data, spec, mode = "apply")

  expect_identical(observed$metadata$overall_status, "prepared")
  expect_identical(names(observed$data), c(spec$name, "extra"))
  expect_identical(nrow(observed$data), nrow(data))
  expect_identical(data, original)
  expect_type(observed$data$amount, "double")
  expect_identical(observed$data$count, c(1L, NA_integer_, 2L))
  expect_identical(levels(observed$data$category), c("b", "a", "unused"))
  expect_true(is.na(observed$data$category[[2]]))
  expect_type(observed$data$binary, "logical")
  expect_identical(observed$data$text, c("first", NA_character_, "second"))
  expect_s3_class(observed$data$date, "Date")
  expect_s3_class(observed$data$datetime, "POSIXct")
  expect_identical(attr(observed$data$datetime, "tzone"), "UTC")
  expect_identical(as.numeric(observed$data$datetime[c(1, 3)]), rep(1704110400, 2))
  expect_true(all(observed$schema_after$type_status[observed$schema_after$expected_present] == "compatible"))
  expect_true(all(observed$audit$status != "planned"))
})

test_that("apply is all-or-nothing and reports every blocker without raw values", {
  secret_numeric <- "patient-secret-4,2"
  secret_level <- "diagnosis-secret"
  data <- data.frame(valid = c(1, 999), bad_number = c("1", secret_numeric), group = c("ok", secret_level))
  spec <- rbind(
    preparation_spec("valid", "numeric", missing_codes = "999"),
    preparation_spec("bad_number", "numeric"),
    preparation_spec("group", "categorical", levels = "ok")
  )

  observed <- epi_eda_prepare(data, spec, mode = "apply")
  rendered_audit <- paste(capture.output(str(observed$audit)), collapse = " ")
  rendered_metadata <- paste(capture.output(str(observed$metadata)), collapse = " ")

  expect_identical(observed$metadata$overall_status, "blocked")
  expect_identical(observed$data, data)
  expect_null(observed$schema_after)
  expect_gte(observed$metadata$n_blocking, 2L)
  expect_identical(audit_row(observed, "valid", "missingness")$status, "planned")
  expect_false(grepl(secret_numeric, rendered_audit, fixed = TRUE))
  expect_false(grepl(secret_level, rendered_audit, fixed = TRUE))
  expect_false(grepl(secret_numeric, rendered_metadata, fixed = TRUE))
  expect_false(grepl(secret_level, rendered_metadata, fixed = TRUE))
})

test_that("integer conversion enforces finite whole representable values", {
  valid <- data.frame(x = c(-.Machine$integer.max, 0, .Machine$integer.max))
  spec <- preparation_spec("x", "integer")
  applied <- epi_eda_prepare(valid, spec, mode = "apply")
  expect_identical(applied$data$x, c(-.Machine$integer.max, 0L, .Machine$integer.max))

  invalid <- data.frame(x = c(1.5, Inf, .Machine$integer.max + 1))
  blocked <- epi_eda_prepare(invalid, spec, mode = "apply")
  expect_identical(audit_row(blocked, "x", "type")$n_invalid, 3L)
  expect_identical(blocked$data, invalid)

  character_values <- epi_eda_prepare(
    data.frame(x = c("999", "999")),
    preparation_spec("x", "integer", missing_codes = "999"),
    mode = "apply"
  )
  expect_identical(character_values$metadata$overall_status, "blocked")
})

test_that("categorical append is deterministic but binary append blocks", {
  data <- data.frame(category = c("z", "b", "a", "z"), binary = c("yes", "no", "maybe", "yes"))
  spec <- rbind(
    preparation_spec("category", "categorical", "b;a"),
    preparation_spec("binary", "binary", "no;yes")
  )

  blocked <- epi_eda_prepare(data, spec, mode = "apply", unexpected_levels = "append")
  expect_identical(blocked$metadata$overall_status, "blocked")
  expect_identical(audit_row(blocked, "category", "levels")$status, "warning")
  expect_identical(audit_row(blocked, "binary", "levels")$status, "blocking")

  categorical_only <- epi_eda_prepare(data["category"], spec[1, ], mode = "apply", unexpected_levels = "append")
  expect_identical(levels(categorical_only$data$category), c("b", "a", "z"))
  expect_identical(spec$levels, c("b;a", "no;yes"))
})

test_that("presence, extras, duplicates and review gates are explicit", {
  data <- data.frame(present = c(1, 1), extra = c("x", "x"))
  spec <- rbind(
    preparation_spec("present", "numeric"),
    preparation_spec("required_missing", "text", required = TRUE),
    preparation_spec("optional_missing", "text", required = NA)
  )
  audited <- epi_eda_prepare(data, spec)
  expect_identical(audit_row(audited, "required_missing", "presence")$status, "blocking")
  expect_identical(audit_row(audited, "required_missing", "presence")$n_total, nrow(data))
  expect_identical(audit_row(audited, "optional_missing", "presence")$status, "skipped")
  expect_identical(audit_row(audited, ".dataset.duplicate_rows", "dataset")$n_affected, 1L)

  kept <- epi_eda_prepare(data, spec[c(1, 3), ], mode = "apply")
  dropped <- epi_eda_prepare(data, spec[c(1, 3), ], mode = "apply", extra_variables = "drop")
  errored <- epi_eda_prepare(data, spec[c(1, 3), ], mode = "apply", extra_variables = "error")
  expect_identical(names(kept$data), c("present", "extra"))
  expect_identical(names(dropped$data), "present")
  expect_identical(audit_row(dropped, "extra", "presence")$n_changed, nrow(data))
  expect_identical(errored$metadata$overall_status, "blocked")

  scaffold <- epi_eda_spec_scaffold(data.frame(present = 1:2))
  expect_identical(epi_eda_prepare(data.frame(present = 1:2), scaffold)$metadata$overall_status, "audited")
  expect_identical(epi_eda_prepare(data.frame(present = 1:2), scaffold, mode = "apply")$metadata$overall_status, "blocked")
  scaffold$review_status <- "reviewed"
  expect_identical(epi_eda_prepare(data.frame(present = 1:2), scaffold, mode = "apply")$metadata$overall_status, "prepared")
})

test_that("strict temporal parsing requires reviewed local timezone and rejects DST ambiguity", {
  spec <- preparation_spec("when", "datetime")
  local <- data.frame(when = "2024-01-15T12:30:00")
  expect_identical(epi_eda_prepare(local, spec, mode = "apply")$metadata$overall_status, "blocked")

  spec$timezone <- "America/New_York"
  applied <- epi_eda_prepare(local, spec, mode = "apply")
  expect_identical(applied$metadata$overall_status, "prepared")
  expect_identical(format(applied$data$when, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "2024-01-15T17:30:00Z")
  fractional <- epi_eda_prepare(
    data.frame(when = "2024-01-15T12:30:00.9"), spec, mode = "apply"
  )
  expect_identical(fractional$metadata$overall_status, "prepared")
  expect_equal(as.numeric(fractional$data$when) %% 1, 0.9, tolerance = 1e-6)

  unusual_offset <- epi_eda_prepare(
    data.frame(when = "2024-01-01T12:00:00+01:01"), spec, mode = "apply"
  )
  expect_identical(
    format(unusual_offset$data$when, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "2024-01-01T10:59:00Z"
  )
  negative_offset <- epi_eda_prepare(
    data.frame(when = "2024-01-01T12:00:00-05:30"), spec,
    mode = "apply"
  )
  expect_identical(
    format(negative_offset$data$when, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    "2024-01-01T17:30:00Z"
  )

  utc_spec <- spec
  utc_spec$timezone <- "UTC"
  utc_local <- epi_eda_prepare(
    data.frame(when = "2024-01-01 12:00:00.25"), utc_spec,
    mode = "apply"
  )
  expect_identical(utc_local$metadata$overall_status, "prepared")
  expect_equal(as.numeric(utc_local$data$when), 1704110400.25, tolerance = 1e-6)

  mixed <- epi_eda_prepare(
    data.frame(when = c("2024-01-15T12:30:00", "2024-01-15T17:30:00Z")),
    spec,
    mode = "apply"
  )
  expect_identical(mixed$metadata$overall_status, "prepared")
  expect_identical(as.numeric(mixed$data$when), rep(1705339800, 2))

  nonexistent <- data.frame(when = "2024-03-10T02:30:00")
  ambiguous <- data.frame(when = "2024-11-03T01:30:00")
  historical_ambiguity <- data.frame(when = "1969-09-30T12:30:00")
  expect_identical(epi_eda_prepare(nonexistent, spec, mode = "apply")$metadata$overall_status, "blocked")
  expect_identical(epi_eda_prepare(ambiguous, spec, mode = "apply")$metadata$overall_status, "blocked")
  historical_spec <- spec
  historical_spec$timezone <- "Pacific/Kwajalein"
  expect_identical(
    epi_eda_prepare(historical_ambiguity, historical_spec, mode = "apply")$metadata$overall_status,
    "blocked"
  )
  historical_unique <- epi_eda_prepare(
    data.frame(when = "1969-09-29T12:30:00"), historical_spec,
    mode = "apply"
  )
  expect_identical(historical_unique$metadata$overall_status, "prepared")
  expect_identical(attr(historical_unique$data$when, "tzone"), "UTC")
  expect_identical(as.numeric(historical_unique$data$when), -8116200)
  historical_gap <- epi_eda_prepare(
    data.frame(when = "1993-08-21T12:30:00"), historical_spec,
    mode = "apply"
  )
  expect_identical(historical_gap$metadata$overall_status, "blocked")

  under_utc <- withr::with_envvar(
    c(TZ = "UTC"),
    epi_eda_prepare(data.frame(when = "1969-09-29T12:30:00"), historical_spec, mode = "apply")
  )
  under_auckland <- withr::with_envvar(
    c(TZ = "Pacific/Auckland"),
    epi_eda_prepare(data.frame(when = "1969-09-29T12:30:00"), historical_spec, mode = "apply")
  )
  expect_identical(under_utc$metadata$overall_status, under_auckland$metadata$overall_status)
  expect_identical(as.numeric(under_utc$data$when), as.numeric(under_auckland$data$when))

  leap_second <- data.frame(when = "2024-01-01T12:00:60Z")
  expect_identical(epi_eda_prepare(leap_second, spec, mode = "apply")$metadata$overall_status, "blocked")
  spec$timezone <- "Not/A-Timezone"
  expect_identical(epi_eda_prepare(local, spec, mode = "apply")$metadata$overall_status, "blocked")
  spec$timezone <- " America/New_York "
  expect_identical(epi_eda_prepare(local, spec, mode = "apply")$metadata$overall_status, "blocked")
})

test_that("temporal blockers remain actionable and value-free", {
  observed_value <- "1969-09-30T12:30:00"
  spec <- preparation_spec("when", "datetime", timezone = "Pacific/Kwajalein")
  result <- expect_no_warning(
    epi_eda_prepare(data.frame(when = observed_value), spec, mode = "apply")
  )
  type_row <- audit_row(result, "when", "type")
  rendered <- paste(capture.output(str(list(result$audit, result$metadata))), collapse = " ")

  expect_identical(result$metadata$overall_status, "blocked")
  expect_identical(type_row$n_invalid, 1L)
  expect_match(type_row$reason, "explicit Z or numeric offset", fixed = TRUE)
  expect_false(grepl(observed_value, rendered, fixed = TRUE))

  invalid_value <- "2024-02-30T12:30:00"
  invalid <- expect_no_warning(
    epi_eda_prepare(data.frame(when = invalid_value), spec, mode = "apply")
  )
  invalid_rendered <- paste(capture.output(str(list(invalid$audit, invalid$metadata))), collapse = " ")
  expect_identical(invalid$metadata$overall_status, "blocked")
  expect_false(grepl(invalid_value, invalid_rendered, fixed = TRUE))
})

test_that("zero rows, unsupported columns and invalid names are handled safely", {
  empty <- data.frame(x = numeric(), extra = character())
  spec <- preparation_spec("x", "integer")
  applied <- epi_eda_prepare(empty, spec, mode = "apply", extra_variables = "drop")
  expect_identical(nrow(applied$data), 0L)
  expect_type(applied$data$x, "integer")

  nested <- data.frame(id = 1:2)
  nested$payload <- I(list(list(a = 1), list(a = 2)))
  nested_spec <- rbind(preparation_spec("id", "integer"), preparation_spec("payload", "text"))
  expect_identical(epi_eda_prepare(nested, nested_spec, mode = "apply")$metadata$overall_status, "blocked")
  expect_identical(epi_eda_prepare(nested, nested_spec[1, ], mode = "apply")$metadata$overall_status, "prepared")

  nested_category <- preparation_spec(
    "payload", "categorical", levels = "", timezone = "UTC"
  )
  nested_audit <- epi_eda_prepare(nested, nested_category)
  expect_identical(
    nested_audit$audit$stage[nested_audit$audit$name == "payload"],
    c("presence", "missingness", "type", "type", "levels")
  )
  expect_identical(audit_row(nested_audit, "payload", "levels")$status, "blocking")

  duplicate <- data.frame(a = 1, b = 2)
  names(duplicate) <- c("a", "a")
  expect_error(epi_eda_prepare(duplicate, preparation_spec("a", "numeric")), "Duplicate")
  reserved <- data.frame(.dataset.secret = 1, check.names = FALSE)
  expect_error(epi_eda_prepare(reserved, preparation_spec("x", "numeric", required = FALSE)), "reserved")

  wrong_empty_storage <- epi_eda_prepare(
    data.frame(x = character()), preparation_spec("x", "numeric"), mode = "apply"
  )
  expect_identical(wrong_empty_storage$metadata$overall_status, "blocked")
})

test_that("literal text, non-finite values, bounds and metadata warnings retain meaning", {
  data <- data.frame(
    number = c(Inf, -Inf, NA_real_),
    category = c("NA", "", " "),
    stringsAsFactors = FALSE
  )
  spec <- rbind(
    preparation_spec("number", "numeric"),
    preparation_spec("category", "categorical", levels = "NA;; ")
  )
  spec$min <- c("0", "")
  spec$max <- c("1", "")
  spec$timezone <- c("Europe/London", "")

  unsafe <- epi_eda_prepare(data, spec, mode = "apply")
  numeric_type <- audit_row(unsafe, "number", "type")
  numeric_type <- numeric_type[numeric_type$action == "retain_numeric", , drop = FALSE]
  expect_identical(unsafe$metadata$overall_status, "blocked")
  expect_identical(numeric_type$n_invalid, 0L)
  expect_identical(numeric_type$status, "unchanged")
  expect_identical(audit_row(unsafe, "number", "type")[2, ]$status, "warning")

  spec$levels[[2]] <- "NA"
  spec$timezone <- ""
  category_only <- epi_eda_prepare(data["category"], spec[2, ], mode = "apply", unexpected_levels = "append")
  expect_identical(category_only$metadata$overall_status, "blocked")
  expect_identical(audit_row(category_only, "category", "levels")$n_unexpected, 2L)

  literal <- epi_eda_prepare(
    data.frame(category = c("NA", NA_character_)),
    preparation_spec("category", "categorical", levels = "NA"),
    mode = "apply"
  )
  expect_identical(as.character(literal$data$category), c("NA", NA_character_))

  trailing_level <- epi_eda_prepare(
    data.frame(category = "a"),
    preparation_spec("category", "categorical", levels = "a;"),
    mode = "apply"
  )
  expect_identical(trailing_level$metadata$overall_status, "blocked")
})

test_that("integer sentinels are masked before coercion and stage order is stable", {
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  data <- data.frame(value = c(1, .Machine$integer.max + 1))
  spec <- preparation_spec(
    "value", "integer", missing_codes = as.character(.Machine$integer.max + 1),
    timezone = "UTC"
  )

  observed <- expect_silent(epi_eda_prepare(data, spec, mode = "apply"))
  expect_identical(observed$data$value, c(1L, NA_integer_))
  expect_identical(observed$audit$stage[observed$audit$name == "value"], c(
    "presence", "missingness", "type", "type"
  ))

  absent <- preparation_spec("category", "categorical", levels = "a", required = FALSE, timezone = "UTC")
  absent_audit <- epi_eda_prepare(data.frame(other = 1), absent)
  expect_identical(absent_audit$audit$stage[absent_audit$audit$name == "category"], c("presence", "type"))
  expect_identical(audit_row(absent_audit, "category", "type")$status, "warning")
})

test_that("successful preparation returns an ordinary copy without data.table mutation", {
  skip_if_not_installed("data.table")
  data <- data.table::data.table(value = c(1L, 2L), extra = c("a", "b"))
  original <- data.table::copy(data)
  spec <- preparation_spec("value", "numeric")

  observed <- epi_eda_prepare(data, spec, mode = "apply", extra_variables = "drop")

  expect_s3_class(observed$data, "data.frame")
  expect_false(inherits(observed$data, "data.table"))
  expect_identical(data, original)
  expect_identical(observed$data$value, c(1, 2))
})

test_that("prepared missingness reconciles with canonical EDA", {
  data <- data.frame(value = c(1, 999, NA_real_), group = c("a", "M", "b"))
  spec <- rbind(
    preparation_spec("value", "numeric", missing_codes = "999"),
    preparation_spec("group", "categorical", levels = "a;b", missing_codes = "M")
  )
  prepared <- epi_eda_prepare(data, spec, mode = "apply")
  canonical <- epi_eda_run(prepared$data, spec)
  missing_rows <- prepared$audit[prepared$audit$stage == "missingness", ]

  expect_identical(canonical$metadata$n_rows, nrow(data))
  expect_identical(
    canonical$summaries$variables$n_missing,
    missing_rows$n_standard_missing + missing_rows$n_sentinel_missing
  )
})
