context("EDA Table 1 presentation")

library(testthat)
library(episcout)

test_that("Table 1 is stable, traceable and contains no inferential fields", {
  fixture <- make_stratified_fixture()
  result <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  table1 <- epi_eda_table1(result)

  expect_named(formals(epi_eda_table1), c("result", "basis"))
  expect_s3_class(table1, "data.frame")
  expect_named(table1, c(
    "variable_order", "row_order", "name", "label", "type", "level",
    "level_label", "statistic", "group_id", "group_label", "group_n",
    "denominator", "display", "note"
  ))
  expect_false("p_value" %in% names(table1))
  expect_false("arm" %in% table1$name)

  mean_a <- table1[
    table1$name == "value" & table1$statistic == "mean_sd" & table1$group_label == "A",
  ]
  source_a <- result$numeric[
    result$numeric$name == "value" & result$numeric$group_label == "A",
  ]
  expect_identical(mean_a$denominator, source_a$n_finite)
  expect_identical(mean_a$display, "2.0 (1.4)")

  missing <- table1[table1$name == "status" & table1$statistic == "missing", ]
  expect_true(all(missing$denominator == missing$group_n))
  expect_false(grepl(
    "disclosure|sharing|approval",
    paste(unique(table1$note), collapse = " "),
    ignore.case = TRUE
  ))
})

test_that("Table 1 categorical bases trace to the shared display contract", {
  fixture <- make_stratified_fixture()
  result <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  compatibility <- epi_eda_table1(result)
  explicit <- epi_eda_table1(result, "compatibility")
  column <- epi_eda_table1(result, "column")

  expect_identical(compatibility, explicit)
  categorical <- compatibility$type %in% c("categorical", "binary")
  expect_identical(
    compatibility[!categorical, , drop = FALSE],
    column[!categorical, , drop = FALSE]
  )
  yes_a <- column$name == "status" & column$group_label == "A" &
    !is.na(column$level) &
    column$level == "yes"
  expect_identical(column$denominator[yes_a], 2L)
  expect_identical(column$display[yes_a], "1 (50.0%)")
  expect_match(column$note[yes_a], "Column percentages")
})

test_that("Table 1 never reproduces text observations and validates input", {
  fixture <- make_stratified_fixture()
  result <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  table1 <- epi_eda_table1(result)
  rendered <- paste(capture.output(table1), collapse = " ")

  expect_false(grepl("secret-a", rendered, fixed = TRUE))
  expect_true(any(table1$name == "note" & table1$statistic == "observed_unique"))
  expect_true(any(table1$name == "note" & table1$statistic == "blank_whitespace"))
  expect_error(epi_eda_table1(list()), "epi_eda_stratified")

  broken <- result
  broken$numeric$mean <- NULL
  expect_error(epi_eda_table1(broken), "component contract")
  broken <- result
  broken$numeric$n_infinite <- NULL
  expect_error(epi_eda_table1(broken), "component contract")
  broken <- result
  broken$numeric$group_order <- NULL
  expect_error(epi_eda_table1(broken), "component contract")
  broken <- result
  broken$metadata <- 1
  expect_error(epi_eda_table1(broken), "component contract")
})

test_that("Table 1 falls back labels and states temporal timezone and units", {
  data <- data.frame(
    arm = c("A", "B"),
    when = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 00:00:02"), tz = "UTC")
  )
  spec <- data.frame(
    name = c("arm", "when"), label = c("Arm", ""),
    type = c("categorical", "datetime"), role = "measure",
    levels = c("A;B", ""), missing_codes = "", stringsAsFactors = FALSE
  )
  result <- epi_eda_profile_stratified(data, spec, "arm")
  table1 <- epi_eda_table1(result)
  temporal <- table1[table1$name == "when", , drop = FALSE]

  expect_true(all(temporal$label == "when"))
  expect_match(paste(temporal$note, collapse = " "), "Timezone: UTC")
  expect_match(paste(temporal$note, collapse = " "), "Range unit: seconds")
})
