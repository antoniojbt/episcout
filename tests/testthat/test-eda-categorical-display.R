context("categorical EDA denominator presentation")

library(testthat)
library(episcout)

make_cat_display_fixture <- function() {
  data <- data.frame(
    arm = c("A", "A", "A", "B", "B", "B", NA),
    status = c("yes", "no", NA, "yes", "other", "MISS", "unused"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("arm", "status"), label = c("Arm", "Status"),
    type = c("categorical", "categorical"), role = c("exposure", "measure"),
    levels = c("A;B;C", "no;yes;unused;never"),
    missing_codes = c("", "MISS"), required = TRUE,
    stringsAsFactors = FALSE
  )
  list(data = data, spec = spec)
}

display_cell <- function(display, group, level = NULL, missing = FALSE) {
  level_match <- if (missing) {
    display$is_missing_level
  } else {
    !display$is_missing_level & !is.na(display$level) & display$level == level
  }
  display[
    display$name == "status" & display$group_label == group & level_match, ,
    drop = FALSE
  ]
}

expect_display_cell <- function(display, group, level = NULL, missing = FALSE,
                                numerator, denominator, proportion) {
  observed <- display_cell(display, group, level, missing)
  expect_equal(nrow(observed), 1L)
  expect_identical(observed$numerator, as.integer(numerator))
  expect_identical(observed$denominator, as.integer(denominator))
  expect_equal(observed$proportion, proportion)
}

test_that("canonical display has a fixed typed aggregate contract", {
  fixture <- make_cat_display_fixture()
  summaries <- epi_eda_profile_summaries(fixture$data, fixture$spec)
  original <- summaries
  observed <- epi_eda_categorical_display(summaries)

  expect_named(formals(epi_eda_categorical_display), c("result", "basis"))
  expect_named(observed, c(
    "variable_order", "level_order", "name", "label", "type", "level",
    "group_id", "group_order", "group_label", "is_overall", "group_n",
    "population_n", "numerator", "denominator", "proportion",
    "percentage_basis", "denominator_scope", "missing_treatment",
    "is_missing_level"
  ))
  expect_identical(vapply(observed, typeof, character(1)), c(
    variable_order = "integer", level_order = "integer", name = "character",
    label = "character", type = "character", level = "character",
    group_id = "character", group_order = "integer", group_label = "character",
    is_overall = "logical", group_n = "integer", population_n = "integer",
    numerator = "integer", denominator = "integer", proportion = "double",
    percentage_basis = "character", denominator_scope = "character",
    missing_treatment = "character", is_missing_level = "logical"
  ))
  expect_true(all(observed$group_id == ".overall"))
  expect_true(all(observed$percentage_basis == "compatibility"))
  expect_display_cell(observed, "Overall", "yes", numerator = 2, denominator = 5, proportion = 2 / 5)
  expect_display_cell(observed, "Overall", missing = TRUE, numerator = 2, denominator = 7, proportion = 2 / 7)
  expect_identical(summaries, original)
})

test_that("stratified display implements all four literal denominator bases", {
  fixture <- make_cat_display_fixture()
  stratified <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  original <- stratified

  compatibility <- epi_eda_categorical_display(stratified)
  expect_display_cell(compatibility, "A", "yes", numerator = 1, denominator = 2, proportion = 1 / 2)
  expect_display_cell(compatibility, "A", missing = TRUE, numerator = 1, denominator = 3, proportion = 1 / 3)
  expect_display_cell(compatibility, "B", "other", numerator = 1, denominator = 2, proportion = 1 / 2)
  expect_display_cell(compatibility, "Overall", "yes", numerator = 2, denominator = 5, proportion = 2 / 5)
  expect_display_cell(compatibility, "Overall", missing = TRUE, numerator = 2, denominator = 7, proportion = 2 / 7)
  c_never <- display_cell(compatibility, "C", "never")
  expect_identical(c_never$denominator, 0L)
  expect_true(is.na(c_never$proportion))

  column <- epi_eda_categorical_display(stratified, "column")
  expect_display_cell(column, "A", "yes", numerator = 1, denominator = 3, proportion = 1 / 3)
  expect_display_cell(column, "Overall", "yes", numerator = 2, denominator = 7, proportion = 2 / 7)

  row <- expect_warning(epi_eda_categorical_display(stratified, "row"), NA)
  expect_display_cell(row, "A", "yes", numerator = 1, denominator = 2, proportion = 1 / 2)
  expect_display_cell(row, "B", "yes", numerator = 1, denominator = 2, proportion = 1 / 2)
  expect_display_cell(row, "Overall", "yes", numerator = 2, denominator = 2, proportion = 1)
  expect_display_cell(row, "Missing", "unused", numerator = 1, denominator = 1, proportion = 1)
  never <- row$name == "status" & !is.na(row$level) & row$level == "never"
  expect_true(all(row$denominator[never] == 0L))
  expect_true(all(is.na(row$proportion[never])))

  overall <- epi_eda_categorical_display(stratified, "overall")
  expect_display_cell(overall, "A", "yes", numerator = 1, denominator = 7, proportion = 1 / 7)
  expect_display_cell(overall, "B", "other", numerator = 1, denominator = 7, proportion = 1 / 7)
  expect_display_cell(overall, "Overall", missing = TRUE, numerator = 2, denominator = 7, proportion = 2 / 7)
  expect_identical(stratified, original)
})

test_that("empty and corrupt aggregate inputs fail or remain explicit", {
  numeric <- epi_eda_profile_summaries(
    data.frame(value = numeric()),
    data.frame(
      name = "value", label = "Value", type = "numeric", role = "measure",
      stringsAsFactors = FALSE
    )
  )
  empty <- epi_eda_categorical_display(numeric)
  expect_equal(nrow(empty), 0L)
  expect_named(empty, getFromNamespace("eda_categorical_display_names", "episcout")())

  fixture <- make_cat_display_fixture()
  canonical <- epi_eda_profile_summaries(fixture$data, fixture$spec)
  expect_error(epi_eda_categorical_display(canonical, "row"), "requires an epi_eda_stratified")
  expect_error(epi_eda_categorical_display(canonical, "invalid"), "arg")
  expect_error(epi_eda_categorical_display(data.frame()), "canonical EDA summary")
  broken <- canonical
  broken$categorical$n[[1]] <- 999L
  error <- tryCatch(epi_eda_categorical_display(broken), error = identity)
  expect_match(conditionMessage(error), "reconcile")
  expect_false(grepl("999", conditionMessage(error), fixed = TRUE))

  stratified <- epi_eda_profile_stratified(fixture$data, fixture$spec, "arm")
  broken <- stratified
  remove <- broken$categorical$name == "status" &
    broken$categorical$group_label == "C" &
    !is.na(broken$categorical$level) & broken$categorical$level == "never"
  broken$categorical <- broken$categorical[!remove, , drop = FALSE]
  expect_error(epi_eda_categorical_display(broken), "did not reconcile")
})

test_that("frequency companions retain count fields and recalculate collapse proportions", {
  fixture <- make_cat_display_fixture()
  entries <- getFromNamespace("eda_data_frame_plot_data", "episcout")(
    fixture$data, epi_eda_spec(fixture$spec), max_plot_levels = 2L
  )
  compact <- entries$entries$status$data

  expect_named(compact, getFromNamespace("eda_frequency_companion_names", "episcout")())
  expect_identical(names(compact)[1:4], c("level", "count", "display_order", "remainder"))
  expect_identical(compact$count, compact$numerator)
  expect_identical(compact$denominator, rep(5L, nrow(compact)))
  expect_equal(compact$proportion, compact$numerator / 5)
  expect_identical(compact$remainder, c(FALSE, FALSE, TRUE))
  expect_identical(compact$count, c(2L, 1L, 2L))
})
