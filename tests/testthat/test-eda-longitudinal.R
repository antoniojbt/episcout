context("thin longitudinal EDA contract")

longitudinal_eda_fixture <- function() {
  spec <- data.frame(
    name = c("entity", "visit", "score", "state", "private_note"),
    label = c("Entity", "Visit", "Score", "State", "Private note"),
    database_type = c("text", "text", "numeric", "text", "text"),
    analysis_type = c("text", "categorical", "numeric", "categorical", "text"),
    role = c("identifier", "time", "measure", "measure", "identifier"),
    levels = c("", "baseline;month_1;month_2", "", "A;B", ""),
    missing_codes = c("", "NOT_DONE", "-99", "MISSING", ""),
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    entity = c("a", "a", "a", "b", "b", "b", "c", NA, "   "),
    visit = c(
      "baseline", "month_1", "month_1", "baseline", "month_1", "month_2",
      "baseline", "baseline", "NOT_DONE"
    ),
    score = c(1, 3, 4, 2, -99, Inf, 5, 99, 88),
    state = c("A", "B", "B", "A", "MISSING", "B", "A", "B", "A"),
    private_note = paste0("private-", seq_len(9L)),
    stringsAsFactors = FALSE
  )
  list(spec = spec, data = data)
}

test_that("longitudinal EDA returns fixed components and canonical summaries", {
  fixture <- longitudinal_eda_fixture()
  original <- fixture$data
  observed <- epi_eda_longitudinal(
    fixture$data, fixture$spec, "entity", "visit",
    variables = c("state", "score")
  )
  summary_spec <- fixture$spec[match(
    c("visit", "state", "score"), fixture$spec$name
  ), , drop = FALSE]
  expected_summaries <- epi_eda_profile_stratified(
    fixture$data, summary_spec, "visit"
  )

  expect_s3_class(observed, "epi_eda_longitudinal")
  expect_named(observed, c(
    "metadata", "structure", "followup", "timepoints", "missingness",
    "summaries", "change", "issues"
  ))
  expect_identical(observed$summaries, expected_summaries)
  expect_identical(observed$metadata$resolved_variables[[1L]], c("state", "score"))
  expect_identical(fixture$data, original)
  expect_false(any(grepl("private-", capture.output(str(observed)), fixed = TRUE)))
})

test_that("panel counts, state precedence and signed change reconcile by hand", {
  fixture <- longitudinal_eda_fixture()
  observed <- epi_eda_longitudinal(
    fixture$data, fixture$spec, "entity", "visit",
    variables = c("score", "state")
  )

  expect_identical(observed$structure$n_valid_panel_rows, 7)
  expect_identical(observed$structure$n_valid_entities, 3)
  expect_identical(observed$structure$n_observed_id_time_cells, 6)
  expect_identical(observed$structure$n_duplicate_cells, 1)
  expect_identical(observed$structure$n_duplicate_excess, 1)
  expect_identical(observed$structure$n_expected_cells, 9)
  expect_identical(observed$followup$observation_count$n_entities, c(1, 1, 1))

  score_month_1 <- observed$missingness$by_time[
    observed$missingness$by_time$variable == "score" &
      observed$missingness$by_time$timepoint == "month_1", , drop = FALSE
  ]
  expect_identical(score_month_1$n_present_entities, 2)
  expect_identical(score_month_1$n_usable, 0)
  expect_identical(score_month_1$n_missing, 1)
  expect_identical(score_month_1$n_conflicting, 1)

  adjacent <- observed$change$adjacent[observed$change$adjacent$from_timepoint == "baseline", , drop = FALSE]
  expect_identical(adjacent$n_present_both, 2)
  expect_identical(adjacent$n_excluded_missing, 1)
  expect_identical(adjacent$n_excluded_conflict, 1)
  expect_identical(adjacent$n_eligible, 0)
  expect_identical(adjacent$status, "available")
  expect_identical(adjacent$reason, "zero_eligible")
  first_last <- observed$change$first_to_last
  expect_identical(first_last$n_excluded_single_timepoint, 1)
  expect_identical(first_last$n_excluded_conflict, 1)
  expect_identical(first_last$n_excluded_nonfinite, 1)
  expect_identical(first_last$n_eligible, 0)
  expect_identical(
    observed$issues$issue_code,
    c(
      "missing_entity_id", "blank_entity_id", "missing_time",
      "duplicate_id_time", "conflicting_variable_cell"
    )
  )
})

test_that("reviewed time order and selections fail before construction", {
  fixture <- longitudinal_eda_fixture()
  expect_error(
    epi_eda_longitudinal(
      fixture$data, fixture$spec, "entity", "visit",
      time_order = c("month_1", "baseline", "month_2")
    ),
    "exactly match"
  )
  expect_error(
    epi_eda_longitudinal(
      fixture$data, fixture$spec, "entity", "visit", variables = "entity"
    ),
    "identifier-role"
  )
  expect_error(
    epi_eda_longitudinal(
      fixture$data, fixture$spec, "entity", "visit", variables = "absent"
    ),
    "outside"
  )
  unexpected <- fixture$data
  unexpected$visit[[1L]] <- "unreviewed"
  expect_error(
    epi_eda_longitudinal(unexpected, fixture$spec, "entity", "visit"),
    "reviewed time levels"
  )
  no_levels <- fixture$spec
  no_levels$levels[no_levels$name == "visit"] <- ""
  expect_error(
    epi_eda_longitudinal(fixture$data, no_levels, "entity", "visit"),
    "reviewed declared levels"
  )
})

test_that("identifier representations and zero rows retain typed contracts", {
  fixture <- longitudinal_eda_fixture()
  numeric_id <- fixture$data
  numeric_id$entity <- c(1, 1, 1, 2, 2, 2, 3, NA, 4)
  expect_s3_class(
    epi_eda_longitudinal(numeric_id, fixture$spec, "entity", "visit"),
    "epi_eda_longitudinal"
  )
  numeric_id$entity[[1L]] <- 1.5
  expect_error(
    epi_eda_longitudinal(numeric_id, fixture$spec, "entity", "visit"),
    "finite exact integers"
  )

  empty <- fixture$data[0, , drop = FALSE]
  observed <- epi_eda_longitudinal(empty, fixture$spec, "entity", "visit")
  expect_identical(observed$timepoints$timepoint, c("baseline", "month_1", "month_2"))
  expect_identical(observed$timepoints$n_entities, c(0, 0, 0))
  expect_identical(observed$followup$observation_count$n_entities, c(0, 0, 0))
  expect_true(all(is.na(observed$timepoints$p_retained)))
  expect_true(is.list(observed$followup) && is.list(observed$missingness) && is.list(observed$change))
  expect_identical(
    observed$issues$issue_code,
    rep("zero_observation_timepoint", 3L)
  )
  expect_identical(observed$issues$n_affected, c(0, 0, 0))
})

test_that("empty custom schemas retain stable column types", {
  missingness <- getFromNamespace("le_empty_missingness", "episcout")()
  change <- getFromNamespace("le_empty_change", "episcout")()
  issues <- getFromNamespace("le_empty_issues", "episcout")()
  expect_identical(names(missingness), c("variable_index", "variable", "time_index", "timepoint", "n_present_entities", "n_usable", "n_missing", "n_conflicting", "usable_numerator", "usable_denominator", "p_usable", "missing_numerator", "missing_denominator", "p_missing", "conflicting_numerator", "conflicting_denominator", "p_conflicting"))
  expect_identical(names(change), c("variable_index", "variable", "n_entities_with_presence", "n_excluded_single_timepoint", "n_present_both", "n_eligible", "n_excluded_missing", "n_excluded_conflict", "n_excluded_nonfinite", "delta_n", "mean", "sd", "min", "q1", "median", "q3", "max", "iqr", "status", "reason"))
  expect_identical(
    names(getFromNamespace("le_empty_adjacent_change", "episcout")()),
    c("from_time_index", "from_timepoint", "to_time_index", "to_timepoint", "variable_index", "variable", "n_present_both", "n_eligible", "n_excluded_missing", "n_excluded_conflict", "n_excluded_nonfinite", "delta_n", "mean", "sd", "min", "q1", "median", "q3", "max", "iqr", "status", "reason")
  )
  expect_identical(names(issues), c(
    "issue_code", "severity", "time_index", "timepoint", "variable_index",
    "variable", "n_affected", "message"
  ))
  expect_type(missingness$n_present_entities, "double")
  expect_type(change$delta_n, "double")
  expect_type(issues$n_affected, "double")
})
