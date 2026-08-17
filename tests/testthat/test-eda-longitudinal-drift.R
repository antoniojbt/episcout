context("longitudinal PostgreSQL variable drift contract")

drift_fake_source <- function(con) {
  structure(
    list(
      con = con,
      schema = "reviewed_schema",
      relation = "reviewed_period",
      relation_kind = "table",
      columns = data.frame(name = character(), stringsAsFactors = FALSE),
      source_version = "postgres-source-1"
    ),
    class = c("epi_eda_postgres_source", "list")
  )
}

drift_spec <- function() {
  data.frame(
    name = c("measure", "group", "note"),
    label = c("Measure", "Group", "Note"),
    database_type = c("numeric", "text", "text"),
    analysis_type = c("numeric", "categorical", "text"),
    role = c("measure", "exposure", "comment"),
    levels = c("", "A;B", ""),
    stringsAsFactors = FALSE
  )
}

test_that("drift inputs preserve explicit order and enforce hard bounds", {
  validate <- getFromNamespace("ld_inputs", "episcout")
  con <- new.env(parent = emptyenv())
  sources <- list(
    later = drift_fake_source(con),
    earlier = drift_fake_source(con)
  )

  observed <- validate(sources, drift_spec(), c("note", "measure"), 3L)
  expect_identical(observed$period_labels, c("later", "earlier"))
  expect_identical(observed$variables, c("note", "measure"))
  expect_identical(observed$selected$name, c("note", "measure"))
  expect_identical(observed$max_levels, 3L)

  all_variables <- validate(sources, drift_spec(), NULL, 3L)
  expect_identical(all_variables$variables, drift_spec()$name)
  expect_error(validate(sources, drift_spec(), character(), 3L), "unique")
  expect_error(
    validate(sources, drift_spec(), c("measure", "measure"), 3L),
    "unique"
  )
  expect_error(validate(sources, drift_spec(), "outside", 3L), "outside")
  expect_error(validate(sources, drift_spec(), NULL, 1L), "declared")
  expect_error(validate(sources, drift_spec(), NULL, 0L), "positive")
  expect_error(validate(sources, drift_spec(), NULL, 1.5), "positive")
  expect_error(validate(sources, "spec.csv", NULL, 3L), "data frame")
})

test_that("typed drift component schemas remain stable when empty", {
  empty_functions <- c(
    "ld_empty_schema", "ld_empty_missingness",
    "ld_empty_missingness_adjacent", "ld_empty_numeric",
    "ld_empty_numeric_adjacent", "ld_empty_categorical",
    "ld_empty_categorical_adjacent", "ld_empty_temporal",
    "ld_empty_temporal_adjacent", "ld_empty_skipped"
  )
  observed <- lapply(empty_functions, function(name) {
    getFromNamespace(name, "episcout")()
  })
  expect_true(all(vapply(observed, is.data.frame, logical(1))))
  expect_true(all(vapply(observed, nrow, integer(1)) == 0L))
  expect_type(observed[[2L]]$n, "integer")
  expect_type(observed[[2L]]$p_missing, "double")
  expect_type(observed[[4L]]$n_finite, "integer")
  expect_type(observed[[6L]]$p_observed, "double")
  expect_type(observed[[8L]]$min, "character")
  expect_type(observed[[10L]]$code, "character")
  expect_identical(names(observed[[1L]]), c(
    "period_index", "period", "variable_index", "variable", "analysis_type",
    "expected_database_type", "observed_type", "observed_present",
    "type_status", "type_reason"
  ))
  expect_identical(names(observed[[2L]]), c(
    "period_index", "period", "variable_index", "variable", "n",
    "n_missing", "n_observed", "p_missing", "status", "reason"
  ))
  expect_identical(names(observed[[3L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "n_left", "n_missing_left",
    "n_observed_left", "p_missing_left", "n_right", "n_missing_right",
    "n_observed_right", "p_missing_right", "absolute_change",
    "relative_change", "relative_denominator", "status", "reason"
  ))
  expect_identical(names(observed[[4L]]), c(
    "period_index", "period", "variable_index", "variable", "n",
    "n_missing", "n_observed", "n_infinite", "n_finite", "min", "q1",
    "mean", "median", "q3", "max", "iqr", "sd", "status", "reason"
  ))
  numeric_metrics <- c("min", "q1", "mean", "median", "q3", "max", "iqr", "sd")
  numeric_adjacent_fields <- unlist(lapply(numeric_metrics, function(metric) {
    c(paste0("left_", metric), paste0("right_", metric), paste0(metric, "_change"))
  }))
  expect_identical(names(observed[[5L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "left_n_finite", "right_n_finite",
    numeric_adjacent_fields, "status", "reason"
  ))
  expect_identical(names(observed[[6L]]), c(
    "period_index", "period", "variable_index", "variable", "level", "n",
    "p_total", "p_observed", "is_declared", "is_unexpected", "status", "reason"
  ))
  expect_identical(names(observed[[7L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "level", "left_n", "right_n",
    "left_total_denominator", "right_total_denominator", "left_p_total",
    "right_p_total", "p_total_difference", "left_observed_denominator",
    "right_observed_denominator", "left_p_observed", "right_p_observed",
    "p_observed_difference", "is_declared", "is_unexpected", "level_status",
    "status", "reason"
  ))
  expect_identical(names(observed[[8L]]), c(
    "period_index", "period", "variable_index", "variable", "n",
    "n_missing", "n_observed", "min", "max", "range_value", "unit",
    "status", "reason"
  ))
  expect_identical(names(observed[[9L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "left_min", "right_min", "min_shift",
    "left_max", "right_max", "max_shift", "left_range_value",
    "right_range_value", "range_change", "unit", "status", "reason"
  ))
  expect_identical(names(observed[[10L]]), c(
    "period_index", "period", "variable_index", "variable", "component",
    "code", "message"
  ))
})

test_that("missingness changes retain explicit relative denominators", {
  adjacent <- getFromNamespace("ld_missing_adjacent", "episcout")
  context <- list(
    sources = list(list(), list()),
    period_labels = c("baseline", "follow_up"),
    variables = "measure",
    selected = data.frame(analysis_type = "numeric")
  )
  missingness <- data.frame(
    period_index = 1:2,
    period = context$period_labels,
    variable_index = c(1L, 1L),
    variable = "measure",
    analysis_type = "numeric",
    n = c(4L, 5L),
    n_missing = c(1L, 0L),
    n_observed = c(3L, 5L),
    p_missing = c(0.25, 0),
    status = "available",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
  observed <- adjacent(missingness, context)
  expect_identical(observed$absolute_change, -0.25)
  expect_identical(observed$absolute_change, -0.25)
  expect_identical(observed$relative_denominator, 0.25)
  expect_identical(observed$relative_change, -1)
  expect_identical(observed$status, "available")

  zero_left <- missingness
  zero_left$p_missing[[1L]] <- 0
  zero <- adjacent(zero_left, context)
  expect_true(is.na(zero$relative_change))
  expect_true(is.na(zero$relative_denominator))
  expect_identical(zero$status, "available")
  expect_true(is.na(zero$reason))
})

test_that("numeric adjacent changes are signed right minus left", {
  adjacent <- getFromNamespace("ld_numeric_adjacent", "episcout")
  numeric_row <- function(value) {
    data.frame(
      n_finite = 3L,
      min = value, q1 = value + 1, mean = value + 2,
      median = value + 2, q3 = value + 3, max = value + 4,
      iqr = 2, sd = value / 2,
      stringsAsFactors = FALSE
    )
  }
  result <- list(
    cell_data = list(numeric_row(1), numeric_row(3)),
    states = list(
      list(status = "available", unavailable = NULL),
      list(status = "available", unavailable = NULL)
    )
  )
  context <- list(
    sources = list(list(), list()),
    period_labels = c("one", "two"),
    variables = "measure",
    selected = data.frame(analysis_type = "numeric")
  )
  observed <- adjacent(result, context)
  expect_identical(observed$min_change, 2)
  expect_identical(observed$mean_change, 2)
  expect_identical(observed$iqr_change, 0)
  expect_identical(observed$sd_change, 1)
})

test_that("temporal adjacent shifts retain canonical endpoint units", {
  adjacent <- getFromNamespace("ld_temporal_adjacent", "episcout")
  temporal_data <- function(minimum, maximum, range) {
    list(
      row = data.frame(
        min = "2020-01-01", max = "2020-01-05", stringsAsFactors = FALSE
      ),
      values = c(
        min = minimum, max = maximum,
        range_value = range
      )
    )
  }
  result <- list(
    cell_data = list(temporal_data(0, 4, 4), temporal_data(1.75, 9.75, 8)),
    states = list(
      list(status = "available", unavailable = NULL),
      list(status = "available", unavailable = NULL)
    )
  )
  context <- list(
    sources = list(list(), list()),
    period_labels = c("one", "two"),
    variables = "event_date",
    selected = data.frame(analysis_type = "date")
  )
  observed <- adjacent(result, context)
  expect_identical(observed$min_shift, 1.75)
  expect_identical(observed$max_shift, 5.75)
  expect_identical(observed$range_change, 4)
  expect_identical(observed$unit, "days")
})

test_that("PostgreSQL temporal summaries retain raw quantiles internally", {
  summarise <- getFromNamespace("eda_postgres_temporal_summary", "episcout")
  aggregate <- data.frame(
    n = "4", n_missing = "1", n_observed = "3", n_unique = "3",
    min = 0, q1 = 0.5, median = 1, q3 = 1.5, max = 2,
    stringsAsFactors = FALSE
  )
  observed <- with_mocked_bindings(
    summarise(
      list(con = NULL),
      data.frame(name = "event_date"),
      list(sql = "missing", params = list()),
      "date",
      1L,
      NULL
    ),
    eda_postgres_value_expression = function(...) "value",
    eda_postgres_table_sql = function(...) "reviewed_period",
    eda_db_fetch = function(...) aggregate,
    .package = "episcout"
  )
  expect_identical(observed$data$q1, "1970-01-01")
  expect_identical(observed$numeric_values[["q1"]], 0.5)
  expect_identical(observed$numeric_values[["range_value"]], 2)
})

test_that("categorical level states follow positive counts", {
  classify <- getFromNamespace("ld_level_status", "episcout")
  expect_identical(classify(1L, 2L), "present_both")
  expect_identical(classify(2L, 0L), "removed")
  expect_identical(classify(0L, 2L), "introduced")
  expect_identical(classify(0L, 0L), "absent_both")
  expect_true(is.na(classify(NA_integer_, 1L)))

  order_domain <- getFromNamespace("ld_ordered_domain", "episcout")
  expect_identical(
    order_domain(c("B", "A"), c("C", "A", "D")),
    c("B", "A", "C", "D")
  )
})

test_that("numeric drift treats a zero finite denominator as unavailable", {
  summarise <- getFromNamespace("ld_distribution_one", "episcout")
  cell <- list(
    period_index = 1L,
    variable_index = 1L,
    name = "measure",
    type = "numeric",
    unavailable = NULL
  )
  context <- list(period_labels = "period", variables = "measure")
  observed <- with_mocked_bindings(
    summarise(
      cell,
      context,
      2L,
      list(n_missing = 0L, n_observed = 2L),
      NULL
    ),
    ld_numeric_one = function(...) {
      list(
        row = data.frame(value = NA_real_),
        counts = list(
          n_missing = 0L, n_observed = 2L, n_infinite = 2L, n_finite = 0L
        ),
        data = data.frame(value = NA_real_)
      )
    },
    .package = "episcout"
  )
  expect_identical(observed$state$status, "unavailable")
  expect_identical(observed$state$unavailable$code, "zero_denominator")
  expect_identical(observed$row$reason, "zero_denominator")
})

test_that("all-missing undeclared categories keep a typed zero-row table", {
  summarise <- getFromNamespace("ld_distribution_one", "episcout")
  cell <- list(
    period_index = 1L,
    variable_index = 1L,
    name = "group",
    type = "categorical",
    levels = character(),
    unavailable = NULL
  )
  context <- list(period_labels = "period", variables = "group")
  cache <- list(
    data = data.frame(
      level = character(), n = integer(), p_total = numeric(),
      p_observed = numeric(), is_declared = logical(),
      is_unexpected = logical(), stringsAsFactors = FALSE
    ),
    counts = list(n_missing = 2L, n_observed = 0L, n_unique = 0L)
  )
  observed <- summarise(
    cell,
    context,
    2L,
    list(n_missing = 2L, n_observed = 0L),
    cache
  )
  expect_identical(nrow(observed$row), 0L)
  expect_identical(
    names(observed$row),
    names(getFromNamespace("ld_empty_categorical", "episcout")())
  )
  expect_identical(observed$state$unavailable$code, "zero_denominator")
  expect_identical(observed$skipped$code, "zero_denominator")
  values <- getFromNamespace("ld_level_values", "episcout")(
    observed$data, "A"
  )
  expect_identical(values$n, 0L)
  expect_identical(values$n_total, 2L)
  expect_identical(values$n_observed, 0L)
  expect_identical(values$p_total, 0)
  expect_true(is.na(values$p_observed))
})

test_that("shared transaction keeps population QC lifecycle messages", {
  testthat::skip_if_not_installed("RSQLite")
  transaction <- getFromNamespace("longitudinal_qc_transaction", "episcout")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- structure(
    list(con = con), class = c("epi_eda_postgres_source", "list")
  )
  observed <- tryCatch(
    with_mocked_bindings(
      transaction(list(source, source), 1L),
      eda_validate_postgres_source = function(...) invisible(TRUE),
      eda_db_begin = function(...) stop("PRIVATE_BEGIN_VALUE"),
      .package = "episcout"
    ),
    error = identity
  )
  expect_match(conditionMessage(observed), "longitudinal QC transaction")
  expect_false(grepl("PRIVATE_BEGIN_VALUE", conditionMessage(observed)))
})
