context("longitudinal PostgreSQL state-transition contract")

transition_fake_source <- function(con) {
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

transition_spec <- function() {
  data.frame(
    name = c("entity", "state", "open_state", "measure"),
    label = c("Entity", "State", "Open state", "Measure"),
    database_type = c("text", "text", "text", "numeric"),
    analysis_type = c("text", "categorical", "categorical", "numeric"),
    role = c("identifier", "measure", "measure", "measure"),
    levels = c("", "B;A;B", "", ""),
    stringsAsFactors = FALSE
  )
}

test_that("transition inputs require explicit ordered non-identifier variables", {
  validate <- getFromNamespace("lt_inputs", "episcout")
  con <- new.env(parent = emptyenv())
  sources <- list(
    follow_up = transition_fake_source(con),
    baseline = transition_fake_source(con)
  )

  observed <- validate(
    sources, "entity", transition_spec(), c("open_state", "state"), 3L
  )
  expect_identical(observed$period_labels, c("follow_up", "baseline"))
  expect_identical(observed$variables, c("open_state", "state"))
  expect_identical(observed$selected$name, c("open_state", "state"))
  expect_identical(observed$max_levels, 3L)

  expect_error(validate(sources, "entity", transition_spec(), NULL, 3L), "unique")
  expect_error(
    validate(sources, "entity", transition_spec(), character(), 3L), "unique"
  )
  expect_error(
    validate(
      sources, "entity", transition_spec(), c("state", "state"), 3L
    ),
    "unique"
  )
  expect_error(
    validate(sources, "entity", transition_spec(), "outside", 3L), "outside"
  )
  expect_error(
    validate(sources, "entity", transition_spec(), "entity", 3L),
    "identifier"
  )
  expect_error(
    epi_eda_longitudinal_transitions(
      sources, "entity", transition_spec(), "entity", 3L
    ),
    "identifier"
  )
  role_spec <- transition_spec()
  role_spec$role[role_spec$name == "state"] <- " Identifier "
  expect_error(
    validate(sources, "entity", role_spec, "state", 3L), "identifier"
  )
})

test_that("transition level bounds protect the complete square", {
  validate <- getFromNamespace("lt_inputs", "episcout")
  validate_max <- getFromNamespace("lt_max_levels", "episcout")
  declared <- getFromNamespace("lt_declared_levels", "episcout")
  con <- new.env(parent = emptyenv())
  sources <- list(
    one = transition_fake_source(con),
    two = transition_fake_source(con)
  )

  expect_identical(
    declared(transition_spec()[2L, , drop = FALSE]), c("B", "A")
  )
  expect_identical(validate_max(1), 1L)
  expect_identical(validate_max(50), 50L)
  expect_error(validate_max(0), "positive whole number")
  expect_error(validate_max(1.5), "positive whole number")
  expect_error(validate_max(51), "no greater than 50")
  expect_error(
    validate(sources, "entity", transition_spec(), "state", 1L),
    "declared transition domain"
  )
  fifty_one <- transition_spec()
  fifty_one$levels[fifty_one$name == "state"] <- paste0(
    "level-", seq_len(51L), collapse = ";"
  )
  expect_error(
    validate(sources, "entity", fifty_one, "state", 50L),
    "declared transition domain"
  )
})

test_that("adjacent domain preflight is database-side and bounded", {
  preflight <- getFromNamespace(
    "lt_adjacent_domain_preflights", "episcout"
  )
  con <- new.env(parent = emptyenv())
  context <- list(
    sources = list(
      one = transition_fake_source(con),
      two = transition_fake_source(con)
    ),
    selected = transition_spec()[3L, , drop = FALSE],
    cells = list(
      list(unavailable = NULL),
      list(unavailable = NULL)
    ),
    variables = "open_state",
    max_levels = 50L
  )
  calls <- list()
  observed <- with_mocked_bindings(
    preflight(context),
    lt_state_ctes = function(context, cell, prefix, offset = 0L) {
      list(
        sql = paste0(prefix, "_cte AS (SELECT 1)"),
        params = list(),
        states = paste0(prefix, "_states")
      )
    },
    lt_declared_domain_cte = function(...) {
      "declared_states(state) AS (SELECT NULL::text WHERE FALSE)"
    },
    eda_db_fetch = function(con, query, params, query_kind, limit,
                            variable_index, name) {
      calls[[length(calls) + 1L]] <<- list(
        query = query, query_kind = query_kind, limit = limit,
        variable_index = variable_index, name = name
      )
      data.frame(state = c("A", "B"), stringsAsFactors = FALSE)
    },
    .package = "episcout"
  )
  expect_null(observed)
  expect_length(calls, 1L)
  expect_identical(
    calls[[1L]]$query_kind, "transition_adjacent_domain_preflight"
  )
  expect_identical(calls[[1L]]$limit, 51L)
  expect_match(calls[[1L]]$query, "LIMIT 51", fixed = TRUE)
  expect_match(calls[[1L]]$query, "UNION", fixed = TRUE)
})

test_that("typed transition component schemas remain stable when empty", {
  empty_functions <- c(
    "lt_empty_state_audit", "lt_empty_transition_summary",
    "lt_empty_transition_counts", "lt_empty_issues"
  )
  observed <- lapply(empty_functions, function(name) {
    getFromNamespace(name, "episcout")()
  })
  expect_true(all(vapply(observed, is.data.frame, logical(1))))
  expect_true(all(vapply(observed, nrow, integer(1)) == 0L))
  expect_identical(names(observed[[1L]]), c(
    "period_index", "period", "variable_index", "variable",
    "n_valid_entities", "n_usable_state", "n_missing_state",
    "n_conflicting_state", "status", "reason"
  ))
  expect_identical(names(observed[[2L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "n_retained", "n_eligible",
    "n_excluded_missing", "n_excluded_conflict", "n_transition_cells",
    "eligible_denominator", "status", "reason"
  ))
  expect_identical(names(observed[[3L]]), c(
    "left_period_index", "left_period", "right_period_index", "right_period",
    "variable_index", "variable", "from_state", "to_state", "n",
    "eligible_denominator", "proportion", "from_is_declared",
    "from_is_unexpected", "to_is_declared", "to_is_unexpected", "status",
    "reason"
  ))
  expect_identical(names(observed[[4L]]), c(
    "issue_code", "severity", "period_index", "period", "left_period_index",
    "left_period", "right_period_index", "right_period", "variable_index",
    "variable", "n_affected", "message"
  ))
  expect_type(observed[[1L]]$period_index, "integer")
  expect_type(observed[[1L]]$n_valid_entities, "double")
  expect_type(observed[[2L]]$n_transition_cells, "integer")
  expect_type(observed[[3L]]$n, "double")
  expect_type(observed[[4L]]$n_affected, "double")
})

test_that("transition metadata retains the frozen one-row contract", {
  metadata <- getFromNamespace("lt_metadata", "episcout")
  con <- new.env(parent = emptyenv())
  source <- transition_fake_source(con)
  context <- list(
    sources = list(one = source, two = source),
    period_labels = c("one", "two"),
    entity_id = "entity",
    spec = transition_spec(),
    selected = transition_spec()[c(3L, 2L), , drop = FALSE],
    variables = c("open_state", "state"),
    max_levels = 3L
  )
  observed <- metadata(context)
  expect_identical(names(observed), c(
    "contract_version", "n_periods", "n_spec_variables", "n_variables",
    "period_labels", "source_fingerprints", "source_set_fingerprint_sha256",
    "specification_fingerprint_sha256",
    "selected_specification_fingerprint_sha256", "resolved_variables",
    "entity_id", "max_levels", "count_contract", "count_maximum",
    "snapshot_mode"
  ))
  expect_equal(nrow(observed), 1L)
  expect_identical(observed$contract_version, "longitudinal-transitions-1")
  expect_identical(observed$n_periods, 2L)
  expect_identical(observed$n_spec_variables, 4L)
  expect_identical(observed$n_variables, 2L)
  expect_identical(observed$period_labels[[1L]], c("one", "two"))
  expect_identical(
    observed$resolved_variables[[1L]], c("open_state", "state")
  )
  expect_identical(observed$count_contract, "exact-base-r-double")
  expect_identical(observed$count_maximum, 9007199254740991)
  expect_type(observed$n_periods, "integer")
  expect_type(observed$count_maximum, "double")
})

test_that("complete transition matrices retain order, zeros and denominators", {
  complete <- getFromNamespace("lt_complete_transition_counts", "episcout")
  selected <- transition_spec()[2L, , drop = FALSE]
  context <- list(
    period_labels = c("baseline", "follow_up"),
    variables = "state",
    selected = selected
  )
  pair <- list(
    from_state = c("A", "B"),
    to_state = c("B", "B"),
    n = c(1, 1),
    n_eligible = 2
  )
  observed <- complete(
    context, 1L, 2L, 1L, c("B", "A", "C"), pair,
    "available", NA_character_
  )
  expect_identical(observed$from_state, rep(c("B", "A", "C"), each = 3L))
  expect_identical(observed$to_state, rep(c("B", "A", "C"), times = 3L))
  expect_identical(observed$n, c(1, 0, 0, 1, 0, 0, 0, 0, 0))
  expect_equal(sum(observed$proportion), 1)
  expect_identical(observed$eligible_denominator, rep(2, 9L))
  expect_identical(observed$from_is_declared, c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE
  ))
  expect_identical(observed$from_is_unexpected, !observed$from_is_declared)
  expect_identical(observed$to_is_unexpected, !observed$to_is_declared)

  zero <- pair
  zero$from_state <- zero$to_state <- character()
  zero$n <- numeric()
  zero$n_eligible <- 0
  unavailable <- complete(
    context, 1L, 2L, 1L, c("B", "A"), zero,
    "unavailable", "zero_eligible"
  )
  expect_identical(unavailable$n, rep(0, 4L))
  expect_true(all(is.na(unavailable$proportion)))
  expect_identical(unavailable$reason, rep("zero_eligible", 4L))

  empty <- complete(
    context, 1L, 2L, 1L, character(), zero,
    "unavailable", "zero_eligible"
  )
  expect_identical(empty, getFromNamespace(
    "lt_empty_transition_counts", "episcout"
  )())
})

test_that("transition issues preserve category precedence and typed scope", {
  issues <- getFromNamespace("lt_issues", "episcout")
  unavailable <- getFromNamespace("ld_unavailable", "episcout")
  state_audit <- data.frame(
    period_index = c(1L, 1L, 2L),
    period = c("one", "one", "two"),
    variable_index = c(1L, 2L, 1L),
    variable = c("state", "measure", "state"),
    n_valid_entities = c(5, 5, 4),
    n_usable_state = c(3, NA, 4),
    n_missing_state = c(1, NA, 0),
    n_conflicting_state = c(1, NA, 0),
    status = c("available", "unavailable", "available"),
    reason = c(NA, "unsupported_analysis_type", NA),
    stringsAsFactors = FALSE
  )
  transition_summary <- data.frame(
    left_period_index = 1L, left_period = "one",
    right_period_index = 2L, right_period = "two",
    variable_index = 1L, variable = "state", n_retained = 2,
    reason = "zero_eligible", stringsAsFactors = FALSE
  )
  context <- list(
    selected = data.frame(name = c("state", "measure")),
    cells = list(
      list(unavailable = NULL),
      list(unavailable = unavailable(
        "unsupported_analysis_type", "Only categorical states are supported."
      )),
      list(unavailable = NULL),
      list(unavailable = NULL)
    )
  )
  observed <- issues(state_audit, transition_summary, context)
  expect_identical(
    observed$issue_code,
    c("unsupported_analysis_type", "conflicting_state", "zero_eligible")
  )
  expect_identical(observed$severity, rep("warning", 3L))
  expect_identical(observed$n_affected, c(5, 1, 2))
  expect_identical(observed$period_index, c(1L, 1L, NA_integer_))
  expect_identical(observed$left_period_index, c(
    NA_integer_, NA_integer_, 1L
  ))

  period_issue <- getFromNamespace("lt_period_issue", "episcout")
  absent_row <- state_audit[2L, , drop = FALSE]
  absent_row$variable_index <- 1L
  absent_row$reason <- "absent_variable"
  absent_context <- list(
    selected = data.frame(name = "measure"),
    cells = list(list(unavailable = unavailable(
      "absent_variable", "The reviewed variable is absent in this period."
    )))
  )
  absent <- period_issue(absent_row, absent_context)
  expect_identical(absent$issue_code, "absent_variable")
  expect_identical(absent$severity, "error")
  expect_identical(absent$n_affected, 5)
})
