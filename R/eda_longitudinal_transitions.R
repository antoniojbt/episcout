#' Summarise categorical state transitions across PostgreSQL periods
#'
#' Summarise the same caller-selected categorical or binary variables for
#' distinct valid entities retained across adjacent completed periods. Entity
#' reconciliation and transition aggregation remain inside PostgreSQL; only
#' bounded state labels and aggregate counts are returned.
#'
#' @param sources A uniquely named list of at least two unmodified
#'   [epi_eda_postgres_source()] objects sharing one caller-owned connection.
#'   List order defines period order.
#' @param entity_id One column name present in every source. Supported common
#'   PostgreSQL families are text/varchar, integral, and UUID.
#' @param spec A validated EDA specification data frame as returned by
#'   [epi_eda_spec()].
#' @param variables A unique, non-empty character vector of specification
#'   variable names. Caller order is retained. Entity identifiers and variables
#'   whose specification role is `id` or `identifier` are prohibited.
#' @param max_levels Positive whole-number hard bound for each declared domain,
#'   each usable period domain, and each adjacent union. V1 accepts at most 50
#'   states, so a complete transition matrix has at most 2,500 cells.
#'
#' @return An `epi_eda_longitudinal_transitions` list with fixed components
#'   `metadata`, `state_audit`, `transition_summary`, `transition_counts`, and
#'   `issues`.
#'
#' @details Null entity identifiers and blank text representations are excluded
#' from period membership. Repeated entity rows do not multiply membership.
#' Within each entity-period, zero distinct non-missing canonical values is a
#' missing state, exactly one is usable, and more than one is conflicting.
#' Repeated identical values and one value mixed with missing rows remain
#' usable. Conflict takes precedence over missingness when adjacent retained
#' entities are excluded.
#'
#' Only adjacent periods are compared. `n_retained` describes the distinct
#' valid-entity intersection. `n_eligible` includes retained entities with one
#' usable state on both sides; it is also the transition-proportion denominator.
#' Missing and conflicting exclusions reconcile with the retained population.
#' A zero eligible denominator produces `NA` proportions and a `zero_eligible`
#' warning rather than treating proportions as zero.
#'
#' Each available pair returns the complete square domain: first-occurrence
#' deduplicated declared levels in specification order, followed by unexpected
#' usable states observed on either side in bytewise order. Zero-count cells are
#' retained. Missing values are never a state. Bound excess, count overflow,
#' query failure, or reconciliation failure aborts the whole call without a
#' partial result.
#'
#' All source validation and calculations occur in one `REPEATABLE READ READ
#' ONLY` transaction. The operation creates no database objects, collects no
#' row-level records or entity values, and leaves the caller-owned connection
#' open and idle. The result is generic descriptive evidence: it does not infer
#' scientific desirability, plausibility, entry or exit meaning, cross-variable
#' transitions, or project-specific derived concepts.
#'
#' `epi_eda_longitudinal_qc()` separately describes population continuity,
#' entry, and exit. `epi_eda_longitudinal_drift()` separately describes
#' period-level schema, missingness, and marginal distributions without joining
#' retained entities.
#'
#' @section Result schema:
#' `metadata` is one row recording contract version, period and variable counts,
#' ordered labels, source and specification fingerprints, selected variables,
#' `entity_id`, `max_levels`, the exact-base-R-double count contract and maximum,
#' and snapshot mode.
#'
#' `state_audit` has integer `period_index`, `period`, integer `variable_index`,
#' `variable`, double `n_valid_entities`, `n_usable_state`, `n_missing_state`, and
#' `n_conflicting_state`, plus `status` and machine-readable `reason`.
#'
#' `transition_summary` names the left and right periods and variable, then has
#' double `n_retained`, `n_eligible`, `n_excluded_missing`, and
#' `n_excluded_conflict`, integer `n_transition_cells`, double
#' `eligible_denominator`, and availability fields. `transition_counts` adds
#' `from_state`, `to_state`, double `n`, `eligible_denominator`, `proportion`,
#' declaration flags for each side, and availability fields.
#'
#' `issues` has `issue_code`, `severity`, period and adjacent-pair scope fields,
#' integer variable keys, double `n_affected`, and a value-free `message`.
#' Period unavailability issues precede period conflicts, which precede pair
#' zero-denominator warnings.
#'
#' @family EDA
#' @export
epi_eda_longitudinal_transitions <- function( # nolint: object_length_linter
                                             sources,
                                             entity_id,
                                             spec,
                                             variables,
                                             max_levels = 50L) {
  inputs <- lt_inputs(sources, entity_id, spec, variables, max_levels)
  eda_longitudinal_transaction( # nolint: object_usage_linter
    inputs$sources,
    {
      context <- lt_context(inputs)
      lt_adjacent_domain_preflights(context)
      profiles <- lt_period_profiles(context)
      transitions <- lt_transitions(context, profiles)
      issues <- lt_issues(
        profiles$state_audit, transitions$transition_summary, context
      )
      structure(
        list(
          metadata = lt_metadata(context),
          state_audit = profiles$state_audit,
          transition_summary = transitions$transition_summary,
          transition_counts = transitions$transition_counts,
          issues = issues
        ),
        class = c("epi_eda_longitudinal_transitions", "list")
      )
    },
    operation = "state-transition summary"
  )
}

#' @export
print.epi_eda_longitudinal_transitions <- function( # nolint: object_length_linter
                                                   x, ...) {
  cat("<epi_eda_longitudinal_transitions>\n")
  cat("  Periods: ", x$metadata$n_periods[[1L]], "\n", sep = "")
  cat("  Reviewed variables: ", x$metadata$n_variables[[1L]], "\n", sep = "")
  cat("  Adjacent summaries: ", nrow(x$transition_summary), "\n", sep = "")
  cat("  Technical findings: ", nrow(x$issues), "\n", sep = "")
  cat("  Entity values and row-level states: not returned\n")
  invisible(x)
}

lt_inputs <- function(sources,
                      entity_id,
                      spec,
                      variables,
                      max_levels) {
  source_inputs <- eda_longitudinal_source_inputs(sources) # nolint: object_usage_linter
  entity_id <- eda_postgres_identifier(entity_id, "entity_id")
  if (!is.data.frame(spec)) {
    stop("spec must be a validated EDA specification data frame.", call. = FALSE)
  }
  spec <- epi_eda_spec(spec)
  valid_variables <- is.character(variables) && length(variables) >= 1L &&
    !anyNA(variables) && !anyDuplicated(variables) &&
    all(nzchar(trimws(variables)))
  if (!valid_variables) {
    stop(
      "variables must be a unique, non-blank, non-empty character vector.",
      call. = FALSE
    )
  }
  outside <- setdiff(variables, spec$name)
  if (length(outside) > 0L) {
    stop("variables cannot select outside the EDA specification.", call. = FALSE)
  }
  selected <- spec[match(variables, spec$name), , drop = FALSE]
  rownames(selected) <- NULL
  private <- selected$name == entity_id |
    trimws(tolower(as.character(selected$role))) %in% c("id", "identifier")
  if (any(private)) {
    stop(
      "variables cannot include entity_id or a specification identifier role.",
      call. = FALSE
    )
  }
  max_levels <- lt_max_levels(max_levels)
  declared_counts <- vapply(seq_len(nrow(selected)), function(index) {
    length(lt_declared_levels(selected[index, , drop = FALSE]))
  }, integer(1))
  if (any(declared_counts > max_levels)) {
    stop("A declared transition domain exceeds max_levels.", call. = FALSE)
  }
  list(
    sources = source_inputs$sources,
    period_labels = source_inputs$period_labels,
    entity_id = entity_id,
    spec = spec,
    selected = selected,
    variables = unname(variables),
    max_levels = max_levels
  )
}

lt_max_levels <- function(max_levels) {
  valid <- is.numeric(max_levels) && length(max_levels) == 1L &&
    !is.na(max_levels) && is.finite(max_levels) && max_levels >= 1 &&
    max_levels == floor(max_levels) && max_levels <= 50
  if (!valid) {
    stop(
      "max_levels must be a positive whole number no greater than 50.",
      call. = FALSE
    )
  }
  as.integer(max_levels)
}

lt_declared_levels <- function(spec_row) {
  if (!(as.character(spec_row$analysis_type[[1L]]) %in%
          c("categorical", "binary")) || !("levels" %in% names(spec_row))) {
    return(character())
  }
  unique(eda_spec_levels(spec_row$levels[[1L]]))
}

lt_context <- function(inputs) {
  entity <- longitudinal_qc_context( # nolint: object_usage_linter
    inputs$sources, inputs$period_labels, inputs$entity_id, NULL
  )
  cells <- vector("list", length(inputs$sources) * nrow(inputs$selected))
  cell_index <- 0L
  for (period_index in seq_along(inputs$sources)) {
    source <- inputs$sources[[period_index]]
    for (variable_index in seq_len(nrow(inputs$selected))) {
      cell_index <- cell_index + 1L
      spec_row <- inputs$selected[variable_index, , drop = FALSE]
      name <- as.character(spec_row$name[[1L]])
      type <- as.character(spec_row$analysis_type[[1L]])
      levels <- lt_declared_levels(spec_row)
      column <- eda_postgres_column(source, name)
      compatibility <- eda_pg_type_compatibility(column, type, levels)
      codes <- eda_missing_codes(inputs$spec, name)
      contract <- if (is.null(column)) {
        NULL
      } else {
        eda_postgres_missing_contract(source, column, type, codes)
      }
      unavailable <- lt_cell_unavailable(
        column, type, compatibility, contract
      )
      cells[[cell_index]] <- list(
        period_index = as.integer(period_index),
        variable_index = as.integer(variable_index),
        source = source,
        spec_row = spec_row,
        name = name,
        type = type,
        levels = levels,
        column = column,
        missing_codes = codes,
        unavailable = unavailable
      )
    }
  }
  inputs$entity_family <- entity$entity_family
  inputs$entity_columns <- entity$entity_columns
  inputs$cells <- cells
  inputs
}

lt_cell_unavailable <- function(column,
                                type,
                                compatibility,
                                contract) {
  if (is.null(column)) {
    return(ld_unavailable( # nolint: object_usage_linter
      "absent_variable", "The reviewed variable is absent in this period."
    ))
  }
  if (!(type %in% c("categorical", "binary"))) {
    return(ld_unavailable( # nolint: object_usage_linter
      "unsupported_analysis_type",
      "Only declared categorical or binary variables have transition states."
    ))
  }
  if (identical(compatibility$status, "incompatible")) {
    return(ld_unavailable( # nolint: object_usage_linter
      "incompatible_type", compatibility$reason
    ))
  }
  if (!isTRUE(contract$valid)) {
    return(ld_unavailable( # nolint: object_usage_linter
      "invalid_missing_contract", contract$reason
    ))
  }
  NULL
}

lt_metadata <- function(context) {
  source_fingerprints <- unname(vapply(
    context$sources, eda_pg_source_fingerprint, character(1)
  ))
  specification_fingerprint <- eda_postgres_fingerprint(context$spec)
  selected_fingerprint <- eda_postgres_fingerprint(context$selected)
  source_contract <- list(
    contract_version = "longitudinal-transitions-1",
    period_labels = context$period_labels,
    source_fingerprints = source_fingerprints,
    specification_fingerprint = specification_fingerprint,
    selected_specification_fingerprint = selected_fingerprint,
    resolved_variables = context$variables,
    entity_id = context$entity_id,
    max_levels = context$max_levels,
    count_contract = "exact-base-r-double",
    snapshot_mode = "REPEATABLE READ READ ONLY"
  )
  data.frame(
    contract_version = "longitudinal-transitions-1",
    n_periods = as.integer(length(context$sources)),
    n_spec_variables = as.integer(nrow(context$spec)),
    n_variables = as.integer(nrow(context$selected)),
    period_labels = I(list(unname(context$period_labels))),
    source_fingerprints = I(list(source_fingerprints)),
    source_set_fingerprint_sha256 = eda_postgres_fingerprint(source_contract),
    specification_fingerprint_sha256 = specification_fingerprint,
    selected_specification_fingerprint_sha256 = selected_fingerprint,
    resolved_variables = I(list(unname(context$variables))),
    entity_id = context$entity_id,
    max_levels = as.integer(context$max_levels),
    count_contract = "exact-base-r-double",
    count_maximum = 9007199254740991,
    snapshot_mode = "REPEATABLE READ READ ONLY",
    stringsAsFactors = FALSE
  )
}

lt_cell_index <- function(period_index, variable_index, context) {
  as.integer(
    (period_index - 1L) * nrow(context$selected) + variable_index
  )
}

lt_population_counts <- function(context) {
  vapply(seq_along(context$sources), function(period_index) {
    source <- context$sources[[period_index]]
    query <- paste0(
      "SELECT COUNT(DISTINCT ",
      longitudinal_qc_entity_sql( # nolint: object_usage_linter
        source, context$entity_id
      ),
      ")::text AS n_valid_entities FROM ",
      eda_postgres_table_sql(source), " WHERE ",
      longitudinal_entity_predicate( # nolint: object_usage_linter
        source, context$entity_id, context$entity_family
      )
    )
    observed <- eda_db_fetch(
      source$con, query, query_kind = "transition_population_count", limit = 1L
    )
    if (nrow(observed) != 1L ||
          !identical(names(observed), "n_valid_entities")) {
      stop(
        "PostgreSQL transition population count was incomplete.",
        call. = FALSE
      )
    }
    longitudinal_qc_checked_count( # nolint: object_usage_linter
      observed$n_valid_entities[[1L]], "transition n_valid_entities"
    )
  }, numeric(1))
}

lt_contract <- function(cell, offset = 0L) {
  eda_postgres_missing_contract(
    cell$source, cell$column, cell$type, cell$missing_codes, offset = offset
  )
}

lt_state_ctes <- function(context, cell, prefix, offset = 0L) {
  source <- cell$source
  contract <- lt_contract(cell, offset)
  expression <- eda_postgres_value_expression(
    source, cell$column, cell$type
  )
  entity <- longitudinal_qc_entity_sql( # nolint: object_usage_linter
    source, context$entity_id
  )
  valid <- longitudinal_entity_predicate( # nolint: object_usage_linter
    source, context$entity_id, context$entity_family
  )
  classified <- paste0(prefix, "_classified")
  states <- paste0(prefix, "_states")
  sql <- paste0(
    classified, " AS (SELECT ", entity, " AS entity_value, ",
    contract$sql, " AS missing, (", expression,
    ")::text COLLATE \"C\" AS state FROM ",
    eda_postgres_table_sql(source), " WHERE ", valid, "), ",
    states, " AS (SELECT entity_value, ",
    "LEAST(COUNT(DISTINCT state) FILTER (WHERE NOT missing), 2)::integer ",
    "AS n_states, CASE WHEN COUNT(DISTINCT state) FILTER (WHERE NOT missing) ",
    "= 1 THEN MIN(state) FILTER (WHERE NOT missing) END AS state FROM ", classified,
    " GROUP BY entity_value)"
  )
  list(sql = sql, params = contract$params, states = states)
}

lt_period_profiles <- function(context) {
  populations <- lt_population_counts(context)
  audit_rows <- vector("list", length(context$cells))
  profiles <- vector("list", length(context$cells))
  for (cell_index in seq_along(context$cells)) {
    cell <- context$cells[[cell_index]]
    population <- populations[[cell$period_index]]
    if (is.null(cell$unavailable)) {
      profile <- lt_period_profile(context, cell, population)
      audit_rows[[cell_index]] <- profile$audit
      profiles[[cell_index]] <- profile
    } else {
      audit_rows[[cell_index]] <- lt_state_audit_row(
        context, cell, population, NA_real_, NA_real_, NA_real_,
        "unavailable", cell$unavailable$code
      )
      profiles[[cell_index]] <- list(
        states = character(), counts = numeric(), audit = audit_rows[[cell_index]],
        unavailable = cell$unavailable
      )
    }
  }
  list(
    state_audit = lt_bind(audit_rows, lt_empty_state_audit()),
    profiles = profiles,
    populations = populations
  )
}

lt_period_profile <- function(context, cell, population) {
  ctes <- lt_state_ctes(context, cell, "period")
  query <- paste0(
    "WITH ", ctes$sql,
    ", totals AS (SELECT COUNT(*)::text AS n_valid_entities, ",
    "COUNT(*) FILTER (WHERE n_states = 1)::text AS n_usable_state, ",
    "COUNT(*) FILTER (WHERE n_states = 0)::text AS n_missing_state, ",
    "COUNT(*) FILTER (WHERE n_states > 1)::text AS n_conflicting_state FROM ",
    ctes$states, "), state_counts AS (SELECT state, COUNT(*)::text AS n ",
    "FROM ", ctes$states,
    " WHERE n_states = 1 GROUP BY state ORDER BY state COLLATE \"C\" ",
    "LIMIT ", as.integer(context$max_levels + 1L),
    ") SELECT state_counts.state, state_counts.n, totals.n_valid_entities, ",
    "totals.n_usable_state, totals.n_missing_state, ",
    "totals.n_conflicting_state FROM totals LEFT JOIN state_counts ON TRUE ",
    "ORDER BY state_counts.state COLLATE \"C\" NULLS FIRST"
  )
  observed <- eda_db_fetch(
    cell$source$con,
    query,
    params = ctes$params,
    query_kind = "transition_period_states",
    limit = as.integer(context$max_levels + 1L),
    variable_index = cell$variable_index,
    name = cell$name
  )
  expected <- c(
    "state", "n", "n_valid_entities", "n_usable_state",
    "n_missing_state", "n_conflicting_state"
  )
  if (nrow(observed) < 1L || !identical(names(observed), expected)) {
    stop("PostgreSQL transition period audit was incomplete.", call. = FALSE)
  }
  state_rows <- !is.na(observed$state)
  if (sum(state_rows) > context$max_levels) {
    stop(
      "A PostgreSQL transition period domain exceeds max_levels.",
      call. = FALSE
    )
  }
  totals <- vapply(expected[3:6], function(field) {
    longitudinal_qc_checked_count( # nolint: object_usage_linter
      observed[[field]][[1L]], paste("transition", field)
    )
  }, numeric(1))
  repeated_totals <- vapply(expected[3:6], function(field) {
    length(unique(as.character(observed[[field]]))) == 1L
  }, logical(1))
  if (!all(repeated_totals)) {
    stop("PostgreSQL transition period counts did not reconcile.", call. = FALSE)
  }
  states <- as.character(observed$state[state_rows])
  counts <- if (length(states) == 0L) {
    numeric()
  } else {
    vapply(observed$n[state_rows], function(value) {
      longitudinal_qc_checked_count( # nolint: object_usage_linter
        value, "transition usable state count"
      )
    }, numeric(1))
  }
  names(counts) <- states
  period_domain <- lt_ordered_domain(cell$levels, states)
  reconciled <- totals[["n_valid_entities"]] == population &&
    sum(totals[c(
      "n_usable_state", "n_missing_state", "n_conflicting_state"
    )]) == population &&
    sum(counts) == totals[["n_usable_state"]] &&
    length(states) == length(unique(states))
  if (!reconciled) {
    stop("PostgreSQL transition period counts did not reconcile.", call. = FALSE)
  }
  if (length(period_domain) > context$max_levels) {
    stop(
      "A PostgreSQL transition period domain exceeds max_levels.",
      call. = FALSE
    )
  }
  audit <- lt_state_audit_row(
    context, cell, population,
    totals[["n_usable_state"]], totals[["n_missing_state"]],
    totals[["n_conflicting_state"]], "available", NA_character_
  )
  list(
    states = states,
    counts = counts,
    audit = audit,
    unavailable = NULL
  )
}

lt_state_audit_row <- function(context,
                               cell,
                               n_valid_entities,
                               n_usable_state,
                               n_missing_state,
                               n_conflicting_state,
                               status,
                               reason) {
  data.frame(
    period_index = as.integer(cell$period_index),
    period = context$period_labels[[cell$period_index]],
    variable_index = as.integer(cell$variable_index),
    variable = cell$name,
    n_valid_entities = as.numeric(n_valid_entities),
    n_usable_state = as.numeric(n_usable_state),
    n_missing_state = as.numeric(n_missing_state),
    n_conflicting_state = as.numeric(n_conflicting_state),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

lt_ordered_domain <- function(declared, observed) {
  unexpected <- sort(setdiff(unique(observed), declared), method = "radix")
  if (length(declared) > 0L) c(declared, unexpected) else unexpected
}

lt_declared_domain_cte <- function(con, declared) {
  if (length(declared) == 0L) {
    return(
      "declared_states(state) AS (SELECT NULL::text COLLATE \"C\" WHERE FALSE)"
    )
  }
  quoted <- as.character(DBI::dbQuoteString(con, declared))
  values <- paste0("(", quoted, "::text COLLATE \"C\")")
  paste0(
    "declared_states(state) AS (VALUES ",
    paste(values, collapse = ", "), ")"
  )
}

lt_adjacent_domain_preflights <- function(context) {
  for (left_period in seq_len(length(context$sources) - 1L)) {
    right_period <- left_period + 1L
    for (variable_index in seq_len(nrow(context$selected))) {
      left_cell <- context$cells[[lt_cell_index(
        left_period, variable_index, context
      )]]
      right_cell <- context$cells[[lt_cell_index(
        right_period, variable_index, context
      )]]
      if (!is.null(left_cell$unavailable) ||
            !is.null(right_cell$unavailable)) {
        next
      }
      left_ctes <- lt_state_ctes(context, left_cell, "left_preflight", 0L)
      right_ctes <- lt_state_ctes(
        context, right_cell, "right_preflight", length(left_ctes$params)
      )
      declared <- lt_declared_levels(
        context$selected[variable_index, , drop = FALSE]
      )
      query <- paste0(
        "WITH ", left_ctes$sql, ", ", right_ctes$sql, ", ",
        lt_declared_domain_cte(left_cell$source$con, declared),
        ", bounded_domain AS (SELECT state FROM declared_states UNION ",
        "SELECT state FROM ", left_ctes$states, " WHERE n_states = 1 UNION ",
        "SELECT state FROM ", right_ctes$states, " WHERE n_states = 1) ",
        "SELECT state FROM bounded_domain ORDER BY state COLLATE \"C\" LIMIT ",
        as.integer(context$max_levels + 1L)
      )
      observed <- eda_db_fetch(
        left_cell$source$con,
        query,
        params = c(left_ctes$params, right_ctes$params),
        query_kind = "transition_adjacent_domain_preflight",
        limit = as.integer(context$max_levels + 1L),
        variable_index = as.integer(variable_index),
        name = context$variables[[variable_index]]
      )
      if (!identical(names(observed), "state")) {
        stop(
          "PostgreSQL transition adjacent-domain preflight was incomplete.",
          call. = FALSE
        )
      }
      if (nrow(observed) > context$max_levels) {
        stop(
          "A PostgreSQL transition adjacent union exceeds max_levels.",
          call. = FALSE
        )
      }
    }
  }
  invisible(NULL)
}

lt_transitions <- function(context, profiles) {
  summary_rows <- list()
  count_rows <- list()
  for (left_period in seq_len(length(context$sources) - 1L)) {
    right_period <- left_period + 1L
    for (variable_index in seq_len(nrow(context$selected))) {
      left_index <- lt_cell_index(left_period, variable_index, context)
      right_index <- lt_cell_index(right_period, variable_index, context)
      left <- profiles$profiles[[left_index]]
      right <- profiles$profiles[[right_index]]
      cell <- context$cells[[left_index]]
      if (!is.null(left$unavailable) || !is.null(right$unavailable)) {
        reason <- lt_unavailable_reason(left, right)
        retained <- lt_retained_count(
          context, left_period, right_period, variable_index
        )
        summary_rows[[length(summary_rows) + 1L]] <- lt_transition_summary_row(
          context, left_period, right_period, variable_index,
          retained, NA_real_, NA_real_, NA_real_, 0L, NA_real_,
          "unavailable", reason
        )
        next
      }
      domain <- lt_ordered_domain(
        cell$levels, c(left$states, right$states)
      )
      if (length(domain) > context$max_levels) {
        stop(
          "A PostgreSQL transition adjacent union exceeds max_levels.",
          call. = FALSE
        )
      }
      pair <- lt_pair_profile(
        context, left_period, right_period, variable_index, domain
      )
      zero <- pair$n_eligible == 0
      status <- if (zero) "unavailable" else "available"
      reason <- if (zero) "zero_eligible" else NA_character_
      summary_rows[[length(summary_rows) + 1L]] <- lt_transition_summary_row(
        context, left_period, right_period, variable_index,
        pair$n_retained, pair$n_eligible, pair$n_excluded_missing,
        pair$n_excluded_conflict, as.integer(length(domain)^2),
        pair$n_eligible, status, reason
      )
      rows <- lt_complete_transition_counts(
        context, left_period, right_period, variable_index, domain, pair,
        status, reason
      )
      if (nrow(rows) > 0L) count_rows[[length(count_rows) + 1L]] <- rows
    }
  }
  list(
    transition_summary = lt_bind(
      summary_rows, lt_empty_transition_summary()
    ),
    transition_counts = lt_bind(
      count_rows, lt_empty_transition_counts()
    )
  )
}

lt_unavailable_reason <- function(left, right) {
  if (!is.null(left$unavailable)) return(left$unavailable$code)
  right$unavailable$code
}

lt_retained_count <- function(context,
                              left_period,
                              right_period,
                              variable_index) {
  left_source <- context$sources[[left_period]]
  right_source <- context$sources[[right_period]]
  left_entity <- longitudinal_qc_entity_sql( # nolint: object_usage_linter
    left_source, context$entity_id
  )
  right_entity <- longitudinal_qc_entity_sql( # nolint: object_usage_linter
    right_source, context$entity_id
  )
  query <- paste0(
    "WITH left_members AS (SELECT DISTINCT ", left_entity,
    " AS entity_value FROM ", eda_postgres_table_sql(left_source),
    " WHERE ", longitudinal_entity_predicate( # nolint: object_usage_linter
      left_source, context$entity_id, context$entity_family
    ), "), right_members AS (SELECT DISTINCT ", right_entity,
    " AS entity_value FROM ", eda_postgres_table_sql(right_source),
    " WHERE ", longitudinal_entity_predicate( # nolint: object_usage_linter
      right_source, context$entity_id, context$entity_family
    ), ") SELECT COUNT(*)::text AS n_retained FROM left_members ",
    "INNER JOIN right_members USING (entity_value)"
  )
  observed <- eda_db_fetch(
    left_source$con, query, query_kind = "transition_retained_count", limit = 1L,
    variable_index = as.integer(variable_index),
    name = context$variables[[variable_index]]
  )
  if (nrow(observed) != 1L || !identical(names(observed), "n_retained")) {
    stop("PostgreSQL transition retained count was incomplete.", call. = FALSE)
  }
  longitudinal_qc_checked_count( # nolint: object_usage_linter
    observed$n_retained[[1L]], "transition n_retained"
  )
}

lt_pair_profile <- function(context,
                            left_period,
                            right_period,
                            variable_index,
                            domain) {
  left_cell <- context$cells[[lt_cell_index(
    left_period, variable_index, context
  )]]
  right_cell <- context$cells[[lt_cell_index(
    right_period, variable_index, context
  )]]
  left_ctes <- lt_state_ctes(context, left_cell, "left", 0L)
  right_ctes <- lt_state_ctes(
    context, right_cell, "right", length(left_ctes$params)
  )
  cell_limit <- as.integer(length(domain)^2 + 1)
  query <- paste0(
    "WITH ", left_ctes$sql, ", ", right_ctes$sql,
    ", retained AS (SELECT left_states.n_states AS left_n_states, ",
    "left_states.state AS from_state, ",
    "right_states.n_states AS right_n_states, ",
    "right_states.state AS to_state FROM ", left_ctes$states,
    " AS left_states INNER JOIN ", right_ctes$states,
    " AS right_states USING (entity_value)), totals AS (SELECT ",
    "COUNT(*)::text AS n_retained, ",
    "COUNT(*) FILTER (WHERE left_n_states = 1 AND right_n_states = 1)::text ",
    "AS n_eligible, COUNT(*) FILTER (WHERE left_n_states <= 1 AND ",
    "right_n_states <= 1 AND (left_n_states = 0 OR right_n_states = 0))::text ",
    "AS n_excluded_missing, COUNT(*) FILTER (WHERE left_n_states > 1 OR ",
    "right_n_states > 1)::text AS n_excluded_conflict FROM retained), ",
    "transition_counts AS (SELECT from_state, to_state, COUNT(*)::text AS n ",
    "FROM retained WHERE left_n_states = 1 AND right_n_states = 1 ",
    "GROUP BY from_state, to_state) SELECT transition_counts.from_state, ",
    "transition_counts.to_state, transition_counts.n, totals.n_retained, ",
    "totals.n_eligible, totals.n_excluded_missing, ",
    "totals.n_excluded_conflict FROM totals LEFT JOIN transition_counts ",
    "ON TRUE ORDER BY transition_counts.from_state COLLATE \"C\" NULLS FIRST, ",
    "transition_counts.to_state COLLATE \"C\" NULLS FIRST LIMIT ", cell_limit
  )
  observed <- eda_db_fetch(
    left_cell$source$con,
    query,
    params = c(left_ctes$params, right_ctes$params),
    query_kind = "transition_adjacent_counts",
    limit = cell_limit,
    variable_index = as.integer(variable_index),
    name = context$variables[[variable_index]]
  )
  expected <- c(
    "from_state", "to_state", "n", "n_retained", "n_eligible",
    "n_excluded_missing", "n_excluded_conflict"
  )
  if (nrow(observed) < 1L || !identical(names(observed), expected)) {
    stop("PostgreSQL adjacent transition audit was incomplete.", call. = FALSE)
  }
  totals <- vapply(expected[4:7], function(field) {
    longitudinal_qc_checked_count( # nolint: object_usage_linter
      observed[[field]][[1L]], paste("transition", field)
    )
  }, numeric(1))
  repeated_totals <- vapply(expected[4:7], function(field) {
    length(unique(as.character(observed[[field]]))) == 1L
  }, logical(1))
  mapped <- !is.na(observed$from_state) & !is.na(observed$to_state)
  mapping_counts <- if (any(mapped)) {
    vapply(observed$n[mapped], function(value) {
      longitudinal_qc_checked_count( # nolint: object_usage_linter
        value, "transition cell count"
      )
    }, numeric(1))
  } else {
    numeric()
  }
  from_states <- as.character(observed$from_state[mapped])
  to_states <- as.character(observed$to_state[mapped])
  valid <- all(repeated_totals) &&
    totals[["n_retained"]] == sum(totals[c(
      "n_eligible", "n_excluded_missing", "n_excluded_conflict"
    )]) &&
    sum(mapping_counts) == totals[["n_eligible"]] &&
    all(from_states %in% domain) && all(to_states %in% domain) &&
    !anyDuplicated(data.frame(from_states, to_states))
  if (!valid) {
    stop("PostgreSQL adjacent transition counts did not reconcile.", call. = FALSE)
  }
  list(
    from_state = from_states,
    to_state = to_states,
    n = mapping_counts,
    n_retained = totals[["n_retained"]],
    n_eligible = totals[["n_eligible"]],
    n_excluded_missing = totals[["n_excluded_missing"]],
    n_excluded_conflict = totals[["n_excluded_conflict"]]
  )
}

lt_transition_summary_row <- function(context,
                                      left_period,
                                      right_period,
                                      variable_index,
                                      n_retained,
                                      n_eligible,
                                      n_excluded_missing,
                                      n_excluded_conflict,
                                      n_transition_cells,
                                      eligible_denominator,
                                      status,
                                      reason) {
  data.frame(
    left_period_index = as.integer(left_period),
    left_period = context$period_labels[[left_period]],
    right_period_index = as.integer(right_period),
    right_period = context$period_labels[[right_period]],
    variable_index = as.integer(variable_index),
    variable = context$variables[[variable_index]],
    n_retained = as.numeric(n_retained),
    n_eligible = as.numeric(n_eligible),
    n_excluded_missing = as.numeric(n_excluded_missing),
    n_excluded_conflict = as.numeric(n_excluded_conflict),
    n_transition_cells = as.integer(n_transition_cells),
    eligible_denominator = as.numeric(eligible_denominator),
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

lt_complete_transition_counts <- function(context,
                                          left_period,
                                          right_period,
                                          variable_index,
                                          domain,
                                          pair,
                                          status,
                                          reason) {
  if (length(domain) == 0L) return(lt_empty_transition_counts())
  domain_size <- length(domain)
  from_state <- rep(domain, each = domain_size)
  to_state <- rep(domain, times = domain_size)
  counts <- numeric(length(from_state))
  if (length(pair$n) > 0L) {
    from_index <- match(pair$from_state, domain)
    to_index <- match(pair$to_state, domain)
    index <- (from_index - 1L) * domain_size + to_index
    counts[index] <- pair$n
  }
  declared <- lt_declared_levels(
    context$selected[variable_index, , drop = FALSE]
  )
  has_declared <- length(declared) > 0L
  from_declared <- if (has_declared) {
    from_state %in% declared
  } else {
    rep(NA, length(from_state))
  }
  to_declared <- if (has_declared) {
    to_state %in% declared
  } else {
    rep(NA, length(to_state))
  }
  data.frame(
    left_period_index = rep(as.integer(left_period), length(from_state)),
    left_period = rep(context$period_labels[[left_period]], length(from_state)),
    right_period_index = rep(as.integer(right_period), length(from_state)),
    right_period = rep(context$period_labels[[right_period]], length(from_state)),
    variable_index = rep(as.integer(variable_index), length(from_state)),
    variable = rep(context$variables[[variable_index]], length(from_state)),
    from_state = from_state,
    to_state = to_state,
    n = counts,
    eligible_denominator = rep(as.numeric(pair$n_eligible), length(from_state)),
    proportion = if (pair$n_eligible == 0) {
      rep(NA_real_, length(from_state))
    } else {
      counts / pair$n_eligible
    },
    from_is_declared = from_declared,
    from_is_unexpected = if (has_declared) {
      !from_declared
    } else {
      rep(FALSE, length(from_state))
    },
    to_is_declared = to_declared,
    to_is_unexpected = if (has_declared) {
      !to_declared
    } else {
      rep(FALSE, length(to_state))
    },
    status = rep(status, length(from_state)),
    reason = rep(reason, length(from_state)),
    stringsAsFactors = FALSE
  )
}

lt_issues <- function(state_audit, transition_summary, context) {
  unavailable <- list()
  conflicts <- list()
  zero <- list()
  for (row_index in seq_len(nrow(state_audit))) {
    row <- state_audit[row_index, , drop = FALSE]
    if (row$status[[1L]] == "unavailable") {
      unavailable[[length(unavailable) + 1L]] <- lt_period_issue(row, context)
    }
    if (!is.na(row$n_conflicting_state[[1L]]) &&
          row$n_conflicting_state[[1L]] > 0) {
      conflicts[[length(conflicts) + 1L]] <- lt_conflict_issue(row)
    }
  }
  for (row_index in seq_len(nrow(transition_summary))) {
    row <- transition_summary[row_index, , drop = FALSE]
    if (identical(row$reason[[1L]], "zero_eligible")) {
      zero[[length(zero) + 1L]] <- lt_zero_issue(row)
    }
  }
  lt_bind(c(unavailable, conflicts, zero), lt_empty_issues())
}

lt_period_issue <- function(row, context) {
  cell <- context$cells[[lt_cell_index(
    row$period_index[[1L]], row$variable_index[[1L]], context
  )]]
  severity <- if (row$reason[[1L]] == "unsupported_analysis_type") {
    "warning"
  } else {
    "error"
  }
  lt_issue_row(
    issue_code = row$reason[[1L]],
    severity = severity,
    period_index = row$period_index[[1L]],
    period = row$period[[1L]],
    variable_index = row$variable_index[[1L]],
    variable = row$variable[[1L]],
    n_affected = row$n_valid_entities[[1L]],
    message = cell$unavailable$reason
  )
}

lt_conflict_issue <- function(row) {
  lt_issue_row(
    issue_code = "conflicting_state",
    severity = "warning",
    period_index = row$period_index[[1L]],
    period = row$period[[1L]],
    variable_index = row$variable_index[[1L]],
    variable = row$variable[[1L]],
    n_affected = row$n_conflicting_state[[1L]],
    message = "Valid entities have more than one distinct non-missing state in the period."
  )
}

lt_zero_issue <- function(row) {
  lt_issue_row(
    issue_code = "zero_eligible",
    severity = "warning",
    left_period_index = row$left_period_index[[1L]],
    left_period = row$left_period[[1L]],
    right_period_index = row$right_period_index[[1L]],
    right_period = row$right_period[[1L]],
    variable_index = row$variable_index[[1L]],
    variable = row$variable[[1L]],
    n_affected = row$n_retained[[1L]],
    message = "No retained entity has one usable state in both adjacent periods."
  )
}

lt_issue_row <- function(issue_code,
                         severity,
                         period_index = NA_integer_,
                         period = NA_character_,
                         left_period_index = NA_integer_,
                         left_period = NA_character_,
                         right_period_index = NA_integer_,
                         right_period = NA_character_,
                         variable_index,
                         variable,
                         n_affected,
                         message) {
  data.frame(
    issue_code = issue_code,
    severity = severity,
    period_index = as.integer(period_index),
    period = period,
    left_period_index = as.integer(left_period_index),
    left_period = left_period,
    right_period_index = as.integer(right_period_index),
    right_period = right_period,
    variable_index = as.integer(variable_index),
    variable = variable,
    n_affected = as.numeric(n_affected),
    message = message,
    stringsAsFactors = FALSE
  )
}

lt_bind <- function(rows, empty) {
  if (length(rows) == 0L) return(empty)
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

lt_empty_state_audit <- function() {
  data.frame(
    period_index = integer(), period = character(),
    variable_index = integer(), variable = character(),
    n_valid_entities = numeric(), n_usable_state = numeric(),
    n_missing_state = numeric(), n_conflicting_state = numeric(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

lt_empty_transition_summary <- function() {
  data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    n_retained = numeric(), n_eligible = numeric(),
    n_excluded_missing = numeric(), n_excluded_conflict = numeric(),
    n_transition_cells = integer(), eligible_denominator = numeric(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

lt_empty_transition_counts <- function() {
  data.frame(
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    from_state = character(), to_state = character(), n = numeric(),
    eligible_denominator = numeric(), proportion = numeric(),
    from_is_declared = logical(), from_is_unexpected = logical(),
    to_is_declared = logical(), to_is_unexpected = logical(),
    status = character(), reason = character(), stringsAsFactors = FALSE
  )
}

lt_empty_issues <- function() {
  data.frame(
    issue_code = character(), severity = character(),
    period_index = integer(), period = character(),
    left_period_index = integer(), left_period = character(),
    right_period_index = integer(), right_period = character(),
    variable_index = integer(), variable = character(),
    n_affected = numeric(), message = character(),
    stringsAsFactors = FALSE
  )
}
