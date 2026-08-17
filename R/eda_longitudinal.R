#' Summarise one reviewed long-form longitudinal panel
#'
#' Add a thin descriptive layer over one already-curated long-form panel. The
#' function describes panel structure, follow-up, time-point presence,
#' entity-aware variable completeness, ordinary time-stratified summaries and
#' signed numeric change. It does not clean, impute, balance or interpret the
#' panel.
#'
#' @param data A data frame or one unmodified [epi_eda_postgres_source()].
#' @param spec An EDA specification accepted by [epi_eda_spec()].
#' @param id A single entity-identifier column name.
#' @param time A distinct categorical or binary discrete-time column name.
#' @param time_order `NULL`, or the reviewed declared time levels in exactly
#'   their declared order.
#' @param variables `NULL` to select eligible specification variables in
#'   specification order, or a unique character vector retaining caller order.
#'
#' @return An `epi_eda_longitudinal` list with `metadata`, `structure`,
#'   `followup`, `timepoints`, `missingness`, `summaries`, `change`, and
#'   aggregate value-free `issues` components.
#'
#' @details Entity-time cells use the same missing-state, usable-state and
#' conflicting-state precedence as [epi_eda_longitudinal_transitions()]: zero,
#' one, or more than one distinct non-missing canonical value respectively.
#' Declared time points with no observations are retained. The `summaries`
#' component is the unchanged result of [epi_eda_profile_stratified()] for the
#' deterministic time-plus-selected-variable specification.
#'
#' PostgreSQL calculations use exact checked aggregate counts and one read-only
#' repeatable-read snapshot. Entity values and entity histories are used only
#' for database-side grouping and are never returned. Failures return no
#' partial object and the connection remains caller-owned.
#'
#' @export
epi_eda_longitudinal <- function(data,
                                 spec,
                                 id,
                                 time,
                                 time_order = NULL,
                                 variables = NULL) {
  inputs <- le_inputs(data, spec, id, time, time_order, variables)
  if (inherits(data, "epi_eda_postgres_source")) {
    return(le_postgres(inputs))
  }
  le_data_frame(inputs)
}

#' @export
print.epi_eda_longitudinal <- function(x, ...) {
  cat("<epi_eda_longitudinal>\n")
  cat("  Time points: ", length(x$metadata$time_order[[1L]]), "\n", sep = "")
  cat("  Variables: ", length(x$metadata$resolved_variables[[1L]]), "\n", sep = "")
  cat("  Valid entities: ", x$structure$n_valid_entities[[1L]], "\n", sep = "")
  cat("  Technical findings: ", nrow(x$issues), "\n", sep = "")
  invisible(x)
}

le_inputs <- function(data, spec, id, time, time_order, variables) {
  postgres <- inherits(data, "epi_eda_postgres_source")
  if (postgres) {
    eda_validate_postgres_source(data, require_idle = TRUE)
  } else {
    stratified_validate_data(data)
  }
  spec <- epi_eda_spec(spec)
  id <- le_name(id, "id")
  time <- le_name(time, "time")
  if (identical(id, time)) {
    stop("id and time must name distinct columns.", call. = FALSE)
  }
  if (!time %in% spec$name) {
    stop("time must be represented in the EDA specification.", call. = FALSE)
  }
  observed_names <- if (postgres) data$columns$name else names(data)
  if (any(!c(id, time) %in% observed_names)) {
    stop("id and time must be present in data.", call. = FALSE)
  }
  time_row <- spec[match(time, spec$name), , drop = FALSE]
  if (!time_row$analysis_type[[1L]] %in% c("categorical", "binary")) {
    stop("time must be declared categorical or binary.", call. = FALSE)
  }
  levels <- le_time_levels(data, time_row, postgres)
  if (!is.null(time_order)) {
    valid_order <- is.character(time_order) && !anyNA(time_order) &&
      !anyDuplicated(time_order) && all(nzchar(trimws(time_order)))
    if (!valid_order || !identical(unname(time_order), levels)) {
      stop("time_order must exactly match the reviewed declared time levels and order.", call. = FALSE)
    }
  }
  private <- trimws(tolower(as.character(spec$role))) %in% c("id", "identifier")
  eligible <- spec$name[!private & !spec$name %in% c(id, time)]
  if (is.null(variables)) {
    variables <- eligible
  } else {
    valid_variables <- is.character(variables) && length(variables) > 0L && !anyNA(variables) &&
      !anyDuplicated(variables) && all(nzchar(trimws(variables)))
    if (!valid_variables) {
      stop("variables must be NULL or a unique non-blank character vector.", call. = FALSE)
    }
    if (any(!variables %in% spec$name)) {
      stop("variables cannot select outside the EDA specification.", call. = FALSE)
    }
    if (any(!variables %in% eligible)) {
      stop("variables cannot include id, time, or an identifier-role variable.", call. = FALSE)
    }
    variables <- unname(variables)
  }
  if (any(!variables %in% observed_names)) {
    stop("Every selected variable must be present in data.", call. = FALSE)
  }
  selected <- spec[match(variables, spec$name), , drop = FALSE]
  rownames(selected) <- NULL
  summary_spec <- rbind(
    spec[match(time, spec$name), , drop = FALSE],
    selected
  )
  rownames(summary_spec) <- NULL
  le_validate_storage(data, spec, id, time, selected, levels, postgres)
  list(
    data = data, spec = spec, id = id, time = time, levels = levels,
    variables = variables, selected = selected, summary_spec = summary_spec,
    postgres = postgres
  )
}

le_name <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
    stop(name, " must be a single non-blank character column name.", call. = FALSE)
  }
  unname(value)
}

le_time_levels <- function(data, time_row, postgres) {
  levels <- if ("levels" %in% names(time_row)) {
    eda_spec_levels(time_row$levels[[1L]])
  } else {
    character()
  }
  logical_time <- if (postgres) {
    eda_postgres_storage_family(eda_postgres_column(data, time_row$name[[1L]])) ==
      "boolean"
  } else {
    is.logical(data[[time_row$name[[1L]]]])
  }
  if (length(levels) == 0L &&
        time_row$analysis_type[[1L]] == "binary" && logical_time) {
    levels <- c("FALSE", "TRUE")
  }
  if (length(levels) == 0L) {
    stop("time requires reviewed declared levels; logical binary time uses FALSE, TRUE.", call. = FALSE)
  }
  declared <- prepare_declared_levels(time_row)
  if (!declared$safe || anyDuplicated(levels)) {
    stop("time levels must be unique and safely represented by the semicolon contract.", call. = FALSE)
  }
  levels
}

le_validate_storage <- function(data, spec, id, time, selected, levels, postgres) {
  if (postgres) {
    id_column <- eda_postgres_column(data, id)
    family <- sec_identifier_family(as.character(id_column$base_udt_name[[1L]]))
    if (is.na(family)) {
      stop("PostgreSQL id must use text, integral, or UUID storage.", call. = FALSE)
    }
    if (family == "text" && !longitudinal_qc_deterministic(id_column)) {
      stop("Textual id equality requires a deterministic PostgreSQL collation.", call. = FALSE)
    }
    rows <- rbind(spec[match(time, spec$name), , drop = FALSE], selected)
    for (index in seq_len(nrow(rows))) {
      row <- rows[index, , drop = FALSE]
      column <- eda_postgres_column(data, row$name[[1L]])
      declared <- if (row$name[[1L]] == time) levels else le_declared(row)
      compatibility <- eda_pg_type_compatibility(
        column, row$analysis_type[[1L]], declared
      )
      contract <- eda_postgres_missing_contract(
        data, column, row$analysis_type[[1L]],
        eda_missing_codes(spec, row$name[[1L]])
      )
      if (identical(compatibility$status, "incompatible") || !contract$valid) {
        stop("A required longitudinal selection has incompatible PostgreSQL storage.", call. = FALSE)
      }
    }
    return(invisible(TRUE))
  }
  ids <- data[[id]]
  valid_id_class <- is.character(ids) || is.factor(ids) ||
    (is.integer(ids) && !inherits(ids, "IDate")) ||
    (is.double(ids) && !inherits(ids, c("Date", "POSIXt")))
  if (!valid_id_class) {
    stop("Data-frame id must be character, factor, integer, or finite exact-integer numeric.", call. = FALSE)
  }
  observed_id <- ids[!is.na(ids)]
  if (is.double(ids) &&
        any(!is.finite(observed_id) | observed_id != floor(observed_id) |
              abs(observed_id) > 9007199254740991)) {
    stop("Numeric id values must be finite exact integers within 2^53 - 1.", call. = FALSE)
  }
  schema <- epi_eda_check_schema(data, rbind(
    spec[match(time, spec$name), , drop = FALSE], selected
  ))
  required <- c(time, selected$name)
  if (any(schema$type_status[match(required, schema$name)] == "incompatible")) {
    stop("A required longitudinal selection has incompatible observed storage.", call. = FALSE)
  }
  invisible(TRUE)
}

le_declared <- function(row) {
  if ("levels" %in% names(row)) eda_spec_levels(row$levels[[1L]]) else character()
}

le_data_frame <- function(inputs) {
  panel <- le_df_panel(inputs)
  summaries <- epi_eda_profile_stratified(
    inputs$data, inputs$summary_spec, inputs$time,
    include_overall = TRUE, include_missing_stratum = TRUE
  )
  structure(
    list(
      metadata = le_metadata(inputs, "data_frame"),
      structure = le_df_structure(panel, inputs),
      followup = le_df_followup(panel, inputs),
      timepoints = le_df_timepoints(panel, inputs),
      missingness = le_df_missingness(panel, inputs),
      summaries = summaries,
      change = le_df_change(panel, inputs),
      issues = le_df_issues(panel, inputs)
    ),
    class = c("epi_eda_longitudinal", "list")
  )
}

le_metadata <- function(inputs, backend) {
  data.frame(
    contract_version = "longitudinal-eda-1",
    backend = backend,
    id = inputs$id,
    time = inputs$time,
    resolved_variables = I(list(unname(inputs$variables))),
    time_order = I(list(unname(inputs$levels))),
    specification_fingerprint_sha256 = eda_postgres_fingerprint(inputs$spec),
    selected_specification_fingerprint_sha256 = eda_postgres_fingerprint(inputs$selected),
    source_fingerprint_sha256 = if (inputs$postgres) eda_pg_source_fingerprint(inputs$data) else NA_character_,
    count_contract = "exact-base-r-double",
    count_maximum = 9007199254740991,
    snapshot_mode = if (inputs$postgres) "REPEATABLE READ READ ONLY caller-owned snapshot" else "caller-owned-in-memory",
    stringsAsFactors = FALSE
  )
}

le_df_panel <- function(inputs) {
  data <- inputs$data
  ids <- data[[inputs$id]]
  id_missing <- is.na(ids)
  id_blank <- !id_missing & (is.character(ids) || is.factor(ids)) &
    trimws(as.character(ids)) == ""
  id_valid <- !id_missing & !id_blank
  id_value <- as.character(ids)
  times <- data[[inputs$time]]
  time_missing <- summary_missing_mask(
    times, eda_missing_codes(inputs$spec, inputs$time)
  )
  time_value <- as.character(times)
  unexpected <- unique(time_value[!time_missing & !time_value %in% inputs$levels])
  if (length(unexpected) > 0L) {
    stop("Observed non-missing time values must belong to the reviewed time levels.", call. = FALSE)
  }
  valid <- id_valid & !time_missing
  time_index <- match(time_value, inputs$levels)
  cell_key <- rep(NA_integer_, nrow(data))
  if (any(valid)) {
    cell_key[valid] <- as.integer(interaction(
      factor(id_value[valid], exclude = NULL),
      factor(time_index[valid], levels = seq_along(inputs$levels)),
      drop = TRUE, lex.order = TRUE
    ))
  }
  list(
    data = data, id_value = id_value, time_value = time_value,
    time_index = time_index, id_missing = id_missing, id_blank = id_blank,
    time_missing = time_missing, valid = valid, cell_key = cell_key
  )
}

le_df_structure <- function(panel, inputs) {
  valid_keys <- panel$cell_key[panel$valid]
  frequencies <- table(valid_keys)
  entities <- unique(panel$id_value[panel$valid])
  n_cells <- length(frequencies)
  possible <- length(entities) * length(inputs$levels)
  complete <- if (length(entities) == 0L) 0L else sum(
    vapply(entities, function(id) {
      length(unique(panel$time_index[panel$valid & panel$id_value == id])) ==
        length(inputs$levels)
    }, logical(1))
  )
  rows_per_cell <- as.numeric(frequencies)
  data.frame(
    n_rows = as.numeric(nrow(panel$data)),
    n_missing_id = as.numeric(sum(panel$id_missing)),
    n_blank_id = as.numeric(sum(panel$id_blank)),
    n_missing_time = as.numeric(sum(panel$time_missing)),
    n_invalid_id_and_missing_time = as.numeric(sum((panel$id_missing | panel$id_blank) & panel$time_missing)),
    n_valid_panel_rows = as.numeric(sum(panel$valid)),
    n_valid_entities = as.numeric(length(entities)),
    n_declared_timepoints = as.numeric(length(inputs$levels)),
    n_observed_timepoints = as.numeric(sum(vapply(seq_along(inputs$levels), function(i) any(panel$valid & panel$time_index == i), logical(1)))),
    n_observed_id_time_cells = as.numeric(n_cells),
    n_duplicate_cells = as.numeric(sum(frequencies > 1L)),
    n_duplicate_excess = as.numeric(sum(pmax(rows_per_cell - 1, 0))),
    max_rows_per_cell = as.numeric(if (length(rows_per_cell)) max(rows_per_cell) else 0),
    n_entities_with_duplicate_cell = as.numeric(length(unique(panel$id_value[panel$valid & panel$cell_key %in% as.integer(names(frequencies)[frequencies > 1L])]))),
    n_expected_cells = as.numeric(possible),
    n_complete_entities = as.numeric(complete),
    n_incomplete_entities = as.numeric(length(entities) - complete),
    stringsAsFactors = FALSE
  )
}

le_df_followup <- function(panel, inputs) {
  entities <- unique(panel$id_value[panel$valid])
  counts <- if (length(entities) == 0L) integer() else vapply(
    entities,
    function(id) length(unique(panel$time_index[panel$valid & panel$id_value == id])),
    integer(1)
  )
  observation_count <- le_bind(lapply(seq_along(inputs$levels), function(n) {
    affected <- sum(counts == n)
    data.frame(
      n_timepoints_observed = as.integer(n),
      n_entities = as.numeric(affected),
      stringsAsFactors = FALSE
    )
  }), data.frame(n_timepoints_observed = integer(), n_entities = numeric()))
  present <- lapply(entities, function(entity) sort(unique(panel$time_index[panel$valid & panel$id_value == entity])))
  first <- vapply(present, function(x) if (length(x)) x[[1L]] else NA_integer_, integer(1))
  last <- vapply(present, function(x) if (length(x)) x[[length(x)]] else NA_integer_, integer(1))
  gaps <- vapply(present, function(x) length(x) >= 2L && any(!seq.int(x[[1L]], x[[length(x)]]) %in% x), logical(1))
  list(
    observation_count = observation_count,
    first_observation = data.frame(time_index = as.integer(seq_along(inputs$levels)), timepoint = inputs$levels, n_entities = as.numeric(vapply(seq_along(inputs$levels), function(i) sum(first == i, na.rm = TRUE), integer(1))), stringsAsFactors = FALSE),
    last_observation = data.frame(time_index = as.integer(seq_along(inputs$levels)), timepoint = inputs$levels, n_entities = as.numeric(vapply(seq_along(inputs$levels), function(i) sum(last == i, na.rm = TRUE), integer(1))), stringsAsFactors = FALSE),
    gap_status = data.frame(has_gap = c(FALSE, TRUE), n_entities = as.numeric(c(sum(!gaps), sum(gaps))), stringsAsFactors = FALSE)
  )
}

le_df_timepoints <- function(panel, inputs) {
  prior <- character()
  rows <- vector("list", length(inputs$levels))
  for (index in seq_along(inputs$levels)) {
    mask <- panel$valid & panel$time_index == index
    current <- unique(panel$id_value[mask])
    retained <- if (index == 1L) NA_real_ else as.numeric(length(intersect(prior, current)))
    absent_prior <- if (index == 1L) NA_real_ else as.numeric(length(setdiff(current, prior)))
    rows[[index]] <- data.frame(
      time_index = as.integer(index),
      timepoint = inputs$levels[[index]],
      n_rows = as.numeric(sum(mask)),
      n_entities = as.numeric(length(current)),
      n_first_observed = as.numeric(length(setdiff(current, unique(panel$id_value[panel$valid & panel$time_index < index])))),
      n_last_observed = as.numeric(length(setdiff(current, unique(panel$id_value[panel$valid & panel$time_index > index])))),
      n_retained = retained,
      n_not_present_previous = absent_prior,
      retention_numerator = retained,
      retention_denominator = if (index == 1L) NA_real_ else as.numeric(length(prior)),
      p_retained = if (index == 1L) NA_real_ else longitudinal_qc_proportion(retained, length(prior)),
      presence_numerator = absent_prior,
      presence_denominator = if (index == 1L) NA_real_ else as.numeric(length(current)),
      p_not_present_previous = if (index == 1L) NA_real_ else longitudinal_qc_proportion(absent_prior, length(current)),
      stringsAsFactors = FALSE
    )
    prior <- current
  }
  do.call(rbind, rows)
}

le_df_cell_states <- function(panel, inputs, variable_index) {
  name <- inputs$variables[[variable_index]]
  values <- panel$data[[name]]
  missing <- summary_missing_mask(values, eda_missing_codes(inputs$spec, name))
  keys <- unique(panel$cell_key[panel$valid])
  rows <- lapply(keys, function(key) {
    hit <- panel$valid & panel$cell_key == key
    observed <- values[hit & !missing]
    canonical <- unique(as.character(observed))
    data.frame(
      cell_key = key,
      id_value = panel$id_value[which(hit)[[1L]]],
      time_index = as.integer(panel$time_index[which(hit)[[1L]]]),
      n_states = as.integer(length(canonical)),
      value = if (length(canonical) == 1L) suppressWarnings(as.numeric(observed[[1L]])) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      cell_key = integer(), id_value = character(), time_index = integer(),
      n_states = integer(), value = numeric(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

le_df_missingness <- function(panel, inputs) {
  rows <- list()
  entity_rows <- list()
  distribution_rows <- list()
  interior_rows <- list()
  for (variable_index in seq_along(inputs$variables)) {
    states <- le_df_cell_states(panel, inputs, variable_index)
    for (time_index in seq_along(inputs$levels)) {
      at_time <- states[states$time_index == time_index, , drop = FALSE]
      total <- nrow(at_time)
      usable <- sum(at_time$n_states == 1L)
      missing <- sum(at_time$n_states == 0L)
      conflict <- sum(at_time$n_states > 1L)
      if (usable + missing + conflict != total) {
        stop("Longitudinal variable-cell states did not reconcile.", call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        variable_index = as.integer(variable_index),
        variable = inputs$variables[[variable_index]],
        time_index = as.integer(time_index),
        timepoint = inputs$levels[[time_index]],
        n_present_entities = as.numeric(total),
        n_usable = as.numeric(usable),
        n_missing = as.numeric(missing),
        n_conflicting = as.numeric(conflict),
        usable_numerator = as.numeric(usable), usable_denominator = as.numeric(total), p_usable = longitudinal_qc_proportion(usable, total),
        missing_numerator = as.numeric(missing), missing_denominator = as.numeric(total), p_missing = longitudinal_qc_proportion(missing, total),
        conflicting_numerator = as.numeric(conflict), conflicting_denominator = as.numeric(total), p_conflicting = longitudinal_qc_proportion(conflict, total),
        stringsAsFactors = FALSE
      )
    }
    entities <- unique(panel$id_value[panel$valid])
    usable_counts <- vapply(entities, function(entity) sum(states$n_states[states$id_value == entity] == 1L), integer(1))
    present_counts <- vapply(entities, function(entity) sum(states$id_value == entity), integer(1))
    entity_rows[[length(entity_rows) + 1L]] <- data.frame(variable_index = as.integer(variable_index), variable = inputs$variables[[variable_index]], n_valid_entities = as.numeric(length(entities)), n_never_observed = as.numeric(sum(usable_counts == 0L)), n_observed_at_least_once = as.numeric(sum(usable_counts > 0L)), n_complete_among_present = as.numeric(sum(present_counts > 0L & usable_counts == present_counts)), n_incomplete_among_present = as.numeric(sum(present_counts > 0L & usable_counts != present_counts)), stringsAsFactors = FALSE)
    distribution_rows[[length(distribution_rows) + 1L]] <- data.frame(variable_index = as.integer(variable_index), variable = inputs$variables[[variable_index]], n_usable_measurements = as.integer(0:length(inputs$levels)), n_entities = as.numeric(vapply(0:length(inputs$levels), function(n) sum(usable_counts == n), integer(1))), stringsAsFactors = FALSE)
    interior <- vapply(entities, function(entity) {
      own <- states[states$id_value == entity, , drop = FALSE]
      usable_times <- own$time_index[own$n_states == 1L]
      missing_times <- own$time_index[own$n_states == 0L]
      length(usable_times) >= 2L && any(missing_times > min(usable_times) & missing_times < max(usable_times))
    }, logical(1))
    interior_rows[[length(interior_rows) + 1L]] <- data.frame(variable_index = as.integer(variable_index), variable = inputs$variables[[variable_index]], n_entities_interior_missing = as.numeric(sum(interior)), stringsAsFactors = FALSE)
  }
  list(by_time = le_bind(rows, le_empty_missingness()), entity_summary = le_bind(entity_rows, le_empty_entity_summary()), usable_measurement_distribution = le_bind(distribution_rows, le_empty_measurement_dist()), interior_missing = le_bind(interior_rows, le_empty_interior_missing()))
}

le_df_change <- function(panel, inputs) {
  numeric_variables <- which(inputs$selected$analysis_type %in% c("numeric", "integer"))
  adjacent <- list()
  first_to_last <- list()
  for (variable_index in numeric_variables) {
    states <- le_df_cell_states(panel, inputs, variable_index)
    for (left in seq_len(max(length(inputs$levels) - 1L, 0L))) {
      adjacent[[length(adjacent) + 1L]] <- le_df_change_pair(
        states, inputs, variable_index, "adjacent", left, left + 1L
      )
    }
    first_to_last[[length(first_to_last) + 1L]] <- le_df_change_first_last(
      states, inputs, variable_index
    )
  }
  list(first_to_last = le_bind(first_to_last, le_empty_first_to_last()), adjacent = le_bind(adjacent, le_empty_adjacent_change()))
}

le_df_change_pair <- function(states, inputs, variable_index, comparison, left, right) {
  left_rows <- states[states$time_index == left, , drop = FALSE]
  right_rows <- states[states$time_index == right, , drop = FALSE]
  ids <- intersect(left_rows$id_value, right_rows$id_value)
  pairs <- lapply(ids, function(id) {
    list(
      left = left_rows[left_rows$id_value == id, , drop = FALSE],
      right = right_rows[right_rows$id_value == id, , drop = FALSE]
    )
  })
  row <- le_change_row(pairs, inputs, variable_index, comparison, left, right, 0)
  row <- row[, setdiff(names(row), c(
    "n_entities_with_presence", "n_excluded_single_timepoint"
  )), drop = FALSE]
  data.frame(from_time_index = as.integer(left), from_timepoint = inputs$levels[[left]], to_time_index = as.integer(right), to_timepoint = inputs$levels[[right]], row, stringsAsFactors = FALSE)
}

le_df_change_first_last <- function(states, inputs, variable_index) {
  ids <- unique(states$id_value)
  single <- 0L
  pairs <- list()
  for (id in ids) {
    rows <- states[states$id_value == id, , drop = FALSE]
    rows <- rows[order(rows$time_index), , drop = FALSE]
    if (nrow(rows) < 2L) {
      single <- single + 1L
    } else {
      pairs[[length(pairs) + 1L]] <- list(
        left = rows[1L, , drop = FALSE], right = rows[nrow(rows), , drop = FALSE]
      )
    }
  }
  row <- le_change_row(
    pairs, inputs, variable_index, "first_to_last", NA_integer_, NA_integer_, single
  )
  row
}

le_change_row <- function(pairs, inputs, variable_index, comparison, left, right, single) {
  conflict <- missing <- nonfinite <- 0L
  changes <- numeric()
  for (pair in pairs) {
    if (pair$left$n_states[[1L]] > 1L || pair$right$n_states[[1L]] > 1L) {
      conflict <- conflict + 1L
    } else if (pair$left$n_states[[1L]] == 0L || pair$right$n_states[[1L]] == 0L) {
      missing <- missing + 1L
    } else if (!is.finite(pair$left$value[[1L]]) || !is.finite(pair$right$value[[1L]])) {
      nonfinite <- nonfinite + 1L
    } else {
      change <- pair$right$value[[1L]] - pair$left$value[[1L]]
      if (is.finite(change)) {
        changes <- c(changes, change)
      } else {
        nonfinite <- nonfinite + 1L
      }
    }
  }
  candidates <- length(pairs)
  eligible <- length(changes)
  if (eligible + missing + conflict + nonfinite != candidates) {
    stop("Longitudinal change exclusions did not reconcile.", call. = FALSE)
  }
  data.frame(
    variable_index = as.integer(variable_index),
    variable = inputs$variables[[variable_index]],
    n_entities_with_presence = as.numeric(candidates + single),
    n_excluded_single_timepoint = as.numeric(single),
    n_present_both = as.numeric(candidates), n_eligible = as.numeric(eligible),
    n_excluded_missing = as.numeric(missing),
    n_excluded_conflict = as.numeric(conflict),
    n_excluded_nonfinite = as.numeric(nonfinite),
    delta_n = as.numeric(eligible), mean = if (eligible == 0L) NA_real_ else mean(changes), sd = if (eligible < 2L) NA_real_ else stats::sd(changes), min = if (eligible == 0L) NA_real_ else min(changes), q1 = if (eligible == 0L) NA_real_ else as.numeric(stats::quantile(changes, .25, type = 7, names = FALSE)), median = if (eligible == 0L) NA_real_ else stats::median(changes), q3 = if (eligible == 0L) NA_real_ else as.numeric(stats::quantile(changes, .75, type = 7, names = FALSE)), max = if (eligible == 0L) NA_real_ else max(changes), iqr = if (eligible == 0L) NA_real_ else stats::IQR(changes, type = 7), status = "available", reason = if (eligible == 0L) "zero_eligible" else NA_character_,
    stringsAsFactors = FALSE
  )
}

le_df_issues <- function(panel, inputs) {
  rows <- list()
  rows <- le_add_issue(rows, "missing_entity_id", n = sum(panel$id_missing), inputs = inputs)
  rows <- le_add_issue(rows, "blank_entity_id", n = sum(panel$id_blank), inputs = inputs)
  rows <- le_add_issue(rows, "missing_time", n = sum(panel$time_missing), inputs = inputs)
  frequencies <- table(panel$cell_key[panel$valid])
  rows <- le_add_issue(
    rows, "duplicate_id_time", n = sum(frequencies > 1L), inputs = inputs
  )
  for (variable_index in seq_along(inputs$variables)) {
    states <- le_df_cell_states(panel, inputs, variable_index)
    for (time_index in seq_along(inputs$levels)) {
      rows <- le_add_issue(
        rows, "conflicting_variable_cell", time_index, variable_index,
        sum(states$time_index == time_index & states$n_states > 1L), inputs
      )
    }
  }
  for (time_index in seq_along(inputs$levels)) {
    if (!any(panel$valid & panel$time_index == time_index)) {
      rows <- le_add_issue(
        rows, "zero_observation_timepoint", time_index, NA_integer_, 0,
        inputs, allow_zero = TRUE
      )
    }
  }
  le_sort_issues(le_bind(rows, le_empty_issues()))
}

le_add_issue <- function(rows,
                         code,
                         time_index = NA_integer_,
                         variable_index = NA_integer_,
                         n,
                         inputs,
                         allow_zero = FALSE) {
  if (n > 0 || (isTRUE(allow_zero) && n == 0)) {
    rows[[length(rows) + 1L]] <- le_issue_row(
      code, time_index, variable_index, n, inputs
    )
  }
  rows
}

le_issue_row <- function(code, time_index, variable_index, n, inputs) {
  messages <- c(
    missing_entity_id = "Rows with missing entity identifiers were excluded from panel aggregates.",
    blank_entity_id = "Rows with blank entity identifiers were excluded from panel aggregates.",
    missing_time = "Rows with missing reviewed time values were excluded from panel aggregates.",
    duplicate_id_time = "Valid entity-time cells contain repeated rows.",
    conflicting_variable_cell = "Entity-time cells contain conflicting non-missing values.",
    zero_observation_timepoint = "A reviewed time point has no valid entity observations."
  )
  data.frame(
    issue_code = code,
    severity = "warning",
    time_index = as.integer(time_index),
    timepoint = if (is.na(time_index)) NA_character_ else inputs$levels[[time_index]],
    variable_index = as.integer(variable_index),
    variable = if (is.na(variable_index)) NA_character_ else inputs$variables[[variable_index]],
    n_affected = as.numeric(n),
    message = unname(messages[[code]]),
    stringsAsFactors = FALSE
  )
}

le_bind <- function(rows, empty) {
  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

le_empty_missingness <- function() {
  data.frame(
    variable_index = integer(), variable = character(), time_index = integer(), timepoint = character(), n_present_entities = numeric(), n_usable = numeric(), n_missing = numeric(), n_conflicting = numeric(), usable_numerator = numeric(), usable_denominator = numeric(), p_usable = numeric(), missing_numerator = numeric(), missing_denominator = numeric(), p_missing = numeric(), conflicting_numerator = numeric(), conflicting_denominator = numeric(), p_conflicting = numeric(),
    stringsAsFactors = FALSE
  )
}

le_empty_entity_summary <- function() data.frame(variable_index = integer(), variable = character(), n_valid_entities = numeric(), n_never_observed = numeric(), n_observed_at_least_once = numeric(), n_complete_among_present = numeric(), n_incomplete_among_present = numeric(), stringsAsFactors = FALSE)
le_empty_measurement_dist <- function() data.frame(variable_index = integer(), variable = character(), n_usable_measurements = integer(), n_entities = numeric(), stringsAsFactors = FALSE)
le_empty_interior_missing <- function() data.frame(variable_index = integer(), variable = character(), n_entities_interior_missing = numeric(), stringsAsFactors = FALSE)

le_empty_change <- function() {
  data.frame(
    variable_index = integer(), variable = character(), n_entities_with_presence = numeric(), n_excluded_single_timepoint = numeric(), n_present_both = numeric(), n_eligible = numeric(),
    n_excluded_missing = numeric(), n_excluded_conflict = numeric(),
    n_excluded_nonfinite = numeric(), delta_n = numeric(), mean = numeric(), sd = numeric(), min = numeric(), q1 = numeric(), median = numeric(), q3 = numeric(), max = numeric(), iqr = numeric(), status = character(), reason = character(), stringsAsFactors = FALSE
  )
}
le_empty_first_to_last <- le_empty_change
le_empty_adjacent_change <- function() {
  metrics <- le_empty_change()[, setdiff(names(le_empty_change()), c(
    "n_entities_with_presence", "n_excluded_single_timepoint"
  )), drop = FALSE]
  data.frame(
    from_time_index = integer(), from_timepoint = character(),
    to_time_index = integer(), to_timepoint = character(), metrics,
    stringsAsFactors = FALSE
  )
}

le_empty_issues <- function() {
  data.frame(
    issue_code = character(), severity = character(), time_index = integer(),
    timepoint = character(), variable_index = integer(), variable = character(),
    n_affected = numeric(), message = character(), stringsAsFactors = FALSE
  )
}

le_sort_issues <- function(issues) {
  if (nrow(issues) == 0L) return(issues)
  code_order <- c(
    "missing_entity_id", "blank_entity_id", "missing_time",
    "duplicate_id_time", "conflicting_variable_cell",
    "zero_observation_timepoint"
  )
  code_index <- match(issues$issue_code, code_order)
  if (anyNA(code_index)) {
    stop("Longitudinal EDA produced an unsupported issue code.", call. = FALSE)
  }
  issues <- issues[order(code_index, issues$time_index, issues$variable_index), , drop = FALSE]
  rownames(issues) <- NULL
  issues
}

le_postgres <- function(inputs) {
  source <- inputs$data
  strata <- eda_pg_stratifier_contract(source, inputs$spec, inputs$time)
  eda_postgres_transaction(
    source,
    {
      context <- le_pg_context(inputs)
      le_pg_validate_times(context)
      custom <- le_pg_components(context)
      summaries <- eda_pg_stratified_inside(
        source, inputs$summary_spec, strata$strata_row, strata$column,
        strata$missing, TRUE, TRUE
      )
      structure(
        list(
          metadata = le_metadata(inputs, "postgresql"),
          structure = custom$structure,
          followup = custom$followup,
          timepoints = custom$timepoints,
          missingness = custom$missingness,
          summaries = summaries,
          change = custom$change,
          issues = custom$issues
        ),
        class = c("epi_eda_longitudinal", "list")
      )
    }
  )
}

le_pg_context <- function(inputs) {
  source <- inputs$data
  id_column <- eda_postgres_column(source, inputs$id)
  id_family <- sec_identifier_family(as.character(id_column$base_udt_name[[1L]]))
  time_column <- eda_postgres_column(source, inputs$time)
  time_contract <- eda_postgres_missing_contract(
    source, time_column,
    inputs$spec$analysis_type[match(inputs$time, inputs$spec$name)],
    eda_missing_codes(inputs$spec, inputs$time)
  )
  time_missing <- eda_pg_inline_query_params(
    source$con, time_contract$sql, time_contract$params
  )
  time_expression <- eda_postgres_value_expression(
    source, time_column,
    inputs$spec$analysis_type[match(inputs$time, inputs$spec$name)]
  )
  time_case <- paste0(
    "CASE ", paste(vapply(seq_along(inputs$levels), function(index) {
      paste0(
        "WHEN ", time_expression, " = ",
        as.character(DBI::dbQuoteLiteral(source$con, inputs$levels[[index]])),
        "::text THEN ", index, "::integer"
      )
    }, character(1)), collapse = " "), " END"
  )
  id_sql <- eda_postgres_column_sql(source, inputs$id)
  id_missing <- paste0(id_sql, " IS NULL")
  id_blank <- if (id_family == "text") {
    paste0(id_sql, " IS NOT NULL AND btrim(", id_sql, "::text) = ''")
  } else {
    "FALSE"
  }
  id_valid <- longitudinal_entity_predicate(source, inputs$id, id_family)
  inputs$id_family <- id_family
  inputs$id_value_sql <- longitudinal_qc_entity_sql(source, inputs$id)
  inputs$id_missing_sql <- id_missing
  inputs$id_blank_sql <- id_blank
  inputs$id_valid_sql <- id_valid
  inputs$time_missing_sql <- time_missing
  inputs$time_expression_sql <- time_expression
  inputs$time_case_sql <- time_case
  inputs$table_sql <- eda_postgres_table_sql(source)
  inputs
}

le_pg_validate_times <- function(context) {
  query <- paste0(
    "SELECT EXISTS (SELECT 1 FROM ", context$table_sql,
    " WHERE NOT (", context$time_missing_sql, ") AND (",
    context$time_case_sql, ") IS NULL) AS has_unreviewed_time"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_time_validation",
    limit = 1L
  )
  if (nrow(observed) != 1L || !"has_unreviewed_time" %in% names(observed)) {
    stop("PostgreSQL longitudinal EDA time validation was incomplete.", call. = FALSE)
  }
  if (isTRUE(observed$has_unreviewed_time[[1L]])) {
    stop("Observed non-missing time values must belong to the reviewed time levels.", call. = FALSE)
  }
  invisible(TRUE)
}

le_pg_base_cte <- function(context) {
  paste0(
    "base AS (SELECT ", context$id_value_sql, " AS entity_value, ",
    context$time_case_sql, " AS time_index, ", context$id_missing_sql,
    " AS id_missing, (", context$id_blank_sql, ") AS id_blank, (",
    context$time_missing_sql, ") AS time_missing FROM ", context$table_sql,
    "), valid AS (SELECT entity_value, time_index FROM base WHERE NOT id_missing ",
    "AND NOT id_blank AND NOT time_missing), cells AS (SELECT entity_value, ",
    "time_index, COUNT(*)::bigint AS n_rows FROM valid GROUP BY entity_value, time_index)"
  )
}

le_pg_components <- function(context) {
  structure <- le_pg_structure(context)
  followup <- le_pg_followup(context, structure$n_valid_entities[[1L]])
  timepoints <- le_pg_timepoints(context, structure$n_valid_entities[[1L]])
  profiles <- le_pg_variable_profiles(context)
  change <- le_pg_change(context)
  issues <- le_pg_issues(context, structure, timepoints, profiles$conflicts)
  list(
    structure = structure, followup = followup, timepoints = timepoints,
    missingness = profiles$missingness, change = change, issues = issues
  )
}

le_pg_fetch_counts <- function(observed, fields, prefix = "longitudinal EDA") {
  if (nrow(observed) != 1L || any(!fields %in% names(observed))) {
    stop("PostgreSQL longitudinal EDA returned incomplete aggregates.", call. = FALSE)
  }
  values <- vapply(fields, function(field) {
    longitudinal_qc_checked_count(observed[[field]][[1L]], paste(prefix, field))
  }, numeric(1))
  names(values) <- fields
  values
}

le_pg_structure <- function(context) {
  query <- paste0(
    "WITH ", le_pg_base_cte(context),
    ", entities AS (SELECT entity_value, COUNT(*)::bigint AS n_times FROM cells GROUP BY entity_value), duplicates AS (SELECT entity_value, n_rows FROM cells WHERE n_rows > 1) ",
    "SELECT (SELECT COUNT(*)::text FROM base) AS n_rows, ",
    "(SELECT COUNT(*) FILTER (WHERE id_missing)::text FROM base) AS n_missing_id, ",
    "(SELECT COUNT(*) FILTER (WHERE id_blank)::text FROM base) AS n_blank_id, ",
    "(SELECT COUNT(*) FILTER (WHERE time_missing)::text FROM base) AS n_missing_time, ",
    "(SELECT COUNT(*) FILTER (WHERE (id_missing OR id_blank) AND time_missing)::text FROM base) AS n_invalid_id_and_missing_time, ",
    "(SELECT COUNT(*)::text FROM valid) AS n_valid_panel_rows, ",
    "(SELECT COUNT(*)::text FROM entities) AS n_valid_entities, ",
    "(SELECT COUNT(DISTINCT time_index)::text FROM cells) AS n_observed_timepoints, ",
    "(SELECT COUNT(*)::text FROM cells) AS n_observed_id_time_cells, ",
    "(SELECT COUNT(*)::text FROM duplicates) AS n_duplicate_cells, ",
    "(SELECT COALESCE(SUM(n_rows - 1), 0)::text FROM duplicates) AS n_duplicate_excess, ",
    "(SELECT COALESCE(MAX(n_rows), 0)::text FROM cells) AS max_rows_per_cell, ",
    "(SELECT COUNT(DISTINCT entity_value)::text FROM duplicates) AS n_entities_with_duplicate_cell, ",
    "(SELECT COUNT(*)::text FROM entities WHERE n_times = ", length(context$levels),
    ") AS n_complete_entities"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_structure", limit = 1L
  )
  counts <- le_pg_fetch_counts(observed, c(
    "n_rows", "n_missing_id", "n_blank_id", "n_missing_time", "n_invalid_id_and_missing_time", "n_valid_panel_rows", "n_valid_entities", "n_observed_timepoints", "n_observed_id_time_cells", "n_duplicate_cells", "n_duplicate_excess", "max_rows_per_cell", "n_entities_with_duplicate_cell", "n_complete_entities"
  ))
  possible <- counts[["n_valid_entities"]] * length(context$levels)
  if (!is.finite(possible) || possible > 9007199254740991) {
    stop("longitudinal EDA n_possible_entity_time_cells exceeds the exact base-R double count range.", call. = FALSE)
  }
  valid <- counts[["n_observed_id_time_cells"]] <= possible && counts[["n_complete_entities"]] <= counts[["n_valid_entities"]] && counts[["n_duplicate_excess"]] == counts[["n_valid_panel_rows"]] - counts[["n_observed_id_time_cells"]]
  if (!isTRUE(valid)) {
    stop("PostgreSQL longitudinal EDA structure counts did not reconcile.", call. = FALSE)
  }
  data.frame(
    n_rows = counts[["n_rows"]],
    n_missing_id = counts[["n_missing_id"]], n_blank_id = counts[["n_blank_id"]], n_missing_time = counts[["n_missing_time"]], n_invalid_id_and_missing_time = counts[["n_invalid_id_and_missing_time"]],
    n_valid_panel_rows = counts[["n_valid_panel_rows"]],
    n_valid_entities = counts[["n_valid_entities"]], n_declared_timepoints = as.numeric(length(context$levels)), n_observed_timepoints = counts[["n_observed_timepoints"]], n_observed_id_time_cells = counts[["n_observed_id_time_cells"]], n_duplicate_cells = counts[["n_duplicate_cells"]], n_duplicate_excess = counts[["n_duplicate_excess"]], max_rows_per_cell = counts[["max_rows_per_cell"]], n_entities_with_duplicate_cell = counts[["n_entities_with_duplicate_cell"]], n_expected_cells = possible,
    n_complete_entities = counts[["n_complete_entities"]],
    n_incomplete_entities = counts[["n_valid_entities"]] - counts[["n_complete_entities"]],
    stringsAsFactors = FALSE
  )
}

le_pg_followup <- function(context, denominator) {
  query <- paste0(
    "WITH ", le_pg_base_cte(context),
    ", entities AS (SELECT entity_value, COUNT(*)::integer AS n_times FROM cells GROUP BY entity_value), ",
    "levels AS (SELECT generate_series(1, ", length(context$levels),
    ")::integer AS n_timepoints_observed) SELECT levels.n_timepoints_observed, ",
    "COUNT(entities.entity_value)::text AS n_entities FROM levels LEFT JOIN entities ",
    "ON entities.n_times = levels.n_timepoints_observed GROUP BY levels.n_timepoints_observed ",
    "ORDER BY levels.n_timepoints_observed"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_followup",
    limit = length(context$levels)
  )
  if (nrow(observed) != length(context$levels) ||
        !identical(as.integer(observed$n_timepoints_observed), seq_along(context$levels))) {
    stop("PostgreSQL longitudinal EDA follow-up aggregates were incomplete.", call. = FALSE)
  }
  counts <- vapply(
    observed$n_entities, longitudinal_qc_checked_count, numeric(1),
    field = "longitudinal EDA follow-up count"
  )
  if (sum(counts) != denominator) {
    stop("PostgreSQL longitudinal EDA follow-up counts did not reconcile.", call. = FALSE)
  }
  observation_count <- data.frame(
    n_timepoints_observed = as.integer(seq_along(context$levels)),
    n_entities = counts,
    stringsAsFactors = FALSE
  )
  bounds_query <- paste0("WITH ", le_pg_base_cte(context), ", entities AS (SELECT entity_value, MIN(time_index) AS first_time, MAX(time_index) AS last_time, COUNT(*) AS n_times FROM cells GROUP BY entity_value), levels AS (SELECT generate_series(1, ", length(context$levels), ")::integer AS time_index) SELECT levels.time_index, COUNT(*) FILTER (WHERE entities.first_time = levels.time_index)::text AS n_first, COUNT(*) FILTER (WHERE entities.last_time = levels.time_index)::text AS n_last FROM levels LEFT JOIN entities ON TRUE GROUP BY levels.time_index ORDER BY levels.time_index")
  bounds <- eda_db_fetch(context$data$con, bounds_query, query_kind = "longitudinal_eda_followup_bounds", limit = length(context$levels))
  gaps_query <- paste0("WITH ", le_pg_base_cte(context), ", entities AS (SELECT entity_value, MIN(time_index) AS first_time, MAX(time_index) AS last_time FROM cells GROUP BY entity_value), gaps AS (SELECT entities.entity_value, EXISTS (SELECT 1 FROM generate_series(entities.first_time + 1, entities.last_time - 1) AS t(time_index) LEFT JOIN cells ON cells.entity_value = entities.entity_value AND cells.time_index = t.time_index WHERE cells.entity_value IS NULL) AS has_gap FROM entities) SELECT COUNT(*) FILTER (WHERE NOT has_gap)::text AS no_gap, COUNT(*) FILTER (WHERE has_gap)::text AS has_gap FROM gaps")
  gap <- eda_db_fetch(context$data$con, gaps_query, query_kind = "longitudinal_eda_followup_gaps", limit = 1L)
  list(observation_count = observation_count, first_observation = data.frame(time_index = as.integer(bounds$time_index), timepoint = context$levels, n_entities = unname(vapply(bounds$n_first, longitudinal_qc_checked_count, numeric(1), field = "first observation")), stringsAsFactors = FALSE), last_observation = data.frame(time_index = as.integer(bounds$time_index), timepoint = context$levels, n_entities = unname(vapply(bounds$n_last, longitudinal_qc_checked_count, numeric(1), field = "last observation")), stringsAsFactors = FALSE), gap_status = data.frame(has_gap = c(FALSE, TRUE), n_entities = c(longitudinal_qc_checked_count(gap$no_gap[[1L]], "no gap"), longitudinal_qc_checked_count(gap$has_gap[[1L]], "has gap")), stringsAsFactors = FALSE))
}

le_pg_timepoints <- function(context, denominator) {
  query <- paste0(
    "WITH ", le_pg_base_cte(context),
    ", levels AS (SELECT generate_series(1, ", length(context$levels),
    ")::integer AS time_index), time_counts AS (SELECT time_index, ",
    "SUM(n_rows)::text AS n_rows, COUNT(*)::text AS n_entities FROM cells GROUP BY time_index), bounds AS (SELECT entity_value, MIN(time_index) AS first_time, MAX(time_index) AS last_time FROM cells GROUP BY entity_value), ",
    "retained AS (SELECT right_cells.time_index, COUNT(*)::text AS n_retained FROM cells left_cells ",
    "INNER JOIN cells right_cells ON right_cells.entity_value = left_cells.entity_value ",
    "AND right_cells.time_index = left_cells.time_index + 1 GROUP BY right_cells.time_index) ",
    "SELECT levels.time_index, COALESCE(time_counts.n_rows, '0') AS n_rows, ",
    "COALESCE(time_counts.n_entities, '0') AS n_entities, ",
    "COALESCE(retained.n_retained, '0') AS n_retained, (SELECT COUNT(*)::text FROM bounds WHERE first_time = levels.time_index) AS n_first, (SELECT COUNT(*)::text FROM bounds WHERE last_time = levels.time_index) AS n_last FROM levels LEFT JOIN time_counts USING (time_index) ",
    "LEFT JOIN retained USING (time_index) ORDER BY levels.time_index"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_timepoints",
    limit = length(context$levels)
  )
  if (nrow(observed) != length(context$levels) ||
        !identical(as.integer(observed$time_index), seq_along(context$levels))) {
    stop("PostgreSQL longitudinal EDA time-point aggregates were incomplete.", call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(observed)), function(index) {
    counts <- le_pg_fetch_counts(
      observed[index, , drop = FALSE], c("n_rows", "n_entities", "n_retained", "n_first", "n_last")
    )
    prior <- if (index == 1L) NA_real_ else
      longitudinal_qc_checked_count(observed$n_entities[[index - 1L]], "timepoint prior count")
    data.frame(
      time_index = as.integer(index), timepoint = context$levels[[index]],
      n_rows = counts[["n_rows"]], n_entities = counts[["n_entities"]],
      n_first_observed = counts[["n_first"]], n_last_observed = counts[["n_last"]],
      n_retained = if (index == 1L) NA_real_ else counts[["n_retained"]], n_not_present_previous = if (index == 1L) NA_real_ else counts[["n_entities"]] - counts[["n_retained"]],
      retention_numerator = if (index == 1L) NA_real_ else counts[["n_retained"]],
      retention_denominator = prior,
      p_retained = if (index == 1L) NA_real_ else
        longitudinal_qc_proportion(counts[["n_retained"]], prior),
      presence_numerator = if (index == 1L) NA_real_ else counts[["n_entities"]] - counts[["n_retained"]], presence_denominator = if (index == 1L) NA_real_ else counts[["n_entities"]], p_not_present_previous = if (index == 1L) NA_real_ else longitudinal_qc_proportion(counts[["n_entities"]] - counts[["n_retained"]], counts[["n_entities"]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

le_pg_variable_cte <- function(context, variable_index) {
  row <- context$selected[variable_index, , drop = FALSE]
  column <- eda_postgres_column(context$data, row$name[[1L]])
  contract <- eda_postgres_missing_contract(
    context$data, column, row$analysis_type[[1L]],
    eda_missing_codes(context$spec, row$name[[1L]])
  )
  missing <- eda_pg_inline_query_params(
    context$data$con, contract$sql, contract$params
  )
  value <- eda_postgres_value_expression(
    context$data, column, row$analysis_type[[1L]]
  )
  paste0(
    "classified AS (SELECT ", context$id_value_sql, " AS entity_value, ",
    context$time_case_sql, " AS time_index, (", missing, ") AS missing, ",
    value, " AS value FROM ", context$table_sql, " WHERE ", context$id_valid_sql,
    " AND NOT (", context$time_missing_sql, ")), states AS (SELECT entity_value, ",
    "time_index, LEAST(COUNT(DISTINCT value) FILTER (WHERE NOT missing), 2)::integer AS n_states, ",
    "CASE WHEN COUNT(DISTINCT value) FILTER (WHERE NOT missing) = 1 THEN ",
    "MIN(value) FILTER (WHERE NOT missing) END AS value FROM classified GROUP BY entity_value, time_index)"
  )
}

le_pg_variable_profiles <- function(context) {
  rows <- list()
  entity_rows <- list()
  distribution_rows <- list()
  interior_rows <- list()
  conflicts <- list()
  for (variable_index in seq_along(context$variables)) {
    query <- paste0(
      "WITH ", le_pg_variable_cte(context, variable_index),
      ", levels AS (SELECT generate_series(1, ", length(context$levels),
      ")::integer AS time_index), totals AS (SELECT time_index, COUNT(*)::text AS n_cells, ",
      "COUNT(*) FILTER (WHERE n_states = 1)::text AS n_usable, ",
      "COUNT(*) FILTER (WHERE n_states = 0)::text AS n_missing, ",
      "COUNT(*) FILTER (WHERE n_states > 1)::text AS n_conflicting FROM states GROUP BY time_index) ",
      "SELECT levels.time_index, COALESCE(n_cells, '0') AS n_cells, ",
      "COALESCE(n_usable, '0') AS n_usable, COALESCE(n_missing, '0') AS n_missing, ",
      "COALESCE(n_conflicting, '0') AS n_conflicting FROM levels LEFT JOIN totals USING (time_index) ",
      "ORDER BY levels.time_index"
    )
    observed <- eda_db_fetch(
      context$data$con, query, query_kind = "longitudinal_eda_missingness",
      limit = length(context$levels), variable_index = variable_index,
      name = context$variables[[variable_index]]
    )
    if (nrow(observed) != length(context$levels)) {
      stop("PostgreSQL longitudinal EDA missingness aggregates were incomplete.", call. = FALSE)
    }
    for (time_index in seq_along(context$levels)) {
      counts <- le_pg_fetch_counts(
        observed[time_index, , drop = FALSE],
        c("n_cells", "n_usable", "n_missing", "n_conflicting")
      )
      if (sum(counts[c("n_usable", "n_missing", "n_conflicting")]) != counts[["n_cells"]]) {
        stop("PostgreSQL longitudinal variable-cell states did not reconcile.", call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        variable_index = as.integer(variable_index), variable = context$variables[[variable_index]], time_index = as.integer(time_index), timepoint = context$levels[[time_index]],
        n_present_entities = counts[["n_cells"]], n_usable = counts[["n_usable"]],
        n_missing = counts[["n_missing"]], n_conflicting = counts[["n_conflicting"]],
        usable_numerator = counts[["n_usable"]], usable_denominator = counts[["n_cells"]], p_usable = longitudinal_qc_proportion(counts[["n_usable"]], counts[["n_cells"]]), missing_numerator = counts[["n_missing"]], missing_denominator = counts[["n_cells"]], p_missing = longitudinal_qc_proportion(counts[["n_missing"]], counts[["n_cells"]]), conflicting_numerator = counts[["n_conflicting"]], conflicting_denominator = counts[["n_cells"]], p_conflicting = longitudinal_qc_proportion(counts[["n_conflicting"]], counts[["n_cells"]]), stringsAsFactors = FALSE
      )
      conflicts[[length(conflicts) + 1L]] <- c(
        time_index = time_index, variable_index = variable_index,
        n = counts[["n_conflicting"]]
      )
    }
    entity_query <- paste0(
      "WITH ", le_pg_variable_cte(context, variable_index),
      ", per_entity AS (SELECT entity_value, COUNT(*)::bigint AS n_present, ",
      "COUNT(*) FILTER (WHERE n_states = 1)::bigint AS n_usable FROM states ",
      "GROUP BY entity_value) SELECT COUNT(*)::text AS n_valid_entities, ",
      "COUNT(*) FILTER (WHERE n_usable = 0)::text AS n_never_observed, ",
      "COUNT(*) FILTER (WHERE n_usable > 0)::text AS n_observed_at_least_once, ",
      "COUNT(*) FILTER (WHERE n_present > 0 AND n_usable = n_present)::text AS n_complete_among_present, ",
      "COUNT(*) FILTER (WHERE n_present > 0 AND n_usable <> n_present)::text AS n_incomplete_among_present ",
      "FROM per_entity"
    )
    entity_observed <- eda_db_fetch(
      context$data$con, entity_query, query_kind = "longitudinal_eda_entity_summary",
      limit = 1L, variable_index = variable_index, name = context$variables[[variable_index]]
    )
    entity_counts <- le_pg_fetch_counts(entity_observed, c(
      "n_valid_entities", "n_never_observed", "n_observed_at_least_once",
      "n_complete_among_present", "n_incomplete_among_present"
    ))
    if (entity_counts[["n_never_observed"]] + entity_counts[["n_observed_at_least_once"]] != entity_counts[["n_valid_entities"]] ||
          entity_counts[["n_complete_among_present"]] + entity_counts[["n_incomplete_among_present"]] != entity_counts[["n_valid_entities"]]) {
      stop("PostgreSQL longitudinal entity completeness counts did not reconcile.", call. = FALSE)
    }
    entity_rows[[length(entity_rows) + 1L]] <- data.frame(
      variable_index = as.integer(variable_index), variable = context$variables[[variable_index]],
      as.list(entity_counts), stringsAsFactors = FALSE
    )
    distribution_query <- paste0(
      "WITH ", le_pg_variable_cte(context, variable_index),
      ", per_entity AS (SELECT entity_value, COUNT(*) FILTER (WHERE n_states = 1)::integer AS n_usable FROM states GROUP BY entity_value), ",
      "levels AS (SELECT generate_series(0, ", length(context$levels), ")::integer AS n_usable) ",
      "SELECT levels.n_usable AS n_usable_measurements, COUNT(per_entity.entity_value)::text AS n_entities ",
      "FROM levels LEFT JOIN per_entity USING (n_usable) GROUP BY levels.n_usable ORDER BY levels.n_usable"
    )
    distribution_observed <- eda_db_fetch(
      context$data$con, distribution_query,
      query_kind = "longitudinal_eda_usable_measurement_distribution",
      limit = length(context$levels) + 1L, variable_index = variable_index,
      name = context$variables[[variable_index]]
    )
    if (nrow(distribution_observed) != length(context$levels) + 1L ||
          !identical(as.integer(distribution_observed$n_usable_measurements), 0:length(context$levels))) {
      stop("PostgreSQL longitudinal usable-measurement distribution was incomplete.", call. = FALSE)
    }
    distribution_counts <- vapply(distribution_observed$n_entities, longitudinal_qc_checked_count, numeric(1), field = "longitudinal EDA usable-measurement distribution")
    if (sum(distribution_counts) != entity_counts[["n_valid_entities"]]) {
      stop("PostgreSQL longitudinal usable-measurement distribution did not reconcile.", call. = FALSE)
    }
    distribution_rows[[length(distribution_rows) + 1L]] <- data.frame(
      variable_index = as.integer(variable_index), variable = context$variables[[variable_index]],
      n_usable_measurements = as.integer(distribution_observed$n_usable_measurements),
      n_entities = distribution_counts, stringsAsFactors = FALSE
    )
    interior_query <- paste0(
      "WITH ", le_pg_variable_cte(context, variable_index),
      ", per_entity AS (SELECT entity_value, MIN(time_index) FILTER (WHERE n_states = 1) AS first_usable, ",
      "MAX(time_index) FILTER (WHERE n_states = 1) AS last_usable, ",
      "BOOL_OR(n_states = 0) AS has_missing FROM states GROUP BY entity_value), ",
      "interior AS (SELECT states.entity_value, BOOL_OR(states.n_states = 0 AND states.time_index > per_entity.first_usable AND states.time_index < per_entity.last_usable) AS has_interior_missing ",
      "FROM states INNER JOIN per_entity USING (entity_value) GROUP BY states.entity_value) ",
      "SELECT COUNT(*) FILTER (WHERE has_interior_missing)::text AS n_entities_interior_missing FROM interior"
    )
    interior_observed <- eda_db_fetch(
      context$data$con, interior_query, query_kind = "longitudinal_eda_interior_missing",
      limit = 1L, variable_index = variable_index, name = context$variables[[variable_index]]
    )
    interior_rows[[length(interior_rows) + 1L]] <- data.frame(
      variable_index = as.integer(variable_index), variable = context$variables[[variable_index]],
      n_entities_interior_missing = le_pg_fetch_counts(interior_observed, "n_entities_interior_missing")[[1L]],
      stringsAsFactors = FALSE
    )
  }
  list(missingness = list(
    by_time = le_bind(rows, le_empty_missingness()),
    entity_summary = le_bind(entity_rows, le_empty_entity_summary()),
    usable_measurement_distribution = le_bind(distribution_rows, le_empty_measurement_dist()),
    interior_missing = le_bind(interior_rows, le_empty_interior_missing())
  ), conflicts = conflicts)
}

le_pg_change <- function(context) {
  variables <- which(context$selected$analysis_type %in% c("numeric", "integer"))
  adjacent <- list()
  first_to_last <- list()
  for (variable_index in variables) {
    for (left in seq_len(max(length(context$levels) - 1L, 0L))) {
      adjacent[[length(adjacent) + 1L]] <- le_pg_change_pair(
        context, variable_index, "adjacent", left, left + 1L
      )
    }
    first_to_last[[length(first_to_last) + 1L]] <- le_pg_change_pair(
      context, variable_index, "first_to_last", NA_integer_, NA_integer_
    )
  }
  list(
    first_to_last = le_bind(first_to_last, le_empty_first_to_last()),
    adjacent = le_bind(adjacent, le_empty_adjacent_change())
  )
}

le_pg_change_pair <- function(context, variable_index, comparison, left, right) {
  pair_cte <- if (comparison == "adjacent") {
    paste0(
      "pairs AS (SELECT left_state.entity_value, left_state.n_states AS left_states, ",
      "right_state.n_states AS right_states, left_state.value AS left_value, ",
      "right_state.value AS right_value FROM states left_state INNER JOIN states right_state ",
      "ON right_state.entity_value = left_state.entity_value WHERE left_state.time_index = ",
      left, " AND right_state.time_index = ", right, "), singles AS (SELECT 0::bigint AS n)"
    )
  } else {
    paste0(
      "bounds AS (SELECT entity_value, MIN(time_index) AS first_time, MAX(time_index) AS last_time, ",
      "COUNT(*) AS n_times FROM states GROUP BY entity_value), pairs AS (SELECT bounds.entity_value, ",
      "left_state.n_states AS left_states, right_state.n_states AS right_states, ",
      "left_state.value AS left_value, right_state.value AS right_value FROM bounds ",
      "INNER JOIN states left_state ON left_state.entity_value = bounds.entity_value AND ",
      "left_state.time_index = bounds.first_time INNER JOIN states right_state ON ",
      "right_state.entity_value = bounds.entity_value AND right_state.time_index = bounds.last_time ",
      "WHERE bounds.n_times > 1), singles AS (SELECT COUNT(*)::bigint AS n FROM bounds WHERE n_times = 1)"
    )
  }
  query <- paste0(
    "WITH ", le_pg_variable_cte(context, variable_index), ", ", pair_cte,
    ", change_classified AS (SELECT *, CASE WHEN left_states > 1 OR right_states > 1 THEN 'conflict' ",
    "WHEN left_states = 0 OR right_states = 0 THEN 'missing' WHEN left_value::text IN ",
    "('NaN', 'Infinity', '-Infinity') OR right_value::text IN ",
    "('NaN', 'Infinity', '-Infinity') OR (right_value - left_value)::text IN ",
    "('NaN', 'Infinity', '-Infinity') THEN 'non_finite' ",
    "ELSE 'eligible' END AS class FROM pairs), ",
    "changes AS (SELECT right_value - left_value AS change FROM change_classified WHERE class = 'eligible') ",
    "SELECT (SELECT COUNT(*)::text FROM pairs) AS n_candidates, ",
    "(SELECT n::text FROM singles) AS n_single, ",
    "COUNT(*)::text AS n_eligible, ",
    "(SELECT COUNT(*)::text FROM change_classified WHERE class = 'missing') AS n_missing, ",
    "(SELECT COUNT(*)::text FROM change_classified WHERE class = 'conflict') AS n_conflict, ",
    "(SELECT COUNT(*)::text FROM change_classified WHERE class = 'non_finite') AS n_non_finite, ",
    "COUNT(*) FILTER (WHERE change < 0)::text AS n_decrease, ",
    "COUNT(*) FILTER (WHERE change = 0)::text AS n_no_change, ",
    "COUNT(*) FILTER (WHERE change > 0)::text AS n_increase, AVG(change) AS mean_change, ",
    "STDDEV_SAMP(change) AS sd_change, MIN(change) AS min_change, ",
    "percentile_cont(0.25) WITHIN GROUP (ORDER BY change) AS q1_change, ",
    "percentile_cont(0.5) WITHIN GROUP (ORDER BY change) AS median_change, ",
    "percentile_cont(0.75) WITHIN GROUP (ORDER BY change) AS q3_change, ",
    "MAX(change) AS max_change FROM changes"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_change", limit = 1L,
    variable_index = variable_index, name = context$variables[[variable_index]]
  )
  fields <- c(
    "n_candidates", "n_single", "n_eligible", "n_missing", "n_conflict",
    "n_non_finite", "n_decrease", "n_no_change", "n_increase"
  )
  counts <- le_pg_fetch_counts(observed, fields)
  reconciled <- counts[["n_eligible"]] + counts[["n_missing"]] +
    counts[["n_conflict"]] + counts[["n_non_finite"]] == counts[["n_candidates"]] &&
    sum(counts[c("n_decrease", "n_no_change", "n_increase")]) == counts[["n_eligible"]]
  if (!isTRUE(reconciled)) {
    stop("PostgreSQL longitudinal change counts did not reconcile.", call. = FALSE)
  }
  number <- function(field) {
    value <- observed[[field]][[1L]]
    if (is.na(value)) NA_real_ else as.numeric(value)
  }
  row <- data.frame(
    variable_index = as.integer(variable_index), variable = context$variables[[variable_index]],
    n_entities_with_presence = counts[["n_candidates"]] + counts[["n_single"]],
    n_excluded_single_timepoint = counts[["n_single"]],
    n_present_both = counts[["n_candidates"]], n_eligible = counts[["n_eligible"]],
    n_excluded_missing = counts[["n_missing"]], n_excluded_conflict = counts[["n_conflict"]],
    n_excluded_nonfinite = counts[["n_non_finite"]], delta_n = counts[["n_eligible"]],
    mean = number("mean_change"), sd = number("sd_change"), min = number("min_change"),
    q1 = number("q1_change"), median = number("median_change"), q3 = number("q3_change"),
    max = number("max_change"), iqr = number("q3_change") - number("q1_change"),
    status = "available", reason = if (counts[["n_eligible"]] == 0) "zero_eligible" else NA_character_,
    stringsAsFactors = FALSE
  )
  if (comparison == "adjacent") {
    row <- row[, setdiff(names(row), c(
      "n_entities_with_presence", "n_excluded_single_timepoint"
    )), drop = FALSE]
    return(data.frame(
      from_time_index = as.integer(left), from_timepoint = context$levels[[left]],
      to_time_index = as.integer(right), to_timepoint = context$levels[[right]],
      row, stringsAsFactors = FALSE
    ))
  }
  row
}

le_pg_issues <- function(context, structure, timepoints, conflicts) {
  query <- paste0(
    "WITH ", le_pg_base_cte(context), " SELECT ",
    "COUNT(*) FILTER (WHERE id_missing)::text AS n_missing_id, ",
    "COUNT(*) FILTER (WHERE id_blank)::text AS n_blank_id, ",
    "COUNT(*) FILTER (WHERE time_missing)::text AS n_missing_time FROM base"
  )
  observed <- eda_db_fetch(
    context$data$con, query, query_kind = "longitudinal_eda_issues", limit = 1L
  )
  counts <- le_pg_fetch_counts(observed, c("n_missing_id", "n_blank_id", "n_missing_time"))
  rows <- list()
  rows <- le_add_issue(
    rows, "missing_entity_id", n = counts[["n_missing_id"]], inputs = context
  )
  rows <- le_add_issue(
    rows, "blank_entity_id", n = counts[["n_blank_id"]], inputs = context
  )
  rows <- le_add_issue(
    rows, "missing_time", n = counts[["n_missing_time"]], inputs = context
  )
  rows <- le_add_issue(
    rows, "duplicate_id_time",
    n = structure$n_duplicate_cells[[1L]], inputs = context
  )
  for (value in conflicts) {
    rows <- le_add_issue(
      rows, "conflicting_variable_cell", as.integer(value[["time_index"]]),
      as.integer(value[["variable_index"]]), value[["n"]], context
    )
  }
  for (time_index in seq_along(context$levels)) {
    if (timepoints$n_entities[[time_index]] == 0) {
      rows <- le_add_issue(
        rows, "zero_observation_timepoint", time_index, NA_integer_, 0,
        context, allow_zero = TRUE
      )
    }
  }
  le_sort_issues(le_bind(rows, le_empty_issues()))
}
