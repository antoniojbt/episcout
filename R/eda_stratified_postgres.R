eda_pg_profile_stratified <- function(source,
                                      spec,
                                      strata,
                                      include_overall,
                                      include_missing_stratum) {
  spec <- epi_eda_spec(spec)
  stratified_validate_flag(include_overall, "include_overall")
  stratified_validate_flag(
    include_missing_stratum,
    "include_missing_stratum"
  )
  if (!is.character(strata) || length(strata) != 1L || is.na(strata) ||
        !nzchar(strata)) {
    stop(
      "strata must be a single non-missing character variable name.",
      call. = FALSE
    )
  }
  eda_validate_postgres_source(source, require_idle = TRUE)
  contract <- eda_pg_stratifier_contract(source, spec, strata)

  eda_postgres_transaction(
    source,
    eda_pg_stratified_inside(
      source,
      spec,
      contract$strata_row,
      contract$column,
      contract$missing,
      include_overall,
      include_missing_stratum
    )
  )
}

eda_pg_stratifier_contract <- function(source, spec, strata) {
  if (!strata %in% spec$name) {
    stop(
      "The strata variable must be represented in the EDA specification.",
      call. = FALSE
    )
  }
  column <- eda_postgres_column(source, strata)
  if (is.null(column)) {
    stop("The strata variable must be present in data.", call. = FALSE)
  }
  strata_row <- spec[match(strata, spec$name), , drop = FALSE]
  if (!strata_row$analysis_type[[1]] %in% c("categorical", "binary")) {
    stop(
      "The strata variable must be declared categorical or binary.",
      call. = FALSE
    )
  }
  declared <- prepare_declared_levels(strata_row)
  if (!declared$safe) {
    stop(
      "Strata levels must be unique and safely represented by the semicolon contract.",
      call. = FALSE
    )
  }
  family <- eda_postgres_storage_family(column)
  if (strata_row$analysis_type[[1]] == "binary" &&
        !(length(declared$levels) == 2L ||
            (length(declared$levels) == 0L && family == "boolean"))) {
    stop(
      "Binary strata require exactly two declared levels or logical storage.",
      call. = FALSE
    )
  }
  compatibility <- eda_pg_type_compatibility(
    column,
    strata_row$analysis_type[[1]],
    declared$levels
  )
  if (!compatibility$status %in% c("compatible", "coercible")) {
    stop(
      "The strata variable requires specification-guided preparation before grouping.",
      call. = FALSE
    )
  }
  missing <- eda_postgres_missing_contract(
    source,
    column,
    strata_row$analysis_type[[1]],
    eda_missing_codes(spec, strata)
  )
  if (!missing$valid) {
    stop(
      "The strata missing-value contract is incompatible with PostgreSQL storage.",
      call. = FALSE
    )
  }

  list(strata_row = strata_row, column = column, missing = missing)
}

eda_pg_stratified_inside <- function(source,
                                     spec,
                                     strata_row,
                                     strata_column,
                                     missing_contract,
                                     include_overall,
                                     include_missing_stratum,
                                     timing_env = NULL) {
  group_profile <- eda_pg_strata_groups(
    source,
    strata_row,
    strata_column,
    missing_contract,
    include_overall,
    include_missing_stratum,
    timing_env
  )
  groups <- group_profile$groups
  included_source <- eda_pg_filtered_source(
    source,
    group_profile$included_filter
  )
  included_summary <- eda_postgres_summaries_inside(
    included_source,
    spec,
    timing_env,
    n_total = group_profile$n_included,
    allow_value_vectors = FALSE
  )
  universes <- eda_pg_stratified_universes(included_summary, spec)
  extras <- setdiff(source$columns$name, spec$name)
  components <- lapply(seq_len(nrow(groups)), function(index) {
    group <- groups[index, , drop = FALSE]
    canonical <- if (group$is_overall[[1]]) {
      included_summary
    } else {
      filtered <- eda_pg_filtered_source(
        source,
        group_profile$filters[[group$group_id[[1]]]]
      )
      eda_postgres_summaries_inside(
        filtered,
        spec,
        timing_env,
        n_total = group$n[[1]],
        allow_value_vectors = FALSE
      )
    }
    eda_pg_stratified_component(
      canonical,
      source,
      spec,
      group,
      universes,
      extras
    )
  })
  output <- stratified_bind_components(components)
  metadata <- data.frame(
    strata = strata_row$name[[1]],
    strata_label = stratified_label(
      strata_row$label[[1]],
      strata_row$name[[1]]
    ),
    include_overall = include_overall,
    include_missing_stratum = include_missing_stratum,
    n_input = group_profile$n_input,
    n_included = group_profile$n_included,
    n_omitted_missing_stratum = group_profile$n_omitted,
    n_strata = as.integer(sum(!groups$is_overall)),
    summary_contract = "canonical-1",
    stratified_contract = "stratified-1",
    source_contract = "postgres-source-1",
    normality_contract = "not_calculated_no_analysis_value_collection",
    stringsAsFactors = FALSE
  )
  result <- structure(
    c(list(groups = groups), output, list(metadata = metadata)),
    class = c("epi_eda_stratified", "list")
  )
  eda_pg_reconcile_stratified(result)
  result
}

eda_pg_strata_groups <- function(source,
                                 strata_row,
                                 column,
                                 missing_contract,
                                 include_overall,
                                 include_missing,
                                 timing_env = NULL) {
  expression <- eda_postgres_value_expression(
    source,
    column,
    strata_row$analysis_type[[1]]
  )
  observed <- eda_db_fetch(
    source$con,
    paste0(
      "WITH v AS (SELECT ", expression, " AS value, ",
      missing_contract$sql, " AS missing FROM ",
      eda_postgres_table_sql(source), ") ",
      "SELECT CASE WHEN missing THEN NULL ELSE value END AS level, ",
      "missing, count(*)::text AS n FROM v ",
      "GROUP BY CASE WHEN missing THEN NULL ELSE value END, missing ",
      "ORDER BY missing, level"
    ),
    params = missing_contract$params,
    query_kind = "stratified_group_counts",
    limit = Inf,
    timing_env = timing_env,
    name = column$name[[1]]
  )
  if (nrow(observed) > 0L &&
        (!is.logical(observed$missing) || anyNA(observed$missing))) {
    stop(
      "PostgreSQL stratified group aggregates have an invalid schema.",
      call. = FALSE
    )
  }
  counts <- if (nrow(observed) == 0L) {
    integer()
  } else {
    vapply(
      observed$n,
      eda_checked_count,
      integer(1),
      field = "PostgreSQL stratified group count"
    )
  }
  missing_n <- sum(counts[observed$missing])
  ordinary <- observed[!observed$missing, , drop = FALSE]
  ordinary_counts <- counts[!observed$missing]
  names(ordinary_counts) <- as.character(ordinary$level)
  declared <- if ("levels" %in% names(strata_row)) {
    eda_spec_levels(strata_row$levels)
  } else {
    character()
  }
  if (length(declared) == 0L &&
        eda_postgres_storage_family(column) == "boolean") {
    declared <- c("FALSE", "TRUE")
  }
  unexpected <- sort(
    setdiff(names(ordinary_counts), declared),
    method = "radix"
  )
  levels <- c(declared, unexpected)
  level_counts <- unname(ordinary_counts[levels])
  level_counts[is.na(level_counts)] <- 0L
  n_input <- as.integer(sum(counts))
  n_included <- as.integer(
    if (include_missing) n_input else n_input - missing_n
  )
  rows <- list()
  filters <- list()
  missing_sql <- eda_pg_inline_query_params(
    source$con,
    missing_contract$sql,
    missing_contract$params
  )
  included_filter <- if (include_missing) {
    DBI::SQL("TRUE")
  } else {
    DBI::SQL(paste0("NOT (", missing_sql, ")"))
  }
  if (include_overall) {
    rows[[length(rows) + 1L]] <- stratified_group_row(
      ".overall", NA_character_, "Overall", TRUE, FALSE, FALSE, FALSE,
      n_included
    )
    filters[[".overall"]] <- included_filter
  }
  for (index in seq_along(levels)) {
    value <- levels[[index]]
    id <- sprintf(".stratum.%03d", index)
    rows[[length(rows) + 1L]] <- stratified_group_row(
      id,
      value,
      value,
      FALSE,
      FALSE,
      value %in% unexpected,
      value %in% declared,
      level_counts[[index]]
    )
    literal <- as.character(DBI::dbQuoteLiteral(source$con, value))
    filters[[id]] <- DBI::SQL(paste0(
      "NOT (", missing_sql, ") AND ", expression, " = ", literal, "::text"
    ))
  }
  if (include_missing && missing_n > 0L) {
    rows[[length(rows) + 1L]] <- stratified_group_row(
      ".missing", NA_character_, "Missing", FALSE, TRUE, FALSE, FALSE,
      missing_n
    )
    filters[[".missing"]] <- DBI::SQL(missing_sql)
  }
  if (length(rows) == 0L) {
    groups <- stratified_group_row(
      character(), character(), character(), logical(), logical(), logical(),
      logical(), integer()
    )
  } else {
    groups <- do.call(rbind, rows)
    groups$group_order <- seq_len(nrow(groups))
    groups <- groups[c(
      "group_id", "group_order", "group_value", "group_label", "is_overall",
      "is_missing_stratum", "is_unexpected_stratum", "is_declared_stratum",
      "n"
    )]
  }
  list(
    groups = groups,
    filters = filters,
    included_filter = included_filter,
    n_input = n_input,
    n_included = n_included,
    n_omitted = as.integer(n_input - n_included)
  )
}

eda_pg_inline_query_params <- function(con, sql, params) {
  if (length(params) == 0L) {
    return(as.character(sql))
  }
  interpolated <- as.character(sql)
  for (index in rev(seq_along(params))) {
    interpolated <- sub(
      paste0("\\$", index, "(?![0-9])"),
      paste0("?p", index),
      interpolated,
      perl = TRUE
    )
  }
  values <- stats::setNames(params, paste0("p", seq_along(params)))
  as.character(DBI::sqlInterpolate(
    con,
    interpolated,
    .dots = values
  ))
}

eda_pg_filtered_source <- function(source, filter) {
  if (!inherits(filter, "SQL") || length(filter) != 1L || is.na(filter)) {
    stop("Internal PostgreSQL row filter is invalid.", call. = FALSE)
  }
  filtered <- source
  attr(filtered, "eda_row_filter_sql") <- filter
  filtered
}

eda_pg_stratified_universes <- function(summary, spec) {
  names <- spec$name[spec$analysis_type %in% c("categorical", "binary")]
  out <- stats::setNames(vector("list", length(names)), names)
  for (name in names) {
    out[[name]] <- summary$categorical[
      summary$categorical$name == name,
      c("level", "n", "p_total", "p_observed", "is_declared", "is_unexpected"),
      drop = FALSE
    ]
  }
  out
}

eda_pg_stratified_component <- function(canonical,
                                        source,
                                        spec,
                                        group,
                                        universes,
                                        extras) {
  for (name in extras) {
    canonical$skipped <- rbind(
      canonical$skipped,
      canonical_skipped_row(
        name,
        NA_character_,
        as.character(
          eda_postgres_column(source, name)$formatted_type[[1]]
        ),
        "Observed data variable is not declared in the EDA specification."
      )
    )
  }
  variables <- stratified_prefix(canonical$variables, group)
  list(
    variables = variables,
    numeric = stratified_numeric(
      canonical$numeric,
      canonical$variables,
      group,
      spec
    ),
    categorical = stratified_categorical(
      canonical$categorical,
      canonical$variables,
      group,
      spec,
      universes
    ),
    text = stratified_prefix(canonical$text, group),
    temporal = stratified_prefix(canonical$temporal, group),
    skipped = stratified_prefix(canonical$skipped, group)
  )
}

eda_pg_reconcile_stratified <- function(result) {
  groups <- result$groups
  metadata <- result$metadata
  non_overall <- groups[!groups$is_overall, , drop = FALSE]
  overall <- groups[groups$is_overall, , drop = FALSE]
  valid <- sum(as.numeric(non_overall$n)) == metadata$n_included[[1]] &&
    nrow(overall) <= 1L &&
    (nrow(overall) == 0L || overall$n[[1]] == metadata$n_included[[1]]) &&
    metadata$n_input[[1]] == metadata$n_included[[1]] +
      metadata$n_omitted_missing_stratum[[1]]
  if (!isTRUE(valid)) {
    stop("PostgreSQL stratified group totals did not reconcile.", call. = FALSE)
  }
  for (group_id in groups$group_id) {
    variables <- result$variables[
      result$variables$group_id == group_id & !is.na(result$variables$n), ,
      drop = FALSE
    ]
    group_n <- groups$n[match(group_id, groups$group_id)]
    if (nrow(variables) > 0L &&
          (any(variables$n != group_n) ||
             any(variables$n_missing + variables$n_observed != variables$n))) {
      stop(
        "PostgreSQL stratified variable totals did not reconcile.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}
