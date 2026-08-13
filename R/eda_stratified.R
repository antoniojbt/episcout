#' Profile specification-aware stratified summaries
#'
#' Calculate canonical descriptive summaries overall and across exactly one
#' categorical or binary specification variable. Results are long-form,
#' machine-readable, and retain declared empty, unexpected, and missing strata.
#'
#' @param data A data frame containing observed or prepared data, or an
#'   [epi_eda_postgres_source()].
#' @param spec An EDA specification accepted by [epi_eda_spec()].
#' @param strata A single character name of a categorical or binary variable.
#' @param include_overall Include an Overall group calculated directly from the
#'   included analysis population.
#' @param include_missing_stratum Retain standard and declared-sentinel missing
#'   strata as an explicit group. When false, metadata records omitted rows and
#'   Overall describes only retained rows.
#'
#' @return An `epi_eda_stratified` list with `groups`, `variables`, `numeric`,
#'   `categorical`, `text`, `temporal`, `skipped`, and `metadata` data frames.
#'   Text output contains aggregate diagnostics only. No files are written.
#'
#' @details PostgreSQL sources are profiled inside one read-only repeatable-read
#'   transaction. Queries return grouped or scalar aggregates only. Shapiro-Wilk
#'   p-values are `NA` for PostgreSQL stratified numeric summaries because that
#'   calculation would require collecting an analysis-value vector; all other
#'   supported canonical numeric fields remain aggregate queries.
#'
#' @export
epi_eda_profile_stratified <- function(data,
                                       spec,
                                       strata,
                                       include_overall = TRUE,
                                       include_missing_stratum = TRUE) {
  if (inherits(data, "epi_eda_postgres_source")) {
    return(eda_pg_profile_stratified(
      data,
      spec,
      strata,
      include_overall,
      include_missing_stratum
    ))
  }
  stratified_validate_data(data)
  spec <- epi_eda_spec(spec)
  stratified_validate_flag(include_overall, "include_overall")
  stratified_validate_flag(include_missing_stratum, "include_missing_stratum")
  if (!is.character(strata) || length(strata) != 1L || is.na(strata) || !nzchar(strata)) {
    stop("strata must be a single non-missing character variable name.", call. = FALSE)
  }
  if (!strata %in% spec$name) {
    stop("The strata variable must be represented in the EDA specification.", call. = FALSE)
  }
  if (!strata %in% names(data)) {
    stop("The strata variable must be present in data.", call. = FALSE)
  }
  strata_row <- spec[match(strata, spec$name), , drop = FALSE]
  if (!strata_row$type[[1]] %in% c("categorical", "binary")) {
    stop("The strata variable must be declared categorical or binary.", call. = FALSE)
  }
  strata_values <- data[[strata]]
  if (!is.atomic(strata_values) || inherits(strata_values, c("Date", "POSIXt")) || !is.null(dim(strata_values))) {
    stop("The strata variable requires specification-guided preparation before grouping.", call. = FALSE)
  }
  level_contract <- prepare_declared_levels(strata_row)
  if (!level_contract$safe) {
    stop("Strata levels must be unique and safely represented by the semicolon contract.", call. = FALSE)
  }
  if (strata_row$type[[1]] == "binary" &&
        !(length(level_contract$levels) == 2L ||
            (length(level_contract$levels) == 0L && is.logical(strata_values)))) {
    stop("Binary strata require exactly two declared levels or logical storage.", call. = FALSE)
  }

  spec_codes <- eda_missing_codes(spec, strata)
  strata_missing <- summary_missing_mask(strata_values, spec_codes)
  included <- if (include_missing_stratum) rep(TRUE, nrow(data)) else !strata_missing
  groups <- stratified_groups(
    strata_values, strata_missing, included, strata_row,
    include_overall, include_missing_stratum
  )
  exclusions <- stratified_exclusions(data, spec)
  universes <- stratified_level_universes(
    data[included, , drop = FALSE], spec, exclusions
  )
  components <- lapply(seq_len(nrow(groups)), function(index) {
    mask <- stratified_group_mask(groups[index, , drop = FALSE], strata_values, strata_missing, included)
    stratified_summarise_group(
      data[mask, , drop = FALSE], spec, groups[index, , drop = FALSE],
      universes, exclusions, setdiff(names(data), spec$name)
    )
  })
  output <- stratified_bind_components(components)
  strata_label <- stratified_label(strata_row$label[[1]], strata)
  metadata <- data.frame(
    strata = strata,
    strata_label = strata_label,
    include_overall = include_overall,
    include_missing_stratum = include_missing_stratum,
    n_input = as.integer(nrow(data)),
    n_included = as.integer(sum(included)),
    n_omitted_missing_stratum = as.integer(sum(!included)),
    n_strata = as.integer(sum(!groups$is_overall)),
    summary_contract = "canonical-1",
    stratified_contract = "stratified-1",
    stringsAsFactors = FALSE
  )
  structure(
    c(list(groups = groups), output, list(metadata = metadata)),
    class = c("epi_eda_stratified", "list")
  )
}

stratified_validate_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  names <- names(data)
  if (any(is.na(names) | trimws(names) == "")) {
    stop("Data variable names must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(names)) {
    stop("Duplicate data variable names are not supported.", call. = FALSE)
  }
  invisible(TRUE)
}

stratified_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

stratified_groups <- function(values,
                              missing,
                              included,
                              strata_row,
                              include_overall,
                              include_missing) {
  declared <- if ("levels" %in% names(strata_row)) {
    eda_spec_levels(strata_row$levels)
  } else {
    character()
  }
  observed <- as.character(values[included & !missing])
  if (length(declared) == 0L && is.logical(values)) {
    declared <- c("FALSE", "TRUE")
  }
  unexpected <- sort(setdiff(unique(observed), declared), method = "radix")
  levels <- c(declared, unexpected)
  rows <- list()
  if (include_overall) {
    rows[[length(rows) + 1L]] <- stratified_group_row(
      ".overall", NA_character_, "Overall", TRUE, FALSE, FALSE, FALSE,
      sum(included)
    )
  }
  for (index in seq_along(levels)) {
    value <- levels[[index]]
    rows[[length(rows) + 1L]] <- stratified_group_row(
      sprintf(".stratum.%03d", index), value, value, FALSE, FALSE,
      value %in% unexpected, value %in% declared,
      sum(included & !missing & as.character(values) == value, na.rm = TRUE)
    )
  }
  if (include_missing && any(missing)) {
    rows[[length(rows) + 1L]] <- stratified_group_row(
      ".missing", NA_character_, "Missing", FALSE, TRUE, FALSE, FALSE,
      sum(missing)
    )
  }
  if (length(rows) == 0L) {
    return(stratified_group_row(
      character(), character(), character(), logical(), logical(), logical(),
      logical(), integer()
    ))
  }
  out <- do.call(rbind, rows)
  out$group_order <- seq_len(nrow(out))
  out[c(
    "group_id", "group_order", "group_value", "group_label", "is_overall",
    "is_missing_stratum", "is_unexpected_stratum", "is_declared_stratum", "n"
  )]
}

stratified_group_row <- function(id, value, label, overall, missing, unexpected, declared, n) {
  data.frame(
    group_id = as.character(id),
    group_order = integer(length(id)),
    group_value = as.character(value),
    group_label = as.character(label),
    is_overall = as.logical(overall),
    is_missing_stratum = as.logical(missing),
    is_unexpected_stratum = as.logical(unexpected),
    is_declared_stratum = as.logical(declared),
    n = as.integer(n),
    stringsAsFactors = FALSE
  )
}

stratified_group_mask <- function(group, values, missing, included) {
  if (group$is_overall[[1]]) {
    return(included)
  }
  if (group$is_missing_stratum[[1]]) {
    return(missing)
  }
  included & !missing & as.character(values) == group$group_value[[1]]
}

stratified_level_universes <- function(data, spec, exclusions) {
  names <- spec$name[
    spec$type %in% c("categorical", "binary") &
      spec$name %in% names(data) & !spec$name %in% names(exclusions)
  ]
  out <- stats::setNames(vector("list", length(names)), names)
  for (name in names) {
    row <- spec[match(name, spec$name), , drop = FALSE]
    declared <- if ("levels" %in% names(row)) eda_spec_levels(row$levels) else character()
    if (length(declared) == 0L && is.logical(data[[name]])) {
      declared <- c("FALSE", "TRUE")
    }
    codes <- eda_missing_codes(spec, name)
    core <- summary_categorical_core(
      data[[name]], codes, if (length(declared) > 0L) declared else NULL
    )
    out[[name]] <- core
  }
  out
}

stratified_exclusions <- function(data, spec) {
  reasons <- stats::setNames(character(), character())
  schema <- epi_eda_check_schema(data, spec)
  incompatible <- schema$name[schema$expected_present & schema$observed_present & schema$type_status == "incompatible"]
  reasons[incompatible] <- "Observed storage is incompatible; run epi_eda_prepare() before stratified summaries."
  for (index in which(spec$type == "datetime" & spec$name %in% names(data))) {
    name <- spec$name[[index]]
    values <- data[[name]]
    if (is.character(values)) {
      missing <- summary_missing_mask(values, eda_missing_codes(spec, name))
      shapes <- prepare_datetime_shapes(values[!missing])
      if (any(shapes$local)) {
        reasons[name] <- "Local character datetimes require epi_eda_prepare() before stratified summaries."
      }
    }
  }
  reasons
}

stratified_summarise_group <- function(data, spec, group, universes, exclusions, extras) {
  canonical <- build_typed_summaries(data, spec)
  canonical <- eda_apply_summary_exclusions(
    canonical, data, spec, exclusions
  )
  for (name in extras) {
    canonical$skipped <- rbind(canonical$skipped, canonical_skipped_row(
      name, NA_character_, paste(class(data[[name]]), collapse = "/"),
      "Observed data variable is not declared in the EDA specification."
    ))
  }

  variables <- stratified_prefix(canonical$variables, group)
  numeric <- stratified_numeric(canonical$numeric, canonical$variables, group, spec)
  categorical <- stratified_categorical(
    canonical$categorical, canonical$variables, group, spec, universes
  )
  list(
    variables = variables,
    numeric = numeric,
    categorical = categorical,
    text = stratified_prefix(canonical$text, group),
    temporal = stratified_prefix(canonical$temporal, group),
    skipped = stratified_prefix(canonical$skipped, group)
  )
}

stratified_numeric <- function(numeric, variables, group, spec) {
  if (nrow(numeric) == 0L) {
    template <- cbind(
      data.frame(
        name = character(), type = character(), n = integer(),
        n_missing = integer(), n_observed = integer(), n_infinite = integer()
      ),
      empty_eda_numeric()[setdiff(names(empty_eda_numeric()), "name")]
    )
    return(stratified_prefix(template, group))
  }
  index <- match(numeric$name, variables$name)
  out <- cbind(
    data.frame(
      name = numeric$name,
      type = spec$type[match(numeric$name, spec$name)],
      n = variables$n[index],
      n_missing = variables$n_missing[index],
      n_observed = variables$n_observed[index],
      n_infinite = variables$n_infinite[index],
      stringsAsFactors = FALSE
    ),
    numeric[setdiff(names(numeric), "name")]
  )
  stratified_prefix(out, group)
}

stratified_categorical <- function(categorical, variables, group, spec, universes) {
  rows <- list()
  names <- intersect(
    names(universes),
    variables$name[variables$status == "summarised"]
  )
  for (name in names) {
    universe <- universes[[name]]
    source <- categorical[categorical$name == name, , drop = FALSE]
    index <- match(universe$level, source$level)
    counts <- source$n[index]
    counts[is.na(counts)] <- 0L
    variable <- variables[variables$name == name, , drop = FALSE]
    n_total <- variable$n[[1]]
    n_observed <- variable$n_observed[[1]]
    rows[[length(rows) + 1L]] <- data.frame(
      name = name,
      type = spec$type[match(name, spec$name)],
      level = universe$level,
      n = as.integer(counts),
      n_total = rep(as.integer(n_total), nrow(universe)),
      n_observed = rep(as.integer(n_observed), nrow(universe)),
      p_total = summary_safe_proportion(counts, n_total),
      p_observed = summary_safe_proportion(counts, n_observed),
      is_declared = universe$is_declared,
      is_unexpected = universe$is_unexpected,
      is_missing_level = FALSE,
      stringsAsFactors = FALSE
    )
    missing_n <- variable$n_missing[[1]]
    rows[[length(rows) + 1L]] <- data.frame(
      name = name, type = spec$type[match(name, spec$name)],
      level = NA_character_, n = as.integer(missing_n),
      n_total = as.integer(n_total), n_observed = as.integer(n_observed),
      p_total = summary_safe_proportion(missing_n, n_total),
      p_observed = NA_real_, is_declared = FALSE, is_unexpected = FALSE,
      is_missing_level = TRUE, stringsAsFactors = FALSE
    )
  }
  template <- data.frame(
    name = character(), type = character(), level = character(), n = integer(),
    n_total = integer(), n_observed = integer(), p_total = numeric(),
    p_observed = numeric(), is_declared = logical(), is_unexpected = logical(),
    is_missing_level = logical(), stringsAsFactors = FALSE
  )
  stratified_prefix(if (length(rows) == 0L) template else do.call(rbind, rows), group)
}

stratified_prefix <- function(data, group) {
  prefix_names <- c(
    "group_id", "group_order", "group_value", "group_label", "is_overall",
    "is_missing_stratum", "is_unexpected_stratum"
  )
  prefix <- group[rep(1L, nrow(data)), prefix_names, drop = FALSE]
  row.names(prefix) <- NULL
  cbind(prefix, data)
}

stratified_bind_components <- function(components) {
  names <- c("variables", "numeric", "categorical", "text", "temporal", "skipped")
  if (length(components) == 0L) {
    group <- stratified_group_row(
      ".empty", NA_character_, "", FALSE, FALSE, FALSE, FALSE, 0L
    )
    empty <- build_typed_summaries(data.frame(), epi_eda_validate_spec(data.frame(
      name = character(), label = character(), type = character(), role = character()
    )))
    return(list(
      variables = stratified_prefix(empty$variables, group)[0, ],
      numeric = stratified_numeric(empty$numeric, empty$variables, group, data.frame(name = character(), type = character()))[0, ],
      categorical = stratified_categorical(empty$categorical, empty$variables, group, data.frame(name = character(), type = character()), list())[0, ],
      text = stratified_prefix(empty$text, group)[0, ],
      temporal = stratified_prefix(empty$temporal, group)[0, ],
      skipped = stratified_prefix(empty$skipped, group)[0, ]
    ))
  }
  stats::setNames(lapply(names, function(name) {
    out <- do.call(rbind, lapply(components, `[[`, name))
    row.names(out) <- NULL
    out
  }), names)
}

stratified_label <- function(label, name) {
  if (length(label) == 0L || is.na(label[[1]]) ||
        !nzchar(trimws(as.character(label[[1]])))) {
    return(as.character(name))
  }
  as.character(label[[1]])
}
