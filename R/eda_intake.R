#' Run a data intake-to-report workflow
#'
#' Compose semantic specification scaffolding, preparation, canonical summaries,
#' optional stratified summaries, and a report bundle. Invalid function
#' arguments and unsafe output collisions are errors; factual incompatibilities
#' return a blocked result.
#'
#' @param data An in-memory data frame. Use [epi_read()] before this function for
#'   supported delimited files.
#' @param spec `NULL`, an EDA specification data frame, or a CSV path accepted
#'   by [epi_eda_spec()]. `NULL` generates, saves, and uses a lean semantic
#'   dictionary based on storage-derived types.
#' @param output_dir Directory for the workflow-owned report bundle. It is
#'   created when absent.
#' @param prepare One of `"none"`, `"audit"`, or `"apply"`. `"none"` proceeds
#'   only when the audit shows that no preparation is required.
#' @param strata Optional single categorical or binary variable name for
#'   [epi_eda_profile_stratified()] and [epi_eda_table1()].
#' @param render Whether to create `report.html` from the saved CSV artifacts.
#' @param overwrite Whether known workflow-owned files in the exact target
#'   directory may be replaced. Unrelated files are never removed.
#' @param source_id Optional non-sensitive source identifier. Absolute paths are
#'   rejected and no source identifier is inferred from the environment.
#' @param maps Whether to create one geometry-only point map for every declared
#'   coordinate pair.
#' @param map_vars Unique declared variables for additional thematic maps.
#' @param max_map_points Inclusive maximum number of rows allowed for mapping.
#'
#' @return An `epi_eda_intake` list with fixed components `status`, `stage`,
#'   `output_dir`, `manifest`, `input`, `spec`, `schema_before`, `schema_after`,
#'   `preparation_audit`, `missing`, `geo`, `maps`, `map_inventory`, `summary`,
#'   `stratified`, `table1`, `report`, `messages`, and `metadata`.
#'   Status is one of `blocked`, `audit_complete`, or `complete`. Manifest paths
#'   and the report path are relative to `output_dir`.
#'
#' @details The bundle never writes a source or prepared row-level table, row
#'   previews, raw free-text examples, identifier values, or pseudonymisation
#'   bridge tables. Explicitly requested maps can represent individual point
#'   locations and thematic values. episcout creates the outputs explicitly
#'   requested by the analyst and does not decide whether they may be shared.
#'
#' @export
epi_eda_intake_run <- function(data,
                               spec = NULL,
                               output_dir,
                               prepare = c("none", "audit", "apply"),
                               strata = NULL,
                               render = TRUE,
                               overwrite = FALSE,
                               source_id = NULL,
                               maps = FALSE,
                               map_vars = character(),
                               max_map_points = 10000L) {
  prepare <- match.arg(prepare)
  map_values <- eda_map_option_values(maps, map_vars, max_map_points)
  intake_validate_data(data)
  strata <- intake_validate_strata(strata)
  render <- intake_validate_flag(render, "render")
  overwrite <- intake_validate_flag(overwrite, "overwrite")
  source_id <- intake_validate_source_id(source_id)
  intake_validate_spec_argument(spec)
  bundle <- intake_prepare_output_dir(output_dir, overwrite)
  on.exit({
    unpublished <- !exists("state", inherits = FALSE) || !isTRUE(state$published)
    if (unpublished && dir.exists(bundle$staging_dir)) {
      unlink(bundle$staging_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  state <- intake_state(bundle$staging_dir, bundle$output_dir)
  started_at <- intake_timestamp()
  input <- intake_metadata(
    data, source_id, prepare, strata, render, overwrite, started_at,
    map_values
  )
  messages <- intake_empty_messages()
  result <- intake_empty_result(bundle$output_dir, state, input, render)

  intake_write_csv(state, "intake_metadata", input)

  generated_spec <- is.null(spec)
  if (generated_spec) {
    parsed_spec <- tryCatch(epi_eda_spec_scaffold(data), error = identity)
    if (inherits(parsed_spec, "error")) {
      messages <- intake_add_message(
        messages, "intake", "blocker", "spec_scaffold",
        conditionMessage(parsed_spec),
        "Resolve the reported source structure before generating a specification scaffold."
      )
      result$status <- "blocked"
      result$stage <- "intake"
      result$spec <- intake_spec_state("invalid", NULL, "generated", "")
      return(intake_finish(result, state, input, messages, render))
    }
  } else {
    parsed_spec <- tryCatch(epi_eda_spec(spec), error = identity)
    if (inherits(parsed_spec, "error")) {
      messages <- intake_add_message(
        messages, "specification", "blocker", "specification",
        conditionMessage(parsed_spec),
        "Migrate the specification to the lean semantic schema and rerun the workflow."
      )
      result$status <- "blocked"
      result$stage <- "intake"
      result$spec <- intake_spec_state(
        "invalid", NULL, intake_spec_source(spec), "", intake_spec_source_name(spec)
      )
      return(intake_finish(result, state, input, messages, render))
    }
  }

  fingerprint <- intake_spec_fingerprint(parsed_spec)
  map_options <- eda_map_options(
    parsed_spec, maps, map_vars, max_map_points
  )
  eda_validate_map_columns(names(data), map_options)
  spec_state <- if (generated_spec) "generated" else "supplied"
  spec_source <- if (generated_spec) "generated" else intake_spec_source(spec)
  result$spec <- intake_spec_state(
    spec_state, parsed_spec, spec_source, fingerprint,
    if (generated_spec) NA_character_ else intake_spec_source_name(spec)
  )
  intake_write_csv(state, "specification", parsed_spec)

  audit_result <- tryCatch(
    epi_eda_prepare(data, parsed_spec, mode = "audit"),
    error = identity
  )
  if (inherits(audit_result, "error")) {
    messages <- intake_add_message(
      messages, "audit", "blocker", "preparation_audit",
      conditionMessage(audit_result),
      "Correct the data or specification before rerunning the audit."
    )
    result$status <- "blocked"
    result$stage <- "intake"
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }

  result$schema_before <- audit_result$schema_before
  result$preparation_audit <- audit_result$audit
  intake_write_csv(state, "schema_before", result$schema_before)
  intake_write_csv(state, "preparation_audit", result$preparation_audit)
  messages <- intake_audit_messages(messages, result$preparation_audit)
  result$stage <- "audit"

  has_blockers <- any(result$preparation_audit$status == "blocking")
  if (has_blockers) {
    result$status <- "blocked"
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }

  if (prepare == "audit") {
    result$status <- "audit_complete"
    messages <- intake_add_message(
      messages, "audit", "info", "workflow",
      "The preparation audit completed without blocking findings; data were not transformed.",
      "Review the audit and rerun with prepare = 'apply' or, when no changes are planned, prepare = 'none'."
    )
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }

  analysis_data <- data
  if (prepare == "none") {
    change_rows <- intake_preparation_change_rows(result$preparation_audit)
    if (length(change_rows) > 0L) {
      for (index in change_rows) {
        row <- result$preparation_audit[index, , drop = FALSE]
        messages <- intake_add_message(
          messages, "audit", "blocker", as.character(row$name[[1]]),
          paste0("Preparation is required: ", row$reason[[1]]),
          "Rerun with prepare = 'apply' after reviewing the preparation audit."
        )
      }
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
  } else {
    prepared <- tryCatch(
      epi_eda_prepare(data, parsed_spec, mode = "apply"),
      error = identity
    )
    if (inherits(prepared, "error") ||
          !identical(prepared$metadata$overall_status[[1]], "prepared")) {
      reason <- if (inherits(prepared, "error")) {
        conditionMessage(prepared)
      } else {
        "All-or-nothing preparation did not produce a prepared result."
      }
      messages <- intake_add_message(
        messages, "preparation", "blocker", "preparation",
        reason,
        "Resolve preparation findings and rerun; no prepared row-level data were written."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    analysis_data <- prepared$data
    result$schema_after <- prepared$schema_after
    result$preparation_audit <- prepared$audit
    intake_write_csv(state, "schema_after", result$schema_after)
    intake_write_csv(state, "preparation_audit", result$preparation_audit)
    result$stage <- "preparation"
  }

  analysis_frame <- as.data.frame(analysis_data, stringsAsFactors = FALSE)
  missing <- epi_eda_profile_missing(analysis_frame, parsed_spec)
  result$missing <- missing
  intake_write_csv(state, "missing", missing)
  geo <- tryCatch(
    epi_eda_profile_geo(analysis_frame, parsed_spec),
    error = identity
  )
  if (inherits(geo, "error")) {
    messages <- intake_add_message(
      messages, "analysis", "blocker", "geo_qa",
      conditionMessage(geo),
      "Resolve the declared coordinate-pair contract before rerunning the workflow."
    )
    result$status <- "blocked"
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }
  eda_geo_reconcile(geo, nrow(analysis_frame))
  result$geo <- geo
  intake_write_csv(state, "geo_qa", geo)
  map_result <- eda_data_frame_maps(
    analysis_frame, parsed_spec, geo, map_options
  )
  result$maps <- map_result$maps
  result$map_inventory <- map_result$map_inventory
  intake_write_csv(state, "map_inventory", result$map_inventory)
  eda_write_maps(
    result$maps, result$map_inventory, state$output_dir, "intake EDA"
  )
  intake_register_map_artifacts(state, result$map_inventory)
  summaries <- tryCatch(
    epi_eda_profile_summaries(analysis_frame, parsed_spec),
    error = identity
  )
  if (inherits(summaries, "error")) {
    messages <- intake_add_message(
      messages, "analysis", "blocker", "canonical_summary",
      conditionMessage(summaries),
      "Resolve the summary contract finding and rerun the workflow."
    )
    result$status <- "blocked"
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }
  reconciliation <- intake_reconcile_canonical(
    summaries, missing, analysis_frame, parsed_spec
  )
  if (!is.null(reconciliation)) {
    messages <- intake_add_message(
      messages, "analysis", "blocker", "canonical_summary",
      reconciliation,
      "Inspect the component counts before using or sharing the bundle."
    )
    result$status <- "blocked"
    return(intake_finish(result, state, input, messages, render, fingerprint))
  }
  result$summary <- summaries
  for (name in names(result$summary)) {
    intake_write_csv(state, paste0("summary_", name), result$summary[[name]])
  }
  result$stage <- "canonical_summary"

  if (!is.null(strata)) {
    stratified <- tryCatch(
      epi_eda_profile_stratified(analysis_frame, parsed_spec, strata),
      error = identity
    )
    if (inherits(stratified, "error")) {
      messages <- intake_add_message(
        messages, "analysis", "blocker", strata,
        conditionMessage(stratified),
        "Choose one declared categorical or binary stratifier and rerun."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    reconciliation <- intake_reconcile_stratified(
      summaries, stratified, analysis_frame
    )
    if (!is.null(reconciliation)) {
      messages <- intake_add_message(
        messages, "analysis", "blocker", strata,
        reconciliation,
        "Inspect canonical and grouped counts before using or sharing the bundle."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    result$stratified <- stratified
    for (name in names(stratified)) {
      intake_write_csv(state, paste0("stratified_", name), stratified[[name]])
    }
    result$stage <- "stratified_summary"
    table1 <- tryCatch(epi_eda_table1(stratified), error = identity)
    if (inherits(table1, "error")) {
      messages <- intake_add_message(
        messages, "analysis", "blocker", "table1.csv",
        "Table 1 could not be created from the reconciled stratified summaries.",
        "Use the retained stratified machine components to verify the Table 1 contract before rerunning."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    result$table1 <- table1
    intake_write_csv(state, "table1", table1)
  }

  result$status <- "complete"
  intake_finish(result, state, input, messages, render, fingerprint)
}

intake_empty_result <- function(output_dir, state, input, render) {
  list(
    status = NA_character_,
    stage = NA_character_,
    output_dir = output_dir,
    manifest = state$manifest,
    input = input,
    spec = intake_spec_state("not_supplied", NULL, "none", ""),
    schema_before = NULL,
    schema_after = NULL,
    preparation_audit = NULL,
    missing = NULL,
    geo = NULL,
    maps = stats::setNames(vector("list", 0L), character()),
    map_inventory = eda_map_empty_inventory(),
    summary = NULL,
    stratified = NULL,
    table1 = NULL,
    report = list(
      requested = render,
      created = FALSE,
      path = NA_character_,
      reason = if (render) "Report rendering has not run." else "Report rendering was not requested."
    ),
    messages = intake_empty_messages(),
    metadata = NULL
  )
}

intake_finish <- function(result,
                          state,
                          input,
                          messages,
                          render,
                          fingerprint = "") {
  finished_at <- intake_timestamp()
  input <- intake_complete_metadata(
    input, result$status, result$stage, fingerprint, finished_at
  )
  input <- intake_complete_spec_metadata(input, result$spec)
  intake_write_csv(state, "intake_metadata", input)
  intake_write_csv(state, "messages", messages)

  if (render) {
    rendered <- tryCatch(
      intake_render_report(state$output_dir, state$manifest, result$status, result$stage),
      error = identity
    )
    if (inherits(rendered, "error")) {
      partial_report <- file.path(state$output_dir, "report.html")
      if (file.exists(partial_report) && !dir.exists(partial_report)) {
        unlink(partial_report, force = FALSE)
      }
      messages <- intake_add_message(
        messages, "report", "blocker", "report.html",
        "The HTML report could not be created from the saved workflow artifacts.",
        "Use the retained machine-readable artifacts and rerun rendering after resolving the report failure."
      )
      if (result$status == "complete") {
        result$status <- "blocked"
      }
      finished_at <- intake_timestamp()
      input <- intake_complete_metadata(
        input, result$status, result$stage, fingerprint, finished_at
      )
      input <- intake_complete_spec_metadata(input, result$spec)
      intake_write_csv(state, "intake_metadata", input)
      intake_write_csv(state, "messages", messages)
      result$report <- list(
        requested = TRUE,
        created = FALSE,
        path = NA_character_,
        reason = "The HTML report could not be created from the saved workflow artifacts."
      )
    } else {
      intake_register_existing(state, "report")
      result$report <- list(
        requested = TRUE,
        created = TRUE,
        path = "report.html",
        reason = "Report created from saved workflow artifacts."
      )
    }
  }

  result$manifest <- state$manifest
  result$input <- input
  result$messages <- messages
  result$metadata <- intake_run_metadata(input)
  intake_publish_bundle(state)
  result$output_dir <- state$target_dir
  structure(result, class = c("epi_eda_intake", "list"))
}

intake_validate_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be an in-memory data frame.", call. = FALSE)
  }
  names <- names(data)
  if (any(is.na(names) | trimws(names) == "")) {
    stop("data column names must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(names)) {
    stop("Duplicate data column names are not supported.", call. = FALSE)
  }
  if (any(startsWith(names, ".dataset."))) {
    stop("Data column names using the reserved .dataset. prefix are not supported.", call. = FALSE)
  }
  invisible(TRUE)
}

intake_validate_strata <- function(strata) {
  if (is.null(strata)) {
    return(NULL)
  }
  if (!is.character(strata) || length(strata) != 1L || is.na(strata) || !nzchar(strata)) {
    stop("strata must be NULL or one non-empty character variable name.", call. = FALSE)
  }
  strata
}

intake_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

intake_validate_source_id <- function(source_id) {
  if (is.null(source_id)) {
    return(NULL)
  }
  if (!is.character(source_id) || length(source_id) != 1L || is.na(source_id) ||
        !nzchar(trimws(source_id)) || grepl("[\r\n]", source_id)) {
    stop("source_id must be NULL or one non-empty character identifier.", call. = FALSE)
  }
  if (grepl("^/", source_id) || startsWith(source_id, "\\") ||
        grepl("^[A-Za-z]:[/\\\\]", source_id)) {
    stop("source_id must not be an absolute filesystem path.", call. = FALSE)
  }
  source_id
}

intake_validate_spec_argument <- function(spec) {
  if (is.null(spec) || is.data.frame(spec)) {
    return(invisible(TRUE))
  }
  if (!is.character(spec) || length(spec) != 1L || is.na(spec) || !nzchar(spec)) {
    stop("spec must be NULL, a data frame, or one local CSV path.", call. = FALSE)
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", spec)) {
    stop("spec must be a local CSV path; network URLs are not supported.", call. = FALSE)
  }
  if (!file.exists(spec) || dir.exists(spec) || !utils::file_test("-f", spec)) {
    stop("The supplied specification CSV path must exist and be a regular file.", call. = FALSE)
  }
  invisible(TRUE)
}

intake_prepare_output_dir <- function(output_dir, overwrite) {
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) ||
        !nzchar(trimws(output_dir))) {
    stop("output_dir must be one non-empty directory path.", call. = FALSE)
  }
  requested <- path.expand(output_dir)
  output_link <- Sys.readlink(requested)
  if (!is.na(output_link) && nzchar(output_link)) {
    stop("output_dir must not be a symbolic link.", call. = FALSE)
  }
  if (file.exists(requested) && !dir.exists(requested)) {
    stop("output_dir exists and is not a directory.", call. = FALSE)
  }
  parent <- dirname(requested)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("The parent of output_dir could not be created.", call. = FALSE)
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output_dir <- file.path(parent, basename(requested))
  if (identical(output_dir, "/")) {
    stop("output_dir must not be the filesystem root.", call. = FALSE)
  }
  if (dir.exists(output_dir)) {
    entries <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 0L && !overwrite) {
      stop("output_dir is non-empty; set overwrite = TRUE to replace workflow-owned artifacts.", call. = FALSE)
    }
    if (length(entries) > 0L && overwrite) {
      intake_validate_owned_bundle(output_dir, entries)
    }
  }
  staging_dir <- tempfile(
    paste0(".", basename(output_dir), "-staging-"), tmpdir = parent
  )
  if (!dir.create(staging_dir, showWarnings = FALSE)) {
    stop("A staging directory could not be created beside output_dir.", call. = FALSE)
  }
  list(
    output_dir = output_dir,
    staging_dir = normalizePath(staging_dir, winslash = "/", mustWork = TRUE)
  )
}

intake_validate_owned_bundle <- function(output_dir, entries) {
  registry <- intake_manifest_registry()
  manifest_path <- file.path(output_dir, "manifest.csv")
  if (!file.exists(manifest_path) || dir.exists(manifest_path)) {
    stop("overwrite = TRUE requires a valid prior episcout intake manifest in a non-empty output_dir.", call. = FALSE)
  }
  all_entries <- list.files(
    output_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE,
    include.dirs = TRUE, full.names = TRUE
  )
  if (any(nzchar(Sys.readlink(all_entries)))) {
    stop("A non-empty output_dir containing symbolic links cannot be overwritten safely.", call. = FALSE)
  }
  info <- file.info(all_entries)
  directories <- all_entries[!is.na(info$isdir) & info$isdir]
  relative_directories <- substring(
    directories, nchar(normalizePath(output_dir, winslash = "/")) + 2L
  )
  if (length(relative_directories) > 0L &&
        !all(relative_directories == "maps")) {
    stop("A non-empty output_dir contains an unowned directory and cannot be overwritten safely.", call. = FALSE)
  }
  files <- list.files(
    output_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE,
    include.dirs = FALSE
  )
  file_paths <- file.path(output_dir, files)
  if (!all(utils::file_test("-f", file_paths))) {
    stop("A non-empty output_dir containing non-regular files cannot be overwritten safely.", call. = FALSE)
  }
  prior <- tryCatch(
    utils::read.csv(
      manifest_path,
      check.names = FALSE, stringsAsFactors = FALSE,
      na.strings = character()
    ),
    error = identity
  )
  if (!inherits(prior, "error") && "sensitivity" %in% names(prior)) {
    stop(
      "The prior intake manifest uses the removed sensitivity schema; regenerate the bundle with the five-column core manifest before using overwrite = TRUE.",
      call. = FALSE
    )
  }
  static_index <- if (!inherits(prior, "error")) {
    match(registry$artifact, prior$artifact)
  } else {
    rep(NA_integer_, nrow(registry))
  }
  dynamic <- if (!inherits(prior, "error")) {
    !prior$artifact %in% registry$artifact
  } else {
    logical()
  }
  dynamic_valid <- length(dynamic) == 0L || all(
    prior$type[dynamic] == "map" &
      prior$status[dynamic] == "created" &
      grepl("^maps/map-p[0-9]{3,}-(geometry|v[0-9]{3,})\\.svg$", prior$path[dynamic]) &
      prior$artifact[dynamic] == sub(
        "\\.svg$", "", sub("^maps/", "", prior$path[dynamic])
      )
  )
  valid <- !inherits(prior, "error") &&
    identical(names(prior), names(registry)) &&
    !anyDuplicated(prior$artifact) && !anyDuplicated(prior$path) &&
    !anyNA(static_index) &&
    identical(as.character(prior$artifact[static_index]), as.character(registry$artifact)) &&
    identical(as.character(prior$type[static_index]), as.character(registry$type)) &&
    identical(as.character(prior$path[static_index]), as.character(registry$path)) &&
    dynamic_valid &&
    all(prior$status %in% c("created", "not_created")) &&
    identical(prior$status[prior$artifact == "manifest"], "created") &&
    identical(prior$checksum_md5[prior$artifact == "manifest"], "") &&
    all(prior$checksum_md5[prior$status == "not_created"] == "")
  if (!valid) {
    stop("overwrite = TRUE requires a valid prior episcout intake manifest.", call. = FALSE)
  }
  expected <- sort(as.character(prior$path[prior$status == "created"]))
  if (!identical(sort(files), expected)) {
    stop("output_dir contents do not match the prior intake manifest and cannot be overwritten safely.", call. = FALSE)
  }
  checked <- prior$artifact != "manifest" & prior$status == "created"
  prior_checksums <- as.character(prior$checksum_md5[checked])
  actual_checksums <- unname(tools::md5sum(file.path(
    output_dir, prior$path[checked]
  )))
  if (any(!nzchar(prior_checksums)) || !identical(prior_checksums, actual_checksums)) {
    stop("output_dir artifact checksums do not match the prior intake manifest.", call. = FALSE)
  }
  invisible(TRUE)
}

intake_manifest_registry <- function() {
  paths <- c(
    manifest = "manifest.csv",
    intake_metadata = "intake_metadata.csv",
    messages = "messages.csv",
    specification = "specification.csv",
    schema_before = "schema_before.csv",
    schema_after = "schema_after.csv",
    preparation_audit = "preparation_audit.csv",
    missing = "missing.csv",
    geo_qa = "geo_qa.csv",
    map_inventory = "map_inventory.csv",
    summary_variables = "summary_variables.csv",
    summary_numeric = "summary_numeric.csv",
    summary_categorical = "summary_categorical.csv",
    summary_text = "summary_text.csv",
    summary_temporal = "summary_temporal.csv",
    summary_skipped = "summary_skipped.csv",
    stratified_groups = "stratified_groups.csv",
    stratified_variables = "stratified_variables.csv",
    stratified_numeric = "stratified_numeric.csv",
    stratified_categorical = "stratified_categorical.csv",
    stratified_text = "stratified_text.csv",
    stratified_temporal = "stratified_temporal.csv",
    stratified_skipped = "stratified_skipped.csv",
    stratified_metadata = "stratified_metadata.csv",
    table1 = "table1.csv",
    report = "report.html"
  )
  types <- c(
    "manifest", "metadata", "messages", "specification", "schema", "schema",
    "audit", "missingness", "geo_qa", "map_inventory",
    rep("canonical_summary", 6L), rep("stratified_summary", 8L),
    "presentation", "report"
  )
  data.frame(
    artifact = names(paths),
    type = types,
    path = unname(paths),
    status = "not_created",
    checksum_md5 = "",
    stringsAsFactors = FALSE
  )
}

intake_state <- function(staging_dir, output_dir) {
  state <- new.env(parent = emptyenv())
  state$output_dir <- staging_dir
  state$staging_dir <- staging_dir
  state$target_dir <- output_dir
  state$published <- FALSE
  state$manifest <- intake_manifest_registry()
  state$manifest$status[state$manifest$artifact == "manifest"] <- "created"
  intake_refresh_manifest(state)
  state
}

intake_publish_bundle <- function(state) {
  target <- state$target_dir
  backup <- NA_character_
  if (dir.exists(target)) {
    backup <- tempfile(
      paste0(".", basename(target), "-backup-"), tmpdir = dirname(target)
    )
    if (!intake_rename(target, backup)) {
      stop("The existing output bundle could not be moved to a recovery backup.", call. = FALSE)
    }
  }
  if (!intake_rename(state$staging_dir, target)) {
    if (!is.na(backup) && dir.exists(backup)) {
      intake_rename(backup, target)
    }
    stop("The staged output bundle could not be finalised; the prior bundle was restored where possible.", call. = FALSE)
  }
  state$published <- TRUE
  state$output_dir <- target
  if (!is.na(backup) && dir.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(target)
}

intake_rename <- function(from, to) {
  file.rename(from, to)
}

intake_refresh_manifest <- function(state) {
  manifest <- state$manifest
  for (index in seq_len(nrow(manifest))) {
    if (manifest$artifact[[index]] == "manifest") {
      manifest$status[[index]] <- "created"
      manifest$checksum_md5[[index]] <- ""
      next
    }
    path <- file.path(state$output_dir, manifest$path[[index]])
    created <- file.exists(path) && !dir.exists(path)
    manifest$status[[index]] <- if (created) "created" else "not_created"
    manifest$checksum_md5[[index]] <- if (created) {
      unname(tools::md5sum(path)[[1]])
    } else {
      ""
    }
  }
  state$manifest <- manifest
  intake_atomic_csv(
    manifest, file.path(state$output_dir, "manifest.csv")
  )
  invisible(TRUE)
}

intake_write_csv <- function(state, artifact, value) {
  path <- intake_artifact_path(state, artifact)
  intake_atomic_csv(value, path)
  intake_refresh_manifest(state)
  invisible(path)
}

intake_write_text <- function(state, artifact, value) {
  path <- intake_artifact_path(state, artifact)
  intake_atomic_text(value, path)
  intake_refresh_manifest(state)
  invisible(path)
}

intake_register_existing <- function(state, artifact) {
  path <- intake_artifact_path(state, artifact)
  if (!file.exists(path) || dir.exists(path)) {
    stop("The expected workflow artifact was not created: ", artifact, ".", call. = FALSE)
  }
  intake_refresh_manifest(state)
  invisible(path)
}

intake_register_map_artifacts <- function(state, inventory) {
  created <- inventory[inventory$status == "created", , drop = FALSE]
  if (nrow(created) == 0L) {
    return(invisible(TRUE))
  }
  dynamic <- data.frame(
    artifact = created$map_id,
    type = "map",
    path = created$path,
    status = "created",
    checksum_md5 = "",
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(c(state$manifest$artifact, dynamic$artifact)) ||
        anyDuplicated(c(state$manifest$path, dynamic$path))) {
    stop("EDA map artifacts did not have unique deterministic identifiers and paths.", call. = FALSE)
  }
  state$manifest <- rbind(state$manifest, dynamic)
  intake_refresh_manifest(state)
  invisible(TRUE)
}

intake_artifact_path <- function(state, artifact) {
  index <- match(artifact, state$manifest$artifact)
  if (is.na(index)) {
    stop("Unknown workflow artifact: ", artifact, ".", call. = FALSE)
  }
  file.path(state$output_dir, state$manifest$path[[index]])
}

intake_atomic_csv <- function(value, path) {
  value <- as.data.frame(value, stringsAsFactors = FALSE)
  temporary <- tempfile(".episcout-write-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  intake_replace_file(temporary, path)
}

intake_atomic_text <- function(value, path) {
  temporary <- tempfile(".episcout-write-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(enc2utf8(value), temporary, useBytes = TRUE)
  intake_replace_file(temporary, path)
}

intake_replace_file <- function(temporary, path) {
  if (file.exists(path) && unlink(path, force = FALSE) != 0L) {
    stop("An existing workflow artifact could not be replaced safely.", call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop("A workflow artifact could not be finalised in output_dir.", call. = FALSE)
  }
  invisible(path)
}

intake_empty_messages <- function() {
  data.frame(
    stage = character(),
    severity = character(),
    subject = character(),
    reason = character(),
    recommended_action = character(),
    stringsAsFactors = FALSE
  )
}

intake_add_message <- function(messages,
                               stage,
                               severity,
                               subject,
                               reason,
                               recommended_action) {
  rbind(
    messages,
    data.frame(
      stage = stage,
      severity = severity,
      subject = subject,
      reason = reason,
      recommended_action = recommended_action,
      stringsAsFactors = FALSE
    )
  )
}

intake_audit_messages <- function(messages, audit) {
  rows <- which(audit$status %in% c("blocking", "warning"))
  for (index in rows) {
    row <- audit[index, , drop = FALSE]
    severity <- if (row$status[[1]] == "blocking") "blocker" else "warning"
    messages <- intake_add_message(
      messages, "audit", severity, as.character(row$name[[1]]),
      as.character(row$reason[[1]]),
      if (severity == "blocker") {
        "Resolve this finding in the source data or specification before preparation."
      } else {
        "Review this finding and document whether it is acceptable for the analysis."
      }
    )
  }
  messages
}

intake_preparation_change_rows <- function(audit) {
  which(
    audit$status == "planned" |
      (!is.na(audit$n_changed) & audit$n_changed > 0L)
  )
}

intake_spec_state <- function(state,
                              data,
                              source,
                              fingerprint,
                              source_name = NA_character_) {
  list(
    state = state,
    source = source,
    source_name = source_name,
    fingerprint_sha256 = fingerprint,
    data = data
  )
}

intake_spec_source <- function(spec) {
  if (is.character(spec) && length(spec) == 1L) "csv" else "data_frame"
}

intake_spec_source_name <- function(spec) {
  if (is.character(spec) && length(spec) == 1L) basename(spec) else NA_character_
}

intake_spec_fingerprint <- function(spec) {
  raw <- serialize(spec, connection = NULL, ascii = TRUE, version = 2L)
  as.character(openssl::sha256(raw))
}

intake_metadata <- function(data,
                            source_id,
                            prepare,
                            strata,
                            render,
                            overwrite,
                            started_at,
                            map_options) {
  rows <- data.frame(
    field = c(
      "workflow_contract", "n_rows", "n_columns", "source_id",
      "prepare", "strata", "include_missing_stratum", "render", "overwrite",
      "maps", "map_vars", "max_map_points",
      "spec_state", "spec_source", "spec_source_name", "package_version",
      "r_version", "dependency.openssl", "dependency.rmarkdown",
      "started_at_utc", "finished_at_utc", "status", "stage",
      "spec_fingerprint_sha256"
    ),
    value = c(
      "intake-1", as.character(nrow(data)), as.character(ncol(data)),
      if (is.null(source_id)) "" else source_id,
      prepare, if (is.null(strata)) "" else strata,
      "TRUE", as.character(render), as.character(overwrite),
      as.character(map_options$maps), paste(map_options$map_vars, collapse = ";"),
      as.character(map_options$max_map_points), "", "", "",
      intake_package_version(),
      paste(R.version$major, R.version$minor, sep = "."),
      intake_dependency_version("openssl"),
      intake_dependency_version("rmarkdown"),
      started_at, "", "", "", ""
    ),
    stringsAsFactors = FALSE
  )
  columns <- lapply(seq_along(data), function(index) {
    data.frame(
      field = c(
        sprintf("column.%03d.name", index),
        sprintf("column.%03d.class", index)
      ),
      value = c(names(data)[[index]], paste(class(data[[index]]), collapse = "/")),
      stringsAsFactors = FALSE
    )
  })
  if (length(columns) > 0L) {
    rows <- rbind(rows, do.call(rbind, columns))
  }
  row.names(rows) <- NULL
  rows
}

intake_complete_metadata <- function(input, status, stage, fingerprint, finished_at) {
  replacements <- c(
    finished_at_utc = finished_at,
    status = status,
    stage = stage,
    spec_fingerprint_sha256 = fingerprint
  )
  for (field in names(replacements)) {
    input$value[input$field == field] <- replacements[[field]]
  }
  input
}

intake_complete_spec_metadata <- function(input, spec) {
  values <- c(
    spec_state = spec$state,
    spec_source = spec$source,
    spec_source_name = if (is.na(spec$source_name)) "" else spec$source_name
  )
  for (field in names(values)) {
    input$value[input$field == field] <- values[[field]]
  }
  input
}

intake_run_metadata <- function(input) {
  fields <- c(
    "workflow_contract", "package_version", "r_version",
    "dependency.openssl", "dependency.rmarkdown", "prepare", "strata",
    "include_missing_stratum", "render", "overwrite", "maps", "map_vars",
    "max_map_points", "spec_state",
    "spec_source", "spec_source_name", "started_at_utc", "finished_at_utc", "status",
    "stage", "spec_fingerprint_sha256"
  )
  values <- stats::setNames(input$value[match(fields, input$field)], fields)
  as.data.frame(as.list(values), stringsAsFactors = FALSE, check.names = FALSE)
}

intake_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("episcout")),
    error = function(e) "development"
  )
}

intake_dependency_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return("not_installed")
  }
  as.character(utils::packageVersion(package))
}

intake_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

intake_reconcile_canonical <- function(summaries, missing, data, spec) {
  required <- c("variables", "numeric", "categorical", "text", "temporal", "skipped")
  if (!identical(names(summaries), required)) {
    return("Canonical summary components do not match the required six-component contract.")
  }
  variables <- summaries$variables
  if (!identical(as.character(variables$name), as.character(spec$name))) {
    return("Canonical variable membership does not match the specification.")
  }
  expected_components <- list(
    numeric = variables$name[
      variables$status == "summarised" & variables$type %in% c("numeric", "integer")
    ],
    categorical = variables$name[
      variables$status == "summarised" & variables$type %in% c("categorical", "binary")
    ],
    text = variables$name[
      variables$status == "summarised" & variables$type == "text"
    ],
    temporal = variables$name[
      variables$status == "summarised" & variables$type %in% c("date", "datetime")
    ]
  )
  for (component in c("numeric", "text", "temporal")) {
    actual <- as.character(summaries[[component]]$name)
    if (anyDuplicated(actual) || !identical(actual, expected_components[[component]])) {
      return("Canonical type-specific component membership does not reconcile with variable statuses.")
    }
  }
  categorical_key <- summaries$categorical[c("name", "level")]
  if (anyDuplicated(categorical_key) || !setequal(
    unique(summaries$categorical$name), expected_components$categorical
  )) {
    return("Canonical type-specific component membership does not reconcile with variable statuses.")
  }
  expected_skipped <- variables$name[variables$status == "skipped"]
  if (anyDuplicated(summaries$skipped$name) ||
        !setequal(summaries$skipped$name, expected_skipped)) {
    return("Canonical skipped-variable membership does not reconcile with variable statuses.")
  }
  present <- variables$name %in% intersect(spec$name, names(data))
  known_n <- present & !is.na(variables$n)
  if (any(variables$n[known_n] != nrow(data))) {
    return("Canonical per-variable row counts do not reconcile with the analysis population.")
  }
  if (any(
    variables$n_missing[known_n] + variables$n_observed[known_n] !=
      variables$n[known_n]
  )) {
    return("Canonical missing and observed counts do not reconcile with per-variable row counts.")
  }
  missing_index <- match(variables$name[known_n], missing$name)
  if (any(is.na(missing_index)) || any(
    missing$n_missing[missing_index] != variables$n_missing[known_n]
  )) {
    return("Canonical missingness and variable-summary counts do not reconcile.")
  }
  categorical_names <- unique(summaries$categorical$name)
  for (name in categorical_names) {
    expected <- variables$n_observed[match(name, variables$name)]
    observed <- sum(summaries$categorical$n[summaries$categorical$name == name])
    if (is.na(expected) || observed != expected) {
      return("Canonical categorical counts do not reconcile with observed denominators.")
    }
  }
  numeric_names <- summaries$numeric$name
  if (length(numeric_names) > 0L) {
    index <- match(numeric_names, variables$name)
    observed <- summaries$numeric$n_finite + variables$n_infinite[index]
    if (any(observed != variables$n_observed[index])) {
      return("Canonical finite and infinite numeric counts do not reconcile with observed denominators.")
    }
  }
  NULL
}

intake_reconcile_stratified <- function(summaries, stratified, data) {
  if (nrow(stratified$metadata) != 1L ||
        stratified$metadata$n_input[[1]] != nrow(data) ||
        stratified$metadata$n_included[[1]] != nrow(data)) {
    return("Stratified input and included counts do not reconcile with the canonical analysis population.")
  }
  overall <- stratified$groups[stratified$groups$is_overall, , drop = FALSE]
  if (nrow(overall) != 1L || is.na(overall$n[[1]]) || overall$n[[1]] != nrow(data)) {
    return("The stratified Overall group does not reconcile with the canonical analysis population.")
  }
  grouped <- stratified$groups[!stratified$groups$is_overall, , drop = FALSE]
  if (sum(grouped$n) != stratified$metadata$n_included[[1]]) {
    return("Stratified group counts do not reconcile with the included analysis population.")
  }
  group_n <- stratified$groups$n[match(
    stratified$variables$group_id, stratified$groups$group_id
  )]
  variable_n_known <- !is.na(stratified$variables$n)
  if (any(stratified$variables$n[variable_n_known] != group_n[variable_n_known])) {
    return("Stratified variable counts do not reconcile with their groups.")
  }
  group_components <- intake_reconcile_groups(stratified)
  if (!is.null(group_components)) {
    return(group_components)
  }
  overall_variables <- stratified$variables[
    stratified$variables$group_id == overall$group_id[[1]], , drop = FALSE
  ]
  common <- intersect(summaries$variables$name, overall_variables$name)
  summary_index <- match(common, summaries$variables$name)
  overall_index <- match(common, overall_variables$name)
  comparable <- !is.na(summaries$variables$n[summary_index])
  if (any(
    summaries$variables$n[summary_index][comparable] !=
      overall_variables$n[overall_index][comparable]
  )) {
    return("Stratified Overall variable counts do not reconcile with canonical summaries.")
  }
  expected_variables <- summaries$variables
  observed_variables <- overall_variables[
    overall_variables$name %in% expected_variables$name,
    names(expected_variables), drop = FALSE
  ]
  if (!intake_frames_equal(expected_variables, observed_variables)) {
    return("Stratified Overall variable summaries do not agree with canonical summaries.")
  }
  overall_numeric <- stratified$numeric[
    stratified$numeric$group_id == overall$group_id[[1]], , drop = FALSE
  ]
  if (nrow(overall_numeric) > 0L) {
    variable_index <- match(overall_numeric$name, summaries$variables$name)
    numeric_counts <- c("n", "n_missing", "n_observed", "n_infinite")
    for (field in numeric_counts) {
      if (any(
        overall_numeric[[field]] != summaries$variables[[field]][variable_index]
      )) {
        return("Stratified Overall numeric denominators do not agree with canonical variable counts.")
      }
    }
  }
  for (component in c("numeric", "text", "temporal")) {
    expected <- summaries[[component]]
    observed <- stratified[[component]][
      stratified[[component]]$group_id == overall$group_id[[1]],
      names(expected), drop = FALSE
    ]
    if (!intake_frames_equal(expected, observed)) {
      return(paste0(
        "Stratified Overall ", component,
        " summaries do not agree with canonical summaries."
      ))
    }
  }
  expected_categorical <- summaries$categorical
  overall_categorical <- stratified$categorical[
    stratified$categorical$group_id == overall$group_id[[1]], , drop = FALSE
  ]
  observed_categorical <- overall_categorical[
    !overall_categorical$is_missing_level,
    names(expected_categorical), drop = FALSE
  ]
  if (!intake_frames_equal(expected_categorical, observed_categorical)) {
    return("Stratified Overall categorical summaries do not agree with canonical summaries.")
  }
  categorical_names <- unique(overall_categorical$name)
  for (name in categorical_names) {
    rows <- overall_categorical[overall_categorical$name == name, , drop = FALSE]
    variable <- summaries$variables[summaries$variables$name == name, , drop = FALSE]
    if (nrow(variable) != 1L || any(rows$n_total != variable$n[[1]]) ||
          any(rows$n_observed != variable$n_observed[[1]])) {
      return("Stratified Overall categorical denominators do not agree with canonical variable counts.")
    }
    missing <- rows[rows$is_missing_level, , drop = FALSE]
    expected_missing_p <- summary_safe_proportion(
      variable$n_missing[[1]], variable$n[[1]]
    )
    if (nrow(missing) != 1L || missing$n[[1]] != variable$n_missing[[1]] ||
          !isTRUE(all.equal(missing$p_total[[1]], expected_missing_p)) ||
          !is.na(missing$p_observed[[1]])) {
      return("Stratified Overall categorical missingness does not agree with canonical variable counts.")
    }
  }
  canonical_n <- summaries$variables$n[!is.na(summaries$variables$n)]
  if (length(canonical_n) > 0L && any(canonical_n != nrow(data))) {
    return("Canonical summary counts changed before stratified reconciliation.")
  }
  NULL
}

intake_frames_equal <- function(expected, observed) {
  row.names(expected) <- NULL
  row.names(observed) <- NULL
  isTRUE(all.equal(expected, observed, check.attributes = FALSE))
}

intake_reconcile_groups <- function(stratified) {
  variable_key <- paste(
    stratified$variables$group_id, stratified$variables$name, sep = "\r"
  )
  if (anyDuplicated(stratified$groups$group_id) || anyDuplicated(variable_key)) {
    return("Stratified group or group-variable membership contains duplicates.")
  }
  expected_components <- list(
    numeric = variable_key[
      stratified$variables$status == "summarised" &
        stratified$variables$type %in% c("numeric", "integer")
    ],
    categorical = variable_key[
      stratified$variables$status == "summarised" &
        stratified$variables$type %in% c("categorical", "binary")
    ],
    text = variable_key[
      stratified$variables$status == "summarised" &
        stratified$variables$type == "text"
    ],
    temporal = variable_key[
      stratified$variables$status == "summarised" &
        stratified$variables$type %in% c("date", "datetime")
    ],
    skipped = variable_key[stratified$variables$status == "skipped"]
  )
  fields <- list(
    numeric = c("n", "n_missing", "n_observed", "n_infinite"),
    text = c("n", "n_missing", "n_observed", "n_unique"),
    temporal = c("n", "n_missing", "n_observed", "n_unique")
  )
  for (component in names(fields)) {
    source <- stratified[[component]]
    source_key <- paste(source$group_id, source$name, sep = "\r")
    if (anyDuplicated(source_key) ||
          !identical(source_key, expected_components[[component]])) {
      return("Stratified type-specific component membership does not reconcile with group variable statuses.")
    }
    index <- match(source_key, variable_key)
    if (any(is.na(index))) {
      return("A stratified type-specific row does not trace to its group variable summary.")
    }
    for (field in fields[[component]]) {
      if (!intake_values_equal(
        source[[field]], stratified$variables[[field]][index]
      )) {
        return("Stratified type-specific denominators do not reconcile with group variable counts.")
      }
    }
  }

  categorical <- stratified$categorical
  categorical_key <- paste(categorical$group_id, categorical$name, sep = "\r")
  categorical_row_key <- categorical[
    c("group_id", "name", "level", "is_missing_level")
  ]
  if (anyDuplicated(categorical_row_key) ||
        !setequal(unique(categorical_key), expected_components$categorical)) {
    return("Stratified categorical component membership does not reconcile with group variable statuses.")
  }
  index <- match(categorical_key, variable_key)
  if (any(is.na(index)) ||
    !intake_values_equal(categorical$n_total, stratified$variables$n[index]) ||
    !intake_values_equal(
      categorical$n_observed, stratified$variables$n_observed[index]
    )) {
    return("Stratified categorical denominators do not reconcile with group variable counts.")
  }
  for (key in unique(categorical_key)) {
    rows <- categorical[categorical_key == key, , drop = FALSE]
    variable <- stratified$variables[variable_key == key, , drop = FALSE]
    missing <- rows[rows$is_missing_level, , drop = FALSE]
    ordinary <- rows[!rows$is_missing_level, , drop = FALSE]
    if (nrow(variable) != 1L || nrow(missing) != 1L ||
      missing$n[[1]] != variable$n_missing[[1]] ||
      sum(ordinary$n) != variable$n_observed[[1]] ||
      !intake_values_equal(
        rows$p_total,
        summary_safe_proportion(rows$n, rows$n_total[[1]])
      ) ||
      !intake_values_equal(
        ordinary$p_observed,
        summary_safe_proportion(
          ordinary$n, ordinary$n_observed[[1]]
        )
      ) ||
      !is.na(missing$p_observed[[1]])) {
      return("Stratified categorical counts or proportions do not reconcile within a group.")
    }
  }
  skipped_key <- paste(
    stratified$skipped$group_id, stratified$skipped$name, sep = "\r"
  )
  if (anyDuplicated(skipped_key) ||
        !identical(skipped_key, expected_components$skipped)) {
    return("Stratified skipped-variable membership does not reconcile with group variable statuses.")
  }
  NULL
}

intake_values_equal <- function(x, y) {
  isTRUE(all.equal(x, y, check.attributes = FALSE))
}

intake_render_report <- function(output_dir, manifest, status, stage) {
  path <- file.path(output_dir, "report.html")
  created <- manifest[manifest$status == "created", , drop = FALSE]
  links <- vapply(seq_len(nrow(created)), function(index) {
    paste0(
      "<li><a href=\"", intake_html_escape(created$path[[index]]), "\">",
      intake_html_escape(created$path[[index]]), "</a></li>"
    )
  }, character(1))
  sections <- intake_report_sections(output_dir, created$path)
  banner_class <- if (status == "complete") "complete" else "incomplete"
  banner <- if (status == "complete") {
    "Analysis completed."
  } else {
    paste0("INCOMPLETE: workflow status is ", status, ". Follow messages.csv before rerunning or interpreting outputs.")
  }
  html <- c(
    "<!doctype html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>episcout data intake report</title>",
    "<style>body{font-family:system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#202124} .banner{padding:1rem;border-radius:.3rem;font-weight:700}.complete{background:#e6f4ea}.incomplete{background:#fce8e6} table{border-collapse:collapse;width:100%;display:block;overflow-x:auto;margin-bottom:2rem}th,td{border:1px solid #dadce0;padding:.35rem;text-align:left;vertical-align:top}th{background:#f1f3f4}code{background:#f1f3f4;padding:.1rem .25rem}img.map{display:block;max-width:100%;height:auto;margin-bottom:2rem}</style>",
    "</head><body>",
    "<h1>episcout data intake report</h1>",
    paste0("<div class=\"banner ", banner_class, "\">", intake_html_escape(banner), "</div>"),
    paste0("<p><strong>Status:</strong> ", intake_html_escape(status), "<br><strong>Last completed stage:</strong> ", intake_html_escape(stage), "</p>"),
    "<p>This report is a view of saved machine-readable artifacts. It does not recalculate statistics, write row-level data, or perform pseudonymisation. episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared.</p>",
    "<h2>Artifacts</h2><ul>", links, "</ul>", sections,
    "</body></html>"
  )
  intake_atomic_text(html, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

intake_report_sections <- function(output_dir, created_paths) {
  display <- c(
    "intake_metadata.csv" = "Input and run metadata",
    "messages.csv" = "Workflow messages",
    "schema_before.csv" = "Schema before preparation",
    "schema_after.csv" = "Schema after preparation",
    "preparation_audit.csv" = "Preparation audit",
    "missing.csv" = "Canonical missingness",
    "geo_qa.csv" = "Declared coordinate-pair aggregate QA",
    "map_inventory.csv" = "Map inventory",
    "summary_variables.csv" = "Canonical variable summaries",
    "summary_numeric.csv" = "Canonical numeric summaries",
    "summary_categorical.csv" = "Canonical categorical summaries",
    "summary_text.csv" = "Canonical text summaries",
    "summary_temporal.csv" = "Canonical temporal summaries",
    "summary_skipped.csv" = "Canonical skipped variables",
    "stratified_groups.csv" = "Stratified groups",
    "stratified_numeric.csv" = "Stratified numeric summaries",
    "stratified_categorical.csv" = "Stratified categorical summaries",
    "stratified_text.csv" = "Stratified text summaries",
    "stratified_temporal.csv" = "Stratified temporal summaries",
    "stratified_skipped.csv" = "Stratified skipped variables",
    "table1.csv" = "Table 1"
  )
  available <- intersect(names(display), created_paths)
  tables <- unlist(lapply(available, function(relative_path) {
    table <- utils::read.csv(
      file.path(output_dir, relative_path),
      check.names = FALSE, stringsAsFactors = FALSE,
      na.strings = character()
    )
    c(
      paste0("<h2>", intake_html_escape(display[[relative_path]]), "</h2>"),
      intake_html_table(table)
    )
  }), use.names = FALSE)
  map_paths <- grep(
    "^maps/map-p[0-9]{3,}-(geometry|v[0-9]{3,})\\.svg$",
    created_paths,
    value = TRUE
  )
  maps <- unlist(lapply(map_paths, function(relative_path) {
    map_id <- sub("\\.svg$", "", sub("^maps/", "", relative_path))
    c(
      paste0("<h3>", intake_html_escape(map_id), "</h3>"),
      paste0(
        "<img class=\"map\" src=\"", intake_html_escape(relative_path),
        "\" alt=\"", intake_html_escape(map_id), " point map\">"
      )
    )
  }), use.names = FALSE)
  if (length(tables) == 0L && length(maps) == 0L) {
    return("<p>No calculation tables or maps were created.</p>")
  }
  c(tables, if (length(maps) > 0L) c("<h2>Maps</h2>", maps) else character())
}

intake_html_table <- function(data) {
  if (nrow(data) == 0L) {
    return("<p>No rows.</p>")
  }
  header <- paste0(
    "<tr>", paste0("<th>", intake_html_escape(names(data)), "</th>", collapse = ""), "</tr>"
  )
  rows <- vapply(seq_len(nrow(data)), function(index) {
    values <- vapply(data[index, , drop = FALSE], function(value) {
      if (length(value) == 0L || is.na(value[[1]])) "" else as.character(value[[1]])
    }, character(1))
    paste0(
      "<tr>", paste0("<td>", intake_html_escape(values), "</td>", collapse = ""), "</tr>"
    )
  }, character(1))
  paste0("<table><thead>", header, "</thead><tbody>", paste0(rows, collapse = ""), "</tbody></table>")
}

intake_html_escape <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}
