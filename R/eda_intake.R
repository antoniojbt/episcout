#' Run a stage-gated data intake-to-report workflow
#'
#' Compose specification scaffolding, reviewed preparation, canonical summaries,
#' optional stratified summaries, and an aggregate-only report bundle. Expected
#' review gates return an object with a non-complete status instead of throwing
#' an error. Invalid function arguments and unsafe output collisions remain
#' errors.
#'
#' @param data An in-memory data frame. Use [epi_read()] before this function for
#'   supported delimited files.
#' @param spec `NULL`, a reviewed EDA specification data frame, or a CSV path
#'   accepted by [epi_eda_spec()]. `NULL` writes a scaffold and stops for review.
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
#'
#' @return An `epi_eda_intake` list with fixed components `status`, `stage`,
#'   `output_dir`, `manifest`, `input`, `spec`, `schema_before`, `schema_after`,
#'   `preparation_audit`, `missing`, `summary`, `stratified`, `table1`, `report`,
#'   `messages`, and `metadata`.
#'   Status is one of `review_required`, `blocked`, `audit_complete`, or
#'   `complete`. Manifest paths and the report path are relative to `output_dir`.
#'
#' @details The bundle never writes source or prepared row-level data, row
#'   previews, raw free-text examples, identifier values, or pseudonymisation
#'   bridge tables. Specification files can contain reviewed level and missing
#'   code metadata and are marked accordingly in the manifest. Summary outputs
#'   are not disclosure-controlled and require review before sharing.
#'
#' @export
epi_eda_intake_run <- function(data,
                               spec = NULL,
                               output_dir,
                               prepare = c("none", "audit", "apply"),
                               strata = NULL,
                               render = TRUE,
                               overwrite = FALSE,
                               source_id = NULL) {
  prepare <- match.arg(prepare)
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
    data, source_id, prepare, strata, render, overwrite, started_at
  )
  messages <- intake_empty_messages()
  result <- intake_empty_result(bundle$output_dir, state, input, render)

  intake_write_csv(state, "intake_metadata", input)

  if (is.null(spec)) {
    scaffold <- tryCatch(epi_eda_spec_scaffold(data), error = identity)
    if (inherits(scaffold, "error")) {
      messages <- intake_add_message(
        messages, "intake", "blocker", "spec_scaffold",
        conditionMessage(scaffold),
        "Resolve the reported source structure before generating a specification scaffold."
      )
      result$status <- "blocked"
      result$stage <- "intake"
      result$spec <- intake_spec_state("invalid", NULL, "generated", "")
      return(intake_finish(result, state, input, messages, render))
    }
    intake_write_csv(state, "spec_scaffold", scaffold)
    intake_write_text(state, "review_guide", intake_review_guide())
    messages <- intake_add_message(
      messages, "intake", "warning", "specification",
      "The generated specification is a scaffold and has not been reviewed.",
      "Review every specification field, set each review_status to reviewed, and rerun with the reviewed specification."
    )
    result$status <- "review_required"
    result$stage <- "intake"
    result$spec <- intake_spec_state(
      "review_required", scaffold, "generated", intake_spec_fingerprint(scaffold)
    )
    return(intake_finish(
      result, state, input, messages, render,
      intake_spec_fingerprint(scaffold)
    ))
  }

  parsed_spec <- tryCatch(epi_eda_spec(spec), error = identity)
  if (inherits(parsed_spec, "error")) {
    messages <- intake_add_message(
      messages, "specification", "blocker", "specification",
      "The supplied specification did not satisfy the EDA specification contract.",
      "Correct the specification contract or CSV syntax and rerun the workflow."
    )
    result$status <- "blocked"
    result$stage <- "intake"
    result$spec <- intake_spec_state(
      "invalid", NULL, intake_spec_source(spec), "", intake_spec_source_name(spec)
    )
    return(intake_finish(result, state, input, messages, render))
  }

  fingerprint <- intake_spec_fingerprint(parsed_spec)
  review_state <- if (is_eda_scaffold_spec(parsed_spec)) {
    reviewed <- !is.na(parsed_spec$review_status) &
      as.character(parsed_spec$review_status) == "reviewed"
    if (all(reviewed)) "reviewed" else "review_required"
  } else {
    "caller_asserted"
  }
  result$spec <- intake_spec_state(
    review_state, parsed_spec, intake_spec_source(spec), fingerprint,
    intake_spec_source_name(spec)
  )
  if (review_state == "caller_asserted") {
    messages <- intake_add_message(
      messages, "specification", "warning", "specification",
      "The supplied specification has no scaffold review evidence and is treated as caller-asserted.",
      "Confirm that scientific roles, types, levels, missing codes and privacy handling were reviewed before analysis."
    )
  }
  intake_write_csv(state, "spec_reviewed", parsed_spec)

  audit_result <- tryCatch(
    epi_eda_prepare(data, parsed_spec, mode = "audit"),
    error = identity
  )
  if (inherits(audit_result, "error")) {
    messages <- intake_add_message(
      messages, "audit", "blocker", "preparation_audit",
      conditionMessage(audit_result),
      "Correct the data or reviewed specification before rerunning the audit."
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
  exclusions <- eda_summary_exclusions(analysis_frame, parsed_spec)
  profile_data <- analysis_frame[
    , !names(analysis_frame) %in% names(exclusions), drop = FALSE
  ]
  missing <- epi_eda_profile_missing(profile_data, parsed_spec)
  result$missing <- missing
  intake_write_csv(state, "missing", missing)
  summaries <- tryCatch(
    epi_eda_profile_summaries(profile_data, parsed_spec),
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
  summaries <- eda_apply_summary_exclusions(
    summaries, analysis_frame, parsed_spec, exclusions
  )
  reconciliation <- intake_reconcile_canonical(
    summaries, missing, profile_data, parsed_spec
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
  identifier_names <- names(exclusions)
  if (length(identifier_names) > 0L) {
    messages <- intake_add_message(
      messages, "analysis", "warning", paste(identifier_names, collapse = ", "),
      "Explicit identifier-role variables were excluded from analytical missingness and type-specific summaries.",
      "Review identifier handling separately; no observed values read from those data columns entered analytical artifacts."
    )
  }
  result$stage <- "canonical_summary"

  if (!is.null(strata)) {
    strata_role <- trimws(tolower(as.character(
      parsed_spec$role[match(strata, parsed_spec$name)]
    )))
    if (length(strata_role) == 1L && !is.na(strata_role) &&
          strata_role %in% c("id", "identifier")) {
      messages <- intake_add_message(
        messages, "analysis", "blocker", strata,
        "A variable with an explicit identifier role cannot be used as an intake stratifier.",
        "Choose a reviewed non-identifier categorical or binary stratifier."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    stratified <- tryCatch(
      epi_eda_profile_stratified(analysis_frame, parsed_spec, strata),
      error = identity
    )
    if (inherits(stratified, "error")) {
      messages <- intake_add_message(
        messages, "analysis", "blocker", strata,
        conditionMessage(stratified),
        "Choose one reviewed categorical or binary stratifier and rerun."
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
        "Use the retained stratified machine components and review the Table 1 contract before rerunning."
      )
      result$status <- "blocked"
      return(intake_finish(result, state, input, messages, render, fingerprint))
    }
    result$table1 <- table1
    intake_write_csv(state, "table1", table1)
    messages <- intake_add_message(
      messages, "analysis", "warning", "stratified_outputs",
      "Stratified summaries and Table 1 are not disclosure-controlled.",
      "Review small cells and sharing risk before distributing these artifacts."
    )
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
  entry_paths <- file.path(output_dir, entries)
  entry_links <- Sys.readlink(entry_paths)
  if (any(file.info(entry_paths)$isdir, na.rm = TRUE) ||
        any(!is.na(entry_links) & nzchar(entry_links))) {
    stop("A non-empty output_dir containing directories or symbolic links cannot be overwritten safely.", call. = FALSE)
  }
  if (!all(utils::file_test("-f", entry_paths))) {
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
  valid <- !inherits(prior, "error") &&
    identical(names(prior), names(registry)) &&
    identical(as.character(prior$artifact), as.character(registry$artifact)) &&
    identical(as.character(prior$type), as.character(registry$type)) &&
    identical(as.character(prior$path), as.character(registry$path)) &&
    identical(
      as.character(prior$sensitivity), as.character(registry$sensitivity)
    ) &&
    all(prior$status %in% c("created", "not_created")) &&
    identical(prior$status[prior$artifact == "manifest"], "created") &&
    identical(prior$checksum_md5[prior$artifact == "manifest"], "") &&
    all(prior$checksum_md5[prior$status == "not_created"] == "")
  if (!valid) {
    stop("overwrite = TRUE requires a valid prior episcout intake manifest.", call. = FALSE)
  }
  expected <- sort(as.character(prior$path[prior$status == "created"]))
  if (!identical(sort(entries), expected)) {
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
    spec_scaffold = "spec_scaffold.csv",
    review_guide = "review_guide.md",
    spec_reviewed = "spec_reviewed.csv",
    schema_before = "schema_before.csv",
    schema_after = "schema_after.csv",
    preparation_audit = "preparation_audit.csv",
    missing = "missing.csv",
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
    "manifest", "metadata", "messages", "specification", "guide",
    "specification", "schema", "schema", "audit", "missingness",
    rep("canonical_summary", 6L), rep("stratified_summary", 8L),
    "presentation", "report"
  )
  sensitivity <- c(
    "internal_review", "internal_review", "internal_review", "specification_review",
    "internal_review", "specification_review", "internal_review",
    "internal_review", "internal_review", "disclosure_review",
    rep("disclosure_review", 6L), rep("disclosure_review", 8L),
    "disclosure_review", "disclosure_review"
  )
  data.frame(
    artifact = names(paths),
    type = types,
    path = unname(paths),
    status = "not_created",
    sensitivity = sensitivity,
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
        "Resolve this finding in the source data or reviewed specification before preparation."
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
                            started_at) {
  rows <- data.frame(
    field = c(
      "workflow_contract", "n_rows", "n_columns", "source_id",
      "prepare", "strata", "include_missing_stratum", "render", "overwrite",
      "spec_review_state", "spec_source", "spec_source_name", "package_version",
      "r_version", "dependency.openssl", "dependency.rmarkdown",
      "started_at_utc", "finished_at_utc", "status", "stage",
      "spec_fingerprint_sha256"
    ),
    value = c(
      "intake-1", as.character(nrow(data)), as.character(ncol(data)),
      if (is.null(source_id)) "" else source_id,
      prepare, if (is.null(strata)) "" else strata,
      "TRUE", as.character(render), as.character(overwrite), "", "", "",
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
    spec_review_state = spec$state,
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
    "include_missing_stratum", "render", "overwrite", "spec_review_state",
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

intake_review_guide <- function() {
  c(
    "# EDA specification review required",
    "",
    "The generated scaffold is not approval. Review every row before preparation or analysis.",
    "",
    "For each variable, confirm `name`, `label`, `type`, `role`, `units`, declared `levels`, `missing_codes`, `required`, `group`, and `description`.",
    "Treat observed class and count fields as aggregate evidence only. `candidate_type` is a prompt for review, not a semantic decision.",
    "Set `review_status` to `reviewed` only after resolving `review_reason`; do not add raw observations, identifiers, or free-text examples to the specification.",
    "Column names and copied factor-level metadata may themselves be sensitive; review the scaffold before storing or sharing it.",
    "Rerun `epi_eda_intake_run()` with the reviewed CSV and `prepare = 'audit'` before applying transformations.",
    "",
    "The eventual summary bundle is not disclosure-controlled or automatically de-identified. Pseudonymisation and bridge-table handling are separate explicit actions."
  )
}

intake_reconcile_canonical <- function(summaries, missing, data, spec) {
  required <- c("variables", "numeric", "categorical", "text", "temporal", "skipped")
  if (!identical(names(summaries), required)) {
    return("Canonical summary components do not match the required six-component contract.")
  }
  variables <- summaries$variables
  if (!identical(as.character(variables$name), as.character(spec$name))) {
    return("Canonical variable membership does not match the reviewed specification.")
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
  identifier <- trimws(tolower(as.character(summaries$variables$role))) %in%
    c("id", "identifier")
  expected_variables <- summaries$variables[!identifier, , drop = FALSE]
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
      intake_html_escape(created$path[[index]]), "</a> - ",
      intake_html_escape(created$sensitivity[[index]]), "</li>"
    )
  }, character(1))
  sections <- intake_report_sections(output_dir, created$path)
  banner_class <- if (status == "complete") "complete" else "incomplete"
  banner <- if (status == "complete") {
    "Analysis completed. Outputs remain subject to disclosure review."
  } else {
    paste0("INCOMPLETE: workflow status is ", status, ". Follow messages.csv before analysis or sharing.")
  }
  html <- c(
    "<!doctype html>",
    "<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>episcout data intake report</title>",
    "<style>body{font-family:system-ui,sans-serif;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#202124} .banner{padding:1rem;border-radius:.3rem;font-weight:700}.complete{background:#e6f4ea}.incomplete{background:#fce8e6} table{border-collapse:collapse;width:100%;display:block;overflow-x:auto;margin-bottom:2rem}th,td{border:1px solid #dadce0;padding:.35rem;text-align:left;vertical-align:top}th{background:#f1f3f4}code{background:#f1f3f4;padding:.1rem .25rem}</style>",
    "</head><body>",
    "<h1>episcout data intake report</h1>",
    paste0("<div class=\"banner ", banner_class, "\">", intake_html_escape(banner), "</div>"),
    paste0("<p><strong>Status:</strong> ", intake_html_escape(status), "<br><strong>Last completed stage:</strong> ", intake_html_escape(stage), "</p>"),
    "<p>This report is a view of saved machine-readable artifacts. It does not recalculate statistics, write row-level data, or perform pseudonymisation. The bundle is not disclosure-controlled or automatically de-identified.</p>",
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
  if (length(available) == 0L) {
    return("<p>No calculation tables were created at this review stage.</p>")
  }
  unlist(lapply(available, function(relative_path) {
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
