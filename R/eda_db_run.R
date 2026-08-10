#' Run specification-first EDA against PostgreSQL
#'
#' Execute schema, missingness, canonical summary, identifier-QA, compact plot
#' preparation, and optional bounded point-map collection against one read-only
#' repeatable-read snapshot, then publish a staged bundle.
#'
#' @param source An [epi_eda_postgres_source()].
#' @param spec An EDA specification data frame or local CSV path.
#' @param output_dir A local directory for the database-EDA bundle.
#' @param overwrite Whether an exact, unchanged prior database-EDA bundle with
#'   matching source, specification, and plot options may be replaced.
#' @param plots Whether deterministic SVG plots are rendered and written.
#' @param max_plot_levels Whole number from 2 through 100 controlling only the
#'   displayed categorical levels. Canonical frequencies remain complete.
#' @param maps Whether to create one geometry-only point map for every declared
#'   coordinate pair.
#' @param map_vars Unique declared variables for additional thematic maps.
#' @param max_map_points Inclusive maximum number of rows allowed for mapping.
#' @param layout Output layout. `"bundle"` preserves the existing flat
#'   aggregate bundle; `"delivery"` adds the canonical directory tree and
#'   portable HTML entry point.
#' @param quiet Logical passed to the delivery report renderer. It is ignored
#'   when `layout = "bundle"`.
#'
#' @return An `epi_eda_db_run` list with fixed components `status`,
#'   `output_dir`, `manifest`, `source`, `spec`, `schema`, `missing`,
#'   `summaries`, `identifier_qa`, `geo`, `plots`, `plot_inventory`, `maps`,
#'   `map_inventory`, `timings`, `messages`, and `metadata`.
#'
#' @details The bundle contains aggregates, the caller-authored specification,
#' and only explicitly requested bounded point maps. It contains no source-row
#' table, SQL, query parameters, credentials, or connection attributes.
#' PostgreSQL and driver/server logs remain the caller's infrastructure
#' responsibility. episcout creates the outputs explicitly requested by the
#' analyst and does not decide whether they may be shared.
#'
#' @export
epi_eda_db_run <- function(source,
                           spec,
                           output_dir,
                           overwrite = FALSE,
                           plots = TRUE,
                           max_plot_levels = 20L,
                           maps = FALSE,
                           map_vars = character(),
                           max_map_points = 10000L,
                           layout = c("bundle", "delivery"),
                           quiet = TRUE) {
  if (!inherits(source, "epi_eda_postgres_source")) {
    stop("source must be an epi_eda_postgres_source.", call. = FALSE)
  }
  layout <- match.arg(layout)
  intake_validate_flag(overwrite, "overwrite")
  intake_validate_flag(plots, "plots")
  if (layout == "delivery") {
    intake_validate_flag(quiet, "quiet")
    eda_db_report_dependencies()
  }
  max_plot_levels <- eda_db_whole_number(max_plot_levels, "max_plot_levels", 2L, 100L)
  spec <- epi_eda_spec(spec)
  map_options <- eda_map_options(spec, maps, map_vars, max_map_points)
  eda_validate_map_columns(source$columns$name, map_options)
  catalogue <- eda_validate_postgres_source(source, require_idle = TRUE)
  source_fingerprint <- eda_pg_source_fingerprint(source)
  spec_fingerprint <- eda_postgres_fingerprint(spec)
  paths <- eda_db_prepare_output_dir(
    output_dir, overwrite, source_fingerprint, spec_fingerprint, plots,
    max_plot_levels, map_options, layout
  )
  published <- FALSE
  on.exit(
    {
      if (!published && dir.exists(paths$staging_dir)) {
        unlink(paths$staging_dir, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  started_at <- intake_timestamp()
  started_elapsed <- proc.time()[["elapsed"]]
  timing_env <- new.env(parent = emptyenv())
  timing_env$rows <- list()
  snapshot <- eda_postgres_transaction(
    source,
    {
      n_total <- eda_postgres_row_count(source, timing_env)
      schema <- eda_postgres_schema_inside(source, spec, timing_env)
      missing <- eda_postgres_missing_inside(source, spec, timing_env, n_total)
      geo <- eda_postgres_geo_inside(source, spec, timing_env)
      map_data <- eda_postgres_map_data_inside(
        source, spec, geo, map_options, timing_env, n_total
      )
      summaries <- eda_postgres_summaries_inside(source, spec, timing_env, n_total)
      identifier_qa <- eda_pg_identifier_qa_inside(source, spec, timing_env, n_total)
      plot_data <- eda_postgres_plot_data_inside(
        source, spec, summaries, max_plot_levels, timing_env
      )
      eda_db_reconcile(
        n_total, spec, missing, summaries, identifier_qa, geo, plot_data
      )
      list(
        n_total = n_total, schema = schema, missing = missing,
        summaries = summaries, identifier_qa = identifier_qa, geo = geo,
        plot_data = plot_data, map_data = map_data
      )
    },
    timing_env = timing_env
  )

  rendered <- if (plots) eda_render_plot_entries(snapshot$plot_data$entries) else stats::setNames(vector("list", 0L), character())
  plot_inventory <- eda_db_bundle_plot_inventory(
    snapshot$plot_data$entries, plots
  )
  if (plots) {
    eda_db_write_plots(paths$staging_dir, snapshot$plot_data$entries, rendered, plot_inventory)
  }
  map_result <- eda_data_frame_maps(
    snapshot$map_data, spec, snapshot$geo, map_options
  )
  snapshot$map_data <- NULL
  eda_write_maps(
    map_result$maps, map_result$map_inventory, paths$staging_dir,
    "database EDA"
  )
  messages <- eda_db_messages(snapshot$summaries, snapshot$identifier_qa)
  finished_at <- intake_timestamp()
  source_metadata <- data.frame(
    schema_name = source$schema,
    relation_name = source$relation,
    relation_kind = source$relation_kind,
    column_count = as.integer(nrow(source$columns)),
    source_contract = source$source_version,
    server_version_num = catalogue$server_version_num,
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    workflow_contract = "postgres-eda-bundle-2",
    canonical_summary_contract = "canonical-summary-1",
    geo_qa_contract = "declared-coordinate-pair-2",
    plot_data_contract = if (layout == "delivery") {
      "compact-plot-data-2"
    } else {
      "compact-plot-data-1"
    },
    package_version = intake_package_version(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    dependency_DBI = intake_dependency_version("DBI"),
    dependency_RPostgres = intake_dependency_version("RPostgres"),
    n_rows = snapshot$n_total,
    n_columns = as.integer(nrow(source$columns)),
    n_spec_variables = as.integer(nrow(spec)),
    plots = plots,
    max_plot_levels = max_plot_levels,
    maps = map_options$maps,
    map_vars = paste(map_options$map_vars, collapse = ";"),
    map_vars_fingerprint_sha256 = eda_postgres_fingerprint(map_options$map_vars),
    max_map_points = map_options$max_map_points,
    source_fingerprint_sha256 = source_fingerprint,
    spec_fingerprint_sha256 = spec_fingerprint,
    started_at_utc = started_at,
    finished_at_utc = finished_at,
    status = "complete",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  timings <- eda_db_timings(timing_env)
  elapsed <- proc.time()[["elapsed"]] - started_elapsed
  timings <- rbind(timings, data.frame(
    stage = "complete_run", variable_index = NA_integer_, name = NA_character_,
    query_kind = "end_to_end", elapsed_seconds = as.numeric(elapsed),
    rows_returned = 0L, bounded_limit = 0L, status = "complete",
    stringsAsFactors = FALSE
  ))

  eda_db_write_bundle_tables(
    paths$staging_dir, metadata, messages, spec, source_metadata,
    snapshot, plot_inventory, map_result$map_inventory, timings, layout
  )
  plot_data_registry <- eda_db_write_plot_data(
    paths$staging_dir, snapshot$plot_data$entries, layout
  )
  manifest <- eda_db_create_manifest(
    paths$staging_dir, plot_inventory, map_result$map_inventory, layout,
    plot_data_registry
  )
  manifest_path <- file.path(
    paths$staging_dir, eda_db_manifest_relative_path(layout)
  )
  intake_atomic_csv(manifest, manifest_path)
  eda_db_validate_staged_bundle(paths$staging_dir, manifest)
  if (layout == "delivery") {
    rendered_bundle <- eda_db_render_staged_bundle(
      paths$staging_dir,
      quiet = quiet
    )
    manifest <- rendered_bundle$manifest
  }

  state <- new.env(parent = emptyenv())
  state$target_dir <- paths$output_dir
  state$staging_dir <- paths$staging_dir
  state$published <- FALSE
  intake_publish_bundle(state)
  published <- TRUE
  normalized_output <- normalizePath(paths$output_dir, winslash = "/", mustWork = TRUE)
  structure(
    list(
      status = "complete",
      output_dir = normalized_output,
      manifest = manifest,
      source = source_metadata,
      spec = spec,
      schema = snapshot$schema,
      missing = snapshot$missing,
      summaries = snapshot$summaries,
      identifier_qa = snapshot$identifier_qa,
      geo = snapshot$geo,
      plots = rendered,
      plot_inventory = plot_inventory,
      maps = map_result$maps,
      map_inventory = map_result$map_inventory,
      timings = timings,
      messages = messages,
      metadata = metadata
    ),
    class = c("epi_eda_db_run", "list")
  )
}

eda_db_whole_number <- function(value, name, minimum, maximum) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value == floor(value) && value >= minimum && value <= maximum
  if (!valid) stop(name, " must be a whole number from ", minimum, " through ", maximum, ".", call. = FALSE)
  as.integer(value)
}

eda_pg_source_fingerprint <- function(source) {
  eda_postgres_fingerprint(list(
    schema = source$schema,
    relation = source$relation,
    relation_kind = source$relation_kind,
    relation_oid = attr(source$columns, "relation_oid"),
    catalogue = attr(source$columns, "catalogue_fingerprint"),
    source_version = source$source_version
  ))
}

eda_db_prepare_output_dir <- function(output_dir,
                                      overwrite,
                                      source_fingerprint,
                                      spec_fingerprint,
                                      plots,
                                      max_plot_levels,
                                      map_options,
                                      layout) {
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(trimws(output_dir))) {
    stop("output_dir must be one non-empty local directory path.", call. = FALSE)
  }
  requested <- path.expand(output_dir)
  link <- Sys.readlink(requested)
  if (!is.na(link) && nzchar(link)) stop("output_dir must not be a symbolic link.", call. = FALSE)
  if (file.exists(requested) && !dir.exists(requested)) stop("output_dir exists and is not a directory.", call. = FALSE)
  parent <- dirname(requested)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) stop("The parent of output_dir could not be created.", call. = FALSE)
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output_dir <- file.path(parent, basename(requested))
  prohibited <- unique(c("/", normalizePath(path.expand("~"), winslash = "/", mustWork = TRUE), normalizePath(getwd(), winslash = "/", mustWork = TRUE)))
  if (output_dir %in% prohibited) stop("output_dir must not be the filesystem root, home directory, or workspace root.", call. = FALSE)
  if (dir.exists(output_dir)) {
    entries <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(entries) > 0L && !overwrite) stop("output_dir is non-empty; set overwrite = TRUE only for an unchanged owned database-EDA bundle.", call. = FALSE)
    if (length(entries) > 0L) {
      eda_db_validate_prior_bundle(
        output_dir, source_fingerprint, spec_fingerprint, plots,
        max_plot_levels, map_options, layout
      )
    }
  }
  staging_dir <- tempfile(paste0(".", basename(output_dir), "-staging-"), tmpdir = parent)
  if (!dir.create(staging_dir, showWarnings = FALSE)) stop("A staging directory could not be created beside output_dir.", call. = FALSE)
  list(output_dir = output_dir, staging_dir = normalizePath(staging_dir, winslash = "/", mustWork = TRUE))
}

eda_db_validate_prior_bundle <- function(output_dir,
                                         source_fingerprint,
                                         spec_fingerprint,
                                         plots,
                                         max_plot_levels,
                                         map_options,
                                         layout) {
  bundle <- tryCatch(
    eda_db_read_bundle(output_dir),
    error = function(error) {
      stop(conditionMessage(error), call. = FALSE)
    }
  )
  if (!identical(bundle$layout, layout)) {
    stop("Prior bundle layout does not match this run.", call. = FALSE)
  }
  metadata <- bundle$tables$run_metadata
  identity_fields <- c(
    "workflow_contract", "source_fingerprint_sha256",
    "spec_fingerprint_sha256", "plots", "max_plot_levels", "maps",
    "map_vars_fingerprint_sha256", "max_map_points"
  )
  valid_identity <- !inherits(metadata, "error") && nrow(metadata) == 1L &&
    all(identity_fields %in% names(metadata)) &&
    identical(as.character(metadata$workflow_contract[[1]]), "postgres-eda-bundle-2") &&
    identical(as.character(metadata$source_fingerprint_sha256[[1]]), as.character(source_fingerprint)) &&
    identical(as.character(metadata$spec_fingerprint_sha256[[1]]), as.character(spec_fingerprint)) &&
    identical(toupper(as.character(metadata$plots[[1]])), toupper(as.character(plots))) &&
    identical(as.integer(metadata$max_plot_levels[[1]]), as.integer(max_plot_levels)) &&
    identical(toupper(as.character(metadata$maps[[1]])), toupper(as.character(map_options$maps))) &&
    identical(
      as.character(metadata$map_vars_fingerprint_sha256[[1]]),
      as.character(eda_postgres_fingerprint(map_options$map_vars))
    ) &&
    identical(as.integer(metadata$max_map_points[[1]]), map_options$max_map_points)
  if (!valid_identity) stop("Prior bundle source, specification, plot options, or map options do not match this run.", call. = FALSE)
  invisible(TRUE)
}

eda_db_reconcile <- function(n_total, spec, missing, summaries, identifier_qa,
                             geo, plot_data) {
  if (!identical(missing$name, spec$name) || any(!is.na(missing$n) & missing$n != n_total)) stop("Database EDA missingness reconciliation failed.", call. = FALSE)
  present <- !is.na(summaries$variables$n)
  reconciled <- summaries$variables$n_missing[present] + summaries$variables$n_observed[present] == summaries$variables$n[present]
  if (any(is.na(reconciled) | !reconciled)) stop("Database EDA summary counts did not reconcile.", call. = FALSE)
  if (nrow(identifier_qa) > 0L) {
    ok <- is.na(identifier_qa$duplicate_excess) | identifier_qa$duplicate_excess == identifier_qa$n_observed - identifier_qa$n_distinct
    if (any(!ok)) stop("Database EDA identifier QA did not reconcile.", call. = FALSE)
  }
  eda_geo_reconcile(geo, n_total)
  plotted <- vapply(plot_data$entries, function(entry) if (is.null(entry$data)) 0L else sum(entry$data$count), integer(1))
  expected <- vapply(plot_data$entries, function(entry) entry$n_plotted, integer(1))
  if (any(plotted != expected)) stop("Database EDA compact plot counts did not reconcile.", call. = FALSE)
  invisible(TRUE)
}

eda_db_bundle_plot_inventory <- function(entries, create_plots) {
  rows <- list()
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    status <- if (create_plots && !is.null(entry$data)) "created" else "not_created"
    reason <- if (status == "created") NA_character_ else if (!create_plots && !is.null(entry$data)) "Plot creation was disabled by the caller." else entry$reason
    primary_path <- if (status == "created") sprintf("plots/%03d-%s.svg", index, entry$plot_type) else NA_character_
    rows[[length(rows) + 1L]] <- data.frame(
      variable_index = as.integer(index), name = entry$name, type = entry$type,
      plot_type = entry$plot_type, n_total = entry$n_total,
      n_missing = entry$n_missing, n_plotted = entry$n_plotted,
      n_excluded_non_finite = entry$n_excluded_non_finite,
      n_displayed_levels = entry$n_displayed_levels,
      n_collapsed_levels = entry$n_collapsed_levels,
      status = status, reason = reason, path = primary_path,
      stringsAsFactors = FALSE
    )
    if (!is.null(entry$box_data)) {
      rows[[length(rows) + 1L]] <- data.frame(
        variable_index = as.integer(index), name = entry$name, type = entry$type,
        plot_type = "quantile_box", n_total = entry$n_total,
        n_missing = entry$n_missing, n_plotted = entry$n_plotted,
        n_excluded_non_finite = entry$n_excluded_non_finite,
        n_displayed_levels = NA_integer_, n_collapsed_levels = NA_integer_,
        status = if (create_plots) "created" else "not_created",
        reason = if (create_plots) NA_character_ else "Plot creation was disabled by the caller.",
        path = if (create_plots) sprintf("plots/%03d-quantile-box.svg", index) else NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(eda_plot_inventory(list()))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_render_quantile_box <- function(entry) {
  data <- entry$box_data
  data$x <- entry$label
  data$whisker_min <- pmax(data$min, data$lower_fence)
  data$whisker_max <- pmin(data$max, data$upper_fence)
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$x, ymin = .data$whisker_min, lower = .data$q1,
      middle = .data$median, upper = .data$q3, ymax = .data$whisker_max
    )
  ) +
    ggplot2::geom_boxplot(stat = "identity") +
    ggplot2::labs(
      x = NULL, y = entry$label,
      title = paste0(entry$label, " aggregate quantile box"),
      subtitle = paste0("Below/above fences: ", data$n_below_lower, "/", data$n_above_upper)
    ) +
    ggplot2::theme_minimal()
}

eda_db_write_plots <- function(staging_dir, entries, rendered, inventory) {
  plots_dir <- file.path(staging_dir, "plots")
  if (!dir.create(plots_dir, showWarnings = FALSE)) stop("The database-EDA plot directory could not be created.", call. = FALSE)
  for (row_index in which(inventory$status == "created")) {
    row <- inventory[row_index, , drop = FALSE]
    entry <- entries[[row$variable_index[[1]]]]
    plot <- if (row$plot_type[[1]] == "quantile_box") eda_render_quantile_box(entry) else rendered[[entry$name]]
    path <- file.path(staging_dir, row$path[[1]])
    tryCatch(
      ggplot2::ggsave(path, plot = plot, device = grDevices::svg, width = 8, height = 5, units = "in"),
      error = function(error) stop("A database-EDA SVG could not be rendered safely.", call. = FALSE)
    )
  }
  invisible(TRUE)
}

eda_db_messages <- function(summaries, identifier_qa) {
  rows <- list()
  if (nrow(summaries$skipped) > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      stage = "aggregate_profiling", severity = "warning",
      subject = summaries$skipped$name, reason = summaries$skipped$reason,
      recommended_action = "Review the specification and PostgreSQL type guidance before interpreting the bundle.",
      stringsAsFactors = FALSE
    )
  }
  failed_identifier <- identifier_qa$status != "summarised"
  if (any(failed_identifier)) {
    rows[[length(rows) + 1L]] <- data.frame(
      stage = "identifier_qa", severity = "warning",
      subject = identifier_qa$name[failed_identifier],
      reason = identifier_qa$reason[failed_identifier],
      recommended_action = "Review the identifier declaration and source view before rerunning.",
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(intake_empty_messages())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_db_timings <- function(timing_env) {
  if (length(timing_env$rows) == 0L) {
    return(data.frame(
      stage = character(), variable_index = integer(), name = character(),
      query_kind = character(), elapsed_seconds = numeric(),
      rows_returned = integer(), bounded_limit = integer(), status = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, timing_env$rows)
  row.names(out) <- NULL
  out
}

eda_db_write_bundle_tables <- function(staging_dir,
                                       metadata,
                                       messages,
                                       spec,
                                       source_metadata,
                                       snapshot,
                                       plot_inventory,
                                       map_inventory,
                                       timings,
                                       layout) {
  tables <- list(
    run_metadata = metadata,
    messages = messages,
    specification = spec,
    source_metadata = source_metadata,
    schema = snapshot$schema,
    missing = snapshot$missing,
    summary_variables = snapshot$summaries$variables,
    summary_numeric = snapshot$summaries$numeric,
    summary_categorical = snapshot$summaries$categorical,
    summary_text = snapshot$summaries$text,
    summary_temporal = snapshot$summaries$temporal,
    summary_skipped = snapshot$summaries$skipped,
    identifier_qa = snapshot$identifier_qa,
    geo_qa = snapshot$geo,
    plot_inventory = plot_inventory,
    map_inventory = map_inventory,
    query_timings = timings
  )
  if (layout == "delivery") {
    tables$delivery_metadata <- data.frame(
      delivery_contract = "postgres-eda-delivery-1",
      layout = "delivery",
      report_path = "reports/eda-report.html",
      root_contract = "canonical-output-root-1",
      stringsAsFactors = FALSE
    )
  }
  registry <- eda_db_artifact_registry(layout)
  for (artifact in names(tables)) {
    relative_path <- registry$path[match(artifact, registry$artifact)]
    directory <- dirname(file.path(staging_dir, relative_path))
    directory_ready <- dir.exists(directory) ||
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!directory_ready) {
      stop("A database-EDA artifact directory could not be created.", call. = FALSE)
    }
    intake_atomic_csv(tables[[artifact]], file.path(staging_dir, relative_path))
  }
  invisible(TRUE)
}

eda_db_artifact_registry <- function(layout = c("bundle", "delivery")) {
  layout <- match.arg(layout)
  artifacts <- c(
    "manifest", "run_metadata", "messages", "specification", "source_metadata",
    "schema", "missing", "summary_variables", "summary_numeric",
    "summary_categorical", "summary_text", "summary_temporal",
    "summary_skipped", "identifier_qa", "geo_qa", "plot_inventory",
    "map_inventory", "query_timings"
  )
  types <- c(
    "manifest", "metadata", "messages", "specification", "source_metadata",
    "schema", "missingness", rep("canonical_summary", 6L), "identifier_qa",
    "geo_qa", "plot_inventory", "map_inventory", "query_timings"
  )
  paths <- paste0(artifacts, ".csv")
  paths[artifacts == "manifest"] <- "manifest.csv"
  if (layout == "delivery") {
    qa <- artifacts %in% c(
      "schema", "missing", "summary_variables", "summary_numeric",
      "summary_categorical", "summary_text", "summary_temporal",
      "summary_skipped", "identifier_qa", "geo_qa", "plot_inventory",
      "map_inventory"
    )
    paths[qa] <- file.path("QA_QC", paths[qa])
    paths[!qa] <- file.path("run_manifests", paths[!qa])
    artifacts <- c(artifacts, "delivery_metadata")
    types <- c(types, "delivery_metadata")
    paths <- c(paths, "run_manifests/delivery_metadata.csv")
  }
  data.frame(
    artifact = artifacts, type = types, path = paths,
    status = "created", checksum_md5 = "", stringsAsFactors = FALSE
  )
}

eda_db_manifest_relative_path <- function(layout = c("bundle", "delivery")) {
  layout <- match.arg(layout)
  if (layout == "delivery") "run_manifests/manifest.csv" else "manifest.csv"
}

eda_db_write_plot_data <- function(staging_dir, entries, layout) {
  empty <- data.frame(
    artifact = character(), type = character(), path = character(),
    status = character(), checksum_md5 = character(), stringsAsFactors = FALSE
  )
  if (layout != "delivery") {
    return(empty)
  }
  rows <- list()
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    components <- list()
    if (!is.null(entry$data)) components[[entry$plot_type]] <- entry$data
    if (!is.null(entry$box_data)) components[["quantile-box"]] <- entry$box_data
    for (component in names(components)) {
      relative_path <- sprintf("plot_data/%03d-%s.csv", index, component)
      directory <- file.path(staging_dir, "plot_data")
      if (!dir.exists(directory) && !dir.create(directory, showWarnings = FALSE)) {
        stop("The database-EDA plot-data directory could not be created.", call. = FALSE)
      }
      intake_atomic_csv(components[[component]], file.path(staging_dir, relative_path))
      rows[[length(rows) + 1L]] <- data.frame(
        artifact = sprintf("plot_data_%03d_%s", index, gsub("-", "_", component)),
        type = "plot_data", path = relative_path, status = "created",
        checksum_md5 = "", stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(empty)
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

eda_db_create_manifest <- function(staging_dir,
                                   plot_inventory,
                                   map_inventory,
                                   layout = c("bundle", "delivery"),
                                   plot_data_registry = NULL) {
  layout <- match.arg(layout)
  manifest <- eda_db_artifact_registry(layout)
  if (!is.null(plot_data_registry) && nrow(plot_data_registry) > 0L) {
    manifest <- rbind(manifest, plot_data_registry)
  }
  created_plots <- plot_inventory[plot_inventory$status == "created", , drop = FALSE]
  if (nrow(created_plots) > 0L) {
    dynamic <- data.frame(
      artifact = sprintf("plot_%03d_%s", created_plots$variable_index, created_plots$plot_type),
      type = "plot", path = created_plots$path, status = "created",
      checksum_md5 = "",
      stringsAsFactors = FALSE
    )
    manifest <- rbind(manifest, dynamic)
  }
  created_maps <- map_inventory[
    map_inventory$status == "created", ,
    drop = FALSE
  ]
  if (nrow(created_maps) > 0L) {
    dynamic <- data.frame(
      artifact = created_maps$map_id,
      type = "map",
      path = created_maps$path,
      status = "created",
      checksum_md5 = "",
      stringsAsFactors = FALSE
    )
    manifest <- rbind(manifest, dynamic)
  }
  if (anyDuplicated(manifest$artifact) || anyDuplicated(manifest$path)) {
    stop("Database-EDA artifacts did not have unique deterministic identifiers and paths.", call. = FALSE)
  }
  checked <- manifest$artifact != "manifest"
  manifest$checksum_md5[checked] <- unname(tools::md5sum(file.path(staging_dir, manifest$path[checked])))
  manifest
}

eda_db_validate_staged_bundle <- function(staging_dir, manifest) {
  all_entries <- list.files(staging_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  if (any(nzchar(Sys.readlink(all_entries)))) stop("The staged database-EDA bundle contains a symbolic link.", call. = FALSE)
  actual <- sort(list.files(staging_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE, include.dirs = FALSE))
  expected <- sort(manifest$path[manifest$status == "created"])
  if (!identical(actual, expected)) stop("The staged database-EDA bundle does not match its manifest.", call. = FALSE)
  paths <- file.path(staging_dir, expected)
  if (any(!utils::file_test("-f", paths))) stop("Every staged database-EDA artifact must be a regular file.", call. = FALSE)
  checked <- manifest$artifact != "manifest" & manifest$status == "created"
  actual_checksums <- unname(tools::md5sum(file.path(staging_dir, manifest$path[checked])))
  if (!identical(manifest$checksum_md5[checked], actual_checksums)) stop("The staged database-EDA checksums did not reconcile.", call. = FALSE)
  invisible(TRUE)
}
