#' Render a completed PostgreSQL EDA bundle
#'
#' Validate and render a completed aggregate-only PostgreSQL EDA bundle without
#' opening a database connection or reading source observations. The report and
#' README are published inside the same manifest-owned root.
#'
#' @param bundle An [epi_eda_db_run()] result or one local bundle-directory
#'   path. A result contributes only its `output_dir`; artifacts are re-read
#'   from disk.
#' @param overwrite Whether an existing owned database-EDA report and README
#'   may be replaced.
#' @param quiet Logical passed to [rmarkdown::render()].
#'
#' @return A normalized path to `reports/eda-report.html`.
#'
#' @details Rendering consumes only validated aggregate CSV and SVG artifacts.
#' It never uses the PostgreSQL connection retained by a run result. episcout
#' creates the outputs explicitly requested by the analyst and does not decide
#' whether they may be shared.
#'
#' @export
epi_eda_render_db_report <- function(bundle, overwrite = FALSE, quiet = TRUE) {
  intake_validate_flag(overwrite, "overwrite")
  intake_validate_flag(quiet, "quiet")
  eda_db_report_dependencies()
  output_dir <- eda_db_bundle_root(bundle)
  parsed <- eda_db_read_bundle(output_dir)
  has_report <- any(parsed$manifest$artifact %in% c("readme", "report"))
  if (has_report && !overwrite) {
    stop(
      "The database-EDA bundle already has an owned report; set overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  staging_dir <- tempfile(
    paste0(".", basename(output_dir), "-report-staging-"),
    tmpdir = dirname(output_dir)
  )
  if (!dir.create(staging_dir, showWarnings = FALSE)) {
    stop("A report staging directory could not be created beside the bundle.", call. = FALSE)
  }
  published <- FALSE
  on.exit(
    {
      if (!published && dir.exists(staging_dir)) {
        unlink(staging_dir, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  eda_db_copy_bundle(parsed, staging_dir)
  rendered <- eda_db_render_staged_bundle(
    staging_dir,
    quiet = quiet
  )

  state <- new.env(parent = emptyenv())
  state$target_dir <- output_dir
  state$staging_dir <- staging_dir
  state$published <- FALSE
  intake_publish_bundle(state)
  published <- TRUE
  normalizePath(
    file.path(output_dir, rendered$report_path),
    winslash = "/", mustWork = TRUE
  )
}

eda_db_report_dependencies <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
        !requireNamespace("knitr", quietly = TRUE)) {
    stop("The rmarkdown and knitr packages are required for database-EDA reports.", call. = FALSE)
  }
  if (!isTRUE(rmarkdown::pandoc_available())) {
    stop("Pandoc is required for database-EDA reports.", call. = FALSE)
  }
  template <- system.file(
    "report-template", "eda-db.qmd",
    package = "episcout"
  )
  if (!nzchar(template) || !file.exists(template) ||
        !utils::file_test("-f", template)) {
    stop("The bundled database-EDA report template could not be found.", call. = FALSE)
  }
  invisible(template)
}

eda_db_bundle_root <- function(bundle) {
  if (inherits(bundle, "epi_eda_db_run")) {
    bundle <- bundle$output_dir
  }
  if (!is.character(bundle) || length(bundle) != 1L || is.na(bundle) ||
        !nzchar(trimws(bundle))) {
    stop("bundle must be one database-EDA result or local directory path.", call. = FALSE)
  }
  requested <- path.expand(bundle)
  link <- Sys.readlink(requested)
  if (!is.na(link) && nzchar(link)) {
    stop("The database-EDA bundle root must not be a symbolic link.", call. = FALSE)
  }
  if (!dir.exists(requested)) {
    stop("The database-EDA bundle directory does not exist.", call. = FALSE)
  }
  normalizePath(requested, winslash = "/", mustWork = TRUE)
}

eda_db_read_bundle <- function(bundle) {
  output_dir <- eda_db_bundle_root(bundle)
  manifest_candidates <- c(
    bundle = "manifest.csv",
    delivery = "run_manifests/manifest.csv"
  )
  present <- file.exists(file.path(output_dir, manifest_candidates))
  if (sum(present) != 1L) {
    stop("The database-EDA bundle must contain one unambiguous manifest.", call. = FALSE)
  }
  layout <- names(manifest_candidates)[present]
  manifest_relative <- unname(manifest_candidates[present])
  manifest_path <- file.path(output_dir, manifest_relative)
  if (!utils::file_test("-f", manifest_path)) {
    stop("The database-EDA manifest must be a regular file.", call. = FALSE)
  }
  manifest <- tryCatch(
    utils::read.csv(
      manifest_path,
      check.names = FALSE, stringsAsFactors = FALSE,
      na.strings = character()
    ),
    error = identity
  )
  if (!inherits(manifest, "error") && "sensitivity" %in% names(manifest)) {
    stop(
      "The database-EDA manifest uses the removed sensitivity schema; regenerate it with the five-column core manifest before rendering.",
      call. = FALSE
    )
  }
  expected_names <- c("artifact", "type", "path", "status", "checksum_md5")
  valid_manifest <- !inherits(manifest, "error") &&
    identical(names(manifest), expected_names) && nrow(manifest) > 0L &&
    !anyNA(manifest) &&
    all(nzchar(manifest$artifact)) && all(nzchar(manifest$type)) &&
    all(nzchar(manifest$path)) &&
    !anyDuplicated(manifest$artifact) && !anyDuplicated(manifest$path) &&
    !anyDuplicated(tolower(manifest$path)) &&
    all(manifest$status %in% c("created", "not_created"))
  if (!valid_manifest) {
    stop("The database-EDA manifest is incompatible with the five-column contract.", call. = FALSE)
  }
  if (!all(vapply(manifest$path, eda_db_safe_relative_path, logical(1)))) {
    stop("The database-EDA manifest contains an unsafe relative path.", call. = FALSE)
  }
  self <- which(manifest$artifact == "manifest")
  if (length(self) != 1L || manifest$type[self] != "manifest" ||
        manifest$status[self] != "created" ||
        manifest$path[self] != manifest_relative ||
        !identical(as.character(manifest$checksum_md5[self]), "")) {
    stop("The database-EDA manifest does not identify itself correctly.", call. = FALSE)
  }
  if (any(manifest$status == "not_created" &
            nzchar(as.character(manifest$checksum_md5)))) {
    stop("Not-created database-EDA artifacts must not have checksums.", call. = FALSE)
  }

  all_entries <- list.files(
    output_dir,
    all.files = TRUE, no.. = TRUE, recursive = TRUE,
    include.dirs = TRUE, full.names = TRUE
  )
  if (any(nzchar(Sys.readlink(all_entries)))) {
    stop("The database-EDA bundle must not contain symbolic links.", call. = FALSE)
  }
  actual_files <- sort(list.files(
    output_dir,
    all.files = TRUE, no.. = TRUE, recursive = TRUE,
    include.dirs = FALSE
  ))
  expected_files <- sort(manifest$path[manifest$status == "created"])
  if (!identical(actual_files, expected_files)) {
    stop("Database-EDA bundle contents do not exactly match the manifest.", call. = FALSE)
  }
  actual_paths <- file.path(output_dir, actual_files)
  if (any(!utils::file_test("-f", actual_paths))) {
    stop("Database-EDA artifacts must all be regular files.", call. = FALSE)
  }
  actual_directories <- all_entries[file.info(all_entries)$isdir %in% TRUE]
  actual_directories <- sort(substring(
    normalizePath(actual_directories, winslash = "/", mustWork = TRUE),
    nchar(output_dir) + 2L
  ))
  owned_directories <- sort(unique(unlist(lapply(expected_files, function(path) {
    pieces <- strsplit(dirname(path), "/", fixed = TRUE)[[1]]
    if (identical(pieces, ".")) {
      return(character())
    }
    vapply(seq_along(pieces), function(index) {
      paste(pieces[seq_len(index)], collapse = "/")
    }, character(1))
  }), use.names = FALSE)))
  if (!identical(actual_directories, owned_directories)) {
    stop("Database-EDA bundle directories do not match manifest ownership.", call. = FALSE)
  }
  checked <- manifest$status == "created" & manifest$artifact != "manifest"
  recorded <- as.character(manifest$checksum_md5[checked])
  actual_checksums <- unname(tools::md5sum(file.path(
    output_dir, manifest$path[checked]
  )))
  if (any(!nzchar(recorded)) || !identical(recorded, actual_checksums)) {
    stop("Database-EDA bundle checksums do not match the manifest.", call. = FALSE)
  }

  registry <- eda_db_artifact_registry(layout)
  index <- match(registry$artifact, manifest$artifact)
  registry_valid <- !anyNA(index) &&
    identical(as.character(manifest$type[index]), as.character(registry$type)) &&
    identical(as.character(manifest$path[index]), as.character(registry$path)) &&
    all(manifest$status[index] == "created")
  if (!registry_valid) {
    stop("The database-EDA bundle is missing required aggregate artifacts.", call. = FALSE)
  }
  extra <- !manifest$artifact %in% registry$artifact
  supported_types <- c("plot", "map", "plot_data", "index", "report")
  if (any(extra) && !all(manifest$type[extra] %in% supported_types)) {
    stop("The database-EDA manifest contains an unsupported artifact type.", call. = FALSE)
  }
  plot_rows <- manifest$type == "plot"
  map_rows <- manifest$type == "map"
  plot_data_rows <- manifest$type == "plot_data"
  dynamic_valid <- all(manifest$status[extra] == "created") &&
    all(grepl("^plot_[0-9]{3,}_[A-Za-z0-9_]+$", manifest$artifact[plot_rows])) &&
    all(grepl("^plots/[0-9]{3,}-[A-Za-z0-9_-]+\\.svg$", manifest$path[plot_rows])) &&
    all(grepl("^map-p[0-9]{3,}-(geometry|v[0-9]{3,})$", manifest$artifact[map_rows])) &&
    all(manifest$path[map_rows] == paste0("maps/", manifest$artifact[map_rows], ".svg")) &&
    all(grepl("^plot_data_[0-9]{3,}_[A-Za-z0-9_]+$", manifest$artifact[plot_data_rows])) &&
    all(grepl("^plot_data/[0-9]{3,}-[A-Za-z0-9_-]+\\.csv$", manifest$path[plot_data_rows])) &&
    (!any(plot_data_rows) || layout == "delivery") &&
    !any(
      manifest$type %in% c("index", "report") &
        !manifest$artifact %in% c("readme", "report")
    )
  if (!dynamic_valid) {
    stop("The database-EDA manifest contains incompatible dynamic artifacts.", call. = FALSE)
  }
  report_rows <- manifest$artifact %in% c("readme", "report")
  if (any(report_rows)) {
    expected_report <- data.frame(
      artifact = c("readme", "report"),
      type = c("index", "report"),
      path = c("README.md", "reports/eda-report.html"),
      stringsAsFactors = FALSE
    )
    report_index <- match(expected_report$artifact, manifest$artifact)
    observed <- manifest[
      report_index, c("artifact", "type", "path"),
      drop = FALSE
    ]
    row.names(observed) <- NULL
    if (anyNA(observed) || !identical(observed, expected_report)) {
      stop("The database-EDA report ownership rows are incomplete.", call. = FALSE)
    }
  }

  tables <- eda_db_read_tables(output_dir, manifest, registry$artifact)
  eda_db_validate_bundle_tables(tables, manifest, layout)
  list(
    output_dir = output_dir, layout = layout, manifest = manifest,
    manifest_path = manifest_relative, tables = tables
  )
}

eda_db_safe_relative_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
        !nzchar(path) || grepl("\\\\", path) || grepl("^/", path) ||
        grepl("^[A-Za-z]:", path) || !grepl("^[A-Za-z0-9._/-]+$", path)) {
    return(FALSE)
  }
  pieces <- strsplit(path, "/", fixed = TRUE)[[1]]
  length(pieces) > 0L && all(nzchar(pieces)) &&
    !any(pieces %in% c(".", ".."))
}

eda_db_read_tables <- function(output_dir, manifest, artifacts) {
  artifacts <- setdiff(artifacts, "manifest")
  tables <- lapply(artifacts, function(artifact) {
    index <- match(artifact, manifest$artifact)
    tryCatch(
      utils::read.csv(
        file.path(output_dir, manifest$path[index]),
        check.names = FALSE,
        stringsAsFactors = FALSE, na.strings = character()
      ),
      error = function(error) {
        stop("A required database-EDA aggregate CSV could not be read.", call. = FALSE)
      }
    )
  })
  names(tables) <- artifacts
  tables
}

eda_db_validate_bundle_tables <- function(tables, manifest, layout) {
  required <- list(
    run_metadata = c("workflow_contract", "status", "n_rows", "n_spec_variables"),
    messages = c("stage", "severity", "subject", "reason", "recommended_action"),
    specification = c("name", "label", "type", "role"),
    source_metadata = c("relation_kind", "column_count", "source_contract"),
    schema = "name",
    missing = c("name", "n", "n_missing", "p_missing"),
    summary_variables = c("name", "n", "n_missing", "n_observed", "status", "reason"),
    summary_numeric = c("name", "n_finite", "min", "mean", "max"),
    summary_categorical = c("name", "level", "n", "p_total", "p_observed"),
    summary_text = c("name", "n", "n_missing", "n_observed", "n_unique"),
    summary_temporal = c("name", "n", "n_missing", "n_observed", "range_unit"),
    summary_skipped = c("name", "type", "reason"),
    identifier_qa = c("name", "status", "reason"),
    geo_qa = c("geo_pair", "status", "reason"),
    plot_inventory = c("variable_index", "name", "status", "reason", "path"),
    map_inventory = c(
      "map_id", "geo_pair", "value", "status", "reason", "n_source_rows",
      "n_mapped", "path"
    ),
    query_timings = c("stage", "query_kind", "elapsed_seconds", "status")
  )
  valid <- all(vapply(names(required), function(name) {
    is.data.frame(tables[[name]]) && all(required[[name]] %in% names(tables[[name]]))
  }, logical(1)))
  metadata <- tables$run_metadata
  scalar_metadata <- is.data.frame(metadata) && nrow(metadata) == 1L &&
    all(c("n_rows", "n_spec_variables") %in% names(metadata))
  n_rows <- if (scalar_metadata) {
    suppressWarnings(as.integer(metadata$n_rows[[1]]))
  } else {
    NA_integer_
  }
  n_spec <- if (scalar_metadata) {
    suppressWarnings(as.integer(metadata$n_spec_variables[[1]]))
  } else {
    NA_integer_
  }
  valid <- valid && nrow(metadata) == 1L &&
    identical(as.character(metadata$workflow_contract[[1]]), "postgres-eda-bundle-2") &&
    identical(as.character(metadata$status[[1]]), "complete") &&
    !is.na(n_rows) && n_rows >= 0L &&
    !is.na(n_spec) && n_spec == nrow(tables$specification) &&
    identical(as.character(tables$missing$name), as.character(tables$specification$name)) &&
    identical(as.character(tables$summary_variables$name), as.character(tables$specification$name)) &&
    isTRUE(all(
      is.na(tables$missing$n) | as.integer(tables$missing$n) == n_rows
    )) &&
    identical(
      as.integer(tables$missing$n_missing),
      as.integer(tables$summary_variables$n_missing)
    ) &&
    isTRUE(all(
      is.na(tables$summary_variables$n) |
        as.integer(tables$summary_variables$n) == n_rows
    )) &&
    isTRUE(all(
      is.na(tables$summary_variables$n) |
        as.integer(tables$summary_variables$n_missing) +
          as.integer(tables$summary_variables$n_observed) ==
          as.integer(tables$summary_variables$n)
    ))
  plot_paths <- eda_db_inventory_paths(tables$plot_inventory)
  map_paths <- eda_db_inventory_paths(tables$map_inventory)
  inventory_statuses <- c("created", "not_created")
  valid <- valid &&
    isTRUE(all(tables$plot_inventory$status %in% inventory_statuses)) &&
    isTRUE(all(tables$map_inventory$status %in% inventory_statuses)) &&
    isTRUE(all(
      tables$map_inventory$status != "not_created" |
        as.integer(tables$map_inventory$n_mapped) == 0L
    )) && identical(
    sort(plot_paths), sort(manifest$path[
      manifest$type == "plot" & manifest$status == "created"
    ])
  ) && identical(
    sort(map_paths), sort(manifest$path[
      manifest$type == "map" & manifest$status == "created"
    ])
  )
  if (layout == "delivery") {
    delivery <- tables$delivery_metadata
    valid <- valid && is.data.frame(delivery) && nrow(delivery) == 1L &&
      all(c(
        "delivery_contract", "layout", "report_path", "root_contract"
      ) %in% names(delivery)) &&
      identical(
        as.character(delivery$delivery_contract[[1]]),
        "postgres-eda-delivery-1"
      ) &&
      identical(as.character(delivery$layout[[1]]), "delivery") &&
      identical(
        as.character(delivery$report_path[[1]]),
        "reports/eda-report.html"
      ) &&
      identical(
        as.character(delivery$root_contract[[1]]),
        "canonical-output-root-1"
      )
  }
  if (!valid) {
    stop("The database-EDA aggregate schemas are incomplete or inconsistent.", call. = FALSE)
  }
  invisible(TRUE)
}

eda_db_inventory_paths <- function(inventory) {
  paths <- as.character(inventory$path[inventory$status == "created"])
  if (anyNA(paths) || any(!nzchar(paths))) {
    return(NA_character_)
  }
  paths
}

eda_db_copy_bundle <- function(bundle, staging_dir) {
  files <- bundle$manifest$path[bundle$manifest$status == "created"]
  for (relative_path in files) {
    destination <- file.path(staging_dir, relative_path)
    directory <- dirname(destination)
    directory_ready <- dir.exists(directory) ||
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    if (!directory_ready) {
      stop("A report staging directory could not be prepared.", call. = FALSE)
    }
    if (!file.copy(
      file.path(bundle$output_dir, relative_path), destination,
      overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE
    )) {
      stop("A database-EDA artifact could not be copied into report staging.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

eda_db_render_staged_bundle <- function(staging_dir, quiet) {
  parsed <- eda_db_read_bundle(staging_dir)
  manifest <- parsed$manifest
  template <- eda_db_report_dependencies()
  reports_dir <- file.path(staging_dir, "reports")
  reports_ready <- dir.exists(reports_dir) ||
    dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
  if (!reports_ready) {
    stop("The database-EDA report directory could not be created.", call. = FALSE)
  }
  render_dir <- tempfile("episcout-db-report-")
  if (!dir.create(render_dir, showWarnings = FALSE)) {
    stop("The database-EDA report input could not be staged.", call. = FALSE)
  }
  on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)
  render_input <- file.path(render_dir, "eda-db.qmd")
  if (!file.copy(template, render_input, overwrite = FALSE)) {
    stop("The database-EDA report input could not be staged.", call. = FALSE)
  }
  params <- eda_db_report_params(parsed)
  report_path <- tryCatch(
    rmarkdown::render(
      input = render_input,
      output_format = rmarkdown::html_document(
        self_contained = TRUE, toc = TRUE, theme = "default"
      ),
      output_file = "eda-report.html",
      output_dir = reports_dir,
      params = list(bundle = params),
      quiet = quiet,
      envir = new.env(parent = baseenv())
    ),
    error = function(error) {
      stop("The database-EDA HTML report could not be rendered safely.", call. = FALSE)
    }
  )
  if (!file.exists(report_path) || !utils::file_test("-f", report_path)) {
    stop("The database-EDA HTML report was not created.", call. = FALSE)
  }
  intake_atomic_text(eda_db_report_readme(), file.path(staging_dir, "README.md"))

  manifest <- manifest[!manifest$artifact %in% c("readme", "report"), , drop = FALSE]
  manifest <- rbind(
    manifest,
    data.frame(
      artifact = c("readme", "report"), type = c("index", "report"),
      path = c("README.md", "reports/eda-report.html"), status = "created",
      checksum_md5 = "", stringsAsFactors = FALSE
    )
  )
  checked <- manifest$status == "created" & manifest$artifact != "manifest"
  manifest$checksum_md5[checked] <- unname(tools::md5sum(file.path(
    staging_dir, manifest$path[checked]
  )))
  manifest_relative <- parsed$manifest_path
  intake_atomic_csv(manifest, file.path(staging_dir, manifest_relative))
  eda_db_validate_staged_bundle(staging_dir, manifest)
  eda_db_read_bundle(staging_dir)
  list(manifest = manifest, report_path = "reports/eda-report.html")
}

eda_db_report_params <- function(bundle) {
  plot_rows <- bundle$manifest[
    bundle$manifest$type == "plot" & bundle$manifest$status == "created",
    c("artifact", "path"),
    drop = FALSE
  ]
  map_rows <- bundle$manifest[
    bundle$manifest$type == "map" & bundle$manifest$status == "created",
    c("artifact", "path"),
    drop = FALSE
  ]
  evidence_manifest <- bundle$manifest[
    !bundle$manifest$artifact %in% c("readme", "report"), ,
    drop = FALSE
  ]
  list(
    layout = bundle$layout,
    tables = bundle$tables,
    manifest = evidence_manifest,
    plots = plot_rows,
    maps = map_rows
  )
}

eda_db_report_readme <- function() {
  c(
    "# episcout EDA delivery",
    "",
    "Open [the EDA report](reports/eda-report.html) for the human-facing summary.",
    "",
    "Run status: complete.",
    "",
    "- `QA_QC/` (or the root of a legacy bundle) contains aggregate checks and summaries.",
    "- `plots/` and `maps/` contain deterministic SVG outputs when requested and created.",
    "- `plot_data/` contains compact aggregate inputs for database plots in delivery layouts.",
    "- `run_manifests/` (or the root of a legacy bundle) contains provenance, timings, and the checksum manifest.",
    "",
    "CSV, SVG, checksums, and provenance remain the canonical evidence behind the report.",
    "episcout creates the outputs explicitly requested by the analyst and does not decide whether they may be shared."
  )
}
