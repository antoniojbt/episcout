#' Run a specification-first EDA workflow
#'
#' Orchestrate the specification-first EDA helpers for either observed data or deterministic synthetic data. The function validates the specification, runs schema, missingness, summary and plot profiling, optionally writes machine-readable outputs, and returns all results as a named list.
#'
#' @param data A data frame containing observed or caller-declared synthetic data. Required when `synthetic = FALSE`; ignored when `synthetic = TRUE`.
#' @param spec An EDA specification data frame or CSV path accepted by [epi_eda_spec()].
#' @param output_dir Optional directory where machine-readable CSV outputs are written. The directory must already exist.
#' @param synthetic Logical; when `TRUE`, generate synthetic data from `spec` before running the workflow.
#' @param n Number of synthetic rows to generate when `synthetic = TRUE`.
#' @param seed Optional random seed passed to [epi_eda_generate_synthetic_data()].
#' @param maps Whether to create one geometry-only point map for every declared
#'   coordinate pair.
#' @param map_vars Unique declared variables for additional thematic maps.
#' @param max_map_points Inclusive maximum number of rows allowed for mapping.
#' @param plot_style Optional function passed to [epi_eda_profile_plots()].
#' @param data_origin Optional canonical origin for `data`. One of `"observed"`,
#'   `"episcout_generated_synthetic"` or `"caller_declared_synthetic"`.
#'   When omitted, `synthetic = FALSE` maps to `"observed"` and
#'   `synthetic = TRUE` maps to `"episcout_generated_synthetic"`.
#' @return A named list with `metadata`, `schema`, `missing`, `geo`,
#'   `summaries`, `categorical_display`, `plots`, `maps`, and `map_inventory`
#'   components.
#'
#' @export
epi_eda_run <- function(data,
                        spec,
                        output_dir = NULL,
                        synthetic = FALSE,
                        n = 100,
                        seed = NULL,
                        maps = FALSE,
                        map_vars = character(),
                        max_map_points = 10000L,
                        plot_style = NULL,
                        data_origin = NULL) {
  synthetic <- validate_run_eda_synthetic(synthetic)
  data_origin <- resolve_run_eda_data_origin(synthetic, data_origin, data)
  spec <- epi_eda_spec(spec)
  map_options <- eda_map_options(spec, maps, map_vars, max_map_points)

  if (synthetic) {
    data <- epi_eda_generate_synthetic_data(spec = spec, n = n, seed = seed)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame when synthetic is FALSE.", call. = FALSE)
  }
  eda_validate_map_columns(names(data), map_options)

  if (!is.null(output_dir)) {
    validate_run_eda_output_dir(output_dir)
  }

  plot_spec <- spec[spec$name %in% names(data), , drop = FALSE]
  geo <- epi_eda_profile_geo(data, spec)
  map_result <- eda_data_frame_maps(data, spec, geo, map_options)
  summaries <- epi_eda_profile_summaries(data, spec)
  results <- c(list(
    metadata = run_eda_metadata(
      data, spec,
      data_origin = data_origin, map_options = map_options
    ),
    schema = epi_eda_check_schema(data, spec),
    missing = epi_eda_profile_missing(data, spec),
    geo = geo,
    summaries = summaries,
    categorical_display = epi_eda_categorical_display(summaries),
    plots = epi_eda_profile_plots(data, plot_spec, plot_style = plot_style)
  ), map_result)

  if (!is.null(output_dir)) {
    write_run_eda_outputs(results, output_dir)
  }

  results
}

validate_run_eda_synthetic <- function(synthetic) {
  if (!is.logical(synthetic) || length(synthetic) != 1 || is.na(synthetic)) {
    stop("synthetic must be TRUE or FALSE.", call. = FALSE)
  }
  synthetic
}

resolve_run_eda_data_origin <- function(synthetic, data_origin, data) {
  origins <- c(
    "observed",
    "episcout_generated_synthetic",
    "caller_declared_synthetic"
  )

  if (is.null(data_origin)) {
    return(if (synthetic) origins[[2]] else origins[[1]])
  }
  if (!is.character(data_origin) || length(data_origin) != 1L ||
        is.na(data_origin) || !(data_origin %in% origins)) {
    stop(
      "data_origin must be NULL, 'observed', 'episcout_generated_synthetic' or 'caller_declared_synthetic'.",
      call. = FALSE
    )
  }
  if (identical(data_origin, "episcout_generated_synthetic") && !synthetic) {
    stop("episcout_generated_synthetic requires synthetic = TRUE.", call. = FALSE)
  }
  if (!identical(data_origin, "episcout_generated_synthetic") && synthetic) {
    stop("synthetic = TRUE requires episcout_generated_synthetic data_origin.", call. = FALSE)
  }
  if (identical(data_origin, "caller_declared_synthetic") && !is.data.frame(data)) {
    stop("caller_declared_synthetic requires a supplied data frame.", call. = FALSE)
  }

  data_origin
}

validate_run_eda_output_dir <- function(output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
    stop("output_dir must be NULL or a single directory path.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("output_dir must exist before epi_eda_run() writes outputs.", call. = FALSE)
  }

  invisible(TRUE)
}

run_eda_metadata <- function(data, spec, data_origin, map_options) {
  data.frame(
    synthetic = !identical(data_origin, "observed"),
    n_rows = as.integer(nrow(data)),
    n_columns = as.integer(ncol(data)),
    n_spec_variables = as.integer(nrow(spec)),
    maps = map_options$maps,
    map_vars = paste(map_options$map_vars, collapse = ";"),
    max_map_points = map_options$max_map_points,
    data_origin = data_origin,
    stringsAsFactors = FALSE
  )
}

write_run_eda_outputs <- function(results, output_dir) {
  utils::write.csv(
    results$metadata,
    file.path(output_dir, "metadata.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results$schema,
    file.path(output_dir, "schema.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results$missing,
    file.path(output_dir, "missing.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results$geo,
    file.path(output_dir, "geo_qa.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results$map_inventory,
    file.path(output_dir, "map_inventory.csv"),
    row.names = FALSE
  )
  for (name in names(results$summaries)) {
    utils::write.csv(
      results$summaries[[name]],
      file.path(output_dir, paste0("summary_", name, ".csv")),
      row.names = FALSE
    )
  }
  utils::write.csv(
    results$categorical_display,
    file.path(output_dir, "categorical_display.csv"),
    row.names = FALSE
  )
  eda_write_maps(results$maps, results$map_inventory, output_dir)

  invisible(TRUE)
}
