#' Run a specification-first EDA workflow
#'
#' Orchestrate the specification-first EDA helpers for either observed data or
#' deterministic synthetic data. The function validates the specification, runs
#' schema, missingness, summary and plot profiling, optionally writes
#' machine-readable outputs, and returns all results as a named list.
#'
#' @param data A data frame containing observed data. Required when
#'   `synthetic = FALSE`; ignored when `synthetic = TRUE`.
#' @param spec An EDA specification data frame or CSV path accepted by
#'   [epi_eda_spec()].
#' @param output_dir Optional directory where machine-readable CSV outputs are
#'   written. The directory must already exist.
#' @param synthetic Logical; when `TRUE`, generate synthetic data from `spec`
#'   before running the workflow.
#' @param n Number of synthetic rows to generate when `synthetic = TRUE`.
#' @param seed Optional random seed passed to [epi_eda_generate_synthetic_data()].
#'
#' @return A named list with `metadata`, `schema`, `missing`, `summaries` and
#'   `plots` components.
#'
#' @export
epi_eda_run <- function(data,
                        spec,
                        output_dir = NULL,
                        synthetic = FALSE,
                        n = 100,
                        seed = NULL) {
  synthetic <- validate_run_eda_synthetic(synthetic)
  spec <- epi_eda_spec(spec)

  if (synthetic) {
    data <- epi_eda_generate_synthetic_data(spec = spec, n = n, seed = seed)
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame when synthetic is FALSE.", call. = FALSE)
  }

  if (!is.null(output_dir)) {
    validate_run_eda_output_dir(output_dir)
  }

  results <- list(
    metadata = run_eda_metadata(data, spec, synthetic = synthetic),
    schema = epi_eda_check_schema(data, spec),
    missing = epi_eda_profile_missing(data, spec),
    summaries = epi_eda_profile_summaries(data, spec),
    plots = epi_eda_profile_plots(data, spec)
  )

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

validate_run_eda_output_dir <- function(output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
    stop("output_dir must be NULL or a single directory path.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("output_dir must exist before epi_eda_run() writes outputs.", call. = FALSE)
  }

  invisible(TRUE)
}

run_eda_metadata <- function(data, spec, synthetic) {
  data.frame(
    synthetic = synthetic,
    n_rows = as.integer(nrow(data)),
    n_columns = as.integer(ncol(data)),
    n_spec_variables = as.integer(nrow(spec)),
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
    results$summaries$numeric,
    file.path(output_dir, "summary_numeric.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results$summaries$categorical,
    file.path(output_dir, "summary_categorical.csv"),
    row.names = FALSE
  )

  invisible(TRUE)
}
