#' Render a specification-first EDA report
#'
#' Run the specification-first EDA workflow and render a bundled Quarto HTML
#' report from the workflow outputs.
#'
#' @param data A data frame containing observed data. Required when
#'   `synthetic = FALSE`; ignored when `synthetic = TRUE`.
#' @param spec An EDA specification data frame or CSV path accepted by
#'   [eda_spec()].
#' @param output_dir Directory where machine-readable workflow outputs and the
#'   rendered report are written. The directory must already exist.
#' @param synthetic Logical; when `TRUE`, generate synthetic data from `spec`
#'   before running the workflow.
#' @param n Number of synthetic rows to generate when `synthetic = TRUE`.
#' @param seed Optional random seed passed to [generate_synthetic_data()].
#' @param quiet Logical; passed to [rmarkdown::render()] to control render
#'   output.
#'
#' @return A single character string containing the rendered HTML report path.
#'
#' @export
render_eda_report <- function(data,
                              spec,
                              output_dir,
                              synthetic = FALSE,
                              n = 100,
                              seed = NULL,
                              quiet = TRUE) {
  validate_run_eda_output_dir(output_dir)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The rmarkdown package is required for render_eda_report().", call. = FALSE)
  }

  template_path <- system.file(
    "report-template",
    "eda.qmd",
    package = "episcout"
  )

  if (!nzchar(template_path) || !file.exists(template_path)) {
    stop("The bundled EDA report template could not be found.", call. = FALSE)
  }

  results <- run_eda(
    data = data,
    spec = spec,
    output_dir = output_dir,
    synthetic = synthetic,
    n = n,
    seed = seed
  )

  render_input <- file.path(tempdir(), "episcout-eda-report.qmd")
  file.copy(template_path, render_input, overwrite = TRUE)

  report_path <- rmarkdown::render(
    input = render_input,
    output_file = "eda-report.html",
    output_dir = output_dir,
    params = list(
      results = results,
      synthetic = isTRUE(results$metadata$synthetic[[1]])
    ),
    quiet = quiet,
    envir = new.env(parent = globalenv())
  )

  normalizePath(report_path, winslash = "/", mustWork = TRUE)
}
