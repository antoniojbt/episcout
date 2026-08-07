#' Save a Data Frame as CSV or TSV
#'
#' This function constructs a file path from an existing directory, file name
#' and suffix, then calls [epi_write()] with the delimiter required by the
#' suffix.
#'
#' @param df A data frame to be written to the file.
#' @param results_subdir One character string naming an existing directory.
#'   The directory is not created automatically.
#' @param file_n One non-empty character string specifying the base filename,
#'   without a directory or suffix.
#' @param suffix One character string specifying the filename extension. The
#'   supported values are `"csv"` (comma separated) and `"tsv"` (tab
#'   separated), matched without regard to case.
#' @param sep `NULL`, or the one-character delimiter corresponding to `suffix`.
#'   When `NULL`, the delimiter is inferred from `suffix`. A contradictory
#'   delimiter is rejected.
#'
#' @details Existing files are overwritten. As inherited from [epi_write()],
#'   column names are written, row names and quoting are disabled, and missing
#'   values are written as `NA`. Call [epi_write()] directly when different
#'   quoting or missing-value behaviour is required.
#'
#' @return The full file path of the saved file. Prints a message indicating the
#'   file's location.
#'
#' @examples
#' \dontrun{
#' results_subdir <- "output"
#' file_n <- "desc_dates"
#' suffix <- "tsv"
#' epi_write_df(sum_dates_df, results_subdir, file_n, suffix)
#' }
#'
#' @export
epi_write_df <- function(df, results_subdir, file_n, suffix, sep = NULL) {
  if (!is.character(results_subdir) ||
        length(results_subdir) != 1L ||
        is.na(results_subdir) ||
        !nzchar(results_subdir) ||
        !dir.exists(results_subdir)) {
    stop("`results_subdir` must be an existing directory.", call. = FALSE)
  }

  if (!is.character(file_n) ||
        length(file_n) != 1L ||
        is.na(file_n) ||
        !nzchar(file_n)) {
    stop("`file_n` must be one non-empty string.", call. = FALSE)
  }
  if (grepl("[/\\\\]", file_n)) {
    stop("`file_n` must not contain a directory path.", call. = FALSE)
  }

  if (!is.character(suffix) ||
        length(suffix) != 1L ||
        is.na(suffix) ||
        !nzchar(suffix)) {
    stop("`suffix` must be either \"csv\" or \"tsv\".", call. = FALSE)
  }

  suffix_key <- tolower(suffix)
  expected_sep <- switch(
    suffix_key,
    csv = ",",
    tsv = "\t",
    NULL
  )
  if (is.null(expected_sep)) {
    stop("`suffix` must be either \"csv\" or \"tsv\".", call. = FALSE)
  }

  if (!is.null(sep)) {
    if (!is.character(sep) ||
          length(sep) != 1L ||
          is.na(sep) ||
          nchar(sep, type = "bytes") != 1L) {
      stop("`sep` must be NULL or one character.", call. = FALSE)
    }
    if (!identical(sep, expected_sep)) {
      stop("`sep` must match the delimiter required by `suffix`.", call. = FALSE)
    }
  }

  outfile <- file.path(results_subdir, paste0(file_n, ".", suffix))

  epi_write(df, outfile, sep = expected_sep)

  message("File saved to: ", outfile)

  outfile
}
