#' Save a Data Frame to a File with Custom Formatting
#'
#' This function constructs a file path using a specified subdirectory, file name, and suffix, then writes a data frame to that file. It is designed for flexibility in saving results with standardized naming conventions.
#'
#' @param df A data frame to be written to the file.
#' @param results_subdir A character string specifying the directory where the file will be saved.
#' @param file_n A character string specifying the base name of the file (without the suffix).
#' @param suffix A character string specifying the file extension (e.g., "txt", "csv").
#'
#' @return The full file path of the saved file.
#'         Prints a message indicating the file's location.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' results_subdir <- "output"
#' file_n <- "desc_dates"
#' suffix <- "txt"
#' epi_write_df(sum_dates_df, results_subdir, file_n, suffix)
#' }
#'
#' @export
epi_write_df <- function(df, results_subdir, file_n, suffix) {
  # Construct the file path
  outfile <- sprintf(
    fmt = "%s/%s.%s",
    results_subdir,
    file_n,
    suffix
  )

  # Write the data frame to the file
  epi_write(df, outfile)

  # Print the file path
  message("File saved to: ", outfile)

  # Return the full file path
  outfile
}
