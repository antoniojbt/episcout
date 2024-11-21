#' Set up a Directory with a Date or Custom Name
#'
#' This function creates a `results` directory within a specified `project_root` folder, using either the current date (in `dd_mm_yyyy` format) or a custom string to create a unique subdirectory. If the directory already exists, it does not create a new one.
#' The function also lists the contents of the directory and returns the full path to the results directory.
#'
#' @param project_root A character string specifying the path to the project root directory where the directory will be created.
#' @param name Optional. A character string to use as the name of the subdirectory within the `results` directory. If not provided, the current date will be used.
#'
#' @return The full path to the created or existing results directory.
#'         Prints a message indicating whether the directory was created or already exists
#'         and lists the contents of the directory.
#'
#' @examples
#' \dontrun{
#' # Set up a results directory in the project root using the current date
#' project_root <- "path/to/project_root"
#' results_dir <- setup_results_dir(project_root)
#'
#' # Set up a results directory with a custom name
#' results_dir_custom <- setup_results_dir(project_root, name = "custom_analysis")
#' }
#'
#' @export
epi_create_dir <- function(project_root, name = NULL) {
  # Use the custom name if provided; otherwise, use the current date
  subdirectory_name <- if (!is.null(name)) name else format(Sys.Date(), "%d_%m_%Y")

  # Create the results folder path
  results_dir <- file.path(project_root, paste0("results/", subdirectory_name))

  # Create the directory if it doesn't exist
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
    message("Created directory: ", results_dir)
  } else {
    message("Directory already exists: ", results_dir)
  }

  # Print contents of the directory
  contents <- dir(path = normalizePath(results_dir), all.files = TRUE)
  print(contents)

  # Return the results directory path
  return(results_dir)
}
