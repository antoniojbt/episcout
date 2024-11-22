#' Create a Directory and Nested Subdirectories
#'
#' This function creates a directory at the specified path, including nested subdirectories if needed.
#' If the directory already exists, it does nothing. The directory name can include subdirectories (e.g., `results/today_xxx/`), and these will be created recursively.
#' The subdirectory can either be a custom name or the current date (`dd_mm_yyyy` format).
#'
#' @param base_path A character string specifying the base path where the directory will be created.
#' @param subdir Optional. A character string specifying the name of the subdirectory. If not provided, the current date will be used. Can include nested paths (e.g., `results/today_xxx`).
#'
#'
#' @return The full path to the created or existing directory.
#'         Prints a message indicating whether the directory was created or already existed. Also prints the contents.
#'
#' @examples
#' \dontrun{
#' # Create a directory named with the current date
#' base_dir <- "path/to/base_directory"
#' dated_dir <- epi_create_dir(base_dir)
#'
#' # Create a directory with a custom name
#' custom_dir <- epi_create_dir(base_dir, subdir = "custom_name")
#'
#' # Create a nested directory with today's date
#' nested_dir <- epi_create_dir(base_dir, subdir = "results/today_xxx")
#' }
#'
#' @export
#'

epi_create_dir <- function(base_path, subdir = NULL) {
   # Use the custom name if provided; otherwise, use the current date
  subdirectory_name <- if (!is.null(subdir)) subdir else format(Sys.Date(), "%d_%m_%Y")

  # Combine base path and subdirectory
  target_dir <- file.path(base_path, subdirectory_name)

  # Create the directory recursively if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    message("Created directory: ", normalizePath(target_dir))
  } else {
    message("Directory already exists: ", normalizePath(target_dir))
  }

  # Print the contents of the directory
  contents <- dir(path = normalizePath(target_dir), all.files = TRUE)
  print(contents)

  # Return the full path of the target directory
  return(normalizePath(target_dir))
}


