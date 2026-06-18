#' Create an episcout project scaffold
#'
#' Copy the bundled specification-first EDA project template into a destination
#' directory.
#'
#' @param path A single directory path where the project scaffold should be
#'   created.
#' @param overwrite Logical; when `FALSE`, existing destination files are left
#'   untouched and cause an error. When `TRUE`, existing destination files may
#'   be replaced by template files.
#'
#' @return The normalized project path, invisibly.
#'
#' @export
epi_eda_create_project <- function(path, overwrite = FALSE) {
  path <- validate_episcout_project_path(path)
  overwrite <- validate_project_overwrite(overwrite)

  template_path <- system.file("project-template", package = "episcout")
  if (!nzchar(template_path) || !dir.exists(template_path)) {
    stop("The bundled episcout project template could not be found.", call. = FALSE)
  }

  if (file.exists(path) && !dir.exists(path)) {
    stop("path exists and is not a directory.", call. = FALSE)
  }

  template_files <- list.files(
    template_path,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )

  destination_files <- file.path(path, template_files)
  existing_directories <- destination_files[dir.exists(destination_files)]
  if (length(existing_directories) > 0) {
    stop(
      "Destination paths exist as directories and cannot be replaced by files: ",
      paste(existing_directories, collapse = ", "),
      call. = FALSE
    )
  }

  existing_files <- destination_files[file.exists(destination_files)]
  if (length(existing_files) > 0 && !overwrite) {
    stop(
      "Destination files already exist; use overwrite = TRUE to replace them: ",
      paste(existing_files, collapse = ", "),
      call. = FALSE
    )
  }

  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!created) {
      stop("Could not create project directory.", call. = FALSE)
    }
  }

  for (relative_file in template_files) {
    source_file <- file.path(template_path, relative_file)
    destination_file <- file.path(path, relative_file)
    destination_dir <- dirname(destination_file)

    if (!dir.exists(destination_dir)) {
      created <- dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
      if (!created) {
        stop("Could not create project template directory.", call. = FALSE)
      }
    }

    copied <- file.copy(
      from = source_file,
      to = destination_file,
      overwrite = overwrite,
      copy.date = TRUE
    )
    if (!isTRUE(copied)) {
      stop("Could not copy project template file: ", relative_file, call. = FALSE)
    }
  }

  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

validate_episcout_project_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty directory path.", call. = FALSE)
  }

  path.expand(path)
}

validate_project_overwrite <- function(overwrite) {
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }

  overwrite
}
