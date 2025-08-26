#' Export text files to a single Excel workbook
#'
#' Reads text files from a directory and writes each as a separate sheet in a
#' new Excel workbook.
#'
#' @param path_to_files Directory containing the files to be combined.
#' @param pattern Regular expression used to match files. Defaults to
#'   `"\\.txt$"`.
#' @param output_file Path for the resulting workbook. Defaults to
#'   `file.path(path_to_files, "grob_files.xlsx")`.
#'
#' @return Invisibly returns the path to the saved workbook.
#' @details Sheet names are derived from file names, truncated to 29 characters
#'   to comply with Excel's limit. Numeric suffixes are appended to resolve
#'   clashes.
#' @examples
#' \dontrun{
#' grob_files_to_excel("path/to/files")
#' }
#' @export
grob_files_to_excel <- function(path_to_files,
                                pattern = "\\.txt$",
                                output_file = file.path(path_to_files, "grob_files.xlsx")) {
  file_names <- dir(
    path_to_files,
    pattern = pattern,
    full.names = TRUE,
    all.files = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )
  if (length(file_names) == 0) {
    stop("No files found matching pattern.")
  }
  files_list <- lapply(file_names, epi_read) # nolint: object_usage_linter
  wb <- openxlsx::createWorkbook()
  used_sheet_names <- character()
  for (i in seq_along(file_names)) {
    original_name <- sub(pattern, "", basename(file_names[i]))
    unique_name <- make_unique_name(original_name, used_sheet_names)
    used_sheet_names <- c(used_sheet_names, unique_name)
    openxlsx::addWorksheet(wb, unique_name)
    openxlsx::writeData(wb, sheet = unique_name, files_list[[i]])
  }
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)
  invisible(output_file)
}

#' @noRd
truncate_name <- function(name) {
  if (nchar(name) > 29) {
    substr(name, 1, 29)
  } else {
    name
  }
}

#' @noRd
make_unique_name <- function(name, names_so_far) {
  base_name <- truncate_name(name)
  if (!(tolower(base_name) %in% tolower(names_so_far))) {
    base_name
  } else {
    suffix <- 2
    unique_name <- paste0(base_name, "_", suffix)
    while (tolower(unique_name) %in% tolower(names_so_far)) {
      suffix <- suffix + 1
      unique_name <- paste0(base_name, "_", suffix)
    }
    unique_name
  }
}
