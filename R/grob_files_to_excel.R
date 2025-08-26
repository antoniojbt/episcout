############
###
# Functions

# Truncate file names so they are max 31 chars to Excel's sheet name limit:
truncate_name <- function(name) {
  if (nchar(name) > 29) {
    substr(name, 1, 29) # to 29 because 2 characters may be added if there is a clash in names
  } else {
    name
  }
}

# Create unique sheet names:
make_unique_name <- function(name, names_so_far) {
  base_name <- truncate_name(name) # Ensure base name is truncated
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

#' Combine TSV files into an Excel workbook
#'
#' Read all tab-separated text files in a directory and store each in a
#' separate sheet of an Excel workbook. Sheet names are truncated to 31
#' characters and made unique.
#'
#' @param path_to_files Directory containing TSV files.
#' @param output_name Name of the output Excel file. If missing, `.xlsx` is
#' appended.
#'
#' @return Path to the created Excel workbook.
#' @export
#'
#' @examples
#' \dontrun{
#' grob_files_to_excel("data/results", "combined")
#' }
grob_files_to_excel <- function(path_to_files, output_name) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package openxlsx needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  file_names <- dir(
    path = path_to_files,
    pattern = "\\.(txt|tsv)$",
    full.names = TRUE,
    all.files = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

  if (length(file_names) == 0) {
    stop("No tab-separated files found in 'path_to_files'.", call. = FALSE)
  }

  files_list <- lapply(file_names, epi_read) # nolint

  wb <- openxlsx::createWorkbook()
  used_sheet_names <- character()

  for (i in seq_along(file_names)) {
    original_name <- sub("\\.[^.]+$", "", basename(file_names[i]))
    unique_name <- make_unique_name(original_name, used_sheet_names)
    used_sheet_names <- c(used_sheet_names, unique_name)

    openxlsx::addWorksheet(wb, unique_name)
    openxlsx::writeData(wb, sheet = unique_name, files_list[[i]])
  }

  if (!grepl("\\.xlsx$", output_name, ignore.case = TRUE)) {
    output_name <- paste0(output_name, ".xlsx")
  }

  out_path <- file.path(path_to_files, output_name)
  openxlsx::saveWorkbook(wb, file = out_path, overwrite = TRUE)
  out_path
}
