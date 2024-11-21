############
library(openxlsx)

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
  base_name <- truncate_name(name)  # Ensure base name is truncated
  if (!(tolower(base_name) %in% tolower(names_so_far))) {
    return(base_name)
  } else {
    suffix <- 2
    unique_name <- paste0(base_name, "_", suffix)
    while (tolower(unique_name) %in% tolower(names_so_far)) {
      suffix <- suffix + 1
      unique_name <- paste0(base_name, "_", suffix)
    }
    return(unique_name)
  }
}
###


###
# Set the directory containing TSV files
path_to_files <- '../results/COVID19MEXICO_2021_2022_COVID-only_COISS-only/11_June_2024/uni_regressions_DiD/1_OR_and_chi/'

# List and count files directly:
file_names <- dir(path_to_files,
                 pattern = "\\.txt$",
                 full.names = TRUE,
                 all.files = TRUE,
                 recursive = FALSE,
                 ignore.case = FALSE,
                 include.dirs = FALSE
                 # no.. = FALSE
                 )
length(file_names)
head(file_names)

# Read files into a list of data frames:
files_list <- lapply(file_names, epi_read)
length(files_list)
lapply(files_list, epi_head_and_tail, cols = 4)

# Create a new Excel workbook:
wb <- openxlsx::createWorkbook()
###


###
# extension <- 'txt'
# Add each data frame as a separate sheet
used_sheet_names <- character()
for (i in seq_along(file_names)) {
  original_name <- sub("\\.txt$", "", basename(file_names[i]))  # Remove extension
  unique_name <- make_unique_name(original_name, used_sheet_names)
  used_sheet_names <- c(used_sheet_names, unique_name)  # Track names used

  addWorksheet(wb, unique_name)
  writeData(wb, sheet = unique_name, files_list[[i]])
}

# Save the workbook:
# base_name <- 'COVID19MEXICO2021_2022_COVID-only_COISS-only/1_general_desc/'
file_name <- '1_OR_and_chi'
file_name <- sprintf('%s/%s.xlsx', path_to_files, file_name)
file_name
saveWorkbook(wb, file = file_name,
             overwrite = TRUE
             )
############
