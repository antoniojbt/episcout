# Function to convert true integer columns to factors, excluding IDate:
# TO DO: move to episcout
epi_clean_int_to_factor <- function(dt) { # dt as it's a data.table object
  for (col in names(dt)) {
    # Check if the column is integer and not an IDate:
    if (is.integer(dt[[col]]) && !inherits(dt[[col]], "IDate")) {
      dt[[col]] <- as.factor(dt[[col]])
    }
  }
  dt
}

# # Convert int cols to factors:
# data_f <- epi_clean_int_to_factor(data_f)
