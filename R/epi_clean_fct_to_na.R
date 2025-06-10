# Loop through to recode NAs but preserve attributes:
# Function to convert specified factor values to NA
epi_clean_fct_to_na <- function(factor_var, values_to_na) {
  # Ensure the input is a factor:
  if (!is.factor(factor_var)) {
    stop("The input variable is not a factor.")
  }

  # Convert specified values to NA:
  levels_to_na <- levels(factor_var) %in% values_to_na
  factor_var[as.character(factor_var) %in% values_to_na] <- NA

  # Preserve the factor structure
  levels(factor_var)[levels_to_na] <- NA

  return(factor_var)
}

# # Apply the function to the factor variable in the data frame
# vec_test <- epi_clean_fct_to_na(data_f$SEXO, NA_labels)
# summary(vec_test)
#
# # Specify columns to modify:
# cols_mod <- colnames(df_ord)[7:40]
# str(df_ord)
#
# for (i in cols_mod) {
#     df_ord[[i]] <- epi_clean_fct_to_na(df_ord[[i]], NA_labels)
# }
