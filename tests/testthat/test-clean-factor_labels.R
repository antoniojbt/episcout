context("episcout stats function tests")

######################
library(testthat)
library(dplyr)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")
######################

######################
# Set a test set:

# Define a sample dataframe:
sample_data_df <- data.frame(
  ORIGEN = c(1, 2, 99, 1),
  SECTOR = c(1, 2, 4, 5),
  stringsAsFactors = FALSE
)

# Define a sample lookup dataframe:
sample_lookup_df <- data.frame(
  variable = c("ORIGEN", "ORIGEN", "ORIGEN", "SECTOR", "SECTOR", "SECTOR"),
  level = as.character(c(1, 2, 99, 1, 2, 4)),
  label = c("USMER", "FUERA DE USMER", "NO ESPECIFICADO", "CRUZ ROJA", "DIF", "IMSS"),
  stringsAsFactors = FALSE
)
######################



######################
print("Function being tested: epi_clean_label")

test_that("epi_clean_label correctly applies factor levels and labels", {
  # Apply the function to the sample data
  result_df <- epi_clean_label(sample_data_df, sample_lookup_df)

  # Test if the ORIGEN column is correctly transformed into a factor with the correct levels and labels
  expect_is(result_df$ORIGEN, "factor")
  expect_equal(levels(result_df$ORIGEN), c("USMER", "FUERA DE USMER", "NO ESPECIFICADO"))
  expect_equal(
    as.character(result_df$ORIGEN),
    c("USMER", "FUERA DE USMER", "NO ESPECIFICADO", "USMER")
  )

  # Test if the SECTOR column is correctly transformed into a factor with the correct levels and labels
  expect_is(result_df$SECTOR, "factor")
  expect_equal(levels(result_df$SECTOR), c("CRUZ ROJA", "DIF", "IMSS"))
  expect_equal(as.character(result_df$SECTOR), c("CRUZ ROJA", "DIF", "IMSS", NA))
})

# Additional test to check handling of missing levels
test_that("epi_clean_label handles missing levels correctly", {
  # Introduce a SECTOR level that isn't in the lookup (e.g., level 5)
  altered_data_df <- sample_data_df
  altered_data_df$SECTOR[4] <- 5 # Level not in lookup

  # Expected to handle this by introducing NA for missing level
  result_df <- epi_clean_label(altered_data_df, sample_lookup_df)
  expect_true(is.na(result_df$SECTOR[4]))
})
######################


######################
#
######################

######################
#
######################

######################
#
######################
