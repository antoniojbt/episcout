# Drop levels with 0 as these are NAs

# Function to drop zero levels from a single factor vector
epi_clean_drop_zero_levels_vector <- function(factor_var) {
    # Ensure the input is a factor
    if (!is.factor(factor_var)) {
        stop("The input variable is not a factor.")
    }

    # Get the levels that are present in the factor
    present_levels <- levels(factor_var)[table(factor_var) > 0]

    # Drop levels that are zero
    cleaned_factor <- factor(factor_var, levels = present_levels)

    return(cleaned_factor)
}


# Sample factor vector
factor_var <- factor(c("a", "b", "a", "c", "b", "a", "b", "c"))

# Convert some values to NA (simulating the presence of zero levels)
factor_var[c(2, 5)] <- NA

# Apply the function to drop zero levels
cleaned_factor_var <- epi_clean_drop_zero_levels_vector(factor_var)

# Print the resulting factor
print(cleaned_factor_var)


# Sample data frame
df <- data.frame(
    col1 = factor(c("a", "b", "a", "c", "b")),
    col2 = factor(c("d", "d", "e", "f", "d")),
    col3 = factor(c("g", "h", "g", "g", "h"))
)

# Convert some values to NA (simulating the presence of zero levels)
df$col1[c(2, 5)] <- NA
df$col2[4] <- NA
df$col3[5] <- NA

# Apply the function to each factor column in the data frame
df[] <- lapply(df, function(column) {
    if (is.factor(column)) {
        epi_clean_drop_zero_levels_vector(column)
    } else {
        column
    }
})

# Print the resulting data frame
print(df)

