#############
# Descriptive stats for dates
# Range, min, max,etc
# Example data (already in testthat functions)
# test_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01", "2021-07-15"))

# Set seed for reproducibility
set.seed(42)

# Define the start and end dates
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2023-12-31")

# Calculate the difference in days between start and end dates
days_between <- as.integer(end_date - start_date)
days_between

# Generate 300 random numbers within the range of days
random_days <- sample(0:days_between, 300, replace = TRUE)

# Add these random days to the start date to get random dates
random_dates <- start_date + random_days
random_dates

# Sort the dates
sorted_random_dates <- sort(random_dates)
sorted_random_dates

# Convert to character for display or further processing
sorted_random_dates_str <- as.character(sorted_random_dates)
sorted_random_dates_str

# Print the first 10 sorted random dates
sorted_random_dates_str[1:10]
str(sorted_random_dates_str)
sorted_random_dates_str <- as.Date(sorted_random_dates_str)
str(sorted_random_dates_str)

test_dates <- sorted_random_dates_str
#############


#############
#' @title Calculate Descriptive Date Statistics
#'
#' @description epi_stats_dates() calculates and returns key descriptive statistics for a vector of dates, including minimum, maximum, median, interquartile range (IQR), and the quartiles. It is compatible with both `Date` and `IDate` objects.
#'
#' @param date_vector A vector of dates of class `Date` or `IDate`.
#' @return A `data.frame` containing the following statistics:
#'   \itemize{
#'     \item Min: The earliest date in the vector.
#'     \item Max: The latest date in the vector.
#'     \item Median: The median date.
#'     \item IQR: The interquartile range of the dates.
#'     \item 1st Quartile: The first quartile (25th percentile).
#'     \item Median (again): The median (50th percentile), duplicated for clarity.
#'     \item 3rd Quartile: The third quartile (75th percentile).
#'   }
#'
##' @examples
##' sample_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01", "2021-07-15"))
##' date_stats <- epi_stats_dates(sample_dates)
##' print(date_stats)
#'
#'
#'
#' @note Note that origin date by default in R is "1970-01-01".
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
# TO DO:
#' @seealso \code{\link{epi_stats_summary}},
#' \code{\link{epi_stats_format}},
#' \code{\link{epi_stats_numeric}}.
#'
# TO DO:
##' @example vignettes/summary_funcs_examples.R
#'
#' @export
#'

epi_stats_dates <- function(date_vector) {
    # Ensure the input is a Date or IDate class:
    # date_vector <- data_f$FECHA_INGRESO
    if (!inherits(date_vector, "Date") && !inherits(date_vector, "IDate")) {
        stop("Input must be a vector of type Date or IDate.")
    }

    # Convert IDate to Date if necessary
    if (inherits(date_vector, "IDate")) {
        date_vector <- as.Date(date_vector)
    }

    # Calculate various statistics
    min_date <- min(date_vector, na.rm = TRUE)
    max_date <- max(date_vector, na.rm = TRUE)
    # median_date <- median(date_vector, na.rm = TRUE)
    iqr_value <- IQR(date_vector, na.rm = TRUE)
    # Get quantiles from dates:
    # Convert dates to numeric, origin date by default in R is "1970-01-01":
    quantiles_numeric <- quantile(as.numeric(date_vector), na.rm = TRUE)
    # Convert numeric quantiles back to dates
    quartiles_dates <- as.Date(quantiles_numeric,
                               origin = "1970-01-01",
                               probs = c(0, 0.25, 0.5, 0.75, 100),
                               na.rm = TRUE
                               )
    # quartiles_dates[3]

    # Create a dataframe to return results
    sum_stats <- data.frame(
        Statistic = c("Min", "25%", "Median", "75%", "Max", "IQR"),
        Value = c(as.character(min_date),
                  as.character(quartiles_dates[2]),
                  as.character(quartiles_dates[3]),
                  as.character(quartiles_dates[4]),
                  as.character(max_date),
                  as.character(iqr_value)
        )
    )

    return(sum_stats)
}
#############

#############
# TO DO: Move to a function
# Loop:
sum_dates_df <- data.frame('variable' = character(0),
                           'Min' = numeric(0),
                           'q25%' = numeric(0),
                           'Median' = numeric(0),
                           'q75%' = numeric(0),
                           'Max' = numeric(0),
                           'IQR' = numeric(0),
                           stringsAsFactors = FALSE
)
# sum_dates_df

for (i in col_dates) {
    sum_dates <- epi_stats_dates(data_f[[i]])
    # print(sum_dates)
    stats_vector <- sum_dates$Value  # Extract the values

    # Data frame row to append:
    new_row <- data.frame(
        Variable = i,
        Min = stats_vector[1],
        `q25%` = stats_vector[2],
        Median = stats_vector[3],
        `q75%` = stats_vector[4],
        Max = stats_vector[5],
        IQR = stats_vector[6]
    )
    sum_dates_df <- rbind(sum_dates_df,
                          new_row)
}
sum_dates_df
file_n <- 'COISS_02052024/desc_dates.txt'
epi_write(sum_dates_df, file_n)
#############


############
# Get frequency tables
dates_list <- list()
for (i in col_dates) {
    # Calculate differences between consecutive dates to find gaps and clusters:
    date_ord <- as.numeric(data_f[[i]])
    inds <- order(date_ord,
                  decreasing = FALSE,
                  na.last = TRUE
    )
    date_ord <- date_ord[inds]
    date_diffs <- diff(date_ord)
    # date_diffs
    # Convert numeric differences back to an interpretable form (e.g., days):
    # date_diffs_days <- as.Date(date_diffs, origin = "1970-01-01") - as.Date("1970-01-01") # although diff() was already working on days, so same result
    # date_diffs_days
    # Frequency table by month-year:
    date_frequencies <- table(format(data_f[[i]], "%Y-%m"))  # Counts by year and month
    # date_frequencies
    dates_list[[i]] <- list(
        Date_Differences = date_diffs,
        Frequencies = date_frequencies
    )
}
names(dates_list)
names(dates_list$FECHA_INGRESO)


# Save:
for (i in names(dates_list)) {
    # print(i)
    out_n <- sprintf('freq_%s_%s.txt', i, "Date_Differences")
    # dates_list$FECHA_ACTUALIZACION$Date_Differences
    file_out <- as.data.frame(dates_list[[i]][[1]])
    epi_write(file_out, out_n)

    out_n <- sprintf('freq_%s_%s.txt', i, "Frequencies")
    # dates_list$FECHA_ACTUALIZACION$Frequencies
    file_out <- as.data.frame(dates_list[[i]][[2]])
    epi_write(file_out, out_n)
}

############

#############
#

#############
