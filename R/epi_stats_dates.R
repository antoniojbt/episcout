#############
#' @title Calculate Descriptive Date Statistics
#'
#' @description Calculates and returns key descriptive statistics for a vector of
#' dates, including minimum, maximum, interquartile range (IQR), and quartiles. It
#' is compatible with both `Date` and `IDate` objects.
#'
#' @param date_vector A vector of dates of class `Date` or `IDate`.
#'
#' @return A `data.frame` containing statistics such as N, N Missing, N Unique, Min,
#'   25%, Median, 75%, Max, IQR, Most Common, and Range (Days).
#'
#' @examples
#' sample_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01"))
#' epi_stats_dates(sample_dates)
#'
#' @note The default origin for numeric date conversion in R is "1970-01-01".
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}},
#'   \code{\link{epi_stats_format}},
#'   \code{\link{epi_stats_numeric}}
#'
#' @export
#'
#' @importFrom stats IQR quantile

epi_stats_dates <- function(date_vector) {
  if (!inherits(date_vector, "Date") && !inherits(date_vector, "IDate")) {
    stop("Input must be a vector of type Date or IDate.")
  }
  if (inherits(date_vector, "IDate")) {
    date_vector <- as.Date(date_vector)
  }

  min_date <- min(date_vector, na.rm = TRUE)
  max_date <- max(date_vector, na.rm = TRUE)
  iqr_value <- IQR(date_vector, na.rm = TRUE)
  quantiles_numeric <- quantile(as.numeric(date_vector), na.rm = TRUE)
  quartiles_dates <- as.Date(
    quantiles_numeric,
    origin = "1970-01-01",
    probs = c(0, 0.25, 0.5, 0.75, 100),
    na.rm = TRUE
  )

  sum_stats <- data.frame(
    Statistic = c(
      "N", "N Missing", "N Unique", "Min", "25%", "Median", "75%", "Max",
      "IQR", "Most Common", "Range (Days)"
    ),
    Value = c(
      as.character(length(date_vector)),
      as.character(sum(is.na(date_vector))),
      as.character(dplyr::n_distinct(date_vector)),
      as.character(min_date),
      as.character(quartiles_dates[2]),
      as.character(quartiles_dates[3]),
      as.character(quartiles_dates[4]),
      as.character(max_date),
      as.character(iqr_value),
      names(which.max(table(date_vector))),
      max_date - min_date
    )
  )

  sum_stats
}
#############

#' @title Summarise Multiple Date Columns
#'
#' @description Applies [epi_stats_dates()] to each date column in a data frame and
#'   returns a wide-format tibble with the results.
#'
#' @param df A data frame containing one or more date columns.
#'
#' @return A tibble where each row corresponds to a date column and columns contain
#'   the statistics produced by [epi_stats_dates()].
#'
#' @param df A dataframe containing multiple date columns
#' @return A wide-format tibble summarizing date statistics
#' 
#' @examples
#' df <- data.frame(
#'   start_date = as.Date("2020-01-01") + 0:2,
#'   end_date = as.Date("2021-01-01") + 0:2
#' )
#' epi_stats_dates_multi(df)
#'
#' @export
epi_stats_dates_multi <- function(df) {
  date_cols <- df %>% dplyr::select(dplyr::where(function(x) inherits(x, "Date")))

  summary_table <- lapply(names(date_cols), function(col) {
    epi_stats_dates(date_cols[[col]]) %>%
      tidyr::pivot_wider(names_from = Statistic, values_from = Value) %>%
      dplyr::mutate(Column = col) %>%
      dplyr::relocate(Column)
  }) %>%
    dplyr::bind_rows()

  summary_table
}

#############
#' @title Date Differences and Monthly Frequencies
#'
#' @description Computes differences between consecutive dates and frequency of
#'   observations by year-month for a vector of dates.
#'
#' @param date_vector A vector of dates of class `Date` or `IDate`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{date_differences}{Differences in days between sorted consecutive dates.}
#'     \item{frequencies}{A table of counts by year-month.}
#'   }
#'
#' @examples
#' sample_dates <- as.Date(c("2020-01-01", "2020-01-03", "2020-02-01"))
#' epi_stats_dates_freq(sample_dates)
#'
#' @export
epi_stats_dates_freq <- function(date_vector) {
  if (!inherits(date_vector, "Date") && !inherits(date_vector, "IDate")) {
    stop("Input must be a vector of type Date or IDate.")
  }
  if (inherits(date_vector, "IDate")) {
    date_vector <- as.Date(date_vector)
  }

  date_ord <- sort(as.Date(date_vector))
  date_diffs <- diff(as.numeric(date_ord))
  date_freq <- table(format(date_vector, "%Y-%m"))

  list(
    date_differences = date_diffs,
    frequencies = date_freq
  )
}

#############
