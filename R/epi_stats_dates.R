#############
#' @title Calculate Descriptive Date Statistics
#'
#' @description Calculates and returns key descriptive statistics for a vector of dates or datetimes, including minimum, maximum, interquartile range (IQR), and quartiles. It is compatible with `Date`, `IDate`, `POSIXct` and `POSIXlt` objects.
#'
#' @param date_vector A vector of dates or datetimes of class `Date`, `IDate`, `POSIXct` or `POSIXlt`.
#'
#' @return A `data.frame` containing statistics such as N, N Missing, N Unique, Min, 25%, Median, 75%, Max, IQR, Most Common, and Range (Days).
#'
#' @examples
#' sample_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01"))
#' epi_stats_dates(sample_dates)
#'
#' @note The default origin for numeric date conversion in R is "1970-01-01".
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_stats_summary}}, \code{\link{epi_stats_format}}, \code{\link{epi_stats_numeric}}
#'
#' @export
#'
#' @importFrom stats IQR quantile

epi_stats_dates <- function(date_vector) {
  type <- if (inherits(date_vector, c("Date", "IDate"))) {
    "date"
  } else if (inherits(date_vector, c("POSIXct", "POSIXlt"))) {
    "datetime"
  } else {
    stop("Input must be a vector of type Date, IDate, POSIXct or POSIXlt.", call. = FALSE)
  }
  core <- summary_temporal_core(date_vector, type)
  iqr_value <- core$iqr_value
  range_value <- core$range_value
  if (type == "datetime") {
    iqr_value <- iqr_value / 86400
    range_value <- range_value / 86400
  }
  data.frame(
    Statistic = c(
      "N", "N Missing", "N Unique", "Min", "25%", "Median", "75%", "Max",
      "IQR", "Most Common", "Range (Days)"
    ),
    Value = c(
      as.character(core$n),
      as.character(core$n_missing),
      as.character(core$n_unique),
      core$min,
      core$q1,
      core$median,
      core$q3,
      core$max,
      as.character(iqr_value),
      core$most_common,
      as.character(range_value)
    ),
    stringsAsFactors = FALSE
  )
}
#############

#' @title Summarise Multiple Date Columns
#'
#' @description Applies [epi_stats_dates()] to each date or datetime column in a data frame and returns a wide-format tibble with the results.
#'
#' @param df A data frame containing one or more date columns.
#'
#' @return A tibble where each row corresponds to a date column and columns contain the statistics produced by [epi_stats_dates()].
#'
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
  date_cols <- df %>% dplyr::select(dplyr::where(~ inherits(.x, c("Date", "IDate", "POSIXct", "POSIXlt"))))

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
#' @description Computes differences between consecutive dates and frequency of observations by year-month for a vector of dates.
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
  date_vector <- summary_as_date_vector(date_vector)

  date_ord <- sort(as.Date(date_vector))
  date_diffs <- diff(as.numeric(date_ord))
  date_freq <- table(format(date_vector, "%Y-%m"))

  list(
    date_differences = date_diffs,
    frequencies = date_freq
  )
}

#############
