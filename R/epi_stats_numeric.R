#' Summarise Numeric Vector with Descriptive Statistics
#'
#' Compute a comprehensive set of descriptive statistics for a numeric vector, including measures of location, dispersion, shape, normality, and outliers.
#'
#' @param num_vec Numeric vector to summarise.
#' @param na.rm Logical; should \code{NA} values be removed prior to computations? Default is \code{TRUE}.
#' @param coef Numeric; multiplier for the IQR in Tukey's outlier rule. Values more than \code{coef * IQR} below \code{Q1} or above \code{Q3} are counted as outliers. Default is \code{1.5}.
#' @param ... Additional arguments passed to \code{\link[e1071]{skewness}} and
#'   \code{\link[e1071]{kurtosis}} (e.g., \code{type}).
#'
#' @return A one-row \code{data.frame} with the following columns:
#' \describe{
#'   \item{\code{n}}{Total length of the input vector.}
#'   \item{\code{n_nonNA}}{Number of non-\code{NA} observations.}
#'   \item{\code{NA_count}}{Count of \code{NA} values.}
#'   \item{\code{NA_percentage}}{Percentage of values that are \code{NA}; \code{NA} for zero-length inputs.}
#'   \item{\code{sum}}{Sum of values.}
#'   \item{\code{min}}{Minimum value.}
#'   \item{\code{quantile_25}}{25th percentile (\code{Q1}).}
#'   \item{\code{mean}}{Arithmetic mean.}
#'   \item{\code{median}}{Median.}
#'   \item{\code{quantile_75}}{75th percentile (\code{Q3}).}
#'   \item{\code{max}}{Maximum value.}
#'   \item{\code{IQR}}{Interquartile range (\code{Q3 - Q1}).}
#'   \item{\code{SD}}{Standard deviation.}
#'   \item{\code{CV}}{Coefficient of variation (\code{SD / mean}); \code{NA} when the mean is zero or unavailable.}
#'   \item{\code{variance}}{Variance.}
#'   \item{\code{sem}}{Standard error of the mean (\code{SD / sqrt(n_nonNA)}).}
#'   \item{\code{skewness}}{Sample skewness (via \pkg{e1071}).}
#'   \item{\code{kurtosis}}{Sample kurtosis (via \pkg{e1071}).}
#'   \item{\code{Shapiro_Wilk_p_value}}{P-value from Shapiro-Wilk test (if 3 &lt; \code{n_nonNA} &lt; 5000; else \code{NA}).}
#'   \item{\code{lower_fence}}{Lower Tukey fence (\code{Q1 - coef * IQR}).}
#'   \item{\code{upper_fence}}{Upper Tukey fence (\code{Q3} + \code{coef * IQR}).}
#'   \item{\code{n_below_lower}}{Count of values &lt; \code{lower_fence}.}
#'   \item{\code{n_above_upper}}{Count of values &gt; \code{upper_fence}.}
#'   \item{\code{outlier_count}}{Total number of Tukey outliers.}
#'   \item{\code{outlier_percentage}}{Percentage of non-\code{NA} values flagged as outliers.}
#' }
#'
#' @details When `na.rm = TRUE`, missing and non-finite values are excluded from descriptive calculations; infinite values remain included in the historical `n_nonNA` count because they are not `NA`. When `na.rm = FALSE` and the input contains a missing value, count and missingness fields remain populated while analytical summary, normality and outlier fields are returned as typed `NA` values. The Shapiro-Wilk test for normality is only run for finite sample sizes between 4 and 4999 with non-zero variation; otherwise its p-value is reported as \code{NA}. Empty and all-missing inputs return one row with unavailable summaries reported as \code{NA}. For skewness: negative/longer left tail, positive/longer right tail, values above 1 usually means non-normality. For kurtosis consider lower values, broader shape and longer tails (platy ~<3), normal (meso ~3) and slender/no tails (lepto ~>3). Outliers are detected with the Tukey method (above and below 1.5 * IQR) or using the multiplier \code{coef}. Coefficient of variation (\code{CV}) is calculated as \code{SD / mean}. You may pass further arguments (e.g.,
#' \code{type}) to the skewness and kurtosis functions from \pkg{e1071}.
#'
#' @author Antonio J. Berlanga-Taylor
#'
#' @seealso \code{\link{epi_stats_count_outliers}}, \code{\link[e1071]{skewness}}, \code{\link[e1071]{kurtosis}}, \code{\link[stats]{shapiro.test}}
#'
#' @importFrom stats median quantile sd var shapiro.test IQR
#' @importFrom e1071 skewness kurtosis
#' @export
epi_stats_numeric <- function(num_vec = NULL,
                              na.rm = TRUE, # nolint: object_name_linter
                              coef = 1.5,
                              ...) {
  if (is.null(num_vec) || !is.numeric(num_vec) || inherits(num_vec, c("Date", "POSIXt"))) {
    stop("num_vec must be a numeric vector.", call. = FALSE)
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("na.rm must be a single non-missing logical value.", call. = FALSE)
  }
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop(
      "Package e1071 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  core <- summary_numeric_core(num_vec, coef = coef, ...)
  result <- data.frame(
    n = core$n,
    n_nonNA = core$n_observed, # nolint: object_name_linter
    NA_count = core$n_missing,
    NA_percentage = summary_safe_proportion(core$n_missing * 100, core$n),
    sum = core$sum,
    min = core$min,
    quantile_25 = core$q1,
    mean = core$mean,
    median = core$median,
    quantile_75 = core$q3,
    max = core$max,
    IQR = core$iqr,
    SD = core$sd,
    CV = core$cv,
    variance = core$variance,
    sem = core$sem,
    skewness = core$skewness,
    kurtosis = core$kurtosis,
    Shapiro_Wilk_p_value = core$shapiro_p,
    lower_fence = core$lower_fence,
    upper_fence = core$upper_fence,
    n_below_lower = core$n_below_lower,
    n_above_upper = core$n_above_upper,
    outlier_count = core$outlier_count,
    outlier_percentage = core$outlier_percentage,
    row.names = NULL,
    check.names = FALSE
  )

  if (!na.rm && anyNA(num_vec)) {
    numeric_results <- c(
      "sum", "min", "quantile_25", "mean", "median", "quantile_75", "max",
      "IQR", "SD", "CV", "variance", "sem", "skewness", "kurtosis",
      "Shapiro_Wilk_p_value", "lower_fence", "upper_fence", "outlier_percentage"
    )
    integer_results <- c("n_below_lower", "n_above_upper", "outlier_count")
    result[numeric_results] <- lapply(numeric_results, function(name) NA_real_)
    result[integer_results] <- lapply(integer_results, function(name) NA_integer_)
  }

  result
}
