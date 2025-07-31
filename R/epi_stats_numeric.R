#' Summarise Numeric Vector with Descriptive Statistics
#'
#' Compute a comprehensive set of descriptive statistics for a numeric vector,
#' including measures of location, dispersion, shape, normality, and outliers.
#'
#' @param num_vec Numeric vector to summarise.
#' @param na.rm Logical; should \code{NA} values be removed prior to computations?
#'   Default is \code{TRUE}.
#' @param coef Numeric; multiplier for the IQR in Tukey’s outlier rule.
#'   Values more than \code{coef * IQR} below \code{Q1} or above \code{Q3} are
#'   counted as outliers. Default is \code{1.5}.
#' @param ... Additional arguments passed to \code{\link[e1071]{skewness}} and
#'   \code{\link[e1071]{kurtosis}} (e.g., \code{type}).
#'
#' @return A one‐row \code{data.frame} with the following columns:
#' \describe{
#'   \item{\code{n}}{Total length of the input vector.}
#'   \item{\code{n_nonNA}}{Number of non‐\code{NA} observations.}
#'   \item{\code{NA_count}}{Count of \code{NA} values.}
#'   \item{\code{NA_percentage}}{Percentage of values that are \code{NA}.}
#'   \item{\code{sum}}{Sum of values.}
#'   \item{\code{min}}{Minimum value.}
#'   \item{\code{quantile_25}}{25th percentile (\code{Q1}).}
#'   \item{\code{mean}}{Arithmetic mean.}
#'   \item{\code{median}}{Median.}
#'   \item{\code{quantile_75}}{75th percentile (\code{Q3}).}
#'   \item{\code{max}}{Maximum value.}
#'   \item{\code{IQR}}{Interquartile range (\code{Q3} – \code{Q1}).}
#'   \item{\code{SD}}{Standard deviation.}
#'   \item{\code{CV}}{Coefficient of variation (\code{SD / mean}).}
#'   \item{\code{variance}}{Variance.}
#'   \item{\code{sem}}{Standard error of the mean (\code{SD / sqrt(n_nonNA)}).}
#'   \item{\code{skewness}}{Sample skewness (via \pkg{e1071}).}
#'   \item{\code{kurtosis}}{Sample kurtosis (via \pkg{e1071}).}
#'   \item{\code{Shapiro_Wilk_p_value}}{P‐value from Shapiro–Wilk test (if 3 &lt; \code{n_nonNA} &lt; 5000; else \code{NA}).}
#'   \item{\code{lower_fence}}{Lower Tukey fence (\code{Q1} − \code{coef * IQR}).}
#'   \item{\code{upper_fence}}{Upper Tukey fence (\code{Q3} + \code{coef * IQR}).}
#'   \item{\code{n_below_lower}}{Count of values &lt; \code{lower_fence}.}
#'   \item{\code{n_above_upper}}{Count of values &gt; \code{upper_fence}.}
#'   \item{\code{outlier_count}}{Total number of Tukey outliers.}
#'   \item{\code{outlier_percentage}}{Percentage of non‐\code{NA} values flagged as outliers.}
#' }
#'
#' @details
#' Missing values are dropped when \code{na.rm = TRUE}.  The Shapiro–Wilk test
#' for normality is only run for sample sizes between 4 and 4999; otherwise
#' its p‐value is reported as \code{NA}. For skewness: negative/longer left tail,
#' positive/longer right tail, values above 1 usually means non-normality.
#' For kurtosis consider lower values, broader shape and longer tails (platy ~<3),
#' normal (meso ~3) and slender/no tails (lepto ~<3).
#' Outliers are detected with the Tukey method (above and below 1.5 * IQR) or
#' using the multiplier \code{coef}.  Coefficient of variation (\code{CV}) is
#' calculated as \code{SD / mean}.  You may pass further arguments (e.g.,
#' \code{type}) to the skewness and kurtosis functions from \pkg{e1071}.
#' na.rm is TRUE by default for all tests.
#'
#' @author Antonio J. Berlanga-Taylor
#'
#' @seealso
#' \code{\link{epi_stats_count_outliers}}, \code{\link[e1071]{skewness}},
#' \code{\link[e1071]{kurtosis}}, \code{\link[stats]{shapiro.test}}
#'
#' @importFrom stats median quantile sd var shapiro.test IQR
#' @importFrom e1071 skewness kurtosis
#' @export
epi_stats_numeric <- function(num_vec = NULL,
                              na.rm = TRUE,
                              coef = 1.5,
                              ...) {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Package e1071 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  # Basic counts
  n <- length(num_vec)
  n_nonNA <- sum(!is.na(num_vec))
  NA_count <- n - n_nonNA
  NA_percentage <- (NA_count / n) * 100

  # Summaries
  q1 <- quantile(num_vec, 0.25, na.rm = na.rm, names = FALSE)
  q3 <- quantile(num_vec, 0.75, na.rm = na.rm, names = FALSE)
  iqr_val <- IQR(num_vec, na.rm = na.rm)

  # Tukey fences & outlier counts
  lower_fence <- q1 - coef * iqr_val
  upper_fence <- q3 + coef * iqr_val
  n_below_lower <- sum(num_vec < lower_fence, na.rm = TRUE)
  n_above_upper <- sum(num_vec > upper_fence, na.rm = TRUE)
  outlier_count <- epi_stats_count_outliers(num_vec, coef = coef)
  outlier_percentage <- if (n_nonNA > 0) outlier_count / n_nonNA * 100 else NA_real_

  # Shapiro–Wilk normality
  normality <- NA_real_
  if (n_nonNA > 3 && n_nonNA < 5000) {
    normality <- tryCatch(
      shapiro.test(num_vec)$p.value,
      error = function(e) NA_real_
    )
  }

  # Assemble
  desc_stats <-
    data.frame(
      n                    = n,
      n_nonNA              = n_nonNA,
      NA_count             = NA_count,
      NA_percentage        = NA_percentage,
      sum                  = sum(num_vec, na.rm = na.rm),
      min                  = min(num_vec, na.rm = na.rm),
      quantile_25          = q1,
      mean                 = mean(num_vec, na.rm = na.rm),
      median               = median(num_vec, na.rm = na.rm),
      quantile_75          = q3,
      max                  = max(num_vec, na.rm = na.rm),
      IQR                  = iqr_val,
      SD                   = sd(num_vec, na.rm = na.rm),
      CV                   = sd(num_vec, na.rm = na.rm) / mean(num_vec, na.rm = na.rm),
      variance             = var(num_vec, na.rm = na.rm),
      sem                  = sd(num_vec, na.rm = na.rm) / sqrt(n_nonNA),
      skewness             = e1071::skewness(num_vec, na.rm = na.rm, ...),
      kurtosis             = e1071::kurtosis(num_vec, na.rm = na.rm, ...),
      Shapiro_Wilk_p_value = normality,
      lower_fence          = lower_fence,
      upper_fence          = upper_fence,
      n_below_lower        = n_below_lower,
      n_above_upper        = n_above_upper,
      outlier_count        = outlier_count,
      outlier_percentage   = outlier_percentage,
      row.names            = NULL,
      check.names          = FALSE
    )
  desc_stats
}
