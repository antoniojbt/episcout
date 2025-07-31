#' @title Count univariate outliers
#'
#' @description epi_stat_count_outliers() counts how many outliers a vector has
#' using a univariate approach. Returns the number of observations that are greater than
#' the cutoff. Outliers are detected with the Tukey method (above and below coef * IQR).
#'
#' @param num_vec Numeric vector to test.
#' @param coef Coefficient for outlier detection, default is 1.5
#' @param ... Other parameters that can be passed to boxplot.stats().
#'
#' @return Returns the number of observations above the cut-off specified.
#'
#' @note coef = 0 returns no outliers, see ?boxplot.stats
#' An alternative, not implemented, is to consider those eg > 5 * SD
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[grDevices]{boxplot.stats}}
#'
#' @examples
#' \dontrun{
#' n <- 1000
#' df <- data.frame(
#'   var_id = rep(1:(n / 2), each = 2),
#'   var_to_rep = rep(c("Pre", "Post"), n / 2),
#'   x = rnorm(n),
#'   y = rbinom(n, 1, 0.50),
#'   z = rpois(n, 2)
#' )
#' epi_head_and_tail(df)
#' epi_stat_count_outliers(num_vec = df$x, coef = 0)
#' epi_stat_count_outliers(num_vec = df$x)
#' summary(df$x)
#' }
#'
#' @export
#'
#' @importFrom grDevices boxplot.stats
#'

# TO DO:
# Clean-up, stop exporting as little value in having it as individual wrapper

epi_stats_count_outliers <- function(num_vec = NULL,
                                     coef = 1.5,
                                     ...) {
  if (coef == 0) {
    return(0L)
  }
  # if(method == 'SD') {
  # get_SD <- sd(num_vec, na.rm = na.rm)
  # count_above <- length(get_SD * )
  # }
  # if(method == 'IQR') {}
  q1 <- stats::quantile(num_vec, 0.25, na.rm = TRUE, type = 7)
  q3 <- stats::quantile(num_vec, 0.75, na.rm = TRUE, type = 7)
  iqr_val <- q3 - q1
  lower <- q1 - coef * iqr_val
  upper <- q3 + coef * iqr_val
  outliers <- sum(num_vec < lower | num_vec > upper, na.rm = TRUE)
  outliers
}

# TO DO: add:
# SD * eg 5
# (IQR * 1.5) * 1.5 inner (or 3 for outer)
# See:
# https://stats.stackexchange.com/questions/350256/iterative-outlier-diagnostic?rq=1
# regression - Iterative outlier diagnostic - Cross Validated
# https://stats.stackexchange.com/questions/38001/detecting-outliers-using-standard-deviations?rq=1
# Detecting outliers using standard deviations - Cross Validated
# https://stats.stackexchange.com/questions/175999/determine-outliers-using-iqr-or-standard-deviation?rq=1
# Determine outliers using IQR or standard deviation? - Cross Validated

# Using the Qn estimator to avoid assuming that the underlying distribution
# is symmetric:
# https://stats.stackexchange.com/questions/1519/on-univariate-outlier-tests-or-dixon-q-versus-grubbs?rq=1
# median+/-delta* Qn
# get_qn <- robustbase::Qn(num_vec)
# median <- median(num_vec, na.rm = na.rm)
# ?
