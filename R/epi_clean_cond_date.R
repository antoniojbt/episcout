#' @title Check if column is date using lubridate
#'
#' epi_clean_cond_date()
#'
#' @description Check if column is date (TRUE). Useful when
#' extracting columns from a large data frame using dplyr
#' (ie get all date type columns)
#'
#' @param col column header as string or integer. This will be passed to
#' lubridate::is.Date(col) and lubridate::is.POSIXt(col)
#'
#' @return boolean TRUE/FALSE indicating whether column passed is type date
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_clean_cond_numeric}}, \code{\link{epi_clean_class_to_factor}},
#' \code{\link{epi_clean_count_classes}},
#' \code{\link[base]{is.character}}, \code{\link[base]{is.factor}}
#'
#' @examples
#'
#' \dontrun{
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(
#' 	var_id = rep(1:(n / 2), each = 2),
#' var_to_rep = rep(c('Pre', 'Post'), n / 2),
#' 	x = rnorm(n),
#' 	y = rbinom(n, 1, 0.50),
#' 	z = rpois(n, 2)
#' )
#' df_date <- df
#' df_date$date_col <- seq(as.Date("2018/1/1"), by = "year", length.out = 5)
#' df_date %>% select_if(~ epi_clean_cond_date(.))
#' epi_clean_cond_date(df_date[[2]]) # should be 'FALSE'
#' epi_clean_cond_date(df_date[, 'date_col']) # should be 'TRUE'
#' }
#'

epi_clean_cond_date <- function(col = NULL) {
	if (!requireNamespace('lubridate', quietly = TRUE)) {
		stop("Package lubridate needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	lubridate::is.Date(col) == TRUE | lubridate::is.POSIXt(col) == TRUE
}
