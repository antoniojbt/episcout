#' @title Save a log of an R session
#'
#' @description epi_utils_log() is a convenience function to log details of an R
#' session.
#'
#' @param output_prefix File name as string, '_log.txt' is appended
#' to a log file. Default is e.g.
#' 'session_date_log.txt' eg 'session_2019-03-01_log.txt'
#'
#' @return Saves a log file to disk
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[utils]{sessionInfo}},
#' \code{\link[episcout]{epi_utils_session}}.
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- stats::runif(20)
#' y <- list(a = 1, b = TRUE, c = "oops")
#' # See objects to save:
#' ls()
#' objects_to_save <- c('x', 'y')
#' epi_utils_session(output_prefix = 'xy',
#'                   objects_to_save = objects_to_save
#'                  )
#' epi_utils_log('xy')
#' }
#'
#' @export
#'
#' @importFrom utils capture.output sessionInfo

epi_utils_log <- function(output_prefix = NULL) {
  # Filename to save current R session, data and objects at the end:
  if (is.character(output_prefix)) {
    output_name <- sprintf('%s_log.txt', output_prefix)
    } else {
      output_name <- sprintf('session_%s_log.txt', Sys.Date())
    }
  print(sprintf('Session information saved in: %s', output_name))
  writeLines(capture.output(sessionInfo()), output_name)
  }
