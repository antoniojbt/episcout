#' @title Save an R session
#'
#' @description epi_utils_session() is a convenience function to save an R
#' session.
#'
#' @param output_prefix File name as string, '.RData' is appended
#' to saved objects. Default is e.g.
#' 'session_date.RData' eg 'session_2019-03-01.RData'
#'
#' @param objects_to_save String with list of objects to save. Assumes you only
#' want a subset from the R session. Use saveRDS() for one object or save.image()
#' for the whole session instead.
#'
#' @param ... pass any other parameters from save() such as compress = 'gzip'
#'
#' @return Saves an external representation of the R objects to file
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[base]{save.image}},
#' \code{\link[base]{saveRDS}},
#' \code{\link[base]{load}},
#' \code{\link[base]{readRDS}},
#' \code{\link[episcout]{epi_utils_log}}.
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
#' dir()
#' rm(list = objects_to_save)
#' ls()
#' load('xy.RData')
#' ls()
#' }
#'
#' @export
#'

epi_utils_session <- function(output_prefix = NULL,
                              objects_to_save = NULL,
                              ...
                              ) {

  objects_to_save <- c(objects_to_save) # needs to be a character vector

  # Filename to save current R session, data and objects at the end:
  if (is.character(output_prefix)) {
    output_name <- sprintf('%s.RData', output_prefix)
    } else {
      output_name <- sprintf('session_%s.RData', Sys.Date())
    }
  save(
    list = objects_to_save,
    file = output_name,
    envir = parent.frame(),
    ...
  )
  print(sprintf('Saved objects as: %s', output_name))
  }
