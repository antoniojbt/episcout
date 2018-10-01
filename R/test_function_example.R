#' @title XXXX
#'
#' Creates XXXX in \code{\link{episcout}}
#'
#' @description This function allows you to XXXX
#' @param x XXXX Defaults to TRUE.
#' @param name description describes the function’s inputs or parameters.
#' The description should provide a succinct summary of the type of the parameter
#'  (e.g., string, numeric vector) and, if not obvious from the name, what the
#'  parameter does.
#'
#' @return describes the output from the function. Not necessary but useful
#' if your function returns different types of output depending on the input,
#' or if you’re returning an S3, S4 or RC object.
#'
#' @author Antonio Berlanga-Taylor \email{some.name@@uni.ac.uk}
#' @references \url{XXXX}
#' @seealso \code{\link{functioname}}, or another package \code{\link[packagename]{functioname}}.
#' @keywords keyword1 keyword2 ... adds standardised keywords.
#' Keywords are optional but must be taken from a predefined
#' list found in file.path(R.home('doc'), 'KEYWORDS').
#' @examples provides executable R code showing how to use the function in
#' practice. Many people look at the examples first.
#' Example code is run automatically as part of R CMD check.
#' Use \dontrun{code in here} to include code in the example that is not run.
#' @export
#' @importFrom lib1 lib2

dogs_over_cats <- function(agree = TRUE){
  if(agree == TRUE){
    print('Woof woof!')
  }
  else {
    print('Try again.')
  }
}
