#' @title A Dog Function
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @description This function allows you to express your love for the superior furry animal.
#' @param agree Do you agree dogs are the best pet? Defaults to TRUE.
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#'
#' @return None
#'
#' @author Karl W Broman, \email{broman@@wisc.edu}
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @seealso \code{\link{brocolors}}
#' @keywords dogs
#' @examples
#' dogs_over_cats()
#' @export
#' @importFrom grDevices rgb2hsv
#' @importFrom graphics par plot rect text
#'

dogs_over_cats <- function(agree=TRUE){
	if(agree==TRUE){
		print("Woof woof!")
	}
	else {
		print("Try again.")
	}
}
