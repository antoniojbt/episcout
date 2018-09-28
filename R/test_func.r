#' @title A Dog Function
#'
#' @description This function allows you to express your love for the superior furry animal.
#' @param agree Do you agree dogs are the best pet? Defaults to TRUE.
#' @keywords dogs
#' @export
#' @examples
#' dogs_over_cats()

dogs_over_cats <- function(agree=TRUE){
	if(agree==TRUE){
		print("Woof woof!")
	}
	else {
		print("Try again.")
	}
}
