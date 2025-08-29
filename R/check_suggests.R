#' Ensure suggested packages are installed
#'
#' Checks that all packages listed are available for optional functionality.
#'
#' @param pkgs Character vector of package names.
#'
#' @return Invisibly returns TRUE if all packages are installed. Throws an
#'   error listing missing packages otherwise.
#' @keywords internal
#' @examples
#' \dontrun{
#' check_suggests(c("ggplot2", "data.table"))
#' }
check_suggests <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing suggested package(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
