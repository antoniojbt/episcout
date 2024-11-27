#' @title Setup a parallel run
#'
#' @description epi_utils_multicore() setups a parallel function call
#' using the package future.
#'
#' @param num_cores Number of cores to use. Default cores - 1.
#' @param future_plan The strategy to use for plan().
#' Default is 'multiprocess'.
#' @param gc Garbage collection. Default is TRUE. Not all strategies use it though
#' and cause an overhead.
#' @param verbose Prints out the settings for future::plan(). Default is TRUE.
#' @param ... pass any further parameters accepted by future::plan().
#'
#' @return Nothing
#'
#' @note num_cores is calculated as max(1, parallel::detectCores() - 1). If using
#' sequential a warning will be emitted for workers. Not all strategies use gc().
#' See \code{\link[future]{plan}}.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link[future]{plan}},
#' \code{\link[parallel]{detectCores}},
#' \code{\link[doFuture]{registerDoFuture}},
#' \code{\link[future]{availableCores}},
#' \code{\link[future]{availableWorkers}}.
#'
#' @examples
#' \dontrun{
#' epi_utils_multicore(num_cores = 1, future_plan = 'sequential')
#' future_v %<-% { sum(1 + 2)}
#' future_v
#' epi_utils_multicore(future_plan = 'multiprocess')
#' future_v %<-% {sum(2 + 2)}
#' future_v
#' }
#
#' @export
#'

epi_utils_multicore <- function(num_cores = NULL,
                                future_plan = "multisession",  # Updated default plan
                                gc = TRUE,
                                verbose = TRUE,
                                ...) {
  # Check for required packages
  required_pkgs <- c("doFuture", "future", "foreach", "iterators", "parallel")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(sprintf("The following packages are required but not installed: %s",
                 paste(missing_pkgs, collapse = ", ")))
  }

  # Set number of cores
  num_cores <- if (is.null(num_cores)) max(1, parallel::detectCores() - 1) else num_cores

  # Register doFuture and configure plan
  doFuture::registerDoFuture()
  future::plan(strategy = future_plan, workers = num_cores, gc = gc, ...)

  # Log details if verbose is TRUE
  if (verbose) {
    message("Parallel backend registered with the following configuration:")
    message(sprintf("- Available cores: %s", future::availableCores()))
    message(sprintf("- Requested cores: %s", num_cores))
    message(sprintf("- Execution plan: %s", future::plan()))
  }

  # Optional garbage collection
  if (gc) {
    gc()
  }

  # Return the number of cores for reference
  invisible(num_cores)
}
