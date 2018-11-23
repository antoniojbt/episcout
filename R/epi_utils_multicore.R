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
																future_plan = 'multiprocess',
																gc = TRUE,
																verbose = TRUE,
																...
																# loop_start = 1,
																# loop_end = NULL
																) {
	# Check packages needed:
	if (!requireNamespace('doFuture', quietly = TRUE)) {
		stop("Package doFuture needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('future', quietly = TRUE)) {
		stop("Package future needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('foreach', quietly = TRUE)) {
		stop("Package foreach needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('iterators', quietly = TRUE)) {
		stop("Package iterators needed for this function to work. Please install it.",
				 call. = FALSE)
	}
	if (!requireNamespace('parallel', quietly = TRUE)) {
		stop("Package parallel needed for this function to work. Please install it.",
				 call. = FALSE)
	}

	# Define cores:
	if (is.null(num_cores)) {
		num_cores <- max(1, parallel::detectCores() - 1)
	} else {
		num_cores = num_cores
	}

  doFuture::registerDoFuture()
  # Cores:
  av_cores <- future::availableCores()
  # Workers:
  av_workers <- future::availableWorkers()
  # Plan:
  exec_plan <- future::plan(strategy = future_plan,
  													workers = num_cores,
  													gc = gc,
  													...
  													)
  exec_plan

  if (verbose == TRUE) {
  	print('Registering the doFuture parallel adaptor for foreach.')
  	print(sprintf('Available cores: %s', av_cores))
  	print(sprintf('Requested cores: %s', num_cores))
  	print(sprintf('Available workers: %s', av_workers))
  	print(exec_plan)
  	}

  # # Create loop in parallel:
  # # Create output holder:
  # list_length <- length(unique(df[[some_col]]))
  # # List of unique individuals to extract:
  # list_unique_ids <- unique(df[[some_id]])
  # # Output holder:
  # unique_inds <- vector(mode = 'list', length = list_length)
  #
# my_out <- foreach(i = loop_start:length(list_unique_ids)) %dopar% {
# # Do something here:
#	#
# }
# # Cleaning memory:
# print('Cleaning memory').
# gc() # Clear memory, may not be necessary, especially if running with functions
}
