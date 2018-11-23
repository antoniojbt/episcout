context("episcout utility function tests")

######################
library(episcout)
library(testthat)
library(future)
library(parallel)
library(doFuture)
library(foreach)
library(iterators)
######################

######################
# Working directory for informal tests, should be from pkg/tests/testthat/:
# setwd("")
######################

######################
# Set a test set:
# Test set df:
# set.seed(12345)
# n <- 20
# df <- data.frame(var_id = rep(1:(n / 2), each = 2),
# 								 var_to_rep = rep(c("Pre", "Post"), n / 2),
# 															x = rnorm(n),
# 															y = rbinom(n, 1, 0.50),
# 															z = rpois(n, 2)
#                  )
# df

# Set variables used in more than one test:
# input_file <- "inst/extdata/df.tsv"
######################

######################
print("Function being tested: epi_utils_multicore")

test_that("epi_utils_multicore sequential", {
	epi_utils_multicore(num_cores = 1,
											future_plan = 'sequential')
	core_s <- capture.output(epi_utils_multicore(num_cores = 1,
																							 future_plan = 'sequential')
													 )
	core_s
	# TO DO: fix these, pass in test() but not in check()
	# expect_output(str(core_s[12]), 'sequential')
	future_v %<-% {1 + 2}
	future_v
	expect_identical(future_v, 3)
	future:::ClusterRegistry("stop")
}
)

test_that("epi_utils_multicore multi", {
	epi_utils_multicore(num_cores = 2,
											future_plan = 'multiprocess')
	core_m <- capture.output(epi_utils_multicore(num_cores = 2,
																							 future_plan = 'multiprocess')
													 )
	core_m
	# TO DO: fix these, pass in test() but not in check()
	# expect_output(str(core_m[12]), 'multiprocess')
	# expect_output(str(core_m[13], nchar.max = 300), 'workers = 2')
	future_v %<-% {1 + 2}
	future_v
	expect_identical(future_v, 3)
	future:::ClusterRegistry("stop")
	}
	)
######################
