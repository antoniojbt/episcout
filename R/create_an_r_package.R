######################
# Create an R package
# https://r-mageddon.netlify.com/post/writing-an-r-package-from-scratch/?utm_medium=email&utm_source=topic+optin&utm_campaign=awareness&utm_content=20180912+data+nl&mkt_tok=eyJpIjoiT1dNMU16ZGtaamc0TXpRNCIsInQiOiJrXC9YZU1LdVhNRThpcGZuYWJRZ3hzc3kwWFlBOXJBUnRmelQ2WVN4eVdmRm9PZUFHK3VzTXdrandzXC92WnRzMWFZYXB0QUx2UGhYNHlzWGpnZlhzQ3NIWkRMNW1ibWFDS2hTSXBFTWNRdVp3a1wvYkxOUnNiZ3h4OHFMVDd2bWNTaSJ9
# https://kbroman.org/pkg_primer/
# http://r-pkgs.had.co.nz/
# Antonio B
# October 2018
######################

######################
# Load all packages needed:
# pkgs <- c("devtools", "roxygen2", "usethis")
# install.packages(pkgs)
library(devtools) # load function
library(roxygen2) # create documentation
library(usethis) # create packages and functions more easily
# See
# http://usethis.r-lib.org/
######################

######################
# An R package basically looks like this:
# Code (R/)
# Package metadata (DESCRIPTION)
# Object documentation (man/)
# Vignettes (vignettes/)
# Testing (tests/)
# Namespaces (NAMESPACE)
# Data (data/)
# Compiled code (src/)
# Installed files (inst/)
# Git and GitHub or equivalent version control repository
######################

######################
# Workflow:
# 1. Use this script/instructions to create the necessary templates
# 2. Edit files and manually create the github repository, hooks (coveralls.io,
# travisci, etc.), etc.
# 3. Follow the git cycle (eg git add, commit, pull, push)
# 4. Update documentation (http://r-pkgs.had.co.nz/man.html):
  # use_roxygen_md() and add roxygen comments to your .R files.
  # Run devtools::document() to convert roxygen comments to .Rd files (LaTeX)
  # Preview documentation with ?
# 5. Add code tests:
  # For testing (http://r-pkgs.had.co.nz/tests.html):
    # First run devtools::use_testthat() to setup boilerplate and
    # a tests/testthat directory. Then:
      # Modify your code or tests.
      # Test your package with Ctrl/Cmd + Shift + T or devtools::test().
      # Repeat until all tests pass.
    # Run devtools::use_travis() to set up a basic .travis.yml config file.
    # Add coveralls.io if possible
# 6. Update code, version control and document as needed:
  # Once the skeleton files, documentation and your functions are in place,
  # update when needed using eg
  # usethis::use_version()
  # which will increase the version number in DESCRIPTION plus other helpful things
  # See: http://usethis.r-lib.org/reference/use_version.html
# 7. Check your code:
  # Use R CMD check (or preferably devtools::check()) to automatically check
  # your code for common problems
  # Check the Errors, Warnings and Notes, correct each until tests pass
# 8. Upload to CRAN when a new version is ready (http://r-pkgs.had.co.nz/release.html):
  # Create cran-comments.md with use_cran_comments()
  # devtools::build()
  # Run R CMD check on CRAN’s servers with devtools::build_win()
  # devtools::release() or Upload to http://cran.r-project.org/submit.html
# 9. On GitHub create a tag with the new version. Use .9000 suffix
  # in DESCRIPTION to indicate that this is a development version if needed
# 10. Add/improve functions, documentation and vignettes.
# 11. Repeat cycle
######################

######################
# Go to the directory where you want to create the package:
# setwd('/Users/antoniob/Documents/github.dir/AntonioJBT/')
######################

######################
# Create a package, use a good name!
# http://r-pkgs.had.co.nz/package.html#naming
pkg_name <- 'episcout'
usethis::create_package(pkg_name)
# setwd(pkg_name)

# Add a licence:
author <- 'Antonio Berlanga-Taylor'
email <- 'some.name@@uni.ac.uk' # needs double @@ for roxygen docs
usethis::use_gpl3_license(author)

# Add suggestions for packages to load which are dependencies:
# https://kbroman.org/pkg_primer/pages/depends.html
pkgs <- c('covr',
					'dplyr',
					'tibble',
					'data.table',
					'compare',
					'stringi',
					'stringr',
					'lubridate',
					'purrr',
					'e1071',
					'Hmisc',
					'ggplot2',
					'cowplot')

for (i in pkgs) {
	use_package(i, 'Suggests')
	}

# Add documentation to package:
use_readme_md() # creates a readme and opens it for editing

# Initialise a git repository:
use_git()

# In your github account you can then create a repository and:
# git add *
# git commit -m 'first message)
# git remote add origin https://github.com/XXXX/package_XXXX
# git push -u origin master

# Manually update:
# DESCRIPTION
# README.md
# .gitignore
######################

######################
# Create a first function and organise your code
# http://r-pkgs.had.co.nz/r.html
# Remember package code should create objects, mostly functions.
# Do not change the user's R landscape, so avoid:
# library, require, options, source, setwd, par, etc.
# Make use of on.exit, .onLoad(), .onAttach() if needed
# Have a balance between one function per file and too many functions
# within one file
# File and function names should be meaningful to you and future users
usethis::use_r('useful_name.R')
# This creates a file with the name of the function Open it and create an actual
# function, follow the examples from the web pages above
# Also try:
file_and_function_name <- 'R/test_function_example.R'
# If modifying add additional escape slashes manually (\\), these won't be printed
text_to_add <- sprintf("#' @title XXXX
#'
#' Creates XXXX in \\code{\\link{%s}}
#'
#' @description This function allows you to XXXX
#' @param name description describes the function’s inputs or parameters.
#' The description should provide a succinct summary of the type of the parameter
#'  (e.g., string, numeric vector) and, if not obvious from the name, what the
#'  parameter does.
#'
#' @return describes the output from the function. Not necessary but useful
#' if your function returns different types of output depending on the input,
#' or if you’re returning an S3, S4 or RC object.
#'
#' @author %s \\email{%s}
#' @references \\url{XXXX}
#' @seealso \\code{\\link{functioname}}, or another package \\code{\\link[packagename]{functioname}}.
#' @keywords keyword1 keyword2 ... adds standardised keywords.
#' Keywords are optional but must be taken from a predefined
#' list found in file.path(R.home('doc'), 'KEYWORDS').
#' @examples provides executable R code showing how to use the function in
#' practice. Many people look at the examples first.
#' Example code is run automatically as part of R CMD check.
#' Use \\dontrun{code in here} to include code in the example that is not run.
#' @export
#' @importFrom lib1 lib2

dogs_over_cats <- function(agree = TRUE){
  if(agree == TRUE){
    print('Woof woof!')
  }
  else {
    print('Try again.')
  }
}",
											 pkg_name,
											 author,
											 email
											 )
text_to_add
# Write a more complete function file with dummy roxygen text holders:
write(text_to_add,
			file = file_and_function_name
			# append = TRUE
			)

# Load it up:
devtools::load_code()

# Try it here:
dogs_over_cats()

# TO DO:
# Add documentation (directly in the function file)
# See: https://kbroman.org/pkg_primer/pages/docs.html
# Run devtools to automatically document with roxygen2
# devtools::document()


# Add documentation to functions:
use_roxygen_md()
# devtools::document()
######################

######################
# TO DO: continue here
# TO DO:
# add tests
# documentation
# separate each function into its own file
######################

######################
# Start adding documentation for the whole package:
usethis::use_package_doc()
# Adds a dummy .R file that will prompt roxygen to generate basic
# package-level documentation.
# means user can type ?pkg
# and devtools::document() plus roxygen will work their magic
# TO DO: check what else is needed here?
# see: https://kbroman.org/pkg_primer/pages/vignettes.html

# Create a vignette template and open it:
use_vignette(sprintf('introduction_%s', pkg_name))


# If submitting to CRAN, track your efforts to comply:
use_cran_comments(open = interactive())
# Add some text:
write(
	"There was 1 NOTE:

	*

	* This is a new release.

	## Downstream dependencies
	I have also run R CMD check on downstream dependencies of:
	package
	(webpage)

	All packages that I could install passed except:

	*

	",
	file = 'cran-comments.md',
	append = TRUE
	)
# Manually edit cran-comments.md as needed as you go along
# This will make CRAN submission easier and faster
######################

######################
# TO DO:
# see: https://kbroman.org/pkg_primer/pages/data.html
# Create package data for testing and examples:
use_data_raw()
use_data()
######################

######################
# TO DO:
# see: https://kbroman.org/pkg_primer/pages/tests.html
# Prepare tests for functions:
use_travis()
# paste the travis shield to the README
# after adding coveralls.io add add their emblem too (see below)

use_testthat()

# Add the required lines for travis to work with testthat and coveralls:
# You need to have manually signed up to Travis-CI and coveralls.io for this
# to work

# Get all dependencies needed for testing:
# pkgs_tests <- vector(mode = 'list', length = length(pkgs))
pkgs_tests <- data.frame(pkgs)
pkgs_tests$string <- paste('- ', pkgs_tests$pkgs)
pkgs_tests$string

travis_cat <- c('r_packages:',
								pkgs_tests$string, '\n',
								'r_binary_packages:',
								'- testthat',
								'- knitr', '\n',
								'# Add coverage testing:',
								'# https://covr.r-lib.org/',
								'r_github_packages:',
								'- r-lib/covr', '\n',
								'#after_success:',
								"#- Rscript -e 'library(covr); coveralls()'", '\n',
								'after_success:',
								"- Rscript -e 'covr::codecov()'", '\n'
								)

write(travis_cat,
			file = '.travis.yml',
			append = TRUE)
######################

######################
# Document the whole package (takes info from DESCRIPTION and functions):
devtools::document()

# Install your own package and check:
devtools::install()
package?#pkg_name
######################

######################
# Add a logo to your README (if you really want to):
logo_location <- ''
# resizes and places in the correct package structure location:
use_logo(logo_location)
# Manually add the code to the README
######################

######################
# Clean up your code and check your style is consistent
# install.packages("formatR")
library('formatR')
formatR::tidy_dir("R")
######################

######################
# With everything in place run tests on your package (from the CLI):
# system(sprintf('R CMD build %s', pkg_name))
# if tests pass it will create eg pkg_name_version.tar.gz

# Test with local install:
# version <- 'xxx'
# system(sprintf('R CMD INSTALL %s_%s.tar.gz', pkg_name, version))

# Alternatively just do:
devtools::build()
devtools::install()
######################

######################
# TO DO:
# If all this works, put it on CRAN, see:
# https://kbroman.org/pkg_primer/pages/cran.html
######################

######################
# End:
q()
######################
