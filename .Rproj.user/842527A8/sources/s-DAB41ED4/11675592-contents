######################
# Create an R package
# https://r-mageddon.netlify.com/post/writing-an-r-package-from-scratch/?utm_medium=email&utm_source=topic+optin&utm_campaign=awareness&utm_content=20180912+data+nl&mkt_tok=eyJpIjoiT1dNMU16ZGtaamc0TXpRNCIsInQiOiJrXC9YZU1LdVhNRThpcGZuYWJRZ3hzc3kwWFlBOXJBUnRmelQ2WVN4eVdmRm9PZUFHK3VzTXdrandzXC92WnRzMWFZYXB0QUx2UGhYNHlzWGpnZlhzQ3NIWkRMNW1ibWFDS2hTSXBFTWNRdVp3a1wvYkxOUnNiZ3h4OHFMVDd2bWNTaSJ9
# https://kbroman.org/pkg_primer/
# http://r-pkgs.had.co.nz/
#


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
# Workflow:
# 1. Use this script to create all the necessary templates
# 2. Edit all files and manually create the github reposirtory, hooks, etc.
# 3. Update, version control and document as needed:
  # Once the skeleton files, documentation and your functions are in place,
  # update when needed using eg
  # usethis::use_version()
  # which will increase the version number in DESCRIPTION plus other helpful things
  # See: http://usethis.r-lib.org/reference/use_version.html
# 4. Follow the git cycle (eg git add, commit, pull, push)
# TO DO: 5. Update documentation with:
  # use_roxygen_md()
# 6. Keep track of functions, vignette, tests
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
usethis::use_gpl3_license(author)


# Add suggestions for packages to load which are dependencies:
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
# Start adding documentation for the whole package:
usethis::use_package_doc()
# Adds a dummy .R file that will prompt roxygen to generate basic
# package-level documentation.
# means user can type ?pkg
# and devtools::document() plus roxygen will work their magic
# TO DO: check what else is needed here?

# Create a vignette template and open it:
use_vignette(sprintf('introduction_%s', pkg_name))


# If submitting to CRAN, track your efforts to comply:
use_cran_comments(open = interactive())
# Manually edit cran-comments.md as needed as you go along
######################


######################
# Create package data:
use_data_raw()
use_data()
######################

######################
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
# Create a first function:
usethis::use_r('test_func.r')
# This creates a file with the name of the function
# Open it and create an actual function, follow the example from the
# web page above

# Load it up:
devtools::load_code()

# Try it here:
dogs_over_cats()

# Add documentation following the example (copy and paste into the function file)
# Run devtools to automatically
# devtools::document()

# Add documentation to functions:
use_roxygen_md()
# devtools::document()
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
# End:
q()
######################
