# TO DO: needed? cean up, covnert to function?, etc
#
# ############
# # Generic renv management
# # Copy to new project directory
# # No input
# # Creates / updates in project directory:
#   # - A folder 'renv' in the project directory, and
#   # - A lockfile called 'renv.lock' in the project directory
#   # - .gitignore
#   # - .Rbuildignore
#   # - .Rprofile
#
# # Local cache of data on the filesystem, located at:
#   # - "~/Library/Caches/org.R-project.R/R/renv"
#
# # See:
#   # vignette("renv")
# ############
#
#
# ############
# # Directory locations
# # getwd()
# # dir(path = normalizePath('xx'), all.files = TRUE)
#
# project_name <- 'xx'
# projects_dir <- '~/Documents/work/xx'
#
# project_root <- sprintf('%s/%s', projects_dir, project_name)
# print(project_root)
# setwd(project_root)
# getwd()
#
# # code_loc <- "~/Documents/work/devel/xx"
# ############
#
#
# ############
# # Import libraries
# library(renv)
# ############
#
#
# ############
# # Check current state
# # system and project libraries
# .libPaths()
#
# # See which packages are available in each library:
# lapply(.libPaths(), list.files)
# ############
#
#
# ############
# # Get started with an environment:
# # TO DO: add if so that if there's a lockfile already it uses that by default
# renv::init() # requires interaction on the console if it finds an existing renv.lock
#
# # Install, load libraries, update (manually):
# # TO DO: run checks before, update automatically (?)
# renv::install()
# renv::update() # checks for updates and asks interactively
#
# # Update the lock file with current libraries:
# renv::snapshot() # also needs user confirmation
#
# # Check for renv Sync Issues with
# renv::status()
#
# # Restore to the state in renv.lock file at project root:
# renv::restore()
#
# # Clear the existing renv setup
# unlink("renv", recursive = TRUE)
# unlink("renv.lock")
#
# # Reinitialize renv
# renv::init() # will symlink for efficiency all global R library packages already present
#
# # Reinstall any project-specific packages
# # install.packages(c("dplyr", "ggplot2"))  # Replace with your project's packages
#
# # Snapshot the environment to save the state
# renv::snapshot()
#
# # TO DO: complete cycle (?)
# # To
# # renv::restore()
# ############
#
#
# ############
# # renv might interrupt console needing user input (eg for CLI running)
#
#
# # Options:
# # renv::status()
# # renv::load() instead of load()
#
# # disable temporarily if batch running, e.g.:
#     # Disable renv prompts
#     # Sys.setenv(RENV_CONFIG_PROMPT_ENABLED = "FALSE")
#     # Load the .rdata.gzip file
#     # load("xxx.rdata.gzip")
#     # Optionally re-enable prompts if desired
#     # Sys.unsetenv("RENV_CONFIG_PROMPT_ENABLED")
#
#
# # Ensure No Automatic Reload coming from .Rprofile, e.g.:
# # if (file.exists("renv/activate.R")) {
# #   source("renv/activate.R")
# # }
#
# ############
#
#
#
# ############
# # The end:
# sessionInfo()
# # q()
# ############
