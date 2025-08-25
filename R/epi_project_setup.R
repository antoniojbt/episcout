# renv Project Setup Template
#
# This is a template script for setting up renv environment management 
# in R projects. Copy and modify as needed for your specific project.
#
# For more information about renv, see: vignette("renv")
#
# ############
# Basic renv workflow:
# ############
#
# # 1. Initialize renv in your project directory
# renv::init()
#
# # 2. Install packages as needed
# install.packages(c("package1", "package2"))
# 
# # 3. Save the state of your project library 
# renv::snapshot()
#
# # 4. Restore packages from lockfile when needed
# renv::restore()
#
# ############
# Additional renv commands:
# ############
#
# # Check status of project library
# renv::status()
#
# # Update packages to latest versions
# renv::update()
#
# # Install packages from lockfile  
# renv::install()
#
# # Disable renv prompts for batch processing
# Sys.setenv(RENV_CONFIG_PROMPT_ENABLED = "FALSE")
#
# # Re-enable prompts
# Sys.unsetenv("RENV_CONFIG_PROMPT_ENABLED")
#
# ############
# End of template
# ############
