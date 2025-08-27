# This is a workaround for NSE to avoid R CMD check NOTEs on
# "no visible global function definition"

# Some workarounds:
# Favour standard eval over non-standard eval, e.g.:
# For ggplot2 code use aes_string() instead of aes()
# Use the approrpriate setup for DESCRIPTION and NAMESPACE eg add:
# #' @importFrom magrittr "%>%"
# If above doesn't work, check usethis::use_pipe() which adds magrittr to Imports
# in DESCRIPTION and a utils-pipe.R file that imports the pipe
# For '.' import as global var with code in this file:
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", "level", "variable", "label", "Column", "Variable", "Value",
      "Statistic", "Var1", "n_missing", "x", "time", "surv"
    )
  )
}

# For other NSE see also rlang::.data in package functions
# Can also run usethis::use_tidy_eval() at package creation which adds
# rlang to Imports and a utils-tidy-eval.R file with the necessary code.
