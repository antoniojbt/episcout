library(targets)
library(episcout)

source("R/project-derivations.R")

tar_option_set(packages = c("episcout"))

list(
  tar_target(
    raw_data,
    read.csv("data/input.csv", stringsAsFactors = FALSE)
  ),
  tar_target(
    analysis_data,
    derive_project_data(raw_data)
  ),
  tar_target(
    eda_report,
    epi_eda_render_report(
      data = analysis_data,
      spec = "metadata/data_dictionary.csv",
      output_dir = "outputs"
    ),
    format = "file"
  )
)
