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
    eda_spec,
    epi_eda_spec("metadata/data_dictionary.csv")
  ),
  tar_target(
    preparation_audit,
    epi_eda_prepare(analysis_data, eda_spec, mode = "audit")
  ),
  tar_target(
    prepared_data,
    {
      prepared <- epi_eda_prepare(analysis_data, eda_spec, mode = "apply")
      if (!identical(prepared$metadata$overall_status[[1]], "prepared")) {
        stop("EDA preparation is blocked; review preparation_audit before reporting.")
      }
      prepared$data
    }
  ),
  tar_target(
    eda_report,
    epi_eda_render_report(
      data = prepared_data,
      spec = eda_spec,
      output_dir = "outputs"
    ),
    format = "file"
  )
)
