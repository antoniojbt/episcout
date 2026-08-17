# Longitudinal EDA

Status: Completed. Tracking issue: #349; implementation PR: #356.

Provide one neutral, post-curation long-form panel description: structure, follow-up, presence, present-cell completeness, unchanged ordinary summaries and signed numeric changes. The explicit identifier and reviewed discrete time variable define the panel.

The public outcome is `epi_eda_longitudinal(data, spec, id, time, time_order = NULL, variables = NULL)` returning class `c("epi_eda_longitudinal", "list")`. It accepts one data frame or one unmodified PostgreSQL source and returns only aggregate results. Data-frame and PostgreSQL custom components have the same contract. Ordinary summaries are the canonical stratified object for the selected specification, not a second descriptive engine.

This is not cleaning, QC acceptance, inference, categorical transition analysis or reporting. It neither interprets entry/absence nor imposes project thresholds. Runtime code, examples and fixtures remain project-neutral.
