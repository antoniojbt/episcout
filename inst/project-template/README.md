# episcout EDA project

This scaffold is a starting point for specification-first exploratory data
analysis with `episcout`.

## Files

- `metadata/data_dictionary.csv`: edit this first. It defines the expected
  variables and drives synthetic data, schema checks, summaries and plots.
- `data/input.csv`: place the real input dataset here when it is available.
- `outputs/`: generated CSV outputs and the HTML report are written here.
- `R/project-derivations.R`: add project-specific derived-variable code.
- `_targets.R`: optional targets pipeline.
- `reports/eda.qmd`: direct report-rendering example.

## Direct use

```r
library(episcout)

data <- read.csv("data/input.csv", stringsAsFactors = FALSE)

epi_eda_render_report(
  data = data,
  spec = "metadata/data_dictionary.csv",
  output_dir = "outputs"
)
```

## Synthetic-data preparation

Use synthetic data while access to real data is pending:

```r
library(episcout)

epi_eda_render_report(
  data = NULL,
  spec = "metadata/data_dictionary.csv",
  output_dir = "outputs",
  synthetic = TRUE,
  n = 100,
  seed = 1
)
```

Synthetic data are for pipeline preparation and testing only. They are not for
inference or disclosure control.

## Optional targets workflow

If `targets` is installed, run:

```r
targets::tar_make()
```

The targets pipeline reads `data/input.csv`, applies `derive_project_data()`,
and renders the EDA report.
