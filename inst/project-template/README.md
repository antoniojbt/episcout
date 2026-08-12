# episcout EDA project

This scaffold is a file-based starting point for specification-first exploratory data analysis with `episcout`. For the complete dictionary, preparation, summary, map and delivery contracts, read `vignette("specification-first-eda", package = "episcout")`.

## Files

- `metadata/data_dictionary.csv`: replace the neutral example row and review the dictionary before analysis. The specification drives synthetic generation, schema and missingness checks, summaries, plots and optional point maps.
- `data/input.csv`: place an authorised input data set here. The scaffold does not copy or manage source data.
- `outputs/`: machine-readable CSV outputs, optional map SVGs and `eda-report.html` are written here.
- `R/project-derivations.R`: add project-specific derived-variable code, including reviewed preparation not provided by the lower-level renderer.
- `_targets.R`: optional `targets` pipeline for an authorised input data set.
- `reports/eda.qmd`: direct report-rendering example for either an authorised input data set or synthetic pipeline preparation.
- `config/eda.yml`: a human-readable record of the default scaffold paths and synthetic settings; the example R code does not parse it automatically.

## 1. Review the dictionary

The copied dictionary has the same 15-field semantic contract returned by `epi_eda_spec_scaffold()`: `name`, `label`, `type`, `role`, `units`, `levels`, `min`, `max`, `missing_codes`, `required`, `group`, `description`, `geo_role`, `geo_pair` and `geo_crs`. Replace the example row with source-authorised declarations. Blank coordinate fields mean that no coordinate pair is declared.

Do not add database credentials, privacy classification, pseudonymisation policy or approval status to this dictionary. Follow the package's separate longitudinal pseudonymisation workflow when related restricted tables require stable pseudonyms.

## 2. Audit and prepare authorised data

The direct report and `targets` examples call `epi_eda_render_report()`, which does not run `epi_eda_prepare()`. Audit the loaded data first and apply reviewed transformations before reporting when the specification requires them.

```r
library(episcout)

data <- read.csv("data/input.csv", stringsAsFactors = FALSE)
spec <- epi_eda_spec("metadata/data_dictionary.csv")

audit <- epi_eda_prepare(data, spec, mode = "audit")
audit$audit

prepared <- epi_eda_prepare(data, spec, mode = "apply")
stopifnot(prepared$metadata$overall_status == "prepared")
```

For an owned audit/apply bundle with factual statuses `audit_complete`, `complete` or `blocked`, use `epi_eda_intake_run()` as described in the canonical vignette. Intake creates its output directory and never writes source or prepared rows.

## 3. Render the data-frame report

The lower-level renderer requires an existing output directory. It writes aggregate machine-readable components and `eda-report.html`; it does not create an intake checksum manifest.

```r
dir.create("outputs", showWarnings = FALSE)

epi_eda_render_report(
  data = prepared$data,
  spec = spec,
  output_dir = "outputs"
)
```

To request point maps, first declare complete reviewed `geo_role`, `geo_pair` and `geo_crs` fields in the dictionary, then pass `maps = TRUE`, optional unique `map_vars`, and an appropriate `max_map_points`. Point maps contain individual locations and can contain thematic values; map readiness is not disclosure approval. See `vignette("geospatial-mapping-primer", package = "episcout")` for non-point and PostGIS geometry workflows.

## Synthetic pipeline preparation

Generate synthetic observations directly from the reviewed dictionary while real-data access is pending:

```r
library(episcout)

dir.create("outputs", showWarnings = FALSE)
epi_eda_render_report(
  data = NULL,
  spec = "metadata/data_dictionary.csv",
  output_dir = "outputs",
  synthetic = TRUE,
  n = 100,
  seed = 1
)
```

Synthetic data are for pipeline preparation and testing only. They are not suitable for inference, anonymity or disclosure control.

## Optional targets workflow

If `targets` is installed, place authorised input data at `data/input.csv`, review `derive_project_data()` and run:

```r
targets::tar_make()
```

The pipeline reads the input, applies `derive_project_data()`, audits and applies specification-guided preparation, then renders the report. A blocking preparation result stops the pipeline before reporting. The pipeline does not use the `synthetic` settings in `config/eda.yml`.

## Database sources and output ownership

Do not export identifiable PostgreSQL rows merely to use this file-based scaffold. The caller owns database infrastructure, connections and credentials. For PostgreSQL-backed aggregate EDA, use `epi_eda_postgres_source()` and `epi_eda_db_run(layout = "delivery")` as described in the canonical vignette. For a runnable neutral synthetic example that connects database inventory, longitudinal pseudonymisation and PostgreSQL-backed delivery, open the installed `examples/db-to-report/walkthrough.R` script.

Neither data-frame nor PostgreSQL output is automatically safe to share. Retain each workflow's complete owned output root and review dictionary metadata, categories, small cells, plots and point maps. Pseudonymised observations remain restricted personal data; they are not anonymous or automatically disclosure-controlled.
