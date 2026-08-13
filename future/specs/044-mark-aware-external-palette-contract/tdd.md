# Test Design

Spec ID: `044-mark-aware-external-palette-contract`
Status: Review

## Independent Expectations

A three-level factor fixture with declared order `low`, `medium`, `high` establishes the expected mapping independently of package rendering: a named mapping associates each name with its supplied colour, while an unnamed three-value vector associates its first, second and third value with that declared order. A two-value vector is insufficient and must fail rather than repeat.

## Focused Tests

- `epi_plot_bar()` applies a valid named mapping exactly and keeps the factor order unchanged.
- `epi_plot_bar()` converts a complete unnamed mapping into the documented level order.
- Grouped `epi_plot_box()` routes a valid mapping to the fill scale and does not alter jitter or summary-point colour.
- Invalid colours, duplicate/blank names, missing names, extra names and insufficient positional values fail with actionable messages.
- `custom_palette` retains its documented positional recycling with a deprecation warning, while a short `fill_values` vector fails and the two arguments conflict when both are supplied.
- `NULL` preserves existing plotted data, layer mapping and default-scale behaviour.
- Both historical palette exports remain present and unchanged.

## Visual Regression

Regenerate and inspect only the changed bar/box fixtures at their delivered SVG size. The review confirms that category order, legend keys, bars, boxes, outlines, point marks and labels remain legible. Snapshot agreement is regression evidence, not an accessibility claim.

## Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'plotting|additional_coverage|epi_plot_box_layers', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); styler::style_file(c('R/epi_plot_bar.R', 'R/epi_plot_box.R', 'R/epi_plot_colour_palettes.R')); lintr::lint_package()"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
```
