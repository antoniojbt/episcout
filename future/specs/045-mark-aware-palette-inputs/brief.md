# Brief

Spec ID: `045-mark-aware-palette-inputs`
Status: Active
Owner: Codex
Tracking issue: issue-316

## Goal

Implement the accepted issue-310/spec-044 contract: explicit exact `fill_values` mappings for `epi_plot_bar()` and grouped `epi_plot_box()`, retaining the released `custom_palette` recycling path as a deprecating compatibility route.

## Boundaries

This slice changes plot scales only. It does not alter observations, factor/category order, missing values, statistics, labels, themes, historical IMSS palette exports, or introduce an external design dependency. Generic colour, point and line contracts remain outside this implementation.
