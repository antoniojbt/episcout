# Brief

Spec ID: `044-mark-aware-external-palette-contract`
Status: Review
Owner: Codex
Tracking issue: issue-310

## Problem

The maintained plotting helpers expose one generic `custom_palette` input only on `epi_plot_bar()`. It applies to the bars' `fill` aesthetic and silently recycles short vectors. Callers cannot tell from the interface whether an external palette belongs on fill or colour, cannot supply a category-preserving mapping, and would have to rely on the historical institutional palette exports to infer a convention.

## Goal

Define a neutral, explicit and mark-aware discrete-palette contract. The first implementation slice covers the two demonstrated categorical mark families: filled bars and filled grouped box plots. It gives each helper an explicit `fill_values` mapping, rejects ambiguous explicit mappings, and preserves existing released calls through a documented `custom_palette` compatibility path. A separate later slice may add a `colour_values` input only to helpers whose current mark maps a categorical stroke, line or point colour.

## Evidence Boundary

This design is based on issue-310, the active public plotting source and tests on canonical `master`, the exported namespace, repository callers, release `0.4.0`, and the closed AEIS 0.2 foundation PR. The source inventory found no maintained internal or example caller of `palette_IMSS` or `palette_IMSS_accessible`; that is evidence about this repository, not proof that downstream consumers do not exist.

## Non-goals

- No AEIS dependency, palette values, global style options, theme redesign or accessibility certification.
- No change to plotted observations, calculations, category values, factor ordering, missingness, denominators or labels.
- No silent deletion, regrouping, relabelling or recycling of an explicit named mapping.
- No removal or deprecation of the historical palette exports in this release.

## Successor

Issue-316 is the single implementation successor. It remains `dispatch:blocked` until this planning issue is accepted, merged and closed out. Issue-311 remains blocked because its creation-time hook must consume the explicit palette semantics defined here without duplicating them.
