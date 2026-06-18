# Brief

Spec ID: `001-phase-1-helper-stabilization`  
Status: Implemented

## Problem

Some existing helper functions have under-specified edge-case behaviour. The
highest-risk examples are `epi_stats_numeric()` and `epi_stats_na_perc()`.

## Goal

Create a small TDD-first hardening PR that makes selected edge cases
deterministic without redesigning the public helper API.

## Non-goals

- Package-wide modernization.
- Renaming exported functions.
- Broad replacement of older tidyverse idioms.
- Changes to specification-first EDA functions.
- Changes to fixtures or report/project templates.

## Candidate Files

- `R/epi_stats_numeric.R`
- `R/epi_stats_na_perc.R`
- `tests/testthat/test-epi_stats_numeric.R`
- `tests/testthat/test-stats.R`

## Activation Triggers

- CI or local checks expose helper failures.
- Release preparation requires helper hardening.
- A future EDA implementation depends directly on these helpers.
- The user explicitly activates this spec.
