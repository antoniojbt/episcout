# Brief

Spec ID: `046-eda-creation-style-hooks`
Status: Active
Owner: Codex
Tracking issue: issue-311

## Goal

Add the accepted generic creation-time styling hook to EDA plot creation without changing analysis, default plots or the aggregate-only PostgreSQL boundary.

## Scope

The hook applies a caller function to each completed ggplot together with compact plot metadata. Data-frame profiling and `epi_eda_run()` receive the optional callback. PostgreSQL EDA applies it after its repeatable-read snapshot and before staged SVG publication, and records an explicit non-secret style identifier for persistent provenance and overwrite compatibility.

## Exclusions

This slice does not introduce a theme registry, external design dependencies, global styling state, row-level callback data, SQL/connection access, statistical changes or arbitrary source persistence.
