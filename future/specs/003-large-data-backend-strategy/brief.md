# Brief

Spec ID: `003-large-data-backend-strategy`  
Status: Draft  

## Problem

The EDA MVP works with ordinary data frames and tibbles. The package describes
large-ish epidemiological data, but the new EDA workflow does not yet define how
larger backends should be introduced.

## Goal

Write a design-first plan for large-data backend support before implementation.

## Non-goals

- Implementing Arrow, DuckDB or data.table EDA backends in the design PR.
- Rewriting the existing EDA workflow.
- Adding heavy dependencies without an implementation spec.

## Candidate Backends

- data.table for fast in-memory aggregation.
- DuckDB for local SQL-style analysis.
- Arrow for multi-file Parquet datasets.
