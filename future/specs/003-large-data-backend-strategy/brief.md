# Brief

Spec ID: `003-large-data-backend-strategy`
Status: Draft; ready for PostgreSQL-first revision

## Problem

The EDA MVP works with ordinary data frames and tibbles. The package describes large-ish epidemiological data, but the new EDA workflow does not yet define how larger backends should be introduced.

## Goal

Revise this design into a PostgreSQL-first large-data backend contract before implementation, using `future/scratch/episcout_postgresql_backend_plan.md` as an input rather than as implementation authority.

## Non-goals

- Implementing package code before the revised specification is reviewed and activated.
- Implementing Arrow, DuckDB, data.table or generic DBI EDA backends in the first implementation.
- Rewriting the existing EDA workflow.
- Adding heavy dependencies without an implementation spec.

## Activation Prerequisites

- A representative external large-data workload has now been identified without copying its data, schema, dictionary or terminology into this repository.
- The revision must make the proposed five-minute external benchmark threshold and the data-frame limitation measurable and reviewable before activation.

## First Backend

- PostgreSQL is the only backend in the first design and implementation scope.
- Arrow, DuckDB, data.table and generic DBI support remain later candidates.
