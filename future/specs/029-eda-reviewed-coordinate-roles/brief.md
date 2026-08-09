# Brief

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Draft
Owner: repository-owner
Tracking issue: #237

## Problem

The seven-type EDA specification can describe storage and analysis semantics, but it cannot record that two reviewed numeric variables form an x/y coordinate pair. The mapping API can convert explicitly selected columns, yet the ordinary EDA workflows have no value-free way to report whether a reviewed pair is structurally eligible before that separate feature-level action. Inferring coordinate meaning from names, ranges or values would be scientifically unsafe and could expose sensitive locations.

## Goal

Add optional `geo_role`, `geo_pair` and `geo_crs` specification metadata plus one aggregate-only coordinate-pair QA component. The data-frame and PostgreSQL paths must agree on counts and stable eligibility reasons while ordinary returned objects and bundles contain no coordinate values, geometry, pair bounds, point identifiers or maps.

## Non-goals

- Adding an eighth EDA type or inferring x/y roles, pair membership, axis order or CRS.
- Constructing geometry, swapping axes, assigning a missing CRS or invoking mapping automatically.
- Adding geometry columns to ordinary EDA, PostGIS requirements or any database write.
- Returning bounds, WKT/WKB, feature identifiers, raw coordinates or row-level blockers.
- Adding raster, web mapping, geocoding, routing or inferential spatial analysis.
- Claiming that eligibility proves location meaning, disclosure safety or scientific validity.

## Candidate Files

- `R/eda_spec.R`, `R/eda_spec_scaffold.R`, `R/eda_geo.R`
- `R/run_eda.R`, `R/eda_intake.R`
- `R/eda_postgres_queries.R`, `R/eda_db_run.R`
- `tests/testthat/test-eda-geo.R`, `tests/testthat/test-eda-geo-postgres.R`
- `README.md`, `NEWS.md`, `vignettes/specification-first-eda.Rmd`, `vignettes/geospatial-mapping-primer.Rmd`
- generated help and namespace files

## Risks

- Pair metadata could be accepted without an exact x/y partner or a genuinely resolvable shared CRS.
- Missing-code, `NaN` and infinity handling could diverge between R and PostgreSQL.
- A query or error path could collect or reproduce coordinate values despite returning aggregate counts.
- Adding the component to mature bundle contracts could invalidate overwrite/reconciliation guarantees.
- Treating a technically eligible pair as scientifically meaningful could encourage unsafe mapping.

## Terminal Outcome

This implementation completes the bounded three-phase geospatial programme designed in issue #218. Any later spatial inference requires a new issue grounded in a concrete epidemiological question, population and denominator contract, authoritative method and independent validation.
