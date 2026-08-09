# Brief

Spec ID: `028-epi-geo-postgis`
Status: Active
Owner: repository-owner
Tracking issue: [#233](https://github.com/antoniojbt/episcout/issues/233)

## Problem

Phase A provides reviewed local and in-memory vector handling, but it cannot inspect a PostGIS relation without collecting features or enforce an explicit upper bound before spatial data enters R. Direct `sf` database reads can therefore bypass the package's catalogue, privacy and selection gates.

## Goal

Add a value-free PostGIS source, aggregate-only spatial description and explicit bounded collection path using a caller-owned RPostgres connection. All database work is read-only, identifiers are exact and quoted, extent values are bound, and collection refuses rather than truncates.

## Observable Outcome

- `epi_geo_postgis_source()` validates one approved table, view or materialized view and records a value-free fingerprinted spatial catalogue.
- `epi_geo_postgis_describe()` returns aggregate counts, observed type/SRID/dimension reconciliation and a classified bounding box from one read-only repeatable-read snapshot.
- `epi_geo_postgis_collect()` validates exact non-geometry columns and an optional typed source-CRS bbox, checks the selected count against a positive limit, and returns an `sf` object only when the complete selection fits.
- Caller-owned connections remain open and usable after success and handled failure.
- Disposable live tests prove boundary behaviour, catalogue drift, transaction recovery and that description does not materialise features.

## Non-goals

- Database writes, DDL, extension installation, index creation or tuning.
- Credentials, connection strings, arbitrary SQL, predicates or ordering expressions.
- Automatic collection from `epi_geo_map()` or ordinary EDA bundles.
- Phase-C coordinate roles, raster, web mapping, geocoding, routing or spatial inference.

## Risks

- Exact geometry and narrow extents may disclose locations even without direct identifiers.
- Views may expose mixed runtime types, dimensions or SRIDs despite permissive catalogue declarations.
- A count followed by an unbounded fetch outside one snapshot could race and exceed the reviewed limit.
- Driver conversion can hide unsupported geometry unless database and returned `sf` contracts are both reconciled.

## Successor Or Terminal Outcome

Phase-C specification-first EDA coordinate integration is tracked separately in issue #237 and remains gated on Phase-B merge and closeout.
