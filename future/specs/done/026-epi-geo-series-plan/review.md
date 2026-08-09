# Review Notes

- Spec ID: `026-epi-geo-series-plan`
- Status: Completed design-only plan

## Executive Conclusion

The appropriate first `epi_geo_*` contribution is a small vector-mapping foundation over `sf`, not a broad spatial-analysis package. The plan adds value where episcout can enforce explicit coordinate meaning, aggregate QA, privacy boundaries, safe file publication and consistent PostgreSQL behaviour; it delegates general geometry operations and layer composition to `sf` and `ggplot2`.

Issue #218's descriptive and inferential aspirations are separated deliberately. Phase A supports descriptive geometry QA and static maps. Inferential spatial methods remain unplanned until a concrete epidemiological question establishes the population, spatial/temporal unit, denominator, adjacency/distance model, null hypothesis, multiple-testing policy, uncertainty and independent reference truth.

## Current Repository Baseline

- `DESCRIPTION` has no geospatial dependency. `ggplot2` and `RPostgres` are suggested, while `DBI` is imported.
- No active `R/`, `tests/testthat/`, vignette or README path defines geometry, coordinate pairs, CRS/SRID, GeoPackage, Shapefile or PostGIS spatial behaviour.
- `epi_eda_validate_spec()` accepts seven semantic types and does not constrain the general `role` field beyond downstream exact identifier handling.
- `epi_eda_spec_scaffold()` classifies only ordinary atomic/date/factor storage and deliberately avoids scientific role inference.
- PostgreSQL-backed EDA is aggregate-only and its source object accepts separate schema/relation identifiers rather than credentials or SQL.
- Standard EDA bundles may contain plots and aggregate tables but intentionally exclude explicit identifier-role values. Exact location would create a new and potentially more revealing value-bearing output.

## Source Review

- Official `sf` documentation establishes that `sf` extends data-frame-like objects with an active simple-feature list column, converts explicit coordinate columns with `st_as_sf()`, reads files/databases with `st_read()`, writes through `st_write()`, transforms with `st_transform()` and checks validity with `st_is_valid()`.
- Official `sf` CRS documentation warns that replacing CRS metadata does not transform coordinates. This supports separate assignment-at-construction and transformation contracts.
- Official GDAL documentation establishes GeoPackage as a SQLite container with spatial metadata/tables and describes Shapefile as a dataset/layer that may comprise `.shp`, `.shx`, `.dbf` and related files. This supports GeoPackage-first output and Shapefile compatibility input.
- Official PostGIS material establishes geometry/geography types, SRIDs, spatial functions and spatial indexes. The `sf` database vignette confirms DBI-backed PostGIS reading and notes that spatial predicates can restrict large sources before collection.
- Official `ggplot2` documentation establishes `geom_sf()`/`coord_sf()` as the native static plotting path for `sf` objects and describes their CRS handling.

The cited sources define software semantics, not episcout's scientific or privacy policy. The aggregate-only EDA boundary and refusal to infer coordinates are design decisions derived from current repository safety principles.

## Alternatives Considered

### Add thin aliases for common `sf` calls

Rejected as the complete design because aliases alone would duplicate a mature API without addressing coordinate review, privacy, overwrite recovery, PostGIS bounds or EDA integration. Direct `sf` calls remain appropriate for operations where episcout adds no domain contract.

### Import a broad spatial stack immediately

Rejected. `sf` plus existing `ggplot2`, `DBI` and `RPostgres` cover the requested vector foundation. Raster, web-map and modelling packages would add system/runtime complexity without a first-phase requirement.

### Add `geometry` as an eighth EDA type

Rejected for coordinate pairs. The existing `type` field describes column semantics/storage used by canonical summaries, while x/y meaning is a relationship between two numeric columns. Additive `geo_role`/`geo_pair`/`geo_crs` metadata preserves existing specifications and avoids automatic class coercion. A true PostGIS or `sf` geometry-column EDA contract can be designed later.

### Infer coordinate pairs from names and ranges

Rejected. Plausible longitude/latitude ranges and familiar column names do not establish axis order, CRS, units, coordinate epoch or scientific meaning, and could silently map unrelated measurements.

### Let ordinary EDA produce a point map automatically

Rejected. Feature-level points are row-level values and can disclose sensitive locations. Aggregate pair QA satisfies workflow integration without weakening the existing ordinary EDA output boundary.

### Support PostGIS writes in the first database phase

Rejected. Table creation/replacement, grants, indexes, extension management and recovery require a separate mutation contract. Read-only source metadata and explicit bounded collection are useful independently.

## Decisions

1. Plan spec 026 is complete as a design artifact and does not authorise implementation.
2. Phase A is one coherent file/in-memory mapping implementation with an offline primer vignette.
3. Phase B is a separate read-only PostGIS source and bounded-collection implementation.
4. Phase C is a separate additive EDA coordinate-metadata and aggregate-QA implementation.
5. GeoPackage is the created-file format; Shapefile is import compatibility only in phase A.
6. Feature-level mapping remains outside standard EDA bundles.
7. Inferential analysis requires question-specific future work and cannot be added as a generic convenience family.

## Checklist Review

### Truth and semantics

The plan identifies the reviewed data dictionary and source documentation as authoritative for coordinate meaning, distinguishes software validation from scientific validity, makes missing/non-finite coordinate behaviour blocking rather than silently lossy, and states that maps and aggregate bounds are not automatically disclosure-safe. No numerical geospatial or epidemiological conclusion is made.

### Copy-edit

The new planning documents use British English, consistent `PostGIS`, `GeoPackage`, `Shapefile`, CRS and SRID terminology, direct official links without tracking parameters, and explicit phase/status language. Structured lists and tables retain intentional line breaks; prose paragraphs are not hard-wrapped.

## Open Questions For Later Implementation Specs

- Which minimum `sf` and external-library versions are supportable on the package's GitHub and user platforms?
- Which neutral vector fixture has the best immutable provenance, redistribution licence and independent transformation controls?
- Should phase-A coordinate conversion block any missing pair, or should a reviewed explicit `missing = "keep_empty"` policy be supported?
- Which CRS representations are accepted at public boundaries: EPSG integer, authority string and/or complete WKT2?
- What exact attribute classes can be round-tripped through GeoPackage without a lossy-conversion blocker?
- Which maximum PostGIS collection default and stable-order requirement are justified by neutral performance evidence?
- Does future EDA aggregate QA need a bounding-box field? This plan defaults to no because even aggregate extents can reveal sensitive location.

These questions do not block completion of the design plan because each is confined to a later implementation specification and no current behaviour depends on it.

## Closeout

The plan satisfies issue #218 by defining the series, beginner vignette, file formats, terms, libraries, basic descriptive mapping, PostGIS path and EDA coordinate integration while preserving a simple first implementation. It does not implement or promise inferential spatial analysis, raster support, web mapping or automatic coordinate discovery.
