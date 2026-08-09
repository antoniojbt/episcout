# Brief

Spec ID: `027-epi-geo-phase-a`
Status: Active
Owner: repository-owner
Tracking issue: [#226](https://github.com/antoniojbt/episcout/issues/226)

## Problem

Completed design spec 026 defines a staged `epi_geo_*` programme, but the released package has no reviewed vector-file or in-memory spatial workflow. Users currently receive no package-level guard against ambiguous layers, missing CRS, incomplete coordinate pairs, silent layer replacement or feature-level location disclosure through ordinary EDA paths.

## Goal

Implement the bounded phase-A foundation: explicit reviewed coordinate conversion, GeoPackage and Shapefile input, aggregate geometry description, true CRS transformation, one static `ggplot2` map, staged GeoPackage publication and an offline first-user primer. The implementation uses `sf` objects directly and leaves database and EDA integration to later trackers.

## Observable Outcome

- Six exported `epi_geo_*` functions implement the accepted phase-A interface from spec 026.
- Coordinate conversion reports all blockers without partially converting rows.
- File reading refuses ambiguous, unsupported or missing-CRS spatial layers.
- Description returns aggregate structure and bounds with explicit privacy warnings in its documentation.
- Mapping returns an ordinary extensible `ggplot` object with geometry-aware value aesthetics.
- GeoPackage writing never silently replaces a layer and preserves unrelated layers through staged publication.
- A neutral offline vignette teaches reviewed file, CRS, QA, transformation, mapping and writing steps without implying scientific or disclosure approval.

## Non-goals

- PostGIS source description, bounded collection, database writes or arbitrary GDAL inputs.
- Specification-first EDA coordinate roles or feature-level output in ordinary EDA bundles.
- Geometry repair, coordinate inference, axis swapping, missing-row dropping or automatic CRS assignment.
- Raster, basemaps, web maps, geocoding, routing or inferential spatial methods.
- A lossless Shapefile round-trip promise.

## Candidate Files

- `DESCRIPTION`
- `R/epi_geo.R`
- `tests/testthat/test-epi-geo.R`
- `vignettes/geospatial-mapping-primer.Rmd`
- generated `man/` and `NAMESPACE`
- `README.md`, `NEWS.md`, `PROJECT_MAP.md` and future lifecycle records

## Risks

- Plausible but wrongly assigned axes or CRS can produce misleading locations.
- Exact geometry, bounds and rare attributes may disclose sensitive locations.
- GDAL driver differences can alter attribute types or geometry normalisation.
- Layer replacement can corrupt an existing multi-layer GeoPackage unless publication is staged at the whole-file boundary.
- A plotting wrapper can hide geometry-specific semantics or imply inappropriate thematic defaults.

## Successor Or Terminal Outcome

- Create a separate issue for phase-B read-only PostGIS description and bounded collection after phase A reaches review, unless the owner explicitly terminates that programme.
- Phase C remains deferred in completed design spec 026 until both preceding contracts are stable.
