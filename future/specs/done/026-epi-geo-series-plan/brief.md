# Brief

- Spec ID: `026-epi-geo-series-plan`
- Status: Completed design-only plan
- Owner: Antonio Berlanga-Taylor

## Problem

Issue [#218](https://github.com/antoniojbt/episcout/issues/218) requests an `epi_geo_*` family, introductory guidance for common vector formats and spatial concepts, PostGIS support and integration with specification-first EDA when reviewed columns are coordinates. The current package has no geospatial dependency, spatial object contract, coordinate-reference-system policy, spatial privacy boundary or geospatial test fixture. Implementing isolated wrappers before those choices are reviewed would create a second, inconsistent data workflow and could expose precise locations through ordinary EDA artifacts.

## Goal

Define a small staged vector-mapping programme that uses `sf` as the spatial object layer, preserves episcout's review-gated and PostgreSQL conventions, and leaves inferential spatial analysis, raster work and automatic coordinate inference for separately approved work.

## Observable Outcome

- A first implementation can read reviewed GeoPackage or Shapefile layers, convert explicitly declared coordinate pairs to `sf`, return value-aware spatial QA, transform to a caller-selected CRS, create one static `ggplot2` map and write GeoPackage output without silent replacement.
- A later PostGIS implementation can describe and explicitly collect a bounded spatial relation through a caller-owned connection without accepting credentials or arbitrary SQL and without making raw geometry part of ordinary EDA output.
- A later EDA integration can recognise only explicitly reviewed coordinate metadata and return aggregate coordinate-pair QA; it never infers coordinates from names or values and never adds feature-level locations to standard EDA bundles.
- A primer vignette can explain vector files, geometry, attributes, CRS/SRID, projection, validity, spatial joins, privacy and the boundary between descriptive mapping and inferential analysis.

## Success Criteria

- The first implementation surface, staged PostGIS and EDA extensions, input/output contracts, privacy boundary and failure behaviour are explicit.
- GeoPackage is the preferred interchange/output format; Shapefile is an import compatibility format whose multi-file and schema limitations remain visible.
- CRS assignment and coordinate transformation are never conflated, and no CRS is guessed from column names, ranges or locale.
- Optional dependencies are minimal: `sf` is the spatial engine, `ggplot2` is the existing static-plot engine, and the existing `DBI`/`RPostgres` path is reused for PostGIS.
- Future tests cover independently authored geometries, axis order, missing and invalid coordinates, geometry validity, file round trips, map layer data, bounded PostGIS collection, aggregate-only EDA integration and privacy failures.
- No package code, exported API, dependency declaration, vignette or generated documentation changes in this planning contribution.

## Scope

- Vector `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON` and `MULTIPOLYGON` data represented as `sf` objects.
- GeoPackage and Shapefile input, GeoPackage output, explicit data-frame coordinate conversion, aggregate spatial description, CRS transformation and static mapping.
- Read-only PostGIS source description and bounded explicit collection in a later phase.
- Additive, reviewed coordinate metadata and aggregate coordinate QA in a later EDA phase.
- A beginner-facing vignette that distinguishes data formats, spatial operations, descriptive mapping and inferential questions.

## Non-goals

- Raster, imagery, tiles, routing, geocoding, address matching, GPS processing, web maps, dashboards or map servers.
- Automatic discovery of longitude, latitude, easting, northing, addresses or geometry from variable names or observed values.
- Spatial hypothesis tests, disease-cluster detection, smoothing, interpolation, areal models, spatial regression, causal claims or automatic model selection.
- Generic support for every GDAL driver or database backend.
- PostGIS schema creation, extension installation, grants, index creation, table replacement or unrestricted SQL.
- Claims that a valid geometry, plausible coordinate range or attractive map establishes scientific validity, confidentiality or publication safety.

## Primary Evidence

- Current EDA specification, schema, preparation, plotting and PostgreSQL source contracts under `R/`.
- Current dependency and vignette policy in `DESCRIPTION`, `AGENTS.MD`, `README.md` and `future/README.md`.
- Official `sf`, GDAL, PostGIS and `ggplot2` references listed in `sdd.md` and `review.md`.

## Principal Risks

- Axis-order or CRS mistakes can place features in the wrong location while still producing a plausible plot.
- Exact points, bounding boxes, rare attribute combinations and small spatial cells can disclose sensitive locations.
- Shapefile sidecars, field-name limits, encodings and mixed single/multi geometry types can make apparently successful round trips lossy.
- A convenience PostGIS wrapper could materialise a very large or restricted relation in R unless collection is explicit and bounded.
- A broad plotting wrapper could duplicate `sf` and `ggplot2` APIs without adding an episcout-specific review, validation or privacy contract.
