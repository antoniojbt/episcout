# Software Design

Spec ID: `027-epi-geo-phase-a`
Status: Active

## Scope And Authority

This specification implements only phase A of completed design spec 026. The reviewed data dictionary and source documentation remain authoritative for axis meaning, CRS, units, geometry meaning, privacy classification and publication approval. Successful technical validation does not establish epidemiological validity or disclosure safety.

## Public API

```r
epi_geo_read(dsn, layer = NULL)
epi_geo_from_coords(data, x, y, crs, remove = FALSE)
epi_geo_describe(x)
epi_geo_transform(x, crs)
epi_geo_map(x, value = NULL, geometry_colour = NULL, geometry_fill = NULL, na_colour = "grey80")
epi_geo_write(x, dsn, layer, overwrite = FALSE)
```

Every entry point guards the optional `sf` dependency. `epi_geo_map()` also guards `ggplot2`. Inputs are local objects or local paths; URLs, GDAL virtual-file prefixes, connections, SQL and open-option passthrough are rejected.

## Shared Spatial Contract

Supported data have one active `sfc` geometry column, a non-missing CRS, XY dimensionality and geometry types among `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON` and `MULTIPOLYGON`. Empty members are retained. Geometry collections and Z/M/XYZM inputs fail with value-free actionable conditions. Functions do not repair, cast, split, union, drop or reorder features.

Public CRS inputs accept one EPSG integer or one character value resolvable by `sf::st_crs()`. Missing, length-not-one, non-finite numeric and unresolvable values fail. CRS assignment occurs only while constructing new point geometry; transforming existing geometry always uses `sf::st_transform()`.

## Coordinate Conversion

`data` must be a data frame with unique names. `x` and `y` are distinct exact single character names of numeric columns. `remove` is one non-missing logical. A row is blocked when either coordinate is missing or non-finite. For geographic EPSG:4326 input, x outside `[-180, 180]` or y outside `[-90, 90]` is also blocked.

The return is a fixed list with `data`, `audit` and `metadata`. `audit` contains only aggregate counts: rows, complete pairs, missing-x, missing-y, both-missing, non-finite and range failures, plus an eligibility flag. `metadata` records the reviewed x/y names, resolved CRS and whether source columns would be removed. When blocked, `data` is `NULL`; when eligible it is the complete `sf` object in original row order. No coordinate values enter conditions or metadata.

## File Reading

`dsn` must be one existing local `.gpkg` or `.shp` path. A GeoPackage with multiple spatial layers requires an exact `layer`; a one-layer dataset can omit it. Shapefile `layer`, when supplied, must equal the file stem. The selected layer is read quietly through `sf::st_read()` and then checked against the shared contract. Non-spatial tables, missing layers, unreadable inputs and missing CRS fail without echoing feature values.

Shapefile is input compatibility only. The vignette explains required sidecars and schema/encoding limitations; the API makes no lossless round-trip claim.

## Aggregate Description

`epi_geo_describe()` returns `dataset`, `geometry_types`, `validity` and `messages`. Dataset fields include feature and attribute counts, geometry-column name, dimension, resolved CRS input/name/EPSG, geographic status, coordinate units and named bounds. Geometry-type rows include empty members explicitly. Validity reconciles valid, invalid, missing-or-exception and empty counts without identifiers or reasons containing coordinates. Messages use fixed value-free text for empty data, mixed types and other review states. Bounds are returned because this is an explicit geospatial workflow, but documentation classifies them as potentially sensitive.

## Transformation

The transformer validates the source object and target CRS, performs `sf::st_transform()`, then verifies unchanged feature count, attribute names/order and supported output structure. Identity transformation is permitted. Transform failures are rethrown with fixed value-free text.

## Static Mapping

The mapper accepts `value = NULL` or one exact non-geometry attribute name. Geometry-only maps use a constant fill for polygonal data and constant colour for linear/point data. Value maps use fill for exclusively polygonal data and colour for exclusively point/linear data. Mixed polygonal and non-polygonal input is refused so users compose explicit `ggplot2` layers. Caller constants may not conflict with the selected value aesthetic. Missing mapped values use `na_colour`. The result is an ordinary `ggplot` with `geom_sf()` and `coord_sf()` behaviour and no save, basemap, classification or suppression side effect.

## GeoPackage Publication

`epi_geo_write()` requires a local `.gpkg` path and one non-empty layer name. `overwrite` authorises replacement of that exact layer only. A new staging file is created beside the destination so final rename stays on one filesystem. When the destination exists, it is copied to the staging path before the target layer is added or replaced; unrelated layers therefore remain present.

The staged layer is re-read and reconciled against the input feature count, attribute names, geometry families, CRS and aggregate bounds within a numeric tolerance. The destination is then swapped through a same-directory backup: rename destination to backup, rename stage to destination, and restore the backup if publication fails. A successful publication removes the backup; failures preserve or restore the prior file. Append semantics and Shapefile output are absent.

## Dependency And Platform Policy

`sf (>= 1.0-17)` is added to `Suggests`; this is the release that introduced `st_can_transform()` and the undefined-CRS write behaviour guarded by this implementation. `ggplot2` remains suggested. Routine non-geo package use must work when `sf` is unavailable. Tests record `sf::sf_extSoftVersion()` and exercise GeoPackage round trips in the maintained mamba environment. CI package checks supply independent platform coverage.

## Privacy, Errors And Recovery

Geometry and bounds are value-bearing. Functions never include coordinates, WKT/WKB, feature identifiers, connection details or attribute values in errors, warnings, audit metadata, snapshots or manifests. File paths and caller-selected column/layer names may appear only when needed for actionable validation and must not be used in committed restricted-data fixtures.

Validation precedes work where practical. Conversion is all-or-nothing, transformation does not mutate its input, mapping has no write side effect and file publication uses owned same-directory staging and backup files with exit-time cleanup.
