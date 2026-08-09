# Software Design Plan

- Spec ID: `026-epi-geo-series-plan`
- Status: Completed design-only plan

## Design Authority And Boundary

This plan defines a future episcout interface, not current package behaviour. The reviewed data dictionary and source-system documentation remain authoritative for coordinate meaning, axis order, CRS, units, privacy classification and whether a location is suitable for analysis or sharing. File metadata, an `sf` object and PostGIS catalogues are technical evidence; they do not establish scientific meaning or publication approval.

The first implementation is deliberately a vector-mapping foundation. Descriptive geometry QA supports review, but no inferential spatial method belongs in the first implementation. Each later implementation phase requires its own numbered specification, tests, compatibility review and pull request.

## Core Decisions

1. Use `sf` objects as the in-memory spatial contract and do not create a parallel episcout geometry class.
2. Prefer GeoPackage for package-created vector files; accept Shapefile as a compatibility input and teach that it is a multi-file dataset with driver-dependent limitations.
3. Require an explicit CRS whenever ordinary numeric columns are converted to geometry; never infer a CRS or silently relabel coordinates.
4. Return ordinary `ggplot2` objects for static maps so users retain the normal layer, scale, theme and save workflow.
5. Keep feature-level geometry outside standard specification-first EDA bundles. Coordinate-pair integration is aggregate QA unless a user enters a separate explicit geospatial workflow.
6. Reuse the existing caller-owned `DBI`/`RPostgres` connection conventions for PostGIS, but require a distinct spatial source class because geometry metadata and collection risks differ from aggregate-only EDA.
7. Keep `sf` optional in `Suggests` for the initial series and fail with an actionable installation message when a geo function is called without it. Revisit `Imports` only if geospatial behaviour becomes a core package path.

## Vocabulary For The Primer

| Term | Meaning in this plan |
| --- | --- |
| Feature | One real-world entity represented by one row and one geometry. |
| Attribute | A non-geometry field associated with a feature. |
| Geometry | A point, line, polygon or supported multi-geometry stored in the active `sfc` column. |
| CRS | The coordinate reference system that gives coordinate values their spatial meaning. |
| SRID | A database/file identifier for a spatial reference system; it is metadata, not a transformation. |
| Projection | A coordinate operation that changes coordinate values from one CRS to another. |
| Bounding box | The smallest axis-aligned rectangle enclosing a geometry set; it is aggregate spatial information but may still be sensitive. |
| Geometry validity | Whether geometry structure satisfies the applicable simple-feature rules; it does not establish epidemiological validity. |
| Spatial join | A join whose match is defined by a reviewed spatial predicate rather than ordinary key equality. |

## Staged Public Interface

### Phase A — File And In-memory Mapping Foundation

The first implementation specification should review these candidate functions together because they define one coherent learning path. Names and exact arguments remain proposals until that specification is accepted.

```r
epi_geo_read(dsn, layer = NULL)

epi_geo_from_coords(
  data,
  x,
  y,
  crs,
  remove = FALSE
)

epi_geo_describe(x)

epi_geo_transform(x, crs)

epi_geo_map(
  x,
  value = NULL,
  geometry_colour = NULL,
  geometry_fill = NULL,
  na_colour = "grey80"
)

epi_geo_write(x, dsn, layer = NULL, overwrite = FALSE)
```

#### `epi_geo_read()`

`dsn` is one existing local path to a `.gpkg` file or one Shapefile dataset/layer. URLs, virtual file-system prefixes, inline GeoJSON, database connection strings and arbitrary GDAL open options are not accepted in phase A. `layer` is required when a GeoPackage contains more than one readable spatial layer and is otherwise optional. The function returns an `sf` object without changing its geometry, attributes, CRS or feature order.

The reader validates that the selected layer is spatial, has exactly one active geometry column, uses a supported vector geometry family and has a non-missing CRS. Empty layers return a typed zero-feature `sf` object when the driver can establish the schema. Unsupported, ambiguous, unreadable or missing-CRS inputs fail before downstream mapping.

Shapefile input is resolved as a dataset, not as an isolated `.shp` byte stream. The vignette must tell users to keep required sidecars together and must not promise lossless field names, encodings or geometry normalisation across drivers. GeoPackage is the recommended conversion and output target.

#### `epi_geo_from_coords()`

`data` is an ordinary data frame. `x` and `y` are exact single column names in display/GIS order, normally longitude/easting then latitude/northing. Both columns must be numeric, finite where observed and distinct. `crs` is required and must resolve through `sf::st_crs()`. `remove` is one non-missing logical and defaults to retaining the source coordinate columns for auditability.

Rows with one or two missing coordinates are not silently discarded. The first implementation should return a structured result containing `data`, `audit` and `metadata`, where `data` is an `sf` object only when every row has a complete valid pair; otherwise audit mode reports blockers without a partial conversion. A later implementation may approve an explicit missing-row policy, but phase A must not invent one.

When the reviewed CRS is geographic WGS 84, x must be in `[-180, 180]` and y in `[-90, 90]`. Other CRSs use their own coordinate domain and are not checked against longitude/latitude ranges. Range plausibility does not prove axis order or CRS correctness.

#### `epi_geo_describe()`

`x` is an `sf` object. The function returns a fixed list with `dataset`, `geometry_types`, `validity` and `messages` components. It never repairs geometry.

| Component | Required aggregate fields |
| --- | --- |
| `dataset` | Feature count, attribute count, active geometry column, dimension, CRS input/name/EPSG when available, geographic/projected flag, coordinate units and bounding box. |
| `geometry_types` | One row per observed geometry type with count and proportion, including empty geometry. |
| `validity` | Counts of valid, invalid, missing/exception and empty geometries; no feature identifiers or invalidity coordinates. |
| `messages` | Stable value-free notices for mixed types, missing Z/M support, empty data and other review conditions. |

Bounding boxes and type counts are descriptive outputs, not automatically safe aggregates. Documentation must warn that a narrow bounding box or rare geometry/attribute combination can reveal location.

#### `epi_geo_transform()`

`x` is an `sf` object with a non-missing source CRS, and `crs` is an explicit resolvable target. The function checks transform availability and calls the `sf` transformation path. It returns the same feature and attribute order with transformed geometry and target CRS. It never uses CRS assignment to imitate transformation, repairs invalid geometry, drops empty features or changes dimensionality silently.

#### `epi_geo_map()`

`x` is a supported `sf` object with a non-missing CRS. `value` is `NULL` for a geometry-only map or one exact attribute name for a simple choropleth/symbol map. The colour/fill arguments set constant geometry appearance and may not conflict with `value`. The function returns one `ggplot` object built from `ggplot2::geom_sf()` and `coord_sf()` semantics; it does not save a file, download a basemap, label features automatically, classify continuous values, suppress small cells or claim a thematic default is scientifically appropriate.

The implementation must choose geometry-type-aware defaults: polygon value maps use fill, line and point value maps use colour, and mixed incompatible types require explicit user composition with `ggplot2`. Missing attribute values remain visible using `na_colour`. Exact layer data and coordinate-system metadata must be testable without relying only on image snapshots.

#### `epi_geo_write()`

Phase A writes GeoPackage only. `dsn` is a local `.gpkg` path, `layer` is one non-empty layer name and `overwrite` is one non-missing logical. Existing datasets or layers cause an error unless the exact replacement is authorised. Replacement authority must be limited to the named layer and must never delete a directory or unrelated GeoPackage layers.

The implementation must write to a staging path, read the staged layer back, reconcile feature count, attribute names/types within the documented GDAL contract, geometry types, CRS and aggregate bounds, then publish. A failed write or reconciliation leaves the previous destination unchanged. Shapefile export, append and database write are out of scope for phase A.

### Phase B — Read-only PostGIS Source And Bounded Collection

```r
epi_geo_postgis_source(con, schema, relation, geometry_column = NULL)

epi_geo_postgis_describe(source)

epi_geo_postgis_collect(
  source,
  columns = character(),
  bbox = NULL,
  max_features = 10000L
)
```

`epi_geo_postgis_source()` follows `epi_eda_postgres_source()` conventions: it accepts one open caller-owned RPostgres connection plus separate schema, relation and optional geometry-column identifiers; it rejects credentials, connection strings, SQL text and unqualified dotted names. The constructor catalogues relation kind, geometry/geography column, declared type, dimensions and SRID without collecting geometry or attributes. Ambiguous multi-geometry relations require an explicit geometry column.

`epi_geo_postgis_describe()` performs read-only aggregate catalogue and geometry QA in one owned transaction. It may return relation and geometry counts, declared and observed type counts, SRID consistency, empty/null/invalid counts and an explicitly classified bounding box. It returns no WKT, WKB, coordinates, attributes, database notices, executable SQL or connection details.

`epi_geo_postgis_collect()` is the only phase-B path that materialises features in R. Collection must be explicit, require a positive `max_features`, fail rather than truncate when the reviewed selection exceeds the bound and preserve stable ordering only when an approved key contract exists. `columns` is an allow-list of exact non-geometry identifiers. `bbox` is a typed reviewed extent in the source CRS and is translated through quoted identifiers and bound values, not string interpolation. Arbitrary predicates and SQL are out of scope.

Database writes, DDL, extension installation, index creation, unrestricted query arguments and automatic collection inside `epi_geo_map()` remain out of scope. The vignette may explain that spatial indexes matter for large PostGIS selections, but episcout does not create or tune them.

### Phase C — Specification-first EDA Coordinate Integration

The current seven EDA types remain unchanged. Coordinate meaning is orthogonal metadata, not an eighth storage type. A later additive specification may introduce optional `geo_role`, `geo_pair` and `geo_crs` fields:

| Field | Contract |
| --- | --- |
| `geo_role` | Blank, `x` or `y` for numeric/integer coordinate columns; future geometry-column support requires a separate contract. |
| `geo_pair` | Non-empty reviewed identifier shared by exactly one x row and one y row. |
| `geo_crs` | Same explicit resolvable CRS on both rows in a pair. |

The scaffold leaves these fields blank and never proposes coordinate roles from names, ranges, cardinality or correlations. Validation rejects incomplete, duplicated, type-incompatible or CRS-disagreeing pairs. Existing specifications without the fields retain identical behaviour.

A future `geo` EDA component may return one aggregate row per reviewed pair with total rows, complete pairs, missing-x, missing-y, both-missing, non-finite and reviewed-CRS range-failure counts plus an eligibility/status reason. It must not return coordinates, WKT/WKB, point identifiers, feature maps or a bounding box in the ordinary EDA bundle. The separate explicit `epi_geo_from_coords()`/`epi_geo_map()` path handles feature-level mapping after privacy review.

PostgreSQL-backed EDA computes the same pair counts in SQL without collecting coordinate values. It does not assume PostGIS is installed merely because a relation contains numeric coordinate columns. Geometry-column integration requires a later PostGIS-specific specification.

## Vignette Contract

The first implementation adds one canonical vignette, provisionally `vignettes/geospatial-mapping-primer.Rmd`, with this order:

1. State the learning goal, privacy warning and mapping-only boundary.
2. Explain features, attributes, geometry families, CRS/SRID, projection and validity.
3. Compare GeoPackage and Shapefile, including layer selection and Shapefile sidecars/limitations.
4. Read a neutral small fixture and inspect aggregate geometry metadata.
5. Convert explicitly declared x/y columns with a reviewed CRS and inspect blockers.
6. Transform to a suitable display/measurement CRS while distinguishing assignment from transformation.
7. Perform only basic reviewed operations needed for mapping, such as selection, crop and one spatial join, using direct `sf` calls where episcout adds no contract.
8. Create geometry-only and one-attribute static maps with `epi_geo_map()` and then extend them with ordinary `ggplot2` layers.
9. Write and re-read a GeoPackage safely.
10. Explain the later read-only PostGIS source and bounded collection workflow without requiring a live database to build the vignette.
11. Explain how reviewed coordinate metadata enters aggregate EDA QA without publishing points.
12. Distinguish descriptive mapping from spatial inference and list inference topics as future work without teaching or recommending a method.

The vignette uses a redistributable, pinned, neutral fixture with recorded provenance and no real patient, household, clinic or precise sensitive location. It builds offline and does not require PostGIS, network access or optional web-map services.

## Dependencies

- Add `sf` to `Suggests` in phase A and guard every exported geo function with `requireNamespace("sf", quietly = TRUE)`.
- Reuse `ggplot2` from `Suggests`; do not add `tmap`, `leaflet`, `mapview` or another plotting grammar for the static foundation.
- Reuse imported `DBI` and suggested `RPostgres` in phase B.
- Do not add `terra`, `raster`, `sp`, `rgdal`, `rgeos`, `lwgeom`, `units`, `s2` or spatial-modelling packages unless a later contract demonstrates a direct need not already satisfied transitively by `sf`.
- Record GDAL, GEOS and PROJ versions in diagnostic metadata where they can materially affect format, validity or transformation behaviour, but do not promise identical rendering across every system-library combination.

## Privacy And Scientific Safety

Geometry is value-bearing data. Exact points, lines and polygons must be treated according to the source's reviewed classification. A map is not de-identified merely because direct identifiers are absent, and aggregation does not automatically make small areas safe. Ordinary errors, warnings, logs, snapshots, examples, manifests and PR fixtures must contain no restricted coordinates or database connection details.

The package may validate structure, CRS presence, finite coordinates, reviewed geographic ranges and geometry validity. It cannot establish that a coordinate represents the intended person/place/time, that boundaries are historically appropriate, that a spatial join defines the right population, that a map denominator is valid or that an inferential model is justified. Those remain reviewed analytical decisions.

## Failure And Recovery Principles

- Validation completes before transformation, collection, plotting or writing where practical.
- Read and conversion failures do not create partial output objects that look complete.
- Database calls remain read-only and leave caller-owned connections open and usable.
- File publication is staged and exact-layer replacement is explicit; a failed operation preserves the previous destination.
- No function silently repairs geometry, swaps axes, assigns a missing CRS, drops features, truncates a database result or changes geometry type.

## Official References Consulted

- [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html) and [`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html) for file/database layer semantics and overwrite hazards.
- [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html), [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html), [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html) and [`sf::st_is_valid()`](https://r-spatial.github.io/sf/reference/valid.html) for coordinate conversion, CRS assignment/transformation and validity.
- [GDAL GeoPackage](https://gdal.org/en/stable/drivers/vector/gpkg.html) and [GDAL Shapefile](https://gdal.org/en/stable/drivers/vector/shapefile.html) driver documentation for format scope and limitations.
- [PostGIS spatial data model](https://postgis.net/docs/using_postgis_dbmanagement.html), [PostGIS spatial indexing](https://postgis.net/workshops/postgis-intro/indexing.html) and the [`sf` database vignette](https://r-spatial.github.io/sf/articles/sf2.html) for geometry/geography, SRID, indexed selection and DBI-backed reading.
- [`ggplot2::geom_sf()` and `coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) for the static mapping contract.

## Implementation Sequence And Gates

1. Complete the current roadmap sequence; this design plan does not displace issue #220 or the planned issue #217 work.
2. Open and approve a bounded phase-A implementation issue/spec covering the file/in-memory foundation and offline vignette.
3. Verify phase A on supported package-check platforms, including system-library diagnostics and safe GeoPackage round trips, before considering PostGIS collection.
4. Open a separate phase-B issue/spec for read-only PostGIS metadata and bounded collection, with mandatory disposable PostGIS integration tests.
5. Open a separate phase-C issue/spec for additive EDA coordinate metadata and aggregate QA after both EDA and geo contracts are stable.
6. Consider inferential spatial work only from a concrete epidemiological question, authoritative method, explicit population/denominator contract and independent numerical validation; do not create a generic inference grab-bag.
