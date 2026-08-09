# Software Design

Spec ID: `028-epi-geo-postgis`
Status: Review

## Public API

```r
epi_geo_postgis_source(con, schema, relation, geometry_column = NULL)
epi_geo_postgis_describe(source)
epi_geo_postgis_collect(source, columns = character(), bbox = NULL, max_features = 10000L)
```

`epi_geo_postgis_source()` accepts one open idle RPostgres connection and plain undotted identifiers. It supports persistent tables, views and materialized views. It requires the PostGIS extension in the current database, catalogues geometry/geography columns from PostgreSQL types and PostGIS metadata, and requires `geometry_column` when more than one exists. The returned fixed-shape source retains the caller connection for execution but its print and structure methods expose only relation kind, storage kind, declared geometry type, dimension and SRID status. A fingerprint covers relation OID, relation kind, ordinary catalogue and spatial metadata.

`epi_geo_postgis_describe()` revalidates the source, begins an owned transaction, sets `REPEATABLE READ READ ONLY`, revalidates again and runs aggregate QA. Its fixed return contains `source`, `dataset`, `geometry_types`, `srids`, `dimensions`, `validity`, `bounding_box` and `messages`. The bounding box is a standard `sf::bbox` with numeric `xmin`, `ymin`, `xmax`, `ymax` and the resolved source CRS; zero non-empty geometries produce a typed bbox with missing ordinates. The object is explicitly classified as a sensitive location aggregate in `source` and `messages`. No component contains SQL, WKT/WKB, feature coordinates beyond that reviewed aggregate, ordinary attributes, connection data or native database condition text.

`epi_geo_postgis_collect()` is the only feature materialisation path. `columns` is a unique character allow-list of exact non-geometry columns. `bbox` is `NULL` or an `sf::bbox` with finite ordered limits and a CRS resolving to the source's positive SRID. `max_features` is one positive integer not exceeding the canonical R integer range. Inside one read-only repeatable-read transaction the function revalidates catalogue and aggregate type/SRID/dimension constraints, counts the complete reviewed selection, refuses an over-limit count, then fetches only the allow-listed columns plus geometry. The database query uses quoted identifiers, bound bbox ordinates and `LIMIT max_features + 1`; the returned row count must equal the checked count. The result is converted from EWKB through `sf`, retains relation row order without claiming stable ordering, and is reconciled through the existing Phase-A `sf` validator.

## Spatial Contract

- Storage kinds: PostGIS `geometry` and `geography`.
- Geometry families: POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON and MULTIPOLYGON.
- Dimensions: XY only. Z, M and ZM are rejected.
- SRID: one positive observed SRID matching the declared positive SRID. Empty or all-null relations still require a positive declared SRID.
- Permissive declarations such as `geometry(Geometry, 0)` may be constructed, but describe and collect refuse unknown/mixed runtime metadata. This makes catalogue inspection possible without treating it as collection authority.
- Geography is converted to geometry only at the explicit query boundary; its supported SRID and XY contract are reconciled identically.

## SQL And Data Locality

All identifiers pass `DBI::dbQuoteIdentifier()` and all values pass `dbBind()`. Catalogue queries return at most the declared number of rows. Description uses grouped aggregate queries for type/SRID/dimension counts, one aggregate validity/count row and one scalar extent row; it never selects the geometry column as a result value. Collection first returns one scalar count and then at most `max_features + 1` feature rows. No public argument is inserted as SQL text except after identifier quoting.

## Transactions And Recovery

Both executing APIs require an idle connection, own exactly one transaction and commit only their read-only work. Begin, setup, query or commit failures trigger rollback when possible and preserve fixed value-free conditions. Existing caller transactions are rejected. Disconnection after source construction, catalogue drift and relation replacement require a new source. No function disconnects the caller-owned connection.

## Privacy And Safety

Aggregate geometry metadata and bounds are not de-identified. Documentation requires disclosure review before printing or sharing them. Source methods and conditions omit schema/relation/column values, SQL, query parameters, credentials, native notices and server details. Collection deliberately returns reviewed feature attributes and geometry and therefore inherits their source classification.

## Compatibility And Dependencies

The API is additive. It reuses imported DBI, suggested RPostgres and suggested sf. Existing Phase-A functions and the PostgreSQL EDA source are unchanged. CI uses the upstream `postgis/postgis:17-3.5` service image, preserving the supported PostgreSQL 17 baseline while making the extension available without package-side DDL.

## Official References Consulted

- [PostGIS `ST_Extent`](https://postgis.net/docs/ST_Extent.html) for exact aggregate XY bounds in the source coordinate system.
- [PostGIS spatial reference](https://postgis.net/docs/reference.html) for `ST_SRID()`, `ST_NDims()`, geometry types, validity, envelopes and intersection predicates.
- [PostGIS Docker image](https://github.com/postgis/docker-postgis) for the supported PostgreSQL 17/PostGIS 3.5 CI service and its pre-enabled extension contract.
