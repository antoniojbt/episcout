# Future Test Design

- Spec ID: `026-epi-geo-series-plan`
- Status: Completed plan; executable tests deferred to implementation specs

## Boundary

This document maps the proposed series to independently reviewable behaviour tests. Spec 026 adds no executable tests, fixtures, dependencies or package code. Each implementation phase must refine the relevant rows before changing package behaviour.

## Phase A Behaviour-test Matrix

| Area | Required scenarios | Intended evidence |
| --- | --- | --- |
| Optional dependency | `sf` unavailable and available | Every exported geo function fails with one actionable dependency message when unavailable; non-geo package use remains unaffected. |
| GeoPackage read | One layer, multiple layers, explicit layer, empty layer, absent layer, non-spatial table, missing CRS and unsupported geometry | Exact layer selection, stable feature/attribute order, typed empty output and actionable refusal of ambiguous/unsupported input. |
| Shapefile read | Complete sidecar set, missing required sidecar, long/truncated field metadata, mixed single/multi geometry and encoding metadata | Import limitations are visible and no unsupported lossless-round-trip claim is made. |
| Coordinate conversion | Ordinary x/y, non-syntactic names, reversed requested names, WGS 84 boundaries, projected coordinates, missing pairs, `NaN`/infinity, non-numeric columns, duplicate names, zero rows and retained/removed source columns | Exact axis selection and CRS assignment, all-or-nothing blocker audit, preserved row order and no implicit dropping/coercion. |
| Description | Point, line, polygon, multi-geometry, mixed types, empty/missing/invalid geometry, zero features, geographic and projected CRS | Hand-authored feature counts, type counts, validity counts, units and bounds reconcile without exposing feature identifiers or invalidity coordinates. |
| Transformation | Known control points, identity transform, unavailable transform, missing source CRS, target CRS and empty geometry | Coordinates agree with an authoritative control within a documented tolerance; assignment is never substituted for transformation; attributes/order remain unchanged. |
| Static map | Geometry-only point/line/polygon, one numeric/categorical attribute, missing values, mixed unsupported geometry and absent attribute | Returned class, geometry layer, aesthetic mapping, missing-value representation and CRS metadata are exact; visual review supplements but does not replace layer-data assertions. |
| GeoPackage write | New file, new layer in existing file, exact-layer collision, authorised replacement, invalid target, interrupted stage and re-read mismatch | No silent overwrite, unrelated layers remain intact, previous output survives failure and staged round-trip metadata reconcile. |
| Privacy | Restricted-looking coordinates/attributes in failures, warnings, snapshots and manifests | Conditions and metadata are value-free except for explicitly returned user data; fixtures contain neutral synthetic/public geometry only. |

## Phase B PostGIS Matrix

| Area | Required scenarios | Intended evidence |
| --- | --- | --- |
| Source constructor | Table/view/materialized view as approved, schema/name quoting, no PostGIS extension, zero/one/multiple geometry columns, geometry/geography, mixed/unknown SRID and changed catalogue | Value-free validated source, exact relation identity, explicit ambiguity failures and no credentials/connection attributes in print or structure output. |
| Aggregate description | Null, empty, valid and invalid geometries; type/SRID mixtures; zero rows; concurrent source change | Counts and classified bounds reconcile inside one read-only snapshot; no WKT/WKB/coordinate/attribute result is collected. |
| Bounded collection | Below, at and above bound; explicit columns; absent/private columns; bounding box edges; empty selection; no stable key; connection/query failure | Fail rather than truncate, collect only selected attributes plus geometry, bind extent values, quote identifiers and leave the caller connection usable. |
| Data locality | Instrumented DBI result sizes and query inventory | Only catalogue/aggregate rows are collected during description; feature geometry is collected only by the explicit bounded function. |
| Lifecycle | Existing transaction, rollback, disconnect after construction and server notices/errors | No caller transaction is committed, connection details do not leak and failures leave owned resources cleared. |

## Phase C EDA Matrix

| Area | Required scenarios | Intended evidence |
| --- | --- | --- |
| Specification compatibility | No geo fields, complete x/y pair, incomplete pair, duplicate role, different pair IDs/CRSs, non-numeric type and unknown geo role | Existing specifications behave identically; invalid reviewed metadata fails before profiling. |
| Scaffold | Coordinate-like and ordinary names/values | New geo fields remain blank for every row; no value/name inference occurs. |
| Aggregate pair QA | Complete, x-only missing, y-only missing, both missing, non-finite, WGS 84 boundary/out-of-range and zero-row pairs | One independently calculated value-free row per pair with reconciled denominators and stable statuses. |
| PostgreSQL parity | Same neutral fixture as a data frame and PostgreSQL relation | Aggregate pair QA agrees without collecting coordinate values or requiring PostGIS for numeric pairs. |
| Standard bundle privacy | Coordinate roles with plots/reports enabled | No coordinates, WKT/WKB, feature map or pair bounds enter ordinary returned objects, CSV, HTML, SVG, logs or manifests. |

## Independent Fixture Strategy

- Build a tiny neutral fixture from hand-authored WKT/coordinates whose feature counts, types, bounds and validity expectations are written independently of the package functions under test.
- Include an authoritative transformation control with source/target CRS, input/output coordinates, tolerance and source citation; do not generate expected values using the production `sf::st_transform()` call.
- Pin any external vector fixture by immutable source identifier, licence, checksum and extraction instructions. Prefer a deliberately small repository fixture over downloading during tests.
- Create disposable PostGIS relations during integration tests and drop only owned test schemas. Never commit database dumps, connection strings or real locations.
- Inspect `ggplot_build()` layer data and coordinate metadata for map correctness; use reviewed snapshots only as supplemental rendering evidence.

## Candidate Focused Commands

The exact test filenames belong to later implementation specs. They should use the repository wrapper, for example:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'geo', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'geo-postgres', reporter = 'summary')"
scripts/check-local.sh
```

Release-oriented changes or system-library compatibility work must also run `scripts/check-cran.sh` and the repository's supported GitHub Actions jobs.

## Acceptance Evidence Required Per Phase

- Focused behaviour tests pass with independently authored expectations.
- Package-loaded lint and `scripts/check-local.sh` pass.
- Phase A records GDAL/GEOS/PROJ diagnostics and inspects the rendered vignette and representative maps.
- Phase B passes mandatory live disposable PostGIS tests and structural data-locality assertions.
- Phase C demonstrates byte/content absence of coordinates and geometries from ordinary EDA bundles.
- Coverage changes are measured and material gaps explained; coverage alone is not correctness evidence.
