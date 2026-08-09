# Test Design

Spec ID: `027-epi-geo-phase-a`
Status: Review

## Independent Basis

Tests use hand-authored neutral geometries whose counts, types, emptiness and bounds are evident from literal coordinate matrices. The transformation control uses the published EPSG:3857 spherical Mercator formula for the reviewed EPSG:4326 point `(1, 2)`, expected approximately `(111319.490793, 222684.208506)` metres; the asymmetric input detects axis reversal, and expected values are constants rather than production transformation output.

## Baseline Commands

```bash
scripts/check-workflow-state.sh
scripts/rscript_env_caller.R -e "cat(as.character(getRversion()), '\n'); cat(requireNamespace('sf', quietly = TRUE), '\n'); if (requireNamespace('sf', quietly = TRUE)) print(sf::sf_extSoftVersion())"
scripts/check-local.sh
```

Record the exact outcomes in this file before changing package code.

## Baseline Results On 2026-08-09

- `scripts/check-workflow-state.sh` reported spec 027 as the single active implementation specification and matched `antoniojbt/episcout@master`.
- The maintained wrapper used R 4.5.3. `sf` was not yet installed in the `episcout` environment, so GDAL, GEOS and PROJ versions were unavailable before the dependency was added.
- `scripts/check-local.sh` passed with no lints, all enabled tests passing, 15 expected PostgreSQL/parallel/visual skips and `R CMD check` at 0 errors, 0 warnings and 0 notes.
- Documentation regeneration and disabled visual-snapshot cleanup produced known unrelated side effects; every such file was restored to the clean `master` baseline before package-code changes.

## Behaviour Tests

- [x] Coordinate conversion preserves row and attribute order, exact x/y selection, CRS and `remove` semantics and returns the fixed audit structure.
- [x] Missing, non-finite and EPSG:4326 out-of-range rows block all conversion with independently counted audit fields.
- [x] Read selects one-layer and explicit multi-layer GeoPackages, retains typed zero-feature data and accepts complete Shapefile input while refusing ambiguous or missing layers and missing CRS.
- [x] Description reconciles hand-authored geometry types, empty/valid/invalid counts, attributes, CRS and bounds without feature identifiers.
- [x] Transformation preserves rows and attributes and matches the independent EPSG control within tolerance.
- [x] Geometry-only and value maps return extensible `ggplot` objects with geometry-aware aesthetics and visible missing-value scale settings.
- [x] GeoPackage writing creates a new layer, refuses silent collision, replaces only an authorised layer and retains unrelated layers.
- [x] Forced staged-write, reconciliation and publication failures preserve the previous destination and remove owned temporary files.

## Edge And Failure Tests

- [x] Zero-row, all-empty, non-syntactic-column and mixed single/multi family inputs retain deterministic structure.
- [x] Duplicate names, absent columns, identical x/y, non-numeric coordinates, invalid CRS and invalid scalar arguments fail clearly.
- [x] Geometry collections, missing CRS and Z/M dimensions fail before describe, transform, map or write.
- [x] Unsupported paths, URLs, virtual-file prefixes, non-spatial tables and Shapefile output fail.
- [x] Conditions contain no supplied coordinate values or ordinary attribute values.
- [x] Source inspection verifies every exported geo function has the optional dependency boundary without relying on uninstalling the maintained environment.

## Documentation And Render Checks

- [x] Roxygen regenerates `NAMESPACE` and all six help topics from source.
- [x] The vignette renders offline from neutral synthetic geometry, and its exact HTML is inspected for broken output, maps and disclosure claims.
- [x] A representative geometry-only map and value map are rendered and inspected at their intended vignette size.

## Final Local Evidence On 2026-08-09

- The focused geospatial suite passed every expectation, including deterministic injected inspection/read failures, the asymmetric EPSG transformation control and recovery branches.
- Package-loaded lint found no issues. Coverage measured all 319 coverable lines in `R/epi_geo.R` as exercised by the complete test suite.
- The maintained environment used `sf` 1.1.2 with GEOS 3.14.1, GDAL 3.13.2 and PROJ 9.8.1. The repository wrappers now propagate the matching PROJ/GDAL runtime data paths into both direct Rscript and CRAN-check processes.
- The workflow-state guard correctly distinguishes the new successor issue from the still-empty pull-request field, and validates both live trackers without tab-field drift.
- `scripts/check-local.sh` passed the complete enabled suite with 15 expected PostgreSQL, parallel and visual skips; `R CMD check` completed with 0 errors, 0 warnings and 0 notes.
- `scripts/check-cran.sh` built the source package, rebuilt every vignette, ran tests and generated both manuals successfully. It retained one external/incoming-feasibility NOTE for new-submission context, the existing missing prebuilt vignette index and two existing Stack Overflow URLs returning 403.
- The standalone primer rendered to HTML with all 12 sections and two embedded maps. The neutral polygon value map and point map with an explicit missing-value category were visually inspected at the vignette dimensions.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi-geo', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
scripts/check-workflow-state.sh
```
