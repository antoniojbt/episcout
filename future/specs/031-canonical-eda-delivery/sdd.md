# Software Design

Spec ID: `031-canonical-eda-delivery`  
Status: Draft

## Scope

The implementation covers completed PostgreSQL EDA bundles. It adds an opt-in delivery layout to `epi_eda_db_run()` and a renderer for a completed on-disk bundle. The in-memory data-frame renderer and default flat bundle remain unchanged.

## Public API

1. Extend `epi_eda_db_run()` with `layout = c("bundle", "delivery")` and `quiet = TRUE`.
   - `layout = "bundle"` is the default and is byte/schema/path compatible with the current contract; `quiet` is ignored.
   - `layout = "delivery"` publishes the canonical tree and renders HTML before the staged bundle is atomically published.
   - Delivery mode validates that `rmarkdown`, `knitr`, the bundled template and Pandoc are available before opening the PostgreSQL snapshot.
   - The selected layout is included in overwrite fingerprints. Default bundle
     metadata keeps its existing schema; delivery mode records its additional
     contract in `run_manifests/delivery_metadata.csv`.
2. Add `epi_eda_render_db_report(bundle, overwrite = FALSE, quiet = TRUE)`.
   - `bundle` is either one `epi_eda_db_run` result or one local bundle-directory path.
   - A result object contributes only its normalized `output_dir`; all content is re-read and validated from disk.
   - The function returns the normalized HTML path.
   - It creates `README.md` and `reports/eda-report.html` inside the same owned root, adds them to the five-column manifest and republishes the directory atomically.
   - Existing owned report files require `overwrite = TRUE`; unrelated or unowned files always fail.

## Canonical Delivery Tree

The caller supplies the exact `output_dir`; documentation recommends `YYYYMMDD_eda_cycle` but the package does not infer or enforce calendar meaning.

```text
output_dir/
├── README.md
├── reports/eda-report.html
├── plots/*.svg
├── maps/*.svg
├── QA_QC/*.csv
├── plot_data/*.csv
└── run_manifests/*.csv
```

- `QA_QC/` contains schema, missingness, six canonical summary components, identifier QA, geo QA and plot/map inventories.
- `run_manifests/` contains the five-column manifest, existing run metadata,
  delivery-only metadata, messages, specification, source metadata and query
  timings. Delivery metadata records the layout, report path and contract
  version without changing the default bundle's metadata schema.
- `plot_data/` contains only compact aggregate data already derived for ordinary PostgreSQL plots. It never contains coordinate/theme collections used for maps.
- Each regular file has exactly one manifest-owned relative path. The manifest's own checksum remains blank; every other created file has an MD5 checksum.
- README points to `reports/eda-report.html` first and explains the retained folders in plain language.

## Bundle Rendering

1. Resolve and normalize the root without following a root symlink.
2. Locate the manifest at `manifest.csv` for the legacy flat layout or `run_manifests/manifest.csv` for delivery layout; ambiguity fails.
3. Require a complete database workflow contract, exact five-column manifest, unique artifact/path keys, regular files only, no symlinks anywhere below the root, exact file ownership and matching checksums.
4. Require and parse the fixed aggregate artifacts by manifest artifact name, not by guessed filenames. Reject missing, duplicate, incompatible or unreadable schemas with value-free errors.
5. Render in a sibling staging copy. The report reads only parsed aggregate tables and relative SVG paths. It has no connection/source-row parameter and no database package call.
6. Add or replace only the owned README/report rows, refresh checksums, revalidate the complete staged root, then use the existing owned-directory swap/restore path. Any failure leaves the original bundle byte-for-byte unchanged.

## Report Content

The portable report contains run metadata, schema, missingness, all six canonical summary tables, identifier QA, coordinate-pair QA, messages, plot inventory/gallery, map inventory/gallery and query timing metadata. Empty components and not-created plots/maps are presented factually. Relative links resolve after moving the complete root. CSS and JavaScript, if any, are local/embedded; no network assets are used.

The report and README use one ownership statement: episcout creates the requested outputs and the analyst decides how they are used. They do not add review gates, sharing decisions, disclosure labels, sensitivity classifications, automatic suppression or policy advice.

## Compatibility

- Existing calls, result components, metadata schemas, flat paths and
  five-column manifests under `layout = "bundle"` do not change.
- `epi_eda_render_report()` remains unchanged.
- The new renderer accepts current valid flat bundles and new delivery bundles. Rendering a flat bundle adds only owned `README.md` and `reports/eda-report.html` rows; it does not reorganize existing files.
- Delivery manifests retain the same five columns; layout-specific paths are explicit in the rows.
- An already rendered bundle is valid input. Without `overwrite = TRUE` it fails before staging; with overwrite it is deterministically republished.

## Errors And Failure Modes

All public errors are concise and value-free. Fail before database work for invalid layout or missing render dependencies. Fail before rendering for invalid result/path types, absent/incomplete workflows, legacy sensitivity manifests, path traversal, rooted/backslash paths, case-insensitive path collisions, symlinks, unowned files, checksum drift or incompatible aggregate schemas. Never delete or partially update the original bundle.

## Dependencies

Reuse suggested `rmarkdown`/`knitr`, existing Pandoc discovery and existing atomic bundle helpers. Add no strong dependency and no network requirement.
