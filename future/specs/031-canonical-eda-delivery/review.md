# Review Notes

Spec ID: `031-canonical-eda-delivery`  
Status: Review

## Findings

- The current database run already owns all aggregate tables, plot/map inventories, SVGs, timings and a five-column checksum manifest, so a renderer does not need a PostgreSQL connection or row-level handoff.
- The current in-memory report template depends on R plot/map objects; the database template must instead consume validated CSVs and owned relative SVGs.
- Existing overwrite validation requires exact manifest ownership and sibling staging, providing the correct atomicity model for report publication.
- Database plot preparation is compact/aggregate. Map collection is feature-level and must not be published as plot data.
- Supporting a new layout behind an opt-in argument avoids breaking the current flat bundle and keeps renderer compatibility explicit.

## Open Questions

None. Issue #245 and owner direction settle HTML ownership, compatibility and governance boundaries.

## Planning Handoff

- Planning base: canonical `master` at `e037512a4474ca6653b0cd079341efffa0a12047`.
- Draft planning PR #251 passed PostgreSQL, Ubuntu, macOS, coverage, both
  Codecov gates and CodeFactor at `4b7b10b`; it has no actionable review
  feedback.
- Implementation is active on `agent/implement-canonical-eda-delivery`, stacked
  on that green planning head under the owner's instruction without waiting for
  merge.

## Implementation Evidence

- `epi_eda_db_run()` now has an opt-in delivery layout while its default flat paths, result components and run-metadata schema remain exact. Delivery-only contract fields live in `run_manifests/delivery_metadata.csv`.
- `epi_eda_render_db_report()` re-reads a completed flat or delivery bundle from disk, validates exact ownership, safe relative paths, regular files, checksums and aggregate consistency, then publishes README/HTML through the existing sibling swap/restore path. A run result contributes only `output_dir`.
- Delivery runs close the repeatable-read snapshot before report rendering. Compact aggregate plot inputs are manifest-owned under `plot_data/`; coordinate/theme collections and source rows are never published there.
- The report presents fixed aggregate tables, empty/skipped states and owned relative SVG galleries. README and report use the single analyst-ownership contract and do not add core approval, sensitivity or sharing decisions.
- README, NEWS, both affected EDA vignettes, package help, project template and the installed database walkthrough now distinguish the normal HTML entry point from canonical CSV/SVG/manifest evidence.

## Local Verification

- Focused renderer tests passed with 61 expectations; the live PostgreSQL renderer, coordinate, mapping, parity and source suites passed against disposable PostgreSQL 18.4.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. `scripts/check-cran.sh` completed with the inherited single incoming-feasibility NOTE for new-submission/vignette-index metadata and two existing Stack Overflow URLs returning 403; code, tests, vignettes and both manuals passed.
- `scripts/check-workflow-state.sh` matched GitHub, package-loaded lint was clean, `git diff --check` passed, and changed R files were styled with the repository toolchain.
- The CI-equivalent instrumented live suite completed at 92.16% package coverage; the new renderer measured 95.21% and the changed database runner measured 92.50%.
- The installed database walkthrough completed end to end, removed its three uniquely owned schemas and published exactly 41 manifest-owned files; every non-manifest checksum reconciled independently.
- Representative geometry-only, numeric-theme, categorical-theme, missing-theme and skipped-map states were inspected in generated SVG/HTML. Report links were relative, the self-contained HTML had no network or absolute local asset path, and deterministic re-rendering preserved report bytes.

## Closeout Notes

- Draft implementation PR #252 is open and stacked on green planning PR #251; merge commit is pending.
- Required pull-request checks: pending. Local material exception: the inherited CRAN incoming-feasibility NOTE recorded above.
- Tracking issue and roadmap disposition: #245 ready next under #249; #248 is successor.
