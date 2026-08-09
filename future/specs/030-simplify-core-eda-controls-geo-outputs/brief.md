# Brief

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Active
Owner: repository-owner
Tracking issue: #243

## Problem

Core EDA currently mixes technical data semantics with institutional governance. Data-frame scaffolds include observed-count evidence and approval states; reusable PostgreSQL dictionaries contain privacy/action/profile policy; intake, preparation, summaries, plots and reports gate or suppress outputs by review or role; core manifests classify sensitivity. Phase C also deliberately stopped at aggregate coordinate QA, so an analyst cannot request a bounded point map through the ordinary EDA runners.

These behaviours make the core EDA contract decide whether requested outputs are permitted rather than simply producing technically valid, reproducible results. The same combined dictionary is also consumed by specialised pseudonymisation, preventing a clean semantic handoff from security output into EDA.

## Goal

Make ordinary data-frame and PostgreSQL EDA semantic and analyst-directed. Remove approval, classification, disclosure and role-suppression behaviour while retaining correctness and publication safeguards. Add explicit bounded point-map options based only on declared coordinate pairs, and move privacy/action/validation policy into a required `epi_sec_linkage_*` column component.

## Non-goals

- Consolidating the four existing EDA entry points.
- Inferring axes, coordinate pairs, CRS, thematic variables, administrative boundaries or sharing permission.
- Adding line, polygon, raster, basemap, boundary-join, geocoding, web-map or inferential workflows to EDA.
- Adding direct PostgreSQL aggregate-bundle HTML rendering; issue #196 remains deferred.
- Weakening pseudonymisation registry, linkage, crosswalk, rollback, schema-access or restricted-output safeguards.
- Adding compatibility shims for the breaking scaffold, dictionary, linkage or core-manifest schemas.
- Publishing a release, tag or CRAN submission.

## Candidate Files

- Core specifications and dictionaries: `R/eda_spec*.R`, `R/eda_dictionary.R`
- Core analysis: `R/eda_prepare.R`, `R/eda_summaries.R`, `R/eda_stratified.R`, `R/eda_plots.R`, `R/eda_geo.R`
- Runners and publication: `R/run_eda.R`, `R/eda_intake.R`, `R/eda_db_run.R`, `R/eda_postgres_queries.R`, `R/eda_report.R`
- Security separation: `R/epi_sec_linkage.R`, `R/epi_sec_pseudonymise_db.R`
- Tests, report templates, README, NEWS, vignettes, installed examples and generated help

## Risks

- Observation collection could escape the PostgreSQL snapshot, exceed the explicit bound or fetch unrequested fields.
- Failed QC could accidentally create partial maps or silently omit rows.
- Dynamic SVGs could break exact manifest ownership, checksum or overwrite guarantees.
- Removing dictionary policy before linkage policy is complete could weaken identifier and retained-column controls.
- Declared missing sentinels could render as thematic values rather than the existing missing colour.
- Explicit high-cardinality text themes can create large legends and SVGs; no hidden collapse or cardinality gate is authorised.
- The two existing HTML paths could diverge in map and ownership language.

## Terminal Outcome

The issue is complete when core EDA is semantic-only, the requested point-map contract works with data frames and bounded PostgreSQL sources, specialised security policy is decoupled without regression, migration guidance is complete and all required local/CI evidence is green. No automatic successor is required; remaining PostgreSQL report rendering stays under issue #196.
