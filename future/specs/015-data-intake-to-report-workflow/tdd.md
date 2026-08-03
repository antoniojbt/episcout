# Test Design

Spec ID: `015-data-intake-to-report-workflow`
Status: Completed

Use small synthetic fixtures containing no real participant data. Hand-author expected statuses, counts, filenames and privacy markers; do not generate expected results through the intake workflow under test. Existing component tests remain regression evidence, while intake tests independently reconcile at least one numeric, categorical, missingness and grouped result.

## Test File

- `tests/testthat/test-eda-intake.R`

Use a fresh temporary output directory per case. Report tests exercise the built-in base-R renderer and inspect escaped HTML; they require no `rmarkdown`, Quarto, browser, network service, Codecov credential or developer-specific path.

## Baseline Command

Run before package-code changes and record the result in `review.md`:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-spec|eda-prepare|eda-summaries|eda-stratified|eda-table1|eda-report', reporter = 'summary')"
git status --short
```

## Interface And Return Contract

- [x] Exact formals/defaults, class and top-level component order include `missing`, `table1` and `metadata`.
- [x] Status is limited to `review_required`, `blocked`, `audit_complete`, `complete`; processing stage is limited to `intake`, `audit`, `preparation`, `canonical_summary`, `stratified_summary`.
- [x] Optional not-yet-run components are `NULL`; summary and stratified component names/orders match specs 010 and 014.
- [x] `input` is exactly a value-free `field`/`value` long data frame with deterministic ordered column-name/class fields.
- [x] `spec` is exactly a list with `state`, `source`, `source_name`, `fingerprint_sha256`, `data`; each generated/data-frame/CSV/invalid state is distinguished.
- [x] `report` is exactly a list with `requested`, `created`, relative `path`, `reason`; `metadata` is a one-row data frame derived from final input metadata.
- [x] `messages` has exactly `stage`, `severity`, `subject`, `reason`, `recommended_action` and preserves creation order.
- [x] Data-frame/spec forms, existing local CSV, output path, preparation enum, nullable single stratifier, flags and nullable source ID validate actionably.
- [x] Invalid data/name/scalar arguments, remote/missing spec paths, absolute source IDs and unsafe output targets error before bundle mutation.
- [x] Caller data and specification remain identical on complete, gated, blocked and argument-error paths, including reference-capable inputs where available.

## Intake, Audit And Preparation

- [x] `spec = NULL` writes only intake/scaffold/guide/messages/manifest plus requested incomplete report, returns `review_required` at `intake`, and runs no audit or summary.
- [x] Scaffold data remain `review_required`; the static guide explains exact review fields/rerun action without observations or examples.
- [x] Scaffold failure and invalid supplied specification return `blocked` at `intake` with truthful spec state and actionable message.
- [x] Scaffold-shaped content records state `reviewed` only when every row status is exactly reviewed; any missing/non-reviewed row records `review_required`, appears in metadata and blocks analysis. A core specification without evidence records `caller_asserted` plus warning.
- [x] CSV provenance stores only basename and the same SHA-256 fingerprint as equivalent normalized data-frame content; no absolute path is serialized.
- [x] Blocker-free audit mode returns `audit_complete` at `audit`, retains schema/audit, and creates no after-schema or summaries.
- [x] Preparation audit blockers return `blocked` at `audit`, retain value-free audit artifacts and prevent apply/analysis.
- [x] `prepare = "none"` blocks every planned/positive-change audit action and proceeds only when no transformation is required.
- [x] `prepare = "apply"` retains before/after schemas and final applied audit, then summarizes the in-memory prepared result without returning/writing it.
- [x] Sentinel, type, level, requiredness and temporal blockers preserve the all-or-nothing component contract and prevent downstream artifacts.

## Canonical Privacy And Reconciliation

- [x] A compatible run writes `missing.csv` and exactly six `summary_*` artifacts with stable schemas, returning `complete` at `canonical_summary` when unstratified.
- [x] Hand-derived numeric mean/missing counts and categorical numerator/observed denominator agree with the exported components.
- [x] Every type-specific component's exact variable membership agrees with the canonical `variables` type/status rows; a removed/extra/misclassified member blocks completion.
- [x] Every present non-excluded variable satisfies `n_missing + n_observed = n`; missingness agrees with canonical counts; categorical totals and finite/infinite counts reconcile.
- [x] Explicit roles `id`/`identifier` are removed only from the private profiling view, returned/exported as policy-skipped, and have unavailable missing counts.
- [x] Raw identifier markers and raw free-text markers occur nowhere in returned analytical fields, CSVs or HTML; specification/source metadata are explicit reviewed exceptions.
- [x] No identifier inference occurs from names or uniqueness; non-identifier canonical values match the direct canonical profiler.
- [x] Zero-row/all-missing/constant inputs and empty components preserve fixed schemas and typed unavailable values.

## Stratification And Table 1

- [x] Valid optional stratification writes all eight stratified components and `table1.csv`, returns `complete` at `stratified_summary`, and leaves canonical output identical to the unstratified run.
- [x] Input/included/group and group-variable counts reconcile; exact Overall variable/numeric/text/temporal/categorical fields, denominators and explicit categorical missing rows/proportions agree with canonical results.
- [x] An invalid categorical/type stratifier retains canonical artifacts, creates no grouped artifacts and returns `blocked` at `canonical_summary` with an actionable message.
- [x] An explicit identifier-role stratifier blocks before grouped calculation even when its storage/levels could otherwise be grouped.
- [x] Table 1 remains traceable to numeric component fields and contains no raw text, p-values, implicit suppression or disclosure-safe claim.
- [x] Simulated Table 1 failure returns `blocked` at `stratified_summary`, retains all reconciled stratified return/file components and leaves Table 1 absent.

## Manifest And Output Safety

- [x] Manifest columns are exactly `artifact`, `type`, `path`, `status`, `sensitivity`, `checksum_md5`; registry order and all 26 relative basenames are fixed.
- [x] Status is only `created`/`not_created`; created rows correspond exactly to files present and absent rows remain explicit.
- [x] Every created artifact except manifest has the independently recomputed MD5; manifest and not-created rows have empty checksums.
- [x] Sensitivity is only `internal_review`, `specification_review` or `disclosure_review` and matches artifact type.
- [x] Default overwrite refusal leaves any existing non-empty directory byte-identical.
- [x] Authorised overwrite of a non-empty directory requires an exact prior intake manifest, entries equal to manifest-created paths and matching non-empty MD5 checksums for every created non-manifest artifact.
- [x] An unowned/missing/impostor/modified file, directory, symlink, other special file, malformed manifest or registry/status/path mismatch is refused before target mutation.
- [x] Valid prior-bundle overwrite removes stale owned outputs, so a later scaffold run contains no stale summary/Table 1 files.
- [x] New artifacts are built under a unique sibling staging directory; per-file writes use staging-local temporary files/replacement and immediate manifest refresh.
- [x] A staging failure cleans the unpublished sibling and leaves every prior target file/checksum unchanged.
- [x] Final publication renames a validated prior target to a sibling backup and staging to target, then removes backup; a simulated final swap failure restores the backup where possible and errors.

## Base-R Report And Recovery

- [x] Final status/stage, specification state/source and `finished_at_utc` are saved before the requested report reads metadata; the displayed completion timestamp is populated and current.
- [x] Requested complete/review-required/blocked/audit-complete reports are built from files already marked created and display status, last data stage and complete/incomplete disclosure banner accurately.
- [x] The report includes only relative artifact links and saved CSV tables, uses base-R HTML escaping for every heading/cell, and contains no calculation or template-engine dependency.
- [x] HTML-special fixture metadata are escaped rather than executable; the report includes no remote asset, script, analytics or network request.
- [x] `render = FALSE` leaves report not requested/not created without preventing an otherwise complete analysis.
- [x] Simulated report failure after complete analysis retains analysis artifacts, changes status to `blocked` at the last completed data stage, saves the updated timestamp/messages, removes a partial report and leaves its manifest row `not_created`.
- [x] Simulated report failure at `review_required`, `audit_complete` or pre-existing `blocked` gates preserves that underlying status while adding the report blocker and retaining gate artifacts.

## Privacy And Reproducibility

- [x] Search returned character fields and every artifact for row markers, identifier values, free-text examples, bridge-table markers, secret-like tokens and automatically derived absolute paths.
- [x] Input/specification metadata, category/group labels and aggregate summaries carry their explicit review sensitivity; every report warns that the bundle is not de-identified or disclosure-controlled.
- [x] The implementation performs no network access, upload, telemetry, Codecov interaction, pseudonymisation or row-level write.
- [x] Same normalized data/spec/options yield identical statistical values, component/group/message ordering and specification fingerprint despite different timestamps/directories.

## Regression And Acceptance Commands

- [x] Existing scaffold, preparation, canonical, stratified, Table 1, legacy run and legacy report tests remain unchanged and pass.
- [x] README, NEWS, vignette, roxygen, generated Rd and NAMESPACE agree with exact statuses, filenames, base-R report, overwrite preflight and privacy boundaries.
- [x] The documented scaffold/review/audit/apply/canonical/stratified/report walkthrough runs with synthetic data.
- [x] Record focused tests, package-loaded lint, full local/CRAN checks and whitespace validation; external limitations do not change the contract.

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-intake|eda-spec|eda-prepare|eda-summaries|eda-stratified|eda-table1|eda-report', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```
