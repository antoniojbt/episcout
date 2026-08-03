# Software Design

Spec ID: `015-data-intake-to-report-workflow`
Status: Completed

## Semantic Authority

The supplied reviewed specification is authoritative for declared variable semantics. Observed data determine storage, counts and whether approved transformations are executable; they never establish scientific role, missing sentinels, units, categories, identifying status or approval. When scaffold evidence is present, every `review_status` must be exactly `reviewed` before analysis. A specification without scaffold evidence retains the existing explicit caller-asserted contract and is labelled `caller_asserted`, never inferred or automatically approved.

The implementation composes `epi_eda_spec_scaffold()`, `epi_eda_spec()`, `epi_eda_prepare()`, `epi_eda_profile_missing()`, `epi_eda_profile_summaries()`, `epi_eda_profile_stratified()` and `epi_eda_table1()`. The intake layer adds stage control, policy exclusions, reconciliation, artifacts and a presentation view; it does not add statistical formulae or conversion rules.

## Public API

```r
epi_eda_intake_run(
  data,
  spec = NULL,
  output_dir,
  prepare = c("none", "audit", "apply"),
  strata = NULL,
  render = TRUE,
  overwrite = FALSE,
  source_id = NULL
)
```

`data` is an in-memory data frame. `spec` is `NULL`, a data frame or one existing local CSV path accepted by `epi_eda_spec()`; network URLs are rejected. `output_dir` is one non-empty path and is created when absent. `prepare` uses `match.arg()` and defaults to `"none"`. `strata` is `NULL` or one non-empty character name. `render` and `overwrite` are scalar non-missing logicals. `source_id` is `NULL` or one non-empty, single-line character identifier and cannot be an absolute Unix or Windows filesystem path.

Return class `c("epi_eda_intake", "list")` with fixed components in this order: `status`, `stage`, `output_dir`, `manifest`, `input`, `spec`, `schema_before`, `schema_after`, `preparation_audit`, `missing`, `summary`, `stratified`, `table1`, `report`, `messages`, `metadata`.

`status` is exactly `review_required`, `blocked`, `audit_complete` or `complete`. `stage` is the last successfully completed data-processing stage and is exactly `intake`, `audit`, `preparation`, `canonical_summary` or `stratified_summary`. Report generation is represented separately and never advances the processing stage.

Malformed top-level arguments, unsafe output collisions and failures of the workflow's own filesystem/serialization machinery are R errors. Once a safe bundle exists, invalid specification content, review/preparation findings, invalid stratification, statistical-component reconciliation failures and report failures return the stable run object with an appropriate non-complete status and actionable messages.

## Fixed Return Components

`input` is an ordinary long data frame with exactly `field` and `value` character columns. It contains workflow contract/version, input dimensions, optional caller-supplied source ID, requested options, package/R/relevant dependency versions, UTC start/end timestamps, final status/stage and specification fingerprint. Source columns add ordered `column.NNN.name` and `column.NNN.class` fields. It contains no source values, previews, minima, maxima or distinct values.

`spec` is an explicit list with exactly `state`, `source`, `source_name`, `fingerprint_sha256` and `data`. State is `not_supplied`, `review_required`, `invalid`, `reviewed` or `caller_asserted` as applicable. Scaffold-shaped content is `reviewed` only when every `review_status` is present and exactly `reviewed`; otherwise its state is `review_required` and the preparation audit blocks downstream work. Source is `none`, `generated`, `data_frame` or `csv`. A CSV source name is only `basename(path)`; other source names are typed `NA`. `data` contains the generated or parsed normalized specification when available and is `NULL` for absent/invalid content. The SHA-256 fingerprint is computed from the normalized specification and is blank when none is available.

`schema_before`, `schema_after`, `preparation_audit`, `missing`, `summary`, `stratified` and `table1` are `NULL` until their stage succeeds. Once present, schemas/audits and analytical components use their existing public contracts, subject only to the explicit identifier-policy annotation described below. `summary` has exactly `variables`, `numeric`, `categorical`, `text`, `temporal`, `skipped`; `stratified` retains all eight spec 014 components.

`report` is a list with exactly `requested`, `created`, `path` and `reason`. `requested`/`created` are logical, a created path is the relative `report.html`, and an absent path is typed `NA`. `reason` truthfully distinguishes not requested, not yet run, created, and failed rendering.

`messages` is an ordinary data frame with exactly `stage`, `severity`, `subject`, `reason`, `recommended_action`. Severity may be `info`, `warning` or `blocker`. Messages may name variables/artifacts and report value-free reasons, but never reproduce source observations, unexpected observed values, free text, credentials or absolute source/spec paths.

`metadata` is a one-row wide data frame derived from the completed input metadata. It contains workflow/package/R/relevant dependency versions, requested options, UTC timestamps, final status/stage and the specification fingerprint. `output_dir` is the normalized absolute directory for immediate programmatic use; serialized paths remain relative.

## Stage 1: Intake And Scaffold

Validate all top-level arguments and the output boundary before bundle writes. Write the value-free `intake_metadata.csv` first.

When `spec = NULL`, call `epi_eda_spec_scaffold(data)`. On success write `spec_scaffold.csv` and the static `review_guide.md`, add a review warning, return `review_required` at `intake`, and stop before audit/preparation/analysis. The report may still be created as an explicitly incomplete view when requested. Scaffold failure returns `blocked` at `intake` with no partial specification claim.

The guide explains the fields requiring human review, the exact review-status action and rerun pattern, and the disclosure/pseudonymisation boundary. It contains no generated examples or observed values.

## Stage 2: Specification And Audit

Parse a supplied specification once through `epi_eda_spec()`. Invalid content returns `blocked` at `intake`; a missing/non-local path is an argument error before output creation. Write valid normalized content to `spec_reviewed.csv`. Scaffold evidence yields state `reviewed` only when every row is exactly reviewed and yields `review_required` otherwise; a specification without scaffold evidence yields state `caller_asserted` and a warning.

Run `epi_eda_prepare(data, spec, mode = "audit")` using the existing conservative defaults `unexpected_levels = "error"` and `extra_variables = "keep"`. Write `schema_before.csv` and `preparation_audit.csv`; turn every audit warning/blocker into an ordered value-free workflow message.

Any audit blocker returns `blocked` at `audit`. A blocker-free `prepare = "audit"` returns `audit_complete` at `audit`. A blocker-free `prepare = "none"` proceeds only when no audit row is `planned` and no positive `n_changed` is reported; otherwise it returns `blocked` and recommends explicit apply. Audit warnings alone do not become transformations.

## Stage 3: Approved Preparation

For `prepare = "apply"`, call `epi_eda_prepare()` again in all-or-nothing apply mode. A blocked or erroneous application returns `blocked` without downstream summaries. Success uses prepared data only in memory, replaces the audit artifact with the applied audit, writes `schema_after.csv`, and advances to `preparation`.

The run object deliberately does not return prepared row-level data. Callers requiring the prepared dataset use `epi_eda_prepare()` directly and decide separately how to protect or store it. Source row order/count and caller inputs remain unchanged.

## Stage 4: Canonical And Stratified Summaries

Identify only specification roles exactly `id` or `identifier` after trim/lowercase. Remove those columns from a private row-preserving profiling view. Call canonical missingness and summary APIs on that view with the full specification, then annotate identifier rows as policy-skipped in the returned/exported summary. Identifier missingness is unavailable rather than derived from its values. No observed value sourced from an identifier column enters a type-specific component or report; caller-authored specification metadata remain an explicitly classified exception.

Write `missing.csv` and all six `summary_<component>.csv` files with stable zero-row schemas. Before advancing, reconcile the exact six-component contract; membership of every numeric, categorical, text and temporal table against the `variables` status/type rows; per-variable row/missing/observed counts; canonical missingness; categorical totals; and finite/infinite numeric counts. A missing, extra or misclassified component member prevents completion.

When `strata` is supplied, block an explicitly declared identifier-role stratifier before grouped calculation. Otherwise call `epi_eda_profile_stratified()`, then reconcile input/included/group counts; every group-specific variable denominator; exact Overall variable, numeric, text, temporal and non-missing categorical fields; numeric/categorical denominators; and the explicit Overall categorical missing row/proportion against canonical results. Invalid strata or reconciliation failures preserve completed canonical artifacts and return `blocked` at `canonical_summary`.

After successful reconciliation, write all eight stratified component CSVs and advance to `stratified_summary`, then call `epi_eda_table1()`. A Table 1 error returns `blocked` at `stratified_summary`, retains the reconciled stratified return object/files and leaves `table1`/`table1.csv` absent. Successful Table 1 creation writes `table1.csv`. Passing a stratifier never changes the canonical summary.

No output adds p-values, automatic suppression or a disclosure-safe claim. Text remains aggregate-only and small categorical/group cells remain subject to human disclosure review.

## Report Contract

Before any requested report is generated, final status/stage, specification provenance and `finished_at_utc` are written to the staged `intake_metadata.csv`, followed by current messages. The implementation then creates `report.html` directly with base R. It reads these saved CSVs as its sole tabular source, HTML-escapes headings and every cell, and adds relative links, a prominent complete/incomplete banner, status/stage, privacy/disclosure language and the available stage tables. It performs no statistical calculation and has no `rmarkdown`, Quarto, template or network dependency.

Reports may be created for `review_required`, `blocked` and `audit_complete` bundles as clearly incomplete/status views; missing downstream tables are not presented as empty results. If a report fails at a pre-existing review/audit/blocked gate, that underlying status is preserved and the report blocker is added. If it fails after otherwise complete analysis, status changes to `blocked`. In both cases completed machine artifacts remain, report is `created = FALSE`/path `NA`, any partial report is removed, updated status/messages/timestamp are saved, and the manifest report row remains `not_created`. With `render = FALSE`, a reconciled analysis can still return `complete` and the report remains explicitly not requested.

## Artifact Registry And Manifest

The fixed registry is:

```text
manifest.csv
intake_metadata.csv
messages.csv
spec_scaffold.csv
review_guide.md
spec_reviewed.csv
schema_before.csv
schema_after.csv
preparation_audit.csv
missing.csv
summary_variables.csv
summary_numeric.csv
summary_categorical.csv
summary_text.csv
summary_temporal.csv
summary_skipped.csv
stratified_groups.csv
stratified_variables.csv
stratified_numeric.csv
stratified_categorical.csv
stratified_text.csv
stratified_temporal.csv
stratified_skipped.csv
stratified_metadata.csv
table1.csv
report.html
```

`manifest` and `manifest.csv` have exactly `artifact`, `type`, `path`, `status`, `sensitivity`, `checksum_md5`. Registry order is stable. Status is only `created` or `not_created`; a created row corresponds to an owned regular file and a not-created row corresponds to absence. Sensitivity is `internal_review`, `specification_review` or `disclosure_review` according to artifact type. Paths are portable relative basenames. Every created artifact except the self-referential manifest has an MD5 checksum; the manifest and absent artifacts use an empty checksum.

The manifest exists in a sibling staging bundle from initialization and is refreshed after each artifact write, so expected gates and caught later failures truthfully describe the files to publish.

## Filesystem And Overwrite Safety

An absent output directory is created and normalized. A symlink target, non-directory target or unsafe argument is rejected. With `overwrite = FALSE`, any non-empty target is refused without changes.

With `overwrite = TRUE`, an empty directory is accepted. A non-empty directory is accepted only when it contains a valid prior intake `manifest.csv` with the exact current manifest schema, artifact order and registered paths. Its directory entries must equal exactly the manifest rows marked `created`; every created non-manifest artifact must have a non-empty recorded MD5 that equals its current independently computed checksum. Unowned/missing/impostor files, checksum changes, directories, symlinks and other non-regular/special entries cause refusal before staging or target mutation.

All new artifacts are assembled under a unique sibling staging directory, never inside the live target. CSV/text writes within staging use unique temporary files followed by per-file replacement/rename and manifest refresh. On successful finalization, an existing validated target is renamed to a sibling backup, the complete staging directory is renamed into the exact target, and the backup is removed. If the staging-to-target rename fails, the prior backup is restored where possible and the operation errors. Unpublished staging directories are cleaned on exit. This provides a recoverable directory-swap boundary without claiming recovery from an external failure that also prevents the backup rename.

## Provenance, Privacy And Reproducibility

The input metadata records dimensions, ordered names/classes, caller-supplied source ID, versions, options and UTC timestamps without values. CSV specification provenance records only a basename and normalized-content SHA-256 fingerprint. No source identifier is inferred from local paths or environment state; absolute source IDs and network specification URLs are rejected.

The bundle and return object contain no source/prepared rows, row previews, raw free-text examples, observed identifier-role values, pseudonymisation bridge tables, secrets, telemetry or upload. Caller-authored specification fields, category/group labels, column names, source ID and aggregate/small-cell output can still be sensitive and are explicitly classified for review. The workflow neither detects PII nor claims de-identification, pseudonymisation, disclosure control or publication safety.

Identical data/spec/options produce identical statistical values, component ordering, stable statuses and value-free message ordering; timestamps and filesystem checksums are operational metadata, so byte-identical bundles are not promised. The workflow uses no randomness, remote service or Codecov credential.

## Edge Cases And Compatibility

- Zero rows and zero columns preserve stable component/scaffold schemas and never invent observations.
- Duplicate, empty or reserved `.dataset.` names error before bundle writes; non-syntactic/Unicode names remain exact.
- Missing required variables, unreviewed scaffold rows, unsafe levels, unsupported storage and ambiguous local datetimes follow preparation contracts.
- Explicit identifier roles are policy-skipped; they are never inferred from names or uniqueness.
- Invalid requested strata preserve canonical artifacts and block only the grouped stage.
- Literal `"NA"`, blank text, infinity, declared empty groups and zero denominators follow component contracts.
- Input data/specification objects remain identical, including reference-capable data-frame subclasses.

The API is additive. It does not change `epi_eda_run()`, `epi_eda_render_report()`, `epi_read()` or existing statistical contracts. Use base R, existing `openssl` support for SHA-256, and existing package helpers; add no report engine, JSON dependency, workflow framework, tag or release.

## Recovery

Implementation remains isolated on `feature/data-intake-to-report-workflow`. If component reconciliation, value-free error handling, prior-bundle validation or staged publication cannot satisfy this contract, stop and record the conflict in `review.md` rather than duplicating statistics, weakening a review gate or broadening deletion authority. No Codecov credential, history rewrite, tag or release is authorised.
