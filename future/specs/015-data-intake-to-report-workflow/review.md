# Review Notes

Spec ID: `015-data-intake-to-report-workflow`
Status: Completed

## Planning Findings

- The existing `epi_eda_run()` and `epi_eda_render_report()` are direct pre-preparation conveniences. Reusing them inside the new orchestrator would add plot work, bypass stage gates and inherit direct-write behaviour, so the workflow should compose the lower-level public profile functions instead.
- Spec 012's scaffold contains aggregate evidence and always marks every row `review_required`; successful generation is not approval. Spec 013 already supplies the authoritative reviewed-scaffold, all-or-nothing transformation and value-free audit gates.
- Spec 013 audit mode may contain both `blocking` and `planned` actions while its overall status remains `audited`. The orchestrator must inspect audit action statuses: blockers always stop, and `prepare = "none"` must also stop when any planned transformation exists.
- Specs without scaffold evidence are historically accepted by public EDA APIs. V1 preserves that additive compatibility as an explicit `caller_asserted` review state and warning instead of inventing an approval field or breaking existing dictionaries.
- The issue's recommended bundle omitted canonical `missing.csv` and the stratified `variables`/`metadata` components even though the report requires missingness and every machine component must be exportable. The fixed registry includes them so the saved bundle is complete and traceable.
- Existing report infrastructure uses `rmarkdown` with a bundled `.qmd` template and in-memory results. V1 instead uses a focused base-R HTML view that reads its saved CSV artifacts, HTML-escapes content and adds no renderer/template dependency or second calculation path.
- Canonical categorical output can contain observed levels and the canonical profiler does not currently apply an identifier-role skip. V1 therefore removes only explicitly declared `id`/`identifier` columns from a private profiling view, calls the full canonical contract, annotates those absent results with a policy reason and blocks identifier-role stratification. It does not infer identifiers or duplicate statistical formulae.
- In-place whole-directory clearing is unnecessary and unsafe. V1 accepts a non-empty overwrite target only when its manifest-created paths equal the exact entries and every created non-manifest checksum still matches; missing, modified, unknown and special entries are refused before a sibling staging bundle is created.

## Semantic Decisions

- `review_required`, `blocked`, `audit_complete` and `complete` are the complete v1 status set; `intake`, `audit`, `preparation`, `canonical_summary` and `stratified_summary` identify the last successful data-processing work while report state remains separate.
- Top-level ambiguity or an unsafe target errors before mutation. After safe initialization, expected specification/data/render blockers return a stable object with structured value-free messages.
- Default preparation policies are spec 013's `unexpected_levels = "error"` and `extra_variables = "keep"`; the new public API does not widen them without a separate semantic review.
- `prepare = "none"` means audited and already compatible, not permissive. Any planned action blocks and recommends explicit apply.
- Scaffold evidence is not binary by shape alone: spec state is `reviewed` only when every row is exactly reviewed and remains `review_required` otherwise.
- The run object intentionally omits prepared row-level data even after apply. Analysts who need it use `epi_eda_prepare()` separately and control its storage explicitly.
- Canonical profiles are always saved even when stratification later blocks. Their completed state and every absent stratified artifact remain explicit.
- Status/audit reports may be rendered at expected gates, but they contain only completed saved artifacts and an unmistakable incomplete banner. A report is never evidence that analysis completed.
- `manifest.csv` uses the fixed `artifact`, `type`, `path`, `status`, `sensitivity`, `checksum_md5` schema. Status is only `created`/`not_created`; the manifest has an empty self-checksum and every other created artifact has MD5 evidence.
- Output artifacts and manifest are assembled in a sibling staging directory. Publication renames a validated prior target to a sibling backup, renames staging into place, restores backup where possible if finalization fails, then removes backup after success.
- Canonical reconciliation includes exact type/status membership. Stratified reconciliation checks group-variable counts, exact Overall component fields, denominators and categorical missing rows before presentation; Table 1 failure retains those reconciled machine components at a blocked `stratified_summary` stage.
- Final status/stage/spec provenance/timestamp are written before report generation. Report failure preserves an underlying review/audit/blocked gate and changes only an otherwise complete run to blocked.
- Summary and Table 1 outputs remain subject to disclosure review; no row-level output does not imply de-identification or publication safety.
- A supplied specification and `source_id` are caller-controlled metadata and may themselves be sensitive. The workflow classifies and warns about them; it cannot truthfully guarantee that user-authored metadata contain no identifier-like text.

## Checklist Routing

Software verification applies to the public API, stage machine, stable schemas, filesystem boundaries, error recovery, non-mutation, deterministic order and full regression gates. Truth and semantics applies to approval, missingness, transformations, denominators, omissions, provenance and claims about completion/privacy. Analysis and statistics applies to canonical/stratified reconciliation and independently hand-derived expectations; the orchestrator adds no statistical method. Copy-edit applies to the review guide, walkthrough, status banners, artifact labels and disclosure warnings. Render-and-release applies when the built-in HTML output is exercised and inspected; no release operation is authorised.

## Baseline Evidence

The stacked parent branch for completed spec 014 passed its focused component tests, package-loaded lint and full local check before this workflow implementation. Its CRAN-oriented check retained only the inherited external incoming NOTE. This established the component baseline; the new orchestration semantics were verified independently below.

## Open Questions

None currently. Stop for owner review if implementation evidence requires a broader overwrite policy, prepared-row return/export, automatic specification approval, a new conversion/statistical rule, or any privacy claim beyond the contract.

## Implementation Review

- The workflow composes the existing scaffold, validation, preparation, canonical, stratified and Table 1 contracts. Shared identifier exclusions were extracted for canonical/grouped parity; no statistical formula was duplicated.
- Independent read-only review found and verified fixes for regular-file specification validation, rooted Windows/UNC source identifiers, duplicate and missing canonical/grouped component membership, all-group denominators and categorical proportions, and the distinction between recoverable reconciliation blockers and internal filesystem errors.
- Output publication uses a sibling staging directory, exact registry/type/sensitivity/status/checksum preflight, a target-to-backup then staging-to-target swap, and a tested backup restoration path.
- The return object and artifacts omit source/prepared rows and observed identifier-role values. Reports read only saved CSVs, escape content, use relative links and retain explicit incomplete/disclosure warnings.
- Final independent re-audit reported no unresolved correctness, filesystem, privacy or semantic blocker.

## Verification Evidence

- Focused intake, scaffold, preparation, canonical, stratified and Table 1 tests passed, including hand-derived numeric results, identifier/free-text markers, duplicate/missing subgroup components, subgroup denominators, manifest ownership, failed staging/final swap recovery, report failure and escaped portable HTML.
- Package-loaded lint completed with no findings, and `git diff --check` passed.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes; its two known environment skips were unchanged.
- `scripts/check-cran.sh` completed with two external-environment NOTEs: the inherited new-submission/no-prebuilt-vignette-index and two existing Stack Overflow documentation URLs returning HTTP 403, plus an inability to verify the current time during the final run. An immediately preceding run had only the inherited incoming NOTE.
- Check-generated unrelated Rd changes and skipped plotting-snapshot deletions were restored before closeout.

## Closeout Notes

The stage-gated intake-to-report contract, documentation, recovery paths and executable synthetic workflow are complete. No Codecov credential, network service, tag or release was required or created.
