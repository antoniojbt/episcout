# Review Notes

Spec ID: `033-categorical-denominator-presentation`
Status: Completed

## Planning Findings

- Existing canonical and stratified calculations already contain every required count and population total; the new function must normalise and validate those aggregates rather than create another statistics path.
- Compatibility is deliberately a mixed denominator rule: ordinary levels use observed values and the missing level uses all group rows. Naming this as a separate basis avoids mislabelling it as a conventional column percentage.
- Row percentages require partition groups. Overall must be excluded from their denominator to avoid double-counting, while retaining the Overall display cell makes the calculation traceable.
- Count plots need no visual redesign. Enriched companion rows provide legible percentage evidence without mandatory annotations.
- PostgreSQL already supplies categorical counts to plot preparation. Enrichment therefore requires no query, row collection or report connection.
- Valid `compact-plot-data-1` bundles are a demonstrated renderer input and need an in-memory compatibility path.

## Open Questions

None. Issue #253 and completed spec 032 settle the bounded outcome; this specification makes every material percentage and compatibility choice explicit.

## Planning Verification

- Canonical `master` and GitHub workflow state match at `26fabdf`.
- Focused canonical summary, stratified, Table 1 and PostgreSQL-source tests pass before specification edits.
- Source, callers, report templates, bundle registries and exact compatibility assertions were inspected directly.
- No package source, tests, generated documentation, dependencies or runtime behaviour change in the planning contribution.
- Draft planning PR #257 is the package-implementation activation gate.

## Activation Handoff

- Planning PR #257 passed PostgreSQL integration, Ubuntu, macOS, coverage, CodeFactor and both Codecov gates at `362a94d` with no actionable review feedback.
- Implementation completed on `feature/categorical-denominator-contract`, stacked on that exact planning head under the owner's instruction.

## Implementation Self-Review

- The public calculation uses only canonical or stratified aggregate components and validates group partitions, count identities, group metadata and the common categorical level universe before calculating.
- Literal seven-row expectations independently establish compatibility, column, row and overall cells, including the combined missing level and zero-denominator `NA_real_` result.
- The default Table 1 CSV is byte-identical to the accepted planning head (`0c00543ecf2ea13a4b23604a77e1b97a` for both files); count-plot order and heights remain unchanged.
- A live UTF-8 PostgreSQL 18 run passes the parity and database-report suite. Categorical companion enrichment is also guarded by a no-fetch unit test, and the renderer validates both new and legacy companions from canonical aggregates.
- Package lint, `git diff --check` and `scripts/check-local.sh` pass; the latter reports 0 errors, 0 warnings and 0 notes. `scripts/check-cran.sh` completes with the inherited incoming-feasibility NOTE for new-submission/vignette-index and two Stack Overflow 403 checks.
- The exact rendered categorical CSV reconciles to `1/5`, `2/5`, `1/5`, `0/5`, `1/5` and missing `2/7`; the HTML plain rendering contains those fields, and the 1280 x 800 count plot was visually inspected for its `2, 1, 1, 1, 0` heights and labels.

No actionable self-review finding remains.

## Closeout

Planning PR #257 merged first as `49bf7c4` on 2026-08-10. Implementation PR #258 then merged as `074f13a`, and GitHub closed issue #253 automatically. At final implementation head `d4f03d3`, PostgreSQL integration, Ubuntu, macOS, test coverage, CodeFactor and both Codecov gates passed; Codecov reported 95.00% patch coverage and 93.20% project coverage. Local and fork `master` were fast-forwarded to canonical `074f13a` before this closeout branch was created.

No successor is created: issue #253 completes the bounded categorical-denominator presentation slice, while roadmap #249 retains only separately tracked or deferred work.
