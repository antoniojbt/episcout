# Review Notes

Spec ID: `033-categorical-denominator-presentation`
Status: Draft

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
