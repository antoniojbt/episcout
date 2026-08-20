# Verification Design

Spec ID: `054-transversal-association-diagnostics-audit`
Status: Active

## Audit Evidence

Issue 358 changes planning records only. Verification therefore establishes that the proposed boundary is supported by current source and independent calculations; it does not add executable package tests.

- Map every current Oferta output field to an existing Episcout field or a named successor gap.
- Compare a synthetic PostgreSQL cross-tab calculated directly in SQL with `epi_eda_profile_stratified()` using `include_overall = FALSE` and `include_missing_stratum = FALSE`.
- Run the existing PostgreSQL stratified parity tests to confirm data-frame/PostgreSQL agreement, declared zero rows, missing groups and rollback behaviour remain current.
- Check a tied-rank Spearman example independently with explicit average ranks, base R and the Oferta PostgreSQL expression.
- Check hand-derived 2×2 and 3×2 Cramér's V tables and confirm that adding a zero-total declared column does not change the result when inactive margins are removed.
- Inspect existing correlation, contingency and heatmap tests to distinguish structural coverage from independently anchored statistical truth.

## Required Successor Tests

### Numeric association

- Exact positive, negative and non-perfect tied-rank examples with hand-authored expected ranks and coefficients.
- Pairwise standard missingness, declared missing codes, `NaN`, positive infinity and negative infinity with exact eligible counts.
- Empty pair list, fewer than two eligible rows, constant left, constant right and both constant with typed unavailable reasons.
- Input validation for absent, identifier, unsupported, self, duplicate and unordered-duplicate pairs.
- Data-frame/PostgreSQL parity for values, counts, schemas, order and reasons.
- PostgreSQL query instrumentation proving one read-only repeatable-read snapshot, aggregate-only return, no analysis-vector query and rollback without a partial object.

### Stratified bound and aggregate Cramér's V

- Existing stratified defaults and all current Table 1/report consumers remain unchanged when the new bound is omitted.
- Exact-at-bound success and one-over-bound refusal for stratum, compared observed domain and declared-plus-unexpected union on both backends.
- Hand-derived 2×2 table: `n = 100`, Pearson chi-square `34.02777777777778`, `V = 0.5833333333333334`.
- Hand-derived 3×2 table: `n = 70`, Pearson chi-square `15.55555555555556`, `V = 0.4714045207910317`.
- Declared zero row/column produces the same coefficient after inactive-margin removal.
- Zero-total and one-active-row/column tables return typed unavailable evidence.
- Negative, fractional, missing, unsafe and duplicated cell counts fail before calculation.
- Permuting rows or level labels does not change the coefficient, while output order follows explicit pair order.

### Regression and delivery

- `epi_stats_corr()` and historical contingency helpers retain their released outputs and documentation.
- `epi_eda_db_run()`, Table 1, longitudinal profilers and default database bundles remain unchanged.
- No raw rows, identifiers, SQL text, connections or credentials enter returned or report artefacts.
- Focused tests run before `scripts/check-local.sh`; `scripts/check-cran.sh` and live PostgreSQL checks are required because the successor changes statistical and database contracts.
- Coverage is inspected for changed branches without substituting a blanket target for exact assertions.
