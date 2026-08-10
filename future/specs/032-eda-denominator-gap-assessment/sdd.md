# Gap Assessment And Successor Design

Spec ID: `032-eda-denominator-gap-assessment`
Status: Review

## Evidence Matrix

| Area | Current evidence | Assessment | Disposition |
| --- | --- | --- | --- |
| Canonical categorical summaries | `n`, `n_total`, `n_observed`, `p_total` and `p_observed` are stable machine-readable fields. | Calculation foundation complete. | Preserve exact schema. |
| Stratified population accounting | `groups$n` and metadata `n_input`, `n_included` and `n_omitted_missing_stratum` reconcile the analysis population; categorical rows retain total and observed denominators. | Group and Overall calculation foundation complete. | Preserve and consume rather than recalculate. |
| Table 1 | Each row exposes `group_n`, `denominator` and a count-plus-percentage display. Ordinary levels use observed values while missing rows use group totals. | Current default is traceable but its mixed basis is selected internally; row/column/overall alternatives and a reusable numeric proportion field are absent. | Successor #253 adds an opt-in shared basis calculation while preserving the default display. |
| Frequency plots | Compact categorical plot data contain `level`, `count`, order and remainder state; inventory separately records total, missing and plotted counts. | Counts are bounded and reproducible, but denominator/proportion/basis are not carried in the same companion rows. | Successor #253 enriches the aggregate companion from existing counts only. |
| Data-frame/intake reports | Canonical reports explain `p_total`/`p_observed`; intake reports show stratified tables and Table 1. | Values are present but no single display contract reconciles Table 1 and frequency companions. | Consume the shared successor output and state its basis. |
| PostgreSQL delivery report | The #245 renderer presents validated aggregate summary tables and plot-data artifacts after the snapshot closes. | No new query or observation collection is necessary. | Derive presentation fields after collection and keep them manifest-owned. |
| Missing/not-applicable semantics | Specifications declare `missing_codes`; current summaries treat them with ordinary missing values. | The package cannot and should not infer a separate not-applicable meaning. | Report declared-missing treatment; any finer semantic distinction remains explicit caller work outside this slice. |

## Decision

Create one implementation successor, issue #253, rather than separate calculation, Table 1, chart and report issues. A single shared aggregate calculation is the smallest architecture that can guarantee cross-output reconciliation; splitting its consumers before the common fields exist would create temporary duplicate formulas.

The successor must derive from existing canonical or stratified components and return stable rows containing at least variable/level/group identity, numerator, denominator, numeric proportion, percentage basis, group/population size and missing-level state. Column, row and overall bases must have explicit zero-denominator behaviour. The compatibility default must reproduce current Table 1 output.

## Consumer Boundaries

- Table 1 remains presentation-only and must not derive a second percentage.
- Frequency-plot companion data may expose percentage fields without forcing labels onto every bar.
- Reports state the selected basis and show/refer to the same aggregate rows.
- PostgreSQL uses no additional SQL, snapshot work or row collection.
- Canonical summary schemas and the default flat PostgreSQL bundle remain unchanged.

## Sequence

1. Merge and close out #245/PR #252.
2. Merge this assessment and close #248, leaving #253 as the ready-next package tracker.
3. Write and accept a separate implementation specification for #253 before package changes.
4. Verify calculation fixtures first, then Table 1, plot companion, report, PostgreSQL no-new-query, manifest and compatibility behaviour.
