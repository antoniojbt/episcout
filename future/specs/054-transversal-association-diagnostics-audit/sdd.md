# Gap Assessment And Successor Design

Spec ID: `054-transversal-association-diagnostics-audit`
Status: Review

## Evidence Basis

The audit inspected current Episcout implementations, callers, tests and documentation together with Oferta's active `transversal_descriptive_aggregates.R`, older `semantic_eda.R` equivalent and synthetic integration assertions. Existing code and tests establish current behaviour but are not treated as independent statistical truth.

Independent definitions and calculations use:

- R's documented definition of Spearman correlation as Pearson correlation of ranks, with pairwise reranking when pairwise-complete observations are selected: <https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html>;
- PostgreSQL's definitions of `rank()` peer groups and `corr()` eligibility over rows with both inputs non-null: <https://www.postgresql.org/docs/18/functions-window.html> and <https://www.postgresql.org/docs/current/functions-aggregate.html>;
- NIST's definition of Cramér's coefficient from Pearson chi-square, sample size and the smaller active table dimension: <https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/cramcont.htm>;
- hand-derived tied-rank and contingency-table fixtures recorded in `review.md`.

## Evidence Matrix

| Oferta mechanic | Existing Episcout capability | Demonstrated gap | Disposition | Keep downstream? |
| --- | --- | --- | --- | --- |
| Pairwise Spearman | `epi_stats_corr()` wraps `Hmisc::rcorr()` for an in-memory full matrix and exposes its coefficient, pair-count and p-value matrices. | No PostgreSQL backend, explicit pair contract, canonical finite-value rule, deterministic availability reason or descriptive-only output. Changing the released helper would mix old and new contracts. | Small reusable gap: add a separate EDA-level explicit-pair profiler and preserve `epi_stats_corr()`. | Variable and pair selection remains downstream. |
| Categorical cross-tab | `epi_eda_profile_stratified()` already returns group/level counts, total and observed denominators, proportions, missing levels, declared zero levels and unexpected levels on data-frame and PostgreSQL sources. `include_overall = FALSE` and `include_missing_stratum = FALSE` match Oferta's grouping denominator. | The public profiler has no finite categorical-domain bound; PostgreSQL currently calls categorical aggregation with `max_levels = Inf`. | Reuse the current profiler and add one optional finite fail-not-truncate bound in the successor. Do not add another cross-tab API. | Caller-selected stratum, variables and labels remain downstream. |
| Cramér's V | No package implementation exists. The historical contingency helpers consume source rows, reshape output and optionally perform inferential tests. | No descriptive coefficient from already aggregated counts, no explicit degenerate-state evidence and no reconciliation with declared zero-count levels. | Small reusable gap: one canonical aggregate-count primitive, consumed after stratified EDA. | Selection, ranking and interpretation remain downstream. |
| Missingness overview | Canonical EDA already returns explicit missing counts and proportions; ordinary `ggplot2` construction is short. | No calculation gap. Top-N selection, Spanish wording and AEIS theme are project choices. | Use canonical missingness data; no new plot API. | Yes. |
| Correlation heatmap | `epi_plot_heatmap()` can plot long correlation data; Oferta's code mainly expands symmetric cells, rewrites labels and applies AEIS styling. | The existing plot helper does not remove the downstream scientific/presentation decisions. | No new API. Oferta may reuse the helper only if that simplifies its own code without changing the accepted figure. | Yes. |
| Association ranking plot | Episcout provides generic bar/plot foundations and EDA style callbacks. | Top-N selection, label ordering and highlighted AEIS presentation are the substantive behaviour. | No new API. | Yes. |
| `PLZOCU=0` location grouping | None required. | Literal state, fields and grouping are SIAP-specific. | No abstraction. | Yes. |

## Cross-tab Reuse Decision

Oferta's reusable cross-tab fields map directly to the existing stratified categorical component:

| Oferta field | Existing stratified field |
| --- | --- |
| grouping code | `group_value` |
| compared variable | `name` |
| compared level | `level` |
| missing level | `is_missing_level` |
| cell count | `n` |
| grouping denominator | `n_total` |
| cell proportion | `p_total` |

Passing a specification restricted to the explicit grouping and compared variables avoids automatic scientific selection. Canonical declared zero-count and missing-code rows are retained even though the current Oferta SQL emits only observed SQL `NULL` and ordinary levels. Downstream migration must accept the canonical complete table rather than discard those rows merely to reproduce the older sparse shape.

The successor may add `max_levels` to `epi_eda_profile_stratified()`. Its omitted/default behaviour must preserve the released interface; association callers must supply a finite positive bound. The bound applies independently to the stratum domain, every compared categorical domain and the declared-plus-observed union. Exceeding it fails the complete call before returning a partial object and must limit PostgreSQL collection rather than checking only after unbounded retrieval.

## Numeric Association Contract For The Successor

The successor should add one EDA-level function whose public name is fixed in its own implementation specification. Its contract is:

- input is a data frame or reviewed `epi_eda_postgres_source`, an accepted EDA specification and an ordered two-column character data frame of explicit variable pairs;
- both variables must be declared `numeric` or `integer`, present, distinct within a pair and non-identifier; duplicate unordered pairs are rejected rather than silently deduplicated;
- eligibility is pairwise: exclude standard missing values, declared missing codes and non-finite values in either variable, then rerank each eligible pair independently using average ranks for ties;
- return one row per requested pair in caller order with pair identity, labels, eligible `n`, Spearman `rho`, `status` and `reason`;
- return `status = "unavailable"` and `rho = NA_real_` for fewer than two eligible pairs or zero variance in either rank vector, distinguishing those reasons;
- return no p-value, confidence interval or interpretation field;
- PostgreSQL performs ranking and `corr()` inside one read-only repeatable-read transaction, returns aggregates only and never collects an analysis-value vector;
- data-frame and PostgreSQL output schemas, ordering, counts, coefficients and unavailable reasons are identical, including typed empty output when zero pairs are requested.

This is deliberately separate from `epi_stats_corr()`: the historical helper retains its full-matrix, Hmisc and p-value behaviour without compatibility churn.

## Aggregate Cramér's V Contract For The Successor

The successor should add one canonical function over long-form aggregate contingency counts. It must:

- accept explicit pair identity, row level, column level and non-negative integer cell count fields; require one unambiguous row per cell and reject missing, fractional, negative or unsafe counts;
- retain zero-count cells as evidence but remove zero-total row and column margins before calculating active dimensions;
- compute Pearson expected counts from active margins and calculate `V = sqrt(chi_square / (n * min(r - 1, c - 1)))` without continuity or small-sample bias correction;
- return pair identity, total `n`, active row and column counts, coefficient, `status` and `reason` in deterministic caller order;
- return unavailable evidence for zero total or fewer than two active rows or columns instead of returning an invalid coefficient;
- expose no p-value, significance label, threshold or scientific interpretation;
- accept the existing stratified categorical component without source-row access, so PostgreSQL needs no second association query after the cross-tab is available.

Removing zero-total margins is required because canonical stratified output retains declared zero-count levels. Including those inactive dimensions in `min(r - 1, c - 1)` would make the coefficient depend on unobserved catalogue declarations rather than the observed contingency table.

## Presentation And Ownership Boundary

Episcout owns only validated aggregate calculations and stable machine-readable evidence. Oferta continues to own:

- which pairs, grouping fields and variables are scientifically useful;
- `PLZOCU` and all other SIAP code meaning;
- top-N selection, ordering, Spanish labels and report placement;
- AEIS scales, themes, SVG/PNG publication and report composition;
- the literal `PLZOCU=0` location query and project-specific derived measures.

After an accepted successor is canonical, Oferta may replace duplicated generic calculations in a separate downstream contribution. That migration is not part of issue 358 or its successor.

## Successor Boundary

Create one blocked implementation successor rather than separate correlation, cross-tab and Cramér issues. These changes form one reviewable descriptive-association slice: explicit-pair numeric aggregation, reuse-safe bounded cross-tabs and an aggregate coefficient derived from those cross-tabs. The successor remains narrower than Oferta's helper because it contains no plots, publication, project selections, SIAP semantics or location query.
