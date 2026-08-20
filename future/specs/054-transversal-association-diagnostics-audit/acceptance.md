# Acceptance

Spec ID: `054-transversal-association-diagnostics-audit`
Status: Active

- [x] Current lower-level statistics, canonical summaries, PostgreSQL queries, stratified EDA and plotting helpers were audited before proposing new code.
- [x] Oferta's Spearman calculation is classified as a small reusable gap without changing released `epi_stats_corr()` behaviour.
- [x] Oferta's cross-tabs are mapped directly to the existing stratified categorical schema; only a fail-not-truncate bound remains as a reusable safety gap.
- [x] Cramér's V is absent from Episcout and is specified as one aggregate-count descriptive definition rather than an inferential contingency rewrite.
- [x] Missingness, heatmap and association-ranking plots are separated from the generic calculation boundary and remain downstream.
- [x] The literal `PLZOCU=0` location grouping remains in Oferta.
- [x] The proposed successor is narrower than Oferta's helper and contains no SIAP semantics, selection, plotting or report framework.
- [x] Independent tied-rank, raw-SQL cross-tab and 2×2/N×M contingency calculations support the design decisions.
- [x] Required successor tests cover truth, parity, missingness, non-finite values, degenerate cases, bounds, privacy, snapshot ownership and compatibility.
- [x] Issue 358 changes no package behaviour, source, tests, dependencies, generated documentation, output bundle or downstream project.
- [ ] The design PR passes required checks and receives no unresolved actionable review feedback.
- [ ] The repository owner accepts the boundary and one separately blocked implementation successor is created.
- [ ] The design PR is merged, lifecycle records are reconciled and issue 358 is closed without starting implementation.
