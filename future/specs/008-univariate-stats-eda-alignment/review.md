# Review Notes

Spec ID: `008-univariate-stats-eda-alignment`  
Status: Implemented  

## Baseline Findings

- Full tests passed before package-code changes with the two known skips.
- Package check passed with 0 errors, 0 warnings and the existing `.gitkeep` NOTE.
- The test harness can remove skipped vdiffr snapshots; unchanged snapshots must be restored after full verification.

## Findings

- Shared internal cores now provide one missingness/count policy and numeric, categorical, text and temporal calculations to EDA and the scoped public wrappers.
- EDA v1 remains the unchanged default; explicit v2 runs cover every specification type, return six components, write six summary CSVs and render populated sections.
- Typed `epi_stats_summary()` is additive and its current output mode remains compatible.
- Confirmed public edge defects are corrected: finite-only numeric calculations, stable zero-row character/factor rows, retained unused factor levels, distinct literal and missing `NA`, and stable empty/all-missing/POSIX temporal summaries.
- Real blood-storage and penguins fixtures demonstrate complete variable coverage, including penguins text and date variables.
- A lint-driven native-pipe conversion was rejected because it would raise the package minimum R version; historical `%>%` use remains deliberately unchanged.

## Open Questions

None blocking implementation.

## Closeout Notes

Spec 008 is implemented and verified. The default transition from EDA v1 to v2 remains a later compatibility decision. Correlation, contingency, outcome and presentation redesigns remain outside this spec.
