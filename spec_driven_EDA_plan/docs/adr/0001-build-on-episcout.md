# ADR 0001: Build on episcout rather than rewriting

## Status

Proposed.

## Context

`episcout` already contains useful epidemiology helper functions for cleaning, descriptive statistics, plotting and utilities.

The desired future direction is a specification-first EDA workflow for biomedical datasets. This could be developed as a new package or as a new layer inside `episcout`.

## Decision

Build on `episcout`.

Keep existing `epi_*` functions initially. Add a higher-level specification-first workflow layer with a small public API.

## Consequences

Positive:

- reuses existing code;
- preserves package history;
- avoids a rewrite;
- keeps domain-specific helpers available;
- allows incremental PRs.

Negative:

- current public API is broad;
- some existing functions need hardening;
- architecture may need gradual cleanup.

## Implementation notes

Add new files for the new layer:

```text
R/eda_spec.R
R/eda_schema.R
R/eda_synthetic.R
R/eda_missing.R
R/eda_summaries.R
R/eda_plots.R
R/eda_report.R
R/run_eda.R
```

Do not remove existing exported functions during the MVP.
