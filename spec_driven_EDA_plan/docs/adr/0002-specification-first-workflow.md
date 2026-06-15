# ADR 0002: Use a specification-first workflow

## Status

Proposed.

## Context

Many biomedical projects begin with variable names, a data dictionary or an analysis plan before real data are available.

Other projects already have data but still need structured validation, missingness summaries, descriptive statistics, plots and reports.

A dataset-first EDA package is useful only after real data exist. A specification-first workflow can serve both states.

## Decision

Make the EDA specification the central project object.

The specification should drive:

- synthetic data generation;
- schema checks;
- missing-value standardisation;
- descriptive summaries;
- plot dispatch;
- reporting.

## Consequences

Positive:

- supports work before data access;
- provides a stable contract for expected data;
- allows repeatable checks when real data arrive;
- makes reports more auditable;
- improves project setup consistency.

Negative:

- requires users to maintain a data dictionary;
- requires validation code for the specification itself;
- may feel heavier for very small ad hoc datasets.

## Implementation notes

Initial functions:

```r
epi_eda_spec()
epi_eda_validate_spec()
epi_eda_generate_synthetic_data()
epi_eda_check_schema()
epi_eda_profile_missing()
epi_eda_profile_summaries()
epi_eda_profile_plots()
epi_eda_run()
epi_eda_render_report()
```

The package should also allow a dataset-only convenience path later, where a draft specification can be inferred from real data.
