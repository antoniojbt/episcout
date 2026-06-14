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
eda_spec()
validate_eda_spec()
generate_synthetic_data()
check_schema()
profile_missing()
profile_summaries()
profile_plots()
run_eda()
render_eda_report()
```

The package should also allow a dataset-only convenience path later, where a draft specification can be inferred from real data.
