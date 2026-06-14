# ADR 0004: Support synthetic data before access

## Status

Proposed.

## Context

Biomedical data access is often delayed by governance, permissions, data-use agreements or provider processes.

During this period, teams can still prepare project infrastructure if they have variable names, expected types or a data dictionary.

Synthetic data generated from the specification can support code development, report design and pipeline testing before real data arrive.

## Decision

Add `generate_synthetic_data()` as a first-class part of the specification-first workflow.

Synthetic data should be generated from the same EDA specification used to validate real data.

## Consequences

Positive:

- enables earlier development;
- allows report templates to be tested;
- allows derived-variable code to be prepared;
- supports reproducible project setup;
- reduces idle time while waiting for data access.

Negative:

- synthetic data may create false confidence if treated as realistic;
- users may confuse pipeline-testing data with inferential data;
- more documentation is needed.

## Safeguards

Synthetic data outputs should be clearly labelled.

Reports generated from synthetic data should state that:

- data are synthetic;
- data are for pipeline preparation only;
- no substantive or inferential interpretation is valid.

## Implementation notes

MVP synthetic data should be simple and deterministic.

Future versions may support:

- marginal distributions;
- correlations;
- site-level clustering;
- calendar effects;
- outcome prevalence;
- longitudinal rows per person;
- missing-data mechanisms;
- plausibility constraints.
