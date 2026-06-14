# ADR 0005: Use external real-data fixtures for TDD

## Status

Proposed.

## Context

The new specification-first EDA layer should be reliable, auditable and useful for biomedical datasets.

Existing tests in `episcout` cover many lower-level helper functions, but they do not yet provide end-to-end evidence that the new workflow works with:

- real biomedical data;
- a data dictionary;
- expected schema outputs;
- expected missingness outputs;
- expected summary outputs;
- synthetic data generated from the same specification.

There is a risk that Codex or another coding agent could implement the new API first and then create tests that merely reproduce the implementation.

## Decision

Develop the new spec-first EDA layer using external real-data fixtures and test-driven development.

For every new function where practical:

```text
fixture data + fixture specification + independently computed expected output
```

should be added before implementation.

The first fixture should use `medicaldata::blood_storage`.

## Consequences

Positive:

- tests are grounded in real biomedical data;
- expected outputs are externally verifiable;
- tests are less circular;
- Codex has clearer success criteria;
- implementation behaviour is constrained before code is written.

Negative:

- setup takes longer before the first implementation PR;
- fixture files require provenance documentation;
- data dictionaries need manual review;
- expected outputs must be maintained when fixtures change.

## Implementation notes

Routine tests should not require internet access.

Small CSV fixtures should be committed under:

```text
tests/testthat/fixtures/
```

A regeneration script should live under:

```text
data-raw/test-fixtures/make_external_fixtures.R
```

Expected outputs should be computed using simple independent code, not with the functions being tested.

## Non-goals

This ADR does not require all tests to use external data.

Synthetic-data tests are still useful, but they are not sufficient as the only tests for the workflow.

This ADR does not require use of large external datasets in routine unit tests.
