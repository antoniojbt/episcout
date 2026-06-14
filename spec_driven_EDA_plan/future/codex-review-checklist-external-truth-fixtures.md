# Review checklist: external truth fixtures for agent-generated EDA code

Use this checklist when reviewing PRs that add or consume EDA fixture data.

## Core rule

The validation chain must be:

```text
external fixture data
+ manually reviewed fixture specification
+ independently computed expected output
-> failing tests
-> implementation
```

Do not accept the inverse pattern:

```text
implementation
-> generated output
-> tests that compare future output to that same implementation
```

That is circular validation.

## Fixture source

- [ ] The fixture comes from a public, documented source.
- [ ] The source is appropriate for the test purpose.
- [ ] The fixture is small enough for routine unit tests.
- [ ] The fixture is pinned as committed files under `tests/testthat/fixtures/`.
- [ ] Routine tests do not download live data.
- [ ] Routine tests do not require internet access.
- [ ] Routine tests do not require optional source packages to be installed.

## Provenance

Each fixture directory should include `SOURCE.md` with:

- [ ] Dataset name.
- [ ] Source package or source institution.
- [ ] Source package version, if applicable.
- [ ] Source documentation URL.
- [ ] Row count.
- [ ] Column count.
- [ ] Dataset type and purpose.
- [ ] Regeneration script path.
- [ ] Statement that expected outputs were not computed with `episcout` functions under test.

## Expected outputs

- [ ] Expected files are committed beside the fixture data.
- [ ] Expected outputs are readable CSV or another simple, reviewable format.
- [ ] Expected outputs were generated before implementation code that consumes them.
- [ ] Expected outputs were computed with base R, simple transparent code, or a clearly independent reference.
- [ ] Expected outputs do not call package functions under test.
- [ ] Numeric summaries specify definitions for `sd`, quantiles and missing-value handling.
- [ ] Categorical summaries specify how missing values are counted.
- [ ] Plot expectations test contract and dispatch before visual appearance.

## Regeneration scripts

- [ ] Regeneration scripts are manual utilities, not routine test dependencies.
- [ ] Scripts fail clearly if optional source packages are unavailable.
- [ ] Scripts do not call `library(episcout)`.
- [ ] Scripts do not call `episcout::`.
- [ ] Scripts do not call EDA implementation functions under test.
- [ ] Scripts use deterministic output ordering.
- [ ] Scripts avoid hidden randomness; any randomness has a fixed seed.

Forbidden calls in fixture-generation scripts:

```text
library(episcout)
episcout::
eda_spec(
validate_eda_spec(
check_schema(
profile_missing(
profile_summaries(
profile_plots(
run_eda(
generate_synthetic_data(
```

## Dataset roles

Use fixtures for distinct purposes:

```text
blood_storage
```

Biomedical fixture. Best for clinical schema, missingness, outcome/exposure roles, survival-style variables and realistic epidemiological data.

```text
penguins_raw
```

Standard public EDA fixture. Best for raw column names, mixed variable types, missingness, grouped numeric summaries, categorical summaries, dates, common plots and report examples.

```text
scurvy
```

Tiny fixture. Best for small-n edge cases and categorical handling.

Do not expect one fixture to cover all behaviour.

## PR review questions

Before merging, answer:

1. Does this PR add external truth, or does it merely preserve current implementation behaviour?
2. Could an agent have generated both the code and expected output from the same implementation path?
3. Are fixture outputs understandable by inspection?
4. Are there at least a few hard-coded sentinel values that can be checked manually?
5. Does the PR avoid broad refactoring?
6. Does the PR avoid changing implementation files when it is meant to add fixtures only?
7. Does the PR preserve the TDD sequence?

## Minimum sentinel checks

For each fixture, include at least:

- [ ] Row count.
- [ ] Column count.
- [ ] Selected missing counts.
- [ ] Selected categorical counts.
- [ ] Selected numeric summaries.
- [ ] At least one unsupported or skipped variable, if plot dispatch is being tested.

For `penguins_raw`, suitable sentinel checks include:

```text
rows: 344
columns: 17
```

Additional sentinel values should be generated from the committed fixture using independent base R and then committed as expected CSV files. Do not compute them with the implementation under test.

## Suggested PR label

```text
testing
fixtures
spec-first-EDA
```
