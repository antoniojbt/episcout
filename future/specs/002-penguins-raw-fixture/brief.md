# Brief

Spec ID: `002-penguins-raw-fixture`  
Status: Ready for activation  

## Problem

The current EDA tests use `blood_storage`, which is useful for biomedical
schema and missingness testing but is not a broadly recognised standard EDA
fixture.

## Goal

Add `palmerpenguins::penguins_raw` as a pinned external truth fixture for
standard EDA behaviour.

## Non-goals

- Implementing new EDA functions.
- Replacing `blood_storage`.
- Adding `palmerpenguins` to `Imports`.
- Making routine tests require internet access or `palmerpenguins`.

## Candidate Files

- `data-raw/test-fixtures/make_penguins_raw_fixture.R`
- `tests/testthat/fixtures/penguins_raw/`
- `tests/testthat/fixtures/README.md`
- `tests/testthat/test-fixture-generation-guardrails.R`
