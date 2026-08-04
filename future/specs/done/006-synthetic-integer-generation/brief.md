# Brief

Spec ID: `006-synthetic-integer-generation`
Status: Implemented
Owner: Antonio Berlanga-Taylor

## Problem

Synthetic integer generation passes its candidate values directly to `sample()`. Base R treats a single positive numeric value specially, so bounds such as `min = max = 5` can produce values from 1 through 5 instead of only 5. Fractional bounds containing no integer also lack a clear failure contract.

## Goal

Make synthetic integer generation respect the inclusive specification bounds for singleton, multi-value and empty integer domains, with tests that cannot pass vacuously on all-missing output.

## Non-goals

- Changing the public `epi_eda_generate_synthetic_data()` API.
- Changing seeded output to a prescribed sequence beyond reproducibility.
- Changing numeric, categorical, binary, date, datetime or text generation.
- Implementing plot-dispatch or external-fixture follow-ups.

## Candidate Files

- `R/eda_synthetic.R`
- `tests/testthat/test-eda_synthetic-fixtures.R`

## Risks

- Index-based sampling must preserve valid zero-row output.
- Invalid fractional bounds must fail before attempting to sample.
