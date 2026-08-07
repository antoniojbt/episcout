# Brief

Spec ID: `024-external-fixture-provenance`
Status: Implemented
Owner: repository-owner

## Problem

External fixtures are reproducible from named packages, but their committed
records lack immutable source/archive checksums, complete redistribution notices
and an offline whole-family drift guard.

## Goal

Make both fixture families reproducible, attributable and fail-closed while
keeping routine tests deterministic and offline.

## Non-goals

- Add source packages to package dependencies.
- Download anything during routine tests.
- Treat generator-produced schema projections as independent truth.
- Change analytical package behaviour.

## Candidate Files

- `data-raw/test-fixtures/make_external_fixtures.R`
- `tests/testthat/fixtures/`
- `tests/testthat/test-fixture-provenance.R`
- `tests/testthat/test-fixture-generation-guardrails.R`

## Risks

- Regeneration from an unverified package could silently change fixture bytes.
- A checksum manifest could omit a fixture-family file.
- Licence wording could overstate dataset-specific rights.
