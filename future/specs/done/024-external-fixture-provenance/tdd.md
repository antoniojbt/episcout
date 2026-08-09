# Test Design

Spec ID: `024-external-fixture-provenance`
Status: Implemented

## Test Files

- `tests/testthat/test-fixture-provenance.R`
- `tests/testthat/test-fixture-generation-guardrails.R`
- existing fixture-backed suites

## Behaviour Tests

- [x] Every fixture-family file appears exactly once in `CHECKSUMS.sha256`.
- [x] Every committed file matches its offline SHA-256 entry.
- [x] Source records pin archive and serialized fixture checksums.
- [x] Licence/redistribution evidence is explicit for both families.
- [x] Manual regeneration reproduces committed bytes from verified archives.
- [x] Existing expected outputs and package behaviour remain unchanged.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R
scripts/rscript_env_caller.R -e "devtools::test(filter = 'fixture-(provenance|generation-guardrails)', reporter = 'summary')"
scripts/check-local.sh
git diff --check
```
