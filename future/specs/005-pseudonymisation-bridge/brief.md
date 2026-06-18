# Brief

Spec ID: `005`  
Status: Implemented  
Owner: Antonio Berlanga-Taylor

## Problem

Study datasets often need participant identifiers replaced by non-derivable
tokens before analysis. Hashing names, CURP, email, study IDs or other direct
identifiers is unsafe without carefully managed keyed HMAC because identifiers
can be guessed.

## Goal

Add `epi_sec_pseudonym()` to generate a secure bridge table that maps each
participant identifier to a random cryptographic token.

## Non-goals

- Detecting PII columns.
- Rewriting full study datasets.
- Removing identifier columns from analysis datasets.
- Chunked bridge generation.
- Database-backed bridge storage.

## Candidate Files

- `R/epi_sec_pseudonym.R`
- `tests/testthat/test-epi_sec_pseudonym.R`
- `DESCRIPTION`
- `NEWS.md`
- `future/TODOs.md`

## Risks

- Bridge files remain re-identifying data and must not be treated as anonymous.
- Non-cryptographic or deterministic token generation would weaken the helper.
- Overwriting an existing bridge file could break linkage or audit trails.
