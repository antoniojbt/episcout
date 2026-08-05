# Brief

Spec ID: `017-deterministic-local-time-ambiguity`  
Status: Completed  
Owner: repository-owner  

## Problem

`epi_eda_prepare()` samples offsets through the host timezone database to decide whether an unzoned local datetime identifies exactly one instant. On the documented local macOS environment that database omits the October 1969 Kwajalein rollback, so the ambiguous wall time `1969-09-30T12:30:00` is incorrectly prepared while GitHub macOS and Ubuntu checks block it.

## Goal

Use one packaged IANA timezone engine to classify and convert local wall times deterministically across supported environments. A wall time is preparable only when it maps to exactly one instant; ambiguous, nonexistent, unsupported or unclassifiable inputs remain value-free blockers.

## Non-goals

- Changing `epi_eda_prepare()` formals, result components, audit columns, missingness rules or all-or-nothing application.
- Replacing the existing host-independent arithmetic for `Z` or numeric-offset datetimes.
- Pinning timezone interpretations forever when IANA publishes corrected historical evidence.
- Activating draft backend spec 003, releasing the package or creating a tag.

## Candidate Files

- `R/eda_prepare.R`
- `tests/testthat/test-eda-prepare.R`
- `DESCRIPTION`, `NEWS.md` and generated `man/epi_eda_prepare.Rd`

## Risks

- Mishandling fractional seconds while converting through a second-precision `POSIXct` interface.
- Allowing an engine failure to escape as a raw error instead of a safe blocker.
- Raising the package minimum R version without documenting the compatibility change.
