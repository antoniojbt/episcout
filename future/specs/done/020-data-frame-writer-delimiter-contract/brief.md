# Brief

Spec ID: `020-data-frame-writer-delimiter-contract`
Status: Completed
Owner: repository-owner

## Problem

`epi_write_df(..., suffix = "csv")` names a file as CSV but delegates to the
tab-separated default of `epi_write()`. Downstream readers can therefore infer
the wrong format from the extension.

## Goal

Give `epi_write_df()` a small explicit CSV/TSV contract: infer the delimiter
from a validated suffix, reject contradictory explicit delimiters, preserve TSV
output, and document the intentional CSV bug fix and existing write defaults.

## Non-goals

- A general serialisation framework or a new dependency.
- Changes to `epi_write()` or its historical defaults.
- New delimiter formats beyond CSV and TSV.
- Automatic directory creation or a new overwrite policy.

## Candidate Files

- `R/epi_write_df.R`
- `tests/testthat/test-epi-write-df.R`
- `man/epi_write_df.Rd`
- `NEWS.md`
- planning and closeout records under `future/`

## Risks

- Existing callers that relied on mislabeled tab content with `suffix = "csv"`
  will receive real comma-separated output.
- Permitting a suffix/delimiter mismatch would recreate the defect.
- Tests that parse output rather than inspect bytes could miss a regression.
