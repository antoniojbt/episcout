# Review Notes

Spec ID: `020-data-frame-writer-delimiter-contract`
Status: Completed

## Root Cause

`epi_write_df()` used `suffix` only while constructing its filename and called
`epi_write()` without `sep`, so every suffix inherited the tab delimiter. The
file extension and serialized bytes therefore diverged for CSV.

## Findings

No unresolved local implementation finding. The public function now validates
the existing output directory, base filename and the deliberately narrow
CSV/TSV suffix set, derives the expected delimiter and rejects an explicit
mismatch. The lower-level `epi_write()` API, TSV bytes, overwrite behaviour,
return value and success message remain unchanged.

The CSV change is an intentional user-visible bug fix: callers that previously
requested `suffix = "csv"` receive comma-separated bytes. NEWS and help state
that compatibility decision and direct callers needing other formatting to
`epi_write()`.

## Verification Evidence

- The pre-code `missing-functions` tests passed and a direct probe reproduced
  tab bytes under a CSV filename.
- The new focused suite passed 28 expectations covering exact CSV/TSV bytes,
  case handling, paths, success messages, zero rows, overwrite and validation.
- The legacy `missing-functions` suite passed after implementation.
- Package-loaded `lintr::lint_package()` returned no findings.
- The full test suite passed with the repository's expected environment skips.
- `scripts/check-local.sh` exited successfully. Its known documentation and
  skipped-visual-snapshot cleanup side effects were restored without retaining
  unrelated changes.
- The first Ubuntu CI run stopped at four continuation-indentation findings
  reported by its newer linter; the follow-up aligned those continuations
  without changing behaviour and reran focused tests and package lint locally.
- `git diff --check` passed after restoration.

## Closeout Notes

Software verification confirmed that the raw-byte assertions fail against the
baseline defect and exercise the public API. Truth/semantics review confirmed
that suffix-to-delimiter mapping, rather than parsing through the same writer,
independently establishes the expected output. Copy-edit review found the API,
help and NEWS wording consistent about delimiter, directory, overwrite, quote
and missing-value behaviour. Issue #197/spec 019 was not started. Pull-request
PR #206 passed macOS, Ubuntu, PostgreSQL integration, coverage, Codecov and
CodeFactor checks and was merged into `master` as `4040cf8` on 2026-08-07. The
completed record now belongs under `future/specs/done/`.
