# Software Design

Spec ID: `020-data-frame-writer-delimiter-contract`
Status: Implemented

## Scope

Strengthen only `epi_write_df()`. The lower-level `epi_write()` API and default
tab separator remain unchanged.

## Public API

Add an optional final `sep = NULL` argument. The default resolves from a
case-insensitive suffix mapping: `csv` to comma and `tsv` to tab. An explicit
separator is accepted only when it is the mapped separator. This makes a
contradictory extension impossible through this convenience function.

This is an intentional correction for existing `suffix = "csv"` calls. The
development NEWS records that their bytes change from tab to comma. Existing
`suffix = "tsv"` calls retain their filename and bytes.

## Inputs And Outputs

- `df` continues to be passed to `epi_write()`.
- `results_subdir` must identify an existing directory; no directory is created.
- `file_n` must be one non-empty string and must not contain a path separator.
- `suffix` must be one non-empty `csv` or `tsv` string without a leading dot.
- `sep` is `NULL` or the single delimiter matching the suffix.
- The returned path uses `file.path()` and the existing file is overwritten by
  `data.table::fwrite()` as before.
- Column names are written, row names and quoting are disabled, and missing
  values are written as `NA`, inherited from `epi_write()`.

## Data Flow

1. Validate the directory, base filename, suffix and optional separator.
2. Resolve the canonical delimiter from the suffix.
3. Construct the output path with `file.path()`.
4. Delegate to `epi_write(df, outfile, sep = resolved_sep)`.
5. Emit the existing saved-path message and return the path.

## Edge Cases

- Reject a missing output directory rather than relying on a lower-level error.
- Reject unsupported or malformed suffixes.
- Reject filenames containing `/` or `\\` so `results_subdir` remains the sole
  directory control.
- Reject explicit separators that contradict the suffix.
- Preserve zero-row data-frame headers through `data.table::fwrite()`.

## Errors And Warnings

All new validation errors are value-free, actionable and use `call. = FALSE`.
No new warnings are introduced.

## Dependencies

No dependency changes. Writing continues through the existing optional
`data.table` namespace check in `epi_write()`.
