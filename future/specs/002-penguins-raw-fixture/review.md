# Review Notes

Spec ID: `002-penguins-raw-fixture`  
Status: Implemented

## Review Focus

- Are expected outputs independent of implementation code?
- Are fixture files small, committed and deterministic?
- Do routine tests avoid internet access and optional source packages?
- Does the PR avoid implementation-file changes?
- Does every expected-output file have an executable consumer?
- Do plot inventory tests prove specification-based dispatch without snapshots?

## Findings

- Expected outputs are produced by transparent base R helpers and the executable
  guard rejects calls to the package under test.
- The generator reads each serialized CSV before producing schema and
  missingness expectations.
- Manual review caught empty-string serialization of character missing values;
  using the standard `NA` marker preserves the upstream 336 missing cells.
- Plot tests compare both `GeomBar` and `StatBin`/`StatCount`, proving dispatch
  without visual snapshots.
- Routine tests use only committed files. Source packages remain optional and
  are not declared in `DESCRIPTION`.
- Existing blood fixture data and expected CSVs were unchanged; only provenance
  formatting and the regeneration command changed.
