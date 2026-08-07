# Test Design

Spec ID: `023-package-source-hygiene`
Status: Implemented

## Test Surfaces

- `.Rbuildignore` exact-path behaviour.
- `scripts/check-cran.sh` exact archive inspection.
- The resulting source tarball after `scripts/check-local.sh`.

## Behaviour Tests

- [x] The canonical local check may leave `Rplots.pdf`, but the next source
  archive excludes it.
- [x] The archive excludes `vignettes/R_datasets.xlsx`.
- [x] Packaged test sources contain no obsolete developer path.
- [x] Intended vignettes, SVG references and CSV fixtures remain.

## Acceptance Commands

```bash
scripts/check-local.sh
scripts/check-cran.sh
tar -tzf build/cran-check/episcout_*.tar.gz
git diff --check
```
