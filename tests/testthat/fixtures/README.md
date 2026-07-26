# External test fixtures

This directory contains small, pinned, publicly available external datasets for
specification-first EDA tests.

Fixtures are committed so routine tests do not require internet access or live downloads. Expected outputs are stored beside the source data and must be computed independently with simple transparent code, not with the `episcout` functions that later tests exercise.

Regenerate fixtures manually with:

```sh
scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R
```

The regeneration command rebuilds both fixtures and requires the optional source
packages `medicaldata` and `palmerpenguins`. Neither package is required to use
the committed fixtures in offline tests.

- `blood_storage` provides a biomedical workflow fixture with clinical variable
  semantics.
- `penguins_raw` provides a widely recognised general EDA fixture with raw,
  non-syntactic column names and mixed variable types.
