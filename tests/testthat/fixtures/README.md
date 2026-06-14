# External test fixtures

This directory contains small, pinned, publicly available biomedical datasets for
specification-first EDA tests.

Fixtures are committed so routine tests do not require internet access or live
downloads. Expected outputs are stored beside the source data and must be
computed independently with simple transparent code, not with the `episcout`
functions that later tests exercise.

Regenerate fixtures manually with:

```sh
Rscript data-raw/test-fixtures/make_external_fixtures.R
```

The regeneration script may require optional source packages such as
`medicaldata`, but those packages are not required to use the committed fixtures
in offline tests.
