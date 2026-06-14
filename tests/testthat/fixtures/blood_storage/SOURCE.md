# blood_storage fixture provenance

## Source

- Dataset: `blood_storage`
- Source package: `medicaldata`
- Source package version used for fixture generation: 0.2.0
- Observations: 316
- Variables: 20
- Study type: retrospective cohort
- Clinical area: prostate cancer recurrence after perioperative transfusion

## Citation from source documentation

Cata et al. Blood Storage Duration and Biochemical Recurrence of Cancer after
Radical Prostatectomy. Mayo Clinic Proceedings. 2011;86(2):120-127.

## Fixture files

- `blood_storage.csv`: pinned data exported from `medicaldata::blood_storage`.
- `blood_storage_spec.csv`: manually reviewed fixture data dictionary for the
  specification-first EDA workflow.
- `expected_schema.csv`: independently computed expected schema result for the
  unmodified fixture data.
- `expected_missing.csv`: independently computed expected missingness result for
  the unmodified fixture data.

## Regeneration

Run from the repository root:

```sh
Rscript data-raw/test-fixtures/make_external_fixtures.R
```

The script computes expected outputs with base R and does not call new
`episcout` EDA functions.
