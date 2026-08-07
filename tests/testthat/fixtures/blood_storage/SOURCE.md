# blood_storage fixture provenance

## Source

- Dataset: `blood_storage`
- Source package: `medicaldata`
- Source package version used for fixture generation: 0.2.0
- Canonical source archive: https://cran.r-project.org/src/contrib/medicaldata_0.2.0.tar.gz
- Source archive SHA-256: `56dab0c6078e6f9a9f183427a4481c5497e5d107b795bf965cc7ce4ac4c39236`
- Serialized fixture SHA-256: `e3a1c6b83de9ddae8380ef2a92ce995fe927c5a176c589039d8b6089dae812b9`
- Observations: 316
- Variables: 20
- Licence: MIT (`LICENSE.medicaldata` reproduces the package notice)
- Redistribution basis: the fixture is an exact serialization of the dataset distributed in the MIT-licensed `medicaldata` source package.
- Study type: retrospective cohort
- Clinical area: prostate cancer recurrence after perioperative transfusion

## Citation from source documentation

Cata et al. Blood Storage Duration and Biochemical Recurrence of Cancer after
Radical Prostatectomy. Mayo Clinic Proceedings. 2011;86(2):120-127.

## Fixture files

- `blood_storage.csv`: pinned data exported from `medicaldata::blood_storage`.
- `blood_storage_spec.csv`: manually reviewed fixture data dictionary for the specification-first EDA workflow.
- `expected_schema.csv`: generator-produced regression projection of the historical presence and observed-class fields; it is not independent evidence of type compatibility.
- `expected_missing.csv`: independently computed expected missingness result for the unmodified fixture data.
- `LICENSE.medicaldata`: source-package MIT copyright notice.
- `CHECKSUMS.sha256`: offline drift guard for every committed file in this fixture family.

## Extraction and transformation

The verified source archive is installed into a temporary library and `medicaldata::blood_storage` is loaded from that isolated installation. The object is serialized with `write.csv(row.names = FALSE, na = "")`. No row, column or value is transformed or excluded.

## Regeneration

Run from the repository root:

```sh
scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R
```

The script computes expected outputs with base R and does not call the package under test. The generated schema classifier mirrors the package's historical classifier, so hand-authored tests provide the independent evidence for type compatibility.
