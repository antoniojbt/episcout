# penguins_raw fixture provenance

## Source

- Dataset: `penguins_raw`
- Source package: `palmerpenguins`
- Source package version used for fixture generation: 0.1.1
- Canonical source archive: https://cran.r-project.org/src/contrib/palmerpenguins_0.1.1.tar.gz
- Source archive SHA-256: `2a40d48ba6c7978fdf2a6daf647ccb39cd17590680138931d11194d3dd1a30b4`
- Serialized fixture SHA-256: `a634e85f0676c74c4cd73f94ff8cbf9ec12540d01797434cf1fd0ba8d9af663f`
- Observations: 344
- Variables: 17
- Licence: CC0
- Dataset documentation: https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html
- Package website: https://allisonhorst.github.io/palmerpenguins/

## Citation

Gorman KB, Williams TD, Fraser WR (2014). Ecological Sexual Dimorphism and
Environmental Variability within a Community of Antarctic Penguins (Genus
Pygoscelis). PLoS ONE 9(3): e90081.
https://doi.org/10.1371/journal.pone.0090081

## Fixture files

- `penguins_raw.csv`: pinned data exported from `palmerpenguins::penguins_raw`.
- `penguins_raw_spec.csv`: manually reviewed EDA data dictionary.
- `expected_schema.csv`: generator-produced regression projection of the historical presence and observed-class fields; it is not independent evidence of type compatibility.
- `expected_missing.csv`: independently computed missingness contract.
- `expected_summary_numeric.csv`: independently computed numeric summaries.
- `expected_summary_categorical.csv`: independently computed categorical summaries.
- `expected_plot_inventory.csv`: independently defined non-visual compact-plot dispatch for every declared variable.
- `CHECKSUMS.sha256`: offline drift guard for every committed file in this fixture family.

## Extraction and transformation

The verified source archive is installed into a temporary library and `palmerpenguins::penguins_raw` is loaded from that isolated installation. The object is serialized with `write.csv(row.names = FALSE, na = "NA")`. No row, column or value is transformed or excluded.

## Regeneration

Run from the repository root:

```sh
scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R
```

The script computes expected outputs with base R and does not call the package under test. The generated schema classifier mirrors the package's historical classifier, so hand-authored tests provide the independent evidence for type compatibility.
