# penguins_raw fixture provenance

## Source

- Dataset: `penguins_raw`
- Source package: `palmerpenguins`
- Source package version used for fixture generation: 0.1.1
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
- `expected_schema.csv`: independently computed schema contract.
- `expected_missing.csv`: independently computed missingness contract.
- `expected_summary_numeric.csv`: independently computed numeric summaries.
- `expected_summary_categorical.csv`: independently computed categorical summaries.
- `expected_plot_inventory.csv`: independently defined non-visual plot dispatch.

## Regeneration

Run from the repository root:

```sh
scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R
```

The script computes expected outputs with base R and does not call the
package under test.
