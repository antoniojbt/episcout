# Brief

Spec ID: `012-data-frame-eda-spec-scaffold`
Status: Completed
Owner: Antonio Berlanga-Taylor

## Problem

The specification-first EDA workflow can validate and use an existing data dictionary, and the database workflow can scaffold a dictionary from an `epi_db_inventory`, but a user who receives an in-memory data frame without a dictionary must manually author every specification row before using `epi_eda_run()`.

Observed storage can provide useful structural evidence, but it cannot establish epidemiological meaning, variable roles, units, privacy classification, sentinel missing codes, valid ranges or contractual requiredness. Automatically inventing those fields would create unsafe analytical claims.

## Goal

Add `epi_eda_spec_scaffold(data, max_candidate_levels = 20L)`, which returns one deterministic, ordinary, CSV-safe data frame row per source column in source order. The result contains a valid draft EDA specification plus aggregate structural evidence that helps a person review it without silently coercing data or asserting semantic meaning.

The observable user journey is to read data, create and save a draft, review and edit it, load the reviewed file with `epi_eda_spec()`, and pass it to `epi_eda_run()`.

## Required User Outcome

- Supported storage classes receive conservative initial EDA types.
- Low-cardinality and strict temporal shapes are review hints only when they do not follow directly from declared storage metadata.
- Semantic fields remain explicitly unset and `required` remains typed `NA`.
- No observed values, identifiers or examples appear in the scaffold, warnings, messages or errors.
- Unsupported columns and unsafe declared level metadata fail clearly before a partial result is returned.
- The returned draft round-trips through CSV and `epi_eda_spec()` and integrates with the canonical six-component EDA summary after human review.

## Non-goals

- Reading files or broadening `epi_read()`.
- Mutating, coercing, dropping or recoding source columns.
- Inferring roles, privacy classes, identifiers, units, missing sentinels, requiredness, validation bounds or scientific meaning.
- Enumerating observed character or integer values in `candidate_levels`.
- Changing the extended database dictionary contract or `epi_eda_dictionary_scaffold()`.
- Changing `epi_stats_summary(output = "typed")` unsupported-column behaviour.
- Adding a general serialisation format or new package dependency.
- Preparing data, producing stratified summaries, orchestrating intake-to-report output, tagging or releasing the package.

## Candidate Files

- `R/eda_spec_scaffold.R`
- `R/eda_spec.R`
- `R/summary_cores.R`
- `tests/testthat/test-eda-spec-scaffold.R`
- `README.md`
- `vignettes/specification-first-eda.Rmd`
- `NEWS.md`
- generated `man/` and `NAMESPACE`

## Risks

- Low cardinality can describe identifiers, site codes or sparse measurements rather than categorical variables.
- Enumerating observed candidate values can disclose sensitive data, even when the number of distinct values is small.
- Semicolon-delimited specification levels cannot faithfully encode levels that contain semicolons or unsafe boundary whitespace.
- Storage subclasses such as `integer64`, labelled vectors, `difftime`, matrix columns and nested data can be misclassified if only base storage predicates are inspected.
- Empty or all-missing vectors can produce false date, datetime or categorical candidates if the implementation treats vacuous predicates as evidence.
- CSV type inference can alter blank and missing fields unless the round-trip contract is tested directly.

## Approval

The repository owner explicitly instructed implementation of issue #181 and the dependent issues on 2026-08-03. This brief, the SDD, TDD and acceptance contract are therefore approved for implementation without further confirmation unless a later ambiguity would change privacy, scientific meaning or the public interface.
