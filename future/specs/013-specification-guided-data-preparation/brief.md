# Brief

Spec ID: `013-specification-guided-data-preparation`
Status: Completed
Owner: Antonio Berlanga-Taylor

## Problem

The specification-first EDA workflow can describe schema compatibility and apply declared sentinel codes during summaries, but it deliberately leaves received data unchanged. Users must currently write ad hoc conversions between a reviewed specification and canonical EDA, which can make the prepared dataset, audit trail and dictionary disagree.

## Goal

Add `epi_eda_prepare()` as an in-memory, specification-guided assessment and preparation boundary. Audit mode reports every proposed action while returning the original data unchanged. Apply mode transforms only when every requested action is unambiguous and non-blocking, returns a complete prepared copy, and never exposes or writes row-level values.

## Required User Outcome

- A user can inspect missing-sentinel, type, level, presence, extra-variable and dataset findings before any transformation occurs.
- Apply mode is all-or-nothing and preserves source row count and order.
- Every transformation has machine-readable status, counts and a value-free reason.
- The reviewed specification remains authoritative; preparation does not invent scientific semantics or silently rewrite it.
- A successfully prepared dataset reconciles with schema checking and canonical EDA.

## Non-goals

- Inferring types, roles, identifiers, keys, privacy classes, missing sentinels, categorical levels, validation ranges or units.
- Locale-dependent character-to-numeric parsing.
- Rounding invalid numeric values or converting invalid/unexpected observations to missing.
- Automatically dropping duplicate rows, inferring duplicate keys or resolving conflicting records.
- Identifying PII, rewriting identifiers, anonymising data or changing `epi_sec_pseudonym()`.
- Writing raw or prepared data, audits or schemas to disk.
- Rewriting the reviewed specification when unexpected categorical levels are appended.
- Orchestrating intake-to-report artifacts, stratified analysis, tagging or releasing the package.

## Candidate Files

- `R/eda_prepare.R`
- `R/eda_schema.R`
- `R/eda_spec.R`
- `R/summary_cores.R`
- `tests/testthat/test-eda-prepare.R`
- `README.md`
- `vignettes/specification-first-eda.Rmd`
- `NEWS.md`
- generated `man/` and `NAMESPACE`

## Risks

- Sentinel replacement can disagree with canonical missingness if matching semantics are duplicated incorrectly.
- Character numeric values can be misread under an unstated decimal mark, grouping mark or locale.
- Local datetimes can shift under the machine timezone or become ambiguous around daylight-saving transitions.
- Categorical append mode can make prepared factor metadata diverge from the reviewed specification.
- A late failure can expose a partially prepared object unless all plans are resolved before mutation.
- Audit reasons can disclose sensitive values if raw examples are included for convenience.
- Extra or nested columns can undermine the claim that output is analysis-ready if their policy is not explicit.

## Approval

The repository owner explicitly instructed implementation of issue #182 and its ordered dependent work on 2026-08-03. This completed planning set is approved and active without further confirmation unless later evidence exposes an ambiguity that would materially change privacy, scientific meaning or the public interface.
