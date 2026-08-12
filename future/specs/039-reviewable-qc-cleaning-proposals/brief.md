# Brief

Spec ID: `039-reviewable-qc-cleaning-proposals`

Status: Active

Owner: repository-owner

Tracking issue: issue-271

## Problem

The current EDA workflow separates reviewed semantic metadata from observations, but it has no reviewable QC result that turns aggregate structural evidence into explicitly pending cleaning-rule proposals. Analysts can inspect descriptive summaries, yet there is no stable value-free linkage between that evidence and a proposal, and no package contract prevents an empirical hint from being mistaken for an approved or executable rule.

The former data-frame scaffold mixed evidence and review fields into the semantic specification. That schema was deliberately removed by spec-030 so the semantic dictionary remains lean and authoritative. Reintroducing proposal fields into the dictionary would repeat the same ambiguity and could silently change reviewed type, units, levels, missing codes or descriptive `min`/`max` metadata.

## Goal

Add one read-only public operation that accepts an in-memory data frame or an `epi_eda_postgres_source`, the same reviewed EDA specification, and a caller-maintained opaque variable-key map. It returns a descriptive evidence table and a separate pending-proposal table. Both tables use only opaque variable keys, deterministic aggregate fields and fixed reason codes; neither table contains source names, source rows, observed categorical/text values, approved rules or cleaned data.

The first contract may prompt review of missing units, an exact observed `0`/`1` binary candidate, non-finite numeric observations and finite values beyond Tukey 1.5-IQR fences. Binary and fence results remain review prompts. The generator never infers a unit or missing-value code, never enumerates observed levels other than the issue-authorised generic `0;1` binary candidate and never promotes a proposal to an approved rule.

## Required User Outcome

- Every reviewed specification row links to one aggregate evidence row through a caller-managed stable opaque `variable_key`; the source name used for lookup is never returned.
- Evidence, pending proposals and future approved rules use different tables, state fields and authorities.
- Equivalent data-frame and PostgreSQL inputs produce the same proposal decisions and equivalent aggregate evidence without collecting categorical or text values from PostgreSQL.
- Existing semantic `type`, `units`, `levels`, `min`, `max` and `missing_codes` remain unchanged and authoritative.
- Zero-row, all-missing, incompatible, unsupported, explicitly declared identifier and high-cardinality inputs have deterministic non-disclosing outcomes.
- The complete operation is read-only and does not mutate the in-memory source, PostgreSQL relation, specification or variable-key map.

## Non-goals

- Approving, applying, validating as executable or persisting cleaning rules.
- Recoding, coercing, imputing, winsorising, filtering or producing a cleaned data set.
- Adding proposal fields to the semantic specification or extended PostgreSQL dictionary.
- Treating observed extrema, Tukey fences or low cardinality as scientific validity limits.
- Inferring domain meaning, roles, units, scientifically plausible ranges, sentinel codes, categories, privacy classes or civil-date meaning from names or values.
- Enumerating observed character, factor, enum or general integer/numeric levels.
- Adding proposal artifacts to intake, runner, report or PostgreSQL delivery bundles in the first implementation.
- Writing CSV, RDS or database outputs, creating tables or changing PostgreSQL privileges.
- Implementing issue-272 approved-rule application or issue-273 civil-date derivation.

## Candidate Files

- new `R/eda_qc_proposals.R`
- bounded aggregate support in `R/eda_postgres_queries.R`
- new `tests/testthat/test-eda-qc-proposals.R`
- generated roxygen help and `NAMESPACE`
- `README.md`, `NEWS.md` and `vignettes/specification-first-eda.Rmd`

## Risks

- Deterministic keys derived from source names would be guessable and would make backend-independent privacy claims unsafe; keys therefore remain caller-managed opaque metadata.
- Complete categorical frequencies or examples would turn high-cardinality QC into an unbounded value collection path; the QC path needs scalar counts only.
- A highly imbalanced `0`/`1` variable can make one binary level appear beyond a Tukey fence; the binary candidate must take precedence and suppress a screening-bound proposal for that variable.
- Tukey fences can be unstable, degenerate or scientifically irrelevant; they may be copied only as labelled screening prompts when finite tail counts are non-zero.
- Numeric extrema and small aggregate counts may still be sensitive in a caller's context even without names or rows; the function must not write or publish them automatically.
- Reusing canonical categorical summaries internally would collect observed levels even if the returned result later discarded them.
- Floating-point database aggregates can differ at machine precision; proposal decisions require exact count predicates while cross-backend numerical evidence uses the repository's declared tolerance.

## Successor

`issue-272` is the staged successor. It will define and validate a separate analyst-approved executable rule schema and apply only approved rules. Editing `proposal_state`, copying a proposal table or receiving an `epi_eda_qc_proposals` result is never approval.
