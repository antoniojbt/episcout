# Brief

Spec ID: `040-approved-cleaning-rules-and-processed-outputs`

Status: Active

Owner: repository-owner

Tracking issue: issue-272

## Problem

Spec-039 produces aggregate descriptive evidence and explicitly pending review prompts, but episcout has no separately validated executable rule contract and no atomic path that applies analyst-approved rules to a complete processed output. Treating the pending proposal table or descriptive dictionary `min` and `max` fields as executable would erase the approval boundary and could publish partially transformed data.

## Goal

Add an exact analyst-approved rule object and one application operation for data-frame and PostgreSQL sources. Approved numeric bounds, categorical or binary allowed values and missing-value codes convert only affected observations to typed missing values. The operation preserves the source, data-frame row order and dimensions; returns processed in-memory data; optionally publishes CSV or RDS without replacement; or creates one new PostgreSQL destination table in a transaction.

Every successful result includes a deterministic approved-rule hash, source and destination dimensions, aggregate missing counts before and after processing, opaque-keyed per-variable transition counts and final reconciliation flags. Returned records, errors, printing, examples and fixtures contain no source rows, source names, relation names, destination names, disallowed values or approval references.

## Required User Outcome

- A caller explicitly constructs approved rules with the exact spec-040 schema; issue-271 evidence and proposal objects are rejected.
- All rules and source compatibility are validated before in-memory transformation or publication.
- Data-frame processing returns a complete processed data frame and can publish that same object as CSV or RDS through a no-replace staged write.
- PostgreSQL processing executes server-side `CASE` transformations and all validation, creation and reconciliation inside one transaction that rolls back on failure.
- Aggregate audit counts reconcile before success, while source values and private source or destination identity are never returned.

## Non-goals

- Approving an issue-271 proposal automatically or authenticating the analyst outside the caller-owned approval record.
- Editing the EDA semantic dictionary or treating descriptive `min`, `max`, `levels` or `missing_codes` as executable.
- Changing a source data frame or PostgreSQL relation, overwriting an output, filtering rows, imputing values, winsorising values or deriving dates.
- Supporting text, date or datetime rules, RData, Parquet, table constraints, indexes, privileges or physical PostgreSQL row ordering in the first contract.
- Implementing issue-273 civil-date derivation.

## Recovery

Failed in-memory validation returns before transformation. Failed file writes remove the private staging file; failed publication reconciliation removes only the newly linked destination. Failed PostgreSQL validation, transformation, reconciliation or commit rolls back the transaction, so the destination table is absent and the caller-owned connection returns idle when PostgreSQL permits rollback.

## Successor

`issue-273` remains the staged terminal successor for explicitly reviewed civil-date derivation. It cannot begin until issue-272 is merged and closed out canonically.
