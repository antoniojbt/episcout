# Brief

Spec ID: `041-reviewed-civil-date-derivation`

Status: Active

Owner: repository-owner

Tracking issue: issue-273

## Problem

Some reviewed source fields mean a civil calendar date even though upstream storage uses a local timestamp. Episcout currently rejects PostgreSQL `timestamp without time zone` for instant-oriented datetime preparation, but it has no separate approved operation that can preserve such a source and derive a `Date` without assigning a timezone. Treating midnight observations, a field name or a database type as semantic evidence would cross the analyst-review boundary.

## Goal

Add an exact analyst-approved civil-date-operation object and one additive application operation for data-frame and PostgreSQL sources. The operation resolves an opaque source key, records a distinct derived name, explicitly declares `civil_date` semantics, requires `preserve_source = TRUE` and `require_midnight = TRUE`, validates every non-missing local timestamp before derivation, and publishes only a complete result.

Supported in-memory local timestamps use strict timezone-free character storage. PostgreSQL support is limited to `timestamp without time zone`. Successful application preserves every source value and adds one `Date`/PostgreSQL `date` column per operation. Missing source values produce missing derived dates. Any non-midnight value blocks the complete call with only one aggregate affected-value count.

## Required User Outcome

- A caller explicitly constructs approved operations; episcout does not infer civil-date meaning from names, storage or observed midnight values.
- Every operation and source is validated before a derived column, file or table is created.
- Data-frame application returns a complete new data frame and can publish CSV or RDS without replacing an existing file.
- PostgreSQL validation and new-table creation occur server-side in one transaction without collecting timestamps or assigning a timezone.
- Failures disclose no timestamp value, row identity, field name, relation identity, destination identity or approval reference.

## Non-goals

- Inferring date meaning, timezone meaning or approval automatically.
- Accepting `POSIXct`, `POSIXlt`, timestamps with offsets or zones, or PostgreSQL `timestamp with time zone` as local timestamps.
- Correcting, rounding or truncating non-midnight values; overwriting or removing the source; changing instant-oriented preparation or ingestion semantics.
- Replacing existing files or PostgreSQL relations, adding constraints/indexes/privileges, or supporting RData or Parquet.

## Recovery

Failed in-memory validation returns before copying or adding columns. Failed file publication uses the existing private same-directory staging and no-replace cleanup contract. Failed PostgreSQL validation, creation, reconciliation or commit rolls back the transaction, leaving the destination absent and the caller-owned connection idle when rollback is available.

## Terminal Outcome

Issue-273 is the terminal authorised cleaning-lane capability. No automatic successor is required after merge and canonical closeout.
