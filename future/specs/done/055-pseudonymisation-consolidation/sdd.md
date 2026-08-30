# Software Design

Spec ID: `055-pseudonymisation-consolidation`
Status: Active

## Compatibility Boundary

`epi_sec_linkage_spec()` retains its exact four components. A new metadata-only identifier contract supplies one rule per linked source table. Omitting it preserves byte-exact identity preparation, no validity expression and fail-on-invalid behaviour. Existing identity-universe version-2 objects remain accepted.

## Identifier Preparation

Rules support `identity`, `trim` and `trim_upper`. Null and blank prepared identifiers remain errors. An optional PostgreSQL regular expression classifies nonblank prepared identifiers. Policy `fail` prevents all writes; `retain_and_flag` still assigns a token and adds one declared Boolean validity column. Any many-to-one collision introduced by preparation is an error before writes.

Preparation metadata is fingerprinted. A populated namespace may not silently change preparation; callers must use a new namespace or registry. Batch-size tuning is operational and excluded from semantic fingerprints.

## Registry Evolution And Import

Registry version 2 records namespace preparation and source/run/table fingerprints. Initialisation migrates a structurally valid version-1 registry transactionally, classifying populated namespaces as exact identity. A neutral audit/apply import copies reviewed source identifier/token pairs into `entities` and `aliases` without collecting identifier vectors in R. It rejects duplicates, blank values, token conflicts and namespace/preparation conflicts atomically.

## Bounded Allocation And Reconciliation

New canonical identifiers are processed in stable database order in caller-bounded batches (default 50,000; range 1..1,000,000). Candidate tokens use set-based temporary tables and set-based collision checks, retrying a batch at most five times. The implementation never holds the complete identifier or token universe in an R vector and remains one atomic transaction rather than a resumable workflow.

Configuration, prepared source and per-table output fingerprints provide machine-readable reconciliation evidence alongside counts. Existing duplicate and count evidence remains.

## Privilege Contract

Audit returns structured, value-free issues for effective source `SELECT`, database `TEMP`, schema use/create and state-dependent registry/output table privileges. Replacement requires effective ownership, including inherited owning roles through `pg_has_role`. Apply repeats the same audit inside its transaction. Episcout never grants privileges or selects a role.

## Delivery Sequence

1. Registry v2, identifier rules and neutral mapping import.
2. Bounded allocation and reconciliation fingerprints.
3. Privilege/preflight parity and effective ownership.
4. Synthetic migration/operator guide and end-to-end acceptance, followed by Episcout 0.6.0.

Each successor depends on the previous merge. Downstream migration begins only after the released replacement exists.
