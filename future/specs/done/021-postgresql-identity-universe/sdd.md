# Software Design

Spec ID: `021-postgresql-identity-universe`

Status: Accepted for implementation

## Semantic Authority And Scope

The confirmed source metadata is authoritative for relation identity, identifier column, one exact identity namespace and provenance. PostgreSQL values establish only observed quality and set membership. One canonical identifier denotes one distinct observed identifier under the reviewed identity rule, not one confirmed person. Version 1 neither corrects values nor infers identity.

## Public API

```r
epi_sec_identity_universe_spec(
  sources,
  normalization = "identity",
  validity_regex = NULL
)

epi_sec_identity_universe_db(
  con,
  spec,
  mode = c("audit", "materialise"),
  output_schema = NULL,
  output_table = NULL,
  existing = "error",
  statement_timeout = 60,
  lock_timeout = 30
)
```

`sources` is a data frame or CSV with exactly `source_schema`, `source_table`, `id_column`, `identity_namespace`, `provenance` and `validation_status`. It contains at least two unique ordinary PostgreSQL relations, exactly one row per relation, one shared non-empty namespace, non-empty provenance and only `validation_status = "confirmed"`. SQL fragments and identifier values are rejected. The returned `epi_sec_identity_universe_spec` is deterministically sorted and contains `sources`, `normalization`, `validity_regex`, `contract_version` and `fingerprint_sha256`.

Only `normalization = "identity"` is accepted. `validity_regex` is `NULL` or one non-empty PostgreSQL regular expression; invalid syntax is detected in PostgreSQL before source aggregation. The fingerprint is SHA-256 over a versioned deterministic serialisation of the normalised value-free contract.

## PostgreSQL Boundary

Require an open, idle PostgreSQL connection. Every source is an ordinary table with the declared identifier column. All identifiers in the shared namespace must use the same supported family: text/varchar, integral or UUID. Text collations must be deterministic. SQL identifiers are quoted through DBI and values are parameters or quoted literals.

Audit owns one transaction, sets `REPEATABLE READ READ ONLY` and a local bounded `statement_timeout`, validates the source catalogue, and computes every result inside that snapshot. No source value is returned to R. Expected data-quality findings become fixed aggregate issues; database or infrastructure failures are surfaced through a fixed value-free error boundary.

Materialisation requires pre-existing `output_schema` restricted from `PUBLIC`, a valid plain `output_table`, `existing = "error"`, an idle connection and a destination distinct from every source. It obtains a bounded session advisory lock for the exact destination, begins one `REPEATABLE READ` transaction, transfers protection to a transaction advisory lock, sets the statement timeout, repeats catalogue and aggregate validation, and creates the destination only when no blocker remains. Any error rolls back the complete operation. It never alters source relations, creates schemas, replaces destinations or uses `CASCADE`.

## Aggregate Contract

The result has class `epi_sec_identity_universe_result` and fixed components `status`, `metadata`, `source_audit`, `namespace_audit`, `overlap_audit` and `issues`.

`source_audit` contains reviewed source metadata plus `n_input`, `n_null`, `n_blank`, `n_invalid`, `n_observed`, `n_distinct`, `n_duplicate_excess`, `max_frequency` and `status`. `n_observed` excludes null, blank and invalid values. Duplicate excess is observed rows minus distinct valid identifiers. Maximum frequency is zero for an empty observed set.

`namespace_audit` contains `identity_namespace`, `n_sources`, `n_input`, `n_observed`, `n_distinct`, `n_single_source`, `n_multi_source`, `n_collisions` and `status`. Source membership counts distinct source relations containing each canonical identifier, not row frequency. The partition invariant is `n_distinct = n_single_source + n_multi_source`.

`overlap_audit` contains one deterministic row per unordered source pair: both source metadata keys, each distinct count, intersection, each source-exclusive count, and directional coverage. Coverage is intersection divided by that source's distinct count and is missing when the denominator is zero.

`issues` contains stable `issue_code`, `severity`, `stage`, reviewed source metadata, `n_affected`, fixed `message` and fixed `recommended_action`. Null, blank, invalid and normalisation-collision findings are blocking. Duplicate excess is a warning because repeated rows need not indicate bad identity data. Empty sources are warnings. The whole result is `blocked` if a blocking issue exists, `audit_complete` after a blocker-free audit, and `complete` only after a committed materialisation.

## Identity And Collision Semantics

Identity normalisation preserves PostgreSQL equality for the supported family. Text comparison is exact under deterministic byte-distinguishing `C` collation; it performs no trimming or case folding. Blank detection uses `btrim(value::text) = ''` only to classify unusable values. Integral and UUID identifiers use their PostgreSQL equality before a canonical text representation is stored. The collision audit groups raw distinct values by canonical representation and blocks if more than one raw value maps to a canonical identifier. Under the version-1 identity rule the expected collision count is zero; retaining the check makes the invariant explicit and protects future extensions.

## Materialised Table And Integration Boundary

The output table contains exactly `identity_namespace text NOT NULL`, `canonical_identifier text COLLATE "C" NOT NULL` and `source_membership_count integer NOT NULL CHECK (source_membership_count >= 1)`, with a named unique constraint over namespace and canonical identifier. It contains one row per valid canonical identifier and no row-level membership table. The workflow revokes all privileges from `PUBLIC` on the table and reports no analyst grant or export.

The output remains restricted and re-identifying. A later explicitly authorised workflow may review it as the single enrolment relation for `epi_sec_linkage_spec()` or another controlled registry-loading step. This function does not initialise or write the registry and does not generate pseudonyms.

## Recovery And Privacy

Timeout, advisory-lock conflict, destination conflict, blocker or database failure yields no committed destination. Ordinary results, prints, errors, tests and documentation contain only neutral metadata and aggregate counts. Native database logs remain outside package control. Recovery is to verify the absent destination, correct the source or reviewed contract, and begin again with audit.
