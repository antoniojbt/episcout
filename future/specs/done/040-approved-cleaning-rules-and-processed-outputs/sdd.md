# Software Design

Spec ID: `040-approved-cleaning-rules-and-processed-outputs`

Status: Active

## Authority Boundary

An approved rule is a caller-authored instruction, not an inference from observations or semantic metadata. `epi_eda_qc_proposals()` remains read-only: its `evidence_state = "descriptive"`, `proposal_state = "pending"`, candidate fields and screening bounds are never executable. Descriptive dictionary `min`, `max`, `levels` and `missing_codes` also remain outside this contract.

The package validates structure and supported operations but does not authenticate a person or decide whether an approval is scientifically justified. Each rule therefore carries a caller-managed opaque approval reference that links to an external reviewed record without placing a person's identity or review text in returned audit output.

## Public Interfaces

```r
epi_eda_approved_rules(rules)

epi_eda_apply_cleaning_rules(
  data,
  rules,
  variable_keys,
  output_path = NULL,
  output_format = NULL,
  destination_schema = NULL,
  destination_table = NULL
)
```

`epi_eda_approved_rules()` validates, canonicalises and sorts a non-empty rule data frame and returns class `c("epi_eda_approved_rules", "data.frame")`. Application requires that class and rebuilds it to detect later mutation. `variable_keys` is the caller-owned `name`/`variable_key` crosswalk introduced by spec-039; it may contain unruled variables, but every rule key must resolve exactly once and names and keys must both be unique.

Data-frame application returns class `c("epi_eda_cleaning_result", "list")` with `data` equal to the processed data frame and an aggregate-only `audit`. PostgreSQL application returns the same result class with `data = NULL` because observations remain server-side.

## Exact Approved-Rule Schema

The input contains exactly these columns in this order:

| Column | Type | Contract |
| --- | --- | --- |
| `variable_key` | character | Unique caller-managed opaque key matching `^var_[a-z0-9]{16,64}$`. |
| `rule_state` | character | Exactly `approved`. |
| `declared_type` | character | One of `numeric`, `integer`, `categorical` or `binary`. |
| `valid_min` | numeric | Optional finite lower bound for numeric/integer rules only. |
| `valid_max` | numeric | Optional finite upper bound for numeric/integer rules only. |
| `allowed_values` | character | Optional semicolon-delimited exact values for categorical/binary rules only. |
| `missing_codes` | character | Optional semicolon-delimited approved codes converted to missing. |
| `approval_id` | character | Opaque caller-managed reference matching `^approval_[a-z0-9]{16,64}$`. |

Blank list fields mean no operation. Tokens are trimmed, must be non-empty, unique and valid UTF-8, and cannot encode an empty string or a literal semicolon in this first schema. Numeric/integer missing codes must be finite numeric/integral values. List order is canonicalised for hashing. Each row must contain at least one executable bound, allowed-value set or missing code.

Numeric/integer rules reject `allowed_values`, non-finite bounds, a lower bound greater than an upper bound and non-integral integer bounds or missing codes. Categorical/binary rules reject bounds and reject overlap between allowed values and missing codes. A populated binary allowed set contains exactly two values. Unsupported types, pending states, extra proposal/dictionary columns, duplicate keys and malformed approval references fail before source processing.

## Data-Frame Transformation

All ruled variables must exist and use supported storage before any output column is changed. Numeric accepts base integer/double storage; integer accepts base integer storage or finite whole-number double storage after approved missing codes are excluded. Categorical and binary rules accept base character, factor, ordered factor, logical or integer storage. Semantic subclasses, matrices, lists, raw, complex, dates and datetimes are rejected for the first contract.

For each rule, standard source missingness is recorded first. A non-missing numeric/integer value transitions to a typed `NA` when it equals an approved missing code or falls below `valid_min` or above `valid_max`. A non-missing categorical/binary value transitions when it equals an approved missing code or, when an allowed set is present, is outside that set. Existing missing values remain missing; factor levels and source vector type are retained. Columns are replaced in a copied ordinary data frame, preserving source rows, row order, row names and column order.

## File Publication

`output_path` and explicit `output_format` are either both absent or both present. Supported formats are exactly `csv` and `rds`. Database destination arguments are rejected for a data-frame source. CSV requires scalar atomic columns because the format cannot preserve list/matrix columns safely.

The complete data-frame transformation and audit reconciliation finish before writing. The implementation writes a private sibling temporary file in the destination directory, validates its dimensions, then uses an atomic no-replace hard link to publish it. An existing destination or a no-replace race fails. RDS reconciliation reads the object back exactly; CSV reconciliation reads only the complete file to confirm dimensions. Any post-link reconciliation failure removes only the destination created by the current call.

## PostgreSQL Transformation

PostgreSQL requires both destination identifiers and rejects file arguments. The destination schema must already exist, and the destination relation must differ from the source and not collide with any relation. Identifiers use the existing strict plain-identifier and quoting helpers. Relation/catalogue identity, every rule/source mapping, storage compatibility, destination availability and all SQL plans are validated inside one repeatable-read write transaction before `CREATE TABLE AS` executes.

Each ruled source column is projected as an allow-listed `CASE WHEN <transition predicate> THEN NULL ELSE <quoted column> END AS <quoted column>` expression. Unruled columns are projected unchanged and in source column order. Values and bounds use bound parameters; source and destination identifiers are never interpolated without identifier quoting. Numeric/integer comparisons remain typed numeric comparisons. Categorical/binary comparisons use the same exact text representation as supported in-memory storage. No row values are collected.

Before creation, scalar server-side counts record standard missing values and non-missing values that will transition for each rule. After creation, scalar counts confirm destination missing values, destination dimensions and `after = before + transitioned` for every rule. A failed query, collision, reconciliation or commit rolls back the new table. PostgreSQL relations have no contractual physical row order; the projection neither filters nor explicitly reorders source rows.

## Aggregate Audit

`audit` contains exactly `summary` and `variables`. `summary` is one row with `rule_set_sha256`, `publication`, source and destination row/column counts, total ruled-variable missing counts before and after, total transitioned count, and logical dimension, transition and publication reconciliation fields. `variables` has one canonical rule-order row per opaque key with `n`, missing before/after, transitioned count and a reconciliation flag.

The SHA-256 hash covers the canonical exact approved-rule data frame, including opaque key and approval reference but not the private key/name crosswalk or source values. The returned audit contains no names, paths, schemas, relations, allowed/disallowed values, bounds, missing codes or approval references. Custom print and structure methods report only aggregate row/rule counts and publication type.

## Determinism And Compatibility

Equivalent canonical rules and unchanged source observations yield the same rule hash, processed values and audit counts; publication type is necessarily backend-specific. Existing APIs and semantic dictionary schemas do not change. CSV intentionally cannot preserve R column classes; the returned processed data and RDS output do.
