# Software Design

Spec ID: 050
Status: Active

## Frozen Decisions

1. Public API: epi_eda_longitudinal_transitions(sources, entity_id, spec, variables, max_levels = 50L). Sources follow #346/#347 and share one open connection. variables is an explicit unique non-empty selection; NULL is invalid.

2. Privacy guard: before database work, reject selection of entity_id and any specification row whose trimmed lower-case role is id or identifier. No entity values, row histories or unbounded state values may enter results, messages or logs.

3. Supported state: only declared categorical and binary variables are available. Presence, compatibility and missing-code validity reuse canonical PostgreSQL helpers. State equality uses eda_postgres_value_expression(), then text comparison, grouping, joining and ordering under PostgreSQL COLLATE "C".

4. Entity-period classification: among valid entities, zero distinct non-missing canonical values is missing, one is usable and more than one is conflicting. Repeated identical values and one value mixed with missing rows remain usable. A conflict is never resolved silently.

5. Eligibility: only adjacent periods are compared. n_retained is the valid distinct-entity intersection. Conflict exclusion takes precedence over missing exclusion. Both-side usable entities are eligible and define the denominator. Every available pair satisfies n_retained = n_eligible + n_excluded_missing + n_excluded_conflict. Entry and exit never become states.

6. Result envelope: class c("epi_eda_longitudinal_transitions", "list"); components in order are metadata, state_audit, transition_summary, transition_counts, issues. Empty components retain exact typed columns.

7. Exact schemas:
   - state_audit: period_index, period, variable_index, variable, n_valid_entities, n_usable_state, n_missing_state, n_conflicting_state, status, reason.
   - transition_summary: left_period_index, left_period, right_period_index, right_period, variable_index, variable, n_retained, n_eligible, n_excluded_missing, n_excluded_conflict, n_transition_cells, eligible_denominator, status, reason.
   - transition_counts: pair and variable keys, from_state, to_state, n, eligible_denominator, proportion, from_is_declared, to_is_declared, from_is_unexpected, to_is_unexpected, status, reason.
   - issues: issue_code, severity, period scope, pair scope, variable_index, variable, n_affected, message, exactly as frozen in #348.

8. Unavailable evidence: state_audit always has period by variable rows and retains n_valid_entities; unavailable state counts are NA_real_. transition_summary always has pair by variable rows and retains n_retained; unavailable state counts and denominator are NA_real_, n_transition_cells is exactly 0L, and no count rows are emitted. Left-side reason precedes right-side reason.

9. Domain and matrix: declared levels are first-occurrence deduplicated and retain declaration order. Unexpected usable states follow in bytewise order. Missing is never a state. An available pair emits the full square domain including zeros. n_transition_cells is emitted domain squared. A zero eligible denominator retains declared-domain zero cells with NA_real_ proportions and explicit unavailable status.

10. Hard bounds: v1 accepts max_levels only from 1 through 50. Declared, period-observed and adjacent-union domains each receive a PostgreSQL-side preflight limited to max_levels + 1 values before labels are retained for result assembly. Excess is a privacy-safe hard error with no partial object. The full matrix is at most 2,500 cells per pair and variable.

11. Types and exactness: indices, max_levels and n_transition_cells are R integers. PostgreSQL population, state, transition and affected counts are exact decimal text converted through the #346 exact-double contract, accepting only 0 through 2^53 - 1. Denominators and proportions are doubles. Reconciliation failure is hard.

12. Issues: period unavailability precedes period conflict warnings, which precede pair zero-denominator warnings. Stable codes, severity and scoping follow #348. Bound excess is never an issue row.

13. Metadata: exact columns are contract_version, n_periods, n_spec_variables, n_variables, period_labels, source_fingerprints, source_set_fingerprint_sha256, specification_fingerprint_sha256, selected_specification_fingerprint_sha256, resolved_variables, entity_id, max_levels, count_contract, count_maximum, snapshot_mode. Values include longitudinal-transitions-1, exact-base-r-double, 9007199254740991 and REPEATABLE READ READ ONLY.

14. Transaction and failure: all validation, domain preflights, audits and aggregation occur in one read-only repeatable-read transaction. The operation writes no database object, returns no partial object on failure, sanitises database errors and leaves the caller connection reusable.

## Internal Design

Reuse the canonical multi-source transaction, #346 entity predicate/equality/count parser, and #347 compatibility, missingness, state-expression, declaration and fingerprint helpers. Add transition-specific aggregate SQL only. Preflight each adjacent union in PostgreSQL before matrix assembly; never collect entity-state rows.
