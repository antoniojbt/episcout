# Acceptance

Spec ID: `025-curp-validation-and-reconciliation`
Status: Completed and accepted through PR #231

Tracking issue: [#225](https://github.com/antoniojbt/episcout/issues/225); source planning issue #217 and PR #224 are complete.

## Planning Acceptance

- [x] Existing implementation and focused tests are baselined without changing package code.
- [x] Current official CURP structure, century-marker and confidentiality rules are cited.
- [x] Local structural validity is separated from official registry validation.
- [x] Proposed comparison states distinguish mismatch, missing reference and unavailable CURP fields.
- [x] Direct-identifier and derived-data boundaries are explicit.
- [x] Owner accepts the proposed audit object and one-cycle legacy compatibility boundary.
- [x] Owner selects strict rejection of lowercase and whitespace without silent normalisation.
- [x] Owner accepts the 1900–2099 local domain and the explicit inability to distinguish possible pre-1900 keys.
- [x] The catalogue published with the 2021 RENAPO assignment rules is approved for pinning.
- [x] Checksum verification is deferred as `not_verified`; issue #230 closed through owner rescope without adopting an unofficial algorithm.
- [x] Fixture provenance excludes the missing photo, government-service generation and real personal data.

## Later Implementation Acceptance

- [x] Spec status is changed from `draft` to `active` before package-code changes.
- [x] Executable tests fail against every targeted baseline defect before implementation.
- [x] Vector, date, year-domain, code, deferred-checksum and comparison contracts pass independently justified tests.
- [x] Legacy compatibility or migration behaviour is documented and tested.
- [x] Results, conditions, printing, examples and artifacts satisfy the privacy contract.
- [x] Focused/full tests, lint and the canonical local package check pass.
- [x] Required pull-request CI passes.
- [x] Review evidence records sources, exact versions, limitations and unresolved registry-validation boundaries.
- [x] PR #231 merged to canonical `master` as `7e42f228969dbc62060f0660c43119882140052f`; issue #225 closed automatically.
- [x] Post-merge closeout reconciles issue #225, roadmap #227, this acceptance record, TODOs, changelog and approved next tracker #226.
