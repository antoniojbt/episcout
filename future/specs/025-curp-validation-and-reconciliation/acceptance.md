# Acceptance

Spec ID: `025-curp-validation-and-reconciliation`
Status: Draft for owner review

## Planning Acceptance

- [x] Existing implementation and focused tests are baselined without changing package code.
- [x] Current official CURP structure, century-marker and confidentiality rules are cited.
- [x] Local structural validity is separated from official registry validation.
- [x] Proposed comparison states distinguish mismatch, missing reference and unavailable CURP fields.
- [x] Direct-identifier and derived-data boundaries are explicit.
- [ ] Owner accepts the proposed audit object and legacy compatibility boundary.
- [ ] Owner selects the lowercase/normalisation policy.
- [ ] Owner accepts a supported year domain and explicit pre-1900 behavior.
- [ ] An exact official birthplace catalogue and version are approved.
- [ ] An official verification-digit algorithm or sufficient official test vectors are obtained.
- [ ] Fixture provenance is approved without using the missing photo or real personal data.

## Later Implementation Acceptance

- [ ] Spec status is changed from `draft` to `active` before package-code changes.
- [ ] Executable tests fail against every targeted baseline defect before implementation.
- [ ] Vector, date, year-domain, code, verification-digit and comparison contracts pass independently justified tests.
- [ ] Legacy compatibility or migration behavior is documented and tested.
- [ ] Results, conditions, printing, examples and artifacts satisfy the privacy contract.
- [ ] Focused/full tests, lint, local checks and required CI pass.
- [ ] Review evidence records sources, exact versions, limitations and unresolved registry-validation boundaries.
