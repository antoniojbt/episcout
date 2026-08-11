# Acceptance

Spec ID: `034-retained-epi-sec-technical-contract`
Status: Completed

- [x] All seven retained exports have an explicit intended argument, result and compatibility contract.
- [x] Every source restriction/output family and all transitive helpers are classified as structural, data-integrity, caller-selected or package governance behaviour.
- [x] Confirmation, privacy classification, package authorisation, disclosure and publication authority are absent from the proposed contract.
- [x] The package does not infer PII or sensitivity from column names, values, types or patterns.
- [x] PostgreSQL grants are server outcomes; package `PUBLIC` refusal and privilege mutation are removed.
- [x] Exact mapping, type/collation, cryptographic randomness, token uniqueness, registry structure, destination collision, duplicate reconciliation, transaction, lock, rollback and row-count protections remain.
- [x] Audit is optional technical inspection and direct apply/materialise repeats checks in its protected transaction.
- [x] Neutral status, issue severity, rollback-condition and diagnostic-value schemas replace policy `blocked`, `blocking`, governance and redaction semantics.
- [x] Released, development-only, compatible and intentionally breaking surfaces are distinguished, with bounded syntax-only adapters.
- [x] The implementation sequence contains three coherent behavioural slices plus the later #269 documentation reconciliation.
- [x] The first implementation issue is fully drafted with scope, exact contract, tests, compatibility, checks and exclusions.
- [x] #274, #275, #269 and #249 reconciliation is explicit without rewriting the longitudinal guide or claiming future behaviour is current.
- [x] No package code, tests, generated help, NAMESPACE, vignette, example, dependency or database was changed.
- [x] First implementation issue #278 is created and recorded as `successor_issue`; implementation has not begun and it remains non-dispatchable until this closeout is canonical.
- [x] Design PR #277 final head `0d74c1b80bad6a8a9a6ff064cdadcd517ecac854` passed macOS and Ubuntu R CMD CHECK, PostgreSQL integration, test coverage, both Codecov gates, CodeFactor and CodeQL with no unresolved finding.
- [x] PR #277 merged to canonical `master` as `8641abe9aa89fb5c1c3ecba19c16985618a3a38e`, GitHub closed issue #276, and roadmap/TODO/changelog closeout records identify #278 as the unstarted successor.
