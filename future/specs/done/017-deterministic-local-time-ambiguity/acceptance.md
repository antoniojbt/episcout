# Acceptance

Spec ID: `017-deterministic-local-time-ambiguity`  
Status: Completed  

- [x] The owner-approved deterministic historical-time contract, R compatibility change and dependency choice are recorded before package-code changes.
- [x] The IANA transition and prior cross-platform failure establish correctness independently of production code.
- [x] Local timezone classification and conversion use bundled tzdb data and no host timezone sampling.
- [x] Ambiguous, nonexistent, unsupported and unclassifiable local times block without observed values.
- [x] Unique historical local times, fractional seconds and offset-bearing inputs preserve their independently justified instants.
- [x] The public function and result/audit schemas remain unchanged.
- [x] Documentation and generated help state the engine and safe blocking behaviour.
- [x] Focused tests, full tests, lint, local/CRAN checks and `git diff --check` pass or unrelated limitations are recorded.
- [ ] GitHub's macOS and Ubuntu jobs return the same status for the historical fixture; publication was outside this local implementation, so this check remains for the eventual pull request.
- [x] Software verification, truth/semantics and copy-edit review have no unresolved blocker.
- [x] Issues #190 and #81 are linked in the completed review record.
