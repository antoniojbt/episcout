# Acceptance

Spec ID: `020-data-frame-writer-delimiter-contract`
Status: Implemented

- [x] CSV filenames contain comma-separated bytes and TSV filenames contain
  tab-separated bytes.
- [x] Existing TSV use is unchanged.
- [x] The CSV correction and compatibility impact are explicit in NEWS and help.
- [x] Unsupported suffixes and suffix/separator contradictions fail clearly.
- [x] Help documents directory, overwrite, delimiter, quoting and missing-value
  behaviour.
- [x] Focused tests cover raw bytes, zero rows, overwrite and validation.
- [x] Focused tests, full tests, lint, local checks and `git diff --check` pass or
  unrelated limitations are recorded.
- [x] Planning, review and changelog records reconcile issue #198 and roadmap
  issue #204 without starting item 2.
- [ ] Pull-request CI passes and the owner accepts and merges the implementation.
