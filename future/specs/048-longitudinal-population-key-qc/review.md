# Review Notes

Spec ID: `048`
Status: Active

## Findings

No change-attributable correctness, privacy, compatibility or documentation finding remains after self-review.

The implementation keeps caller order semantic only for the new API, accepts exactly the five persistent relation kinds through the unchanged PostgreSQL source boundary, revalidates every source inside one read-only repeatable-read transaction and fetches only bounded aggregate rows. PostgreSQL counts are selected as decimal text, guarded through `2^53 - 1` and reconciled before the result is assembled. The existing integer count helper and every predecessor API are unchanged.

The neutral integration fixture states membership sets and expected period, adjacent, all-pairs, history, simple-key and composite-key results independently of production SQL. It also checks empty periods, zero denominators, nulls/blanks, all supported relation kinds, quoted identifiers, deterministic repetition, catalogue drift, mixed connections, no database writes and value leakage. Runtime-generated identifier and key values are not retained in source fixtures or expected output.

Local unit tests and changed-file lint passed. The live PostgreSQL filter ran against disposable local PostgreSQL/PostGIS 17 containers and passed all expectations, including the hand-derived four-period fixture, supported relation kinds, a controlled concurrent write proving the stable snapshot, forced-failure rollback/connection reuse and value-free failures. Each container was removed after testing. After independent review identified missing null/blank separation, union/numerator fields, an incorrect `issues` schema and unsafe derived-union arithmetic in the first draft, those contracts and their tests were corrected before publication. Union counts now remain decimal text until the same exact-range guard used by every other returned count.

`scripts/check-local.sh` then completed with zero errors and zero warnings. Its package-wide test, build, installation, examples, documentation, vignette and `R CMD check` stages passed. The only final check notes were the tracked top-level `docs` directory and an environmental inability to verify current time; neither is introduced by this change. PostgreSQL tests are deliberately skipped by the default local suite and were run separately as described above.

Software-verification review covered inputs, result schemas, failure/rollback boundaries, deterministic ordering, query bounds, generated help and the realistic gated invocation. Truth/semantics review traced missingness, membership, uniqueness, entry/exit, gap and denominator rules to the frozen audit contract and hand-derived set arithmetic. Copy-edit review covered the function help, README, NEWS, project map and longitudinal vignette in British English.

## Open Questions

None in the frozen contract. Hosted PostgreSQL and cross-platform checks remain required PR integration evidence.

## Closeout Notes

- Pull request: #350 is open for required review; merge commit remains pending.
- Required checks and material exceptions: focused unit tests, live PostgreSQL integration and the broader local check passed; hosted checks remain pending publication.
- Tracking issue and roadmap disposition: unchanged.
- Successor issue or terminal reason: pending the live contribution lifecycle.
