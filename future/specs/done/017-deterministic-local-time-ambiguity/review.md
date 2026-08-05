# Review Notes

Spec ID: `017-deterministic-local-time-ambiguity`  
Status: Completed  

## Root Cause And Sources

The old validator discovered candidate UTC offsets by formatting sampled UTC instants through the host timezone database. The local macOS database did not expose the pre-1970 Kwajalein rollback, so only one candidate was constructed. IANA tzdb 2026c `australasia` lines 468-474 independently define UTC+11 until October 1969 and UTC-12 afterwards. A disposable `clock` 0.7.4 probe using tzdb 2025a classified the fixture as `ambiguous`, the neighbouring wall time as `unique`, and the existing New York cases as `nonexistent` and `ambiguous`.

## Findings

No unresolved implementation finding. The public formals, audit columns, preparation statuses, missingness contract, offset-bearing parser and all-or-nothing behaviour remain unchanged. The only compatibility change is the accepted R 4.0 minimum and compiled `clock` dependency.

## Verification Evidence

- Focused `eda-prepare` tests passed, including the historical Kwajalein fold, neighbouring unique instant, 1993 gap, modern New York fold/gap, offsets, fractions, mixed inputs, missingness, timezone validation, process-`TZ` invariance and value-free blocking.
- Package-loaded lint completed with no findings.
- The full test suite passed with four expected environment-dependent skips.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. Its first sandboxed run could not open the multicore server socket; the same check passed outside that restricted environment.
- `scripts/check-cran.sh` completed with 0 errors, 0 warnings and 2 unrelated notes: the inherited incoming-feasibility note and an outdated local HTML Tidy validation note.
- `git diff --check` passed.
- Verification used R 4.5.3 on aarch64 macOS Tahoe 26.5.2 with `clock` 0.7.4 and bundled tzdb 2025a. No machine-specific path is part of package behaviour or documentation.
- GitHub macOS and Ubuntu checks were not run because no commit, push or pull request was authorised. The existing workflow matrix is unchanged; cross-platform confirmation remains an explicit pull-request check.

## Closeout Notes

The implementation closes the deterministic historical-time defect described by GitHub issue #190 and the remaining portability concern from issue #81. The software-verification checklist found direct behavioural tests at public boundaries and no hidden global-state mutation. The truth-and-semantics review confirmed that IANA transition evidence, not the production conversion path, establishes the expected fold/gap results and that missingness is unchanged. The copy-edit review found the help, NEWS and compatibility wording consistent and value-free. Draft spec 003 and the concurrent backend-planning commit remain untouched. Legitimate future IANA corrections may alter historical results when the installed `clock`/tzdb version changes; verification should therefore continue to record both versions.
