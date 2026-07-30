# Software Verification Checklist

Apply this checklist before handing off R code, scripts, pipelines or package-interface changes.

## Blocking checks

- [ ] State the required observable behaviour and its basis; do not infer correctness solely from existing code or tests.
- [ ] Apply `truth-and-semantics.md` when the change interprets data or domain rules.
- [ ] Validate required inputs, failure behaviour, side effects, output locations and important boundaries.
- [ ] Test changed user-visible behaviour and regressions using independently justified expectations.
- [ ] Run the changed code through a realistic invocation and inspect its actual return value or output.
- [ ] Run targeted tests first and the broader relevant suite afterward; record failures and checks that could not run.
- [ ] Confirm that compatibility behaviour protects a released or demonstrated contract rather than unexplained historical behaviour.

## Quality checks

- [ ] Keep the implementation proportionate to actual reuse.
- [ ] Check deterministic behaviour, randomness, dependencies, configuration, platform assumptions, concurrency and cleanup where relevant.
- [ ] Review errors, warnings, logging, privacy, security and resource use in proportion to risk.
- [ ] Confirm that roxygen documentation, README examples, schemas, manifests and commands agree with observed behaviour.

## Evidence to report

Report the behaviour verified, the independent basis for expected results, commands and outcomes, inspected outputs, unresolved failures and relevant checks not run.
