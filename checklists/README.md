# Package Review Checklists

Use these checklists during work and before handoff. They are routed by `AGENTS.md`, support self-review and do not constitute independent review.

## End-goal gate

Before applying specialist checks, compare the planned and actual deliverable with the original stated end goal and the applicable instruction hierarchy. A deliverable does not pass merely because its specification, implementation, tests, snapshots, sources, or workflow agree internally. Record any unauthorised change, narrowing, deferral, or substitution of the end goal as `fail`; do not convert it to `not applicable` or rationalise it as package behaviour.

## Common workflow

1. Define the stated end goal, its required deliverables, and the intended software behaviour, analytical answer or communication purpose without substituting an easier proxy.
2. Identify authoritative specifications, domain rules, benchmarks or independently derived expected results.
3. Resolve material ambiguity before implementation. Mark an unresolved item as `not verified` and stop when the choice could change the result materially.
4. Implement or draft the deliverable.
5. Apply the relevant checklists and inspect actual outputs.
6. Report evidence, failures, unresolved uncertainty and limitations at handoff.

For ordinary work, report the applicable checks and material exceptions in the handoff or pull request. For high-stakes, unusually consequential or explicitly audited work, save a review record with the deliverable and state whether the review was independent.

Use `statistical-test-audit.md` only for an explicitly requested, bounded read-only assessment of tests as independent correctness evidence. It defaults to 2–4 selected functions and prohibits test execution or code changes unless separately authorised.
