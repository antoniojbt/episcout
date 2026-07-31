# Review Notes

Spec ID: `010-canonical-eda-summary-contract`  
Status: Draft  

## Planning Findings

- The `summary_version` interface and v1/v2 terminology were added after release `0.2.0`, while the older two-table output itself was released. The user has explicitly stated that external compatibility does not matter, so the implementation may replace the released return shape without a legacy adapter.
- Current tests establish that the default equals explicit `v1`, but they do not establish exact historical values for important edge cases.
- The current typed path excludes infinities from numeric calculations and records them separately, but reports `sum = 0` when no finite values exist. This conflicts with the repository requirement not to turn all-missing analytical inputs into observed zeroes.
- The current typed path uses the dataset row count as `n` for an absent variable, although no source vector exists for that variable.
- Current categorical core behaviour can treat unused factor levels as declared even when the EDA specification does not declare them. The canonical contract instead treats specification levels as authoritative.

## Open Questions

None blocking planning. Any new ambiguity that materially changes statistical meaning must be recorded and resolved before production code is changed.

## Implementation Review

Not started. This specification currently authorises planning only.

## Closeout Notes

Record implementation findings, independent calculations, commands, rendered-output inspection and unresolved limitations here before marking the specification implemented.
