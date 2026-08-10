# Review Notes

Spec ID: `032-eda-denominator-gap-assessment`
Status: Active design assessment

## Findings

- The canonical categorical calculation already exposes both total-row and observed-value proportions; the stratified calculation adds explicit group identity, group sizes and input/included/omitted population counts.
- Table 1 is traceable through `group_n` and `denominator`, but its categorical basis is a fixed compatibility rule: observed denominator for ordinary levels and total group rows for the missing level.
- Frequency plot companions contain counts and can collapse levels deterministically, but the companion row does not retain denominator, proportion or basis. Plot inventory has the relevant counts separately.
- Both report families already consume aggregate artifacts. The #245 PostgreSQL renderer operates after the snapshot and therefore needs no new database access for richer presentation metadata.
- A separate not-applicable meaning cannot be recovered from `missing_codes`; adding inference or a new semantic taxonomy would conflict with the lean dictionary boundary from spec 030.

## Recommendation

Use one shared aggregate categorical display calculation in successor #253 and make existing consumers use it. Keep current calculation schemas and default presentations compatible. Direct bar annotations should remain optional; a clearly linked companion table/data artifact is sufficient when labels would be crowded.

## Stack And Lifecycle

- Assessment base: fully green #245 implementation/evidence head `a3136a3` in draft PR #252.
- Issue #253 was created before this design contribution can close #248.
- This planning branch must not merge before PR #252 and its post-merge closeout.
- Pull request, checks and merge commit: pending.
