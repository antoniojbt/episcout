# Brief

Spec ID: `055-pseudonymisation-consolidation`
Status: Active

## Problem

Episcout already owns the neutral PostgreSQL registry and multi-table pseudonymisation boundary, but maintained consumer evidence exposes four missing safety capabilities: explicit legacy-compatible identifier preparation, preservation of existing token assignments, bounded token allocation, and privilege/preflight evidence equivalent to apply. Without those capabilities a downstream tool cannot retire its overlapping implementation safely.

## Objective

Deliver a compatible Episcout 0.6.0 replacement through four reviewable dependent slices, then support consumer migration without absorbing project orchestration.

## Non-goals

- No permanent CLI, workflow engine, scheduler, backup manager or project configuration format.
- No credentials, role grants, role selection, disclosure decision or anonymisation claim.
- No private consumer names, paths, schemas, identifiers or operational values in this public repository.
- No removal or warning for currently released calls whose arguments remain valid.
