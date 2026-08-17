# Brief

Spec ID: 050
Status: Completed
Owner: MacBotticus
Tracking issue: #348

## Problem

Population continuity and marginal distribution drift do not describe how retained entities move between caller-declared states. Downstream projects need a generic, aggregate-only transition calculation without project meanings, identity leakage or implicit denominator changes.

## Goal

Add epi_eda_longitudinal_transitions(sources, entity_id, spec, variables, max_levels = 50L) for adjacent-period categorical or binary state transitions. It must audit entity-period states, separate missing and conflicting exclusions, emit complete bounded matrices with explicit eligible denominators, and remain inside one read-only PostgreSQL snapshot.

## Non-goals

- Entry, exit, retention and gaps, which remain in #346.
- Marginal schema, missingness and distribution drift, which remain in #347.
- Cross-variable transitions, cleaning, thresholds, scientific labels or project meanings.
- Downstream project, identity-field, position or period semantics.

## Frozen Authority

The controlling contract is issue #348, including Frozen implementation decisions — 2026-08-17, Contract clarification after behaviour audit — 2026-08-17, and the later v1 operational cap of 50 states. sdd.md restates that contract as implementation decisions; tdd.md and acceptance.md define independent evidence.

## Successor

Issue #349 remains gated on canonical implementation and closeout of this specification.
