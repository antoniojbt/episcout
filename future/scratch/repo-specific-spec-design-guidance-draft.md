# Repository-Specific Specification Design Guidance

Status: Draft for owner review; not an active instruction
Created: 2026-08-04

## Purpose And Authority

This draft extracts useful specification-design practices from the environment-level `ds-pipeline-designer` skill into repository-visible language adapted to `episcout`. It exists so the repository owner can inspect, revise or reject the practices before they influence future work.

This file does not modify `AGENTS.md`, activate a specification, authorise package changes or override any repository instruction. Until the owner explicitly approves and incorporates it into an authoritative repository file, agents must treat it only as a queued scratch proposal.

System and runtime instructions remain outside repository control. Within the project-controlled layer, if this draft conflicts with the user request, root or project `AGENTS.md`, an approved numbered specification, repository checklists, current package contracts or official source requirements, those authorities take precedence. The conflict must be reported rather than silently reconciled.

## Provenance And Portability

The source skill is outside this repository at `$CODEX_HOME/skills/ds-pipeline-designer/`. Its principal instructions are in `SKILL.md`, with additional material under `references/` and `assets/templates/`. Environment-level skills can be hidden from the repository owner, can change independently of repository history and are not a suitable source of project authority. The current runtime may require a listed skill when a task matches its description; this repository cannot disable that higher-level trigger. It can require transparent disclosure, project-specific reconciliation and repository-visible review of every material effect where runtime instructions permit.

Only the generally useful, repository-compatible ideas are retained below. This draft deliberately excludes the skill's Python execution profile, foreign artifact names, scaffolding command, generic library suggestions and assumptions about a separate builder system.

## When A Numbered Specification Is Required

Follow `future/TODOs.md` and `AGENTS.md`. Use the repository SDD-TDD process for multi-component work, migrations, uncertain architecture, consequential scientific/data semantics, operations that may fail partway or changes that could compromise established interfaces. Simple, low-risk tasks remain simple and should not acquire a speculative pipeline design.

Keep only one numbered implementation specification active at a time. A scratch proposal is not an active specification and never authorises package code.

## Repository Artifact Contract

Use the existing numbered directory under `future/specs/` and its established files:

- `brief.md`: problem, user need, observable goal, scope, non-goals, risks and success measures.
- `sdd.md`: public and internal contracts, data flow or stages where useful, failure behaviour, compatibility, privacy, dependencies and stop conditions.
- `tdd.md`: independently justified expected behaviour, fixtures, edge/failure tests, verification order and evidence requirements; executable tests remain under `tests/testthat/`.
- `acceptance.md`: reviewable completion conditions mapped to evidence.
- `manifest.yml`: status, owner, branch, dependencies, related files, checklist routing, commands and recorded baseline.
- `review.md`: findings, assumptions, open questions, baseline, implementation checkpoints, verification evidence and closeout.

Do not create parallel generic files such as `PIPELINE_DESIGN_SPEC.md`, `BUILDER_CHECKLIST.md`, `DECISIONS.md` or `ISSUES.md` unless the owner explicitly changes the repository convention. Record cross-spec decisions in `future/decisions.md` and task priority/status in `future/TODOs.md`.

## Minimum Design Intake

Before drafting, inspect the current implementation, callers, tests, documentation, configuration, relevant completed specs and repository decisions. Establish the following from repository evidence or the user:

1. The problem and user-visible outcome in plain language.
2. Inputs, outputs, users/consumers and affected public interfaces.
3. Measurable success and acceptance criteria.
4. Data sensitivity, provenance, domain/statistical meaning and governance constraints.
5. Compatibility requirements and demonstrated consumers.
6. Reproducibility requirements, including randomness, environment and input/version evidence.
7. Explicit scope and non-goals.
8. Relevant protocols, official sources and repository checklists.

Do not ask the user to repeat information already established in the repository. If a missing choice could materially change scientific meaning, privacy, safety, compatibility or architecture, stop and ask. Otherwise choose the simplest reversible assumption, label its impact and record it in the spec.

## Design-Only Boundary

While revising or reviewing a specification, do not implement package code, executable tests, workflows, deployment scripts or database changes. Planning commands may inspect current behaviour and record a baseline, but must not be presented as implementation evidence.

Keep proposed architecture proportionate to demonstrated needs. Do not introduce dependencies, orchestration, caching, parallel execution, services, databases or generic abstractions for hypothetical reuse. Concrete technology already selected by the user or repository is a constraint, not a decision to reopen through generic guidance.

## Contract Design

Define only the stages or components needed to make implementation and verification unambiguous. A small ordered flow is useful when work has three or more dependent stages, meaningful state transitions or failure recovery; it is not mandatory for a local one-function change.

For each material stage or component, specify:

- Purpose and dependency on prior state.
- Inputs, including types, schemas, invariants, version/provenance and sensitivity.
- Outputs, including stable schemas, ordering, units, paths and side effects.
- Parameters, defaults, allowed values and which existing interface owns them.
- Human decisions, their timing and required authority.
- Validations with explicit pass/fail outcomes and evidence artifacts.
- Expected failure categories, cleanup/recovery and externally visible errors or statuses.
- Resource, determinism, concurrency and platform assumptions when consequential.

Do not hide a consequential semantic or compatibility change inside internal refactoring. State backend or platform differences explicitly rather than describing approximate agreement as parity.

## Truth, Statistics And Data Semantics

Apply `checklists/truth-and-semantics.md` whenever data, missingness, types, domain rules or consequential requirements are interpreted. Apply `checklists/analysis-and-statistics.md` to summaries, metrics, models, tables, derived data and analytical conclusions.

For every consequential measure or transformation, define the population or records, unit of observation, inclusion/exclusion rules, missingness, zero/all-missing behaviour, denominators, units, duplicates/unmatched keys and numerical method. Identify an authoritative official source, reviewed dictionary/protocol or independently justified expectation. Existing code, tests and fixtures show prior behaviour but are not independent proof of correctness.

Expected values must not be generated by the production implementation under test. Map important calculations to a hand calculation, authoritative benchmark, independently authored fixture or genuinely independent method. Record tolerances before observing results and explain why exact equality is or is not appropriate.

## Validation And Evidence Mapping

Every important success measure must map to at least one planned validation and evidence artifact. Every referenced protocol or standard must map to the stage/component it constrains and the evidence that demonstrates compliance, or be marked not applicable with a reason.

Use the smallest sufficient combination of:

- Contract tests for schemas, invariants, validation and failure behaviour.
- Unit tests for local transformations and edge cases.
- Integration tests for boundaries such as databases, filesystems or external formats.
- Small neutral smoke fixtures for end-to-end execution.
- Regression tests for demonstrated contracts.
- Independent statistical/data-quality checks.
- Rendered inspection for figures, vignettes, reports and other user-facing artifacts.
- Privacy/leakage, mutation, cleanup and recovery evidence where relevant.

Passing execution or agreement between two production paths is not sufficient evidence of truth. State what was independently established, what was only internally reconciled and what remains unverified.

## Reproducibility And Provenance

Specify the minimum evidence needed to reproduce or audit a run:

- Code commit or package version.
- Input version, immutable reference, fingerprint or documented external custodian.
- Reviewed specification/configuration and parameter values.
- Runtime and relevant dependency versions.
- Random seed policy, or an explicit statement that no randomness applies.
- Manual decisions with owner/rationale where consequential.
- Stage/output lineage, timestamps and evidence paths where appropriate.

Do not promise byte-identical results when timestamps, environment metadata, floating-point reduction order or filesystem checksums are intentionally variable. Define the stable analytical contract separately from operational metadata.

## Privacy, Security And Governance

Classify inputs, returned objects, logs and written artifacts according to the actual repository workflow. Keep confidential/restricted data, credentials, personal names, local paths and project-specific identifiers out of public fixtures, examples, logs and planning artifacts.

Make side effects, access controls, mutation boundaries, logging limitations and disclosure-review responsibilities explicit. Pseudonymisation, aggregation and omission of direct identifiers must not be described as anonymisation or publication safety unless a separate supported process establishes that claim.

## Ready-To-Activate Gate

A numbered specification is ready to activate only when:

- The observable outcome, scope and affected interfaces are explicit.
- Every material component has testable input/output and failure contracts.
- Scientific, statistical, missingness and privacy semantics are resolved or explicitly blocking.
- Success measures map to validations and evidence.
- Compatibility and migration effects are explicit.
- Reproducibility and provenance requirements are concrete.
- Dependencies and candidate files are proportionate and reviewed.
- Open questions are resolved or clearly non-blocking.
- Applicable repository checklists and authoritative sources are identified.
- `brief.md`, `sdd.md`, `tdd.md`, `acceptance.md`, `manifest.yml`, `review.md`, `future/TODOs.md`, `future/README.md` and `future/changelog.md` agree on status.
- Baseline verification is recorded before package-code changes.

Activation review must say whether it is a self-review or independent review. Activation authorises only the scoped TDD-first implementation; it does not establish that behaviour, correctness, privacy, performance or release readiness has already been achieved.

## Implementation Handoff

The implementation order should normally be:

1. Confirm the branch, baseline and clean scope.
2. Add independently justified failing tests for the first contract boundary.
3. Implement the smallest coherent behaviour needed to satisfy that boundary.
4. Continue stage by stage, validating important reconciliations and side effects at each checkpoint.
5. Update documentation and generated files through repository entry points.
6. Run focused checks first, then the broader relevant suite and package checks.
7. Perform any required independent reviews and resolve findings.
8. Reconcile TODO, changelog, manifest, acceptance and review at every status change and closeout.

Stop and return to owner review if implementation requires materially broader authority, a changed scientific/analytical rule, a breaking interface, weakened valid test, new persistent backend, destructive operation, disclosure claim, unapproved dependency or architecture outside the active specification.

## Practices Deliberately Not Imported

The following environment-skill practices are not adopted:

- A local Python default or Python-oriented dependency suggestions.
- The skill's scaffolding script or artifact filenames.
- A mandatory generic DAG for every task.
- Generic stage names that replace established episcout terminology.
- A blanket prohibition on concrete libraries when the repository has already selected and reviewed them.
- Generic deployment, model-training, experiment-tracking, DOI, orchestration or CI/CD requirements without a demonstrated episcout need.
- Any rule that conflicts with the repository's R wrapper, SDD-TDD files, checklists, public contracts, British English style or one-active-spec policy.

## Owner Review Questions

- Should any part of this draft be incorporated into root/project `AGENTS.md`, a checklist or the spec template, and if so where?
- Is the ready-to-activate gate proportionate, or does it duplicate existing instructions excessively?
- Where runtime instructions permit a choice, should environment-level skills require explicit owner opt-in for repository specification work?
- When runtime instructions require a non-repository skill, should agents disclose its path, relevant mismatches and intended repository-visible effects before acting?
- Which sections should be removed so the final repository instructions remain concise and predictable?
