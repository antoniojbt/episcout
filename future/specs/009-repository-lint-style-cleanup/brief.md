# Brief

Spec ID: `009-repository-lint-style-cleanup`  
Status: Implemented  
Owner: Antonio Berlanga-Taylor  

## Problem

The configured repository lint policy reports 163 genuine findings after the package is loaded. Contributor documentation also recommends an unloaded-package lint command that produces 156 additional cross-file false positives, while the default pipe linter conflicts with the package's established `%>%` policy and current R compatibility floor.

## Goal

Reach zero package-loaded lint findings with a targeted 33-file cleanup, preserve public APIs and R compatibility, and enforce the corrected lint command locally and in CI.

## Non-goals

- A full 146-file styler rewrite.
- Public API renaming or deprecation.
- Enabling line-length linting or hard-wrapping prose.
- Adding `lintr` as a package dependency or package test dependency.

## Risks

- Mechanical formatting can obscure existing uncommitted spec 008 changes.
- Native-pipe conversion would silently raise the minimum R version.
- Renaming historical exported objects, dotted arguments or output columns would break compatibility.
