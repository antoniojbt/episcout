# Brief

Spec ID: `014-stratified-descriptive-summaries`
Status: Completed

## Problem

Canonical EDA provides transparent overall summaries but cannot compare a cohort across a reviewed treatment, site, exposure or other categorical grouping. Analysts otherwise build ad hoc grouped tables whose denominators, missingness and temporal conventions may diverge from the canonical result.

## Goal

Add one specification-aware grouped calculation and one pure Table 1 presentation. Both preserve declared empty groups and levels, identify unexpected and missing groups, account for every included or omitted row, expose numeric denominators, and reuse canonical statistical cores.

## User Outcome

A prepared dataset can be summarized overall and by exactly one reviewed categorical/binary variable. Machine-readable long tables reconcile exactly; a separate plain-data-frame Table 1 is review-friendly and traceable without p-values or hidden suppression.

## Non-goals

P-values, standardized differences, weights, survey/matched/clustered/repeated designs, survival analysis, multivariable strata, automatic small-cell suppression, raw text examples, file export, HTML widgets, data preparation, orchestration, tags and releases.

## Risks

Weighted display values can disagree with canonical Overall; missing-stratum omission can break row accounting; zero cells can disappear; percentages can hide denominators; local character datetimes can acquire machine timezone semantics; presentation can imply inference or disclosure safety.

## Approval

The owner explicitly instructed implementation of issue #183 and dependent work on 2026-08-03. This completed planning contract is active without further confirmation unless evidence changes the scientific or privacy semantics.
