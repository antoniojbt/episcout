# Analysis and Statistics Checklist

Apply this checklist to analytical functions, metrics, statistical summaries, tables, models, derived data and analytical conclusions. Apply `truth-and-semantics.md` first.

## Blocking checks

- [ ] Confirm that the analytical endpoint is the one the user requested, not a proxy selected because it fits available data, a package method, pipeline, or internally consistent outputs. Mark any necessary change, narrowing, deferral, or substitution explicitly.
- [ ] Define the question, target and source populations, unit of observation, time period, analysis population and required measure or estimand where applicable.
- [ ] Identify source fields, provenance, versions, units, coding, inclusion criteria, exclusions and transformations.
- [ ] Define handling of missingness, zeroes, censoring, competing events, suppression, duplicates and unmatched keys.
- [ ] Verify missingness preservation through consequential transformations in both directions and check partial composites and all-missing aggregates explicitly.
- [ ] Reconcile important row counts, totals, joins, dropped records, denominators and derived values against source inputs.
- [ ] Confirm that the statistical or computational method answers the stated question and that material assumptions are plausible or explicitly limited.
- [ ] Verify important calculations with independently derived expected values or an independent method when practical.
- [ ] Ensure reported tables, values and conclusions come from the verified current analysis rather than manual transcription or stale output.

## Quality checks

- [ ] Examine important edge cases, influential observations, convergence or numerical warnings and sensitivity to consequential analytical choices.
- [ ] Report uncertainty, numerators and denominators, time periods, grouping units, exclusions and missingness when they affect interpretation.
- [ ] Distinguish descriptive, associational, predictive and causal conclusions.
- [ ] Record software versions, random seeds, parameters and commands when needed for reproduction.
- [ ] Apply `figures.md` to analytical visualisations and inspect the data underlying each figure.

## Evidence to report

Report reconciliations, independent calculations, commands, warnings, sensitivity findings, material exclusions and unresolved limitations.
