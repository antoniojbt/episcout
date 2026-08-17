# Technical design

Cell states are computed per valid identifier × reviewed time: zero, one or more than one distinct non-missing canonical value produce missing, usable and conflicting states. Conflict precedes missing for numeric-change exclusions. Aggregate counts use exact base-R doubles through `2^53 - 1`; proportions carry their explicit numerator and denominator and are unavailable at zero denominators.

Numeric changes compare first/last distinct present occasions and adjacent declared occasions. Ordinary summaries call the existing stratified contract rather than duplicating summary logic.

Valid-panel presence uses only non-missing/non-blank IDs and non-missing declared times. Structural duplicate counts use rows; histories use at least one row per ID-time. Complete entities are present at every declared time. A gap is an absent declared interior occasion between first and last presence. Adjacent membership reconciles retained plus absent-previous/current-present to the current population.

For a present variable cell, zero distinct non-missing values is missing, one is usable (including identical repeats mixed with missing rows), and more than one is conflicting. Absence is not missingness. Complete among present requires usable state at all present occasions. Interior variable missing requires earlier and later usable cells and a present missing cell between them; absence and conflict do not count.

First-to-last never compares an entity observed at one occasion with itself. Adjacent comparisons use consecutive declared occasions and require presence at both. Eligibility requires two usable finite values; signed delta is right minus left. Conflict precedes missing and nonfinite is evaluated after usable. The presence/single/present-both and eligible/exclusion counts must reconcile exactly. Delta summaries use base-R mean, sample SD and type-7 quartiles; they return typed NA values at zero eligibility.

PostgreSQL implementations may group by ID only within SQL CTEs. They return bounded aggregate tables, parse all custom counts through `longitudinal_qc_checked_count()`, use canonical missing/value expressions and share one transaction. Data-frame and PostgreSQL custom outputs must agree exactly for the neutral truth fixture. Per-backend `summaries` must be identical to the corresponding public stratified call; cross-backend comparison permits only the documented PostgreSQL Shapiro NA.

Zero-row data with valid schema and time declaration succeeds. Unexpected time, incompatible input, non-exact count, source/catalogue change, SQL failure or reconciliation failure returns no partial object. A real PostgreSQL failure test must prove rollback and source reuse; a concurrent mutation test must prove one stable snapshot; privacy canaries must be absent from query outputs, returned objects, warnings and errors.
