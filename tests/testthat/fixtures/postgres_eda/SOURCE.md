# PostgreSQL EDA neutral fixture provenance

The live fixture in `test-eda-postgres-parity.R` is synthetic and is created only in a disposable test schema. It contains no representative, personal, institutional, credential, or project-derived values.

Discrete expected counts are literal hand counts over the six declared rows. The numeric quartiles for finite observed values `1, 2` use R's documented type-7 positions and are `1.25`, `1.5`, and `1.75`. Identifier QA is counted from the neutral multiset `x, x, y, missing, z, z`: five observed values, three distinct values, two repeated distinct values, duplicate excess two, and maximum frequency two. Unicode length expectations use Unicode characters, not encoded bytes.

The production package does not generate this provenance note or its expectations. PostgreSQL relation rows are not committed as a dump.
