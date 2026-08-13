# Test design description

Unit tests assert exact compact context, successful plot transformation, invalid callback inputs, callback failures, non-plot returns and unchanged analytical summaries through `epi_eda_run()`.

The gated PostgreSQL fixture asserts snapshot-compatible styled SVG production, style provenance, same-identifier replacement, identifier mismatch rejection, disabled-plot non-invocation and callback-failure cleanup before publication. Existing source-formal and summary tests protect appended public arguments.
