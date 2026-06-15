# Notes from codex checks from PRs that were not addressed

Review if still valid, if correct/useful/needed/etc.


In R/eda_synthetic.R:

> +    binary = generate_synthetic_binary(row, n),
+    date = generate_synthetic_date(row, n),
+    datetime = generate_synthetic_datetime(row, n),
+    text = generate_synthetic_text(row, n),
+    stop("Unsupported EDA specification type: ", type, call. = FALSE)
+  )
+}
+
+generate_synthetic_numeric <- function(row, n) {
+  bounds <- synthetic_numeric_bounds(row, default_min = 0, default_max = 100)
+  stats::runif(n, min = bounds[["min"]], max = bounds[["max"]])
+}
+
+generate_synthetic_integer <- function(row, n) {
+  bounds <- synthetic_numeric_bounds(row, default_min = 0, default_max = 100)
+  sample(seq.int(ceiling(bounds[["min"]]), floor(bounds[["max"]])), n, replace = TRUE)
P2 Badge Sample singleton integer ranges without expansion

When an integer variable has a single positive allowed value, e.g. min = max = 5, seq.int(...) returns the length-one numeric vector 5, and base sample(5, n, replace = TRUE) samples from 1:5 rather than from the literal value 5. This makes generated integer values fall outside the specification bounds, violating the min/max guarantee for exact-value integer fields.



In tests/testthat/test-eda_plots-fixtures.R:

> +  expect_s3_class(observed$RBC.Age.Group, "ggplot")
+  expect_s3_class(observed$Recurrence, "ggplot")
P2 Badge Assert plot dispatch for spec-coded categories

These assertions only verify that the categorical/binary fixtures return some ggplot, but the fixture columns such as RBC.Age.Group and Recurrence are read from CSV as numeric values. In that scenario, an implementation that ignores spec$type and dispatches purely from the observed column class would still pass by returning numeric histograms for these variables, so the PR would not actually drive the spec-based plot dispatch it is meant to test. Please assert the dispatched plot kind through a non-visual property, such as the expected geom class or an explicit plot-type marker, for at least one spec-coded categorical/binary variable.


In tests/testthat/test-eda_synthetic-fixtures.R:

> +    expect_true(
+      all(observed_values %in% allowed_levels),
P2 Badge Require generated values before level/range assertions

Here, and in the numeric range check below, the assertion runs after na.omit(), so a generator that returns only NA for every fixture column passes: observed_values is empty and all(logical(0)) is TRUE. Since these fixture variables are required and this PR is meant to drive the implementation, add an assertion that each generated column has at least one non-missing value (or no unexpected missingness) before checking levels/ranges.

In R/eda_summaries.R:

> +    levels_value <- categorical_spec$levels[categorical_spec$name == name]
+    levels <- strsplit(levels_value, ";", fixed = TRUE)[[1]]
P2 Badge Do not assume categorical levels are present

eda_spec() accepts a minimal valid specification with only name, label, type, and role, but this code unconditionally reads categorical_spec$levels. For any categorical/binary variable in such a spec, levels_value is NULL and strsplit() errors; with a blank levels value it builds a zero-row level vector and errors in the data.frame() below. This breaks summaries for valid data dictionaries that do not enumerate levels, so either derive observed non-missing levels here or reject missing levels during validation.



In tests/testthat/test-run_eda-fixtures.R:

> +    data = NULL,
+    spec = spec,
+    synthetic = TRUE,
P2 Badge Exercise the documented synthetic mode argument

Instruction 9 requires run_eda() to support mode = "real" and mode = "synthetic", and the SDD's synthetic workflow generates synthetic data first and then calls run_eda(data = data, spec = spec, mode = "synthetic"). This test instead drives an undocumented synthetic = TRUE API with data = NULL, so an implementation that follows the documented contract will fail these new tests, while an implementation that omits mode could pass them.


In R/run_eda.R:

> +run_eda <- function(data,
+                    spec,
+                    output_dir = NULL,
+                    synthetic = FALSE,
+                    n = 100,
+                    seed = NULL) {
P2 Badge Add the planned mode argument to run_eda

The implementation exposes synthetic instead of the planned mode argument, but Instruction 9 requires mode = "real"/"synthetic" and the SDD workflows call run_eda(..., mode = "real") or mode = "synthetic" (spec_driven_EDA_plan/docs/codex/tdd-first-codex-instructions.md:264, spec_driven_EDA_plan/docs/sdd/0001-spec-first-eda.md:83-88). Any caller following that contract, including the upcoming report/project-template work, currently gets unused argument (mode = "real") before the workflow runs.


P2 Badge Derive schema types from the committed fixture

This records classes from the in-memory medicaldata::blood_storage object before CSV serialization, but the fixture-backed tests are meant to consume tests/testthat/fixtures/blood_storage/blood_storage.csv. Reading the committed CSV with base R and preserved names makes columns such as RBC.Age.Group integer while expected_schema.csv records numeric, so any schema test comparing observed types from the committed fixture to this expected file will fail even when the fixture is unchanged. Please compute observed_type after reading back the written CSV using the intended fixture-loading options.
