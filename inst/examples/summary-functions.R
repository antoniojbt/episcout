# Shared examples for the three summary-table helpers.

set.seed(20260805)
example_data <- data.frame(
  visit = rep(c("baseline", "follow_up"), 4),
  group = rep(c("A", "B"), each = 4),
  score = c(10, 12, 14, 13, 9, 11, 15, 16),
  outcome = factor(c(0, 0, 1, 0, 0, 1, 1, 1))
)

# Summarise the character and factor columns after excluding a reviewed code.
categorical_summary <- epi_stats_summary(
  example_data,
  codes = "follow_up",
  class_type = "chr_fct",
  action = "exclude"
)
categorical_summary

# Convert counts to a display table; retain unformatted values for calculations.
code_counts <- epi_stats_summary(
  example_data,
  codes = c("baseline", "A", "0"),
  class_type = "chr_fct",
  action = "codes_only"
)
tidy_counts <- epi_stats_tidy(
  code_counts,
  perc_n = nrow(example_data)
)
epi_stats_format(tidy_counts, digits = 1)

# The typed contract profiles every supported column with explicit coverage.
typed_summary <- epi_stats_summary(example_data, output = "typed")
names(typed_summary)
typed_summary$variables
