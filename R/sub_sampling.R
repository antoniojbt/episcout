############
# Sub-sample from a large dataset
# Weighted, no replacement, based on a binary outcome variable

# # Sample data creation (assuming you have a large dataset)
# set.seed(123)
# data_f <- data.frame(
#   id = 1:10000,
#   outcome = sample(c(0, 1), 10000, replace = TRUE, prob = c(0.7, 0.3)),  # outcome variable with weights
#   other_var = rnorm(10000)
# )

# Variable for weighting:
outcome_var <- 'd_death_30'

# Weighting didn't work
# So split the datasets and sampled separately

# Generate weights:
# For binary variable to balance sub-sample:
# weights <- ifelse(data_f[[outcome_var]] == 1,
#                   1 / sum(data_f[[outcome_var]] == 1),
#                   1 / sum(data_f[[outcome_var]] == 0)
#                   )

# total_1 <- sum(data_f[[outcome_var]] == 1)
# total_0 <- sum(data_f[[outcome_var]] == 0)
# weights <- ifelse(data_f[[outcome_var]] == 1, 1 / total_1, 1 / total_0)
# sum(weights)

# # Normalize the weights to sum to 1:
# weights <- weights / sum(weights)
# sum(weights)
# head(weights)


# Split data based on outcome variable:
data_f_0 <- data_f[data_f[[outcome_var]] == 0, ]
data_f_1 <- data_f[data_f[[outcome_var]] == 1, ]
dim(data_f_0)
dim(data_f_1)

# Calculate percent needed of the total size, var defined in other script:
# perc_needed <- 0.0001
sample_size <- perc_needed * nrow(data_f)
sample_size
sample_size_0 <- round(perc_needed * nrow(data_f_0))
sample_size_1 <- round(perc_needed * nrow(data_f_1))

# Sample without replacement using the normalized weights
set.seed(123)  # for reproducibility

# For each subset get sub-sample:
sample_indices_0 <- sample(1:nrow(data_f_0), size = sample_size_0, replace = FALSE)
sample_indices_1 <- sample(1:nrow(data_f_1), size = sample_size_1, replace = FALSE)

length(sample_indices_0)
length(sample_indices_1)

# Actual sub-samples:
data_f_sub_0 <- data_f_0[sample_indices_0, ]
data_f_sub_1 <- data_f_1[sample_indices_1, ]

# Join datasets:
data_f_sub <- rbind(data_f_sub_0, data_f_sub_1)

dim(data_f_sub)
str(data_f_sub)
epi_head_and_tail(data_f_sub)


# Check distribution of var of interest:
table(data_f[[outcome_var]])
table(data_f_sub[[outcome_var]])
prop.table(table(data_f[[outcome_var]]))
prop.table(table(data_f_sub[[outcome_var]]))
############
