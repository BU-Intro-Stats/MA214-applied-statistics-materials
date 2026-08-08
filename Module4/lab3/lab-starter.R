# ============================================================
# Lab 3: Bayes' Rule
# MA 214 Applied Statistics
# ============================================================
# Submit this R file to Gradescope.
# Do not use setwd().

library(dplyr)
library(ggplot2)
library(readr)

# Part 0: Choose and load a coin file
set.seed(214)
your_study <- sample(1:10, 1)
my_coin_file <- file.path("coin_data_in_lab", sprintf("coin_%02d.csv", your_study))
coin_df <- read.csv(my_coin_file)

p_js <- c(0.15, 0.30, 0.50, 0.70, 0.90)
coin_labels <- c("A", "B", "C", "D", "E")

n_flips <- nrow(coin_df)
n_heads <- sum(coin_df$result)
n_tails <- n_flips - n_heads

# Part 1: Batch updating
prior_probs <- rep(1 / 5, 5)

# TODO: compute likelihood for each coin type
likelihood <- rep(NA, 5)

# TODO: compute numerator, normalizing constant, and posterior
numerator <- rep(NA, 5)
normalizing_constant <- NA
posterior_batch <- rep(NA, 5)

# Part 2: Sequential updating
flips <- coin_df$result

# TODO: manually update the first three flips
p0 <- prior_probs
p1 <- rep(NA, 5)
p2 <- rep(NA, 5)
p3 <- rep(NA, 5)

# TODO: automate sequential updating for all flips
post_mat <- matrix(NA, nrow = n_flips + 1, ncol = 5)
post_mat[1, ] <- prior_probs

for (i in 1:n_flips) {
  # lk <- ...
  # unnorm <- ...
  # post_mat[i + 1, ] <- ...
}

# Part 3: Decision
# TODO: identify final coin type
final_type <- NA
final_posterior <- NA

# Post-lab activity
post_df <- read.csv("lab3_post_data.csv")

# TODO: update posterior probability for the unbiased coin
posterior1 <- NA
posterior2 <- NA
posterior3 <- NA
posterior20 <- NA

# TODO: set TRUE if biased, FALSE if fair
biased <- NA
