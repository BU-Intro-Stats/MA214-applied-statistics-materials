# ============================================================
# Lab 4: Conjugate Families - Beta-Binomial
# MA 214 Applied Statistics
# ============================================================
# Do not use setwd().

library(dplyr)
library(ggplot2)
library(readr)

# Part 0: Choose and load a coin file
set.seed(214)
your_study <- sample(1:10, 1)
my_coin_file <- file.path("Lab4_data", sprintf("lab4_coin_%02d.csv", your_study))
coin_df <- read.csv(my_coin_file)

n_flips <- nrow(coin_df)
n_heads <- sum(coin_df$result)
n_tails <- n_flips - n_heads
sample_prop <- n_heads / n_flips

# Part 1: Batch Beta-Binomial update
# TODO: choose prior parameters
alpha_prior <- NA
beta_prior <- NA

# TODO: compute posterior parameters
alpha_post <- NA
beta_post <- NA

# TODO: compute posterior summaries
post_mean <- NA
post_sd <- NA
cred_int <- c(NA, NA)

# Part 2: Sequential updating
flips <- coin_df$result

# TODO: manually update the first three flips
a1 <- NA
b1 <- NA
a2 <- NA
b2 <- NA
a3 <- NA
b3 <- NA
a4 <- NA
b4 <- NA

# TODO: automate sequential updating
a_seq <- rep(NA, n_flips + 1)
b_seq <- rep(NA, n_flips + 1)
a_seq[1] <- alpha_prior
b_seq[1] <- beta_prior

for (i in 1:n_flips) {
  # a_seq[i + 1] <- ...
  # b_seq[i + 1] <- ...
}

post_mean_seq <- a_seq / (a_seq + b_seq)

# Post-lab activity
post_df <- read.csv("lab4_post_data.csv")

# TODO: define any requested post-lab posterior summaries here
