# ============================================================
# Lab 05: The Coffee Shop Oracle
# MA214 Applied Statistics — Bayesian Linear Regression
# ============================================================
# SUBMISSION INSTRUCTIONS:
#   - Submit THIS file (lab5.R) to GradeScope, NOT the Rmd.
#   - Do NOT use setwd().
#   - Only import: dplyr, ggplot2, readr.
#   - Run this file completely before submitting.
#   - Pass = 4/4 on the autograder.
# ============================================================

library(dplyr)
library(ggplot2)

# ── Task 1: Load the data ────────────────────────────────────
# Read Coffee_df.csv into a data frame called `df`.
# Columns: customers (x), revenue (y).

df <- read.csv("Coffee_df.csv")


# ── Task 2: Define log_posterior() ──────────────────────────
# Write log_posterior(beta0, beta1, sigma2) returning the
# log-posterior (up to a constant) using:
#
#   Likelihood:  y_i ~ N(beta0 + beta1 * x_i, sigma2)
#   Priors:
#     beta0  ~ N(5, 2^2)
#     beta1  ~ N(2, 0.5^2)
#     sigma2 ~ LogNormal(log(1), 0.3^2)
#
# Requirements:
#   (a) return -Inf if sigma2 <= 0
#   (b) use df$customers and df$revenue inside the function
#   (c) return a single finite number at valid inputs

x <- df$customers
y <- df$revenue

log_posterior <- function(beta0, beta1, sigma2) {

  # YOUR CODE HERE

}


# ── Task 3: MCMC (provided — copy exactly, do not modify) ───
# Run this block as-is. It produces post_b0, post_b1, post_s2,
# and beta1_CI which are all needed for the autograder.

set.seed(214214)
n_iter <- 6000
burn   <- 1000

beta0_chain  <- numeric(n_iter)
beta1_chain  <- numeric(n_iter)
sigma2_chain <- numeric(n_iter)

beta0_chain[1]  <- 5
beta1_chain[1]  <- 2
sigma2_chain[1] <- 1

prop_sd <- c(beta0 = 1.2, beta1 = 0.02, log_sigma2 = 0.15)
acc     <- c(b0 = 0L, b1 = 0L, s2 = 0L)

for (i in seq(2, n_iter)) {
  b0_cur <- beta0_chain[i - 1]
  b1_cur <- beta1_chain[i - 1]
  s2_cur <- sigma2_chain[i - 1]

  b0_prop <- rnorm(1, b0_cur, prop_sd["beta0"])
  log_a   <- log_posterior(b0_prop, b1_cur, s2_cur) -
             log_posterior(b0_cur,  b1_cur, s2_cur)
  if (log(runif(1)) < log_a) {
    beta0_chain[i] <- b0_prop; acc["b0"] <- acc["b0"] + 1L
  } else { beta0_chain[i] <- b0_cur }

  b0_cur  <- beta0_chain[i]
  b1_prop <- rnorm(1, b1_cur, prop_sd["beta1"])
  log_a   <- log_posterior(b0_cur, b1_prop, s2_cur) -
             log_posterior(b0_cur, b1_cur,  s2_cur)
  if (log(runif(1)) < log_a) {
    beta1_chain[i] <- b1_prop; acc["b1"] <- acc["b1"] + 1L
  } else { beta1_chain[i] <- b1_cur }

  b1_cur  <- beta1_chain[i]
  s2_prop <- exp(rnorm(1, log(s2_cur), prop_sd["log_sigma2"]))
  log_a   <- log_posterior(b0_cur, b1_cur, s2_prop) -
             log_posterior(b0_cur, b1_cur, s2_cur)
  if (log(runif(1)) < log_a) {
    sigma2_chain[i] <- s2_prop; acc["s2"] <- acc["s2"] + 1L
  } else { sigma2_chain[i] <- s2_cur }
}

post_burn <- burn + 1
post_b0   <- beta0_chain[post_burn:n_iter]
post_b1   <- beta1_chain[post_burn:n_iter]
post_s2   <- sigma2_chain[post_burn:n_iter]

beta1_CI  <- quantile(post_b1, c(0.025, 0.975))


# ── Task 4: Posterior predictive mean ───────────────────────
# For a new day with x_star = 3 customers, compute the
# posterior predictive mean and store it as `post_mean`.
#
# Steps:
#   1. sample n_samp = 1000 indices from
#      post_b1 with replacement.
#   2. For each sampled draw s, compute one predictive value:
#        y_pred[s] = post_b0[s] + post_b1[s] * x_star
#                    + rnorm(1, 0, sqrt(post_s2[s]))
#      Hint: vectorise over idx —
#        post_b0[idx] + post_b1[idx] * x_star
#        + rnorm(n_samp, 0, sqrt(post_s2[idx]))
#   3. post_mean = mean(y_pred)

x_star <- 3
n_samp <- 1000

set.seed(214214)
idx <- sample(length(post_b1), n_samp, replace = TRUE)

# YOUR CODE HERE

# y_pred    <- ...
# post_mean <- ...
