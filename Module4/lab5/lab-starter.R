# ============================================================
# Lab 5: Bayesian Linear Regression
# MA 214 Applied Statistics
# ============================================================
# Do not use setwd().

library(dplyr)
library(ggplot2)
library(readr)

# Task 1: Load data
df <- read.csv("Coffee_df.csv")

x <- df$customers
y <- df$revenue

# Task 2: Define log_posterior()
log_posterior <- function(beta0, beta1, sigma2) {
  # TODO: return -Inf if sigma2 <= 0

  # TODO: compute mu = beta0 + beta1 * x

  # TODO: compute log-likelihood

  # TODO: compute log-priors

  # TODO: return log-posterior
}

# Task 3: MCMC (provided - do not modify)
set.seed(214214)
n_iter <- 6000
burn <- 1000

beta0_chain <- numeric(n_iter)
beta1_chain <- numeric(n_iter)
sigma2_chain <- numeric(n_iter)

beta0_chain[1] <- 5
beta1_chain[1] <- 2
sigma2_chain[1] <- 1

prop_sd <- c(beta0 = 1.2, beta1 = 0.02, log_sigma2 = 0.15)
acc <- c(b0 = 0L, b1 = 0L, s2 = 0L)

for (i in seq(2, n_iter)) {
  b0_cur <- beta0_chain[i - 1]
  b1_cur <- beta1_chain[i - 1]
  s2_cur <- sigma2_chain[i - 1]

  b0_prop <- rnorm(1, b0_cur, prop_sd["beta0"])
  log_a <- log_posterior(b0_prop, b1_cur, s2_cur) -
    log_posterior(b0_cur, b1_cur, s2_cur)
  if (log(runif(1)) < log_a) {
    beta0_chain[i] <- b0_prop
    acc["b0"] <- acc["b0"] + 1L
  } else {
    beta0_chain[i] <- b0_cur
  }

  b0_cur <- beta0_chain[i]
  b1_prop <- rnorm(1, b1_cur, prop_sd["beta1"])
  log_a <- log_posterior(b0_cur, b1_prop, s2_cur) -
    log_posterior(b0_cur, b1_cur, s2_cur)
  if (log(runif(1)) < log_a) {
    beta1_chain[i] <- b1_prop
    acc["b1"] <- acc["b1"] + 1L
  } else {
    beta1_chain[i] <- b1_cur
  }

  b1_cur <- beta1_chain[i]
  s2_prop <- exp(rnorm(1, log(s2_cur), prop_sd["log_sigma2"]))
  log_a <- log_posterior(b0_cur, b1_cur, s2_prop) -
    log_posterior(b0_cur, b1_cur, s2_cur)
  if (log(runif(1)) < log_a) {
    sigma2_chain[i] <- s2_prop
    acc["s2"] <- acc["s2"] + 1L
  } else {
    sigma2_chain[i] <- s2_cur
  }
}

post_burn <- burn + 1
post_b0 <- beta0_chain[post_burn:n_iter]
post_b1 <- beta1_chain[post_burn:n_iter]
post_s2 <- sigma2_chain[post_burn:n_iter]

beta1_CI <- quantile(post_b1, c(0.025, 0.975))

# Task 4: Posterior predictive mean
x_star <- 3
n_samp <- 1000

set.seed(42)
idx <- sample(length(post_b1), n_samp, replace = TRUE)

# TODO: compute predictive draws and posterior predictive mean
y_pred <- NA
post_mean <- NA
