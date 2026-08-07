# MA 214 (Spring 2026) -- MCMC and Diagnostics
# In-class activity

library(tidyverse)

####################################################################
# Setup
####################################################################

# Same SBP data from the L21 activity
sbp_data <- c(115, 117, 119, 120, 121, 122, 123, 124, 125, 126,
              127, 127, 128, 129, 130, 131, 132, 133, 134, 137)
n    <- length(sbp_data)
xbar <- mean(sbp_data)
SS   <- sum((sbp_data - xbar)^2)

# Priors (mu and sigma estimated jointly)
#   mu         ~ N(120, 10^2)         [same prior as L21]
#   log(sigma) ~ N(log(10), 0.5^2)   [log-normal: centered at sigma = 10]

# Log-posterior (unnormalized)
log_posterior <- function(mu, sigma) {
  if (sigma <= 0) return(-Inf)
  dnorm(mu, 120, 10, log = TRUE) +
    dlnorm(sigma, meanlog = log(10), sdlog = 0.5, log = TRUE) +
    (-n * log(sigma) - (SS + n * (xbar - mu)^2) / (2 * sigma^2))
}

# MCMC sampler (Metropolis-Hastings, 2D random walk)
# In practice you would use Stan or PyMC; this is a lightweight
# stand-in so we can focus on interpreting the output.
run_mcmc <- function(M, step, start_mu, start_sigma) {
  mu_samples    <- numeric(M)
  sigma_samples <- numeric(M)
  mu_samples[1]    <- start_mu
  sigma_samples[1] <- start_sigma
  accepts <- 0
  for (m in 2:M) {
    mu_cur    <- mu_samples[m - 1]
    sigma_cur <- sigma_samples[m - 1]
    mu_prop    <- rnorm(1, mu_cur, step)
    sigma_prop <- rnorm(1, sigma_cur, step)
    log_alpha <- log_posterior(mu_prop, sigma_prop) -
                 log_posterior(mu_cur, sigma_cur)
    if (log(runif(1)) < log_alpha) {
      mu_samples[m]    <- mu_prop
      sigma_samples[m] <- sigma_prop
      accepts <- accepts + 1
    } else {
      mu_samples[m]    <- mu_cur
      sigma_samples[m] <- sigma_cur
    }
  }
  list(mu = mu_samples, sigma = sigma_samples, acc_rate = accepts / (M - 1))
}

# R-hat: compares within-chain to between-chain variance
compute_rhat <- function(chains) {
  C <- length(chains)
  N <- min(sapply(chains, length))
  chain_means <- sapply(chains, function(ch) mean(ch[1:N]))
  chain_vars  <- sapply(chains, function(ch) var(ch[1:N]))
  W <- mean(chain_vars)
  B <- N * var(chain_means)
  V_hat <- ((N - 1) / N) * W + (1 / N) * B
  sqrt(V_hat / W)
}

####################################################################
# Part 1: Grid Approximation in 2D
####################################################################

# --- 1A: Contour plot of the joint posterior ---
K <- 101
mu_grid    <- seq(118, 134, length.out = K)
sigma_grid <- seq(3, 15, length.out = K)
grid <- expand_grid(mu = mu_grid, sigma = sigma_grid)

grid$log_post <- mapply(log_posterior, grid$mu, grid$sigma)
grid$log_post <- grid$log_post - max(grid$log_post)
grid$post     <- exp(grid$log_post)

ggplot(grid, aes(x = mu, y = sigma, z = post)) +
  geom_contour_filled(bins = 12) +
  labs(x = expression(mu), y = expression(sigma),
       fill = "Density",
       title = "Joint posterior (grid approximation)") +
  theme_minimal()

# --- 1B: Grid convergence ---
grid_summaries_2d <- function(K) {
  mg <- seq(118, 134, length.out = K)
  sg <- seq(3, 15, length.out = K)
  g  <- expand_grid(mu = mg, sigma = sg)
  g$lp <- mapply(log_posterior, g$mu, g$sigma)
  g$lp <- g$lp - max(g$lp)
  g$w  <- exp(g$lp)
  g$w  <- g$w / sum(g$w)
  tibble(K = K, total_points = K^2,
         mu_mean    = sum(g$mu * g$w),
         sigma_mean = sum(g$sigma * g$w))
}

bind_rows(lapply(c(5, 7, 11, 15, 21, 51, 101), grid_summaries_2d)) |>
  mutate(mu_mean = sprintf("%.4f", mu_mean),
         sigma_mean = sprintf("%.4f", sigma_mean))

# --- 1C: Reference summaries from fine grid ---
K <- 201
mu_grid    <- seq(118, 134, length.out = K)
sigma_grid <- seq(3, 15, length.out = K)
grid_fine <- expand_grid(mu = mu_grid, sigma = sigma_grid)
grid_fine$lp <- mapply(log_posterior, grid_fine$mu, grid_fine$sigma)
grid_fine$lp <- grid_fine$lp - max(grid_fine$lp)
grid_fine$w  <- exp(grid_fine$lp)
grid_fine$w  <- grid_fine$w / sum(grid_fine$w)

grid_mu_mean    <- sum(grid_fine$mu * grid_fine$w)
grid_sigma_mean <- sum(grid_fine$sigma * grid_fine$w)
grid_mu_sd      <- sqrt(sum((grid_fine$mu - grid_mu_mean)^2 * grid_fine$w))
grid_sigma_sd   <- sqrt(sum((grid_fine$sigma - grid_sigma_mean)^2 * grid_fine$w))

tibble(
  parameter = c("mu", "sigma"),
  mean = sprintf("%.4f", c(grid_mu_mean, grid_sigma_mean)),
  sd   = sprintf("%.4f", c(grid_mu_sd, grid_sigma_sd))
)

####################################################################
# Part 2: Running MCMC and Burn-in
####################################################################

# --- 2A: Run from a bad starting value ---
set.seed(214)
result <- run_mcmc(M = 2000, step = 0.5,
                   start_mu = 140, start_sigma = 25)

# Trace plots for both parameters
bind_rows(
  tibble(iter = seq_along(result$mu), value = result$mu,
         parameter = "mu"),
  tibble(iter = seq_along(result$sigma), value = result$sigma,
         parameter = "sigma")
) |>
  ggplot(aes(x = iter, y = value)) +
  geom_line(color = "#569BBD", alpha = 0.7) +
  facet_wrap(~ parameter, ncol = 1, scales = "free_y") +
  labs(x = "Iteration", y = "Value",
       title = "Trace plots (starting far from high-density region)") +
  theme_minimal()

# --- 2B: Effect of burn-in on summaries ---
tibble(
  burn_in = c("None (all 2000)", "Drop first 200", "Drop first 500",
              "Drop first 1000"),
  mu_mean = sprintf("%.4f", c(mean(result$mu),
                               mean(result$mu[-(1:200)]),
                               mean(result$mu[-(1:500)]),
                               mean(result$mu[-(1:1000)]))),
  sigma_mean = sprintf("%.4f", c(mean(result$sigma),
                                  mean(result$sigma[-(1:200)]),
                                  mean(result$sigma[-(1:500)]),
                                  mean(result$sigma[-(1:1000)])))
)

# --- 2C: Compare MCMC (M = 2000) to grid ---
burn <- 500
draws_mu    <- result$mu[-(1:burn)]
draws_sigma <- result$sigma[-(1:burn)]

tibble(
  parameter = c("mu", "sigma"),
  MCMC_mean = sprintf("%.4f", c(mean(draws_mu), mean(draws_sigma))),
  MCMC_sd   = sprintf("%.4f", c(sd(draws_mu), sd(draws_sigma))),
  grid_mean = sprintf("%.4f", c(grid_mu_mean, grid_sigma_mean)),
  grid_sd   = sprintf("%.4f", c(grid_mu_sd, grid_sigma_sd))
)

paste("Acceptance rate:", sprintf("%.4f", result$acc_rate))

# --- 2D: Run a longer chain ---
set.seed(214)
result_long <- run_mcmc(M = 10000, step = 0.5,
                        start_mu = 140, start_sigma = 25)

draws_mu_long    <- result_long$mu[-(1:burn)]
draws_sigma_long <- result_long$sigma[-(1:burn)]

tibble(
  parameter = c("mu", "sigma"),
  MCMC_mean = sprintf("%.4f", c(mean(draws_mu_long), mean(draws_sigma_long))),
  MCMC_sd   = sprintf("%.4f", c(sd(draws_mu_long), sd(draws_sigma_long))),
  grid_mean = sprintf("%.4f", c(grid_mu_mean, grid_sigma_mean)),
  grid_sd   = sprintf("%.4f", c(grid_mu_sd, grid_sigma_sd))
)

####################################################################
# Part 3: Good Chains and Bad Chains
####################################################################

# --- 3A: Three step-size settings ---
set.seed(214)
chain_small <- run_mcmc(2000, step = 0.05, 126, 7)
chain_good  <- run_mcmc(2000, step = 1.5,  126, 7)
chain_big   <- run_mcmc(2000, step = 15,   126, 7)

# Acceptance rates
tibble(
  setting  = c("too small (0.05)", "good (1.5)", "too large (15)"),
  acc_rate = sprintf("%.4f", c(chain_small$acc_rate, chain_good$acc_rate,
                                chain_big$acc_rate))
)

# Trace plots for mu
bind_rows(
  tibble(iter = seq_along(chain_small$mu), mu = chain_small$mu,
         setting = "step = 0.05"),
  tibble(iter = seq_along(chain_good$mu), mu = chain_good$mu,
         setting = "step = 1.5"),
  tibble(iter = seq_along(chain_big$mu), mu = chain_big$mu,
         setting = "step = 15")
) |>
  ggplot(aes(x = iter, y = mu)) +
  geom_line(color = "#569BBD", alpha = 0.7) +
  facet_wrap(~ factor(setting, levels = c("step = 0.05",
                                           "step = 1.5",
                                           "step = 15")),
             ncol = 1) +
  labs(x = "Iteration", y = expression(mu)) +
  theme_minimal()

####################################################################
# Part 4: Multiple Chains and R-hat
####################################################################

# --- 4A: Four chains with good settings ---
set.seed(214)
starts <- list(c(110, 3), c(120, 8), c(130, 15), c(140, 25))
chains_good <- lapply(starts, function(s) {
  run_mcmc(2000, step = 1.5, s[1], s[2])
})

# Overlaid trace plots for mu
bind_rows(lapply(seq_along(chains_good), function(i) {
  tibble(iter = seq_along(chains_good[[i]]$mu),
         mu = chains_good[[i]]$mu,
         chain = paste0("(", starts[[i]][1], ", ", starts[[i]][2], ")"))
})) |>
  ggplot(aes(x = iter, y = mu, color = chain)) +
  geom_line(alpha = 0.5) +
  labs(x = "Iteration", y = expression(mu), color = "Start") +
  theme_minimal()

# R-hat
burn <- 500
mu_draws_good    <- lapply(chains_good, function(ch) ch$mu[-(1:burn)])
sigma_draws_good <- lapply(chains_good, function(ch) ch$sigma[-(1:burn)])

tibble(
  parameter = c("mu", "sigma"),
  R_hat = sprintf("%.4f", c(compute_rhat(mu_draws_good),
                              compute_rhat(sigma_draws_good)))
)

# --- 4B: Four chains with bad settings ---
set.seed(214)
chains_bad <- lapply(starts, function(s) {
  run_mcmc(2000, step = 0.05, s[1], s[2])
})

# Overlaid trace plots for mu
bind_rows(lapply(seq_along(chains_bad), function(i) {
  tibble(iter = seq_along(chains_bad[[i]]$mu),
         mu = chains_bad[[i]]$mu,
         chain = paste0("(", starts[[i]][1], ", ", starts[[i]][2], ")"))
})) |>
  ggplot(aes(x = iter, y = mu, color = chain)) +
  geom_line(alpha = 0.5) +
  labs(x = "Iteration", y = expression(mu), color = "Start") +
  theme_minimal()

# R-hat
mu_draws_bad    <- lapply(chains_bad, function(ch) ch$mu[-(1:burn)])
sigma_draws_bad <- lapply(chains_bad, function(ch) ch$sigma[-(1:burn)])

tibble(
  parameter = c("mu", "sigma"),
  R_hat = sprintf("%.4f", c(compute_rhat(mu_draws_bad),
                              compute_rhat(sigma_draws_bad)))
)
