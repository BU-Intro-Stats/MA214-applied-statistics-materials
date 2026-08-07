# COMPLETE VERSION -- blanks filled in (instructor copy)
# MA 214 (Spring 2026) -- Lecture 21 in-class activity
# Bayesian Linear Regression: Ames Housing
#
# In Lecture 15 you fit this regression the FREQUENTIST way:
#     price_hat = 13.3 + 0.112 * living_area,  s_e = 56.5,  R^2 = 0.500
#     at 1500 sq ft:  CI for the mean [178.8, 182.9];  PI for a house [70.0, 291.7]
# Today you fit THE SAME regression the Bayesian way and compare.
#
# All blanks are filled in with tuned values (acceptance ~0.29, R-hat ~1.001).
# In practice you would use Stan or PyMC; this hand-rolled sampler is a zero-install
# stand-in so we can focus on INTERPRETING the output.

library(tidyverse)
library(broom)

####################################################################
# Setup: data, OLS baseline, priors, sampler
####################################################################

ames <- read_csv("ames_housing.csv", show_col_types = FALSE)
x <- ames$living_area      # square feet
y <- ames$sale_price       # $000s
n <- length(y)

ols_model <- lm(sale_price ~ living_area, data = ames)
b1_ols    <- coef(ols_model)[["living_area"]]
ols_ci    <- confint(ols_model)["living_area", ]

# --- Priors --------------------------------------------------------
#   beta0      ~ N(0, 100^2)          intercept: price of a 0 sq ft house
#   beta1      ~ N(0, 1^2)            slope: $000s per square foot
#   log(sigma) ~ N(log(50), 0.5^2)    residual SD, in $000s
prior_b0_mean <- 0;  prior_b0_sd <- 100
prior_b1_mean <- 0;  prior_b1_sd <- 1
prior_log_sig_mean <- log(50); prior_log_sig_sd <- 0.5

SS_resid <- function(b0, b1) sum((y - b0 - b1 * x)^2)

log_posterior <- function(b0, b1, sigma) {
  if (sigma <= 0) return(-Inf)
  dnorm(b0, prior_b0_mean, prior_b0_sd, log = TRUE) +
    dnorm(b1, prior_b1_mean, prior_b1_sd, log = TRUE) +
    dlnorm(sigma, prior_log_sig_mean, prior_log_sig_sd, log = TRUE) +
    (-n * log(sigma) - SS_resid(b0, b1) / (2 * sigma^2))
}

run_mcmc <- function(M, step_b0, step_b1, step_sig,
                     start_b0, start_b1, start_sig, seed = 1) {
  set.seed(seed)
  b0_s <- numeric(M); b1_s <- numeric(M); sig_s <- numeric(M)
  b0_s[1] <- start_b0; b1_s[1] <- start_b1; sig_s[1] <- start_sig
  accepts <- 0
  for (m in 2:M) {
    b0_p  <- rnorm(1, b0_s[m-1], step_b0)
    b1_p  <- rnorm(1, b1_s[m-1], step_b1)
    sig_p <- rnorm(1, sig_s[m-1], step_sig)
    log_alpha <- log_posterior(b0_p, b1_p, sig_p) -
                 log_posterior(b0_s[m-1], b1_s[m-1], sig_s[m-1])
    if (log(runif(1)) < log_alpha) {
      b0_s[m] <- b0_p; b1_s[m] <- b1_p; sig_s[m] <- sig_p; accepts <- accepts + 1
    } else {
      b0_s[m] <- b0_s[m-1]; b1_s[m] <- b1_s[m-1]; sig_s[m] <- sig_s[m-1]
    }
  }
  tibble(iter = 1:M, beta0 = b0_s, beta1 = b1_s, sigma = sig_s) |>
    mutate(acc_rate = accepts / (M - 1))
}

compute_rhat <- function(chains_list, param, burn) {
  cd <- lapply(chains_list, function(ch) ch[[param]][ch$iter > burn])
  N  <- min(sapply(cd, length))
  cm <- sapply(cd, function(d) mean(d[1:N])); cv <- sapply(cd, function(d) var(d[1:N]))
  W  <- mean(cv); B <- N * var(cm)
  sqrt((((N - 1) / N) * W + (1 / N) * B) / W)
}

####################################################################
# Part 1: Reading the priors   (worksheet only -- no code needed)
####################################################################

####################################################################
# Part 2: Prior predictive check -- what does the prior believe?
####################################################################

# --- 2A: 60 lines drawn from the prior ALONE, over the real data ---
set.seed(24)
pp <- tibble(b0 = rnorm(60, prior_b0_mean, prior_b0_sd),
             b1 = rnorm(60, prior_b1_mean, prior_b1_sd))

ggplot(ames, aes(x = living_area, y = sale_price)) +
  geom_abline(data = pp, aes(intercept = b0, slope = b1),
              alpha = 0.3, colour = "#569BBD") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(alpha = 0.15, size = 0.7, colour = "#F05133") +
  coord_cartesian(xlim = c(0, 4000), ylim = c(-1500, 2000)) +
  labs(x = "Living area (sq ft)", y = "Sale price ($000s)",
       title = "60 regression lines drawn from the prior alone") +
  theme_minimal(base_size = 12)

# --- 2B: what the prior expects the MEAN price of a 1500 sq ft house to be ---
x_chk   <- 1500
pp_mean <- prior_b0_mean + prior_b1_mean * x_chk
pp_sd   <- sqrt(prior_b0_sd^2 + (x_chk * prior_b1_sd)^2)
cat(sprintf("Prior mean price at %d sq ft: %.0f +/- %.0f ($000s)\n", x_chk, pp_mean, pp_sd))
cat(sprintf("Prior 95%% range: [%.0f, %.0f] ($000s)\n",
            pp_mean - 1.96 * pp_sd, pp_mean + 1.96 * pp_sd))

####################################################################
# Part 3: Fit it, and read the slope
####################################################################

# --- 3A: four chains from prior-drawn starts ---
# ADJUST the step sizes until the acceptance rate is between 0.2 and 0.5.
# (Hint: the posterior SDs are roughly  beta0 ~ 3,  beta1 ~ 0.002,  sigma ~ 0.7.
#  A random-walk step is usually of the same order as the SD it is exploring.)
M <- 40000
set.seed(7)
start_b0s  <- rnorm(4, 0, 20)
start_b1s  <- rnorm(4, 0.1, 0.05)
start_sigs <- rlnorm(4, prior_log_sig_mean, prior_log_sig_sd)

chains <- lapply(1:4, function(i)
  run_mcmc(M,
           step_b0  = 2,         # tuned: acceptance ~0.29
           step_b1  = 0.0015,    # tuned
           step_sig = 0.8,       # tuned
           start_b0 = start_b0s[i], start_b1 = start_b1s[i],
           start_sig = start_sigs[i], seed = 7 + i) |>
    mutate(chain = factor(i)))

cat(sprintf("Acceptance rate: %.3f\n", chains[[1]]$acc_rate[1]))

bind_rows(chains) |>
  pivot_longer(c(beta0, beta1, sigma), names_to = "param") |>
  ggplot(aes(x = iter, y = value, colour = chain)) +
  geom_line(alpha = 0.4, linewidth = 0.3) +
  facet_wrap(~param, scales = "free_y", ncol = 1) +
  labs(x = "Iteration", y = "") +
  theme_minimal(base_size = 12)

burn <- 10000        # chains have settled well before this

for (p in c("beta0", "beta1", "sigma"))
  cat(sprintf("R-hat %-6s = %.4f\n", p, compute_rhat(chains, p, burn)))

# --- 3B: credible interval for beta1, next to the OLS confidence interval ---
draws <- bind_rows(chains) |> filter(iter > burn)

tibble(
  method   = c("Frequentist (OLS)", "Bayesian"),
  estimate = c(b1_ols, mean(draws$beta1)),
  lower    = c(ols_ci[1], quantile(draws$beta1, 0.025)),
  upper    = c(ols_ci[2], quantile(draws$beta1, 0.975))
) |> mutate(across(where(is.numeric), ~round(.x, 5)))

####################################################################
# Part 4: Predicting a house
####################################################################

# --- 4A: at 1500 sq ft -- the line, and a new house ---
x_new  <- 1500
y_line <- draws$beta0 + draws$beta1 * x_new                          # the LINE
y_new  <- y_line + rnorm(nrow(draws), 0, draws$sigma)                # a NEW house

tibble(
  quantity = c("the line  E[y|x]", "a new house  y_tilde"),
  estimate = c(mean(y_line), mean(y_new)),
  lower    = c(quantile(y_line, 0.025), quantile(y_new, 0.025)),
  upper    = c(quantile(y_line, 0.975), quantile(y_new, 0.975))
) |> mutate(across(where(is.numeric), ~round(.x, 1)))

# Compare with Lecture 15:  CI [178.8, 182.9]   PI [70.0, 291.7]

####################################################################
# Part 5: Wrap-up -- what a confident prior costs
####################################################################

# --- 5A: a realtor insists on "$150 per square foot", very confidently ---
# Re-run the fit with a SHARP prior on beta1 and see what it does.
prior_b1_mean <- 0.150
prior_b1_sd   <- 0.002        # very confident

chains_sharp <- lapply(1:4, function(i)
  run_mcmc(M, step_b0 = 2, step_b1 = 0.0015, step_sig = 0.8,
           start_b0 = start_b0s[i], start_b1 = start_b1s[i],
           start_sig = start_sigs[i], seed = 7 + i) |>
    mutate(chain = factor(i)))

draws_sharp <- bind_rows(chains_sharp) |> filter(iter > burn)
cat(sprintf("Sharp-prior posterior beta1: %.4f  [%.4f, %.4f]\n",
            mean(draws_sharp$beta1),
            quantile(draws_sharp$beta1, 0.025), quantile(draws_sharp$beta1, 0.975)))
cat(sprintf("Data (OLS) said:             %.4f  [%.4f, %.4f]\n", b1_ols, ols_ci[1], ols_ci[2]))

# Reset the prior so you don't accidentally reuse it
prior_b1_mean <- 0; prior_b1_sd <- 1
