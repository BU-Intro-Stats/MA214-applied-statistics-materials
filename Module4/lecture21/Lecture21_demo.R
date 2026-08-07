# MA214 Lecture 21 -- Bayesian Linear Regression
# Run from the lecture directory:  Rscript Lecture21_demo.R
#   (first: Rscript data/prepare_data.R)
#
# Penguins: body_mass_g ~ flipper_length_mm, n = 40. Every number that appears on
# a slide is printed below; every figure is written to figures/.
#
#   Section 1  Data + OLS baseline        -> figures/penguins_scatter.pdf
#   Section 2  Priors + prior predictive  -> figures/prior_beta1.pdf
#                                            figures/prior_predictive_lines.pdf
#   Section 3  MCMC + diagnostics         -> figures/trace_plots.pdf
#   Section 4  Posterior inference (beta1)-> figures/beta1_prior_post.pdf
#                                            figures/freq_vs_bayes.pdf
#                                            figures/boot_vs_posterior.pdf
#                                            figures/posterior_lines.pdf
#   Section 5  Prediction for a new penguin -> figures/posterior_predictive.pdf
#                                            figures/pred_comparison.pdf
#
# The MCMC machinery (random-walk Metropolis-Hastings, burn-in, R-hat) is Lecture
# 19's; it is used here, not re-taught. Credible intervals, posterior prediction
# and predictive checks are Lecture 20's; this lecture applies them to a regression.

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(patchwork))
dir.create("figures", showWarnings = FALSE)

col_prior <- "#569BBD"   # blue
col_data  <- "#F05133"   # red/orange (data, OLS, frequentist)
col_post  <- "black"     # posterior

rule <- function(t) cat("\n", strrep("-", 68), "\n", t, "\n", strrep("-", 68), "\n", sep = "")

################################################################################
# Section 1 -- Data and the OLS baseline (Module 3 recall)
################################################################################
rule("SLIDE 1: Recall the penguins -- OLS fit")

penguins <- read_csv("data/penguins40.csv", show_col_types = FALSE)
n <- nrow(penguins)
x <- penguins$flipper_length_mm
y <- penguins$body_mass_g

xbar <- mean(x); ybar <- mean(y)
Sxx  <- sum((x - xbar)^2)
Sxy  <- sum((x - xbar) * (y - ybar))
Syy  <- sum((y - ybar)^2)

b1_ols    <- Sxy / Sxx
b0_ols    <- ybar - b1_ols * xbar
resids    <- y - (b0_ols + b1_ols * x)
sigma_ols <- sqrt(sum(resids^2) / (n - 2))
se_b1     <- sigma_ols / sqrt(Sxx)
se_b0     <- sigma_ols * sqrt(1/n + xbar^2/Sxx)
r2        <- 1 - sum(resids^2) / Syy
freq_ci   <- c(b1_ols - 1.96 * se_b1, b1_ols + 1.96 * se_b1)

cat(sprintf("n = %d penguins; flipper %d-%d mm, mass %d-%d g\n",
            n, min(x), max(x), min(y), max(y)))
cat(sprintf("OLS: b0 = %.1f (SE %.1f), b1 = %.1f (SE %.2f), sigma = %.1f, R2 = %.3f\n",
            b0_ols, se_b0, b1_ols, se_b1, sigma_ols, r2))
cat(sprintf("OLS 95%% CI for b1: [%.1f, %.1f]  (width %.1f)\n",
            freq_ci[1], freq_ci[2], diff(freq_ci)))

p_scatter <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.6, size = 2.5, color = "gray30") +
  geom_abline(intercept = b0_ols, slope = b1_ols, color = col_data, linewidth = 1.2) +
  annotate("text", x = 224, y = 4150, label = "OLS fit", colour = col_data,
           size = 7, fontface = "bold") +
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_minimal(base_size = 24)
ggsave("figures/penguins_scatter.pdf", p_scatter, width = 5.5, height = 4.1)

################################################################################
# Section 2 -- Priors for the parameters, and a prior predictive check
################################################################################
rule("SLIDE 2: Priors for the parameters")

# beta0      ~ N(0, 5000^2)        vague: intercept is an extrapolation to 0 mm
# beta1      ~ N(40, 5^2)          informative: prior studies suggest ~40 g/mm
# log(sigma) ~ N(log 400, 0.5^2)   weakly informative: residual SD ~ hundreds of g
prior_b0_mean <- 0;   prior_b0_sd <- 5000
prior_b1_mean <- 40;  prior_b1_sd <- 5
prior_log_sig_mean <- log(400); prior_log_sig_sd <- 0.5

cat(sprintf("beta0      ~ N(%d, %d^2)        [vague]\n", prior_b0_mean, prior_b0_sd))
cat(sprintf("beta1      ~ N(%d, %d^2)          [informative; OLS says %.1f]\n",
            prior_b1_mean, prior_b1_sd, b1_ols))
cat(sprintf("log(sigma) ~ N(log %d, %.1f^2)   [weakly informative]\n",
            400, prior_log_sig_sd))

b1_grid <- seq(20, 70, length.out = 500)
p_prior_b1 <- ggplot() +
  geom_line(aes(x = b1_grid, y = dnorm(b1_grid, prior_b1_mean, prior_b1_sd)),
            color = col_prior, linewidth = 1.2) +
  geom_area(aes(x = b1_grid, y = dnorm(b1_grid, prior_b1_mean, prior_b1_sd)),
            fill = col_prior, alpha = 0.15) +
  geom_vline(xintercept = b1_ols, linewidth = 1, linetype = "dashed", color = col_data) +
  annotate("text", x = b1_ols + 1, y = dnorm(prior_b1_mean, prior_b1_mean, prior_b1_sd) * 0.9,
           label = sprintf("OLS = %.1f", b1_ols), color = col_data,
           hjust = 0, size = 8.2, fontface = "bold") +
  labs(x = expression(beta[1]~"(g per mm)"), y = "Density") +
  theme_minimal(base_size = 24)
ggsave("figures/prior_beta1.pdf", p_prior_b1, width = 6, height = 3.5)

rule("SLIDE 2b: PRIOR PREDICTIVE CHECK -- what lines does this prior believe in?")

# Simulate lines from the prior ALONE (no data yet) and look at them.
set.seed(2024)
n_lines  <- 60
pp_b0    <- rnorm(n_lines, prior_b0_mean, prior_b0_sd)
pp_b1    <- rnorm(n_lines, prior_b1_mean, prior_b1_sd)

# Prior predictive for the mean mass at a typical flipper length
x_chk    <- 200
pp_mean  <- prior_b0_mean + prior_b1_mean * x_chk
pp_sd    <- sqrt(prior_b0_sd^2 + (x_chk * prior_b1_sd)^2)
pp_lo    <- pp_mean - 1.96 * pp_sd; pp_hi <- pp_mean + 1.96 * pp_sd
cat(sprintf("Prior belief about mean mass at flipper = %d mm:\n", x_chk))
cat(sprintf("  %.0f +/- %.0f g  ->  95%% range [%.0f, %.0f] g\n", pp_mean, pp_sd, pp_lo, pp_hi))
cat(sprintf("  Real penguins in the data span %d-%d g.\n", min(y), max(y)))
cat(sprintf("  So the prior allows NEGATIVE mass and %.0f kg penguins: it is vague,\n",
            pp_hi / 1000))
cat("  not thoughtful. Harmless here (40 penguins will overwhelm it), but this is\n")
cat("  exactly the check that catches a prior that would NOT be harmless.\n")

p_prior_pred <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_abline(data = tibble(b0 = pp_b0, b1 = pp_b1),
              aes(intercept = b0, slope = b1),
              alpha = 0.25, colour = col_prior) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "gray40") +
  annotate("text", x = 171, y = -2300, label = "zero mass", hjust = 0,
           colour = "gray30", size = 5.5) +
  geom_point(alpha = 0.8, size = 2, colour = col_data) +
  coord_cartesian(xlim = c(170, 235), ylim = c(-4000, 16000)) +
  scale_y_continuous(labels = function(v) v / 1000) +
  labs(x = "Flipper length (mm)", y = "Mass (kg)") +
  theme_minimal(base_size = 20)
ggsave("figures/prior_predictive_lines.pdf", p_prior_pred, width = 5.0, height = 2.2)

################################################################################
# Section 3 -- Fit by MCMC (Lecture 19's machinery, used not re-taught)
################################################################################
rule("SLIDE 3: Running MCMC -- 4 chains, R-hat")

SS_resid <- function(b0, b1) sum((y - b0 - b1 * x)^2)
log_posterior <- function(b0, b1, sigma) {
  if (sigma <= 0) return(-Inf)
  dnorm(b0, prior_b0_mean, prior_b0_sd, log = TRUE) +
    dnorm(b1, prior_b1_mean, prior_b1_sd, log = TRUE) +
    dlnorm(sigma, prior_log_sig_mean, prior_log_sig_sd, log = TRUE) +
    (-n * log(sigma) - SS_resid(b0, b1) / (2 * sigma^2))
}

run_mcmc_reg <- function(M, step_b0, step_b1, step_sig,
                         start_b0, start_b1, start_sig, seed = 214) {
  set.seed(seed)
  b0_s <- numeric(M); b1_s <- numeric(M); sig_s <- numeric(M)
  b0_s[1] <- start_b0; b1_s[1] <- start_b1; sig_s[1] <- start_sig
  accepts <- 0
  for (m in 2:M) {
    b0_c <- b0_s[m-1]; b1_c <- b1_s[m-1]; sig_c <- sig_s[m-1]
    b0_p <- rnorm(1, b0_c, step_b0)
    b1_p <- rnorm(1, b1_c, step_b1)
    sig_p <- rnorm(1, sig_c, step_sig)
    log_alpha <- log_posterior(b0_p, b1_p, sig_p) - log_posterior(b0_c, b1_c, sig_c)
    if (log(runif(1)) < log_alpha) {
      b0_s[m] <- b0_p; b1_s[m] <- b1_p; sig_s[m] <- sig_p; accepts <- accepts + 1
    } else {
      b0_s[m] <- b0_c; b1_s[m] <- b1_c; sig_s[m] <- sig_c
    }
  }
  tibble(iter = 1:M, beta0 = b0_s, beta1 = b1_s, sigma = sig_s) |>
    mutate(acc_rate = accepts / (M - 1))
}

# beta0 and beta1 are strongly correlated in an uncentered regression, so the
# random walk mixes slowly: at M = 30,000 / burn = 3,000 (the original settings)
# R-hat came out at 1.048 -- a FAIL by Lecture 19's own "R-hat < 1.01" rule, and
# the chains simply had not travelled from their spread-out prior starts. Running
# longer fixes it: M = 100,000 / burn = 30,000 gives R-hat = 1.004.
M <- 100000; burn <- 30000; n_chains <- 4
set.seed(100)
start_b0s  <- rnorm(n_chains, prior_b0_mean, prior_b0_sd)
start_b1s  <- rnorm(n_chains, prior_b1_mean, prior_b1_sd)
start_sigs <- rlnorm(n_chains, prior_log_sig_mean, prior_log_sig_sd)
cat(sprintf("%d chains, M = %s iterations each, burn-in = %s\n",
            n_chains, format(M, big.mark = ",", scientific = FALSE),
            format(burn, big.mark = ",", scientific = FALSE)))
cat("Starting values sampled from the priors (deliberately spread out):\n")
for (c in 1:n_chains)
  cat(sprintf("  Chain %d: b0=%.0f, b1=%.1f, sigma=%.0f\n",
              c, start_b0s[c], start_b1s[c], start_sigs[c]))

chains_list <- lapply(1:n_chains, function(c) {
  run_mcmc_reg(M, step_b0 = 80, step_b1 = 0.4, step_sig = 15,
               start_b0 = start_b0s[c], start_b1 = start_b1s[c],
               start_sig = start_sigs[c], seed = 214 + c) |>
    mutate(chain = factor(c))
})
all_chains <- bind_rows(chains_list)
for (c in 1:n_chains)
  cat(sprintf("Chain %d acceptance rate: %.3f\n", c, chains_list[[c]]$acc_rate[1]))

compute_rhat <- function(chains_list, param, burn) {
  cd <- lapply(chains_list, function(ch) ch[[param]][ch$iter > burn])
  N <- min(sapply(cd, length))
  cm <- sapply(cd, function(d) mean(d[1:N])); cv <- sapply(cd, function(d) var(d[1:N]))
  W <- mean(cv); B <- N * var(cm)
  sqrt((((N - 1)/N) * W + (1/N) * B) / W)
}
rhat_b0  <- compute_rhat(chains_list, "beta0", burn)
rhat_b1  <- compute_rhat(chains_list, "beta1", burn)
rhat_sig <- compute_rhat(chains_list, "sigma", burn)
cat(sprintf("R-hat: beta0=%.4f, beta1=%.4f, sigma=%.4f  (Lecture 19's rule: pass if < 1.01)\n",
            rhat_b0, rhat_b1, rhat_sig))
stopifnot(max(rhat_b0, rhat_b1, rhat_sig) < 1.01)   # fail loudly rather than teach a bad fit

chain_cols <- c("1" = "#E41A1C", "2" = "#377EB8", "3" = "#4DAF4A", "4" = "#984EA3")
mk_trace <- function(var, lab, rh, bottom = FALSE, show_burn_label = FALSE) {
  g <- ggplot(all_chains, aes(x = iter, y = .data[[var]], color = chain)) +
    geom_line(alpha = 0.35, linewidth = 0.3) +
    geom_vline(xintercept = burn, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = chain_cols) +
    # three breaks and headroom: five breaks collided once the panels were
    # short enough to stack, and the R-hat label sat on top of the chains
    scale_y_continuous(n.breaks = 3) +
    labs(x = if (bottom) "Iteration" else NULL, y = lab) +
    theme_minimal(base_size = 22) + theme(legend.position = "none")
  if (!bottom) g <- g + theme(axis.text.x = element_blank())   # one x-axis, not three
  if (show_burn_label)
    g <- g + annotate("text", x = burn, y = Inf, label = "burn-in",
                      hjust = 1.1, vjust = 1.4, size = 5.6, colour = "gray35")
  g
}
p_traces <- mk_trace("beta0", expression(beta[0]), rhat_b0, show_burn_label = TRUE) /
            mk_trace("beta1", expression(beta[1]), rhat_b1) /
            mk_trace("sigma", expression(sigma), rhat_sig, bottom = TRUE)
ggsave("figures/trace_plots.pdf", p_traces, width = 7.0, height = 4.2)

################################################################################
# Section 4 -- Posterior inference for the slope
################################################################################
rule("SLIDE 4: Posterior inference for beta1")

draws    <- all_chains |> filter(iter > burn)
post_b0  <- mean(draws$beta0); post_b1 <- mean(draws$beta1); post_sig <- mean(draws$sigma)
ci_b0    <- quantile(draws$beta0, c(0.025, 0.975))
ci_b1    <- quantile(draws$beta1, c(0.025, 0.975))
ci_sig   <- quantile(draws$sigma, c(0.025, 0.975))
cat(sprintf("Posterior beta0: %.1f  95%% CI [%.1f, %.1f]\n", post_b0, ci_b0[1], ci_b0[2]))
cat(sprintf("Posterior beta1: %.1f  95%% CI [%.1f, %.1f]  (width %.1f)\n",
            post_b1, ci_b1[1], ci_b1[2], diff(ci_b1)))
cat(sprintf("Posterior sigma: %.1f  95%% CI [%.1f, %.1f]\n", post_sig, ci_sig[1], ci_sig[2]))
cat(sprintf("\nSLIDE TABLE -- frequentist vs Bayesian for beta1:\n"))
cat(sprintf("  Freq (OLS): %.1f  [%.1f, %.1f]  width %.1f\n",
            b1_ols, freq_ci[1], freq_ci[2], diff(freq_ci)))
cat(sprintf("  Bayes:      %.1f  [%.1f, %.1f]  width %.1f\n",
            post_b1, ci_b1[1], ci_b1[2], diff(ci_b1)))
cat(sprintf("  The prior (centered at 40) pulls the estimate down and narrows the interval.\n"))

# prior vs (scaled) likelihood vs posterior for beta1
b1_g <- seq(min(draws$beta1), max(draws$beta1), length.out = 400)
# Profile beta0 out: at each candidate beta1, refit the intercept by least
# squares. Holding beta0 at b0_ols instead makes this curve sigma/sqrt(sum x^2)
# wide rather than sigma/sqrt(Sxx) -- about 14x too narrow, which drew the data
# as far more precise than they are and contradicted the posterior on the slide.
lik_b1 <- sapply(b1_g, function(bb) {
  b0_hat <- mean(y) - bb * mean(x)
  exp(-SS_resid(b0_hat, bb) / (2 * sigma_ols^2))
})
lik_b1 <- lik_b1 / max(lik_b1) * max(density(draws$beta1)$y)
pri_b1 <- dnorm(b1_g, prior_b1_mean, prior_b1_sd)
pri_b1 <- pri_b1 / max(pri_b1) * max(density(draws$beta1)$y)
pd <- density(draws$beta1)
p_b1_post <- ggplot() +
  geom_line(aes(b1_g, pri_b1, colour = "Prior"), linewidth = 1.1) +
  geom_line(aes(b1_g, lik_b1, colour = "Likelihood"), linewidth = 1.1) +
  geom_line(aes(pd$x, pd$y, colour = "Posterior"), linewidth = 1.3) +
  scale_colour_manual(values = c("Prior" = col_prior,
                                 "Likelihood" = col_data,
                                 "Posterior" = col_post),
                      breaks = c("Prior", "Likelihood", "Posterior")) +
  labs(x = expression(beta[1]~"(g per mm)"), y = "Density (rescaled)", colour = "") +
  theme_minimal(base_size = 24) + theme(legend.position = "top")
ggsave("figures/beta1_prior_post.pdf", p_b1_post, width = 7, height = 4)

comp_df <- tibble(
  method = factor(c("Frequentist (OLS)", "Bayesian"), levels = c("Bayesian", "Frequentist (OLS)")),
  estimate = c(b1_ols, post_b1), lower = c(freq_ci[1], ci_b1[1]), upper = c(freq_ci[2], ci_b1[2]))
p_compare <- ggplot(comp_df, aes(y = method, x = estimate, xmin = lower, xmax = upper, color = method)) +
  geom_pointrange(linewidth = 1.2, size = 1.5) +
  scale_color_manual(values = c("Frequentist (OLS)" = col_data, "Bayesian" = col_prior)) +
  labs(x = expression(beta[1]~"(g per mm)"), y = "") +
  theme_minimal(base_size = 24) + theme(legend.position = "none")
ggsave("figures/freq_vs_bayes.pdf", p_compare, width = 7, height = 2.5)

rule("SLIDE 4b: Bootstrap vs posterior (Lecture 13's bootstrap, revisited)")
set.seed(42)
B <- 10000
boot_b1 <- numeric(B)
for (b in 1:B) {
  ib <- sample(n, n, replace = TRUE)
  boot_b1[b] <- sum((x[ib] - mean(x[ib])) * (y[ib] - mean(y[ib]))) / sum((x[ib] - mean(x[ib]))^2)
}
boot_ci <- quantile(boot_b1, c(0.025, 0.975))
cat(sprintf("Bootstrap beta1: mean %.1f  95%% interval [%.1f, %.1f]\n",
            mean(boot_b1), boot_ci[1], boot_ci[2]))
cat(sprintf("Posterior beta1: mean %.1f  95%% interval [%.1f, %.1f]\n",
            post_b1, ci_b1[1], ci_b1[2]))

bd <- density(boot_b1, n = 512); pd2 <- density(draws$beta1, n = 512)
p_boot_post <- ggplot() +
  geom_line(aes(bd$x, bd$y, color = "Bootstrap"), linewidth = 1.1) +
  geom_line(aes(pd2$x, pd2$y, color = "Posterior"), linewidth = 1.1) +
  scale_color_manual(values = c("Bootstrap" = col_data, "Posterior" = col_post),
                     breaks = c("Bootstrap", "Posterior")) +
  labs(x = expression(beta[1]~"(g per mm)"), y = "Density", color = "") +
  theme_minimal(base_size = 24) + theme(legend.position = "top")
ggsave("figures/boot_vs_posterior.pdf", p_boot_post, width = 7, height = 3.5)

set.seed(123)
idx <- sample(nrow(draws), 80)
p_lines <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(alpha = 0.4, size = 2, color = "gray30") +
  geom_abline(data = draws[idx, ], aes(intercept = beta0, slope = beta1),
              alpha = 0.08, color = col_prior) +
  geom_abline(intercept = b0_ols, slope = b1_ols, color = col_data, linewidth = 1.2) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_minimal(base_size = 24)
ggsave("figures/posterior_lines.pdf", p_lines, width = 5.2, height = 3.9)

################################################################################
# Section 5 -- Prediction for a new penguin (Lecture 20's recipe, applied)
################################################################################
rule("SLIDE 5: Prediction at flipper = 200 mm -- ybar vs ytilde, Bayes vs freq")

x_new  <- 200
y_mean <- draws$beta0 + draws$beta1 * x_new                                  # the LINE
y_pred <- y_mean + rnorm(nrow(draws), 0, draws$sigma)                        # a NEW penguin
pred_mean <- mean(y_pred)
mean_ci   <- quantile(y_mean, c(0.025, 0.975))
pred_ci   <- quantile(y_pred, c(0.025, 0.975))
cat(sprintf("Bayesian at x = %d mm:\n", x_new))
cat(sprintf("  posterior mean of the line  : %.0f g   95%% CI [%.0f, %.0f]  (width %.0f)\n",
            mean(y_mean), mean_ci[1], mean_ci[2], diff(mean_ci)))
cat(sprintf("  posterior predictive, new y : %.0f g   95%% PI [%.0f, %.0f]  (width %.0f)\n",
            pred_mean, pred_ci[1], pred_ci[2], diff(pred_ci)))

y_hat_freq   <- b0_ols + b1_ols * x_new
t_crit       <- qt(0.975, n - 2)
mean_se_freq <- sigma_ols * sqrt(1/n + (x_new - xbar)^2 / Sxx)
pred_se_freq <- sigma_ols * sqrt(1 + 1/n + (x_new - xbar)^2 / Sxx)
freq_mean_lo <- y_hat_freq - t_crit * mean_se_freq; freq_mean_hi <- y_hat_freq + t_crit * mean_se_freq
freq_pred_lo <- y_hat_freq - t_crit * pred_se_freq; freq_pred_hi <- y_hat_freq + t_crit * pred_se_freq
cat(sprintf("Frequentist at x = %d mm (the Lecture 15 formulas):\n", x_new))
cat(sprintf("  point estimate              : %.0f g\n", y_hat_freq))
cat(sprintf("  95%% CI for E[y|x]           : [%.0f, %.0f]  (width %.0f)\n",
            freq_mean_lo, freq_mean_hi, freq_mean_hi - freq_mean_lo))
cat(sprintf("  95%% PI for a new y          : [%.0f, %.0f]  (width %.0f)\n",
            freq_pred_lo, freq_pred_hi, freq_pred_hi - freq_pred_lo))

p_pred <- ggplot() +
  geom_histogram(aes(x = y_pred, y = after_stat(density), fill = "A new penguin"),
                 bins = 60, alpha = 0.45) +
  geom_histogram(aes(x = y_mean, y = after_stat(density), fill = "The line"),
                 bins = 60, alpha = 0.55) +
  scale_fill_manual(values = c("The line" = col_prior,
                               "A new penguin" = col_data),
                    breaks = c("The line", "A new penguin")) +
  labs(x = "Body mass (g)", y = "Density", fill = "") +
  theme_minimal(base_size = 24) + theme(legend.position = "top")
ggsave("figures/posterior_predictive.pdf", p_pred, width = 7, height = 4)

pred_comp <- tibble(
  method = factor(rep(c("Frequentist", "Bayesian"), each = 2), levels = c("Bayesian", "Frequentist")),
  type = factor(rep(c("The line", "A new penguin"), 2),
                levels = c("A new penguin", "The line")),
  estimate = c(y_hat_freq, y_hat_freq, mean(y_mean), pred_mean),
  lower = c(freq_mean_lo, freq_pred_lo, mean_ci[1], pred_ci[1]),
  upper = c(freq_mean_hi, freq_pred_hi, mean_ci[2], pred_ci[2]))
p_pred_comp <- ggplot(pred_comp, aes(y = type, x = estimate,
                                     xmin = lower, xmax = upper, color = method)) +
  geom_pointrange(linewidth = 1, size = 1.2) +
  # one row per quantity inside a panel per framework: stacking both into a
  # two-line y label ran the four labels into each other
  facet_wrap(~ method, ncol = 1) +
  scale_color_manual(values = c("Frequentist" = col_data, "Bayesian" = col_prior)) +
  labs(x = "Body mass (g)", y = "", color = "") +
  theme_minimal(base_size = 22) + theme(legend.position = "none")
ggsave("figures/pred_comparison.pdf", p_pred_comp, width = 5.4, height = 3.3)

cat("\nAll figures written to figures/\n")
