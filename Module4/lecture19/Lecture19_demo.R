# MA214 Lecture 19 -- Posterior approximation
# In-class demo + figure generation.
#
# The whale-sightings rate from Lecture 18, now with a NON-conjugate log-normal
# prior: no closed-form posterior, so we approximate it two ways --
#   grid approximation  (turn the integral into a sum), and
#   Metropolis-Hastings MCMC  (sample from the posterior).
#
# Data (frozen by data/prepare_data.R):
#   data/whale_sightings.csv   7 days, 31 sightings total.
#
# Every number that appears on a slide is printed below.
# MCMC is random -- set.seed makes the printed numbers and the figures match
# the deck exactly. Run from this directory:  Rscript Lecture19_demo.R

library(tidyverse)

dir.create("figures", showWarnings = FALSE)
pdf(NULL)                      # keep a stray Rplots.pdf from appearing

col_prior <- "#569BBD"
col_data  <- "#F05133"
col_post  <- "black"
have_pw   <- requireNamespace("patchwork", quietly = TRUE)
if (have_pw) library(patchwork)

rule <- function(s) cat("\n", strrep("=", 72), "\n", s, "\n", strrep("=", 72), "\n", sep = "")

####################################################################
# Data and model settings
####################################################################

whales  <- read_csv("data/whale_sightings.csv", show_col_types = FALSE)
n_days  <- nrow(whales)
sum_x   <- sum(whales$sightings)
xbar    <- sum_x / n_days

alpha_0 <- 6; beta_0 <- 2          # conjugate Gamma(6,2) prior (mean 3/day)
alpha_post <- alpha_0 + sum_x      # 37
beta_post  <- beta_0 + n_days      # 9

meanlog <- log(3); sdlog <- 0.5    # non-conjugate log-normal prior

rule("SETUP")
cat("whale data: n =", n_days, " sum x_i =", sum_x, " xbar =", round(xbar, 4), "\n")
cat("conjugate posterior (Lecture 18): Gamma(", alpha_post, ",", beta_post, ")\n")

####################################################################
# Grid approximation over a range of grid sizes  (Poisson likelihood)
#   posterior(lambda) proportional to  prior(lambda) * lambda^(sum x) * e^(-n lambda)
####################################################################

# log-likelihood kernel, stable to evaluate
loglik <- function(lam) sum_x * log(lam) - n_days * lam

grid_summary <- function(K, logprior, lo = 0.01, hi = 12) {
  lam <- seq(lo, hi, length.out = K)
  lp  <- logprior(lam) + loglik(lam)
  w   <- exp(lp - max(lp)); w <- w / sum(w)
  m   <- sum(lam * w)
  v   <- sum((lam - m)^2 * w)
  cq  <- cumsum(w)
  ci  <- c(lam[min(which(cq >= 0.025))], lam[min(which(cq >= 0.975))])
  list(lambda = lam, w = w, mean = m, sd = sqrt(v), ci = ci,
       p_gt3 = sum(w[lam > 3]))
}

lp_gamma <- function(lam) dgamma(lam, alpha_0, beta_0, log = TRUE)
lp_lnorm <- function(lam) dlnorm(lam, meanlog, sdlog, log = TRUE)

rule("SLIDE: Sanity Check -- How Many Grid Points?  (conjugate Gamma prior)")
cat("true posterior mean = 37/9 =", round(alpha_post / beta_post, 4), "\n")
for (K in c(5, 11, 101, 1001)) {
  g <- grid_summary(K, lp_gamma)
  cat(sprintf("  K = %4d : grid posterior mean = %.4f\n", K, g$mean))
}
cat("By K = 101 the grid mean already matches 4.11 to two decimals.\n")

rule("SLIDE: Applying Grid to the Log-Normal Prior  (comparison table)")
gL <- grid_summary(20001, lp_lnorm)
gG <- grid_summary(20001, lp_gamma)
cat("                          Log-Normal prior      Gamma prior (conjugate)\n")
cat(sprintf("  prior mean              %.2f                  %.2f\n",
            exp(meanlog + sdlog^2 / 2), alpha_0 / beta_0))
cat(sprintf("  prior 95%% CI            [%.1f, %.1f]            [%.1f, %.1f]\n",
            qlnorm(.025, meanlog, sdlog), qlnorm(.975, meanlog, sdlog),
            qgamma(.025, alpha_0, beta_0), qgamma(.975, alpha_0, beta_0)))
cat(sprintf("  posterior mean          %.2f                  %.2f  (exact 37/9 = %.4f)\n",
            gL$mean, gG$mean, alpha_post / beta_post))
cat(sprintf("  posterior 95%% CI        [%.1f, %.2f]          [%.1f, %.1f]  (exact [%.2f, %.2f])\n",
            gL$ci[1], gL$ci[2], gG$ci[1], gG$ci[2],
            qgamma(.025, alpha_post, beta_post), qgamma(.975, alpha_post, beta_post)))
cat(sprintf("  P(lambda > 3 | x)       %.2f                  %.2f  (exact %.4f)\n",
            gL$p_gt3, gG$p_gt3, pgamma(3, alpha_post, beta_post, lower.tail = FALSE)))

####################################################################
# Metropolis-Hastings MCMC on the log-normal posterior
####################################################################

log_post_ln <- function(lam) {
  if (lam <= 0) return(-Inf)
  dlnorm(lam, meanlog, sdlog, log = TRUE) + loglik(lam)
}

run_rwmh <- function(M, step, start = 3.0, seed = 214) {
  set.seed(seed)
  x <- numeric(M); x[1] <- start; acc <- 0
  for (m in 2:M) {
    prop <- rnorm(1, x[m - 1], step)
    if (log(runif(1)) < log_post_ln(prop) - log_post_ln(x[m - 1])) {
      x[m] <- prop; acc <- acc + 1
    } else x[m] <- x[m - 1]
  }
  list(x = x, acc = acc / (M - 1))
}

M <- 5000; burn <- 500
good  <- run_rwmh(M, step = 2.0)     # well-tuned: acceptance ~0.4
small <- run_rwmh(M, step = 0.01)    # step too small
large <- run_rwmh(M, step = 8.0)     # step too large

rule("SLIDE: MCMC output -- acceptance rates")
cat(sprintf("  well-tuned  (step 2.0) : acceptance %.2f   <- healthy (aim ~0.2-0.5)\n", good$acc))
cat(sprintf("  step too small (0.01)  : acceptance %.2f   <- accepts almost everything, but crawls\n", small$acc))
cat(sprintf("  step too large (8.0)   : acceptance %.2f   <- rejects almost everything, gets stuck\n", large$acc))

draws <- good$x[-(1:burn)]
cat(sprintf("\nposterior summaries from %d post-burn-in draws:\n", length(draws)))
cat(sprintf("  mean %.3f  SD %.3f  95%% interval [%.2f, %.2f]\n",
            mean(draws), sd(draws), quantile(draws, .025), quantile(draws, .975)))
cat(sprintf("  (grid gave mean %.3f, CI [%.2f, %.2f] -- MCMC agrees)\n",
            gL$mean, gL$ci[1], gL$ci[2]))

# Effective sample size (rough), to make the ESS idea concrete
acf1 <- acf(draws, plot = FALSE, lag.max = 1)$acf[2]
cat(sprintf("  lag-1 autocorrelation %.2f -> draws are correlated, so ESS < %d actual draws\n",
            acf1, length(draws)))

####################################################################
# FIGURES  (only those the deck / worksheet use)
####################################################################

lam_fine <- seq(0.01, 12, length.out = 600)

## --- Prior comparison: log-normal vs Gamma ---
prior_df <- bind_rows(
  tibble(lambda = lam_fine, density = dgamma(lam_fine, alpha_0, beta_0),
         prior = "Gamma(6, 2)  [conjugate]"),
  tibble(lambda = lam_fine, density = dlnorm(lam_fine, meanlog, sdlog),
         prior = "LogNormal")
)
p_priors <- ggplot(prior_df, aes(lambda, density, colour = prior, linetype = prior)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c(col_prior, col_data)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 4, 8, 12)) +
  labs(x = expression(lambda~"(sightings/day)"), y = "Density",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 24) + theme(legend.position = "top")
ggsave("figures/lognormal_vs_gamma.pdf", p_priors, width = 7, height = 4)

## --- Bimodal prior (motivation: no conjugate family can be bimodal) ---
mixture_prior <- function(p) 0.5 * dbeta(p, 6, 34) + 0.5 * dbeta(p, 22, 18)
pg <- seq(0, 1, length.out = 2001)
p_bim <- ggplot(tibble(p = pg, density = mixture_prior(pg)), aes(p, density)) +
  geom_area(fill = col_prior, alpha = 0.15) +
  geom_line(linewidth = 1.2, colour = col_prior) +
  labs(x = expression(p~"(a probability)"), y = "Density") +
  theme_minimal(base_size = 24)
ggsave("figures/bimodal_prior.pdf", p_bim, width = 7, height = 3.6)

## --- Grid idea: 4-panel prior/likelihood/product/normalize ---
K_demo <- 11
lam_d  <- seq(0.5, 10, length.out = K_demo)
prior_d <- dgamma(lam_d, alpha_0, beta_0)
like_d  <- exp(loglik(lam_d) - max(loglik(lam_d)))
unn_d   <- prior_d * like_d
q_d     <- unn_d / sum(unn_d)
true_fine <- dgamma(lam_fine, alpha_post, beta_post)
bw <- diff(lam_d[1:2]) * 0.7
panel <- function(xx, yy, ttl, fill, ref = NULL) {
  g <- ggplot()
  if (!is.null(ref)) g <- g + geom_line(aes(lam_fine, ref), colour = "gray45",
                                        linetype = "dashed", linewidth = 0.8)
  g + geom_col(aes(xx, yy), width = bw, fill = fill, alpha = 0.65) +
    geom_point(aes(xx, yy), size = 1.8, colour = fill) +
    labs(title = ttl, x = expression(lambda), y = NULL) +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    theme_minimal(base_size = 20) +
    theme(plot.title = element_text(face = "bold"))
}
g1 <- panel(lam_d, prior_d, "1. Prior", col_prior)
g2 <- panel(lam_d, like_d / max(like_d) * max(prior_d), "2. Likelihood", col_data)
g3 <- panel(lam_d, unn_d / max(unn_d) * max(true_fine), "3. Multiply", "mediumpurple")
g4 <- panel(lam_d, q_d / diff(lam_d[1:2]), "4. Normalize", "mediumpurple",
            ref = true_fine)
# 2x2, not 1x4: four panels across a 4.3in slide width left the text at ~6pt.
p_grid_idea <- if (have_pw) g1 + g2 + g3 + g4 + plot_layout(nrow = 2) else g1
ggsave("figures/grid_idea.pdf", p_grid_idea, width = 9, height = 5.45)

## --- Grid refinement (Poisson, conjugate) toward true Gamma(37,9) ---
refine <- bind_rows(lapply(c(5, 11, 101, 1001), function(K) {
  g <- grid_summary(K, lp_gamma)
  tibble(lambda = g$lambda, density = g$w * (K - 1) / (12 - 0.01),
         Grid = factor(paste0("K = ", K),
                       levels = paste0("K = ", c(5, 11, 101, 1001))))
}))
p_refine <- ggplot(refine, aes(lambda, density)) +
  geom_function(aes(colour = "True posterior", linetype = "True posterior"),
                fun = \(x) dgamma(x, alpha_post, beta_post), linewidth = 0.8) +
  geom_line(aes(colour = "Grid", linetype = "Grid"), linewidth = 0.9) +
  geom_point(colour = col_data, size = 1.1) +
  facet_wrap(~ Grid, nrow = 1) +
  scale_colour_manual(values = c(Grid = col_data, `True posterior` = col_prior)) +
  scale_linetype_manual(values = c(Grid = "solid", `True posterior` = "dashed")) +
  # Five ticks per panel ran into each other once the type was slide-legible.
  scale_x_continuous(breaks = c(0, 5, 10)) +
  labs(x = expression(lambda), y = "Density", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 24) + theme(legend.position = "top")
ggsave("figures/grid_refinement_poisson.pdf", p_refine, width = 8.5, height = 3.33)

## --- Log-normal posterior via grid, vs conjugate ---
post_ln_dens <- gL$w * (length(gL$lambda) - 1) / (12 - 0.01)
p_ln <- bind_rows(
  tibble(lambda = gL$lambda, density = dlnorm(gL$lambda, meanlog, sdlog),
         curve = "Prior (log-normal)"),
  tibble(lambda = gL$lambda, density = post_ln_dens,
         curve = "Posterior: log-normal"),
  tibble(lambda = gL$lambda, density = dgamma(gL$lambda, alpha_post, beta_post),
         curve = "Posterior: Gamma")
) %>%
  mutate(curve = factor(curve, levels = c("Prior (log-normal)",
                                          "Posterior: log-normal",
                                          "Posterior: Gamma"))) %>%
  ggplot(aes(lambda, density, colour = curve, linetype = curve)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c(col_prior, col_data, col_post)) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  coord_cartesian(xlim = c(0, 9)) +
  labs(x = expression(lambda~"(sightings/day)"), y = "Density",
       colour = NULL, linetype = NULL) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 22) + theme(legend.position = "top")
ggsave("figures/lognormal_posterior.pdf", p_ln, width = 7.2, height = 4.3)

## --- RWMH step-by-step (accept / reject), on the log-normal posterior ---
post_curve <- tibble(lambda = gL$lambda, density = post_ln_dens)
y_max <- max(post_ln_dens) * 1.08
dens_at <- function(lam) approx(post_curve$lambda, post_curve$density, lam)$y
lam_cur  <- 3.5; lam_p1 <- 4.7; lam_p2 <- 6.0
d_cur <- dens_at(lam_cur); d_p1 <- dens_at(lam_p1); d_p2 <- dens_at(lam_p2)
base_layer <- function() list(
  geom_area(data = post_curve, aes(lambda, density), fill = "gray85", alpha = 0.5),
  geom_line(data = post_curve, aes(lambda, density), linewidth = 0.9, colour = "gray40"),
  coord_cartesian(xlim = c(0.5, 9), ylim = c(-0.03, y_max)),
  labs(x = expression(lambda), y = "Density"),
  theme_minimal(base_size = 22),
  theme(plot.title = element_text(face = "bold")))
prop_df <- function(center) { x <- seq(0.5, 9, length.out = 300)
  tibble(lambda = x, density = dnorm(x, center, 0.75) * y_max * 0.28 / dnorm(center, center, 0.75)) }

p_pa <- ggplot() + base_layer() +
  geom_line(data = prop_df(lam_cur), aes(lambda, density), colour = col_prior, linetype = "dashed", linewidth = 0.9) +
  geom_segment(aes(x = lam_cur, xend = lam_cur, y = 0, yend = d_cur), linetype = "dotted", colour = col_data) +
  geom_segment(aes(x = lam_p1, xend = lam_p1, y = 0, yend = d_p1), linetype = "dotted", colour = col_data) +
  geom_point(aes(lam_cur, 0), size = 5, colour = col_data) +
  geom_point(aes(lam_p1, 0), size = 5, colour = col_data, shape = 1, stroke = 1.5) +
  labs(subtitle = "filled = current 3.5\nopen = proposal 4.7") +
  theme(plot.subtitle = element_text(colour = col_data, face = "bold"))
ggsave("figures/rwmh_propose_accept.pdf", p_pa, width = 4.6, height = 3.4)

p_ac <- ggplot() + base_layer() +
  geom_segment(aes(x = lam_cur + 0.08, xend = lam_p1 - 0.08, y = 0, yend = 0),
               arrow = arrow(length = unit(0.18, "inches"), type = "closed"), linewidth = 1.3, colour = col_data) +
  geom_point(aes(lam_cur, 0), size = 5, colour = "gray50", shape = 1, stroke = 1.2) +
  geom_point(aes(lam_p1, 0), size = 5, colour = col_data) +
  labs(subtitle = sprintf("r = %.2f (at least 1)\nACCEPT", d_p1 / d_cur)) +
  theme(plot.subtitle = element_text(colour = col_data, face = "bold"))
ggsave("figures/rwmh_accept.pdf", p_ac, width = 4.6, height = 3.4)

p_pr <- ggplot() + base_layer() +
  geom_line(data = prop_df(lam_p1), aes(lambda, density), colour = col_prior, linetype = "dashed", linewidth = 0.9) +
  geom_segment(aes(x = lam_p1, xend = lam_p1, y = 0, yend = d_p1), linetype = "dotted", colour = col_data) +
  geom_segment(aes(x = lam_p2, xend = lam_p2, y = 0, yend = d_p2), linetype = "dotted", colour = col_data) +
  geom_point(aes(lam_p1, 0), size = 5, colour = col_data) +
  geom_point(aes(lam_p2, 0), size = 5, colour = col_data, shape = 1, stroke = 1.5) +
  labs(subtitle = "filled = current 4.7\nopen = proposal 6.0") +
  theme(plot.subtitle = element_text(colour = col_data, face = "bold"))
ggsave("figures/rwmh_propose_reject.pdf", p_pr, width = 4.6, height = 3.4)

p_rj <- ggplot() + base_layer() +
  geom_segment(aes(x = lam_p1 + 0.08, xend = lam_p2 - 0.08, y = 0, yend = 0),
               arrow = arrow(length = unit(0.15, "inches"), type = "closed"), linewidth = 0.8, colour = "gray50", linetype = "dashed") +
  geom_point(aes(lam_p2, 0), size = 5, colour = col_data, shape = 4, stroke = 2) +
  geom_point(aes(lam_p1, 0), size = 5, colour = col_data) +
  labs(subtitle = sprintf("r = %.2f (well under 1)\nlikely REJECT", d_p2 / d_p1)) +
  theme(plot.subtitle = element_text(colour = col_data, face = "bold"))
ggsave("figures/rwmh_reject.pdf", p_rj, width = 4.6, height = 3.4)

## --- Trace + histogram, three chains: good / too small / too large ---
trace_hist <- function(chain, ttl) {
  tr <- ggplot(tibble(iter = seq_along(chain$x), lambda = chain$x), aes(iter, lambda)) +
    geom_line(colour = col_prior, alpha = 0.75) +
    coord_cartesian(ylim = c(1, 8)) +
    labs(title = ttl, x = "Iteration", y = expression(lambda)) +
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(face = "bold"))
  hi <- ggplot(tibble(lambda = chain$x[-(1:burn)]), aes(lambda)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = col_data, alpha = 0.6) +
    geom_line(data = post_curve, aes(lambda, density), colour = col_post, linewidth = 0.9) +
    coord_cartesian(xlim = c(1, 8)) +
    labs(title = "Draws vs posterior",
         x = expression(lambda), y = "Density") +
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(face = "bold"))
  if (have_pw) tr + hi else tr
}
ggsave("figures/mcmc_trace_good.pdf",  trace_hist(good,  "Well tuned"),        width = 9, height = 3.6)
ggsave("figures/mcmc_trace_small.pdf", trace_hist(small, "Step too small"),     width = 9, height = 3.6)
ggsave("figures/mcmc_trace_large.pdf", trace_hist(large, "Step too large"),     width = 9, height = 3.6)

rule("Figures written")
cat(paste0("figures/", c("lognormal_vs_gamma", "bimodal_prior", "grid_idea",
  "grid_refinement_poisson", "lognormal_posterior", "rwmh_propose_accept",
  "rwmh_accept", "rwmh_propose_reject", "rwmh_reject", "mcmc_trace_good",
  "mcmc_trace_small", "mcmc_trace_large"), ".pdf", collapse = "\n"), "\n")
