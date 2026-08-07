# MA214 Lecture 20 -- Posterior Inference & Prediction
# Run from the lecture directory:  Rscript Lecture20_demo.R
#
# Lecture 20 finally USES the posteriors from Lectures 18-19. Every number that
# appears on a slide is printed below, and every figure is written to figures/.
# The three running examples (beta-binomial drug trial, normal-normal
# birthweights, gamma-poisson whale sightings) come from data/ via prepare_data.R.
#
#   Section 1  Point estimation          -> figures/point_estimate.pdf
#   Section 2  Credible intervals        -> figures/credible_interval.pdf
#   Section 3  Hypothesis testing        -> figures/hypothesis_region.pdf
#   Section 4  Model comparison          -> figures/model_comparison.pdf
#   Section 5  Posterior prediction      -> figures/posterior_predictive_whale.pdf
#   Section 6  Predictive checking       -> figures/predictive_check.pdf
#
# MCMC is not needed here -- these posteriors are conjugate and closed-form, so
# the sampling in Sections 4-5 is ordinary Monte Carlo (seeded for reproducibility).

suppressPackageStartupMessages(library(tidyverse))
dir.create("figures", showWarnings = FALSE)

col_prior <- "#569BBD"   # blue
col_like  <- "#E07B39"   # orange (data / likelihood)
col_post  <- "#2C3E50"   # dark slate (posterior)
col_shade <- "#B03A2E"   # red (highlighted region)

rule <- function(t) cat("\n", strrep("-", 66), "\n", t, "\n", strrep("-", 66), "\n", sep = "")
sval <- function(p) -log2(p)          # s-value: bits of evidence, s = -log2(p_Bayes)

################################################################################
# Section 1 -- Point estimation (posterior mean vs MLE)
################################################################################
rule("SLIDE 1: Point estimate -- posterior mean vs MLE (drug trial)")

trial   <- read_csv("data/drug_trial.csv", show_col_types = FALSE)
n_pat   <- trial$value[trial$parameter == "n"]
x_res   <- trial$value[trial$parameter == "x"]
a0 <- 1; b0 <- 5                       # Banerjee's Beta(1,5) prior (from Lecture 18)
a_post <- a0 + x_res                    # Beta(3, 9)
b_post <- b0 + n_pat - x_res

mle_drug   <- x_res / n_pat
prior_mean <- a0 / (a0 + b0)
post_mean  <- a_post / (a_post + b_post)
cat(sprintf("drug trial: n = %d, x = %d\n", n_pat, x_res))
cat(sprintf("  prior mean     = %.3f   (Beta(%d,%d))\n", prior_mean, a0, b0))
cat(sprintf("  MLE  x/n       = %.3f   (ignores the prior)\n", mle_drug))
cat(sprintf("  posterior mean = %.3f   (Beta(%d,%d)) -- shrunk from the MLE toward the prior\n",
            post_mean, a_post, b_post))

# FIGURE: hold the observed rate fixed at x/n = 1/3 and let n grow. The MLE never
# moves; the posterior mean climbs from the prior mean up to the MLE. This is the
# LO 4.4 point -- MLE vs Bayesian estimation -- NOT Lecture 18's fixed-n shrinkage plot.
n_seq <- 1:60
df_pe <- tibble(
  n    = n_seq,
  mle  = mle_drug,
  post = (a0 + n_seq * mle_drug) / (a0 + b0 + n_seq)
) |> pivot_longer(c(mle, post), names_to = "est", values_to = "value") |>
  mutate(est = factor(est, levels = c("mle", "post"),
                      labels = c("MLE  x/n", "Posterior mean")))
p_pe <- ggplot(df_pe, aes(n, value, colour = est)) +
  geom_hline(yintercept = prior_mean, linetype = "dashed", colour = col_prior) +
  annotate("text", x = 58, y = prior_mean + 0.022, hjust = 1, size = 6.5,
           colour = col_prior, label = "prior mean 0.17") +
  geom_line(linewidth = 1) +
  annotate("point", x = n_pat, y = post_mean, size = 2.6, colour = col_post) +
  annotate("text", x = n_pat + 1.5, y = post_mean - 0.022, hjust = 0, size = 6.5,
           label = "our trial: n = 6, posterior mean 0.25") +
  scale_colour_manual(values = c(col_like, col_post)) +
  labs(x = "Number of patients n",
       y = "Estimate of p", colour = NULL) +
  theme_minimal(base_size = 23) +
  theme(plot.title = element_text(face = "bold", size = 12), legend.position = "top")
ggsave("figures/point_estimate.pdf", p_pe, width = 6.2, height = 3.4)

################################################################################
# Section 2 -- Credible intervals
################################################################################
rule("SLIDE 2a: Credible interval -- drug trial (beta-binomial)")

ci_beta <- qbeta(c(0.025, 0.975), a_post, b_post)
cat(sprintf("posterior Beta(%d, %d), mean = %.3f\n", a_post, b_post, post_mean))
cat(sprintf("95%% credible interval for p: [%.3f, %.3f]\n", ci_beta[1], ci_beta[2]))

rule("SLIDE 2b: Credible interval -- birthweights (normal-normal)")

bw     <- read_csv("data/birthweights.csv", show_col_types = FALSE)
n_bw   <- nrow(bw); xbar_bw <- mean(bw$weight_g)
mu0 <- 3300; sd0 <- 200; sigma <- 500  # prior N(3300, 200^2), known sigma (from Lecture 18)
n0      <- sigma^2 / sd0^2
w0      <- n0 / (n0 + n_bw); w1 <- n_bw / (n0 + n_bw)
mu_n <- w0 * mu0 + w1 * xbar_bw
sigma_n <- sigma / sqrt(n0 + n_bw)
ci_norm <- qnorm(c(0.025, 0.975), mu_n, sigma_n)
cat(sprintf("data: n = %d, xbar = %.0f g\n", n_bw, xbar_bw))
cat(sprintf("posterior N(%.0f, %.0f^2)\n", mu_n, sigma_n))
cat(sprintf("95%% credible interval for mu: [%.0f, %.0f] g\n", ci_norm[1], ci_norm[2]))

# FIGURE: two posteriors, each with its central 95% region shaded
pg <- seq(0.001, 0.999, length.out = 600)
ng <- seq(mu_n - 4*sigma_n, mu_n + 4*sigma_n, length.out = 600)
df_ci <- bind_rows(
  tibble(panel = "Drug:~~response~rate~p",
         x = pg, dens = dbeta(pg, a_post, b_post),
         inside = pg >= ci_beta[1] & pg <= ci_beta[2]),
  tibble(panel = "Birthweight:~~mean~mu~(g)",
         x = ng, dens = dnorm(ng, mu_n, sigma_n),
         inside = ng >= ci_norm[1] & ng <= ci_norm[2])
) |> mutate(panel = factor(panel, levels = c("Drug:~~response~rate~p",
                                             "Birthweight:~~mean~mu~(g)")))
p_ci <- ggplot(df_ci, aes(x, dens)) +
  geom_area(data = ~filter(.x, inside), fill = col_post, alpha = 0.25) +
  geom_line(colour = col_post, linewidth = 0.9) +
  facet_wrap(~panel, scales = "free", labeller = label_parsed) +
  # Five ticks crowded the birthweight panel and the last one was clipped.
  scale_x_continuous(n.breaks = 4) +
  labs(x = "Parameter", y = "Posterior density") +
  theme_minimal(base_size = 23) +
  theme(strip.text = element_text(size = rel(0.85)))
ggsave("figures/credible_interval.pdf", p_ci, width = 8, height = 3.4)

################################################################################
# Section 3 -- Hypothesis testing (posterior tail probability)
################################################################################
rule("SLIDE 3a: Hypothesis test -- drug, H0: p <= 0.15")

pB_drug <- pbeta(0.15, a_post, b_post)      # P(p <= 0.15 | data)
cat(sprintf("p_Bayes = P(p <= 0.15 | data) = %.3f\n", pB_drug))
cat(sprintf("s_Bayes = -log2(p_Bayes)      = %.2f bits of evidence against H0\n", sval(pB_drug)))

rule("SLIDE 3b: Hypothesis test -- whale, H0: lambda <= 3 (historical baseline)")

whale  <- read_csv("data/whale_sightings.csv", show_col_types = FALSE)
S <- sum(whale$sightings); n_w <- nrow(whale)
ga0 <- 6; gb0 <- 2                          # Gamma(6,2) prior (from Lecture 18)
ga_post <- ga0 + S; gb_post <- gb0 + n_w    # Gamma(37, 9)
pB_whale <- pgamma(3, ga_post, gb_post)      # P(lambda <= 3 | data)
cat(sprintf("data: n = %d, sum x = %d, xbar = %.2f/day\n", n_w, S, S/n_w))
cat(sprintf("posterior Gamma(%d, %d), mean = %.2f/day\n", ga_post, gb_post, ga_post/gb_post))
cat(sprintf("p_Bayes = P(lambda <= 3 | data) = %.4f\n", pB_whale))
cat(sprintf("s_Bayes = -log2(p_Bayes)        = %.2f bits of evidence against H0\n", sval(pB_whale)))

# FIGURE: two posteriors, each with the H0 region shaded
bg <- seq(0.001, 0.7, length.out = 600)
lg <- seq(0.001, 9, length.out = 600)
df_hyp <- bind_rows(
  tibble(panel = "Drug:~~H[0]:~p<=0.15", x = bg, dens = dbeta(bg, a_post, b_post),
         h0 = bg <= 0.15),
  tibble(panel = "Whale:~~H[0]:~lambda<=3", x = lg, dens = dgamma(lg, ga_post, gb_post),
         h0 = lg <= 3)
) |> mutate(panel = factor(panel, levels = c("Drug:~~H[0]:~p<=0.15", "Whale:~~H[0]:~lambda<=3")))
p_hyp <- ggplot(df_hyp, aes(x, dens)) +
  geom_area(data = ~filter(.x, h0), fill = col_shade, alpha = 0.35) +
  geom_line(colour = col_post, linewidth = 0.9) +
  facet_wrap(~panel, scales = "free", labeller = label_parsed) +
  scale_x_continuous(n.breaks = 4) +
  labs(x = expression(paste("Parameter   (shaded = the ", H[0], " region)")), y = "Posterior density") +
  theme_minimal(base_size = 23) +
  theme(strip.text = element_text(size = rel(0.85)))
ggsave("figures/hypothesis_region.pdf", p_hyp, width = 8, height = 3.4)

################################################################################
# Section 4 -- Model comparison (posterior model probability)
################################################################################
rule("SLIDE 4: Model comparison -- whale rate, baseline vs surge")

# Two competing gamma-poisson models for the same 7 days of counts.
# Marginal likelihood = prior predictive probability of the observed data:
#   log m(x) = -sum lfactorial(x) + lgamma(a+S) - lgamma(a) + a log b - (a+S) log(b+n)
log_marg <- function(a, b, x) {
  S <- sum(x); n <- length(x)
  -sum(lfactorial(x)) + lgamma(a + S) - lgamma(a) + a*log(b) - (a + S)*log(b + n)
}
x_w <- whale$sightings
models <- tibble(
  model = c("A: baseline", "B: surge"),
  a = c(6, 20), b = c(2, 4),          # Gamma(6,2) mean 3 ; Gamma(20,4) mean 5
  prior = c(0.5, 0.5)
) |>
  mutate(logm = map2_dbl(a, b, ~log_marg(.x, .y, x_w)),
         prior_mean = a / b,
         # marginal likelihoods are tiny (e^-15); rescale so the larger reads 1.00.
         # Rescaling cancels in the normalization, so the posterior is unchanged.
         rel_lik = exp(logm - max(logm)),
         product = prior * rel_lik,               # prior x likelihood
         post    = product / sum(product))        # normalize
cat("SLIDE TABLE (prior -> likelihood -> product -> posterior):\n")
print(models |> select(model, prior, rel_lik, product, post) |>
        mutate(across(where(is.numeric), ~round(.x, 3))))
cat(sprintf("\nraw log marginal likelihoods: A = %.2f, B = %.2f\n", models$logm[1], models$logm[2]))
cat(sprintf("Posterior model probabilities: A = %.3f, B = %.3f\n",
            models$post[1], models$post[2]))

p_mc <- models |>
  select(model, Prior = prior, Posterior = post) |>
  pivot_longer(-model, names_to = "stage", values_to = "prob") |>
  mutate(stage = factor(stage, levels = c("Prior", "Posterior"))) |>
  ggplot(aes(stage, prob, fill = model)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", prob)),
            position = position_dodge(0.7), vjust = -0.4, size = 7.2) +
  scale_fill_manual(values = c(col_prior, col_shade)) +
  ylim(0, 1) +
  labs(x = NULL, y = "Model probability", fill = NULL) +
  theme_minimal(base_size = 23) +
  theme(plot.title = element_text(face = "bold", size = 12), legend.position = "top")
ggsave("figures/model_comparison.pdf", p_mc, width = 5.5, height = 3.2)

################################################################################
# Section 5 -- Posterior prediction (next day's whale count)
################################################################################
rule("SLIDE 5: Posterior prediction -- next day's whale sightings")

set.seed(214)
M <- 100000
lam_draws  <- rgamma(M, ga_post, gb_post)     # posterior draws of lambda
ypred      <- rpois(M, lam_draws)             # posterior predictive draws
xbar_w     <- S / n_w
yplug      <- rpois(M, xbar_w)                # frequentist plug-in: Poisson(xbar)
pi_pred    <- quantile(ypred, c(0.025, 0.975))
cat(sprintf("posterior predictive mean = %.2f (plug-in Poisson mean = %.2f)\n",
            mean(ypred), xbar_w))
cat(sprintf("95%% posterior predictive interval for next day: [%d, %d]\n",
            as.integer(pi_pred[1]), as.integer(pi_pred[2])))
cat(sprintf("P(>= 8 sightings tomorrow | data) = %.3f (Bayesian) vs %.3f (plug-in)\n",
            mean(ypred >= 8), mean(yplug >= 8)))
cat(sprintf("SD of predictive: Bayesian %.2f vs plug-in %.2f (averaging adds parameter uncertainty)\n",
            sd(ypred), sd(yplug)))

kmax <- 13
pmf <- bind_rows(
  tibble(k = 0:kmax, prob = as.numeric(table(factor(ypred, levels = 0:kmax)))/M,
         which = "Bayesian predictive"),
  tibble(k = 0:kmax, prob = as.numeric(table(factor(yplug, levels = 0:kmax)))/M,
         which = "Frequentist plug-in")
)
p_pred <- ggplot(pmf, aes(k, prob, fill = which)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  scale_fill_manual(values = c(col_post, col_like)) +
  scale_x_continuous(breaks = seq(0, kmax, by = 2)) +
  labs(x = "Sightings tomorrow", y = "Probability", fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 23) +
  theme(plot.title = element_text(face = "bold", size = 12), legend.position = "top")
ggsave("figures/posterior_predictive_whale.pdf", p_pred, width = 5.6, height = 3.4)

################################################################################
# Section 6 -- Predictive checking (does the model reproduce the data?)
################################################################################
rule("SLIDE 6: Predictive checking -- simulate replicate weeks, compare to data")

set.seed(2140)
R <- 5000
# each replicate: draw lambda from the posterior, then simulate a fresh week
rep_max <- replicate(R, {
  lam <- rgamma(1, ga_post, gb_post)
  max(rpois(n_w, lam))
})
T_obs <- max(whale$sightings)                 # busiest observed day = 7
pB_check <- mean(rep_max >= T_obs)            # Bayesian predictive p-value
cat(sprintf("test statistic T = busiest day's count; observed T_obs = %d\n", T_obs))
cat(sprintf("predictive p-value P(T_rep >= T_obs) = %.3f\n", pB_check))
cat(sprintf("  (near 0.5 = data look typical of the model; near 0 or 1 = misfit)\n"))

p_check <- tibble(T = rep_max) |>
  ggplot(aes(T)) +
  geom_bar(aes(y = after_stat(prop)), fill = col_prior, alpha = 0.8) +
  geom_vline(xintercept = T_obs, colour = col_shade, linewidth = 1.1) +
  annotate("text", x = T_obs + 0.2, y = Inf, hjust = 0, vjust = 1.4,
           label = "observed = 7", colour = col_shade, size = 7.6) +
  scale_x_continuous(breaks = seq(2, 14, by = 3)) +
  labs(x = "Busiest day's count", y = "Proportion") +
  theme_minimal(base_size = 23) +
  theme(plot.title = element_text(face = "bold", size = 12))
ggsave("figures/predictive_check.pdf", p_check, width = 5.0, height = 3.2)

cat("\nAll figures written to figures/\n")
