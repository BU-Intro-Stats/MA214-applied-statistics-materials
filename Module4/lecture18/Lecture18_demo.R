# MA214 Lecture 18 -- Conjugate models
# In-class demo + figure generation.
#
# Three conjugate families, one pattern:
#   data/drug_trial.csv       beta-binomial   (the archetype; carried over from Lecture 17)
#   data/oncologists.csv      five Beta priors, one dataset, five posteriors
#   data/birthweights.csv     normal-normal
#   data/whale_sightings.csv  gamma-poisson
#
# Every number that appears on a slide is printed below.
# Run from this directory:  Rscript Lecture18_demo.R

library(tidyverse)

dir.create("figures", showWarnings = FALSE)
pdf(NULL)                      # keep a stray Rplots.pdf from appearing

col_prior <- "#569BBD"
col_data  <- "#F05133"
col_post  <- "black"

rule <- function(s) cat("\n", strrep("=", 72), "\n", s, "\n", strrep("=", 72), "\n", sep = "")

####################################################################
# The pattern, in one function
#
# Every conjugate model on these slides has a posterior mean that is a
# weighted average of the prior mean and the MLE, with weights set by a
# "prior sample size" n0 against the real sample size n. This helper is
# used below to CHECK that claim numerically for all three families --
# it is the spine of the lecture, so it should not be taken on faith.
####################################################################

weighted_mean_check <- function(n0, n, prior_mean, mle) {
  (n0 / (n0 + n)) * prior_mean + (n / (n0 + n)) * mle
}

####################################################################
# Section 2 -- Beta-Binomial: the archetype
####################################################################

trial <- read_csv("data/drug_trial.csv", show_col_types = FALSE)
n_pat <- trial$value[trial$parameter == "n"]
x_res <- trial$value[trial$parameter == "x"]

alpha0 <- 1; beta0 <- 5          # Banerjee's prior: expects a low response rate

rule("SLIDE: Example: Back to the Drug Trial")
cat("Prior  Beta(", alpha0, ",", beta0, ")  mean =", round(alpha0 / (alpha0 + beta0), 4),
    " -> slide shows 1/6 ~= 0.167\n")
cat("Data   n =", n_pat, ", x =", x_res, "  MLE =", round(x_res / n_pat, 4),
    " -> slide shows 2/6 ~= 0.333\n")
a_post <- alpha0 + x_res
b_post <- beta0 + n_pat - x_res
cat("Posterior Beta(", alpha0, "+", x_res, ",", beta0, "+", n_pat - x_res, ") = Beta(",
    a_post, ",", b_post, ")\n")
post_mean <- a_post / (a_post + b_post)
cat("Posterior mean =", a_post, "/", a_post + b_post, "=", round(post_mean, 4),
    " -> slide shows 0.250\n")

# The pattern: for beta-binomial the prior sample size is alpha + beta.
cat("\nPattern check: n0 = alpha+beta =", alpha0 + beta0, ", n =", n_pat, "\n")
cat("  weights", round((alpha0 + beta0) / (alpha0 + beta0 + n_pat), 4), "on prior /",
    round(n_pat / (alpha0 + beta0 + n_pat), 4), "on data\n")
chk <- weighted_mean_check(alpha0 + beta0, n_pat, alpha0 / (alpha0 + beta0), x_res / n_pat)
cat("  weighted average =", round(chk, 4), "== posterior mean", round(post_mean, 4), "\n")
stopifnot(all.equal(chk, post_mean))

####################################################################
# FIGURE: Beta shapes
####################################################################

pi_grid <- seq(0.001, 0.999, length.out = 500)
shape_params <- tribble(
  ~alpha, ~beta,
   1,      1,
   1,      5,
   2,      2,
   5,      5,
   5,      2,
   0.5,    0.5,
   3,      7,
   20,     20
) %>% mutate(label = paste0("Beta(", alpha, ", ", beta, ")"))

beta_data <- shape_params %>%
  rowwise() %>%
  mutate(d = list(tibble(p = pi_grid, density = dbeta(pi_grid, alpha, beta)))) %>%
  unnest(d) %>%
  ungroup() %>%
  mutate(label = factor(label, levels = shape_params$label))

p_beta <- ggplot(beta_data, aes(p, density)) +
  geom_line(linewidth = 0.9, colour = col_prior) +
  facet_wrap(~ label, nrow = 2, scales = "free_y") +
  # Eight panels across 8.4in leaves ~1in each: five x labels run into one
  # another once the type is large enough to read on a slide.
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  # Start every panel at 0. With a free lower limit the flat Beta(1,1) got an
  # axis of 0.95-1.05, which reads as structure rather than as "flat".
  expand_limits(y = 0) +
  # Three breaks per panel. With the default five, the free y-axis on the
  # Beta(0.5, 0.5) panel ran "0.0 2.5 5.0 7.5 10.0" and ate enough width to
  # clip that panel's strip label.
  scale_y_continuous(n.breaks = 3) +
  labs(x = "p", y = "Density") +
  theme_minimal(base_size = 21) +
  theme(strip.text = element_text(size = rel(0.78)))

ggsave("figures/beta_shapes.pdf", p_beta, width = 8.4, height = 3.9)

####################################################################
# FIGURE: the beta-binomial update
####################################################################

update_df <- bind_rows(
  tibble(p = pi_grid, density = dbeta(pi_grid, alpha0, beta0), part = "Prior  Beta(1, 5)"),
  tibble(p = pi_grid, density = dbeta(pi_grid, 1 + x_res, 1 + n_pat - x_res),
         part = "Likelihood (rescaled)"),
  tibble(p = pi_grid, density = dbeta(pi_grid, a_post, b_post),
         part = "Posterior  Beta(3, 9)")
) %>%
  mutate(part = factor(part, levels = c("Prior  Beta(1, 5)", "Likelihood (rescaled)",
                                        "Posterior  Beta(3, 9)")))

p_upd <- ggplot(update_df, aes(p, density, colour = part, linetype = part)) +
  geom_line(linewidth = 1.05) +
  scale_colour_manual(values = c(col_prior, col_data, col_post)) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  labs(x = "p  (response rate)", y = "Density", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "right")

ggsave("figures/beta_binomial_update.pdf", p_upd, width = 7.4, height = 3.9)

####################################################################
# Section 3 -- The conjugate pattern: five priors, one dataset
####################################################################

onc <- read_csv("data/oncologists.csv", show_col_types = FALSE) %>%
  mutate(a_post = alpha + x_res,
         b_post = beta + n_pat - x_res,
         prior_mean = alpha / (alpha + beta),
         post_mean  = a_post / (a_post + b_post),
         n0         = alpha + beta)

rule("SLIDE: Five Priors, One Dataset, Five Posteriors")
print(onc %>%
        transmute(oncologist,
                  prior     = paste0("Beta(", alpha, ", ", beta, ")"),
                  posterior = paste0("Beta(", a_post, ", ", b_post, ")"),
                  post_mean = round(post_mean, 3)), n = Inf)
cat("\nEvery one is the same weighted average (n0 = alpha+beta against n = 6):\n")
onc_chk <- onc %>%
  mutate(chk = weighted_mean_check(n0, n_pat, prior_mean, x_res / n_pat))
print(onc_chk %>% transmute(oncologist, n0,
                            weight_prior = round(n0 / (n0 + n_pat), 3),
                            weighted_avg = round(chk, 3),
                            post_mean    = round(post_mean, 3)), n = Inf)
stopifnot(all.equal(onc_chk$chk, onc_chk$post_mean))
cat("  (all five match exactly)\n")

####################################################################
# FIGURE: five oncologists grid
####################################################################

grid_df <- onc %>%
  rowwise() %>%
  mutate(d = list(bind_rows(
    tibble(p = pi_grid, density = dbeta(pi_grid, alpha, beta),   part = "Prior"),
    tibble(p = pi_grid, density = dbeta(pi_grid, a_post, b_post), part = "Posterior")
  ))) %>%
  unnest(d) %>%
  ungroup() %>%
  mutate(oncologist = factor(oncologist, levels = onc$oncologist),
         part       = factor(part, levels = c("Prior", "Posterior")))

p_grid <- ggplot(grid_df, aes(p, density, colour = part, linetype = part)) +
  geom_line(linewidth = 1) +
  # The grey rule is the MLE. It carried no label, so it read as chart furniture.
  geom_vline(aes(xintercept = x_res / n_pat, colour = "MLE"),
             linewidth = 0.5, show.legend = TRUE, key_glyph = "path") +
  facet_wrap(~ oncologist, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = c(Prior = col_prior, Posterior = col_post,
                                 MLE = "gray55"),
                      breaks = c("Prior", "Posterior", "MLE")) +
  scale_linetype_manual(values = c(Prior = "dashed", Posterior = "solid"),
                        guide = "none") +
  guides(colour = guide_legend(
    override.aes = list(linetype = c("dashed", "solid", "solid")))) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
  expand_limits(y = 0) +
  labs(x = "p", y = NULL, colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 21) +
  theme(legend.position = "bottom")

ggsave("figures/five_oncologists_grid.pdf", p_grid, width = 9, height = 3.4)

####################################################################
# Section 4 -- Normal-Normal: birthweight
####################################################################

bw <- read_csv("data/birthweights.csv", show_col_types = FALSE)
n_bw    <- nrow(bw)
xbar_bw <- mean(bw$weight_g)
mu0     <- 3300      # national mean
sd0     <- 200       # prior SD
sigma   <- 500       # known population SD

rule("SLIDE: Birthweight Example: Prior Observations and Weights")
cat("Prior N(", mu0, ",", sd0, "^2); data n =", n_bw, ", xbar =", round(xbar_bw, 1),
    "g, sigma =", sigma, "g\n\n")
n0_bw <- sigma^2 / sd0^2
cat("Step 1  n_0 = sigma^2/sigma_0^2 =", sigma^2, "/", sd0^2, "=", n0_bw,
    " -> slide shows 6.25 (~6 previous births)\n")
cat("Step 2  n_0 + n =", n0_bw, "+", n_bw, "=", n0_bw + n_bw, " -> slide shows 18.25\n")
sigma_n <- sigma / sqrt(n0_bw + n_bw)
cat("        sigma_n = 500/sqrt(18.25) =", round(sigma_n, 2), " -> slide shows ~117 g\n")
w0_bw <- n0_bw / (n0_bw + n_bw); w1_bw <- n_bw / (n0_bw + n_bw)
cat("Step 3  weights", round(w0_bw, 4), "on prior /", round(w1_bw, 4),
    "on data  -> slide shows 0.34 / 0.66\n")
mu_n <- w0_bw * mu0 + w1_bw * xbar_bw
cat("        posterior mean =", round(mu_n, 2), " -> slide shows ~3090 g\n")

rule("SLIDE: Birthweight Example: Posterior Mean and Credible Interval")
ci_bw <- mu_n + c(-1, 1) * 1.96 * sigma_n
cat("posterior mean", round(mu_n, 1), "g; sigma_n", round(sigma_n, 1), "g\n")
cat("95% credible interval = mu_n +/- 1.96 x sigma_n =",
    paste0("(", round(ci_bw[1]), ", ", round(ci_bw[2]), ")"), "g",
    " -> slide shows (2860, 3319)\n")
cat("  exact endpoints", round(ci_bw[1], 1), "and", round(ci_bw[2], 1), "\n")
cat("  NOTE: last semester's deck printed (2861, 3319) -- it rounded to 3090 and 117 first,\n")
cat("        then subtracted. Computing first and rounding last gives 2860. We quote 2860.\n")
cat("The posterior mean", round(mu_n, 0), "is pulled from xbar", round(xbar_bw, 0),
    "toward the national mean", mu0, "\n")

####################################################################
# FIGURE: normal likelihood + MLE, and the normal-normal update
####################################################################

mu_grid <- seq(2400, 3800, length.out = 600)

p_lik <- ggplot(tibble(mu = mu_grid,
                       density = dnorm(mu_grid, xbar_bw, sigma / sqrt(n_bw))),
                aes(mu, density)) +
  geom_line(linewidth = 1.1, colour = col_data) +
  geom_vline(xintercept = xbar_bw, linetype = "dashed", colour = "gray45") +
  annotate("text", x = xbar_bw + 25, y = 0, hjust = 0, vjust = -0.5, size = 5.7,
           colour = "gray35", parse = TRUE,
           label = sprintf("bar(x) == %.0f ~ \"g\"", xbar_bw)) +
  labs(x = expression(mu~"  (mean birthweight, g)"), y = "Likelihood") +
  theme_minimal(base_size = 20)

ggsave("figures/normal_likelihood_mle.pdf", p_lik, width = 6.6, height = 3.5)

nn_df <- bind_rows(
  tibble(mu = mu_grid, density = dnorm(mu_grid, mu0, sd0),                part = "Prior  N(3300, 200 sq.)"),
  tibble(mu = mu_grid, density = dnorm(mu_grid, xbar_bw, sigma/sqrt(n_bw)), part = "Likelihood"),
  tibble(mu = mu_grid, density = dnorm(mu_grid, mu_n, sigma_n),        part = "Posterior")
) %>%
  mutate(part = factor(part, levels = c("Prior  N(3300, 200 sq.)", "Likelihood", "Posterior")))

p_nn <- ggplot(nn_df, aes(mu, density, colour = part, linetype = part)) +
  geom_line(linewidth = 1.05) +
  scale_colour_manual(values = c(col_prior, col_data, col_post)) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  labs(x = expression(mu~"  (mean birthweight, g)"), y = "Density",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "right")

ggsave("figures/normal_normal_update.pdf", p_nn, width = 7.4, height = 3.7)

####################################################################
# Section 5 -- Gamma-Poisson: whale sightings
####################################################################

wh <- read_csv("data/whale_sightings.csv", show_col_types = FALSE)
n_wh    <- nrow(wh)
sum_wh  <- sum(wh$sightings)
xbar_wh <- mean(wh$sightings)
a_wh    <- 6; b_wh <- 2          # Gamma(6,2): mean 3/day, the historical baseline

rule("SLIDE: Motivating Example: Whale Sightings off Cape Cod")
cat("Counts:", paste(wh$sightings, collapse = ", "), "\n")
cat("n =", n_wh, " sum x_i =", sum_wh, " xbar =", round(xbar_wh, 4),
    " -> slide shows 4.43 sightings/day\n")

rule("SLIDE: Whale Sightings: Computing the Posterior")
cat("Prior Gamma(", a_wh, ",", b_wh, ")  mean =", a_wh / b_wh,
    " SD =", round(sqrt(a_wh / b_wh^2), 3), " -> slide shows 3 and ~1.2\n")
a_wh_post <- a_wh + sum_wh
b_wh_post <- b_wh + n_wh
cat("Posterior Gamma(", a_wh, "+", sum_wh, ",", b_wh, "+", n_wh, ") = Gamma(",
    a_wh_post, ",", b_wh_post, ")\n")
wh_post_mean <- a_wh_post / b_wh_post
cat("Posterior mean =", a_wh_post, "/", b_wh_post, "=", round(wh_post_mean, 4),
    " -> slide shows ~4.11\n")
cat("Posterior SD   = sqrt(", a_wh_post, "/", b_wh_post, "^2) =",
    round(sqrt(a_wh_post / b_wh_post^2), 4), " -> slide shows ~0.68\n")

rule("SLIDE: Whale Sightings: Interpreting the Posterior")
cat("Weights:", round(b_wh / (b_wh + n_wh), 4), "on prior /",
    round(n_wh / (b_wh + n_wh), 4), "on data  -> slide shows 0.22 / 0.78\n")
wh_chk <- weighted_mean_check(b_wh, n_wh, a_wh / b_wh, xbar_wh)
cat("  weighted average =", round(wh_chk, 4), "== posterior mean",
    round(wh_post_mean, 4), "\n")
stopifnot(all.equal(wh_chk, wh_post_mean))
ci_wh <- qgamma(c(0.025, 0.975), a_wh_post, b_wh_post)
cat("95% credible interval  qgamma(c(.025,.975),", a_wh_post, ",", b_wh_post, ") =",
    round(ci_wh, 4), " -> slide shows (2.89, 5.54)\n")
p_inc <- pgamma(3, a_wh_post, b_wh_post, lower.tail = FALSE)
cat("P(lambda > 3 | x) = pgamma(3,", a_wh_post, ",", b_wh_post, ", lower.tail=FALSE) =",
    round(p_inc, 4), " -> slide shows ~0.96\n")

####################################################################
# FIGURE: Poisson pmfs, gamma shapes, gamma-poisson update
####################################################################

x_vals <- 0:28
pois_df <- bind_rows(lapply(c(1, 3, 7, 15), function(l)
  tibble(x = x_vals, prob = dpois(x_vals, l),
         lam = paste0("lambda == ", l)))) %>%
  mutate(lam = factor(lam, levels = paste0("lambda == ", c(1, 3, 7, 15))))

p_pois <- ggplot(pois_df, aes(x, prob)) +
  geom_col(fill = col_prior, width = 0.75) +
  facet_wrap(~ lam, nrow = 1, labeller = label_parsed) +
  labs(x = "x  (count)", y = "Probability") +
  theme_minimal(base_size = 20)

ggsave("figures/poisson_pmfs.pdf", p_pois, width = 8.6, height = 2.9)

lam_grid <- seq(0, 12, length.out = 600)
gam_df <- bind_rows(lapply(list(c(1,1), c(2,1), c(5,1), c(6,2), c(9,3)), function(ab)
  tibble(lambda = lam_grid, density = dgamma(lam_grid, ab[1], ab[2]),
         param = paste0("Gamma(", ab[1], ", ", ab[2], ")")))) %>%
  mutate(param = factor(param, levels = c("Gamma(1, 1)", "Gamma(2, 1)", "Gamma(5, 1)",
                                          "Gamma(6, 2)", "Gamma(9, 3)")))

p_gam <- ggplot(gam_df, aes(lambda, density, colour = param, linewidth = param)) +
  geom_line() +
  # Nothing happens past lambda = 10, and trimming the dead space keeps the
  # panel from going wide-and-flat once the figure is shown at 0.86\textwidth.
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1.1)) +
  scale_colour_manual(values = c("Gamma(1, 1)" = "gray70", "Gamma(2, 1)" = "gray55",
                                 "Gamma(5, 1)" = "gray40", "Gamma(6, 2)" = col_post,
                                 "Gamma(9, 3)" = "gray70")) +
  scale_linewidth_manual(values = c("Gamma(1, 1)" = 0.8, "Gamma(2, 1)" = 0.8,
                                    "Gamma(5, 1)" = 0.8, "Gamma(6, 2)" = 1.8,
                                    "Gamma(9, 3)" = 0.8), guide = "none") +
  labs(x = expression(lambda), y = "Density", colour = NULL) +
  theme_minimal(base_size = 22)

ggsave("figures/gamma_shapes.pdf", p_gam, width = 7.4, height = 3.5)

gp_df <- bind_rows(
  tibble(lambda = lam_grid, density = dgamma(lam_grid, a_wh, b_wh),
         part = "Prior  Gamma(6, 2)"),
  tibble(lambda = lam_grid, density = dgamma(lam_grid, sum_wh + 1, n_wh),
         part = "Likelihood (rescaled)"),
  tibble(lambda = lam_grid, density = dgamma(lam_grid, a_wh_post, b_wh_post),
         part = "Posterior  Gamma(37, 9)")
) %>%
  mutate(part = factor(part, levels = c("Prior  Gamma(6, 2)", "Likelihood (rescaled)",
                                        "Posterior  Gamma(37, 9)")))

p_gp <- ggplot(gp_df, aes(lambda, density, colour = part, linetype = part)) +
  geom_line(linewidth = 1.05) +
  scale_colour_manual(values = c(col_prior, col_data, col_post)) +
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  coord_cartesian(xlim = c(0, 9)) +
  labs(x = expression(lambda~"  (sightings per day)"), y = "Density",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "right")

ggsave("figures/gamma_poisson_update.pdf", p_gp, width = 7.4, height = 3.7)

####################################################################
# Wrap-up -- the pattern holds in all three families
####################################################################

rule("SLIDE: Summary of the Three Conjugate Families / The Conjugate Pattern")
pattern <- tibble(
  family      = c("Beta-Binomial", "Normal-Normal", "Gamma-Poisson"),
  prior_n0    = c(alpha0 + beta0, n0_bw, b_wh),
  data_n      = c(n_pat, n_bw, n_wh),
  prior_mean  = c(alpha0 / (alpha0 + beta0), mu0, a_wh / b_wh),
  mle         = c(x_res / n_pat, xbar_bw, xbar_wh),
  posterior   = c(post_mean, mu_n, wh_post_mean)
) %>%
  mutate(weighted_avg = weighted_mean_check(prior_n0, data_n, prior_mean, mle),
         matches      = abs(weighted_avg - posterior) < 1e-9)
print(pattern %>% mutate(across(where(is.numeric), ~ round(.x, 4))), n = Inf)
stopifnot(all(pattern$matches))
cat("\nposterior mean = weighted average of prior mean and MLE -- verified for all three.\n")

####################################################################
# In-class activity -- instructor reference
#
# The worksheet scenario: a community health clinic asks three questions --
# a proportion, a mean, a rate -- i.e. one per conjugate family. Students
# write the code themselves; these are the answers.
####################################################################

rule("ACTIVITY (instructor reference): community health clinic")

cat("Part 1  Beta-Binomial -- uncontrolled hypertension\n")
w_a <- 1; w_b <- 9; w_n <- 50; w_x <- 8      # prior mean 0.10, n0 = 10
cat("  (A) prior Beta(", w_a, ",", w_b, "): mean", round(w_a / (w_a + w_b), 3),
    ", n0 = alpha+beta =", w_a + w_b, "\n")
cat("  (B) posterior Beta(", w_a + w_x, ",", w_b + w_n - w_x, "), mean",
    round((w_a + w_x) / (w_a + w_b + w_n), 4), "; MLE", round(w_x / w_n, 4), "\n")
cat("  (C) weights", round((w_a + w_b) / (w_a + w_b + w_n), 4), "prior /",
    round(w_n / (w_a + w_b + w_n), 4), "data; weighted avg",
    round(weighted_mean_check(w_a + w_b, w_n, w_a / (w_a + w_b), w_x / w_n), 4), "\n")
cat("  (D) leans to the data: n = 50 dwarfs n0 = 10 (5/6 of the weight)\n")

cat("\nPart 2  Normal-Normal -- systolic blood pressure\n")
w_mu0 <- 120; w_sd0 <- 10; w_sig <- 15; w_n2 <- 20; w_xbar <- 126
w_n0 <- w_sig^2 / w_sd0^2
cat("  (B) n0 = 15^2/10^2 =", w_n0, " -> worth about 2 measurements\n")
w_sn <- w_sig / sqrt(w_n0 + w_n2)
cat("  (C) n0+n =", w_n0 + w_n2, ", sigma_n =", round(w_sn, 3), "\n")
w_th <- weighted_mean_check(w_n0, w_n2, w_mu0, w_xbar)
cat("      weights", round(w_n0 / (w_n0 + w_n2), 4), "/",
    round(w_n2 / (w_n0 + w_n2), 4), "; posterior mean", round(w_th, 3), "\n")
cat("  (D) 95% CrI", round(w_th + c(-1, 1) * 1.96 * w_sn, 2), "\n")
cat("  (E) Part 2's prior has LESS say: weight on prior is",
    round(w_n0 / (w_n0 + w_n2), 3), "vs", round((w_a + w_b) / (w_a + w_b + w_n), 3),
    "in Part 1\n")

cat("\nPart 3  Gamma-Poisson -- afternoon walk-ins\n")
w_cnt <- c(6, 4, 7, 5, 8, 3); w_a3 <- 8; w_b3 <- 2
cat("  (A) n =", length(w_cnt), ", sum x_i =", sum(w_cnt), ", xbar =",
    round(mean(w_cnt), 3), "\n")
cat("  (B) prior mean = 8/2 =", w_a3 / w_b3, "; n0 = beta =", w_b3, "\n")
w_ap <- w_a3 + sum(w_cnt); w_bp <- w_b3 + length(w_cnt)
cat("  (C) posterior Gamma(", w_ap, ",", w_bp, "), mean", round(w_ap / w_bp, 4),
    ", SD", round(sqrt(w_ap / w_bp^2), 4), "\n")
cat("  (D) weights", round(w_b3 / w_bp, 4), "/", round(length(w_cnt) / w_bp, 4),
    "; weighted avg",
    round(weighted_mean_check(w_b3, length(w_cnt), w_a3 / w_b3, mean(w_cnt)), 4), "\n")
cat("  (E) P(lambda > 4 | x) =",
    round(pgamma(4, w_ap, w_bp, lower.tail = FALSE), 4), "-> yes, hire\n")

cat("\nPart 4  the pattern table\n")
print(tibble(
  part          = c("1 Beta-Binomial", "2 Normal-Normal", "3 Gamma-Poisson"),
  parameter     = c("p", "mu", "lambda"),
  n0            = c(w_a + w_b, w_n0, w_b3),
  n             = c(w_n, w_n2, length(w_cnt)),
  weight_data   = round(c(w_n / (w_a + w_b + w_n), w_n2 / (w_n0 + w_n2),
                          length(w_cnt) / w_bp), 3)
), n = Inf)
cat("  (B) ranking by prior influence: Part 3 (0.25) > Part 1 (0.167) > Part 2 (0.101)\n")
cat("      -- decided by n0/(n0+n) alone.\n")
cat("  (C) doubling n with identical summaries: every posterior mean moves TOWARD the MLE\n")
cat("      (weight on data rises), and every posterior SD shrinks.\n")

rule("Figures written")
cat(paste0("figures/", c("beta_shapes.pdf", "beta_binomial_update.pdf",
                         "five_oncologists_grid.pdf", "normal_likelihood_mle.pdf",
                         "normal_normal_update.pdf", "poisson_pmfs.pdf",
                         "gamma_shapes.pdf", "gamma_poisson_update.pdf"),
           collapse = "\n"), "\n")
