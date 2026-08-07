# MA214 Lecture 17 -- Review of Bayesian statistics
# In-class demo + figure generation.
#
# Scenarios (frozen by data/prepare_data.R):
#   data/screening_test.csv   a rapid test for a rare disease. Bayes' rule for
#                             events, and sequential updating over repeat tests.
#   data/gw_signal.csv        LIGO source classification: four hypotheses, two
#                             observations. Also carries the order-invariance
#                             demonstration.
#   data/drug_trial.csv       Phase 1 trial: a discrete prior on a parameter.
#
# Every number that appears on a slide is printed below.
# Run from this directory:  Rscript Lecture17_demo.R

library(tidyverse)

dir.create("figures", showWarnings = FALSE)
pdf(NULL)                      # keep a stray Rplots.pdf from appearing

blue <- "#569BBD"
red  <- "#F05133"
gray <- "gray50"

rule <- function(s) cat("\n", strrep("=", 70), "\n", s, "\n", strrep("=", 70), "\n", sep = "")

####################################################################
# Bayes' rule helpers
####################################################################

# Two hypotheses (an event and its complement).
bayes_binary <- function(prior, sensitivity, false_pos_rate) {
  num <- sensitivity * prior
  num / (num + false_pos_rate * (1 - prior))
}

# Any number of hypotheses: posterior proportional to likelihood x prior.
bayes_multi <- function(prior, likelihood) {
  prod <- prior * likelihood
  prod / sum(prod)
}

####################################################################
# Section 3 -- Bayes' rule for events: disease screening
####################################################################

screening   <- read_csv("data/screening_test.csv", show_col_types = FALSE)
prevalence  <- screening$value[screening$parameter == "prevalence"]
sensitivity <- screening$value[screening$parameter == "sensitivity"]
specificity <- screening$value[screening$parameter == "specificity"]
fpr         <- 1 - specificity          # P(+ | D^c)

rule("SLIDE: Example: Disease Screening")
cat("P(D)        =", prevalence,  "\n")
cat("P(+ | D)    =", sensitivity, "  (sensitivity)\n")
cat("P(- | D^c)  =", specificity, "  (specificity)\n")
cat("P(+ | D^c)  =", fpr, "\n\n")

num1 <- sensitivity * prevalence
den1 <- num1 + fpr * (1 - prevalence)
post1 <- num1 / den1
cat("numerator   = 0.99 x 0.001            =", round(num1, 5), "\n")
cat("denominator = 0.99 x 0.001 + 0.05 x 0.999 =", round(den1, 5), "\n")
cat("P(D | +)    =", round(num1, 5), "/", round(den1, 5), "=", round(post1, 5),
    " -> slide shows 0.019\n")

rule("SLIDE: Why Is the Posterior Probability of Disease So Low?")
# The slide counts out 1,000 people. Every figure it shows is derived here.
n_people   <- 1000
n_sick     <- n_people * prevalence              # ~1
n_healthy  <- n_people * (1 - prevalence)        # ~999
true_pos   <- n_sick * sensitivity               # 0.99
false_pos  <- n_healthy * fpr                    # 49.95
all_pos    <- true_pos + false_pos               # 50.94
cat("Of", n_people, "people:\n")
cat("  ~", n_sick, "has the disease, and tests positive", sensitivity, "of the time ->",
    round(true_pos, 2), "true positives\n")
cat("  ~", n_healthy, "are healthy, and", n_healthy, "x", fpr, "=",
    round(false_pos, 2), "of them test positive anyway\n")
cat("  total positives =", round(true_pos, 2), "+", round(false_pos, 2), "=",
    round(all_pos, 2), "\n")
cat("  of which only", round(true_pos, 2), "are real ->",
    round(true_pos, 2), "/", round(all_pos, 2), "=", round(true_pos / all_pos, 5), "\n")
stopifnot(all.equal(true_pos / all_pos, post1))   # must match the Bayes'-rule answer
cat("  (matches P(D | +) from Bayes' rule exactly)\n")

####################################################################
# FIGURE: posterior vs prior  (Section 3)
####################################################################

prior_grid <- seq(0.001, 0.999, by = 0.001)
pv <- tibble(
  prior     = prior_grid,
  posterior = bayes_binary(prior_grid, sensitivity, fpr)
)

# Axis labels are kept short and the callout is set on two lines: at
# 0.72\textwidth on the slide the figure is shrunk ~2x, and anything longer
# runs off the panel.
p_fig1 <- ggplot(pv, aes(prior, posterior)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = gray) +
  annotate("text", x = 0.80, y = 0.72, label = "posterior = prior",
           colour = gray, size = 6.5, angle = 34) +
  geom_line(linewidth = 1.1, colour = blue) +
  annotate("point", x = prevalence, y = post1, size = 3, colour = red) +
  annotate("segment", x = 0.28, xend = 0.03, y = 0.20, yend = post1 + 0.02,
           colour = red, linewidth = 0.4,
           arrow = arrow(length = unit(0.11, "cm"), type = "closed")) +
  annotate("text", x = 0.32, y = 0.20, hjust = 0, size = 6.5, colour = red,
           lineheight = 0.9,
           label = sprintf("prevalence 0.001\ngives posterior %.3f", post1)) +
  labs(x = "Prior probability of disease",
       y = "Posterior P(D | +)") +
  theme_minimal(base_size = 22)

ggsave("figures/posterior_vs_prior.pdf", p_fig1, width = 6.2, height = 4)

####################################################################
# Section 4 -- Conditional distributions and priors: the drug trial
####################################################################

trial <- read_csv("data/drug_trial.csv", show_col_types = FALSE)
n_pat <- 6

discrete_post <- function(x) {
  lik  <- dbinom(x, n_pat, trial$p)
  prod <- trial$prior * lik
  tibble(p = trial$p, prior = trial$prior, likelihood = lik,
         product = prod, posterior = prod / sum(prod))
}

rule("SLIDE: The Binomial Likelihood   (n = 6, x = 2)")
d2 <- discrete_post(2)
print(d2 %>% mutate(across(-p, ~ round(.x, 4))), n = Inf)

rule("SLIDE: Computing the Posterior   (x = 2)")
cat("Total (marginal likelihood) =", round(sum(d2$product), 4), "\n")
cat("Prior mean     E(p)     =", round(sum(trial$p * trial$prior), 3), "\n")
cat("Posterior mean E(p | x) =", round(sum(d2$p * d2$posterior), 3), "\n")

rule("SLIDE: What If 5 of 6 Patients Responded?   (x = 5)")
d5 <- discrete_post(5)
print(d5 %>% mutate(across(-p, ~ round(.x, 4))), n = Inf)
cat("Total (marginal likelihood) =", round(sum(d5$product), 4), "\n")
cat("Prior mean     E(p)     =", round(sum(trial$p * trial$prior), 3), "\n")
cat("Posterior mean E(p | x) =", round(sum(d5$p * d5$posterior), 3), "\n")

####################################################################
# FIGURE: the discrete update  (Section 4)
####################################################################

fig2_dat <- bind_rows(
  d2 %>% mutate(data = "x = 2 of 6"),
  d5 %>% mutate(data = "x = 5 of 6")
) %>%
  # Rescale the likelihood to sum to 1 so all three panels share a y-axis: the
  # likelihood is not a distribution over p, its *shape* is what matters here.
  group_by(data) %>%
  mutate(likelihood = likelihood / sum(likelihood)) %>%
  ungroup() %>%
  pivot_longer(c(prior, likelihood, posterior),
               names_to = "quantity", values_to = "value") %>%
  mutate(quantity = factor(quantity,
                           levels = c("prior", "likelihood", "posterior"),
                           labels = c("Prior f(p)", "Likelihood (scaled)",
                                      "Posterior f(p | x)")))

p_fig2 <- ggplot(fig2_dat, aes(factor(p), value, fill = quantity)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  facet_grid(data ~ quantity) +
  scale_fill_manual(values = c(gray, red, blue)) +
  labs(x = "p  (probability a patient responds)", y = NULL) +
  theme_minimal(base_size = 20) +
  # Strip labels have only a third of the width (or half the height) of the
  # figure to sit in, so they need to be smaller than the axis text.
  theme(panel.grid.major.x = element_blank(),
        strip.text = element_text(size = rel(0.85)))

ggsave("figures/drug_trial_update.pdf", p_fig2, width = 7.2, height = 4.2)

####################################################################
# Section 5 -- Sequentiality: repeat screening tests
####################################################################

rule("SLIDE: Updating with More Data   (repeat positive tests)")
seq_post <- accumulate(1:3, ~ bayes_binary(.x, sensitivity, fpr),
                       .init = prevalence)[-1]
cat("Start from the prior P(D) =", prevalence, "\n\n")
cat("after 1 positive test: P(D | +)   =", round(seq_post[1], 5),
    " -> slide shows 0.019\n")
cat("after 2 positive tests: P(D | ++) =", round(seq_post[2], 5),
    " -> slide shows 0.28\n")
cat("after 3 positive tests: P(D | +++)=", round(seq_post[3], 5),
    " -> slide shows 0.88\n\n")
cat("Second update shown long-hand, using the rounded posterior 0.019 as prior:\n")
cat("  numerator   = 0.99 x 0.019             =", round(0.99 * 0.019, 5), "\n")
cat("  denominator = 0.99 x 0.019 + 0.05 x 0.981 =",
    round(0.99 * 0.019 + 0.05 * 0.981, 5), "\n")
cat("  ratio       =", round(0.99 * 0.019 / (0.99 * 0.019 + 0.05 * 0.981), 4),
    " -> 0.28 either way\n")

####################################################################
# Section 5 -- LIGO: four hypotheses, two observations
####################################################################

gw <- read_csv("data/gw_signal.csv", show_col_types = FALSE)

rule("SLIDE: Posterior After Signal Duration")
prod_d <- gw$prior * gw$lik_long
post_d <- prod_d / sum(prod_d)
print(tibble(source = gw$source, prior = gw$prior, likelihood = gw$lik_long,
             product = round(prod_d, 4), posterior = round(post_d, 3)), n = Inf)
cat("Total =", round(sum(prod_d), 4), "\n")

rule("SLIDE: Posterior After EM Counterpart")
prod_e <- post_d * gw$lik_em
post_e <- prod_e / sum(prod_e)
print(tibble(source = gw$source, prior = round(post_d, 3), likelihood = gw$lik_em,
             product = round(prod_e, 4), posterior = round(post_e, 3)), n = Inf)
cat("Total =", round(sum(prod_e), 4), "\n")

rule("REFERENCE: the whole journey at 2dp (no slide -- the order-invariance figure carries this)")
print(tibble(source = gw$source,
             prior          = round(gw$prior, 2),
             after_duration = round(post_d, 2),
             after_em       = round(post_e, 2)), n = Inf)
cat("NOTE: Noise after duration is", round(post_d[4], 4), "-> rounds to",
    round(post_d[4], 2), "at 2dp. Last semester's summary table printed 0.24, which was wrong;\n")
cat("      the frame carrying it has been dropped, so the error is gone either way.\n")

####################################################################
# Section 5 -- Order invariance
####################################################################

rule("SLIDE: Order Doesn't Matter")
path_de <- bayes_multi(bayes_multi(gw$prior, gw$lik_long), gw$lik_em)
path_ed <- bayes_multi(bayes_multi(gw$prior, gw$lik_em), gw$lik_long)
both    <- bayes_multi(gw$prior, gw$lik_long * gw$lik_em)

print(tibble(source              = gw$source,
             `duration -> EM`    = round(path_de, 4),
             `EM -> duration`    = round(path_ed, 4),
             `both at once`      = round(both, 4)), n = Inf)
cat("\nall three identical? ",
    isTRUE(all.equal(path_de, path_ed)) && isTRUE(all.equal(path_de, both)), "\n\n")
cat("The intermediate posteriors genuinely differ -- the path is not the same:\n")
print(tibble(source                 = gw$source,
             `after duration only`  = round(bayes_multi(gw$prior, gw$lik_long), 4),
             `after EM only`        = round(bayes_multi(gw$prior, gw$lik_em), 4)), n = Inf)

####################################################################
# FIGURE: order invariance  (Section 5)
####################################################################

# The three step labels share one panel, so they are set on two lines: written
# out in full they collide once the type is large enough to read on a slide.
step_levels <- c("Prior", "After\n1st", "After\nboth")

fig3_dat <- bind_rows(
  tibble(route = "Duration, then EM", step = "Prior",
         source = gw$source, prob = gw$prior),
  tibble(route = "Duration, then EM", step = "After\n1st",
         source = gw$source, prob = bayes_multi(gw$prior, gw$lik_long)),
  tibble(route = "Duration, then EM", step = "After\nboth",
         source = gw$source, prob = path_de),
  tibble(route = "EM, then duration", step = "Prior",
         source = gw$source, prob = gw$prior),
  tibble(route = "EM, then duration", step = "After\n1st",
         source = gw$source, prob = bayes_multi(gw$prior, gw$lik_em)),
  tibble(route = "EM, then duration", step = "After\nboth",
         source = gw$source, prob = path_ed)
) %>%
  mutate(step   = factor(step, levels = step_levels),
         route  = factor(route, levels = c("Duration, then EM", "EM, then duration")),
         source = factor(source, levels = gw$source))

p_fig3 <- ggplot(fig3_dat, aes(step, prob, group = source, colour = source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(~ route) +
  scale_colour_manual(values = c(BBH = gray, BNS = red, NSBH = blue, Noise = "gray70")) +
  ylim(0, 1) +
  labs(x = NULL, y = "Probability", colour = "Source") +
  theme_minimal(base_size = 23) +
  # The legend on the right steals a fifth of the width from the two panels,
  # which is what pushed the step labels into each other.
  theme(legend.position = "bottom",
        axis.text.x = element_text(lineheight = 0.85))

ggsave("figures/order_invariance.pdf", p_fig3, width = 7.2, height = 3.8)

####################################################################
# Section 6 -- The same data, two questions
####################################################################

rule("SLIDES: Hypothesis Testing -- frequentist and Bayesian")
cat("H0: no disease. One positive test, prevalence", prevalence, "\n\n")
cat("Frequentist:\n")
cat("  p-value = P(+ | H0) = P(+ | D^c) =", fpr, "\n")
cat("  s-value = -log2(p)  =", round(-log2(fpr), 2), "bits against H0\n")
cat("                        (about as surprising as", round(-log2(fpr)),
    "heads in a row)\n\n")
cat("Bayesian:\n")
p_bayes <- 1 - post1
cat("  P(H0 | +) = 1 - P(D | +) =", round(p_bayes, 4), " -> slide shows 0.98\n")
cat("  s_Bayes   = -log2(P(H0 | +)) =", round(-log2(p_bayes), 3),
    "bits -> slide shows 0.03\n\n")
cat("Same data, opposite readings -- because they answer different questions.\n")

####################################################################
# In-class activity -- instructor reference
#
# The worksheet scenario: a cell culture behaving strangely. Students write
# the code themselves; these are the answers.
####################################################################

rule("ACTIVITY (instructor reference): cell culture contamination")

a_sens <- 0.90
a_fpr  <- 1 - 0.85          # specificity 0.85

cat("Part 1B  P(C | +), prior 0.05        =", round(bayes_binary(0.05, a_sens, a_fpr), 4), "\n")
cat("Part 1D  second positive test        =",
    round(bayes_binary(bayes_binary(0.05, a_sens, a_fpr), a_sens, a_fpr), 4), "\n")
cat("Part 2A  priors 0.02 / 0.10 / 0.30   =",
    round(sapply(c(0.02, 0.10, 0.30), bayes_binary, a_sens, a_fpr), 4), "\n")
cat("Part 3A  five sequential positives   =",
    round(accumulate(1:5, ~ bayes_binary(.x, a_sens, a_fpr), .init = 0.05)[-1], 4), "\n")

cat("\nPart 4: identifying the contaminant\n")
a_prior <- c(Mycoplasma = 0.55, Bacteria = 0.25, Fungal = 0.10, CrossContam = 0.10)
lik_scope <- c(0.95, 0.10, 0.05, 0.90)     # P(no visible particles | H)
lik_pcr   <- c(0.95, 0.05, 0.02, 0.03)     # P(PCR+ for mycoplasma  | H)
post_scope <- bayes_multi(a_prior, lik_scope)
post_pcr   <- bayes_multi(post_scope, lik_pcr)
cat("  4A after microscopy:", round(post_scope, 3),
    " (total product", round(sum(a_prior * lik_scope), 4), ")\n")
cat("  4C after PCR       :", round(post_pcr, 3),
    " (total product", round(sum(post_scope * lik_pcr), 4), ")\n")
cat("  4E order check, PCR first then microscopy:",
    round(bayes_multi(bayes_multi(a_prior, lik_pcr), lik_scope), 3), " <- same\n")

cat("\nPart 5: a prior on a parameter (8 flasks, 3 contaminated)\n")
theta      <- c(0.05, 0.15, 0.30, 0.50)
theta_prior <- c(0.40, 0.30, 0.20, 0.10)
theta_lik  <- dbinom(3, 8, theta)
theta_prod <- theta_prior * theta_lik
theta_post <- theta_prod / sum(theta_prod)
print(tibble(theta, prior = theta_prior, likelihood = round(theta_lik, 4),
             product = round(theta_prod, 5), posterior = round(theta_post, 3)), n = Inf)
cat("  total =", round(sum(theta_prod), 5),
    " | prior mean", round(sum(theta * theta_prior), 3),
    "-> posterior mean", round(sum(theta * theta_post), 3), "\n")
cat("  Note: 3/8 = 0.375, but the posterior peaks at 0.30 -- the prior is still pulling.\n")

rule("Figures written")
cat("figures/posterior_vs_prior.pdf\nfigures/drug_trial_update.pdf\nfigures/order_invariance.pdf\n")
