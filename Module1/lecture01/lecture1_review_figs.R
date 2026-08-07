# Figures for the Lecture 1 "review of key concepts" slides.
# Run from this directory; writes PNGs into figures/.
# All figures use the deck palette (ccBlue/ccGreen/ccTerra/ccGray).

library(tidyverse)

if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

dir.create("figures", showWarnings = FALSE)

ccBlue  <- "#2F6690"
ccGreen <- "#4E8A6B"
ccTerra <- "#C2693E"
ccGray  <- "#5C6B7A"

theme_lec <- theme_minimal(base_size = 24) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    axis.title = element_text(color = "#2B2B2B"),
    legend.position = "none"
  )

# ---- Law of Large Numbers: running proportion of heads ----
# Three independent runs of a fair coin; the observed proportion of heads
# settles toward the true probability p = 0.5 as the number of flips grows.

set.seed(214)
n_flips <- 1000
p_true  <- 0.5
run_cols <- c(ccBlue, ccGreen, ccTerra)

lln <- map_dfr(1:3, function(run) {
  flips <- rbinom(n_flips, size = 1, prob = p_true)
  tibble(
    run   = factor(run),
    n     = seq_len(n_flips),
    phat  = cumsum(flips) / seq_len(n_flips)
  )
})

p_lln <- ggplot(lln, aes(n, phat, color = run)) +
  geom_hline(yintercept = p_true, linetype = "dashed",
             color = ccGray, linewidth = 0.7) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = n_flips, y = p_true + 0.20,
           label = "true probability p = 0.5",
           hjust = 1, color = ccGray, size = 7) +
  scale_color_manual(values = run_cols) +
  scale_x_log10(labels = scales::label_number()) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Number of coin flips (log scale)",
       y = "Proportion of heads") +
  theme_lec

ggsave("figures/lln_running_proportion.png", p_lln,
       width = 8, height = 4.2, dpi = 200, bg = "white")

message("Wrote figures/lln_running_proportion.png")

# ---- Law of Large Numbers: running average of die rolls ----
# Three independent runs of a fair six-sided die; the running average of the
# rolls settles toward the true mean mu = 3.5 as the number of rolls grows.
# This is the "means" companion to the coin "proportions" figure above.

set.seed(2140)
n_rolls <- 1000
mu_true <- 3.5

lln_mean <- map_dfr(1:3, function(run) {
  rolls <- sample(1:6, n_rolls, replace = TRUE)
  tibble(
    run  = factor(run),
    n    = seq_len(n_rolls),
    xbar = cumsum(rolls) / seq_len(n_rolls)
  )
})

p_lln_mean <- ggplot(lln_mean, aes(n, xbar, color = run)) +
  geom_hline(yintercept = mu_true, linetype = "dashed",
             color = ccGray, linewidth = 0.7) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  annotate("text", x = n_rolls, y = mu_true + 0.9,
           label = "true mean mu = 3.5",
           hjust = 1, color = ccGray, size = 7) +
  scale_color_manual(values = run_cols) +
  scale_x_log10(labels = scales::label_number()) +
  coord_cartesian(ylim = c(1, 6)) +
  labs(x = "Number of die rolls (log scale)",
       y = "Running average") +
  theme_lec

ggsave("figures/lln_running_mean.png", p_lln_mean,
       width = 8, height = 4.2, dpi = 200, bg = "white")

message("Wrote figures/lln_running_mean.png")

# ---- Sampling distribution: effect of sample size (n = 100 vs n = 1000) ----
# Population mean blood-pressure reduction 6.6 mmHg (SD 18). Draw many samples of
# each size and record the sample means. A larger sample gives a narrower sampling
# distribution -- less sampling variability. Shown side-by-side on a common axis.

set.seed(2141)
population_mean <- 6.6
population_sd   <- 18
n_samples       <- 1000

draw_means <- function(n) {
  tibble(
    size        = factor(paste0("n = ", n), levels = c("n = 100", "n = 1000")),
    sample_mean = replicate(n_samples, mean(rnorm(n, population_mean, population_sd)))
  )
}
samp <- bind_rows(draw_means(100), draw_means(1000))

p_sampling <- ggplot(samp, aes(sample_mean)) +
  geom_histogram(bins = 30, fill = ccBlue, color = "white") +
  geom_vline(xintercept = population_mean, linetype = "dashed",
             color = ccTerra, linewidth = 0.9) +
  annotate("text", x = population_mean, y = Inf, label = "mu", parse = TRUE,
           color = ccTerra, size = 7, hjust = -0.3, vjust = 1.4) +
  facet_wrap(~ size) +
  labs(x = "Sample mean reduction (mmHg)", y = "Number of samples") +
  theme_lec +
  theme(strip.text = element_text(face = "bold"),
        panel.spacing = unit(1.2, "lines"))

ggsave("figures/sampling_distribution_means.png", p_sampling,
       width = 9, height = 4, dpi = 200, bg = "white")

message("Wrote figures/sampling_distribution_means.png")
