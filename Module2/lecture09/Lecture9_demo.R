# MA214 Lecture 9 -- Confidence intervals with bootstrapping
# In-class demo + figure generation.
#
# Data: data/dm1999.csv -- the 348 kangaroo rats (Dipodomys merriami) weighed
# in 1999 in the Portal Project Teaching Database.
#
# Run from this directory:  Rscript Lecture9_demo.R

library(tidyverse)

set.seed(214)

dm_1999 <- read_csv("data/dm1999.csv", show_col_types = FALSE)
w <- dm_1999$weight
n <- length(w)

# ------------------------------------------------------------------------------
# The bootstrap in four lines (slide: "Doing it in R")
# ------------------------------------------------------------------------------
mean(w)                                        # 43.95
mean(sample(w, replace = TRUE))                # one resample

boot_means <- replicate(2000, mean(sample(w, replace = TRUE)))

quantile(boot_means, c(0.025, 0.975))          # percentile CI: 43.27 44.63
sd(boot_means)                                 # SE_boot: 0.355

# The two recipes agree when the bootstrap distribution is roughly symmetric.
mean(w) + c(-2, 2) * sd(boot_means)            # SE method: 43.24 44.65

# ------------------------------------------------------------------------------
# Figure: one bootstrap distribution, four confidence levels
# ------------------------------------------------------------------------------
levels <- c(0.80, 0.90, 0.95, 0.99)

ci_by_level <- map_dfr(levels, function(lev) {
  a <- (1 - lev) / 2
  q <- quantile(boot_means, c(a, 1 - a))
  tibble(level = lev, lower = q[1], upper = q[2], width = q[2] - q[1])
})

p_levels <- ggplot(tibble(stat = boot_means), aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey80", colour = "white") +
  geom_segment(
    data = ci_by_level,
    aes(x = lower, xend = upper,
        y = -20 - 45 * seq_along(levels), yend = -20 - 45 * seq_along(levels)),
    linewidth = 1.1
  ) +
  geom_text(
    data = ci_by_level,
    aes(x = (lower + upper) / 2,
        y = -20 - 45 * seq_along(levels) + 18,
        label = paste0(100 * level, "%: width ", sprintf("%.2f", width))),
    size = 7.5
  ) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Bootstrap mean weight (g)") +
  theme_minimal(base_size = 28)

ggsave("figures/ci_levels.png", p_levels, width = 6.5, height = 4, dpi = 200)

# ------------------------------------------------------------------------------
# Figure: what actually narrows an interval -- n, not B
# ------------------------------------------------------------------------------
ci_width <- function(x, B = 1000) {
  bm <- replicate(B, mean(sample(x, replace = TRUE)))
  diff(quantile(bm, c(0.025, 0.975)))
}

# width vs sample size (subsample the observed weights)
by_n <- map_dfr(c(10, 25, 50, 100, 200, 348), function(m) {
  reps <- replicate(20, ci_width(sample(w, m)))
  tibble(n = m, width = mean(reps))
})

# width vs number of resamples B (at the full sample size)
by_B <- map_dfr(c(100, 500, 1000, 5000, 20000), function(B) {
  tibble(B = B, width = ci_width(w, B = B))
})

p_n <- ggplot(by_n, aes(x = n, y = width)) +
  geom_line() + geom_point() +
  expand_limits(y = 0) +
  labs(x = "Sample size n", y = "95% CI width (g)") +
  theme_minimal(base_size = 30)

p_B <- ggplot(by_B, aes(x = B, y = width)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  expand_limits(y = c(0, max(by_n$width))) +
  labs(x = "Number of resamples B (log scale)", y = "95% CI width (g)") +
  theme_minimal(base_size = 30)

ggsave("figures/width_vs_n.png", p_n, width = 5.6, height = 3.2, dpi = 200)
ggsave("figures/width_vs_B.png", p_B, width = 5.6, height = 3.2, dpi = 200)

# ------------------------------------------------------------------------------
# Figure: the bootstrap cannot rescue a tiny sample
# ------------------------------------------------------------------------------
# With n = 5, the bootstrap distribution of the mean is visibly chunky: every
# resample is built from the same five numbers, so the CI inherits whatever
# those five numbers happened to be.
small <- sample(w, 5)
boot_small <- replicate(2000, mean(sample(small, replace = TRUE)))

p_small <- ggplot(tibble(stat = boot_small), aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey80", colour = "white") +
  labs(x = "Bootstrap mean weight (g)", y = "Count") +
  theme_minimal(base_size = 30)

# The maximum is worse: the bootstrap distribution of max(x*) can never exceed
# max(x), so the interval is degenerate no matter how large B is.
boot_max <- replicate(2000, max(sample(w, replace = TRUE)))

p_max <- ggplot(tibble(stat = boot_max), aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey80", colour = "white") +
  geom_vline(xintercept = max(w), linetype = 2) +
  labs(x = "Bootstrap maximum weight (g)", y = "Count") +
  theme_minimal(base_size = 30)

ggsave("figures/boot_small_n5.png", p_small, width = 5.6, height = 3.2, dpi = 200)
ggsave("figures/boot_max.png", p_max, width = 5.6, height = 3.2, dpi = 200)

# ------------------------------------------------------------------------------
# Figure: the sample itself (slide "Question. What is a typical body mass?")
# ------------------------------------------------------------------------------
# Shown at 0.42\textwidth, the same size as boot_small_n5.png, so it uses the
# same save width and base_size.
p_hist <- ggplot(dm_1999, aes(x = weight)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "white") +
  geom_vline(xintercept = mean(w), linetype = 2, linewidth = 0.9) +
  annotate("text", x = mean(w), y = Inf, vjust = 1.4, hjust = -0.06,
           size = 8, label = sprintf("mean = %.1f g", mean(w))) +
  labs(x = "Weight (g)", y = "Count") +
  theme_minimal(base_size = 30)

ggsave("figures/dm1999_hist.png", p_hist, width = 5.6, height = 3.2, dpi = 200)

# ------------------------------------------------------------------------------
# Figure: the bootstrap distribution of the mean, with the 95% percentile CI
# ------------------------------------------------------------------------------
ci95 <- quantile(boot_means, c(0.025, 0.975))

p_boot_mean <- ggplot(tibble(stat = boot_means), aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey80", colour = "white") +
  geom_vline(xintercept = ci95, linetype = 2, linewidth = 0.9) +
  annotate("text", x = mean(ci95), y = Inf, vjust = 1.4, size = 7,
           label = sprintf("95%% CI: %.2f to %.2f", ci95[1], ci95[2])) +
  labs(x = "Bootstrap mean weight (g)", y = "Count") +
  theme_minimal(base_size = 24)

ggsave("figures/dm1999_boot_mean.png", p_boot_mean,
       width = 5.6, height = 3.2, dpi = 200)

# ------------------------------------------------------------------------------
# Figure: why "80% confidence" is a statement about the procedure
# ------------------------------------------------------------------------------
# Treat the 348 observed weights as the population, so the true mean is known.
# Draw repeated samples, build an 80% interval from each, and count the misses.
# About 1 interval in 5 should fail to cover -- that is what the level means.
pop_mean <- mean(w)
n_draw   <- 30
n_reps   <- 12          # few enough that each interval is still separable
lev      <- 0.80

cis <- map_dfr(seq_len(n_reps), function(i) {
  s  <- sample(w, n_draw)
  se <- sd(s) / sqrt(n_draw)
  tc <- qt(1 - (1 - lev) / 2, df = n_draw - 1)
  tibble(rep = i, lower = mean(s) - tc * se, upper = mean(s) + tc * se)
}) |>
  mutate(covers = lower <= pop_mean & pop_mean <= upper)

# The missing intervals are colored and named on the plot instead of in a
# legend: at this display size a legend costs more room than it explains.
p_cartoon <- ggplot(cis, aes(y = rep)) +
  geom_vline(xintercept = pop_mean, linewidth = 0.9) +
  geom_segment(aes(x = lower, xend = upper, yend = rep, colour = covers),
               linewidth = 1.2) +
  geom_point(aes(x = (lower + upper) / 2, colour = covers), size = 1.6) +
  annotate("text", x = pop_mean, y = n_reps + 1.5, hjust = -0.08, size = 6,
           label = "true mean") +
  scale_colour_manual(values = c(`TRUE` = "grey55", `FALSE` = "#B23A48")) +
  scale_y_continuous(NULL, breaks = NULL,
                     expand = expansion(add = c(0.7, 2.2))) +
  labs(x = "Weight (g)",
       title = sprintf("%d%% CIs from %d samples: %d missed the true mean",
                       100 * lev, n_reps, sum(!cis$covers))) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(0.7)))

ggsave("figures/ci_cartoon.png", p_cartoon, width = 5.6, height = 3.4, dpi = 200)

cat("\nFigures written to figures/\n")
