# Lecture 8: Hypothesis testing with randomization -- data + figures
# Generates the two example datasets and every figure the deck uses.
# Run from this directory:  Rscript Lecture8_demo.R

library(tidyverse)

theme_set(
  theme_classic(base_size = 23) +
    theme(
      plot.title  = element_text(face = "bold"),
      axis.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

dir.create("figures", showWarnings = FALSE)
dir.create("data",    showWarnings = FALSE)

B <- 10000

# =========================================================================
# EXAMPLE 1 -- one proportion: do more than half of students prefer
# online office hours?   H0: p = 0.5   HA: p != 0.5
# =========================================================================
set.seed(214)          # example 1 gets its own seed, so it is reproducible
                       # independently of everything below
n1  <- 40
p0  <- 0.50

# The observed survey (generated once, then saved so the slides are stable).
survey <- tibble(student = 1:n1,
                 prefers_online = rbinom(n1, 1, 0.68))
write_csv(survey, "data/office_hours.csv")

x_obs    <- sum(survey$prefers_online)
phat_obs <- x_obs / n1
cat("=== EXAMPLE 1: one proportion ===\n")
cat(sprintf("n = %d, successes = %d, phat = %.3f\n", n1, x_obs, phat_obs))

# Simulate the NULL: if H0 were true, each student is a coin flip.
phat_null <- replicate(B, mean(rbinom(n1, 1, p0)))

# Two-sided p-value: how often is the simulated phat at least as FAR from
# 0.5 as what we actually saw?
pval1 <- mean(abs(phat_null - p0) >= abs(phat_obs - p0))
cat(sprintf("p-value (two-sided, %d sims) = %.4f\n", B, pval1))
cat(sprintf("s-value = -log2(p) = %.1f  (like that many heads in a row)\n",
            -log2(pval1)))

# --- Figure: the null distribution IS the p-value picture ---------------
d1 <- tibble(phat = phat_null) |>
  mutate(extreme = abs(phat - p0) >= abs(phat_obs - p0))

p_fig1 <- ggplot(d1, aes(phat, fill = extreme)) +
  geom_histogram(binwidth = 1 / n1, colour = "white") +
  geom_vline(xintercept = phat_obs, colour = "#D95F02", linewidth = 1.3) +
  annotate("text", x = phat_obs + 0.012, y = Inf, vjust = 1.6, hjust = 0,
           label = sprintf("observed\np-hat = %.2f", phat_obs),
           colour = "#D95F02", fontface = "bold", size = 6) +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "#D95F02"),
                    guide = "none") +
  labs(x = expression(hat(p)~"simulated under"~H[0]~":"~p==0.5),
       y = "count")
ggsave("figures/null-dist-proportion.png", plot = p_fig1,
       width = 7.5, height = 4.4, dpi = 150)

# =========================================================================
# EXAMPLE 2 -- two groups: do two teaching methods differ in mean score?
# H0: mu_A = mu_B    HA: mu_A != mu_B
# =========================================================================
# Seed chosen so that the rounded means shown on the slides subtract exactly
# to the rounded difference -- otherwise double-rounding makes the arithmetic
# on the slide look wrong (80.34 - 75.06 = 5.28, but the true diff is 5.29).
set.seed(1)
nA <- 30; nB <- 30
quiz <- bind_rows(
  tibble(group = "A", score = round(rnorm(nA, mean = 79, sd = 8), 1)),
  tibble(group = "B", score = round(rnorm(nB, mean = 74, sd = 8), 1))
)
write_csv(quiz, "data/quiz_scores.csv")

diff_obs <- mean(quiz$score[quiz$group == "A"]) -
            mean(quiz$score[quiz$group == "B"])
cat("\n=== EXAMPLE 2: difference in means ===\n")
cat(sprintf("mean A = %.2f (n=%d), mean B = %.2f (n=%d)\n",
            mean(quiz$score[quiz$group == "A"]), nA,
            mean(quiz$score[quiz$group == "B"]), nB))
cat(sprintf("observed difference = %.2f\n", diff_obs))

# Simulate the NULL by PERMUTING the labels: if the method has no effect,
# a student's score would have been the same under either label.
diff_null <- replicate(B, {
  shuffled <- sample(quiz$group)
  mean(quiz$score[shuffled == "A"]) - mean(quiz$score[shuffled == "B"])
})
pval2 <- mean(abs(diff_null) >= abs(diff_obs))
cat(sprintf("p-value (two-sided, %d permutations) = %.4f\n", B, pval2))
cat(sprintf("s-value = %.1f\n", -log2(pval2)))

d2 <- tibble(diff = diff_null) |> mutate(extreme = abs(diff) >= abs(diff_obs))

p_fig2 <- ggplot(d2, aes(diff, fill = extreme)) +
  geom_histogram(bins = 45, colour = "white") +
  geom_vline(xintercept = diff_obs, colour = "#D95F02", linewidth = 1.3) +
  annotate("text", x = diff_obs + 0.35, y = Inf, vjust = 1.6, hjust = 0,
           label = sprintf("observed\ndiff = %.2f", diff_obs),
           colour = "#D95F02", fontface = "bold", size = 6) +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = "#D95F02"),
                    guide = "none") +
  labs(x = expression(bar(x)[A] - bar(x)[B]~"after shuffling the labels"),
       y = "count")
ggsave("figures/null-dist-permutation.png", plot = p_fig2,
       width = 7.5, height = 4.4, dpi = 150)

# =========================================================================
# How many simulations is enough?  The null distribution settles down.
# =========================================================================
set.seed(99)
reps <- c(100, 1000, 10000)
d3 <- map_dfr(reps, function(B_i) {
  tibble(B = B_i,
         phat = replicate(B_i, mean(rbinom(n1, 1, p0))))
}) |>
  mutate(B = factor(B, levels = reps, labels = paste("B =", format(reps, big.mark = ","))))

p_fig3 <- ggplot(d3, aes(phat)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1 / n1,
                 fill = "grey75", colour = "white") +
  geom_vline(xintercept = phat_obs, colour = "#D95F02", linewidth = 1) +
  facet_wrap(~B) +
  labs(x = expression(hat(p)~"under"~H[0]), y = NULL)
ggsave("figures/how-many-sims.png", plot = p_fig3,
       width = 9, height = 3.4, dpi = 150)

# p-value stability across B
pv <- map_dfr(c(100, 500, 1000, 5000, 10000, 50000), function(B_i) {
  s <- replicate(B_i, mean(rbinom(n1, 1, p0)))
  tibble(B = B_i, p = mean(abs(s - p0) >= abs(phat_obs - p0)))
})
cat("\n=== p-value vs number of simulations ===\n"); print(pv)
cat(sprintf("\nEXACT binomial p-value = %.5f  <- the truth the simulations wobble around\n",
            2 * (1 - pbinom(x_obs - 1, n1, p0))))
cat("NB: B = 100 can return p = 0. A simulated p-value is never really 0 --\n")
cat("    all it means is 'smaller than 1/B'. Report p < 1/B, never p = 0.\n")

cat("\nDone. Figures written to figures/.\n")
