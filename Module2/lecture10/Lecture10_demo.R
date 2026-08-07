# MA214 Lecture 10 -- Inference with mathematical models
# In-class demo + figure generation.
#
# Data:
#   data/dm1999.csv   the 348 kangaroo rats weighed in 1999 (same sample as Lecture 9,
#                     so today's formulas can be checked against last lecture's bootstrap)
#   data/textbooks.csv  73 textbooks priced at the UCLA bookstore and at Amazon (paired)
#
# Run from this directory:  Rscript Lecture11_demo.R

library(tidyverse)

set.seed(214)

dm_1999 <- read_csv("data/dm1999.csv", show_col_types = FALSE)
w <- dm_1999$weight
n <- length(w)

# For the CLT demo we want a population that looks nothing like a normal curve:
# every rodent weighed at the site, all species pooled -- multimodal and strongly
# right-skewed (skewness +2.3, from 4 g to 280 g).
rodents <- read_csv("data/rodents_all.csv", show_col_types = FALSE)
pop <- rodents$weight
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
cat(sprintf("\nCLT demo population: all %d rodents, skewness = %.2f (range %d-%d g)\n",
            length(pop), skew(pop), min(pop), max(pop)))

cat("\n===== 1. The CLT reproduces last lecture's bootstrap =====\n")
xbar <- mean(w)
s    <- sd(w)
se   <- s / sqrt(n)                      # the formula
cat(sprintf("n = %d   xbar = %.2f   s = %.2f\n", n, xbar, s))
cat(sprintf("SE = s/sqrt(n) = %.2f/sqrt(%d) = %.3f\n", s, n, se))

# Lecture 9 got SE_boot by resampling 2000 times:
boot_means <- replicate(2000, mean(sample(w, replace = TRUE)))
cat(sprintf("SE_boot (2000 resamples, Lecture 9) = %.3f\n", sd(boot_means)))

cat("\n----- 95% CIs for mu: three roads -----\n")
ci_formula <- xbar + c(-1, 1) * qnorm(0.975) * se
ci_boot_pct <- quantile(boot_means, c(0.025, 0.975))
ci_boot_se  <- xbar + c(-1, 1) * 2 * sd(boot_means)
cat(sprintf("bootstrap percentile : [%.2f, %.2f]\n", ci_boot_pct[1], ci_boot_pct[2]))
cat(sprintf("bootstrap SE         : [%.2f, %.2f]\n", ci_boot_se[1],  ci_boot_se[2]))
cat(sprintf("CLT formula (z)      : [%.2f, %.2f]\n", ci_formula[1],  ci_formula[2]))

cat("\n===== 2. Exact calculations: the binomial =====\n")
# 20 Bernoulli trials, H0: p = 0.5. How likely is 8 or more successes?
cat(sprintf("P(X >= 8 | n=20, p=0.5) exactly       = %.4f\n", 1 - pbinom(7, 20, 0.5)))
# what simulation gives, for a few values of B
for (B in c(100, 1000, 10000, 100000)) {
  cat(sprintf("  simulated with B = %6d              = %.4f\n",
              B, mean(replicate(B, sum(rbinom(20, 1, 0.5))) >= 8)))
}

# Activity: catch 10 rats, 8 are male. Two-sided exact p-value against p = 0.5.
p_exact <- sum(dbinom(0:10, 10, 0.5)[dbinom(0:10, 10, 0.5) <= dbinom(8, 10, 0.5) + 1e-12])
cat(sprintf("\n10 rats, 8 male: two-sided exact p = %.4f\n", p_exact))
cat(sprintf("   (one-sided P(X >= 8)            = %.4f)\n", 1 - pbinom(7, 10, 0.5)))

cat("\n===== 3. The normal model =====\n")
cat(sprintf("A 62 g rat: z = (62 - %.2f)/%.2f = %.2f\n", xbar, s, (62 - xbar) / s))
cat(sprintf("P(X > 62) under N(%.2f, %.2f) = %.4f\n", xbar, s, pnorm(62, xbar, s, lower.tail = FALSE)))

cat("\n===== 4. The z-test: redo Lecture 8's office-hours example =====\n")
# 29 of 40 students prefer online office hours. H0: p = 0.5 (Lecture 8 got p = 0.005
# by randomization).
phat <- 29 / 40; p0 <- 0.5; n_oh <- 40
se0  <- sqrt(p0 * (1 - p0) / n_oh)          # SE under H0
z    <- (phat - p0) / se0
cat(sprintf("phat = %.3f   SE0 = sqrt(p0(1-p0)/n) = %.4f   z = %.2f\n", phat, se0, z))
cat(sprintf("p-value (formula, two-sided) = %.4f\n", 2 * pnorm(-abs(z))))
cat(sprintf("p-value (randomization, Lecture 8) = 0.005\n"))
cat(sprintf("p-value (exact binomial)     = %.4f\n",
            2 * min(pbinom(29, 40, 0.5, lower.tail = FALSE) + dbinom(29, 40, 0.5),
                    pbinom(29, 40, 0.5))))

cat("\n===== 5. Two independent samples: male vs female rats =====\n")
wm <- dm_1999$weight[dm_1999$sex == "M"]
wf <- dm_1999$weight[dm_1999$sex == "F"]
se_diff <- sqrt(var(wm) / length(wm) + var(wf) / length(wf))
d_obs   <- mean(wm) - mean(wf)
cat(sprintf("males:   n = %3d  mean = %.2f  s = %.2f\n", length(wm), mean(wm), sd(wm)))
cat(sprintf("females: n = %3d  mean = %.2f  s = %.2f\n", length(wf), mean(wf), sd(wf)))
cat(sprintf("difference = %.2f   SE = sqrt(s1^2/n1 + s2^2/n2) = %.3f\n", d_obs, se_diff))
print(t.test(wm, wf))

cat("\n===== 6. Paired data: textbook prices =====\n")
tb <- read_csv("data/textbooks.csv", show_col_types = FALSE)
d  <- tb$diff                                  # UCLA - Amazon, per book
cat(sprintf("n pairs = %d   mean difference = %.2f   s = %.2f   SE = %.2f\n",
            length(d), mean(d), sd(d), sd(d) / sqrt(length(d))))
print(t.test(tb$ucla_new, tb$amaz_new, paired = TRUE))
cat("\n-- treating them (wrongly) as two independent samples:\n")
print(t.test(tb$ucla_new, tb$amaz_new)$p.value)

cat("\n===== 7. t vs z critical values =====\n")
for (df in c(2, 5, 30, n - 1)) {
  cat(sprintf("  df = %3d   t* = %.3f\n", df, qt(0.975, df)))
}
cat(sprintf("  normal     z* = %.3f\n", qnorm(0.975)))

# ==============================================================================
# Figures
# ==============================================================================

## (a) the CLT emerging: a hopeless-looking population, but a normal sample mean ----
sizes <- c(5, 20, 100)

samp_dist <- map_dfr(sizes, function(m) {
  tibble(size = m, value = replicate(4000, mean(sample(pop, m, replace = TRUE))))
})

panels <- bind_rows(
  tibble(panel = "the population (one rodent at a time)", value = pop),
  samp_dist |>
    mutate(panel = paste0("mean of n = ", size, " rodents")) |>
    select(panel, value)
) |>
  mutate(panel = factor(panel, levels = c("the population (one rodent at a time)",
                                          paste0("mean of n = ", sizes, " rodents"))))

# what the CLT predicts for each panel: N(mu, sigma/sqrt(n))
clt_pred <- tibble(
  panel = factor(paste0("mean of n = ", sizes, " rodents"), levels = levels(panels$panel)),
  mu    = mean(pop),
  sigma = sd(pop) / sqrt(sizes)
)

p_clt <- ggplot(panels, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45,
                 fill = "grey80", colour = "white") +
  geom_line(
    data = clt_pred |>
      rowwise() |>
      reframe(panel = panel,
              value = seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 200),
              dens  = dnorm(value, mu, sigma)),
    aes(x = value, y = dens), linewidth = 0.9
  ) +
  facet_wrap(~panel, scales = "free", nrow = 1) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "Weight (g)", y = NULL) +
  theme_minimal(base_size = 23)

ggsave("figures/clt_emerges.png", p_clt, width = 9.5, height = 2.8, dpi = 200)

## (b) THE punchline: the CLT curve on top of Lecture 9's bootstrap ----------
p_punch <- ggplot(tibble(stat = boot_means), aes(x = stat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey80", colour = "white") +
  stat_function(fun = dnorm, args = list(mean = xbar, sd = se),
                linewidth = 1.1, colour = "black") +
  labs(x = "Bootstrap mean weight (g)", y = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal(base_size = 25)

ggsave("figures/clt_vs_bootstrap.png", p_punch, width = 6.8, height = 3.4, dpi = 200)

## (c) t vs normal ------------------------------------------------------------
tgrid <- map_dfr(c(2, 5, 30), function(df) {
  tibble(x = seq(-4, 4, length.out = 400), density = dt(x, df),
         curve = factor(paste0("t, df = ", df),
                        levels = paste0("t, df = ", c(2, 5, 30))))
})

p_t <- ggplot(tgrid, aes(x = x, y = density)) +
  stat_function(fun = dnorm, linewidth = 1.2, colour = "black") +
  geom_line(aes(linetype = curve, colour = curve), linewidth = 0.7) +
  scale_linetype_manual(values = c("dashed", "dotdash", "dotted")) +
  scale_colour_grey(start = 0.1, end = 0.55) +
  annotate("text", x = 1.35, y = 0.385, label = "normal", size = 7.6, hjust = 0) +
  annotate("segment", x = 0.15, xend = 1.3, y = 0.397, yend = 0.387,
           linewidth = 0.3) +
  labs(x = NULL, y = NULL, linetype = NULL, colour = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal(base_size = 25) +
  theme(legend.position = "bottom")

ggsave("figures/t_vs_normal.png", p_t, width = 6.8, height = 3.4, dpi = 200)

## (d) two independent samples: male vs female -------------------------------
group_means <- tibble(sex = c("Female", "Male"), m = c(mean(wf), mean(wm)))

p_two <- dm_1999 |>
  mutate(sex = recode(sex, M = "Male", F = "Female")) |>
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth = 2, fill = "grey80", colour = "white") +
  geom_vline(data = group_means, aes(xintercept = m), linewidth = 0.9) +
  facet_wrap(~sex, ncol = 1) +
  labs(x = "Weight (g)", y = "Count") +
  theme_minimal(base_size = 23)

ggsave("figures/two_sample_sex.png", p_two, width = 6.8, height = 3.6, dpi = 200)

## (e) paired data: textbook price differences --------------------------------
p_pair <- ggplot(tb, aes(x = diff)) +
  geom_histogram(binwidth = 5, fill = "grey80", colour = "white") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_vline(xintercept = mean(d), linewidth = 1) +
  labs(x = "UCLA price - Amazon price, per book ($)", y = "Count") +
  theme_minimal(base_size = 25)

ggsave("figures/paired_textbooks.png", p_pair, width = 6.8, height = 3.2, dpi = 200)

cat("\nFigures written to figures/\n")
