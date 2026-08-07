# MA214 Lecture 12 -- Analysis of variance
# In-class demo + figure generation.
#
# Data:
#   data/plantgrowth.csv     30 plants in 3 groups of 10: a control and two growing
#                            conditions; outcome = dried weight (g).  Dobson (1983).
#                            Small enough to compute SSG and SSE by hand on a slide.
#   data/portal_rodents.csv  15,510 kangaroo rats (genus Dipodomys) from the Portal
#                            Project -- the same desert-rodent study as Lectures 9-11.
#                            Lecture 11 asked whether WHICH species you catch depends
#                            on the plot type; here the outcome is numeric instead.
#                            Both files are built by data/prepare_data.R.
#
# Run from this directory:  Rscript Lecture13_demo.R

library(tidyverse)

set.seed(214)

# The F-statistic, computed straight from the definition. We do NOT call aov() here
# because we need to evaluate it thousands of times inside the permutation loops.
# NOTE: never name a variable `df` in this script -- `df` is R's F density function,
# which stat_function() needs below.
f_stat <- function(y, g) {
  g   <- as.character(g)
  n   <- length(y)
  k   <- length(unique(g))
  gm  <- mean(y)                        # grand mean
  mu  <- tapply(y, g, mean)             # group means
  nn  <- tapply(y, g, length)           # group sizes
  ssg <- sum(nn * (mu - gm)^2)          # between groups
  sse <- sum((y - mu[g])^2)             # within groups
  (ssg / (k - 1)) / (sse / (n - k))
}

# ==============================================================================
# 1. Three groups of numbers
# ==============================================================================
plants <- read_csv("data/plantgrowth.csv", show_col_types = FALSE) |>
  mutate(group = factor(group))

cat("\n===== PlantGrowth: 30 plants, 3 groups =====\n")
print(plants |> group_by(group) |>
        summarise(n = n(), mean = mean(weight), sd = sd(weight)))

cat(sprintf("\nLargest SD / smallest SD = %.2f  (rule of thumb wants < 2)\n",
            max(tapply(plants$weight, plants$group, sd)) /
            min(tapply(plants$weight, plants$group, sd))))

# ==============================================================================
# 2. The arithmetic: SSG and SSE on 30 numbers
# ==============================================================================
y  <- plants$weight
g  <- plants$group
n  <- length(y)
k  <- nlevels(g)

grand <- mean(y)
mu    <- tapply(y, g, mean)
nn    <- tapply(y, g, length)

ssg <- sum(nn * (mu - grand)^2)
sse <- sum((y - mu[as.character(g)])^2)
sst <- sum((y - grand)^2)

msg <- ssg / (k - 1)
mse <- sse / (n - k)

cat("\n===== The arithmetic, by hand =====\n")
cat(sprintf("grand mean          = %.4f\n", grand))
cat(sprintf("SSG = sum n_i (xbar_i - xbar)^2 = %.4f   df1 = k - 1 = %d\n", ssg, k - 1))
cat(sprintf("SSE = sum (x_ij - xbar_i)^2     = %.4f   df2 = n - k = %d\n", sse, n - k))
cat(sprintf("SST = SSG + SSE                 = %.4f   (check: %.4f)\n", ssg + sse, sst))
cat(sprintf("MSG = SSG / %d = %.4f\n", k - 1, msg))
cat(sprintf("MSE = SSE / %d = %.4f\n", n - k, mse))
cat(sprintf("F   = MSG / MSE = %.4f / %.4f = %.4f\n", msg, mse, msg / mse))

cat("\n-- and R's own aov() agrees --\n")
fit <- aov(weight ~ group, data = plants)
print(summary(fit))

f_obs <- msg / mse
df1   <- k - 1
df2   <- n - k

# ==============================================================================
# 3. TWO ROADS to the null distribution of F
# ==============================================================================
# Road A -- shuffle the group labels. If every group has the same mean, a plant's
# growing condition had nothing to do with its weight: the labels are exchangeable.
B         <- 10000
perm_null <- replicate(B, f_stat(y, sample(g)))
p_shuffle <- mean(perm_null >= f_obs)

# Road B -- the mathematical model: F ~ F(df1, df2) when the conditions hold.
p_model <- pf(f_obs, df1, df2, lower.tail = FALSE)

cat("\n===== Two roads =====\n")
cat(sprintf("observed F                        : %.4f  on (%d, %d) df\n", f_obs, df1, df2))
cat(sprintf("randomization (B = %s shuffles) : p = %.4f\n",
            format(B, big.mark = ","), p_shuffle))
cat(sprintf("F model                           : p = %.4f\n", p_model))
cat(sprintf("mean of the shuffled null         : %.4f\n", mean(perm_null)))
cat(sprintf("theory: mean of F(%d, %d) = df2/(df2-2) = %d/%d = %.4f\n",
            df1, df2, df2, df2 - 2, df2 / (df2 - 2)))
cat("--> that is WHY 'under H0 we expect F about 1'.\n")

# ==============================================================================
# 4. So which groups differ?  (the pairwise question, asked safely)
# ==============================================================================
cat("\n===== ANOVA says 'at least one differs'. Which ones? =====\n")
cat("\n-- Tukey's honest significant differences --\n")
print(TukeyHSD(fit))

cat("\n-- pairwise t-tests with a Bonferroni correction --\n")
print(pairwise.t.test(plants$weight, plants$group, p.adjust.method = "bonferroni"))

cat("\n-- and with NO correction, for comparison --\n")
print(pairwise.t.test(plants$weight, plants$group, p.adjust.method = "none"))

# ==============================================================================
# 5. Portal: a real violation of equal variances
# ==============================================================================
portal <- read_csv("data/portal_rodents.csv", show_col_types = FALSE)

cat("\n===== Portal kangaroo rats: three species =====\n")
spp <- portal |> group_by(species_id, species) |>
  summarise(n = n(), mean = mean(weight), sd = sd(weight), .groups = "drop")
print(spp)
cat(sprintf("\nLargest SD / smallest SD = %.2f  <-- the rule of thumb FAILS\n",
            max(spp$sd) / min(spp$sd)))

cat("\n===== Portal: D. merriami by plot type (the activity) =====\n")
dm <- portal |> filter(species_id == "DM") |> mutate(plot_type = factor(plot_type))
print(dm |> group_by(plot_type) |>
        summarise(n = n(), mean = mean(weight), sd = sd(weight)))
dm_fit <- aov(weight ~ plot_type, data = dm)
print(summary(dm_fit))
cat(sprintf("\nLargest SD / smallest SD = %.2f  (conditions look fine here)\n",
            max(tapply(dm$weight, dm$plot_type, sd)) /
            min(tapply(dm$weight, dm$plot_type, sd))))
cat("Note the means span barely one gram: significant is not the same as large.\n")

# ==============================================================================
# 6. Does the condition actually matter?
#
#    The old slides claimed a permutation test "does not assume normality or equal
#    variance". Half of that is false. Shuffling assumes EXCHANGEABILITY: under H0
#    any observation could have carried any label. If the groups have different
#    spreads, that is not true even when the means are equal -- so shuffling breaks
#    too. Here is the evidence: we generate data in which H0 really IS true and
#    count how often each method rejects. A valid test rejects 5% of the time.
# ==============================================================================
alpha <- 0.05
R     <- 2000       # datasets per scenario
B_sim <- 999        # shuffles per dataset

perm_p <- function(y, g, B = B_sim) {
  obs  <- f_stat(y, g)
  null <- replicate(B, f_stat(y, sample(g)))
  (1 + sum(null >= obs)) / (B + 1)
}

scenarios <- list(
  # skewed (exponential) but equal spread and equal group sizes
  `Skewed data, equal spread` = function() {
    tibble(y = rexp(45), g = factor(rep(1:3, each = 15)))
  },
  # normal, but the smallest group has four times the spread of the others
  `Unequal spread, unequal n` = function() {
    tibble(y = c(rnorm(5, 0, 4), rnorm(10, 0, 1), rnorm(30, 0, 1)),
           g = factor(rep(1:3, times = c(5, 10, 30))))
  }
)

type1 <- imap_dfr(scenarios, function(gen, label) {
  hits <- replicate(R, {
    d <- gen()
    c(`F model`     = summary(aov(y ~ g, data = d))[[1]][1, 5] < alpha,
      `shuffling`   = perm_p(d$y, d$g) < alpha,
      `Welch`       = oneway.test(y ~ g, data = d, var.equal = FALSE)$p.value < alpha)
  })
  tibble(scenario = label, method = rownames(hits), rate = rowMeans(hits))
})

cat("\n===== How often does each method reject when H0 is TRUE? (should be 0.05) =====\n")
print(type1 |> pivot_wider(names_from = method, values_from = rate))

mc_se <- sqrt(alpha * (1 - alpha) / R)

# ==============================================================================
# Figures
# ==============================================================================

## (a) THE punchline: the F curve on top of the shuffled null -------------------
p_punch <- ggplot(tibble(f = perm_null), aes(x = f)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "grey80", colour = "white") +
  stat_function(fun = df, args = list(df1 = df1, df2 = df2), linewidth = 1.1) +
  geom_vline(xintercept = f_obs, linetype = 2) +
  annotate("text", x = f_obs + 0.25, y = 0.55,
           label = sprintf("observed F = %.2f", f_obs), hjust = 0, size = 6.1) +
  coord_cartesian(xlim = c(0, 8)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "F", y = NULL) +
  theme_minimal(base_size = 23)

ggsave("figures/f_vs_shuffle.png", p_punch, width = 6.8, height = 3.4, dpi = 200)

## (b) the plants themselves ---------------------------------------------------
p_box <- ggplot(plants, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.size = 1) +
  geom_jitter(width = 0.12, size = 1.4, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke = 1.1) +
  geom_hline(yintercept = grand, linetype = 3) +
  annotate("text", x = 3.42, y = grand + 0.06, label = "grand mean",
           size = 5.8, hjust = 1) +
  labs(x = NULL, y = "Dried weight (g)") +
  theme_minimal(base_size = 21)

ggsave("figures/plantgrowth_box.png", p_box, width = 6.4, height = 3.2, dpi = 200)

## (c) the p-value is the right-hand tail area ---------------------------------
tail_x <- seq(f_obs, 8, length.out = 200)
p_tail <- ggplot(tibble(x = c(0, 8)), aes(x = x)) +
  stat_function(fun = df, args = list(df1 = df1, df2 = df2), linewidth = 0.9) +
  geom_area(data = tibble(x = tail_x, y = df(tail_x, df1, df2)),
            aes(x = x, y = y), fill = "grey60") +
  geom_vline(xintercept = f_obs, linetype = 2) +
  annotate("text", x = f_obs + 0.2, y = 0.28, hjust = 0, size = 6.1,
           label = sprintf("F = %.2f", f_obs)) +
  annotate("text", x = f_obs + 1.6, y = 0.06, hjust = 0, size = 6.1,
           label = sprintf("p = %.4f", p_model)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "F", y = NULL) +
  theme_minimal(base_size = 21)

ggsave("figures/f_tail.png", p_tail, width = 6.4, height = 3.0, dpi = 200)

## (d) what the two df parameters do -------------------------------------------
shapes <- bind_rows(
  expand_grid(x = seq(0.01, 5, length.out = 300), d1 = c(2, 5, 20)) |>
    mutate(y = df(x, d1, 25), panel = "df2 = 25, varying df1",
           lab = factor(d1, labels = paste("df1 =", c(2, 5, 20)))),
  expand_grid(x = seq(0.01, 5, length.out = 300), d2 = c(5, 25, 100)) |>
    mutate(y = df(x, 5, d2), panel = "df1 = 5, varying df2",
           lab = factor(d2, labels = paste("df2 =", c(5, 25, 100))))
)

p_shapes <- ggplot(shapes, aes(x = x, y = y, linetype = lab)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~panel) +
  coord_cartesian(ylim = c(0, 1.05)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "F", y = NULL, linetype = NULL) +
  theme_minimal(base_size = 21) +
  theme(legend.position = "bottom")

ggsave("figures/f_shapes.png", p_shapes, width = 7.4, height = 3.2, dpi = 200)

## (e) between vs within variability -------------------------------------------
set.seed(42)
vary <- bind_rows(
  tibble(scenario = "Between >> within: clear evidence",
         g = factor(rep(c("A", "B", "C"), each = 20)),
         y = rnorm(60, mean = rep(c(4, 7, 10), each = 20), sd = 1)),
  tibble(scenario = "Between << within: no evidence",
         g = factor(rep(c("A", "B", "C"), each = 20)),
         y = rnorm(60, mean = rep(c(6.5, 7, 7.5), each = 20), sd = 3))
)

p_vary <- ggplot(vary, aes(x = g, y = y)) +
  geom_jitter(width = 0.12, size = 1.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3.5, stroke = 1.2) +
  facet_wrap(~scenario) +
  labs(x = NULL, y = "Response") +
  theme_minimal(base_size = 21)

ggsave("figures/anova_variability.png", p_vary, width = 7.4, height = 3.2, dpi = 200)

## (f) diagnostics -------------------------------------------------------------
diag <- tibble(fitted = fitted(fit), resid = residuals(fit))

p_rf <- ggplot(diag, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 1.6, alpha = 0.7) +
  labs(x = "Group mean", y = "Residual") +
  theme_minimal(base_size = 21)

p_qq <- ggplot(diag, aes(sample = resid)) +
  stat_qq(size = 1.6, alpha = 0.7) + stat_qq_line(linetype = 2) +
  labs(x = "Theoretical quantile", y = "Sample quantile") +
  theme_minimal(base_size = 21)

ggsave("figures/anova_resid.png", p_rf, width = 3.9, height = 3.3, dpi = 200)
ggsave("figures/anova_qq.png",    p_qq, width = 3.9, height = 3.3, dpi = 200)

## (g) the Portal species: what a real violation looks like ---------------------
p_spp <- portal |>
  mutate(species = paste0("D. ", species)) |>
  ggplot(aes(x = species, y = weight)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.size = 0.6,
               outlier.alpha = 0.3) +
  labs(x = NULL, y = "Weight (g)") +
  theme_minimal(base_size = 21)

ggsave("figures/portal_species.png", p_spp, width = 6.6, height = 3.2, dpi = 200)

## (h) the condition check: where each road actually breaks ---------------------
p_cond <- type1 |>
  mutate(method = factor(method, levels = c("F model", "shuffling", "Welch"))) |>
  ggplot(aes(x = method, y = rate)) +
  annotate("rect", xmin = 0.4, xmax = 3.6, ymin = alpha - 2 * mc_se,
           ymax = alpha + 2 * mc_se, fill = "grey85") +
  geom_hline(yintercept = alpha, linewidth = 0.4) +
  geom_col(width = 0.55, fill = "grey55", colour = "white") +
  geom_text(aes(label = sprintf("%.2f", rate)), vjust = -0.5, size = 6.1) +
  facet_wrap(~scenario) +
  expand_limits(y = 0.42) +
  labs(x = NULL, y = "Rejection rate when H0 is true") +
  theme_minimal(base_size = 21)

ggsave("figures/anova_condition_check.png", p_cond, width = 7.6, height = 3.6, dpi = 200)

cat("\nAll figures written to figures/\n")
