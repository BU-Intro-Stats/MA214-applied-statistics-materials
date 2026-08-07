# MA214 Lecture 13 -- Inference about the slope
# In-class demo + figure generation.
#
# Data (both frozen by data/prepare_data.R; both REAL):
#   data/penguins.csv       40 Palmer penguins. Regress body mass (g) on flipper
#                           length (mm). A sample -- is the slope real or noise?
#   data/kangaroo_rats.csv  347 Dipodomys merriami from Portal, 1999 -- the same
#                           rats as Lectures 9-10. Regress weight on hindfoot
#                           length. Used by the in-class activity.
#
# Run from this directory:  Rscript Lecture13_demo.R

library(tidyverse)

set.seed(214)

# Slope of y on x, straight from the definition -- fast enough for 10,000 reps.
slope_of <- function(x, y) cov(x, y) / var(x)

# t-statistic for the slope: slope / SE(slope), all from the definitions. This is
# the scale-free statistic whose null distribution is t_{n-2} (the raw slope is
# not scale-free, so its permutation spread would not match the model SE).
t_of <- function(x, y) {
  n  <- length(x)
  b  <- cov(x, y) / var(x)
  a  <- mean(y) - b * mean(x)
  se <- sqrt(sum((y - a - b * x)^2) / (n - 2)) / (sd(x) * sqrt(n - 1))
  b / se
}

# ==============================================================================
# 1. The penguins, and the line
# ==============================================================================
peng <- read_csv("data/penguins.csv", show_col_types = FALSE)
x <- peng$flipper_length_mm
y <- peng$body_mass_g
n <- nrow(peng)

fit <- lm(body_mass_g ~ flipper_length_mm, data = peng)
co  <- summary(fit)$coefficients
b1  <- co["flipper_length_mm", "Estimate"]
se1 <- co["flipper_length_mm", "Std. Error"]
t1  <- co["flipper_length_mm", "t value"]
df  <- n - 2
p_formula <- co["flipper_length_mm", "Pr(>|t|)"]
ci_t <- confint(fit)["flipper_length_mm", ]

cat("\n===== Penguins: lm(body_mass_g ~ flipper_length_mm), n =", n, "=====\n")
cat(sprintf("slope  b1 = %.3f g per mm\n", b1))
cat(sprintf("SE(b1)    = %.3f     (formula: s_e / (s_x * sqrt(n-1)) = %.3f)\n",
            se1, sd(residuals(fit)) * sqrt((n-1)/df) / (sd(x) * sqrt(n-1))))
cat(sprintf("t = b1/SE = %.3f  on df = n - 2 = %d\n", t1, df))
cat(sprintf("model-based p (two-sided) = %.3g\n", p_formula))
cat(sprintf("model-based 95%% CI: [%.2f, %.2f]\n", ci_t[1], ci_t[2]))

# Full coefficient table (both rows) -- this is what the "Reading it off lm()"
# slide prints, so every number on that slide traces back to here.
cat("\n-- full coefficient table with 95% CIs (intercept + slope) --\n")
print(round(cbind(summary(fit)$coefficients, confint(fit)), 2))

# ==============================================================================
# 2. TWO ROADS -- the test of H0: beta1 = 0
# ==============================================================================
B <- 10000

# Road A: shuffle. If flipper length is unrelated to mass, the pairing is random,
# so break it -- shuffle y against x and recompute the t-statistic each time.
perm_t <- replicate(B, t_of(x, sample(y)))
p_perm <- mean(abs(perm_t) >= abs(t1))

# Road B: the t model. t = b1 / SE ~ t_{n-2} under H0.
p_model <- 2 * pt(-abs(t1), df = df)

cat("\n===== Two roads: is the slope real? =====\n")
cat(sprintf("Road A  shuffle (B = %s): p = %.4f\n", format(B, big.mark = ","), p_perm))
cat(sprintf("Road B  t_%d model      : p = %.3g\n", df, p_model))
cat(sprintf("shuffled t: mean = %.3f (t_%d has mean 0), sd = %.3f (t_%d has sd %.3f)\n",
            mean(perm_t), df, sd(perm_t), df, sqrt(df / (df - 2))))

# ==============================================================================
# 3. TWO ROADS -- the confidence interval
# ==============================================================================
# Bootstrap: resample (x, y) PAIRS with replacement, keep the x-y link.
boot_slopes <- replicate(B, {
  i <- sample(n, replace = TRUE)
  slope_of(x[i], y[i])
})
ci_boot <- quantile(boot_slopes, c(0.025, 0.975))

cat("\n===== Two roads: confidence interval for the slope =====\n")
cat(sprintf("Road A  bootstrap percentile 95%%: [%.2f, %.2f]\n", ci_boot[1], ci_boot[2]))
cat(sprintf("Road B  t-based 95%%             : [%.2f, %.2f]\n", ci_t[1], ci_t[2]))

# ==============================================================================
# 4. The activity data: kangaroo rats
# ==============================================================================
kr <- read_csv("data/kangaroo_rats.csv", show_col_types = FALSE)
krfit <- lm(weight ~ hindfoot_length, data = kr)
kco <- summary(krfit)$coefficients
cat("\n===== Kangaroo rats: weight ~ hindfoot_length, n =", nrow(kr), "=====\n")
cat(sprintf("slope = %.3f g/mm   SE = %.3f   t = %.2f   p = %.3g\n",
            kco[2,1], kco[2,2], kco[2,3], kco[2,4]))
cat(sprintf("model t-based 95%% CI: [%.2f, %.2f]   (t* = %.2f)\n",
            confint(krfit)[2,1], confint(krfit)[2,2], qt(0.975, nrow(kr) - 2)))
kr_boot <- replicate(B, { i <- sample(nrow(kr), replace = TRUE)
  slope_of(kr$hindfoot_length[i], kr$weight[i]) })
cat(sprintf("bootstrap 95%% CI:      [%.2f, %.2f]   (used by the worksheet)\n",
            quantile(kr_boot, 0.025), quantile(kr_boot, 0.975)))

# ==============================================================================
# 5. When a condition fails: the two roads DISAGREE, and a transform repairs it
# ==============================================================================
loans <- read_csv("data/loans.csv", show_col_types = FALSE)
lx <- loans$annual_income; ly <- loans$loan_amount; ln <- nrow(loans)

lm_raw <- lm(loan_amount ~ annual_income, data = loans)
ci_raw_t <- confint(lm_raw)["annual_income", ]
boot_loan <- replicate(B, { i <- sample(ln, replace = TRUE); slope_of(lx[i], ly[i]) })
ci_raw_boot <- quantile(boot_loan, c(0.025, 0.975))

lm_log <- lm(log(loan_amount) ~ log(annual_income), data = loans)

cat("\n===== Loans: loan_amount ~ annual_income ($000s), n =", ln, "=====\n")
cat(sprintf("slope = %.1f   model t-CI [%.1f, %.1f]   bootstrap CI [%.1f, %.1f]\n",
            coef(lm_raw)[2], ci_raw_t[1], ci_raw_t[2], ci_raw_boot[1], ci_raw_boot[2]))
cat(sprintf("funnel cor(|resid|,fitted): raw = %.2f, log-log = %.2f;  log-log slope = %.2f\n",
            cor(abs(resid(lm_raw)), fitted(lm_raw)),
            cor(abs(resid(lm_log)), fitted(lm_log)), coef(lm_log)[2]))

# ==============================================================================
# Figures
# ==============================================================================
grey_line <- "grey30"

## (a) the scatter and the fitted line ----------------------------------------
p_scatter <- ggplot(peng, aes(flipper_length_mm, body_mass_g)) +
  geom_point(size = 2, alpha = 0.8, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_minimal(base_size = 27)
ggsave("figures/penguins_scatter.png", p_scatter, width = 6.4, height = 3.4, dpi = 200)

## (b) Road A intuition: shuffled lines are flat ------------------------------
set.seed(99)
shuf <- map_dfr(1:60, function(i) {
  tibble(flipper_length_mm = x, ys = sample(y), rep = i)
})
p_perm_lines <- ggplot() +
  geom_abline(data = map_dfr(1:60, ~{
      ys <- sample(y); m <- lm(ys ~ x); tibble(int = coef(m)[1], sl = coef(m)[2])
    }),
    aes(intercept = int, slope = sl), colour = "grey75", linewidth = 0.3) +
  geom_point(data = peng, aes(flipper_length_mm, body_mass_g),
             size = 1.6, alpha = 0.7, colour = "grey30") +
  geom_smooth(data = peng, aes(flipper_length_mm, body_mass_g),
              method = "lm", se = FALSE, colour = "black", linewidth = 1) +
  labs(x = "Flipper length (mm)", y = "Body mass (g)") +
  theme_minimal(base_size = 25)
ggsave("figures/perm_lines.png", p_perm_lines, width = 6.6, height = 3.4, dpi = 200)

## (c) THE payoff #1: permutation null of t vs the t_{n-2} model (the test) -----
tcurve <- tibble(tt = seq(-4.5, 4.5, length.out = 400), d = dt(tt, df = df))
p_null <- ggplot(tibble(tt = perm_t), aes(tt)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "grey80", colour = "white") +
  geom_line(data = tcurve, aes(tt, d), linewidth = 1.1) +
  coord_cartesian(xlim = c(-4.5, 4.5)) +
  scale_y_continuous(breaks = NULL) +
  annotate("segment", x = 3.9, xend = 4.4, y = 0.06, yend = 0.06,
           arrow = arrow(length = unit(0.15, "cm"))) +
  annotate("text", x = 3.8, y = 0.11, hjust = 1, size = 6.9,
           label = sprintf("observed t = %.1f,\nfar off to the right", t1)) +
  labs(x = expression("t-statistic under "*H[0]), y = NULL) +
  theme_minimal(base_size = 27)
ggsave("figures/slope_null_overlay.png", p_null, width = 6.8, height = 3.4, dpi = 200)

## (d) THE payoff #2: bootstrap CI vs the t CI (the interval) ------------------
p_boot <- ggplot(tibble(s = boot_slopes), aes(s)) +
  geom_histogram(bins = 50, fill = "grey80", colour = "white") +
  geom_vline(xintercept = ci_boot, linetype = 2) +
  geom_vline(xintercept = ci_t, linetype = 3, colour = "grey40") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.6, size = 6.2,
           label = "dashed: bootstrap CI") +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 3.2, size = 6.2,
           colour = "grey40", label = "dotted: t formula CI") +
  labs(x = expression("bootstrap slope "*hat(beta)[1]^"*"), y = "count") +
  theme_minimal(base_size = 27)
ggsave("figures/slope_boot_ci.png", p_boot, width = 6.8, height = 3.2, dpi = 200)

## (e) LINE diagnostics --------------------------------------------------------
diag <- tibble(fitted = fitted(fit), resid = residuals(fit))
p_rf <- ggplot(diag, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 1.8, alpha = 0.75, colour = "grey30") +
  labs(x = "Fitted value", y = "Residual") +
  theme_minimal(base_size = 25)
p_qq <- ggplot(diag, aes(sample = resid)) +
  stat_qq(size = 1.8, alpha = 0.75, colour = "grey30") + stat_qq_line(linetype = 2) +
  labs(x = "Theoretical quantile", y = "Sample quantile") +
  theme_minimal(base_size = 25)
# These two sit side by side in a \twocol{0.5}{0.5} frame, so each is shown at
# 0.94 of a HALF-width column (~2.0in). Saved at 8in their text was shrunk ~3.9x
# and landed near 6pt; at 5.1in it lands near slide body size.
ggsave("figures/resid_fitted.png", p_rf, width = 5.1, height = 4.3, dpi = 200)
ggsave("figures/qq.png",          p_qq, width = 5.1, height = 4.3, dpi = 200)

## (f) what LINE violations look like -----------------------------------------
set.seed(7)
nn <- 120
viol <- bind_rows(
  tibble(panel = "Non-linear", xx = runif(nn, 0, 10),
         rr = (xx - 5)^2 + rnorm(nn, 0, 3), ff = xx),
  tibble(panel = "Unequal variance", xx = runif(nn, 0, 10),
         rr = rnorm(nn, 0, 0.4 * xx), ff = xx)
) |>
  group_by(panel) |> mutate(fitv = predict(lm(rr ~ ff)), res = rr - fitv) |> ungroup()
p_viol <- ggplot(viol, aes(fitv, res)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 1.3, alpha = 0.6, colour = "grey30") +
  facet_wrap(~panel, scales = "free") +
  labs(x = "Fitted value", y = "Residual") +
  theme_minimal(base_size = 25)
ggsave("figures/violations_grid.png", p_viol, width = 7.4, height = 3.2, dpi = 200)


## (h) the two roads DISAGREE: model CI vs bootstrap CI --------------------------
ci_df <- tibble(
  method = c("Model (t)\nassumes equal variance", "Bootstrap\nresamples the pairs"),
  lo = c(ci_raw_t[1], ci_raw_boot[1]),
  hi = c(ci_raw_t[2], ci_raw_boot[2]),
  y  = c(2, 1))
p_ci <- ggplot(ci_df) +
  geom_segment(aes(x = lo, xend = hi, y = y, yend = y), linewidth = 2, colour = "grey55") +
  geom_point(aes(x = coef(lm_raw)[2], y = y), size = 2.6) +
  geom_text(aes(x = (lo+hi)/2, y = y + 0.13, label = method), size = 6.2,
            lineheight = 0.9, vjust = 0) +
  geom_text(aes(x = lo, y = y, label = sprintf("%.0f", lo)), size = 6.2, hjust = 1.35) +
  geom_text(aes(x = hi, y = y, label = sprintf("%.0f", hi)), size = 6.2, hjust = -0.35) +
  coord_cartesian(ylim = c(0.5, 3.0), xlim = c(30, 108)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "slope: extra loan $ per $1000 of income", y = NULL) +
  theme_minimal(base_size = 27)
ggsave("figures/loans_ci.png", p_ci, width = 6.8, height = 3.0, dpi = 200)

## (i) the funnel, and the transform that flattens it ---------------------------
raw_sc <- tibble(xx = lx,        yy = ly,        panel = "Raw")
log_sc <- tibble(xx = log(lx),   yy = log(ly),   panel = "Log-log")
p_fix <- bind_rows(raw_sc, log_sc) |>
  mutate(panel = factor(panel, levels = c("Raw", "Log-log"))) |>
  ggplot(aes(xx, yy)) +
  geom_point(size = 1.1, alpha = 0.4, colour = "grey30") +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.9) +
  facet_wrap(~panel, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 25)
ggsave("figures/loans_resid_fix.png", p_fix, width = 7.4, height = 3.2, dpi = 200)

cat("\nAll figures written to figures/\n")
