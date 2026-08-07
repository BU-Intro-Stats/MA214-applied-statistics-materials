# MA214 Lecture 11 -- Testing for independence
# In-class demo + figure generation.
#
# Data:
#   data/popular.csv       478 schoolchildren: grade (4th-6th) x what matters most
#                          (grades / being popular / sports).  Chase & Dummer (1992).
#   data/portal_plots.csv  Portal captures of three rodent species, by plot type.
#                          Some plots are fenced to keep kangaroo rats out -- so this
#                          is a designed experiment, not an observational table.
#
# Run from this directory:  Rscript Lecture11_demo.R

library(tidyverse)

set.seed(214)

# ==============================================================================
# 1. The two-way table
# ==============================================================================
pop <- read_csv("data/popular.csv", show_col_types = FALSE)

tab <- xtabs(count ~ grade + goal, data = pop)
cat("\n===== Observed counts =====\n")
print(addmargins(tab))

# ==============================================================================
# 2. What would independence predict?  ->  expected counts
# ==============================================================================
# expected_ij = (row total_i) * (col total_j) / n
n   <- sum(tab)
exp_counts <- outer(rowSums(tab), colSums(tab)) / n
names(dimnames(exp_counts)) <- c("grade", "goal")

cat("\n===== Expected counts if grade and goal were independent =====\n")
print(round(exp_counts, 1))
cat("\nSmallest expected count:", round(min(exp_counts), 1),
    " (the condition asks for at least 5)\n")

# worked cell, for the slide: 4th graders who chose Grades
cat(sprintf("\nOne cell by hand: 4th x Grades = %d x %d / %d = %.1f  (observed %d)\n",
            sum(tab["4th", ]), sum(tab[, "Grades"]), n,
            exp_counts["4th", "Grades"], tab["4th", "Grades"]))

# ==============================================================================
# 3. One number for the whole table: the chi-square statistic
# ==============================================================================
x2_obs <- sum((tab - exp_counts)^2 / exp_counts)
df     <- (nrow(tab) - 1) * (ncol(tab) - 1)
cat(sprintf("\n===== X^2 = sum (O-E)^2/E = %.3f    df = (%d-1)(%d-1) = %d =====\n",
            x2_obs, nrow(tab), ncol(tab), df))

# ==============================================================================
# 4. TWO ROADS to the null distribution of X^2
# ==============================================================================
# Road A -- shuffle. If the two variables are independent, a child's goal could
# equally well have belonged to any other child: the labels are exchangeable.
long  <- pop |> uncount(count)                       # back to 478 rows
B     <- 10000

x2_of <- function(g1, g2) {
  o <- table(g1, g2)
  e <- outer(rowSums(o), colSums(o)) / sum(o)
  sum((o - e)^2 / e)
}

perm_null <- replicate(B, x2_of(long$grade, sample(long$goal)))

p_shuffle <- mean(perm_null >= x2_obs)

# Road B -- the mathematical model: X^2 ~ chi-square with df degrees of freedom
p_model <- pchisq(x2_obs, df = df, lower.tail = FALSE)

cat("\n===== Two roads =====\n")
cat(sprintf("randomization (B = %d shuffles) : p = %.4f\n", B, p_shuffle))
cat(sprintf("chi-square model (df = %d)      : p = %.4f\n", df, p_model))
cat(sprintf("mean of the shuffled null       : %.2f   (theory says df = %d)\n",
            mean(perm_null), df))
cat("R's own function agrees:\n")
print(chisq.test(tab))

# ==============================================================================
# 5. The Portal exclosure experiment: what a real effect looks like
# ==============================================================================
portal <- read_csv("data/portal_plots.csv", show_col_types = FALSE)
ptab   <- xtabs(n ~ species_id + plot_type, data = portal)

cat("\n===== Portal: species x plot type =====\n")
print(ptab)
pt <- chisq.test(ptab)
cat(sprintf("\nX^2 = %.0f   df = %d   p = %s\n", pt$statistic, pt$parameter,
            format.pval(pt$p.value)))
cat("Smallest expected count:", round(min(pt$expected), 1), "\n")
cat("\nWhere the table breaks: (O-E)/sqrt(E), the standardized residuals\n")
print(round(pt$residuals, 1))

# ==============================================================================
# Figures
# ==============================================================================

## (a) THE punchline: the chi-square curve on top of the shuffled null ---------
p_punch <- ggplot(tibble(x2 = perm_null), aes(x = x2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "grey80", colour = "white") +
  stat_function(fun = dchisq, args = list(df = df), linewidth = 1.1) +
  geom_vline(xintercept = x2_obs, linetype = 2) +
  annotate("text", x = x2_obs + 0.6, y = 0.205,
           label = sprintf("observed X2 = %.2f", x2_obs), hjust = 0, size = 6.2) +
  # Name the curve: the whole point of the slide is that this smooth reference
  # distribution lands on top of the shuffled histogram.
  annotate("text", x = 8.5, y = dchisq(6, df) + 0.030,
           label = sprintf("chi-square, df = %d", df), hjust = 0, size = 6.2) +
  coord_cartesian(xlim = c(0, 20)) +
  scale_y_continuous(breaks = NULL) +
  labs(x = expression(X^2), y = NULL) +
  theme_minimal(base_size = 23)

ggsave("figures/chisq_vs_shuffle.png", p_punch, width = 6.8, height = 3.4, dpi = 200)

## (b) observed vs expected, side by side --------------------------------------
comp <- bind_rows(
  as_tibble(as.table(tab)) |> rename(count = n) |> mutate(what = "Observed"),
  as_tibble(as.table(exp_counts)) |> rename(count = n) |>
    mutate(what = "Expected if independent")
) |>
  mutate(what = factor(what, levels = c("Observed", "Expected if independent")))

p_oe <- ggplot(comp, aes(x = goal, y = count, fill = what)) +
  geom_col(position = "dodge", colour = "white") +
  facet_wrap(~grade) +
  scale_fill_grey(start = 0.35, end = 0.75) +
  labs(x = NULL, y = "Number of children", fill = NULL) +
  theme_minimal(base_size = 21) +
  theme(legend.position = "bottom")

ggsave("figures/popular_obs_vs_exp.png", p_oe, width = 7.2, height = 3.2, dpi = 200)

## (c) the Portal exclosures: what a real effect looks like --------------------
plot_type_short <- c(
  "Control"                   = "Control",
  "Long-term Krat Exclosure"  = "Long-term\nKrat excl.",
  "Rodent Exclosure"          = "Rodent\nexcl.",
  "Short-term Krat Exclosure" = "Short-term\nKrat excl.",
  "Spectab exclosure"         = "Spectab\nexcl."
)

p_portal <- as_tibble(as.table(pt$residuals)) |>
  rename(resid = n) |>
  mutate(plot_type = factor(plot_type_short[plot_type],
                            levels = unname(plot_type_short))) |>
  ggplot(aes(x = plot_type, y = species_id, fill = resid)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(resid, 0)), size = 7.2) +
  scale_fill_gradient2(low = "#0571b0", mid = "grey95", high = "#e66101", midpoint = 0) +
  labs(x = NULL, y = NULL, fill = "(O-E)/sqrt(E)") +
  # Shown at 0.92\textwidth (~4.0in) on a ~4.33in text block, so this is saved
  # 7.2in wide and shrunk ~1.8x: base_size 21 lands near slide body size.
  # Enlarging the font instead of the slide display makes the five category
  # labels collide, so the figure is shown wider on the slide rather than set
  # in a bigger typeface here.
  theme_minimal(base_size = 21) +
  theme(legend.position = "none")

ggsave("figures/portal_residuals.png", p_portal, width = 7.2, height = 3.0, dpi = 200)

cat("\nFigures written to figures/\n")

## (d) what one shuffle actually does -----------------------------------------
# (folded in from the old chisq_perm_figures.R)
set.seed(11)
small <- long |> slice_sample(n = 24) |> mutate(id = row_number())
one_shuffle <- bind_rows(
  small |> transmute(grade, id, goal, version = "Original"),
  small |> transmute(grade, id, goal = sample(goal), version = "Shuffled")
)

p_step <- ggplot(one_shuffle, aes(x = grade, y = id, colour = goal)) +
  geom_point(size = 2.6, alpha = 0.9) +
  facet_wrap(~version) +
  labs(x = "Grade (fixed)", y = "Student (index)", colour = "Goal") +
  theme_minimal(base_size = 21)

ggsave("figures/popular_perm_step.pdf", p_step, width = 7.2, height = 4.0)

# ==============================================================================
# 6. Why the condition is on the EXPECTED counts
#    (folded in from the old activity script chisq_simulation.R)
#
#    The slide claims: when expected counts are small, the chi-square curve is a
#    poor fit and its p-value is wrong -- while shuffling stays valid whatever the
#    counts. Here is the evidence. We generate tables in which the two variables
#    really ARE independent, and count how often each method wrongly rejects.
#    A test working correctly rejects 5% of the time.
# ==============================================================================
alpha <- 0.05
R     <- 1000       # datasets per setting
B_sim <- 999        # shuffles per dataset

# Balanced margins keep the expected counts healthy; sparse margins starve them.
margins <- list(
  balanced = list(row = c(1, 1, 1) / 3,        col = c(1, 1, 1) / 3),
  sparse   = list(row = c(.15, .70, .15),      col = c(.70, .25, .05))
)

sim_independent_table <- function(n, m) {
  tibble(a = sample(1:3, n, replace = TRUE, prob = m$row),
         b = sample(1:3, n, replace = TRUE, prob = m$col))
}

type1 <- expand_grid(n = c(20, 30, 80, 300), shape = names(margins)) |>
  rowwise() |>
  mutate(out = list({
    m <- margins[[shape]]
    pv <- replicate(R, {
      d <- sim_independent_table(n, m)
      tb <- table(d$a, d$b)
      c(chisq = suppressWarnings(chisq.test(tb, correct = FALSE)$p.value),
        shuffle = suppressWarnings(
          chisq.test(tb, simulate.p.value = TRUE, B = B_sim)$p.value))
    })
    tibble(chisq_model = mean(pv["chisq", ] < alpha, na.rm = TRUE),
           shuffling   = mean(pv["shuffle", ] < alpha, na.rm = TRUE))
  })) |>
  unnest(out) |>
  ungroup()

cat("\n===== How often does each method reject when H0 is TRUE? (should be 0.05) =====\n")
print(type1)

# Anything inside this band is indistinguishable from 5% given only R datasets.
mc_se <- sqrt(alpha * (1 - alpha) / R)

p_cond <- type1 |>
  pivot_longer(c(chisq_model, shuffling), names_to = "method", values_to = "rate") |>
  mutate(method = recode(method, chisq_model = "chi-square model",
                         shuffling = "shuffling")) |>
  ggplot(aes(x = n, y = rate, linetype = method, shape = method)) +
  annotate("rect", xmin = 18, xmax = 340, ymin = alpha - 2 * mc_se,
           ymax = alpha + 2 * mc_se, fill = "grey85") +
  geom_hline(yintercept = alpha, linewidth = 0.4) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~shape, labeller = as_labeller(c(
    balanced = "balanced margins (expected counts healthy)",
    sparse   = "sparse margins (expected counts tiny)"))) +
  scale_x_log10(breaks = c(20, 30, 80, 300)) +
  expand_limits(y = 0) +
  labs(x = "Sample size n (log scale)", y = "Rejection rate when H0 is true",
       linetype = NULL, shape = NULL) +
  theme_minimal(base_size = 21) +
  theme(legend.position = "bottom")

ggsave("figures/chisq_condition_check.png", p_cond, width = 7.6, height = 3.4, dpi = 200)

cat("\nAll figures written to figures/\n")
