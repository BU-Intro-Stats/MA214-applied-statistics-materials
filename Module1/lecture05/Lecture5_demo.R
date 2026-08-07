# Lecture 5: Multiple Regression -- in-class demo + figure generation
# Reproduces every model quoted on the slides, and builds figures/r2-vs-adjr2.png.
# Run from this directory:  Rscript Lecture5_demo.R

library(tidyverse)
library(openintro)

theme_set(
  theme_classic(base_size = 27) +
    theme(
      plot.title  = element_text(face = "bold"),
      axis.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

dir.create("figures", showWarnings = FALSE)

# ---- 1. Categorical predictors: the loans data ---------------------------
loans <- openintro::loans_full_schema

# (a) A two-level indicator: past bankruptcy.  Slides: rate = 12.33 + 0.74 * bankruptcy
loans <- loans |> mutate(bankruptcy = ifelse(public_record_bankrupt >= 1, 1, 0))
cat("=== interest_rate ~ bankruptcy ===\n")
print(round(coef(lm(interest_rate ~ bankruptcy, data = loans)), 4))

# (b) A three-level category -> k-1 = 2 indicators, reference = "Not Verified".
#     Slides: intercept 11.0995, source_only +1.4160, verified +3.2543
cat("\n=== interest_rate ~ verified_income (3 levels) ===\n")
m_ver <- lm(interest_rate ~ verified_income, data = loans)
print(round(coef(summary(m_ver)), 4))
cat("reference level (omitted):", levels(factor(loans$verified_income))[1], "\n")

# ---- 2. Parallel slopes: weights of books --------------------------------
# The book data (allbacks) ships in the DAAG package.  Slides quote:
#   volume only     : weight = 107.68 + 0.71 volume,  R2 = .803, adj = .788
#   volume + cover  : weight = 197.96 + 0.72 volume - 184.05 cover:pb
#                     R2 = .928, adj = .915
if (requireNamespace("DAAG", quietly = TRUE)) {
  data(allbacks, package = "DAAG")
  m_vol   <- lm(weight ~ volume, data = allbacks)
  m_cover <- lm(weight ~ volume + cover, data = allbacks)
  cat("\n=== weight ~ volume ===\n");         print(round(coef(m_vol), 2))
  cat("R2 =", round(summary(m_vol)$r.squared, 3),
      " adjR2 =", round(summary(m_vol)$adj.r.squared, 3), "\n")
  cat("\n=== weight ~ volume + cover ===\n"); print(round(coef(m_cover), 2))
  cat("R2 =", round(summary(m_cover)$r.squared, 3),
      " adjR2 =", round(summary(m_cover)$adj.r.squared, 3), "\n")
} else {
  cat("\n[DAAG not installed -- skipping the books models.",
      "install.packages('DAAG') to reproduce them.]\n")
}

# ---- 3. R^2 and adjusted R^2: the state poverty data ---------------------
# Same 51-state data as Lecture 4.
pov <- read.table("data/poverty.txt", header = TRUE, sep = "\t") |>
  rename(poverty = Poverty, white = White,
         female_house = PercentFemaleHouseholderNoHusbandPresent)

m1 <- lm(poverty ~ female_house, data = pov)
m2 <- lm(poverty ~ female_house + white, data = pov)

cat("\n=== poverty ~ female_house ===\n")
print(anova(m1))
cat("R2 =", round(summary(m1)$r.squared, 4),
    " adjR2 =", round(summary(m1)$adj.r.squared, 4), "\n")

cat("\n=== poverty ~ female_house + white ===\n")
print(anova(m2))
cat("R2 =", round(summary(m2)$r.squared, 4),
    " adjR2 =", round(summary(m2)$adj.r.squared, 4), "\n")
cat("--> R^2 rises 0.28 -> 0.29, but adjusted R^2 stays at 0.26.\n")

# ---- 4. Figure: what happens when we keep adding junk predictors? --------
# Keep the two real predictors, then bolt on pure-noise ones. R^2 can only
# climb; adjusted R^2 is *not expected to*. A single random draw is noisy
# (junk can correlate with y by luck), so we average the EXPECTED behaviour
# over many draws -- that is exactly what "unbiased" means here.
set.seed(214)
n_sim  <- 300
n_junk <- 8
base   <- pov |> select(poverty, female_house, white)

one_sim <- function(i) {
  junk <- as.data.frame(matrix(rnorm(nrow(base) * n_junk), nrow = nrow(base)))
  names(junk) <- paste0("junk", seq_len(n_junk))
  d <- bind_cols(base, junk)
  preds <- c("female_house", "white", names(junk))
  map_dfr(seq_along(preds), function(k) {
    s <- summary(lm(reformulate(preds[1:k], response = "poverty"), data = d))
    tibble(p = k, r2 = s$r.squared, adj = s$adj.r.squared)
  })
}

fits <- map_dfr(seq_len(n_sim), one_sim) |>
  group_by(p) |>
  summarise(`R²` = mean(r2), `adjusted R²` = mean(adj), .groups = "drop")

print(fits)

p_r2 <- ggplot(pivot_longer(fits, -p, names_to = "measure", values_to = "value"),
       aes(p, value, colour = measure)) +
  annotate("rect", xmin = 2.5, xmax = 10.5, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = .22) +
  annotate("text", x = 6.5, y = 0.20, label = "pure-noise predictors",
           fontface = "italic", size = 6.7) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("R²" = "#D95F02", "adjusted R²" = "#1B9E77")) +
  scale_x_continuous(breaks = 1:10) +
  coord_cartesian(ylim = c(0.18, 0.42)) +
  labs(x = "number of predictors (p)", y = NULL, colour = NULL) +
  theme(legend.position = "top")

ggsave("figures/r2-vs-adjr2.png", plot = p_r2, width = 7, height = 4.6, dpi = 150)
cat("\nWrote figures/r2-vs-adjr2.png (averaged over", n_sim, "draws)\n")
