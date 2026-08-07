# Lecture 4: Using Regression Models -- figure generation
# Generates every figure the deck uses, into figures/.
# Run from this directory:  Rscript Lecture4_demo.R
# Example: state-level poverty vs. HS graduation rate (openintro::poverty).

library(tidyverse)
library(openintro)

theme_set(
  theme_classic(base_size = 18) +
    theme(
      plot.title  = element_text(face = "bold"),
      axis.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

dir.create("figures", showWarnings = FALSE)
dir.create("data",    showWarnings = FALSE)

# State-level poverty data (50 states + DC): the canonical OpenIntro example.
pov <- read.table("data/poverty.txt", header = TRUE, sep = "\t") |>
  transmute(state = State, hs_grad = Graduates, poverty = Poverty)

# ---- Models (printed so the slides can quote the exact coefficients) ----
fit <- lm(poverty ~ hs_grad, data = pov)
cat("=== poverty ~ hs_grad ===\n")
print(round(coef(fit), 3))
cat("r  =", round(cor(pov$hs_grad, pov$poverty), 3),
    "   R^2 =", round(summary(fit)$r.squared, 3), "\n")
cat("hs_grad range:", round(range(pov$hs_grad), 1), "\n\n")

# South indicator from base-R Census regions (DC counted as South, per Census).
pov <- pov |>
  mutate(region = as.character(state.region[match(state, state.name)]),
         region = ifelse(state == "District of Columbia", "South", region),
         south  = ifelse(region == "South", 1L, 0L),
         south_lab = factor(ifelse(south == 1, "South", "Not South"),
                            levels = c("Not South", "South")))
fits <- lm(poverty ~ south, data = pov)
cat("=== poverty ~ south ===\n")
print(round(coef(fits), 3))
grp <- pov |> group_by(south_lab) |> summarise(mean_pov = round(mean(poverty), 2), n = n())
print(grp)

b0 <- coef(fit)[1]; b1 <- coef(fit)[2]

# ---- 1. Scatter ---------------------------------------------------------
p_scatter <- ggplot(pov, aes(hs_grad, poverty)) +
  geom_point(size = 2, alpha = .5) +
  labs(x = "% HS graduate", y = "% in poverty")
ggsave("figures/poverty-vs-hsgrad.png", plot = p_scatter, width = 6, height = 5, dpi = 150)

# ---- 2. Scatter + fitted line ------------------------------------------
eqn <- sprintf("hat(poverty) == %.2f - %.2f * (HS~grad)", b0, abs(b1))
p_fit <- ggplot(pov, aes(hs_grad, poverty)) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
           label = eqn, parse = TRUE, size = 5, color = "#2166AC") +
  labs(x = "% HS graduate", y = "% in poverty")
ggsave("figures/poverty-vs-hsgrad-line.png", plot = p_fit, width = 6.5, height = 5, dpi = 150)

# ---- 3. Correlation gallery (what different r look like) ----------------
set.seed(214)
n <- 90
mk <- function(x, y, panel) tibble(x = scale(x)[,1], y = scale(y)[,1], panel = panel)
xa <- runif(n, -3, 3)
xb <- rnorm(n); xd <- rnorm(n)
gallery <- bind_rows(
  mk(xa, xa^2 + rnorm(n, 0, 1.2),        "(a)"),  # curved: strong pattern, r ~ 0
  mk(xb, xb + rnorm(n, 0, 0.45),         "(b)"),  # strong positive
  mk(rnorm(n), rnorm(n),                 "(c)"),  # no relationship
  mk(xd, -xd + rnorm(n, 0, 0.45),        "(d)")   # strong negative
)
p_gallery <- ggplot(gallery, aes(x, y)) +
  geom_point(size = 1.7, alpha = .5) +
  facet_wrap(~panel, scales = "free") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), strip.text = element_text(face = "bold", size = 16))
ggsave("figures/correlation-gallery.png", plot = p_gallery, width = 6.5, height = 5.5, dpi = 150)

# ---- 3b. R^2 gallery (what different R^2 look like) ---------------------
set.seed(2141)
mk_r2 <- function(target, lab) {
  x <- rnorm(80)
  e <- residuals(lm(rnorm(80) ~ x))            # noise orthogonal to x
  y <- x + e * sqrt((1 - target) / target)     # signal:noise set for target R^2
  tibble(x = x, y = y, panel = lab)
}
r2gal <- bind_rows(
  mk_r2(0.20, "R^2 == 0.2"),
  mk_r2(0.55, "R^2 == 0.55"),
  mk_r2(0.90, "R^2 == 0.9")
)
p_r2 <- ggplot(r2gal, aes(x, y)) +
  geom_point(size = 1.6, alpha = .55, color = "#569BBD") +
  geom_smooth(method = "lm", se = FALSE, color = "#F05133", linewidth = 1) +
  facet_wrap(~panel, nrow = 1, scales = "free", labeller = label_parsed) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), strip.text = element_text(size = 16))
ggsave("figures/r2-gallery.png", plot = p_r2, width = 8.4, height = 3.2, dpi = 150)

# ---- 3c. Anscombe's quartet: four data sets, one R^2 -------------------
# Built into base R (datasets::anscombe). All four share r = 0.816, R^2 = 0.67,
# and the same fitted line y-hat = 3 + 0.5 x.
ans <- bind_rows(lapply(1:4, function(i)
  tibble(set = paste("Data set", i),
         x = anscombe[[paste0("x", i)]],
         y = anscombe[[paste0("y", i)]])))
p_ans <- ggplot(ans, aes(x, y)) +
  geom_abline(intercept = 3, slope = 0.5, color = "#F05133", linewidth = 0.9) +
  geom_point(size = 2.3, color = "#569BBD") +
  facet_wrap(~set, nrow = 1) +
  coord_cartesian(xlim = c(3, 19), ylim = c(2, 13)) +
  labs(x = "x", y = "y") +
  theme(strip.text = element_text(face = "bold", size = 15))
ggsave("figures/anscombe.png", plot = p_ans, width = 9.2, height = 2.9, dpi = 150)

# ---- 4. Interpolation vs. extrapolation ---------------------------------
xlo <- min(pov$hs_grad); xhi <- max(pov$hs_grad)
pred <- function(x) b0 + b1 * x
x_in  <- 85;  x_out <- 65
p_extrap <- ggplot(pov, aes(hs_grad, poverty)) +
  annotate("rect", xmin = xlo, xmax = xhi, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = .25) +
  annotate("text", x = (xlo + xhi)/2, y = Inf, vjust = 1.4,
           label = "observed range", fontface = "italic", size = 4.5) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  expand_limits(x = c(62, 95)) +
  annotate("point", x = x_in,  y = pred(x_in),  color = "#1B9E77", size = 4) +
  annotate("point", x = x_out, y = pred(x_out), color = "#D95F02", size = 4) +
  annotate("text", x = x_in,  y = pred(x_in)  - 1.2, label = "interpolation",
           color = "#1B9E77", fontface = "bold", size = 4.3) +
  annotate("text", x = x_out, y = pred(x_out) + 1.2, label = "extrapolation",
           color = "#D95F02", fontface = "bold", size = 4.3) +
  labs(x = "% HS graduate", y = "% in poverty")
ggsave("figures/extrapolation.png", plot = p_extrap, width = 6.8, height = 5, dpi = 150)

# ---- 5. Categorical predictor: poverty by region ------------------------
b0s <- coef(fits)[1]; b1s <- coef(fits)[2]
p_region <- ggplot(pov, aes(south_lab, poverty)) +
  geom_jitter(width = .12, height = 0, size = 2, alpha = .45) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#D95F02") +
  stat_summary(fun = mean, geom = "text", aes(label = sprintf("%.2f", after_stat(y))),
               color = "#D95F02", fontface = "bold", vjust = -1.1, size = 5) +
  labs(x = NULL, y = "% in poverty",
       subtitle = sprintf("intercept = %.2f   difference = %+.2f", b0s, b1s))
ggsave("figures/poverty-by-region.png", plot = p_region, width = 6, height = 5, dpi = 150)

cat("\nDone. Figures written to figures/.\n")
