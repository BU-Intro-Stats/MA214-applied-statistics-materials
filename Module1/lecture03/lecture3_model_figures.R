# Simulated small-sample example for the model-decomposition and
# population-vs-fitted figures. Small n so the fitted line visibly
# differs from the population line.
library(tidyverse)
set.seed(42)
n <- 12
b0 <- 3; b1 <- 0.8; sigma <- 2.8
dat <- tibble(x = runif(n, 0, 10), y = b0 + b1 * x + rnorm(n, sd = sigma))
fit <- lm(y ~ x, data = dat)
cat(sprintf("fitted: intercept=%.2f slope=%.2f (true: %.1f, %.1f)\n",
            coef(fit)[1], coef(fit)[2], b0, b1))

base <- ggplot(dat, aes(x, y)) +
  geom_point(color = "#569BBD", size = 2.4) +
  theme_minimal(base_size = 18) + labs(x = "x", y = "y")

# decomposition: mean line + one highlighted error
i <- which.max(abs(resid(fit)))
p1 <- base +
  geom_abline(intercept = b0, slope = b1, linewidth = 0.9, color = "#2E3742") +
  annotate("segment", x = dat$x[i], xend = dat$x[i],
           y = b0 + b1 * dat$x[i], yend = dat$y[i],
           linetype = "dashed", color = "#F05133", linewidth = 0.8) +
  annotate("text", x = dat$x[i] + 0.35, y = (dat$y[i] + b0 + b1 * dat$x[i]) / 2,
           label = "random~error~epsilon[i]", parse = TRUE, hjust = 0, color = "#F05133", size = 5.8) +
  annotate("text", x = 1.1, y = b0 + b1 * 1.1 + 2.6,
           label = "mean~pattern~beta[0] + beta[1] * x", parse = TRUE, hjust = 0, size = 5.8, color = "#2E3742")
ggsave("figures/lecture3_model_decomposition.png", p1, width = 7.6, height = 4.4, dpi = 150)

# population vs fitted
p2 <- base +
  geom_abline(intercept = b0, slope = b1, linewidth = 0.9, color = "#2E3742", linetype = "dashed") +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], linewidth = 0.9, color = "#569BBD") +
  annotate("text", x = 3.9, y = b0 + b1 * 3.9 - 1.3, label = "population line (unknown)",
           hjust = 0, size = 5.8, color = "#2E3742") +
  annotate("text", x = 6.8, y = coef(fit)[1] + coef(fit)[2] * 10.4, label = "fitted line",
           hjust = 0, size = 5.8, color = "#569BBD")
ggsave("figures/lecture3_population_vs_fitted.png", p2, width = 7.6, height = 4.4, dpi = 150)
cat("wrote both figures\n")
