# MA214 Lecture 16 -- Logistic regression inference
# In-class demo + figure generation.
#
# Data (frozen by data/prepare_data.R):
#   data/species_survival.csv   200 species, simulated from a known logistic
#                               model (set.seed 2140). We carry out inference on
#                               the coefficients, build odds-ratio CIs, check
#                               diagnostics, and compare models by cross-validation.
#   data/heart_disease.csv      the in-class activity (UCI Cleveland).
#
# Every number that appears on a slide is printed below.
# Run from this directory:  Rscript Lecture16_demo.R

library(tidyverse)
library(broom)

set.seed(2140)
dir.create("figures", showWarnings = FALSE)
pdf(NULL)                      # keep a stray Rplots.pdf from appearing

blue <- "#569BBD"
red  <- "#F05133"

species <- read_csv("data/species_survival.csv", show_col_types = FALSE)
n <- nrow(species)

model <- glm(survived ~ habitat_area + population_size +
               genetic_diversity + human_impact, family = binomial, data = species)


# ==============================================================================
cat("\n==============================================================\n")
cat("1. The fitted model -- the coefficient table on the slides\n")
cat("==============================================================\n")
cat(sprintf("n = %d species;  survived = %d (%.0f%%)\n",
            n, sum(species$survived), 100 * mean(species$survived)))
cat("\ntidy(model):\n")
print(as.data.frame(tidy(model) |> mutate(across(where(is.numeric), ~ signif(., 3)))))


# ==============================================================================
cat("\n==============================================================\n")
cat("2. Odds-ratio confidence intervals\n")
cat("==============================================================\n")
cat("tidy(model, conf.int = TRUE, exponentiate = TRUE):\n")
print(as.data.frame(tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
                      select(term, estimate, conf.low, conf.high) |>
                      mutate(across(where(is.numeric), ~ signif(., 4)))))

b  <- coef(model)["habitat_area"]
se <- coef(summary(model))["habitat_area", "Std. Error"]
z  <- qnorm(0.975)
cat(sprintf("\nhabitat_area: b = %.4f, SE = %.4f\n", b, se))
cat(sprintf("  OR per km^2 = e^%.4f = %.4f   95%% CI (%.4f, %.4f)\n",
            b, exp(b), exp(b - z * se), exp(b + z * se)))
cat(sprintf("  a 100 km^2 increase multiplies the odds by %.2f   95%% CI (%.2f, %.2f)\n",
            exp(100 * b), exp(100 * (b - z * se)), exp(100 * (b + z * se))))


# ==============================================================================
cat("\n==============================================================\n")
cat("3. A confidence interval for a predicted probability\n")
cat("==============================================================\n")
# One example species, at the median of every predictor.
newsp <- species |>
  summarise(across(c(habitat_area, population_size, genetic_diversity, human_impact),
                   median))
pr <- predict(model, newsp, type = "link", se.fit = TRUE)      # log-odds scale
eta <- pr$fit; se_eta <- pr$se.fit
ci_link <- c(eta - z * se_eta, eta + z * se_eta)
ci_p    <- plogis(ci_link)
cat("For a species at the median of every predictor:\n")
cat(sprintf("  log-odds  eta-hat = %.3f,  SE = %.3f,  95%% CI (%.3f, %.3f)\n",
            eta, se_eta, ci_link[1], ci_link[2]))
cat(sprintf("  probability  p-hat = %.3f,  95%% CI (%.3f, %.3f)  <- inverse-logit of the endpoints\n",
            plogis(eta), ci_p[1], ci_p[2]))
cat("  the interval stays inside [0, 1] by construction.\n")


# ==============================================================================
cat("\n==============================================================\n")
cat("4. Brier score and log-loss -- the per-observation table (y = 1)\n")
cat("==============================================================\n")
phat <- c(0.99, 0.80, 0.50, 0.20, 0.01)
tab <- tibble(
  p_hat        = phat,
  brier_sq_err = (1 - phat)^2,
  log_loss     = -log(phat)
)
print(as.data.frame(tab |> mutate(across(where(is.numeric), ~ round(., 3)))))
cat("Both reward a confident correct call and punish a confident wrong one --\n")
cat("but log-loss punishes the confident mistake far harder (0.01 -> 4.61).\n")


# ==============================================================================
cat("\n==============================================================\n")
cat("5. Cross-validation for logistic regression -- three losses, two models\n")
cat("==============================================================\n")
# Backs the CV slide and the activity: a full model vs. a smaller one, scored by
# misclassification, Brier, and log-loss under repeated k-fold CV.
loss_fns <- list(
  misclass = function(y, p) mean((p >= 0.5) != y),
  brier    = function(y, p) mean((y - p)^2),
  logloss  = function(y, p) -mean(y * log(p) + (1 - y) * log(1 - p))
)
forms <- list(
  full    = survived ~ habitat_area + population_size + genetic_diversity + human_impact,
  small   = survived ~ habitat_area + genetic_diversity
)
k <- 5; reps <- 20
cv <- map_dfr(names(forms), function(nm) {
  per <- map(1:reps, function(r) {
    folds <- sample(rep(1:k, length.out = n))
    scores <- map(1:k, function(j) {
      tr <- species[folds != j, ]; te <- species[folds == j, ]
      m  <- glm(forms[[nm]], family = binomial, data = tr)
      p  <- predict(m, te, type = "response")
      p  <- pmin(pmax(p, 1e-6), 1 - 1e-6)     # clip for log-loss
      map_dbl(loss_fns, ~ .x(te$survived, p))
    })
    reduce(scores, `+`) / k
  })
  avg <- reduce(per, `+`) / reps
  tibble(model = nm, misclass = avg[["misclass"]],
         brier = avg[["brier"]], logloss = avg[["logloss"]])
})
cat(sprintf("%d-fold CV, %d repeats:\n", k, reps))
print(as.data.frame(cv |> mutate(across(where(is.numeric), ~ round(., 3)))))
cat("All three losses agree the fuller model predicts better here.\n")


# ==============================================================================
cat("\n==============================================================\n")
cat("6. Figures\n")
cat("==============================================================\n")

# --- Figure 1: survival curve ------------------------------------------------
ggsave("figures/survival_curve.pdf",
       ggplot(species, aes(x = habitat_area, y = survived)) +
         geom_jitter(height = 0.05, width = 0, alpha = 0.4, size = 2, color = "gray30") +
         geom_smooth(method = "glm", method.args = list(family = "binomial"),
                     se = TRUE, color = blue, fill = blue, linewidth = 1,
                     formula = y ~ x) +
         labs(x = expression("Habitat Area (km"^2*")"),
              y = "Probability of Survival",
              title = "Fitted logistic curve: survival vs. habitat area") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(face = "bold", size = 12)),
       width = 5.5, height = 4)

# --- Figure 2: log-odds linearity check --------------------------------------
logodds_data <- species |>
  mutate(bin = ntile(habitat_area, 10)) |>
  group_by(bin) |>
  summarise(mid = median(habitat_area), prop = mean(survived), .groups = "drop") |>
  filter(prop > 0, prop < 1) |>
  mutate(logodds = log(prop / (1 - prop)))
ggsave("figures/logodds_linearity.pdf",
       ggplot(logodds_data, aes(x = mid, y = logodds)) +
         geom_point(size = 3, color = blue) +
         geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1,
                     formula = y ~ x) +
         labs(x = expression("Habitat Area (km"^2*")"),
              y = "Empirical log-odds",
              title = "Checking linearity in the log-odds") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(face = "bold", size = 12)),
       width = 6, height = 4)

# --- Figure 3: predicted probabilities by outcome ----------------------------
species$pred_prob <- predict(model, type = "response")
ggsave("figures/predicted_probs.pdf",
       ggplot(species, aes(x = pred_prob, fill = factor(survived))) +
         geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
         scale_fill_manual(name = "Survived",
                           values = c("0" = red, "1" = blue),
                           labels = c("0" = "extinct", "1" = "survived")) +
         labs(x = "Predicted probability", y = "Count",
              title = "Predicted probabilities, by actual outcome") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(face = "bold", size = 12),
               legend.position = "top"),
       width = 5.5, height = 4)

# --- Figure 4: binned residual plot ------------------------------------------
species$resid       <- residuals(model, type = "response")
species$fitted_prob <- fitted(model)
overall_se <- sqrt(mean(species$fitted_prob * (1 - species$fitted_prob)) / n)
binned <- species |>
  mutate(bin = ntile(fitted_prob, 10)) |>
  group_by(bin) |>
  summarise(avg_fitted = mean(fitted_prob), avg_resid = mean(resid), .groups = "drop")
ggsave("figures/binned_residuals.pdf",
       ggplot(binned, aes(x = avg_fitted, y = avg_resid)) +
         geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
         geom_hline(yintercept =  2 * overall_se, linetype = "dashed", color = "gray50") +
         geom_hline(yintercept = -2 * overall_se, linetype = "dashed", color = "gray50") +
         annotate("text", x = Inf, y = 2 * overall_se, hjust = 1.03, vjust = -0.5,
                  size = 5, color = "gray30", label = "±2 SE") +
         geom_point(size = 2.5, color = blue) +
         ylim(-0.15, 0.15) +
         labs(x = "Average fitted probability", y = "Average residual",
              title = "Binned residual plot") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(face = "bold", size = 12)),
       width = 5.5, height = 4)

cat("All figures written to figures/\n")
