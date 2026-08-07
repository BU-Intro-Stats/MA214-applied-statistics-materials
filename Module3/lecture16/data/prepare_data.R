# MA214 Lecture 16 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Freezes the two datasets the lecture and its worksheet use, so the deck is
# reproducible from CSVs alone.
#
#   species_survival.csv  200 species, SIMULATED. Whether each survived a 20-year
#                         window is drawn from a known logistic model, so the
#                         "truth" is known and the signal is clean -- the right
#                         choice for a first logistic-inference example. The data
#                         is regenerated below from set.seed(2140); the committed
#                         CSV matches it exactly.
#
#   heart_disease.csv     303 -> 208 patients, the UCI Cleveland heart-disease
#                         study (Detrano et al., 1989; the classic teaching set).
#                         Age, sex, cholesterol, resting BP, max heart rate, ...
#                         and a binary heart_disease outcome. Used by the
#                         in-class activity and the worksheet. Real data; this
#                         script does not regenerate it, only reports on it.
#
# Both CSVs are committed, so this script does not re-download or overwrite by
# default. To rebuild species_survival.csv:  Rscript data/prepare_data.R --refresh

library(tidyverse)
library(broom)

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

# ---- Species survival: the simulated logistic example ----------------------
# The exact data-generating process. logit(p) is linear in the four predictors,
# so the model is correctly specified and the coefficients recover the DGP.
set.seed(2140)
n <- 200
species <- tibble(
  species_id        = 1:n,
  habitat_area      = runif(n, 1, 500),
  population_size   = round(rlnorm(n, 6, 1.5)),
  genetic_diversity = runif(n, 0.1, 0.9),
  human_impact      = runif(n, 0, 10)
)
eta <- -2 + 0.008 * species$habitat_area + 0.0003 * species$population_size +
  3 * species$genetic_diversity - 0.4 * species$human_impact
species$survived <- rbinom(n, 1, plogis(eta))

if (refresh || !file.exists("data/species_survival.csv")) {
  write_csv(species, "data/species_survival.csv")
  cat("Wrote data/species_survival.csv\n")
}

# Sanity check: the committed CSV must match what set.seed(2140) produces.
frozen <- read_csv("data/species_survival.csv", show_col_types = FALSE)
stopifnot(all.equal(frozen$survived, species$survived))

cat("\n=== species_survival.csv ===\n")
cat(sprintf("n = %d;  survived = %d, extinct = %d  (survival rate %.2f)\n",
            n, sum(species$survived), sum(species$survived == 0), mean(species$survived)))

# The model the whole deck reads. THESE are the numbers the slides must show.
model <- glm(survived ~ habitat_area + population_size +
               genetic_diversity + human_impact, family = binomial, data = species)
cat("\ntidy(model) -- the coefficient table on the 'Software Output' slide:\n")
print(as.data.frame(tidy(model) |> mutate(across(where(is.numeric), ~ signif(., 3)))))

cat("\ntidy(model, conf.int = TRUE, exponentiate = TRUE) -- the odds-ratio CIs:\n")
print(as.data.frame(tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
                      select(term, estimate, conf.low, conf.high) |>
                      mutate(across(where(is.numeric), ~ signif(., 4)))))

# The habitat_area interpretation lines on the 'CI for Odds Ratio' slide.
co <- coef(summary(model))["habitat_area", ]
ci <- confint.default(model)["habitat_area", ]
cat(sprintf("\nhabitat_area:  OR per km^2 = %.4f  95%% CI (%.4f, %.4f)\n",
            exp(co["Estimate"]), exp(ci[1]), exp(ci[2])))
cat(sprintf("  a 100 km^2 increase multiplies the odds by %.2f  95%% CI (%.2f, %.2f)\n",
            exp(co["Estimate"] * 100), exp(ci[1] * 100), exp(ci[2] * 100)))

# ---- Heart disease: the activity data --------------------------------------
heart <- read_csv("data/heart_disease.csv", show_col_types = FALSE)
cat("\n=== heart_disease.csv ===\n")
cat(sprintf("n = %d patients;  %d columns;  %d with heart disease\n",
            nrow(heart), ncol(heart), sum(heart$heart_disease)))
cat("columns:", paste(names(heart), collapse = ", "), "\n")
hm <- glm(heart_disease ~ age + resting_bp + cholesterol + max_heart_rate,
          family = binomial, data = heart)
cat("activity's model, heart_disease ~ age + resting_bp + cholesterol + max_heart_rate:\n")
print(as.data.frame(tidy(hm, conf.int = TRUE, exponentiate = TRUE) |>
                      select(term, estimate, conf.low, conf.high) |>
                      mutate(across(where(is.numeric), ~ signif(., 3)))))
