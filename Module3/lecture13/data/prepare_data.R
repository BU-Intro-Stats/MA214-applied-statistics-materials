# MA214 Lecture 13 -- data preparation
# Run ONCE from the lecture directory:  Rscript data/prepare_data.R
#
# Freezes the two datasets the lecture and its worksheet use, so the deck is
# reproducible from CSVs alone (no palmerpenguins dependency at build time).
#
#   penguins.csv       A fixed random sample of 40 Palmer penguins (Horst,
#                      Hill & Gorman 2020, palmerpenguins). We regress body mass
#                      on flipper length. A SAMPLE, not the whole census -- that
#                      is the point: is the slope we see real, or sampling noise?
#
#   kangaroo_rats.csv  The 348 Dipodomys merriami weighed in 1999 in the Portal
#                      Project -- the SAME rats as Lectures 9 and 11. Here we
#                      regress weight on hindfoot length. Fifth inference on one
#                      dataset: bootstrap mean (Lec 10), two-sample t (Lec 11),
#                      chi-square (Lec 12), ANOVA (Lec 13), now a slope.
#
#   loans.csv          A fixed 400-row sample of Lending Club loans
#                      (openintro::loans_full_schema). Loan amount vs annual
#                      income -- deliberately heteroscedastic (spread grows with
#                      income). Used to show the two roads DISAGREE when a
#                      condition fails, and that a log transform repairs it.

library(tidyverse)
library(palmerpenguins)
library(openintro)

# ---- Penguins: freeze a 40-row sample -------------------------------------
set.seed(2140)
penguins_sample <- penguins |>
  drop_na(flipper_length_mm, body_mass_g) |>
  slice_sample(n = 40) |>
  select(species, sex, flipper_length_mm, body_mass_g)

write_csv(penguins_sample, "data/penguins.csv")

# ---- Kangaroo rats: weight vs hindfoot length -----------------------------
kr <- read_csv("../../../Module2/lecture09/data/dm1999.csv", show_col_types = FALSE) |>
  filter(!is.na(weight), !is.na(hindfoot_length)) |>
  select(record_id, sex, hindfoot_length, weight)

write_csv(kr, "data/kangaroo_rats.csv")

# ---- Loans: a heteroscedastic pair, frozen at n = 400 ----------------------
set.seed(2141)
loans <- loans_full_schema |>
  filter(annual_income > 0, !is.na(loan_amount)) |>
  transmute(annual_income = annual_income / 1000,   # $000s, for readable slope
            loan_amount) |>
  slice_sample(n = 400)

write_csv(loans, "data/loans.csv")

# ---- Report ----------------------------------------------------------------
cat("\n=== penguins.csv:", nrow(penguins_sample), "penguins ===\n")
pm <- lm(body_mass_g ~ flipper_length_mm, data = penguins_sample)
print(round(coef(summary(pm)), 3))
cat("95% CI for slope:\n"); print(round(confint(pm)["flipper_length_mm", ], 2))

cat("\n=== kangaroo_rats.csv:", nrow(kr), "rats ===\n")
km <- lm(weight ~ hindfoot_length, data = kr)
print(round(coef(summary(km)), 3))
cat("95% CI for slope:\n"); print(round(confint(km)["hindfoot_length", ], 2))

cat("\n=== loans.csv:", nrow(loans), "loans (annual_income in $000s) ===\n")
lm_raw <- lm(loan_amount ~ annual_income, data = loans)
sl <- coef(lm_raw)[2]
set.seed(214); slope_of <- function(x, y) cov(x, y) / var(x)
bt <- replicate(4000, { i <- sample(nrow(loans), replace = TRUE)
  slope_of(loans$annual_income[i], loans$loan_amount[i]) })
cat(sprintf("raw slope = %.2f   model t-CI [%.2f, %.2f]   bootstrap CI [%.2f, %.2f]\n",
            sl, confint(lm_raw)[2,1], confint(lm_raw)[2,2],
            quantile(bt, .025), quantile(bt, .975)))
cat(sprintf("funnel cor(|resid|,fitted): raw = %.2f",
            cor(abs(resid(lm_raw)), fitted(lm_raw))))
lm_log <- lm(log(loan_amount) ~ log(annual_income), data = loans)
cat(sprintf("   log-log = %.2f   (log-log slope = %.2f)\n",
            cor(abs(resid(lm_log)), fitted(lm_log)), coef(lm_log)[2]))
