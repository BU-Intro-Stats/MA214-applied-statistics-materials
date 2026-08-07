# MA214 Lecture 20 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Lecture 20 finally USES the posteriors built in Lectures 18-19: credible
# intervals, hypothesis tests, model comparison, prediction, and predictive
# checking. It reuses the same three running examples so the numbers stay
# consistent across the module -- this script is the single place their inputs
# are defined, and Lecture20_demo.R derives every slide number from these files.
#
#   drug_trial.csv    Phase 1 oncology trial, n = 6, x = 2 respond (from Lecture 18).
#                     Beta(1,5) prior -> Beta(3,9) posterior. Used for the
#                     credible interval and the H0: p <= 0.15 hypothesis test.
#
#   birthweights.csv  n = 12 births, simulated so xbar is exactly 2980 g (from
#                     Lecture 18). Prior N(3300, 200^2), known sigma = 500 -> posterior
#                     N(3090, 117^2). Used to formalize the Lecture 18 credible-interval
#                     glimpse.
#
#   whale_sightings.csv  n = 7 days off Cape Cod: 5, 2, 4, 6, 3, 7, 4 (total 31,
#                     mean 4.43/day). Gamma(6,2) prior -> Gamma(37,9) posterior
#                     (from Lecture 18). Carries the hypothesis test, model comparison,
#                     prediction, and predictive-checking sections.
#
# The three files are byte-identical to Lecture 18's so the running examples line
# up; the seeds/values below are copied verbatim from Lecture 18's prepare_data.R.

dir.create("data", showWarnings = FALSE)

# --- Phase 1 drug trial (beta-binomial) -------------------------------------
trial <- data.frame(
  parameter = c("n", "x"),
  value     = c(6,   2),
  meaning   = c("patients enrolled", "patients who responded")
)
write.csv(trial, "data/drug_trial.csv", row.names = FALSE)

# --- Birthweights (normal-normal) -------------------------------------------
# Simulated, then shifted so mean(weight) is exactly 2980 (same seed as Lecture 18).
set.seed(2141)
bw <- rnorm(12, mean = 2980, sd = 500)
bw <- bw - mean(bw) + 2980
birthweights <- data.frame(birth = 1:12, weight_g = round(bw, 1))
stopifnot(abs(mean(birthweights$weight_g) - 2980) < 0.05)
write.csv(birthweights, "data/birthweights.csv", row.names = FALSE)

# --- Whale sightings (gamma-poisson) ----------------------------------------
whales <- data.frame(day = 1:7, sightings = c(5, 2, 4, 6, 3, 7, 4))
stopifnot(sum(whales$sightings) == 31)
write.csv(whales, "data/whale_sightings.csv", row.names = FALSE)

cat("Wrote data/drug_trial.csv, data/birthweights.csv, data/whale_sightings.csv\n")
