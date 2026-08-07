# MA214 Lecture 18 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Lecture 18 runs three conjugate models on three small examples. Two of them
# carry actual observations; the third is summarised by n and xbar. This script
# is the single place those inputs are defined, so Lecture18_demo.R derives
# every slide number from a frozen file rather than from literals in the script.
#
#   drug_trial.csv    Phase 1 oncology trial, n = 6 patients, x = 2 respond.
#                     The same trial Lecture 17 used with a discrete grid on p;
#                     here p gets a continuous Beta prior. Carrying it over on
#                     purpose -- students should see the grid become a curve.
#
#   oncologists.csv   five oncologists' Beta priors on the same response rate,
#                     from sceptical (Alvarez) to optimistic (Eriksen). Used to
#                     show five priors + one dataset -> five posteriors.
#
#   birthweights.csv  n = 12 births at a rural hospital. SIMULATED so that the
#                     sample mean is exactly 2980 g, the value the slides quote.
#                     National reference: mean 3300 g, SD 500 g (the known sigma).
#
#   whale_sightings.csv  n = 7 days of whale counts off Cape Cod during spring
#                     migration: 5, 2, 4, 6, 3, 7, 4. Total 31, mean 4.43/day.
#                     Historical baseline is about 3 sightings/day.

dir.create("data", showWarnings = FALSE)

# --- Phase 1 drug trial (beta-binomial) -------------------------------------
trial <- data.frame(
  parameter = c("n", "x"),
  value     = c(6,   2),
  meaning   = c("patients enrolled", "patients who responded")
)
write.csv(trial, "data/drug_trial.csv", row.names = FALSE)

# --- Five oncologists' priors -----------------------------------------------
oncologists <- data.frame(
  oncologist = c("Alvarez", "Banerjee", "Chen", "Diallo", "Eriksen"),
  stance     = c("very sceptical", "expects a low rate", "no strong opinion",
                 "cautiously optimistic", "very optimistic"),
  alpha      = c(1,  1, 1, 4, 10),
  beta       = c(20, 5, 1, 2,  2)
)
write.csv(oncologists, "data/oncologists.csv", row.names = FALSE)

# --- Birthweights (normal-normal) -------------------------------------------
# Simulated, then shifted so mean(weight) is exactly 2980 -- the slides quote
# xbar = 2980 g, and the deck must not disagree with its own data file.
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

cat("Wrote data/drug_trial.csv, data/oncologists.csv,",
    "data/birthweights.csv, data/whale_sightings.csv\n")
