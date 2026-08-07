# MA214 Lecture 17 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Lecture 17 has no *observed* dataset. Every number in the deck is a specified
# probability -- a prevalence, a test characteristic, a prior, a likelihood.
# This script is the single place those constants are defined and documented;
# it writes them to CSV so the deck's scenarios can be edited without touching
# code, and so Lecture17_demo.R derives every slide number from a frozen file
# rather than from literals scattered through the script.
#
#   screening_test.csv  a rapid test for a rare disease. Prevalence 1 in 1,000.
#                       Sensitivity P(+|D) = 0.99, specificity P(-|D^c) = 0.95.
#                       Illustrative but realistic for a screening assay: the
#                       point of the example is that a good test plus a low base
#                       rate still yields a low posterior.
#
#   gw_signal.csv       the LIGO source-classification scenario. Four candidate
#                       sources with priors reflecting expected detection rates
#                       (binary black holes dominate), and likelihoods for two
#                       observations: a "long" (>1s) signal, and an
#                       electromagnetic counterpart (a kilonova). Modelled on
#                       GW170817, the first confirmed neutron-star merger.
#
#   drug_trial.csv      a Phase 1 trial of a cancer drug in n = 6 patients.
#                       p = probability a patient responds, restricted to a grid
#                       of five candidate values. The prior encodes that early
#                       stage oncology drugs usually have low response rates.

dir.create("data", showWarnings = FALSE)

# --- Screening test ---------------------------------------------------------
screening <- data.frame(
  parameter = c("prevalence", "sensitivity", "specificity"),
  value     = c(0.001,        0.99,          0.95),
  meaning   = c("P(D)", "P(+ | D)", "P(- | D^c)")
)
write.csv(screening, "data/screening_test.csv", row.names = FALSE)

# --- Gravitational-wave signal ----------------------------------------------
gw <- data.frame(
  source      = c("BBH", "BNS", "NSBH", "Noise"),
  description = c("Binary black hole merger", "Binary neutron star merger",
                  "Neutron star--black hole", "Detector glitch"),
  prior       = c(0.50, 0.15, 0.10, 0.25),
  lik_long    = c(0.20, 0.90, 0.50, 0.35),   # P(long signal | source)
  lik_em      = c(0.01, 0.80, 0.20, 0.02)    # P(EM counterpart | source)
)
write.csv(gw, "data/gw_signal.csv", row.names = FALSE)

# --- Phase 1 drug trial -----------------------------------------------------
trial <- data.frame(
  p     = c(0.1,  0.3,  0.5,  0.7,  0.9),
  prior = c(0.45, 0.30, 0.15, 0.07, 0.03)
)
stopifnot(sum(trial$prior) == 1)
write.csv(trial, "data/drug_trial.csv", row.names = FALSE)

cat("Wrote data/screening_test.csv, data/gw_signal.csv, data/drug_trial.csv\n")
