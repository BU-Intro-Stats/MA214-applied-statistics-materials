# MA214 Lecture 19 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Lecture 19 reuses the whale-sightings data from Lecture 18 and asks the same
# question with a NON-conjugate (log-normal) prior, so there is no closed-form
# posterior and we must approximate. The only observed data is the daily counts;
# everything else (priors, grids, MCMC settings) is specified in Lecture19_demo.R.
#
#   whale_sightings.csv   n = 7 days of whale counts off Cape Cod: 5, 2, 4, 6, 3,
#                         7, 4. Total 31, mean 4.43/day. Historical baseline ~3.
#                         Identical to Lecture 18's file, carried over on purpose
#                         so students see the same data give a different workflow.

dir.create("data", showWarnings = FALSE)

whales <- data.frame(day = 1:7, sightings = c(5, 2, 4, 6, 3, 7, 4))
stopifnot(sum(whales$sightings) == 31, nrow(whales) == 7)
write.csv(whales, "data/whale_sightings.csv", row.names = FALSE)

cat("Wrote data/whale_sightings.csv (n = 7, sum = 31)\n")
