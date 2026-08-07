# MA214 Lecture 15 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Freezes the two datasets the lecture and its worksheet use, so the deck is
# reproducible from CSVs alone (no network at build time).
#
#   exoplanets.csv    THE SAME 1,491 planets as Lecture 14 (NASA Exoplanet
#                     Archive; every confirmed planet with both a measured mass
#                     and a measured radius). Lecture 14 asked whether the
#                     coefficients were real; Lecture 15 asks what the model is
#                     FOR -- predicting the radius of the next planet found.
#
#   ames_housing.csv  2,930 houses sold in Ames, Iowa (Dean De Cock, 2011; the
#                     AmesHousing R package, via make_ames()). Sale price in
#                     $000s plus 11 predictors. Used by the in-class activity
#                     and the worksheet.
#
# Both CSVs are committed, so this script does not re-download by default.
# To refresh:  Rscript data/prepare_data.R --refresh

library(tidyverse)

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

# ---- Exoplanets ------------------------------------------------------------
# Frozen by Lecture 14 (see Module3/lecture14/data/prepare_data.R for
# the NASA Exoplanet Archive TAP query that produced it). We keep a local copy
# so this deck builds on its own.
if (refresh) {
  file.copy("../../lecture14/data/exoplanets.csv", "data/exoplanets.csv",
            overwrite = TRUE)
  cat("Refreshed data/exoplanets.csv from Lecture 14.\n")
}

exoplanets <- read_csv("data/exoplanets.csv", show_col_types = FALSE)

# THE SAME analysis set as Lecture 14: complete cases on every column that deck
# used. We deliberately do NOT widen it to all 1,491 planets, even though our
# model only needs mass and radius -- the opening slide says "the model we fitted
# in Lecture 14", and that has to be literally the same fit, on the same planets.
analysis_cols <- c("orbital_period", "semi_major_axis", "planet_mass",
                   "planet_radius", "stellar_mass", "stellar_temp",
                   "equilibrium_temp", "eccentricity")

exo <- exoplanets |>
  drop_na(all_of(analysis_cols)) |>
  mutate(cbrt_mass = planet_mass^(1/3))

cat("\n=== exoplanets.csv ===\n")
cat("raw:", nrow(exoplanets), "planets;  Lecture 14's analysis set: n =", nrow(exo), "\n")

# Lecture 14's cube-root model -- the one this lecture predicts with.
fit <- lm(planet_radius ~ cbrt_mass, data = exo)
cat("\nLecture 16's cube-root model, radius ~ mass^(1/3):\n")
print(round(coef(summary(fit)), 4))
cat(sprintf("adj R^2 = %.3f   residual SE s_e = %.3f   df = n - 2 = %d\n",
            summary(fit)$adj.r.squared, summary(fit)$sigma, nrow(exo) - 2))
cat(sprintf("x-bar (cbrt_mass) = %.3f,  s_x = %.3f\n",
            mean(exo$cbrt_mass), sd(exo$cbrt_mass)))

# The prediction the deck makes: a new planet of 50 Earth masses.
newp <- tibble(cbrt_mass = 50^(1/3))
ci <- predict(fit, newp, interval = "confidence")
pi <- predict(fit, newp, interval = "prediction")
cat(sprintf("\nAt planet_mass = 50 (cbrt = %.3f):  y-hat = %.2f Earth radii\n",
            newp$cbrt_mass, ci[1]))
cat(sprintf("  CI for the MEAN response : [%.2f, %.2f]   width %.2f\n",
            ci[2], ci[3], ci[3] - ci[2]))
cat(sprintf("  PI for an INDIVIDUAL     : [%.2f, %.2f]   width %.2f  (%.1fx wider)\n",
            pi[2], pi[3], pi[3] - pi[2], (pi[3] - pi[2]) / (ci[3] - ci[2])))

# ---- Ames housing ----------------------------------------------------------
ames <- read_csv("data/ames_housing.csv", show_col_types = FALSE)
cat("\n=== ames_housing.csv ===\n")
cat(nrow(ames), "houses;", ncol(ames), "columns (sale_price is in $000s)\n")
cat("columns:", paste(names(ames), collapse = ", "), "\n")
am <- lm(sale_price ~ living_area, data = ames)
cat(sprintf("activity's starting model, sale_price ~ living_area: R^2 = %.3f\n",
            summary(am)$r.squared))
