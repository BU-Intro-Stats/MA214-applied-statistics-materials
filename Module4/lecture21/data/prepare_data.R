# MA214 Lecture 21 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Lecture 21 puts a regression model into the Bayesian frame. Two datasets:
#
#   penguins40.csv    THE LECTURE EXAMPLE. n = 40 Palmer penguins (subsample),
#                     body_mass_g ~ flipper_length_mm. Regenerated below from the
#                     palmerpenguins package with a fixed seed, so Lecture21_demo.R
#                     derives every slide number from a frozen file.
#
#   ames_housing.csv  THE WORKSHEET EXAMPLE. n = 2,930 homes sold in Ames, Iowa
#                     (De Cock, 2011), sale_price (in $000s) ~ living_area (sq ft).
#                     FROZEN FILE -- copied verbatim from Module3/lecture15/data/,
#                     where students already fit this same regression the
#                     *frequentist* way (OLS, CI for the mean, prediction interval).
#                     Lecture 21's worksheet has them redo that regression the
#                     Bayesian way, so the two must be the same file.
#                     Not regenerated here: the AmesHousing / modeldata packages are
#                     not installed, and the CSV is the committed source of truth.
#                     If it ever needs rebuilding: AmesHousing::make_ames().

dir.create("data", showWarnings = FALSE)

# --- Palmer penguins subsample (n = 40) -------------------------------------
# Same seed as the original course script, so the subsample is unchanged.
suppressPackageStartupMessages(library(palmerpenguins))
suppressPackageStartupMessages(library(tidyverse))

penguins_full <- penguins |> drop_na(flipper_length_mm, body_mass_g)
set.seed(2140)
penguins_sub <- penguins_full |>
  slice_sample(n = 40) |>
  select(flipper_length_mm, body_mass_g)

stopifnot(nrow(penguins_sub) == 40)
write_csv(penguins_sub, "data/penguins40.csv")
cat(sprintf("Wrote data/penguins40.csv (%d rows; flipper %d-%d mm, mass %d-%d g)\n",
            nrow(penguins_sub),
            min(penguins_sub$flipper_length_mm), max(penguins_sub$flipper_length_mm),
            min(penguins_sub$body_mass_g), max(penguins_sub$body_mass_g)))

# --- Ames housing -----------------------------------------------------------
if (file.exists("data/ames_housing.csv")) {
  cat("data/ames_housing.csv present (frozen file, shared with Lecture 15)\n")
} else {
  stop("data/ames_housing.csv is missing -- copy it from Module3/lecture15/data/")
}
