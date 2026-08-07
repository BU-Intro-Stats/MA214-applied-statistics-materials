# MA214 Lecture 14 -- data preparation
# Run from the lecture directory:  Rscript data/prepare_data.R
#
# Freezes the two datasets the lecture and its worksheet use, so the deck is
# reproducible from CSVs alone (no network at build time).
#
#   exoplanets.csv       1,491 confirmed exoplanets from the NASA Exoplanet
#                        Archive -- every planet with BOTH a measured mass and a
#                        measured radius (default_flag = 1). Real data, no
#                        simulation. We regress planet radius on mass, host-star
#                        mass, equilibrium temperature and eccentricity; the
#                        SAME planets also give us Kepler's third law
#                        (orbital period vs. semi-major axis) for the
#                        transformation half of the lecture.
#
#   world_happiness.csv  150 countries from the World Happiness Report. Life
#                        expectancy, GDP per capita (already on a LOG scale in
#                        this file), social support, freedom, ... Used by the
#                        in-class activity and the worksheet.
#
#   books.csv            The 15 books of DAAG::allbacks -- weight, volume and
#                        cover type. THE SAME BOOKS AS LECTURE 5, where cover
#                        type entered as an indicator and the two fitted lines
#                        were forced to stay PARALLEL. Lecture 14 lets their
#                        slopes go free, and asks whether the data wanted that.
#
# exoplanets.csv is committed, so this script does NOT re-download by default.
# To refresh it from the archive:  Rscript data/prepare_data.R --refresh

library(tidyverse)
library(DAAG)

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

# ---- Exoplanets: the NASA Exoplanet Archive TAP query ----------------------
# This is the query that produced exoplanets.csv. Confirmed planets, one row per
# planet (default_flag = 1), keeping only those with both mass and radius.
tap_query <- paste0(
  "https://exoplanetarchive.ipac.caltech.edu/TAP/sync?",
  "query=SELECT+pl_name,pl_orbper,pl_orbsmax,pl_bmasse,pl_rade,",
  "st_mass,st_teff,pl_eqt,pl_orbeccen,discoverymethod+",
  "FROM+ps+WHERE+default_flag=1+",
  "AND+pl_rade+IS+NOT+NULL+AND+pl_bmasse+IS+NOT+NULL",
  "&format=csv"
)

if (refresh || !file.exists("data/exoplanets.csv")) {
  cat("Downloading from the NASA Exoplanet Archive...\n")
  tmp <- tempfile(fileext = ".csv")
  download.file(tap_query, tmp, mode = "w")
  read_csv(tmp, show_col_types = FALSE) |>
    rename(planet_name      = pl_name,      orbital_period   = pl_orbper,
           semi_major_axis  = pl_orbsmax,   planet_mass      = pl_bmasse,
           planet_radius    = pl_rade,      stellar_mass     = st_mass,
           stellar_temp     = st_teff,      equilibrium_temp = pl_eqt,
           eccentricity     = pl_orbeccen,  discovery_method = discoverymethod) |>
    write_csv("data/exoplanets.csv")
}

exoplanets <- read_csv("data/exoplanets.csv", show_col_types = FALSE)

# ---- The analysis set ------------------------------------------------------
# ONE complete-case dataset for the whole deck. Every model on every slide is
# fit to these same rows -- otherwise the AIC and adjusted R^2 columns we use to
# compare models (cube-root vs. log-log, main effects vs. interaction) would not
# be comparable, because they would be computed on different planets.
analysis_cols <- c("orbital_period", "semi_major_axis", "planet_mass",
                   "planet_radius", "stellar_mass", "stellar_temp",
                   "equilibrium_temp", "eccentricity")

exo <- exoplanets |> drop_na(all_of(analysis_cols))

cat("\n=== exoplanets.csv ===\n")
cat("raw:", nrow(exoplanets), "planets\n")
for (cc in analysis_cols) {
  cat(sprintf("  %-17s %4d measured, %4d missing\n",
              cc, sum(!is.na(exoplanets[[cc]])), sum(is.na(exoplanets[[cc]]))))
}
cat("complete cases on all", length(analysis_cols), "columns:", nrow(exo), "planets\n")

# The headline model of the lecture.
fit <- lm(planet_radius ~ I(planet_mass^(1/3)) + stellar_mass +
            equilibrium_temp + eccentricity, data = exo)
n <- nrow(exo); p <- 4
cat(sprintf("\nn = %d, p = %d, df = n - p - 1 = %d\n", n, p, n - p - 1))
print(round(coef(summary(fit)), 4))
cat("95% CIs:\n"); print(round(confint(fit), 4))
cat(sprintf("collinear pair: cor(stellar_mass, stellar_temp) = %.3f\n",
            cor(exo$stellar_mass, exo$stellar_temp)))

# ---- Books: the same 15 books as Lecture 5 ---------------------------------
books <- allbacks |>
  as_tibble() |>
  transmute(volume, weight,
            cover = factor(ifelse(cover == "hb", "hardcover", "paperback"),
                           levels = c("hardcover", "paperback")))

write_csv(books, "data/books.csv")

cat("\n=== books.csv ===\n")
cat(nrow(books), "books:", sum(books$cover == "hardcover"), "hardcover,",
    sum(books$cover == "paperback"), "paperback\n")
cat("\nLecture 5's model -- weight ~ volume + cover (slopes forced parallel):\n")
print(round(coef(summary(lm(weight ~ volume + cover, data = books))), 2))
cat("\nLecture 16's model -- weight ~ volume * cover (slopes free):\n")
print(round(coef(summary(lm(weight ~ volume * cover, data = books))), 3))

# ---- World Happiness -------------------------------------------------------
happiness <- read_csv("data/world_happiness.csv", show_col_types = FALSE)
cat("\n=== world_happiness.csv ===\n")
cat(nrow(happiness), "countries;", ncol(happiness), "columns\n")
cat("columns:", paste(names(happiness), collapse = ", "), "\n")
cat("NOTE: gdp_per_capita is on a log scale -- exp() it for the dollar scale.\n")
cat(sprintf("activity model: life_expectancy ~ gdp gives R^2 = %.3f (raw)\n",
            summary(lm(life_expectancy ~ exp(gdp_per_capita),
                       data = happiness))$r.squared))
