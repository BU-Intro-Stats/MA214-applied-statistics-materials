# MA214 Lecture 12 -- data preparation
# Run ONCE from the lecture directory:  Rscript data/prepare_data.R
#
# Writes the two datasets the lecture and its worksheet use. Both are REAL data.
#
#   plantgrowth.csv   R's built-in PlantGrowth (Dobson 1983, "An Introduction to
#                     Statistical Modelling", p. 9). 30 plants, 3 groups of 10:
#                     a control and two treatments; outcome = dried weight (g).
#                     Small enough that SSG/SSE can be computed by hand on a slide.
#
#   portal_rodents.csv  Kangaroo rats (genus Dipodomys) from the Portal Project
#                     Teaching Database -- the same long-term desert-rodent study
#                     used in Lectures 9, 11 and 12. Derived here by joining the
#                     three source tables shipped with Lecture 9:
#                       ../../lecture09/activity-materials/{surveys,species,plots}.csv
#                     Keeps captures with a recorded weight. plot_type is the
#                     experimental treatment whose species counts Lecture 11 tested
#                     with chi-square; here the outcome is numeric (weight) instead.

library(tidyverse)

src <- "../../lecture09/activity-materials"

surveys <- read_csv(file.path(src, "surveys.csv"), show_col_types = FALSE)
species <- read_csv(file.path(src, "species.csv"), show_col_types = FALSE)
plots   <- read_csv(file.path(src, "plots.csv"),   show_col_types = FALSE)

# ---- PlantGrowth ------------------------------------------------------------
data(PlantGrowth)
write_csv(PlantGrowth, "data/plantgrowth.csv")

# ---- Portal kangaroo rats ---------------------------------------------------
portal <- surveys |>
  left_join(species, by = "species_id") |>
  left_join(plots,   by = "plot_id") |>
  filter(genus == "Dipodomys", !is.na(weight), !is.na(plot_type)) |>
  select(record_id, year, species_id, species, plot_id, plot_type, sex, weight)

write_csv(portal, "data/portal_rodents.csv")

# ---- Report -----------------------------------------------------------------
cat("\n=== plantgrowth.csv ===\n")
print(PlantGrowth |> group_by(group) |>
        summarise(n = n(), mean = mean(weight), sd = sd(weight)))

cat("\n=== portal_rodents.csv:", nrow(portal), "captures ===\n")

cat("\n-- by species (the 'unequal variance' example) --\n")
print(portal |> group_by(species_id, species) |>
        summarise(n = n(), mean = mean(weight), sd = sd(weight), .groups = "drop"))

cat("\n-- D. merriami by plot type (the activity) --\n")
print(portal |> filter(species_id == "DM") |> group_by(plot_type) |>
        summarise(n = n(), mean = mean(weight), sd = sd(weight), .groups = "drop"))
