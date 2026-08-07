# MA214 Lecture: Bootstrap confidence intervals (Portal Project)
# Instructor solution script (same as student + a little polish)

library(tidyverse)
library(infer)

# Load data --------------------------------------------------------------------
# Make sure surveys.csv and species.csv are in your working directory.
surveys <- read_csv("surveys.csv", show_col_types = FALSE)
species <- read_csv("species.csv", show_col_types = FALSE)

# Join on species name (optional, just for nicer labels) ------------------------
dat <- surveys %>%
  left_join(species, by = "species_id") %>%
  mutate(scientific_name = paste(genus, species)) 

# Filter to a single species + year (keeps the story focused) ------------------
# We use kangaroo rats (Dipodomys merriami, species_id == "DM") as an example.
dm_1999 <- dat %>%
  filter(species_id == "DM", year == 1999, !is.na(weight))

# Quick check: what does the distribution look like? ---------------------------
dm_1999 %>%
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Weight (g)", y = "Count",
       title = "Kangaroo rat weights (Portal Project, 1999)")

# ------------------------------------------------------------------------------
# PART 1: Bootstrap CI for the MEAN weight
# ------------------------------------------------------------------------------
set.seed(214)

boot_mean <- dm_1999 %>%
  specify(response = weight) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  calculate(stat = "mean")

# 95% percentile CI
ci_mean <- boot_mean %>% get_ci(level = 0.95, type = "percentile")
ci_mean

boot_mean %>%
  visualize() +
  shade_confidence_interval(endpoints = ci_mean) +
  labs(title = "Bootstrap distribution of the mean (1999 DM weight)",
       x = "Bootstrap mean weight (g)")

# ------------------------------------------------------------------------------
# PART 2: CI for Median
# ------------------------------------------------------------------------------

# TODO: Compute boot_median and ci_median by copying and adjusting the code for computing boot_mean
boot_median <- dm_1999 %>%
  specify(response = weight) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  calculate(stat = "median")

# 95% percentile CI
ci_median <- boot_median %>% get_ci(level = 0.95, type = "percentile")
ci_median

# Visualize
boot_median %>%
  visualize() +
  shade_confidence_interval(endpoints = ci_median) +
  labs(title = "Bootstrap distribution: median weight",
       x = "Bootstrap median weight (g)")

# ------------------------------------------------------------------------------
# PART 3: CI for Percentiles 
# ------------------------------------------------------------------------------

# 10th percentile: infer doesn't have a built-in "quantile" stat in calculate()
# The simplest approach: compute it yourself after resampling.
boot_q10 <- dm_1999 %>%
  specify(response = weight) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  summarise(stat = quantile(weight, probs = 0.10, na.rm = TRUE))

ci_q10 <- boot_q10 %>% get_ci(level = 0.95, type = "percentile")
ci_q10

boot_q10 %>%
  ggplot(aes(x = stat)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = ci_q10$lower_ci) +
  geom_vline(xintercept = ci_q10$upper_ci) +
  labs(title = "Bootstrap distribution: 10th percentile of weight",
       x = "Bootstrap 10th percentile (g)",
       y = "Count")

# TODO: do the same for the 90th percentile, including computing 
# boot_q90 and ci_q10, and plotting the bootstrap distribution 
boot_q90 <- dm_1999 %>%
  specify(response = weight) %>%
  generate(reps = 2000, type = "bootstrap") %>%
  summarise(stat = quantile(weight, probs = 0.90, na.rm = TRUE))

ci_q90 <- boot_q90 %>% get_ci(level = 0.95, type = "percentile")
ci_q90

boot_q90 %>%
  ggplot(aes(x = stat)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = ci_q90$lower_ci) +
  geom_vline(xintercept = ci_q90$upper_ci) +
  labs(title = "Bootstrap distribution: 90th percentile of weight",
       x = "Bootstrap 90th percentile (g)",
       y = "Count")

# ------------------------------------------------------------------------------
# PART 4: Compare and explain
# ------------------------------------------------------------------------------

# The following code will plot all four CIs at once along with the data histogram 

# Collect CIs into one table (mean, median, 10th, 90th)
ci_all <- bind_rows(
  ci_mean   %>% mutate(param = "Mean"),
  ci_median %>% mutate(param = "Median"),
  ci_q10    %>% mutate(param = "10th percentile"),
  ci_q90    %>% mutate(param = "90th percentile")
)

# Point estimates for reference (solid lines)
est_all <- tibble(
  param = c("Mean", "Median", "10th percentile", "90th percentile"),
  estimate = c(
    mean(dm_1999$weight, na.rm = TRUE),
    median(dm_1999$weight, na.rm = TRUE),
    quantile(dm_1999$weight, 0.10, na.rm = TRUE),
    quantile(dm_1999$weight, 0.90, na.rm = TRUE)
  )
)

# Turn CI bounds into a long format so we can draw BOTH bounds with one geom
ci_bounds_long <- ci_all %>%
  pivot_longer(cols = c(lower_ci, upper_ci),
               names_to = "bound", values_to = "x")

# Histogram of the data + CI bounds (dashed) + point estimates (solid)
ggplot(dm_1999, aes(x = weight)) +
  geom_histogram(binwidth = 2) +
  # dashed CI bounds
  geom_vline(data = ci_bounds_long,
             aes(xintercept = x, color = param),
             linetype = 2, linewidth = 1) +
  # solid point estimates
  geom_vline(data = est_all,
             aes(xintercept = estimate, color = param),
             linewidth = 1) +
  labs(
    title = "Kangaroo rat weights (1999) with multiple 95% bootstrap CIs",
    subtitle = "Dashed = CI bounds; Solid = point estimate",
    x = "Weight (g)",
    y = "Count",
    color = "Parameter"
  )

# Visuals for slides ------------------------------------------------------------
# base_size is set so the figure text lands near slide body size once the PNG is
# scaled down by \includegraphics: base_size ~ 10 * saved_width / (4.33 * display_frac).
# dm1999_hist is shown at 0.42\textwidth, dm1999_boot_mean at 0.55\textwidth.
p_hist <- dm_1999 %>%
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Weight (g)", y = "Count") +   # no in-figure title: the slide has a frametitle
  theme_minimal(base_size = 33)

p_boot_mean <- boot_mean %>%
  visualize() +
  shade_confidence_interval(endpoints = ci_mean) +
  # visualize() adds its own title; NULL removes it (the slide has a frametitle)
  labs(title = NULL, subtitle = NULL, x = "Bootstrap mean (g)") +
  theme_minimal(base_size = 25)

ggsave("../figures/dm1999_hist.png", p_hist, width = 6, height = 3.5, dpi = 200)
ggsave("../figures/dm1999_boot_mean.png", p_boot_mean, width = 6, height = 3.5, dpi = 200)

p_hist
p_boot_mean
