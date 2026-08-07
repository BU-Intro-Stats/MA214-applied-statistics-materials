library(tidyverse)
library(openintro)
library(broom)      # augment(); without this the script stops at the first augment() call

theme_set(
  theme_classic(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

#############
## Poverty ##
#############

## Fit linear model ##
# Drop the incomplete rows up front so the augmented frame keeps name and state
# alongside the fitted values. lm() would drop them anyway, so the fit is the
# same, but augment() can then be joined back to the county identifiers.
county_clean = county |> filter(!is.na(poverty), !is.na(unemployment_rate))
fit = lm(poverty ~ unemployment_rate, county_clean)
summary(fit)
county_aug = augment(fit, data = county_clean)

## Make plots ##
# scatter plot 
scatter = ggplot(county, aes(x = unemployment_rate, y = poverty)) + 
  geom_point(size = 2, alpha=.5) + 
  labs(y = "poverty rate (%)", x = "unemployment rate (%)")
scatter
ggsave(
  "figures/unemployment-vs-poverty.png",
  width = 6,
  height = 6,
  dpi = 150
)

# scatter plot with line 
scatter + 
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)
ggsave(
  "figures/unemployment-vs-poverty-line.png",
  width = 8,
  height = 6,
  dpi = 150
)

# scatter plot with line and residuals 
ggplot(county_aug, aes(x = unemployment_rate, y = poverty)) + 
  geom_point(size = 2, alpha=.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = unemployment_rate, yend = .fitted), alpha = 0.5, color = 'red') +
  labs(y = "poverty rate (%)", x = "unemployment rate (%)")
ggsave(
  "figures/unemployment-vs-poverty-residuals.png",
  width = 8,
  height = 6,
  dpi = 150
)

# scatter plot with the fitted line carried back to x = 0, so the intercept is
# visible as a point on the plot rather than just a number in the output.
# Shown on the slide at 0.92\textwidth (~4.0in), so saving 7in wide keeps the
# axis text near slide body size.
scatter +
  expand_limits(x = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  annotate(
    "text",
    x = 0.3, y = coef(fit)[["(Intercept)"]],
    label = "Intercept", hjust = 0, vjust = 1.6,
    colour = "purple", fontface = "bold", size = 4
  )
ggsave(
  "figures/unemployment-vs-poverty-with-intercept.png",
  width = 7,
  height = 5.25,
  dpi = 150
)

# residual plot with the two counties the slide calls out by name. An unlabeled
# extreme point is not self-explanatory, so both get a label on the plot.
labeled <- county_aug |>
  filter((name == "Jefferson County" & state == "Mississippi") |
         (name == "Colusa County"    & state == "California")) |>
  mutate(label = paste0(name, "\n(", state, ")"))

ggplot(county_aug, aes(x = unemployment_rate, y = poverty)) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = unemployment_rate, yend = .fitted),
               alpha = 0.5, color = "red") +
  geom_text(
    data = labeled, aes(label = label),
    colour = "purple", fontface = "bold", size = 3.6,
    hjust = 1.1, vjust = 0.5, lineheight = 0.9
  ) +
  labs(y = "poverty rate (%)", x = "unemployment rate (%)")
ggsave(
  "figures/unemployment-vs-poverty-residuals-labeled.png",
  width = 6.5,
  height = 4.9,
  dpi = 150
)

# #################
# ## Log poverty ##
# #################
# 
# ## Fit linear model ##
# logfit = lm(log(poverty) ~ per_capita_income_thousands, county)
# summary(logfit)
# county_aug = augment(logfit) |> 
#   mutate(log_poverty = `log(poverty)`,
#          poverty = exp(`log(poverty)`),
#          poverty_est = exp(.fitted))
# 
# ## Make plots ##
# # scatter plot 
# ggplot(county, aes(x = per_capita_income_thousands, y = poverty)) + 
#   geom_point(size = 2, alpha=.5) + 
#   scale_y_log10() + 
#   labs(y = "poverty rate (%)", x = "income per capita ($thousands)")
# ggsave(
#   "figures/county-income-vs-poverty.png",
#   width = 6,
#   height = 6,
#   dpi = 150
# )
# 
# # with line and residuals 
# # ggplot(county_aug, aes(x = per_capita_income_thousands, y = log_poverty)) + 
# #   geom_point(size = 2, alpha=.5) +
# #   geom_smooth(method = "lm", se = FALSE) +
# #   # geom_line(aes(y = .fitted)) +
# #   geom_segment(aes(xend = per_capita_income_thousands, yend = .fitted), alpha = 0.5) +
# #   # scale_y_log10() + 
# #   labs(y = "poverty rate (%)", x = "income per capita ($thousands)")
# 
# # add line and residuals 
# ggplot(county_aug, aes(x = per_capita_income_thousands, y = poverty)) + 
#   geom_point(size = 2, alpha=.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   geom_segment(aes(xend = per_capita_income_thousands, yend = poverty_est), alpha = 0.5, color = 'red') +
#   scale_y_log10() + 
#   labs(y = "poverty rate (%)", x = "income per capita ($thousands)")
# 
# ggsave(
#   "figures/county-income-vs-poverty-residuals.png",
#   width = 10,
#   height = 6,
#   dpi = 150
# )

