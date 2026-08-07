# MA214 Lecture 15 -- Prediction and cross-validation
# In-class demo + figure generation.
#
# Data (frozen by data/prepare_data.R):
#   data/exoplanets.csv     THE SAME 890 planets Lecture 14 analysed. Lecture 14
#                           asked whether the coefficients were real; today we
#                           ask what the model is FOR -- predicting the next
#                           planet -- and how to tell a model that predicts well
#                           from one that merely fits well.
#   data/ames_housing.csv   2,930 houses -- the in-class activity.
#
# Every number that appears on a slide is printed below.
# Run from this directory:  Rscript Lecture15_demo.R

library(tidyverse)
library(broom)
library(patchwork)

set.seed(214)
dir.create("figures", showWarnings = FALSE)
pdf(NULL)                      # keep stray Rplots.pdf from appearing

blue <- "#569BBD"
red  <- "#F05133"

# The same analysis set as Lecture 14 -- see data/prepare_data.R for why.
exo <- read_csv("data/exoplanets.csv", show_col_types = FALSE) |>
  drop_na(orbital_period, semi_major_axis, planet_mass, planet_radius,
          stellar_mass, stellar_temp, equilibrium_temp, eccentricity) |>
  mutate(cbrt_mass = planet_mass^(1/3))

n <- nrow(exo)

# ==============================================================================
cat("\n==============================================================\n")
cat("1. The model we are predicting with -- Lecture 14's cube-root fit\n")
cat("==============================================================\n")

model <- lm(planet_radius ~ cbrt_mass, data = exo)
s_e   <- summary(model)$sigma
x_bar <- mean(exo$cbrt_mass)
s_x   <- sd(exo$cbrt_mass)

cat(sprintf("n = %d planets (the same ones as Lecture 14)\n", n))
print(round(coef(summary(model)), 3))
cat(sprintf("adj R^2 = %.3f   s_e = %.3f   df = n - 2 = %d\n",
            summary(model)$adj.r.squared, s_e, n - 2))
cat(sprintf("x-bar = %.3f   s_x = %.3f   t* = %.3f\n",
            x_bar, s_x, qt(0.975, n - 2)))
cat("\nNOTE for the slides: Lecture 14 preferred the LOG-LOG model on the\n")
cat("diagnostics. We predict with the cube-root model here for one reason only:\n")
cat("its SE can be written down without matrix algebra, so the three terms are\n")
cat("visible. Section 3 below shows what that choice costs us.\n")


# ==============================================================================
cat("\n==============================================================\n")
cat("2. Two intervals at planet_mass = 50\n")
cat("==============================================================\n")

x_star <- 50^(1/3)
newp   <- tibble(cbrt_mass = x_star)
ci     <- predict(model, newp, interval = "confidence")
pi     <- predict(model, newp, interval = "prediction")

cat(sprintf("x* = 50^(1/3) = %.3f;  y-hat = %.2f Earth radii\n", x_star, ci[1]))
cat(sprintf("  CI for the MEAN response  [%.2f, %.2f]   width %.2f\n",
            ci[2], ci[3], ci[3] - ci[2]))
cat(sprintf("  PI for an INDIVIDUAL      [%.2f, %.2f]   width %.2f   (%.1fx wider)\n",
            pi[2], pi[3], pi[3] - pi[2], (pi[3] - pi[2]) / (ci[3] - ci[2])))

# The three terms of the SE, evaluated at x*.
t1 <- 1
t2 <- 1 / n
t3 <- (x_star - x_bar)^2 / ((n - 1) * s_x^2)
cat(sprintf("\nThe three terms inside the PI's square root, at x*:\n"))
cat(sprintf("  (1) residual variability      1          = %.5f   <- %.1f%% of the total\n",
            t1, 100 * t1 / (t1 + t2 + t3)))
cat(sprintf("  (2) estimating the line       1/n        = %.5f\n", t2))
cat(sprintf("  (3) distance from x-bar       (x*-xbar)^2/((n-1)s_x^2) = %.5f\n", t3))
cat(sprintf("  SE_yhat = s_e * sqrt(sum) = %.3f ;  SE_muhat = s_e * sqrt(2+3) = %.3f\n",
            s_e * sqrt(t1 + t2 + t3), s_e * sqrt(t2 + t3)))
cat("The '1' dominates: almost all of a prediction's uncertainty is irreducible.\n")

cat(sprintf("\nAs n -> infinity:  CI width -> 0, but PI width -> 2 * t* * s_e = %.2f\n",
            2 * qt(0.975, n - 2) * s_e))

cat(sprintf("\n*** The PI's lower bound is %.2f -- a NEGATIVE planet radius. ***\n", pi[2]))
cat("Impossible. Normal errors on the raw scale have no floor. This is the\n")
cat("second, and blunter, reason Lecture 14 preferred the log-log model.\n")

# ---- Figures: CI vs PI ------------------------------------------------------
x_grid    <- tibble(cbrt_mass = seq(min(exo$cbrt_mass), max(exo$cbrt_mass),
                                    length.out = 200))
pred_conf <- as_tibble(predict(model, x_grid, interval = "confidence")) |>
  bind_cols(x_grid)
pred_pred <- as_tibble(predict(model, x_grid, interval = "prediction")) |>
  bind_cols(x_grid)

y_range <- range(c(pred_pred$lwr, pred_pred$upr))

band <- function(rib, fill, alpha, title, ylab) {
  ggplot(exo, aes(x = cbrt_mass, y = planet_radius)) +
    geom_point(alpha = 0.5, size = 1.5, color = "gray50") +
    geom_ribbon(data = rib, aes(x = cbrt_mass, ymin = lwr, ymax = upr),
                alpha = alpha, fill = fill, inherit.aes = FALSE) +
    geom_line(data = pred_conf, aes(x = cbrt_mass, y = fit),
              color = blue, linewidth = 1, inherit.aes = FALSE) +
    coord_cartesian(ylim = y_range) +
    labs(x = expression("Planet Mass"^{1/3}), y = ylab, title = title) +
    theme_minimal(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72)))
}

ggsave("figures/ci_vs_pi_bands.pdf",
       band(pred_conf, blue, 0.35, "CI for the mean",
            "Planet Radius") +
         band(pred_pred, red, 0.20, "PI for an individual", NULL),
       width = 10, height = 4)

ggsave("figures/ci_vs_pi_point.pdf",
       ggplot(tibble(type = factor(c("CI for mean\nresponse", "PI for\nindividual"),
                                   levels = c("CI for mean\nresponse",
                                              "PI for\nindividual")),
                     fit = c(ci[1], pi[1]),
                     lwr = c(ci[2], pi[2]), upr = c(ci[3], pi[3])),
              aes(x = type, y = fit)) +
         geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
         geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15,
                       linewidth = 1.2, color = blue) +
         geom_point(size = 4, color = blue) +
         annotate("text", x = 2, y = pi[2], label = "a negative radius?",
                  vjust = 1.6, size = 3.4, color = red) +
         labs(x = NULL, y = "Planet Radius",
              title = "Both intervals at mass = 50") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72))),
       width = 5, height = 4)

# ---- Figure: how the PI width depends on x* (the third term) ----------------
wide <- tibble(cbrt_mass = seq(0.5, max(exo$cbrt_mass) + 5, length.out = 200))
wide <- as_tibble(predict(model, wide, interval = "prediction")) |>
  bind_cols(wide) |>
  mutate(pi_width = upr - lwr)

ggsave("figures/band_width_xstar.pdf",
       ggplot(wide, aes(x = cbrt_mass, y = pi_width)) +
         geom_line(color = blue, linewidth = 1) +
         geom_vline(xintercept = x_bar, linetype = "dashed", color = "gray40") +
         annotate("text", x = x_bar, y = max(wide$pi_width) * 0.97,
                  label = paste0("mean = ", round(x_bar, 1)), hjust = -0.15,
                  size = 3.5) +
         labs(x = expression("Planet Mass"^{1/3}),
              y = "Width of the 95% PI",
              title = expression("Prediction is sharpest near " * bar(x))) +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72))),
       width = 5, height = 4)


# ==============================================================================
cat("\n==============================================================\n")
cat("3. A prediction interval after a transformation (Lecture 14's model)\n")
cat("==============================================================\n")

loglog <- lm(log(planet_radius) ~ log(planet_mass), data = exo)
pl     <- predict(loglog, tibble(planet_mass = 50), interval = "prediction")
pe     <- exp(pl)

cat("log(radius) ~ log(mass), the model Lecture 14 chose:\n")
cat(sprintf("  on the LOG scale : [%.3f, %.3f]  centre %.3f   arms  -%.3f / +%.3f  (symmetric)\n",
            pl[2], pl[3], pl[1], pl[1] - pl[2], pl[3] - pl[1]))
cat(sprintf("  back-transformed : [%.2f, %.2f]  centre %.2f   arms  -%.2f / +%.2f  (ASYMMETRIC)\n",
            pe[2], pe[3], pe[1], pe[1] - pe[2], pe[3] - pe[1]))
cat(sprintf("  the upper arm is %.2fx the lower one -- and the interval CANNOT go negative.\n",
            (pe[3] - pe[1]) / (pe[1] - pe[2])))
cat(sprintf("  exp(mu-hat) = %.2f is the predicted MEDIAN radius, not the mean.\n", pe[1]))

grid_ll <- tibble(planet_mass = exp(seq(log(min(exo$planet_mass)),
                                        log(max(exo$planet_mass)),
                                        length.out = 200)))
band_ll <- as_tibble(predict(loglog, grid_ll, interval = "prediction")) |>
  bind_cols(grid_ll) |>
  mutate(across(c(fit, lwr, upr), exp))

ggsave("figures/loglog_prediction_band.pdf",
       ggplot(exo, aes(x = planet_mass, y = planet_radius)) +
         geom_point(alpha = 0.3, size = 1.2, color = "gray50") +
         geom_ribbon(data = band_ll, aes(x = planet_mass, ymin = lwr, ymax = upr),
                     alpha = 0.2, fill = red, inherit.aes = FALSE) +
         geom_line(data = band_ll, aes(x = planet_mass, y = fit),
                   color = blue, linewidth = 1, inherit.aes = FALSE) +
         scale_x_log10() + scale_y_log10() +
         labs(x = "Planet Mass (log)",
              y = "Planet Radius (log)",
              title = "Log-log 95% prediction band, back-transformed") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")),
       width = 6, height = 4)


# ==============================================================================
cat("\n==============================================================\n")
cat("4. Extrapolation -- what the bands do past the edge of the data\n")
cat("==============================================================\n")

x_max <- max(exo$cbrt_mass)
far   <- tibble(cbrt_mass = seq(0, 50, length.out = 300))
cf    <- as_tibble(predict(model, far, interval = "confidence")) |> bind_cols(far)
pf    <- as_tibble(predict(model, far, interval = "prediction")) |> bind_cols(far)

cat(sprintf("data stop at mass^(1/3) = %.2f (planet_mass = %.0f Earth masses)\n",
            x_max, x_max^3))
for (xs in c(x_bar, x_max, 50)) {
  p3 <- predict(model, tibble(cbrt_mass = xs), interval = "prediction")
  cat(sprintf("  at mass^(1/3) = %5.1f :  y-hat = %6.1f   PI width = %5.2f\n",
              xs, p3[1], p3[3] - p3[2]))
}
cat("\nThe band barely widens: 15.4 -> 16.9 Earth radii. With n = 890 the third\n")
cat("term is negligible, so the interval stays narrow -- WHILE THE MODEL WALKS OFF\n")
cat("A CLIFF. At mass^(1/3) = 50 it confidently predicts a radius of 63 Earth radii.\n")
cat("Jupiter is 11. The danger of extrapolation is not that the interval warns you.\n")
cat("It is that it does NOT.\n")

ggsave("figures/extrapolation_bands.pdf",
       ggplot(exo, aes(x = cbrt_mass, y = planet_radius)) +
         geom_ribbon(data = pf, aes(x = cbrt_mass, ymin = lwr, ymax = upr),
                     alpha = 0.15, fill = blue, inherit.aes = FALSE) +
         geom_ribbon(data = cf, aes(x = cbrt_mass, ymin = lwr, ymax = upr),
                     alpha = 0.35, fill = blue, inherit.aes = FALSE) +
         geom_line(data = cf, aes(x = cbrt_mass, y = fit), color = blue,
                   linewidth = 1, inherit.aes = FALSE) +
         geom_point(alpha = 0.5, size = 1.5, color = red) +
         geom_vline(xintercept = x_max, linetype = "dashed", color = "gray40",
                    linewidth = 0.8) +
         annotate("text", x = x_max / 2, y = 48, label = "data", fontface = "bold",
                  size = 4, color = "gray40") +
         annotate("text", x = x_max + 8, y = 48, label = "extrapolation",
                  fontface = "bold", size = 4, color = "darkred") +
         labs(x = expression("Planet Mass"^{1/3}), y = "Planet Radius",
              title = "Past the data, the bands only widen") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72))),
       width = 6, height = 4)


# ==============================================================================
cat("\n==============================================================\n")
cat("5. Polynomials, and how badly they extrapolate\n")
cat("==============================================================\n")

for (d in c(1, 3, 10)) {
  m <- lm(planet_radius ~ poly(cbrt_mass, d), data = exo)
  yhat_far <- predict(m, tibble(cbrt_mass = x_max * 1.5))
  cat(sprintf("  degree %2d :  in-sample R^2 = %.3f   |   predicted radius at 1.5x the\n",
              d, summary(m)$r.squared))
  cat(sprintf("               data's edge = %s Earth radii\n",
              format(round(yhat_far, 1), big.mark = ",")))
}

x_far <- tibble(cbrt_mass = seq(0, x_max * 1.8, length.out = 300))
poly_preds <- map_dfr(c(1, 3, 10), function(d) {
  m <- lm(planet_radius ~ poly(cbrt_mass, d), data = exo)
  tibble(degree = factor(paste("Degree", d),
                         levels = paste("Degree", c(1, 3, 10))),
         cbrt_mass = x_far$cbrt_mass,
         fit = predict(m, newdata = x_far))
})

ggsave("figures/polynomial_extrapolation.pdf",
       ggplot(exo, aes(x = cbrt_mass, y = planet_radius)) +
         geom_point(alpha = 0.35, size = 1.2, color = "black") +
         geom_line(data = poly_preds, aes(x = cbrt_mass, y = fit, color = degree),
                   linewidth = 1.1) +
         geom_vline(xintercept = x_max, linetype = "dashed", color = "gray50") +
         annotate("text", x = x_max, y = Inf, vjust = 1.4, hjust = 1.05,
                  size = 5.5, color = "gray30", label = "edge of data") +
         scale_color_manual(values = c("Degree 1" = blue, "Degree 3" = red,
                                       "Degree 10" = "#8B4513")) +
         scale_y_log10() +
         labs(x = expression("Planet Mass"^{1/3}),
              y = "Planet Radius (log)", color = NULL,
              title = "Past the data, the polynomials part company") +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72)),
               legend.position = "top"),
       width = 7, height = 4.6)


# ==============================================================================
cat("\n==============================================================\n")
cat("6. Overfitting -- fits better, predicts worse\n")
cat("==============================================================\n")
cat("Overfitting is a SMALL-SAMPLE phenomenon: with all 890 planets a degree-10\n")
cat("polynomial has plenty of data to pin it down. So we take a random n = 60,\n")
cat("the size of a realistic study, and watch what happens.\n")

n_sub  <- 60
exo_sub <- slice_sample(exo, n = n_sub)

cat(sprintf("\nsubsample: n = %d planets\n", n_sub))
degrees <- 1:10
insample <- map_dfr(degrees, function(d) {
  m <- lm(planet_radius ~ poly(cbrt_mass, d), data = exo_sub)
  tibble(degree = d, r2 = summary(m)$r.squared,
         adj_r2 = summary(m)$adj.r.squared, AIC = AIC(m))
})

x_dense <- tibble(cbrt_mass = seq(min(exo_sub$cbrt_mass), max(exo_sub$cbrt_mass),
                                  length.out = 300))
shown <- c(1, 3, 10)
overfit_preds <- map_dfr(shown, function(d) {
  m <- lm(planet_radius ~ poly(cbrt_mass, d), data = exo_sub)
  tibble(degree = factor(paste("Degree", d), levels = paste("Degree", shown)),
         cbrt_mass = x_dense$cbrt_mass, fit = predict(m, newdata = x_dense))
})

ggsave("figures/overfit_demo.pdf",
       ggplot(exo_sub, aes(x = cbrt_mass, y = planet_radius)) +
         geom_point(alpha = 0.8, size = 2.2, color = "black") +
         geom_line(data = overfit_preds,
                   aes(x = cbrt_mass, y = fit, color = degree), linewidth = 1) +
         scale_color_manual(values = c("Degree 1" = blue, "Degree 3" = "#ff7f0e",
                                       "Degree 10" = "#d62728")) +
         coord_cartesian(ylim = range(exo_sub$planet_radius) + c(-3, 3)) +
         labs(x = expression("Planet Mass"^{1/3}),
              y = "Planet Radius (Earth radii)", color = NULL,
              title = sprintf("n = %d: the wiggles chase noise", n_sub)) +
         theme_minimal(base_size = 25) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72)),
               legend.position = "top"),
       width = 7, height = 4.6)


# ==============================================================================
cat("\n==============================================================\n")
cat("7. Cross-validation -- the only judge that looks at unseen data\n")
cat("==============================================================\n")

# One random 5-fold split at n = 60 is far too noisy to read -- the curve jumps
# around and the minimum lands wherever the split happened to fall. So we REPEAT
# the whole k-fold procedure 20 times with different splits and average. That is
# standard practice, and it is what makes the curve mean something.
k <- 5
reps <- 20

cv <- map_dfr(degrees, function(d) {
  per_rep <- map_dbl(1:reps, function(r) {
    folds <- sample(rep(1:k, length.out = n_sub))
    mean(map_dbl(1:k, function(j) {                  # the MEAN of the k folds
      train <- exo_sub[folds != j, ]
      test  <- exo_sub[folds == j, ]
      m <- lm(planet_radius ~ poly(cbrt_mass, d), data = train)
      mean((test$planet_radius - predict(m, newdata = test))^2)
    }))
  })
  tibble(degree = d, cv_error = mean(per_rep))
})

cmp <- insample |> left_join(cv, by = "degree")
cat(sprintf("\n%d-fold CV, repeated %d times, on the n = %d subsample:\n", k, reps, n_sub))
print(as.data.frame(cmp |> mutate(across(where(is.numeric), ~ round(., 3)))))

cat(sprintf("\n  in-sample R^2 is highest at degree %d  (it can only ever go up)\n",
            cmp$degree[which.max(cmp$r2)]))
cat(sprintf("  adjusted R^2 picks degree %d\n", cmp$degree[which.max(cmp$adj_r2)]))
cat(sprintf("  AIC          picks degree %d\n", cmp$degree[which.min(cmp$AIC)]))
cat(sprintf("  CV-error     picks degree %d   <- the only one that saw unseen data\n",
            cmp$degree[which.min(cmp$cv_error)]))
cat(sprintf("\nThe punchline is not which degree wins. It is the CLIFF: CV-error at\n"))
cat(sprintf("degree 10 is %.0fx its minimum (%.1f vs %.1f), while adjusted R^2 is still\n",
            max(cmp$cv_error) / min(cmp$cv_error), max(cmp$cv_error), min(cmp$cv_error)))
cat(sprintf("cheerfully RISING at degree 10 (%.3f). Only CV sees the cliff.\n",
            cmp$adj_r2[cmp$degree == 10]))

pick <- function(df, yvar, best, ylab, title, lower_better) {
  ggplot(df, aes(x = degree, y = .data[[yvar]])) +
    geom_vline(xintercept = best, linetype = "dashed", color = "gray40") +
    geom_line(linewidth = 1, color = blue) +
    geom_point(size = 2.6, color = blue) +
    scale_x_continuous(breaks = degrees) +
    annotate("text", x = best + 0.25, y = Inf, vjust = 1.6, hjust = 0,
             label = paste("picks", best), size = 3.8, color = "gray30") +
    labs(x = "Polynomial degree", y = ylab, title = title) +
    theme_minimal(base_size = 25) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(0.72)))
}

ggsave("figures/adjr2_by_degree.pdf",
       pick(cmp, "adj_r2", cmp$degree[which.max(cmp$adj_r2)],
            expression("Adjusted " * R^2), "Adjusted R² (higher is better)"),
       width = 6.5, height = 4)
ggsave("figures/aic_by_degree.pdf",
       pick(cmp, "AIC", cmp$degree[which.min(cmp$AIC)], "AIC",
            "AIC (lower is better)"),
       width = 6.5, height = 4)
ggsave("figures/cv_comparison.pdf",
       pick(cmp, "cv_error", cmp$degree[which.min(cmp$cv_error)],
            "CV-error (MSE)",
            sprintf("%d-fold CV-error (lower is better)", k)) +
         scale_y_log10(),
       width = 6.5, height = 4)


# ==============================================================================
cat("\n==============================================================\n")
cat("8. The activity: Ames housing\n")
cat("==============================================================\n")

ames <- read_csv("data/ames_housing.csv", show_col_types = FALSE)
am   <- lm(sale_price ~ living_area, data = ames)
cat(sprintf("%d houses. sale_price ~ living_area:  R^2 = %.3f, s_e = %.1f ($000s)\n",
            nrow(ames), summary(am)$r.squared, summary(am)$sigma))
h <- tibble(living_area = 2000)
cat(sprintf("a 2,000 sq ft house:  CI [%.0f, %.0f]   PI [%.0f, %.0f]  ($000s)\n",
            predict(am, h, interval = "confidence")[2],
            predict(am, h, interval = "confidence")[3],
            predict(am, h, interval = "prediction")[2],
            predict(am, h, interval = "prediction")[3]))

cat("\nAll figures written to figures/\n")
