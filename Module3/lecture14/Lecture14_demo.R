# MA214 Lecture 14 -- Inference for multiple regression
# In-class demo + figure generation.
#
# Data (frozen by data/prepare_data.R; REAL, from the NASA Exoplanet Archive):
#   data/exoplanets.csv       1,491 confirmed exoplanets with both a measured
#                             mass and a measured radius. We keep the 890 with
#                             complete measurements on every column the deck
#                             uses, and fit EVERY model on those same 890 rows,
#                             so the AIC / adjusted R^2 columns are comparable.
#   data/world_happiness.csv  150 countries -- the in-class activity.
#
# Every number that appears on a slide is printed below.
# Run from this directory:  Rscript Lecture14_demo.R

library(tidyverse)
library(broom)
library(gridExtra)

set.seed(214)

dir.create("figures", showWarnings = FALSE)

# arrangeGrob() needs an open device to measure its grobs; point it at a null
# one so the script does not leave a stray Rplots.pdf behind.
pdf(NULL)

blue <- "#569BBD"
red  <- "#F05133"

exoplanets <- read_csv("data/exoplanets.csv", show_col_types = FALSE)

# The one analysis set -- see data/prepare_data.R for why it is a single one.
exo <- exoplanets |>
  drop_na(orbital_period, semi_major_axis, planet_mass, planet_radius,
          stellar_mass, stellar_temp, equilibrium_temp, eccentricity) |>
  mutate(cbrt_mass = planet_mass^(1/3))

n <- nrow(exo)

cat("\n==============================================================\n")
cat("1. The data\n")
cat("==============================================================\n")
cat(sprintf("raw: %d planets;  complete cases: n = %d\n", nrow(exoplanets), n))
cat("\nEach predictor on its own (radius ~ predictor):\n")
for (pred in c("cbrt_mass", "stellar_mass", "equilibrium_temp", "eccentricity")) {
  m <- lm(reformulate(pred, "planet_radius"), data = exo)
  cat(sprintf("  %-17s R^2 = %.3f,  slope = %8.4f\n",
              pred, summary(m)$r.squared, coef(m)[2]))
}
cat(sprintf("\nradius vs raw mass:       R^2 = %.2f\n",
            cor(exo$planet_mass, exo$planet_radius)^2))
cat(sprintf("radius vs mass^(1/3):     R^2 = %.2f   <- the physics-motivated predictor\n",
            cor(exo$cbrt_mass, exo$planet_radius)^2))

# ---- Figure: radius vs each predictor -------------------------------------
pw <- function(xvar, xlab) {
  ggplot(exo, aes(x = .data[[xvar]], y = planet_radius)) +
    geom_point(alpha = 0.4, size = 1.5, color = blue) +
    geom_smooth(method = "lm", se = FALSE, color = red, formula = y ~ x) +
    labs(x = xlab, y = "Planet Radius") +
    scale_x_continuous(n.breaks = 3) +
    theme_minimal(base_size = 24)
}
ggsave("figures/pairwise_predictors.pdf",
       arrangeGrob(pw("cbrt_mass",        expression("Planet Mass"^{1/3})),
                    pw("stellar_mass",     "Stellar Mass"),
                    pw("equilibrium_temp", "Equilib. Temp"),
                    pw("eccentricity",     "Eccentricity"), ncol = 2),
       width = 8, height = 6)

# ---- Figure: raw mass vs cube-root mass ------------------------------------
scat <- function(xvar, xlab, title) {
  r2 <- cor(exo[[xvar]], exo$planet_radius)^2
  ggplot(exo, aes(x = .data[[xvar]], y = planet_radius)) +
    geom_point(alpha = 0.5, size = 1.5, color = blue) +
    geom_smooth(method = "lm", se = FALSE, color = red, formula = y ~ x) +
    annotate("text", x = Inf, y = Inf, hjust = 1.3, vjust = 1,
             label = paste0("R² = ", round(r2, 2)), size = 4.5, color = red) +
    labs(x = xlab, y = "Planet Radius (Earth radii)", title = title) +
    theme_minimal(base_size = 24)
}
ggsave("figures/exoplanet_scatter.pdf",
       scat("planet_mass", "Planet Mass (Earth masses)", "Radius vs Mass"),
       width = 5, height = 4)
ggsave("figures/exoplanet_scatter_cbrt.pdf",
       scat("cbrt_mass", expression("Planet Mass"^{1/3}),
            expression("Radius vs Mass"^{1/3})),
       width = 5, height = 4)


cat("\n==============================================================\n")
cat("2. Reading the software output -- the model on the slides\n")
cat("==============================================================\n")

fit <- lm(planet_radius ~ cbrt_mass + stellar_mass + equilibrium_temp +
            eccentricity, data = exo)
p <- 4

cat(sprintf("n = %d, p = %d predictors, df = n - p - 1 = %d\n", n, p, n - p - 1))
cat("\ntidy(fit, conf.int = TRUE):\n")
print(tidy(fit, conf.int = TRUE) |>
        mutate(across(where(is.numeric), ~ round(., 4))), n = 5)
cat(sprintf("\nadj R^2 = %.3f\n", summary(fit)$adj.r.squared))
cat(sprintf("t* for a 95%% CI on %d df = %.3f\n", n - p - 1, qt(0.975, n - p - 1)))

# The two coefficients the slides interpret in words.
b <- coef(fit); ci <- confint(fit)
cat(sprintf("\ncbrt_mass:    b = %.3f, 95%% CI (%.3f, %.3f)\n",
            b["cbrt_mass"], ci["cbrt_mass", 1], ci["cbrt_mass", 2]))
cat(sprintf("stellar_mass: b = %.2f,  95%% CI (%.2f, %.2f)\n",
            b["stellar_mass"], ci["stellar_mass", 1], ci["stellar_mass", 2]))
cat(sprintf("eccentricity: b = %.2f, p = %.3f, 95%% CI (%.2f, %.2f)  <- the borderline one\n",
            b["eccentricity"], coef(summary(fit))["eccentricity", 4],
            ci["eccentricity", 1], ci["eccentricity", 2]))


cat("\n==============================================================\n")
cat("3. Multicollinearity -- what it does to the coefficient table\n")
cat("==============================================================\n")

r_st <- cor(exo$stellar_mass, exo$stellar_temp)
cat(sprintf("cor(stellar_mass, stellar_temp) = %.3f   ->  VIF = 1/(1-r^2) = %.1f\n",
            r_st, 1 / (1 - r_st^2)))

mod_simple <- lm(planet_radius ~ stellar_mass, data = exo)
mod_collin <- lm(planet_radius ~ stellar_mass + stellar_temp, data = exo)

sm <- tidy(mod_simple, conf.int = TRUE) |> filter(term == "stellar_mass")
cl <- tidy(mod_collin, conf.int = TRUE) |> filter(term == "stellar_mass")

cat("\nThe SAME predictor, stellar_mass, in two models:\n")
cat(sprintf("  radius ~ stellar_mass                 est = %.2f  SE = %.2f  p = %.3f  CI (%.2f, %.2f)\n",
            sm$estimate, sm$std.error, sm$p.value, sm$conf.low, sm$conf.high))
cat(sprintf("  radius ~ stellar_mass + stellar_temp  est = %.2f  SE = %.2f  p = %.3f  CI (%.2f, %.2f)\n",
            cl$estimate, cl$std.error, cl$p.value, cl$conf.low, cl$conf.high))
cat(sprintf("  -> SE inflated by %.0f%%; the CI is %.1fx wider\n",
            100 * (cl$std.error / sm$std.error - 1),
            (cl$conf.high - cl$conf.low) / (sm$conf.high - sm$conf.low)))
cat(sprintf("  -> but the FIT barely moves: R^2 %.3f vs %.3f\n",
            summary(mod_simple)$r.squared, summary(mod_collin)$r.squared))

# ---- Figure: the collinear pair --------------------------------------------
ggsave("figures/stellar_corr.pdf",
       ggplot(exo, aes(x = stellar_temp, y = stellar_mass)) +
         geom_point(alpha = 0.5, size = 1.5, color = blue) +
         geom_smooth(method = "lm", se = FALSE, color = red, formula = y ~ x) +
         annotate("text", x = Inf, y = Inf, hjust = 1.3, vjust = 1.5,
                  label = paste0("r = ", round(r_st, 2)), size = 5, color = red) +
         labs(x = "Stellar Effective Temperature (K)",
              y = "Stellar Mass (Solar masses)") +
         theme_minimal(base_size = 24),
       width = 5, height = 4)

# ---- Figure: what it does to the estimate and its CI -----------------------
coef_df <- bind_rows(mutate(sm, model = "radius ~\nstellar_mass"),
                     mutate(cl, model = "radius ~ stellar_mass\n+ stellar_temp")) |>
  mutate(model = factor(model, levels = unique(model)))

ggsave("figures/multicollinearity_coef.pdf",
       ggplot(coef_df, aes(x = model, y = estimate, color = model)) +
         geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                         size = 1.2, linewidth = 1, show.legend = FALSE) +
         geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
         scale_color_manual(values = c(blue, red)) +
         labs(x = NULL, y = "Coefficient for stellar_mass (95% CI)",
              title = "Adding a collinear predictor") +
         theme_minimal(base_size = 24),
       width = 5, height = 4)


cat("\n==============================================================\n")
cat("4. Checking the conditions -- LINE, for the 4-predictor model\n")
cat("==============================================================\n")

pdf("figures/resid_vs_fitted_exo.pdf", width = 5, height = 4, pointsize = 20)
plot(fit, which = 1, cex = 0.6, caption = "", main = "Residuals vs Fitted", sub = "")
invisible(dev.off())

pdf("figures/qq_exo.pdf", width = 5, height = 4, pointsize = 20)
plot(fit, which = 2, cex = 0.6, caption = "", main = "Normal Q-Q", sub = "")
invisible(dev.off())

rp <- function(xvar, xlab) {
  ggplot(mutate(exo, .resid = residuals(fit)), aes(x = .data[[xvar]], y = .resid)) +
    geom_point(alpha = 0.4, size = 1) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
    geom_smooth(se = FALSE, color = red, linewidth = 0.7, method = "loess",
                formula = y ~ x) +
    labs(x = xlab, y = "Residuals") +
    scale_x_continuous(n.breaks = 3) +
    theme_minimal(base_size = 24)
}
ggsave("figures/resid_vs_predictors_exo.pdf",
       arrangeGrob(rp("cbrt_mass",        expression("Planet Mass"^{1/3})),
                    rp("stellar_mass",     "Stellar Mass"),
                    rp("equilibrium_temp", "Equilib. Temp"),
                    rp("eccentricity",     "Eccentricity"), ncol = 2),
       width = 7, height = 5.5)

cat(sprintf("funnel check, cor(|resid|, fitted) = %.2f  (0 = equal variance)\n",
            cor(abs(resid(fit)), fitted(fit))))
cat("Residuals vs fitted fans out and the Q-Q plot has a heavy right tail:\n")
cat("E and N are strained -> the transformation half of the lecture.\n")


cat("\n==============================================================\n")
cat("5. Transformations -- Kepler's third law\n")
cat("==============================================================\n")

raw_fit <- lm(orbital_period ~ semi_major_axis, data = exo)

ggsave("figures/period_vs_axis_raw.pdf",
       ggplot(exo, aes(x = semi_major_axis, y = orbital_period)) +
         geom_point(alpha = 0.6, size = 2, color = blue) +
         geom_smooth(method = "lm", se = FALSE, color = red, linetype = "dashed",
                     linewidth = 1, formula = y ~ x) +
         labs(x = "Semi-major Axis (AU)", y = "Orbital Period (days)",
              title = "Raw scale") +
         theme_minimal(base_size = 24),
       width = 5, height = 4)

ggsave("figures/period_vs_axis_residuals.pdf",
       ggplot(tibble(fitted = fitted(raw_fit), residuals = residuals(raw_fit)),
              aes(x = fitted, y = residuals)) +
         geom_point(alpha = 0.6, size = 2, color = blue) +
         geom_hline(yintercept = 0, linewidth = 0.8) +
         geom_smooth(se = FALSE, color = red, linewidth = 1, formula = y ~ x,
                     method = "loess") +
         labs(x = "Fitted Values", y = "Residuals",
              title = "Residuals vs Fitted: the raw fit curves") +
         theme_minimal(base_size = 24),
       width = 5, height = 4)

ggsave("figures/period_vs_axis_loglog.pdf",
       ggplot(exo, aes(x = semi_major_axis, y = orbital_period)) +
         geom_point(alpha = 0.6, size = 2, color = blue) +
         scale_x_log10() + scale_y_log10() +
         geom_smooth(method = "lm", se = FALSE, color = red, linewidth = 1,
                     formula = y ~ x) +
         labs(x = "Semi-major Axis (AU, log scale)",
              y = "Orbital Period (days, log scale)", title = "Log-log scale") +
         theme_minimal(base_size = 24),
       width = 5, height = 4)

kep <- exo |> mutate(log_period = log(orbital_period),
                     log_axis   = log(semi_major_axis))
loglog_fit <- lm(log_period ~ log_axis, data = kep)

cat("lm(log(orbital_period) ~ log(semi_major_axis)) |> tidy()\n")
print(tidy(loglog_fit, conf.int = TRUE) |>
        mutate(across(where(is.numeric), ~ round(., 4))))
cat(sprintf("\nKepler's third law predicts a slope of 3/2 = 1.5; we get %.3f.\n",
            coef(loglog_fit)["log_axis"]))
cat(sprintf("R^2 = %.4f   (raw-scale fit: R^2 = %.3f)\n",
            summary(loglog_fit)$r.squared, summary(raw_fit)$r.squared))

pdf("figures/loglog_diagnostics.pdf", width = 10, height = 4, pointsize = 20)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(loglog_fit, which = 1, cex = 0.6, caption = "", main = "Residuals vs Fitted", sub = "")
plot(loglog_fit, which = 2, cex = 0.6, caption = "", main = "Normal Q-Q", sub = "")
invisible(dev.off())

# ---- The two roads, once more: bootstrap vs. the t model -------------------
B <- 5000
boot_slopes <- replicate(B, {
  i <- sample(n, replace = TRUE)
  coef(lm(log_period ~ log_axis, data = kep[i, ]))[["log_axis"]]
})

b1    <- coef(loglog_fit)[["log_axis"]]
se_b1 <- coef(summary(loglog_fit))["log_axis", "Std. Error"]
ci_model <- confint(loglog_fit)["log_axis", ]
ci_boot  <- quantile(boot_slopes, c(0.025, 0.975))

cat("\n--- Two roads to a CI for the log-log slope ---\n")
cat(sprintf("  model-based:  b1 = %.4f, SE = %.4f, 95%% CI [%.4f, %.4f]\n",
            b1, se_b1, ci_model[1], ci_model[2]))
cat(sprintf("  bootstrap:               SE = %.4f, 95%% CI [%.4f, %.4f]\n",
            sd(boot_slopes), ci_boot[1], ci_boot[2]))
cat("  -> the two roads agree, so we can trust the model-based interval here.\n")

ggsave("figures/loglog_bootstrap_vs_model.pdf",
       ggplot() +
         geom_histogram(data = tibble(slope = boot_slopes),
                        aes(x = slope, y = after_stat(density)),
                        bins = 60, fill = blue, alpha = 0.5, color = "white") +
         geom_line(data = tibble(x = seq(b1 - 4 * se_b1, b1 + 4 * se_b1, length.out = 300)) |>
                     mutate(density = dnorm(x, b1, se_b1)),
                   aes(x = x, y = density), color = red, linewidth = 1.2) +
         geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray40",
                    linewidth = 0.8) +
         annotate("text", x = 1.5, y = dnorm(b1, b1, se_b1) * 0.95,
                  label = "Kepler's law (1.5)", hjust = -0.1, size = 3.5,
                  color = "gray40") +
         labs(x = expression(hat(beta)[1] ~ "(slope)"), y = "Density",
              title = "Bootstrap (histogram) vs. the t model (curve)") +
         theme_minimal(base_size = 24) +
         theme(plot.title = element_text(hjust = 0.5, face = "bold")),
       width = 6, height = 4)


cat("\n==============================================================\n")
cat("6. Transformations -- which variable, and why (simulated)\n")
cat("==============================================================\n")
cat("Simulated, so the truth is known. DGP: log(y) = 2 + 0.5x + eps.\n")
cat("Transforming x cannot fix a problem that lives in y.\n")

n_synth <- 200
df_het <- tibble(x = runif(n_synth, 1, 6)) |>
  mutate(y = exp(2 + 0.5 * x + rnorm(n_synth, 0, 0.4)))

fit_raw  <- lm(y ~ x, data = df_het)
fit_logx <- lm(y ~ log(x), data = df_het)
fit_logy <- lm(log(y) ~ x, data = df_het)

pdf("figures/synth_hetero.pdf", width = 12, height = 3.5, pointsize = 20)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
plot(fit_raw,  which = 1, cex = 0.5, caption = "", main = "Raw: y ~ x", sub = "")
plot(fit_logx, which = 1, cex = 0.5, caption = "", main = "log(x): y ~ log(x)", sub = "")
plot(fit_logy, which = 1, cex = 0.5, caption = "", main = "log(y): log(y) ~ x", sub = "")
invisible(dev.off())

pdf("figures/synth_nonnormal.pdf", width = 12, height = 3.5, pointsize = 20)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
plot(fit_raw,  which = 2, cex = 0.5, caption = "", main = "Raw: y ~ x", sub = "")
plot(fit_logx, which = 2, cex = 0.5, caption = "", main = "log(x): y ~ log(x)", sub = "")
plot(fit_logy, which = 2, cex = 0.5, caption = "", main = "log(y): log(y) ~ x", sub = "")
invisible(dev.off())

cat("\nfunnel check, cor(|resid|, fitted):\n")
cat(sprintf("  raw    %.2f\n  log(x) %.2f\n  log(y) %.2f   <- only transforming y flattens it\n",
            cor(abs(resid(fit_raw)),  fitted(fit_raw)),
            cor(abs(resid(fit_logx)), fitted(fit_logx)),
            cor(abs(resid(fit_logy)), fitted(fit_logy))))

# Three shapes of non-linearity, each needing a different transformation.
df_i   <- tibble(x = runif(n_synth, 1, 50)) |>   # diminishing returns  -> log(x)
  mutate(y = 3 + 2 * log(x) + rnorm(n_synth, 0, 0.5))
df_ii  <- tibble(x = runif(n_synth, 0, 5)) |>    # exponential growth   -> log(y)
  mutate(y = exp(1 + 0.4 * x + rnorm(n_synth, 0, 0.3)))
df_iii <- tibble(x = runif(n_synth, 1, 100)) |>  # power law            -> log-log
  mutate(y = exp(0.5 + 1.5 * log(x) + rnorm(n_synth, 0, 0.4)))

pdf("figures/synth_nonlinear.pdf", width = 12, height = 6.6, pointsize = 20)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
plot(lm(y ~ x,             data = df_i),   which = 1, cex = 0.5, caption = "", main = "Raw: y ~ x", sub = "")
plot(lm(y ~ log(x),        data = df_i),   which = 1, cex = 0.5, caption = "", main = "log(x): y ~ log(x)", sub = "")
plot(lm(y ~ x,             data = df_ii),  which = 1, cex = 0.5, caption = "", main = "Raw: y ~ x", sub = "")
plot(lm(log(y) ~ x,        data = df_ii),  which = 1, cex = 0.5, caption = "", main = "log(y): log(y) ~ x", sub = "")
plot(lm(y ~ x,             data = df_iii), which = 1, cex = 0.5, caption = "", main = "Raw: y ~ x", sub = "")
plot(lm(log(y) ~ log(x),   data = df_iii), which = 1, cex = 0.5, caption = "", main = "log-log: log(y) ~ log(x)", sub = "")
invisible(dev.off())


cat("\n==============================================================\n")
cat("7. Back to the radius: cube-root vs. log-log\n")
cat("==============================================================\n")

mod_cbrt   <- lm(planet_radius ~ cbrt_mass, data = exo)
mod_loglog <- lm(log(planet_radius) ~ log(planet_mass), data = exo)

cat(sprintf("cube-root   radius ~ mass^(1/3):        adj R^2 = %.3f   AIC = %.0f\n",
            glance(mod_cbrt)$adj.r.squared, glance(mod_cbrt)$AIC))
cat(sprintf("log-log     log(radius) ~ log(mass):    adj R^2 = %.3f   AIC = %.0f\n",
            glance(mod_loglog)$adj.r.squared, glance(mod_loglog)$AIC))
cat("CAUTION for the slide: the AICs are NOT comparable -- the response is on a\n")
cat("different scale (radius vs. log radius). Compare the DIAGNOSTICS instead.\n")

pdf("figures/radius_cbrtmass_diagnostics.pdf", width = 8, height = 3.5, pointsize = 20)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(mod_cbrt, which = 1, cex = 0.5, caption = "", main = "Residuals vs Fitted", sub = "")
plot(mod_cbrt, which = 2, cex = 0.5, caption = "", main = "Normal Q-Q", sub = "")
invisible(dev.off())

pdf("figures/radius_loglog_diagnostics.pdf", width = 8, height = 3.5, pointsize = 20)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(mod_loglog, which = 1, cex = 0.5, caption = "", main = "Residuals vs Fitted", sub = "")
plot(mod_loglog, which = 2, cex = 0.5, caption = "", main = "Normal Q-Q", sub = "")
invisible(dev.off())

cat(sprintf("\nfunnel check, cor(|resid|, fitted):  cube-root %.2f   log-log %.2f\n",
            cor(abs(resid(mod_cbrt)), fitted(mod_cbrt)),
            cor(abs(resid(mod_loglog)), fitted(mod_loglog))))


cat("\n==============================================================\n")
cat("8. Interaction, case 1: the books -- a CATEGORICAL predictor\n")
cat("==============================================================\n")
cat("The same 15 books as Lecture 5, where the indicator for cover type shifted\n")
cat("the intercept but LEFT THE SLOPES PARALLEL. Now we let them go free.\n")

books <- read_csv("data/books.csv", show_col_types = FALSE) |>
  mutate(cover = factor(cover, levels = c("hardcover", "paperback")))

bk_par <- lm(weight ~ volume + cover, data = books)   # Lecture 5's model
bk_int <- lm(weight ~ volume * cover, data = books)   # slopes free

cat("\nLecture 5 -- weight ~ volume + cover (parallel):\n")
print(tidy(bk_par) |> mutate(across(where(is.numeric), ~ round(., 2))))
cat("\nToday -- weight ~ volume * cover (slopes free):\n")
print(tidy(bk_int, conf.int = TRUE) |>
        mutate(across(where(is.numeric), ~ round(., 3))))

cb <- coef(bk_int)
cat(sprintf("\n  hardcover:  weight = %.1f + %.3f x volume\n", cb[1], cb[2]))
cat(sprintf("  paperback:  weight = %.1f + %.3f x volume\n", cb[1] + cb[3], cb[2] + cb[4]))
cat(sprintf("\ninteraction p = %.3f  <- NOT significant\n",
            coef(summary(bk_int))["volume:coverpaperback", 4]))
cat(sprintf("adj R^2: %.3f (parallel) vs %.3f (free);  AIC: %.1f vs %.1f\n",
            glance(bk_par)$adj.r.squared, glance(bk_int)$adj.r.squared,
            glance(bk_par)$AIC, glance(bk_int)$AIC))
cat("VERDICT: letting the slopes move buys nothing. Lecture 5's parallel-slopes\n")
cat("model was right for these books -- an interaction is a claim you TEST.\n")

# ---- Figure: the two lines, freed --- and barely moving ---------------------
bk_grid <- expand_grid(volume = seq(min(books$volume), max(books$volume),
                                    length.out = 50),
                       cover  = factor(c("hardcover", "paperback"),
                                       levels = c("hardcover", "paperback"))) |>
  mutate(free     = predict(bk_int, newdata = pick(everything())),
         parallel = predict(bk_par, newdata = pick(everything())))

ggsave("figures/books_interaction.pdf",
       ggplot(books, aes(x = volume, y = weight, color = cover)) +
         geom_point(size = 2.4) +
         geom_line(data = bk_grid, aes(y = parallel, color = cover),
                   linetype = "dashed", linewidth = 0.7, alpha = 0.75) +
         geom_line(data = bk_grid, aes(y = free, color = cover), linewidth = 1.1) +
         scale_color_manual(values = c(hardcover = blue, paperback = red)) +
         labs(x = expression("Volume (cm"^3*")"), y = "Weight (g)", color = NULL,
              title = "Solid: slopes free.  Dashed: forced parallel (Lecture 5).") +
         theme_minimal(base_size = 24) +
         theme(legend.position = "bottom",
               plot.title = element_text(size = 11)),
       width = 6, height = 4.2)


cat("\n==============================================================\n")
cat("9. Interaction, case 2: does the mass-radius slope depend on the star?\n")
cat("==============================================================\n")

mod_1 <- lm(log(planet_radius) ~ log(planet_mass), data = exo)
mod_2 <- lm(log(planet_radius) ~ log(planet_mass) + stellar_mass, data = exo)
mod_3 <- lm(log(planet_radius) ~ log(planet_mass) * stellar_mass, data = exo)

cat("Three models for log(radius) -- same 890 planets, so AIC IS comparable:\n")
print(tibble(model  = c("log(mass)", "+ stellar_mass", "* stellar_mass"),
             adj_r2 = round(c(glance(mod_1)$adj.r.squared,
                              glance(mod_2)$adj.r.squared,
                              glance(mod_3)$adj.r.squared), 3),
             AIC    = round(c(glance(mod_1)$AIC, glance(mod_2)$AIC,
                              glance(mod_3)$AIC), 0)))

cat("\nThe interaction model, tidy():\n")
print(tidy(mod_3, conf.int = TRUE) |>
        mutate(across(where(is.numeric), ~ round(., 4))))

b3 <- coef(mod_3)[["log(planet_mass):stellar_mass"]]
b1 <- coef(mod_3)[["log(planet_mass)"]]
cat(sprintf("\nSlope of log(mass) = %.3f + (%.3f) x stellar_mass\n", b1, b3))
qs <- quantile(exo$stellar_mass, c(0.1, 0.5, 0.9))
for (i in seq_along(qs)) {
  cat(sprintf("  stellar_mass = %.2f (%s): slope = %.3f\n",
              qs[i], names(qs)[i], b1 + b3 * qs[i]))
}
cat(sprintf("interaction p-value = %.4f\n",
            coef(summary(mod_3))["log(planet_mass):stellar_mass", 4]))

# ---- Figure: the interaction, made visible ---------------------------------
grid <- expand_grid(
  planet_mass  = exp(seq(log(min(exo$planet_mass)), log(max(exo$planet_mass)),
                         length.out = 100)),
  stellar_mass = qs
) |>
  mutate(fit         = predict(mod_3, newdata = pick(everything())),
         star = factor(round(stellar_mass, 2),
                       labels = c("light star (10th pct)",
                                  "median star (50th pct)",
                                  "heavy star (90th pct)")))

ggsave("figures/interaction_effect.pdf",
       ggplot(exo, aes(x = planet_mass, y = planet_radius)) +
         geom_point(alpha = 0.15, size = 1, color = "grey40") +
         geom_line(data = grid, aes(y = exp(fit), color = star), linewidth = 1.1) +
         scale_x_log10() + scale_y_log10() +
         scale_color_manual(values = c(blue, "#7B9E4C", red)) +
         labs(x = "Planet Mass (Earth masses, log scale)",
              y = "Planet Radius (Earth radii, log scale)", color = NULL,
              title = "The mass-radius slope tilts with the host star's mass") +
         theme_minimal(base_size = 24) +
         theme(legend.position = "bottom"),
       width = 6.5, height = 4.2)

pdf("figures/radius_interaction_diagnostics.pdf", width = 8, height = 3.5, pointsize = 20)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(mod_3, which = 1, cex = 0.5, caption = "", main = "Residuals vs Fitted", sub = "")
plot(mod_3, which = 2, cex = 0.5, caption = "", main = "Normal Q-Q", sub = "")
invisible(dev.off())

cat("\nAll figures written to figures/\n")
