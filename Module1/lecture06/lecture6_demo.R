# Lecture 6: Logistic Regression -- in-class demo
# Reproduces every model quoted on the slides (Donner Party).
# Run from this directory:  Rscript Lecture6_demo.R
#
# The Donner Party data are case2001 in the Sleuth3 package:
#   install.packages("Sleuth3")

library(tidyverse)

if (!requireNamespace("Sleuth3", quietly = TRUE)) {
  stop("Sleuth3 is not installed. Run install.packages('Sleuth3') and try again.")
}

donner <- Sleuth3::case2001            # 45 adults: Age, Sex, Status
cat("n =", nrow(donner), "\n\n")

# Success = Survived. Make Male the reference level for Sex, so the coefficient
# software reports is SexFemale -- which is what the slides show.
donner <- donner |>
  mutate(Status = factor(Status, levels = c("Died", "Survived")),
         Sex    = relevel(factor(Sex), ref = "Male"))

# ---- EDA ----------------------------------------------------------------
cat("=== survival by sex ===\n")
print(table(donner$Status, donner$Sex))

# ---- Model 1: age only --------------------------------------------------
# Slides: log-odds = 1.8185 - 0.0665 * Age
m1 <- glm(Status ~ Age, data = donner, family = binomial)
cat("\n=== Model 1: Status ~ Age ===\n")
print(round(coef(summary(m1)), 4))

# ---- Model 2: age + sex -------------------------------------------------
# Slides: log-odds = 1.6331 - 0.0782*Age + 1.5973*Female
m2 <- glm(Status ~ Age + Sex, data = donner, family = binomial)
cat("\n=== Model 2: Status ~ Age + Sex ===\n")
print(round(coef(summary(m2)), 4))

# ---- Coefficients as odds ratios ---------------------------------------
# Exponentiate: e^beta is the multiplicative effect on the ODDS of survival.
cat("\n=== odds ratios: exp(coef) ===\n")
print(round(exp(coef(m2)), 4))
cat("Age      : each extra year multiplies the odds by",
    round(exp(coef(m2)[["Age"]]), 3),
    sprintf("(a %.1f%% drop per year)\n", (1 - exp(coef(m2)[["Age"]])) * 100))
cat("10 years : odds multiplied by", round(exp(coef(m2)[["Age"]] * 10), 3), "\n")
cat("SexFemale: women had", round(exp(coef(m2)[["SexFemale"]]), 2),
    "times the odds of survival of men the same age\n")

# ---- Prediction: log odds -> probability --------------------------------
# Slides: a 25-year-old man has p ~= 0.42
newdata <- data.frame(Age = 25, Sex = factor("Male", levels = levels(donner$Sex)))
eta <- predict(m2, newdata, type = "link")        # log odds
p   <- predict(m2, newdata, type = "response")    # probability
cat(sprintf("\n25-year-old man: eta = %.3f  ->  p = %.3f\n", eta, p))
cat("(check: exp(eta)/(1+exp(eta)) =", round(exp(eta) / (1 + exp(eta)), 3), ")\n")

# ---- Classification: threshold and confusion matrix ---------------------
# The model gives probabilities; a THRESHOLD turns them into labels.
p_hat <- predict(m2, type = "response")
for (thr in c(0.5, 0.75)) {
  pred <- factor(ifelse(p_hat >= thr, "Survived", "Died"),
                 levels = c("Died", "Survived"))
  cat(sprintf("\n=== threshold %.2f ===\n", thr))
  print(table(Predicted = pred, Actual = donner$Status))
  cat("accuracy:", round(mean(pred == donner$Status), 3), "\n")
}
