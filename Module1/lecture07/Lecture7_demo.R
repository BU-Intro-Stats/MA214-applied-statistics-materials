# Lecture 7: Model Selection -- in-class demo
# Reproduces every number quoted on the slides.
# Run from this directory:  Rscript Lecture7_demo.R
#
# Donner Party data are case2001 in Sleuth3:  install.packages("Sleuth3")

library(tidyverse)

# =========================================================================
# 1. Model selection with AIC: multiple logistic regression (Donner Party)
# =========================================================================
if (!requireNamespace("Sleuth3", quietly = TRUE)) {
  stop("Sleuth3 is not installed. Run install.packages('Sleuth3').")
}

donner <- Sleuth3::case2001 |>
  mutate(Status = factor(Status, levels = c("Died", "Survived")),
         Sex    = relevel(factor(Sex), ref = "Male"))
cat("n =", nrow(donner), "\n")

# Four candidate models for survival.
cands <- list(
  "null"      = glm(Status ~ 1,         data = donner, family = binomial),
  "Age"       = glm(Status ~ Age,       data = donner, family = binomial),
  "Sex"       = glm(Status ~ Sex,       data = donner, family = binomial),
  "Age + Sex" = glm(Status ~ Age + Sex, data = donner, family = binomial)
)

cat("\n=== AIC = -2 log(L) + 2k ===\n")
tab <- map_dfr(names(cands), function(nm) {
  m <- cands[[nm]]
  k <- length(coef(m))
  tibble(model = nm, k = k,
         logLik = as.numeric(logLik(m)),
         AIC_by_hand = -2 * as.numeric(logLik(m)) + 2 * k,
         AIC_from_R  = AIC(m))
})
print(tab, n = Inf)
cat("\n--> lowest AIC wins:", tab$model[which.min(tab$AIC_from_R)], "\n")

# The same comparison, the way you would actually do it in R:
cat("\n=== AIC(...) on the candidates ===\n")
print(AIC(cands[["Age"]], cands[["Sex"]], cands[["Age + Sex"]]))

# Coefficients of the selected model (quoted in Lecture 6 and recalled here).
cat("\n=== selected model: Status ~ Age + Sex ===\n")
print(round(coef(summary(cands[["Age + Sex"]])), 4))

# =========================================================================
# 2. Multicollinearity: the state poverty data
# =========================================================================
pov <- read.table("data/poverty.txt", header = TRUE, sep = "\t") |>
  rename(poverty = Poverty, white = White,
         female_house = PercentFemaleHouseholderNoHusbandPresent)

cat("\n\n=== are the two predictors collinear? ===\n")
cat("cor(female_house, white) =",
    round(cor(pov$female_house, pov$white), 3), "\n")

m1 <- lm(poverty ~ female_house, data = pov)
m2 <- lm(poverty ~ female_house + white, data = pov)

cat("\n=== M1: poverty ~ female_house ===\n")
print(round(coef(summary(m1)), 3))
cat("\n=== M2: poverty ~ female_house + white ===\n")
print(round(coef(summary(m2)), 3))

# The tell-tale sign of collinearity: the standard error INFLATES.
se1 <- coef(summary(m1))["female_house", "Std. Error"]
se2 <- coef(summary(m2))["female_house", "Std. Error"]
cat(sprintf("\n--> female_house SE: %.3f  ->  %.3f  (+%.0f%%)\n",
            se1, se2, (se2 / se1 - 1) * 100))
cat(sprintf("--> and its estimate moved: %.2f -> %.2f\n",
            coef(m1)[["female_house"]], coef(m2)[["female_house"]]))

# Variance inflation factor: VIF = 1/(1 - R^2) of one predictor on the others.
r <- cor(pov$female_house, pov$white)
cat(sprintf("--> VIF = 1/(1 - r^2) = 1/(1 - %.3f^2) = %.2f\n", r, 1 / (1 - r^2)))
