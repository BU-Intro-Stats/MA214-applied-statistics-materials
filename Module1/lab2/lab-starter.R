# ============================================================
# Lab 2: Regression Models
# MA 214 Applied Statistics
# ============================================================
# Do not use setwd().

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Part 1: Simple linear regression with mtcars
mtcars_df <- read.csv("mtcars.csv")

# TODO: inspect mtcars_df


# TODO: scatter plot of mpg versus wt


# TODO: correlation between mpg and wt


# TODO: fit and summarize mpg ~ wt
model_wt <- NULL


# Part 2: Diagnostics and model comparison
# TODO: residual plot for model_wt


# TODO: fit mpg ~ hp and mpg ~ qsec
model_hp <- NULL
model_qsec <- NULL


# TODO: compare models using R-squared


# Part 3: Multiple regression
# TODO: fit a multiple regression model predicting mpg
model_multi <- NULL


# Post-lab/autograder objects
df1 <- read.csv("lab2_data.csv")

# TODO: identify best simple predictor among a, b, c, d
best_fit <- NA

# TODO: fit selected model and save coefficients
b0 <- NA
b1 <- NA

# TODO: compute fitted values and save the 5th fitted value
fitted_5 <- NA

# TODO: save the 10th outlier as requested in the lab instructions
outlier_10 <- NA

df2 <- read.csv("lab2_data2.csv")

# TODO: identify best multiple regression model among A, B, C
best_fit2 <- NA
