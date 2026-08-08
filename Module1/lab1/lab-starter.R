# ============================================================
# Lab 1: Working with R and Data
# MA 214 Applied Statistics
# ============================================================
# Submit this R file to Gradescope.
# Do not use setwd().

library(ggplot2)
library(dplyr)
library(readr)

# Part 1: Import and inspect data
flights <- read.csv("flights.csv")

# TODO: inspect the first rows, dimensions, names, and structure


# Part 2: Summaries and counts
# TODO: count flights arriving in California


# TODO: count flights arriving in Florida


# TODO: count flights departing from JFK, LGA, and EWR
JFK <- NA
LGA <- NA
EWR <- NA

# TODO: count distinct destination airports
destinations <- NA

# TODO: summarize air_time by month in minutes


# TODO: summarize air_time by month in hours


# Part 3: Visualizations
# TODO: create and save at least one plot

