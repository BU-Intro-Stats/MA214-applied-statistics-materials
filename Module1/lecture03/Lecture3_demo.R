# ---- 0. Set up and load libraries, if any ----

library(tidyverse)

# Set the working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))
# (you probably won't need to use this yourselves)

# ---- 1. Load data ----

# Load the .csv data file into data frame:
lax_flights <- read.csv("../lecture-1/data/lax.csv")

# glimpse the data 
glimpse(lax_flights)
