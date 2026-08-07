# ---- 0. Set up and load libraries, if any ----

library(tidyverse)

# Run this script from the lecture directory, e.g.
#   Rscript lecture1_demo.R
# In RStudio: Session > Set Working Directory > To Source File Location.
# (The paths below are relative to this file's directory.)

# ---- 1. Load data ----

# Load the .csv data file into dataframe:
lax_flights <- read.csv("data/lax.csv")

# glimpse the data 
glimpse(lax_flights)

# or, print out the initial rows (more output)
print(lax_flights)

# or, view it in RStudio's data viewer (interactive sessions only, so that
# running this file with Rscript still works)
if (interactive()) View(lax_flights)

# ---- 2. Plot relationships between some of the variables 

# distance vs elapsed time 
ggplot(lax_flights, aes(x = Distance, y = ActualElapsedTime)) +
  geom_jitter(width = 5, height = 0, alpha = 0.1)

# color by airline
ggplot(lax_flights, aes(x = Distance, y = ActualElapsedTime, color = UniqueCarrier)) +
  geom_jitter(width = 5, height = 0, alpha = 0.3)

# distance vs arrival delay
ggplot(lax_flights, aes(x = Distance, y = ArrDelay, color = UniqueCarrier)) +
  geom_jitter(width = 5, height = 0, alpha = 0.3)

