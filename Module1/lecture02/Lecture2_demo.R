# ---- 0. Set up and load libraries, if any ----

library(tidyverse)

# Set the working directory:
if(!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(getSourceEditorContext()$path))
# (you probably won't need to use this yourselves)

# ---- 1. Load data ----

# Load the .csv data file into dataframe:
lax_flights <- read.csv("../lecture-1/data/lax.csv")

# glimpse the data 
View(lax_flights)


# ---- 2. Bar Plots ----

# Bar plot of day of week:
ggplot(data=lax_flights, aes(x = DayOfWeek)) +
  geom_bar() +
  xlab("Day of Week") +
  ylab("Number of Flights")

# Make DayOfWeek a factor
lax_flights <- lax_flights |>
  mutate(
    DayOfWeek = factor(
      DayOfWeek,
      levels = 1:7,
      labels = c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
      )
    )
  )

# Remake bar plot 
ggplot(data=lax_flights, aes(x = DayOfWeek)) +
  geom_bar() +
  xlab("Day of Week") +
  ylab("Number of Flights")


# Make Month a factor as well
lax_flights <- lax_flights |>
  mutate(
    Month = factor(
      Month,
      levels = 1:12,
      labels = c(
        "January", "February", "March",
        "April", "May", "June",
        "July", "August", "September",
        "October", "November", "December"
      )
    )
  )

ggplot(data=lax_flights, aes(x = Month)) +
  geom_bar() +
  xlab("Month") +
  ylab("Number of Flights")


# ---- 2. Histograms and Densities ----

# Arrival delays distribution
ggplot(data=lax_flights, aes(x = ArrDelay)) +
  geom_histogram() +
  xlab("Arrival Delay (minutes)") +
  ylab("Number of Flights")

# Use log scale for counts 
ggplot(data=lax_flights, aes(x = ArrDelay)) +
  geom_histogram(bins = 40) + # try adding bins = 100 option
  xlab("Arrival Delay (minutes)") +
  ylab("Number of Flights") +
  scale_y_log10()

# Split into facets by airline  
ggplot(data=lax_flights, aes(x = ArrDelay, fill = UniqueCarrier)) +
  geom_histogram() + # try adding bins = 100 option
  xlab("Arrival Delay (minutes)") +
  ylab("Number of Flights") +
  scale_y_log10() +
  facet_wrap(~ UniqueCarrier) 

# Use a density plot instead 
ggplot(data=lax_flights, aes(x = ArrDelay, fill = UniqueCarrier)) +
  geom_density(alpha = .5) +
  xlab("Arrival Delay (minutes)") +
  ylab("Number of Flights")
  
# Remove outliers
lax_flights_no_outliers = lax_flights |> filter(ArrDelay < 120)
ggplot(data=lax_flights_no_outliers, aes(x = ArrDelay, fill = UniqueCarrier)) +
  geom_density(alpha = .5) +
  xlab("Arrival Delay (minutes)") +
  ylab("Number of Flights")
  
# Flight time
ggplot(data=lax_flights, aes(x = ActualElapsedTime)) +
  geom_histogram() +
  xlab("Flight Time (minutes)") +
  ylab("Number of Flights")

# Split into facets by airline 
ggplot(data=lax_flights, aes(x = ActualElapsedTime, fill = UniqueCarrier)) +
  geom_histogram() +
  xlab("Flight Time (minutes)") +
  ylab("Number of Flights") +
  facet_wrap(~ UniqueCarrier)

# 
ggplot(data=lax_flights, aes(x = DepTime)) +
  geom_histogram(bins = 50) + 
  xlab("Departure Time (minutes from midnight") +
  ylab("Number of Flights")

# ---- 2. Boxplots ----

ggplot(data=lax_flights, aes(x = UniqueCarrier, y = ArrDelay)) +
  geom_boxplot() +
  xlab("Carrier") +
  ylab("Arrival Delay")

ggplot(data=lax_flights_no_outliers, aes(x = UniqueCarrier, y = ArrDelay)) +
  geom_boxplot() +
  xlab("Carrier") +
  ylab("Arrival Delay")
