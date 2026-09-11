# ============================================================
# MA 214 Lab 1 - TA worked solutions
# Instructor reference: lab-ta-guide.md. Not the student starter.
# Uses all 336,776 records and the 19 original package variables.
# Run interactively, or: Rscript lab-ta-solutions.R
# No installation, downloads, or file writes occur when sourced.
# ============================================================
library(nycflights13)
library(dplyr)
library(ggplot2)
data(flights)

# 1. Inspect, select, and filter.
print(head(flights))
print(dim(flights))
print(names(flights))
glimpse(flights)
airport_counts <- flights %>% count(origin, sort = TRUE)
print(airport_counts)

selected_flights <-
  flights %>%
  select(origin, dest, arr_delay) %>%
  filter(origin == "JFK")

glimpse(selected_flights)

selected_flights <-
  flights %>%
  select(origin, dest, arr_delay) %>%
  filter(origin == "EWR")

glimpse(selected_flights)

selected_flights <-
  flights %>%
  select(origin, dest, arr_delay) %>%
  filter(origin == "LGA")

glimpse(selected_flights)



# One row is a 2013 flight record from EWR, JFK, or LGA (336,776 rows,
# 19 columns). EWR has the most records: 120,835.
# origin/dest/carrier are categorical codes; flight is an identifier even
# though stored as a number. hour is scheduled local departure hour.
# arr_delay and air_time are in minutes; distance is in miles.

# 2. Counts include all flight records, including missing actual departures.
chosen_airport <- "JFK"
hour_counts <- flights %>%
  filter(origin == chosen_airport) %>%
  count(hour, name = "n_flights") %>%
  arrange(desc(n_flights))
busiest_hours <- flights %>%
  count(origin, hour, name = "n_flights") %>%
  group_by(origin) %>%
  filter(n_flights == max(n_flights)) %>% # Retains ties.
  ungroup()
print(busiest_hours)
# EWR: 06:00-06:59, 11,133 flights; JFK: 08:00-08:59, 10,780 flights;
# LGA: 06:00-06:59, 8,558 flights. No ties for these origins.
# Students compare any two origins and evaluate their own predictions.
# Combining the year hides daily and seasonal variation. Counts of flight
# records do not give passenger counts. dep_time is actual HHMM time, so
# its histogram differs from scheduled-hour counts and omits missing times.
hour_plot <- ggplot(hour_counts, aes(hour, n_flights)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(title = paste("Scheduled departures from", chosen_airport),
       x = "Scheduled departure hour (local time)",
       y = "Flight count in 2013")

# 3. How long is the delay?
airport_delays <- flights %>%
  group_by(origin) %>%
  summarize(
    n_observed = sum(!is.na(arr_delay)),
    mean_delay = mean(arr_delay, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_delay))
print(airport_delays)

# Example response: "EWR has the greatest average arrival delay:
# 9.11 minutes, compared with 5.78 minutes at LGA and 5.55 minutes at JFK."
# Missing arrival delays are excluded, not treated as zero.
# The mean includes negative delays (early arrivals).

# 4. Can a late departure still arrive on time?
recovery <- flights %>%
  filter(dep_delay > 30, !is.na(arr_delay)) %>%
  summarize(
    n_observed = n(),
    n_recovered = sum(arr_delay <= 0),
    pct_recovered = 100 * n_recovered / n_observed
  )
print(recovery)
# 458 / 47,905 * 100 = about 0.956%.
# Example: "458 of 47,905 flights with observed arrival delays arrived
# on time or early, about 0.956%."
# Excludes 386 missing arrival delays among 48,291 departures >30 min late.
# Missing arrival delay is not proof of failing to catch up.

# 5. Late leaving, late arriving?
delay_plot <- ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.1, na.rm = TRUE) +
  labs(x = "Departure delay (minutes)", y = "Arrival delay (minutes)")
# Expected: a strong positive relationship. Flights with greater departure
# delays generally have greater arrival delays, with variation around
# the overall pattern. This concerns lateness, not time of day.
# Each point is a flight with both delays observed. No grouping is needed.
# Flights with dep_delay > 30 and arr_delay <= 0 connect to Question 4.

# 6. What else would you like to find out?
# Example question: "Which month has the greatest average departure delay?"
# Variables: month and dep_delay.
# Accept any question answerable using the data with appropriate variables.
# Students only propose a question; no additional code or answer is required.

if (interactive()) {
  print(hour_plot)
  print(delay_plot)
}
