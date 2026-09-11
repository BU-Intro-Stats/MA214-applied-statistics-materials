# Lab 1: TA preparation and answer notes

Use [lab-ta-solutions.R](lab-ta-solutions.R) for worked answers to all six questions. Distribute [lab-starter.R](lab-starter.R) and [lab-activity.pdf](lab-activity.pdf) to students. The starter provides worked examples for students to explain and adapt. Questions 3-5 compare airport means, ask whether late departures can arrive on time, and explore departure delay versus arrival delay with a scatterplot. Question 6 asks students to propose their own question.

The lab uses `nycflights13::flights` directly. The CSV files from earlier preparation are not needed. Install `nycflights13`, `dplyr`, and `ggplot2` once if missing, then load them each session. Run `?flights` in the Console. The solution script prints the answer tables and creates the hourly flight-count plot and departure-delay/arrival-delay scatterplot; sourcing it interactively displays both plots.

Suggested pacing is in [lab1_plan.md](lab1_plan.md). Complete Questions 1-6. Question 5 asks for a plot sketch and 1-2 sentences; Question 6 needs only a proposed question and variable names. Students submit the activity worksheet; the Tutorial 1 hash remains a separate requirement under the course schedule. The script is working code, not a separate submission.

## Getting started and Section 1

Installation places a package on the computer; `library()` makes it available in the session. `data(flights)` loads the named packaged data set. The help page explains context and units that cannot reliably be inferred from a few rows. The data have **336,776 rows and 19 columns**, covering 2013 flight records from EWR, JFK, and LGA. One row is a flight record, including records with missing actual departure or arrival information.

| Variable | Teaching interpretation |
| --- | --- |
| `origin`, `dest` | Categorical airport codes. `dest` is an airport, not a state. |
| `carrier`, `flight` | Airline code and flight-number identifier. Numeric storage does not make the identifier a quantitative measurement. |
| `hour` | Scheduled local departure hour, a discrete time-of-day value; grouping treats hours as categories. |
| `arr_delay` | Quantitative arrival delay in minutes, negative when early. |
| `air_time`, `distance` | Quantitative measurements in minutes and miles. |

`count(origin, sort = TRUE)` counts rows by origin and sorts the largest count first. EWR has 120,835 records, JFK 111,279, and LGA 104,662. `select()` chooses columns; `filter()` chooses rows. `==` tests equality; `<-` assigns a name. Let students explain these differences aloud.

## Section 2: busiest scheduled hour

| Airport | Peak hour (local time) | Flight records in that hour across 2013 |
| --- | --- | ---: |
| EWR | 06:00-06:59 | 11,133 |
| JFK | 08:00-08:59 | 10,780 |
| LGA | 06:00-06:59 | 8,558 |

There are no tied peaks for these origins. The solution retains ties if an adapted analysis produces them. Counts include records with missing actual departure times. The plot summarizes scheduled flights, not actual takeoffs, passengers, terminal crowding, or one particular day. A histogram of raw `dep_time` describes actual HHMM values; HHMM is not a uniform minute scale, and missing departures are omitted. Annual aggregation hides day-to-day and seasonal variation.

## Section 3: one airport-delay comparison

Students enter and run the code printed under Question 3 in the worksheet to identify the origin with the greatest mean arrival delay and report the mean for each airport. The starter contains a placeholder for their code; the TA solution retains the complete implementation. No prediction or extended interpretation is required.

| Origin | Observed arrival delays | Mean arrival delay (minutes) |
| --- | ---: | ---: |
| EWR | 117,127 | 9.11 |
| LGA | 101,140 | 5.78 |
| JFK | 109,079 | 5.55 |

Example: "EWR has the greatest average arrival delay: 9.11 minutes, compared with 5.78 minutes at LGA and 5.55 minutes at JFK."

The mean includes early, on-time, and late observed arrivals. `na.rm = TRUE` excludes missing values; it does not replace them with zero. `sum(!is.na(arr_delay))` counts observed delays. Missing arrivals may differ systematically, so excluding them does not guarantee an unbiased comparison. These are TA discussion notes, not extra written requirements.

## Section 4: can a late departure still arrive on time?

Students enter and run the code printed under Question 4 in the worksheet. The starter contains a placeholder for their code; the TA solution retains the complete implementation.

Among departures more than 30 minutes late, **458 of 47,905 flights with observed arrival delays arrived on time or early: about 0.956%**. Students report the count and percentage; a brief numerical answer is sufficient.

The filter requires both `dep_delay > 30` and a nonmissing arrival delay. This excludes 386 missing arrival delays among 48,291 late departures. `arr_delay <= 0` includes both early and exactly on-time arrivals. `sum()` counts TRUE conditions; `n()` counts retained rows. The denominator is neither the full data set nor all late departures including missing arrivals. Check that students distinguish 0.956% from 95.6%; missing arrival delay does not prove that a flight failed to catch up.

## Section 5: late leaving, late arriving?

Plot departure delay on the x-axis and arrival delay on the y-axis, both in minutes. Use the flights directly; no grouping or summary calculations are needed. Each point represents one flight with both delays observed. Transparent points reduce overplotting, and `na.rm = TRUE` omits missing delay pairs.

Expected description: "There is a strong positive relationship: flights with greater departure delays generally have greater arrival delays. Arrival delays still vary for similar departure delays." This refers to lateness, not time of day. Extreme delays stretch the axes and cluster many points near zero; students need only identify the overall pattern. Points with departure delay above 30 minutes and arrival delay at or below zero are the flights that caught up in Question 4. Require labeled axes, a sketch, and 1-2 sentences; no fitted line or correlation calculation is needed.

## Section 6: what else would you like to find out?

Accept one question that can be investigated with the available data and suitable variable names. For example: "Which month has the greatest average departure delay?" uses `month` and `dep_delay`. Students do not need to carry out another analysis. Questions about ticket prices or passenger counts would require additional data.

Reference: [flights variable documentation](https://nycflights13.tidyverse.org/reference/flights.html). Numerical answers were computed with the accompanying R script.
