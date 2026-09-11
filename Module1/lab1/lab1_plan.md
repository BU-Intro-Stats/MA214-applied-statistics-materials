# Lab 1: Working with R and Data

**Week:** 1

**Student materials:** `lab-activity.pdf`, `lab-starter.R`; the `flights` data set from `nycflights13` (no CSV required).

**TA materials:** `lab-ta-solutions.R`, `lab-ta-guide.md`.

Suggested pacing for a 60-minute session; adjust to the section length. Students complete six questions: explore the data, find the busiest hour, compare average arrival delays, determine whether late departures can arrive on time, plot departure delay against arrival delay, and propose one question of their own.

| Minutes | Activity |
| --- | --- |
| 0-10 | Introductions; install packages if needed, load data, open `?flights`; distinguish installing from loading. |
| 10-20 | Inspect rows, columns, types, units, and missing values; explain a pipeline. |
| 20-35 | Predict and find the busiest scheduled departure hour at two airports; make a bar plot. |
| 35-43 | Enter the code from Question 3 in the worksheet into the script; report each airport's mean arrival delay and identify the greatest. |
| 43-50 | Enter the code from Question 4 in the worksheet into the script; calculate how many departures more than 30 minutes late arrive on time or early and report the count and percentage. |
| 50-57 | Make a scatterplot of departure delay and arrival delay; describe the pattern in 1-2 sentences. |
| 57-60 | Propose one question and name the variables needed; check responses. No additional analysis is required. |

Emphasize that `hour` is scheduled local departure hour, mean arrival delay excludes missing values and includes early arrivals, and a ranking does not establish a cause. Keep observed group sizes next to the means.

For Question 4, the denominator is the number of departures more than 30 minutes late with an observed arrival delay. Count on-time or early arrivals using `arr_delay <= 0`.

Students keep the R script as working notes. Deliverables remain the Tutorial 1 hash and completed activity response sheet, using the Lab 1 deadline exception in the course schedule.

Data reference: [nycflights13 flights documentation](https://nycflights13.tidyverse.org/reference/flights.html).
