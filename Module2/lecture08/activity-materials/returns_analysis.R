library(tidyverse)
library(broom)
library(scales)

##############
### Part 1 ###
##############

### Task 1A: load data and take a look at it 

df <- read_csv("returns.csv")

# TODO: examine the data (e.g., with glimpse, View, etc.)

### Task 1B: compute overall return rate

# TODO: compute the rate at which items are returned 

### Task 1C: EDA 

# Example 1: histogram of return rates across categories 
df %>%
  group_by(category) %>%
  summarise(return_rate = mean(returned), n = n()) %>%
  ggplot(aes(x = category, y = return_rate)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) + 
  labs(y = "Return rate")

# Example 2: return rates as a function of shipping time 
  ggplot(df,aes(x = shipping_days, y = returned)) +
  geom_jitter(height = 0.05, alpha = 0.1) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial),
    se = FALSE
  ) +
  scale_y_continuous(labels = percent_format())

# TODO: make at least two more exploratory plots 

##############
### Part 2 ###
##############

### Task 2A: Fit baseline model  

# TODO: add additional predictors to the model 
modBase <- glm(
  returned ~ price + shipping_days + category,
  data = df,
  family = binomial
)
print(modBase)

### Task 2B: Interpretation 

# TODO: compute the odds ratios 
# HINT: use `modBase$coefficients` to get a list of the coefficients, which you can then transform 
# using the `exp` function so they can be interpreted as odds ratios 
exp(modBase$coefficients)

### Task 2C: Prediction 

# Example: How does shipping days affect return rates for a "typical" product 
new_data <- tibble(
  price = median(df$price),
  discount_pct = median(df$discount_pct),
  shipping_days = c(2, 6),
  late_delivery = 0, 
  category = "Apparel",
  membership = "None",
  prior_returns = median(df$prior_returns),
  prior_purchases = median(df$prior_purchases),
  device = "Mobile",
  region = "NE"
)

# TODO: use `augment` to make predictions
# HINT: don't forget to include the newdata and type.predict arguments 
preds = augment(...)  # replace 
print(preds)

# TODO: create new_data2, which is like new_data but with a different predictor varying. 
# Then predict the return rates for new_data2 using `augment`

##############
### Part 3 ###
##############

### Task 3A: Investigating a hypothesis about interactions 

# TODO: Check the plausibility the following hypothesis:
#
# (HA) Shipping delays matter more for Apparel/Shoes than for Electronics/Home/Beauty.
#
# Fit a logistic regression model that includes an interaction term between `shipping_days` and `category`.
# Use your results from Part 2 to decide which other predictors to include.
modHA <- glm(...)

# TODO: use the model fit to determine if the data provides support for the hypothesis 

### Task 3B: Checking model fit 

# Check if the model is calibrated 
check <- df %>%
  mutate(p = predict(modHA, type = "response"),
         bin = ntile(p, 10)) %>%
  group_by(bin) %>%
  summarise(mean_p = mean(p), obs = mean(returned), n = n())

ggplot(check, aes(x = mean_p, y = obs, size = n)) +
  geom_point(alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Mean predicted probability", y = "Observed return rate")

### Task 3C: Investigate another hypothesis.

### TODO: Repeat the previous two tasks, but for the following hypothesis :
#
# (HB) discounts change return behavior diﬀerently by membership level.


