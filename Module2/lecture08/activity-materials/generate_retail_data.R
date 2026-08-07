# Originally generated with ChatGPT
# Simulate an "online retailer returns" dataset with:
# - real signal (shipping delays, prior returns, price/discount, category)
# - confounding (category ↔ price; membership ↔ prior purchases; membership ↔ shipping)
# - a real interaction (shipping_days × category)
# - a couple distractors (device, region)

library(tidyverse)

simulate_returns <- function(n = 5000, seed = 214) {
  set.seed(seed)
  
  # --- Categorical structure ---
  category_levels <- c("Apparel", "Shoes", "Electronics", "Home", "Beauty")
  region_levels   <- c("NE", "MW", "S", "West")
  device_levels   <- c("Desktop", "Mobile")
  member_levels   <- c("None", "Silver", "Gold")
  
  category <- sample(category_levels, n, replace = TRUE,
                     prob = c(0.28, 0.14, 0.20, 0.22, 0.16))
  region <- sample(region_levels, n, replace = TRUE,
                   prob = c(0.23, 0.24, 0.30, 0.23))
  device <- sample(device_levels, n, replace = TRUE, 
                   prob = c(0.45, 0.55))
  
  # Latent "customer intensity" (drives membership and purchase volume)
  cust_intensity <- rnorm(n)
  
  # Membership depends on intensity (confounding: members buy more & return less)
  p_gold   <- plogis(-1.0 + 1.0 * cust_intensity)
  p_silver <- plogis(-0.2 + 0.8 * cust_intensity) * (1 - p_gold)

d %>% summarise(return_rate = mean(returned))

d %>%
  ggplot(aes(x = shipping_days, y = returned)) +
  geom_jitter(height = 0.05, alpha = 0.1) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE)
  u <- runif(n)
  membership <- case_when(
    u < p_gold ~ "Gold",
    u < p_gold + p_silver ~ "Silver",
    TRUE ~ "None"
  )
  membership <- factor(membership, levels = member_levels)
  
  # Prior purchases depends strongly on intensity + membership
  base_purch <- exp(1.2 + 0.7 * cust_intensity +
                      ifelse(membership == "Silver", 0.25, 0) +
                      ifelse(membership == "Gold", 0.55, 0))
  prior_purchases <- rpois(n, lambda = pmin(base_purch, 80))
  
  # Prior returns depends on purchases + intensity (some people return more)
  # Keep it plausible: return-prone customers exist.
  return_prone <- rnorm(n, sd = 0.7)
  prior_returns <- rpois(
    n,
    lambda = pmax(0.1,
                  0.05 * prior_purchases * exp(0.35 * return_prone))
  )
  
  # --- Price is category-dependent (confounding: category ↔ price) ---
  cat_price_mu <- case_when(
    category == "Apparel"     ~ log(45),
    category == "Shoes"       ~ log(85),
    category == "Electronics" ~ log(220),
    category == "Home"        ~ log(120),
    category == "Beauty"      ~ log(35)
  )
  price <- rlnorm(n, meanlog = cat_price_mu, sdlog = 0.35)
  price <- pmin(price, 800)  # cap extreme outliers
  
  # Discount depends on category + device (mobile slightly more discount exposure)
  disc_base <- case_when(
    category == "Apparel"     ~ 0.22,
    category == "Shoes"       ~ 0.18,
    category == "Electronics" ~ 0.10,
    category == "Home"        ~ 0.14,
    category == "Beauty"      ~ 0.20
  )
  discount_pct <- pmin(
    pmax(rnorm(n, mean = disc_base + ifelse(device == "Mobile", 0.03, 0), sd = 0.10), 0),
    0.70
  )
  
  # --- Shipping: depends on region + membership (confounding: membership ↔ shipping) ---
  # baseline shipping days by region
  ship_mu <- case_when(
    region == "NE"   ~ 2.6,
    region == "MW"   ~ 3.2,
    region == "S"    ~ 3.6,
    region == "West" ~ 4.0
  )
  
  # membership gets faster shipping
  ship_mu <- ship_mu +
    ifelse(membership == "Silver", -0.35, 0) +
    ifelse(membership == "Gold",   -0.70, 0)
  
  # shipping variability (integer days 1..10)
  shipping_days <- round(rnorm(n, mean = ship_mu, sd = 1.15))
  shipping_days <- pmin(pmax(shipping_days, 1), 10)
  
  # Late delivery indicator: more likely as shipping_days increases
  late_delivery <- rbinom(n, 1, prob = plogis(-3.0 + 0.65 * shipping_days))
  
  # --- Optional: size mismatch flag (mostly apparel/shoes) ---
  # size_mismatch_prob <- case_when(
  #   category %in% c("Apparel", "Shoes") ~ 0.18,
  #   TRUE ~ 0.05
  # )
  # size_mismatch_flag <- rbinom(n, 1, prob = size_mismatch_prob)
  
  # --- True return probability model (logistic) ---
  # Big effects:
  # - prior_returns increases return propensity
  # - shipping_days increases returns, *especially* for Apparel/Shoes (interaction)
  # - discount increases returns a bit (impulse/low commitment)
  # - membership reduces returns (service, loyalty), even controlling for other stuff
  # Moderate:
  # - size mismatch increases returns a lot
  # - late_delivery increases returns
  # Small distractions:
  # - device/region have weak effects
  
  # scale a couple vars for stability
  log_price <- log(price)
  disc10 <- discount_pct * 10  # per 10% steps
  
  # interaction: shipping days hurts more in apparel/shoes
  ship_slope <- case_when(
    category == "Apparel" ~ 0.22,
    category == "Shoes" ~ 0.26,
    TRUE ~ 0.12
  )
  
  # base linear predictor
  eta <- -2.1 +
    0.55 * log1p(prior_returns) +
    ship_slope * shipping_days +
    0.12 * disc10 +
    0.10 * log_price +
    0.55 * late_delivery +
    # 0.85 * size_mismatch_flag +
    ifelse(membership == "Silver", -0.22, 0) +
    ifelse(membership == "Gold",   -0.45, 0) +
    ifelse(device == "Mobile", 0.06, 0) +
    ifelse(region == "West",  0.05, 0) +
    ifelse(region == "S",     0.03, 0)
  
  # Slightly lower returns for Electronics even after price (policy / hassle)
  eta <- eta + ifelse(category == "Electronics", -0.25, 0)
  
  p_return <- plogis(eta)
  returned <- rbinom(n, 1, p_return)
  
  tibble(
    returned,
    price = round(price, 2),
    discount_pct = round(discount_pct, 2),
    shipping_days,
    late_delivery,
    category = factor(category, levels = category_levels),
    device = factor(device, levels = device_levels),
    membership,
    prior_returns,
    prior_purchases,
    region = factor(region, levels = region_levels),
    # size_mismatch_flag
  )
}

# ---- Example usage ----
d <- simulate_returns(n = 6000, seed = 214)

# Quick sanity checks:
d %>% summarise(return_rate = mean(returned))
d %>% count(category) %>% mutate(p = n / sum(n))
d %>% count(membership) %>% mutate(p = n / sum(n))

# Write to CSV for students
write_csv(d, "returns.csv")