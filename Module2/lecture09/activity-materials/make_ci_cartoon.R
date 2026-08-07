# Make a simple CI "cartoon" figure for slides (ci_cartoon.png)
# Shows many CIs; some miss the true parameter.

library(tidyverse)

set.seed(1)

# "Population" (skewed on purpose)
pop <- rgamma(200000, shape = 6, rate = 0.25)  # mean = shape/rate = 24

true_mu <- mean(pop)
n <- 35
B <- 20
ci_width = 0.8
crit_val = -qnorm((1 - ci_width) / 2)

cis <- map_dfr(1:B, function(b){
  x <- sample(pop, size = n, replace = FALSE)
  xbar <- mean(x)
  se <- sd(x) / sqrt(n)
  # Normal approx CI just for the cartoon (bootstrap is the lecture topic)
  tibble(
    iter = b,
    lower = xbar - crit_val * se,
    upper = xbar + crit_val * se,
    covers = (xbar - crit_val * se <= true_mu) & (true_mu <= xbar + crit_val * se)
  )
})

p <- ggplot(cis, aes(y = iter)) +
  geom_segment(aes(x = lower, xend = upper, yend = iter, linetype = covers)) +
  geom_vline(xintercept = true_mu) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Repeated 80% CIs: about 80% cover the true mean",
       x = "Parameter value") +
  # shown at 0.55\textwidth: base_size ~ 10 * 6.5 / (4.33 * 0.55)
  theme_minimal(base_size = 27)

ggsave("../figures/ci_cartoon.png", p, width = 6.5, height = 4.2, dpi = 200)
p
