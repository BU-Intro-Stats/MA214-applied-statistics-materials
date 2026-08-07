# Four practice scatterplots (A-D) for the "reading scatterplots" activity.
# Run from this directory: Rscript lecture3_scatter_panels.R
library(tidyverse)

set.seed(214)
n <- 60

panels <- bind_rows(
  tibble(panel = "A", x = runif(n, 0, 10)) |>
    mutate(y = 2 + 1.1 * x + rnorm(n, sd = 1.1)),
  tibble(panel = "B", x = runif(n, 0, 10)) |>
    mutate(y = 28 - 1.4 * x + rnorm(n, sd = 4.2)) |>
    add_row(panel = "B", x = 9.4, y = 34),
  tibble(panel = "C", x = runif(n, 0, 10)) |>
    mutate(y = 3 + 2.4 * (x - 5)^2 / 5 + rnorm(n, sd = 1.1)),
  tibble(panel = "D", x = runif(n, 0, 10)) |>
    mutate(y = 12 + rnorm(n, sd = 3.4))
)

p <- ggplot(panels, aes(x, y)) +
  geom_point(color = "#569BBD", alpha = 0.75, size = 1.6) +
  facet_wrap(~panel, scales = "free_y") +
  labs(x = "x", y = "y") +
  theme_minimal(base_size = 17) +
  theme(strip.text = element_text(face = "bold", size = 14))

ggsave("figures/lecture3_scatter_panels.png", p, width = 7.2, height = 5.2, dpi = 150)
cat("wrote figures/lecture3_scatter_panels.png\n")
