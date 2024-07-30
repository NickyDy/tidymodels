library(tidyverse)
youtube <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv")

youtube %>%
  select(year, funny:use_sex) %>%
  pivot_longer(funny:use_sex) %>%
  group_by(year, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(year, prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "% of commercials")

# Fit a simple model
simple_mod <- lm(year ~ funny + show_product_quickly +
                   patriotic + celebrity + danger + animals + use_sex,
                 data = youtube)

summary(simple_mod)

library(rsample)
bootstraps(youtube, times = 1e3)

set.seed(123)
youtube_intervals <- reg_intervals(year ~ funny + show_product_quickly +
                                     patriotic + celebrity + danger + animals + use_sex,
                                   data = youtube,
                                   type = "percentile",
                                   keep_reps = TRUE)
youtube_intervals

# Explore bootstrap results
youtube_intervals %>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Increase in year for each commercial characteristic",
    y = NULL
  )

youtube_intervals %>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term))

