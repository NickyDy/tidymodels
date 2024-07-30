library(tidyverse)

departures_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv")

departures_raw %>%
  filter(departure_code < 9) %>%
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other")) %>%
  filter(fyear > 1995, fyear < 2019) %>%
  count(fyear, involuntary) %>%
  ggplot(aes(fyear, n, color = involuntary)) +
  geom_line(size = 1.2, alpha = 0.5) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", lty = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Number of CEO departures", color = NULL)

departures <- departures_raw %>%
  filter(departure_code < 9) %>%
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other")) %>%
  filter(fyear > 1995, fyear < 2019)
departures

library(broom)
df <- departures %>%
  count(fyear, involuntary) %>%
  pivot_wider(names_from = involuntary, values_from = n)
mod <- glm(cbind(involuntary, other) ~ fyear, data = df, family = "binomial")
summary(mod)

tidy(mod, exponentiate = TRUE)
# When we use exponentiate = TRUE, we get the model coefficients on the 
# linear scale instead of the logistic scale.

library(rsample)
set.seed(123)
ceo_folds <- bootstraps(departures, times = 1e3)
ceo_folds

fit_binom <- function(split) {
  df <- analysis(split) %>%
    count(fyear, involuntary) %>%
    pivot_wider(names_from = involuntary, values_from = n)
  
  mod <- glm(cbind(involuntary, other) ~ fyear, data = df, family = "binomial")
  tidy(mod, exponentiate = TRUE)
}

boot_models <- ceo_folds %>% mutate(coef_info = map(splits, fit_binom))
boot_models

percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals

boot_models %>%
  unnest(coef_info) %>%
  filter(term == "fyear") %>%
  ggplot(aes(estimate)) +
  geom_vline(xintercept = 1, lty = 2, color = "gray50", size = 2) +
  geom_histogram(fill = "midnightblue") +
  labs(x = "Annual increase in involuntary CEO departures",
    title = "Over this time period, CEO departures are increasingly involuntary",
    subtitle = "Each passing year corresponds to a departure being 1-2% more likely to be involuntary")
