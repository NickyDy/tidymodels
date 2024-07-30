library(tidyverse)
african_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv")
skimr::skim(african_names)

library(knitr)
african_names %>%
  count(port_disembark, sort = TRUE) %>%
  kable()

african_names %>%
  add_count(port_embark) %>%
  mutate(port_embark = case_when(
    n < 4000 ~ "Other",
    TRUE ~ port_embark)) %>%
  ggplot(aes(port_embark, year_arrival, fill = port_embark)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year")

african_names %>%
  ggplot(aes(year_arrival)) +
  geom_histogram(bins = 20, fill = "midnightblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(y = "Number of liberated individuals", x = NULL)

library(naniar)
african_names %>%
  select(gender, age, height, year_arrival) %>%
  gg_miss_upset()

african_names %>%
  ggplot(aes(gender, year_arrival, fill = gender)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year")

african_names %>%
  ggplot(aes(gender, age, fill = gender)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year")

african_names %>%
  filter(year_arrival < 1850) %>%
  group_by(year_arrival) %>%
  summarise(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(year_arrival, age)) +
  geom_line(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Mean age")

library(ggrepel)
african_names %>%
  group_by(name) %>%
  summarise(
    n = n(),
    age = mean(age, na.rm = TRUE),
    year_arrival = mean(year_arrival, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-n) %>%
  filter(n > 30) %>%
  ggplot(aes(year_arrival, age)) +
  geom_text_repel(aes(label = name), size = 3, family = "IBMPlexSans") +
  geom_point(aes(size = n), color = "midnightblue", alpha = 0.7) +
  labs(
    x = "Mean year of arrival", y = "Mean age",
    size = "Number of people",
    title = "Age and year of arrival for most common names of transported captives",
    caption = "African Names Database from slavevoyages.org")

liberated_df <- african_names %>%
  filter(year_arrival < 1850) %>%
  mutate(gender = case_when(
    gender == "Boy" ~ "Man",
    gender == "Girl" ~ "Woman",
    TRUE ~ gender)) %>%
  mutate_if(is.character, factor)
liberated_df

library(recipes)
impute_rec <- recipe(year_arrival ~ gender + age + height, data = liberated_df) %>%
  step_meanimpute(height) %>%
  step_knnimpute(all_predictors())

imputed <- prep(impute_rec) %>% juice()
summary(liberated_df$age)
summary(imputed$age)
summary(liberated_df$gender)
summary(imputed$gender)

fit_lm <- lm(year_arrival ~ gender + age, data = imputed)
summary(fit_lm)
tidy(fit_lm) %>%
  kable(digits = 3)
