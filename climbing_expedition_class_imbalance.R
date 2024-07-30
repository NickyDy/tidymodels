library(tidyverse)
members <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv")
members

members %>%
  group_by(year = 10 * (year %/% 10)) %>%
  summarise(
    died = mean(died),
    success = mean(success)) %>%
  pivot_longer(died:success, names_to = "outcome", values_to = "percent") %>%
  ggplot(aes(year, percent, color = outcome)) +
  geom_line(alpha = 0.7, size = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "% of expedition members", color = NULL)

members %>%
  group_by(age = 10 * (age %/% 10)) %>%
  summarise(
    died = mean(died),
    success = mean(success)) %>%
  pivot_longer(died:success, names_to = "outcome", values_to = "percent") %>%
  ggplot(aes(age, percent, color = outcome)) +
  geom_line(alpha = 0.7, size = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "% of expedition members", color = NULL)

library(knitr)
members %>%
  count(success, died) %>%
  group_by(success) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(col.names = c("Expedition success", "Died", "Number of people", "% of people"),
    align = "llrr")

members %>%
  filter(!is.na(peak_name)) %>%
  mutate(peak_name = fct_lump(peak_name, prop = 0.05)) %>%
  count(peak_name, died) %>%
  group_by(peak_name) %>%
  mutate(percent = scales::percent(n / sum(n))) %>%
  kable(
    col.names = c("Peak", "Died", "Number of people", "% of people"),
    align = "llrr")

members %>%
  filter(season != "Unknown") %>%
  count(season, died) %>%
  group_by(season) %>%
  mutate(percent = n / sum(n),
    died = case_when(died ~ "Died", TRUE ~ "Did not die")) %>%
  ggplot(aes(season, percent, fill = season)) +
  geom_col(alpha = 0.8, position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~died, scales = "free") +
  labs(x = NULL, y = "% of expedition members")

members_df <- members %>%
  filter(season != "Unknown", !is.na(sex), !is.na(citizenship)) %>%
  select(peak_id, year, season, sex, age, citizenship, hired, success, died) %>%
  mutate(died = case_when(
    died ~ "died",
    TRUE ~ "survived"
  )) %>%
  mutate_if(is.character, factor) %>%
  mutate_if(is.logical, as.integer)
members_df

library(tidymodels)
set.seed(123)
members_split <- initial_split(members_df, strata = died)
members_train <- training(members_split)
members_test <- testing(members_split)

set.seed(123)
members_folds <- vfold_cv(members_train, strata = died)
members_folds

library(themis)
members_rec <- recipe(died ~ ., data = members_train) %>%
  step_medianimpute(age) %>%
  step_other(peak_id, citizenship) %>%
  step_dummy(all_nominal(), -died) %>%
  step_smote(died)
members_rec

glm_spec <- logistic_reg() %>%
  set_engine("glm")
glm_spec

rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf_spec

members_wf <- workflow() %>%
  add_recipe(members_rec)
members_wf

members_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)
doParallel::registerDoParallel()
glm_rs <- members_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = members_folds,
    metrics = members_metrics,
    control = control_resamples(save_pred = TRUE))
glm_rs

rf_rs <- members_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = members_folds,
    metrics = members_metrics,
    control = control_resamples(save_pred = TRUE))
rf_rs

collect_metrics(glm_rs)

glm_rs %>%
  conf_mat_resampled()

rf_rs %>%
  conf_mat_resampled()

glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(died, .pred_died) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

members_final <- members_wf %>%
  add_model(glm_spec) %>%
  last_fit(members_split)
members_final

collect_metrics(members_final)

collect_predictions(members_final) %>%
  conf_mat(died, .pred_class)

members_final %>%
  pull(.workflow) %>%
  pluck(1) %>%
  tidy(exponentiate = TRUE) %>%
  arrange(estimate) %>%
  kable(digits = 3)

members_final %>%
  pull(.workflow) %>%
  pluck(1) %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate, fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, color = "gray50", lty = 2, size = 1.2) +
  geom_errorbar(aes(
    xmin = estimate - std.error,
    xmax = estimate + std.error),
  width = .2, color = "gray50", alpha = 0.7) +
  geom_point(size = 2, color = "#85144B") +
  labs(y = NULL, x = "Coefficent from logistic regression")
