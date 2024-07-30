library(tidyverse)

train_raw <- read_csv("air_wild.csv", guess_max = 1e5) %>%
  mutate(damaged = case_when(
    damaged > 0 ~ "damage",
    TRUE ~ "no damage"))
test_raw <- read_csv("test.csv", guess_max = 1e5)

skimr::skim(train_raw)

train_raw %>%
  count(damaged)

library(GGally)
train_raw %>%
  select(damaged, incident_year, height, speed, distance) %>%
  ggpairs(columns = 2:5, aes(color = damaged, alpha = 0.5))

train_raw %>%
  select(
    damaged, precipitation, visibility, engine_type,
    flight_impact, flight_phase, species_quantity
  ) %>%
  pivot_longer(precipitation:species_quantity) %>%
  ggplot(aes(y = value, fill = damaged)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)

bird_df <- train_raw %>%
  select(
    damaged, flight_impact, precipitation,
    visibility, flight_phase, engines, incident_year,
    incident_month, species_id, engine_type,
    aircraft_model, species_quantity, height, speed
  )

library(tidymodels)
set.seed(123)
bird_folds <- vfold_cv(train_raw, v = 5, strata = damaged)
bird_folds

bird_metrics <- metric_set(mn_log_loss, accuracy, sensitivity, specificity)

bird_rec <- recipe(damaged ~ ., data = bird_df) %>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_zv(all_predictors())
bird_rec
#=================================================
df <- bird_rec %>% prep() %>% bake(new_data = NULL)
#=================================================
library(baguette)
bag_spec <-
  bag_tree(min_n = 10) %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")
imb_wf <-
  workflow() %>%
  add_recipe(bird_rec) %>%
  add_model(bag_spec)
imb_fit <- fit(imb_wf, data = bird_df)
imb_fit

doParallel::registerDoParallel()
set.seed(123)
imb_rs <-
  fit_resamples(
    imb_wf,
    resamples = bird_folds,
    metrics = bird_metrics)
collect_metrics(imb_rs)

library(themis)
bal_rec <- bird_rec %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(damaged)
bal_wf <-
  workflow() %>%
  add_recipe(bal_rec) %>%
  add_model(bag_spec)
set.seed(234)
bal_rs <-
  fit_resamples(
    bal_wf,
    resamples = bird_folds,
    metrics = bird_metrics)
collect_metrics(bal_rs)

test_df <- test_raw %>%
  select(
    id, flight_impact, precipitation,
    visibility, flight_phase, engines, incident_year,
    incident_month, species_id, engine_type,
    aircraft_model, species_quantity, height, speed)
augment(imb_fit, test_df) %>%
  select(id, .pred_damage)