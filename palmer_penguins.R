library(tidyverse)
library(palmerpenguins)

penguins

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)

penguins_df <- penguins %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)

library(tidymodels)
set.seed(123)
penguin_split <- initial_split(penguins_df, strata = sex)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)

set.seed(123)
penguin_boot <- bootstraps(penguin_train)
penguin_boot

glm_spec <- logistic_reg() %>%
  set_engine("glm")
glm_spec

rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")
rf_spec

penguin_wf <- workflow() %>%
  add_formula(sex ~ .)
penguin_wf

glm_rs <- penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE))
glm_rs

rf_rs <- penguin_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(resamples = penguin_boot,
    control = control_resamples(save_pred = TRUE))
rf_rs

collect_metrics(rf_rs)

collect_metrics(glm_rs)

glm_rs %>%
  conf_mat_resampled()

glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal()

#The metrics and predictions here are on the testing data
penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)
penguin_final

collect_metrics(penguin_final)

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(bill_depth_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)