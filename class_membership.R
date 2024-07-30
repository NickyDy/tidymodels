library(tidyverse)
library(datasauRus)

datasaurus_dozen

datasaurus_dozen %>%
  ggplot(aes(x, y, color = dataset)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~dataset, ncol = 5)
datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(across(c(x, y), list(mean = mean, sd = sd)),
            x_y_cor = cor(x, y))
datasaurus_dozen %>%
  count(dataset)

library(tidymodels)
set.seed(123)
dino_folds <- datasaurus_dozen %>%
  mutate(dataset = factor(dataset)) %>%
  bootstraps()
dino_folds

rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")
dino_wf <- workflow() %>%
  add_formula(dataset ~ x + y) %>%
  add_model(rf_spec)
dino_wf

doParallel::registerDoParallel()
dino_rs <- fit_resamples(
  dino_wf,
  resamples = dino_folds,
  control = control_resamples(save_pred = TRUE))
dino_rs

collect_metrics(dino_rs)

dino_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  ppv(dataset, .pred_class)

dino_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(dataset, .pred_away:.pred_x_shape) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  facet_wrap(~.level, ncol = 5) +
  coord_equal()

dino_rs %>%
  collect_predictions() %>%
  conf_mat(dataset, .pred_class)

dino_rs %>%
  collect_predictions() %>%
  conf_mat(dataset, .pred_class) %>%
  autoplot(type = "heatmap")

dino_rs %>%
  collect_predictions() %>%
  filter(.pred_class != dataset) %>%
  conf_mat(dataset, .pred_class) %>%
  autoplot(type = "heatmap")
