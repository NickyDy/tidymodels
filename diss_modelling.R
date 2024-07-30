
library(tidyverse)
library(tidymodels)

load("diss.RData")

diss %>%
  count(veg_type)

library(skimr)
skim(diss)

library(GGally)
diss %>%
  select(
    veg_type, elev, slope, s_ha, s_m2) %>%
  ggpairs(mapping = aes(color = veg_type))

diss_df <- diss %>%
  select(veg_type, exp, top, elev, slope, s_ha, s_m2, invasive, rare,
         herb_cover, perennials) %>%
  mutate_if(is.character, factor)

set.seed(1234)
diss_split <- initial_split(diss_df)
diss_train <- training(diss_split)
diss_test <- testing(diss_split)

diss_rec <- recipe(veg_type ~ ., data = diss_train) %>%
  themis::step_downsample(veg_type) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
rec_df<-diss_rec %>% prep() %>% bake(new_data = NULL)

test_proc <- bake(diss_rec, new_data = diss_test)

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")
knn_fit <- knn_spec %>%
  fit(veg_type ~ ., data = juice(diss_rec))
knn_fit

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")
tree_fit <- tree_spec %>%
  fit(veg_type ~ ., data = juice(diss_rec))
tree_fit

set.seed(1234)
validation_splits <- mc_cv(juice(diss_rec), prop = 0.9, strata = veg_type)
validation_splits

knn_res <- fit_resamples(
  knn_spec,
  veg_type ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE))
knn_res %>%
  collect_metrics()

tree_res <- fit_resamples(
  tree_spec,
  veg_type ~ .,
  validation_splits,
  control = control_resamples(save_pred = TRUE))
tree_res %>%
  collect_metrics()

knn_res %>%
  unnest(.predictions) %>%
  mutate(model = "kknn") %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>%
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(veg_type, .pred_f) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2)

knn_conf <- knn_res %>%
  unnest(.predictions) %>%
  conf_mat(veg_type, .pred_class)
knn_conf
knn_conf %>%
  autoplot()

knn_fit %>%
  predict(new_data = test_proc, type = "prob") %>%
  mutate(truth = diss_test$veg_type) %>%
  roc_auc(truth, .pred_f)
