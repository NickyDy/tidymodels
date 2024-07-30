# 4 Tune model parameters-----------------------------------------------------
library(tidyverse)
library(tidymodels)  # for the tune package, along with the rest of tidymodels
library(modeldata)   # for the cells data
library(vip)         # for variable importance plots
library(rpart.plot)

cells <- read_csv("csv/churn.csv", col_types = "fdfffff") %>% janitor::clean_names() %>% slice_sample(n = 5000)

cells<-df

set.seed(123)
cell_split <- initial_split(cells, strata = result)
cell_train <- training(cell_split)
cell_test  <- testing(cell_split)

# Tuning hyperparameters
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")
tune_spec

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 3)
tree_grid
tree_grid %>% 
  count(tree_depth)

set.seed(234)
cell_folds <- vfold_cv(cell_train)

# Model tuning with a grid
set.seed(345)
tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(churn ~ .)

doParallel::registerDoParallel()
set.seed(1234)
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid)
tree_res

tree_res %>% 
  collect_metrics()

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tree_res %>%
  show_best("roc_auc")

best_tree <- tree_res %>%
  select_best("roc_auc")
best_tree

# Finalizing our model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)
final_wf

# Exploring results
final_tree <- 
  final_wf %>%
  fit(data = cell_train) 
final_tree

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()

#The last fit
final_fit <- 
  final_wf %>%
  last_fit(cell_split) 
final_fit %>%
  collect_metrics()
final_fit %>%
  collect_predictions() %>% 
  roc_curve(churn, .pred_no_churn) %>% 
  autoplot()

final_tree <- extract_workflow(final_fit)
final_tree

final_tree %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE, cex = 0.7)

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()
