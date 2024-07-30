library(DataExplorer) 
library(dlookr)
library(tidymodels)
library(flextable)
library(tidyverse)
library(vip)
library(themis)
library(xgboost)

df <- read_csv("Opt_6B.csv", col_types = "f") %>%
  janitor::clean_names() %>% slice_sample(n = 1000)

skimr::skim(df)
glimpse(df)
diagnose_numeric(df) %>% flextable()
plot_intro(df)
plot_histogram(df)
plot_correlate(df)
df %>% plot_bar_category(top = 15)
df %>% plot_bar(by  = "profit")
df %>% select(profit, where(is.numeric)) %>% plot_boxplot(by = "profit")

set.seed(1234)
split <- initial_split(df, prop = 0.75, strata = profit)
train <- split %>% training()
test <- split %>% testing()

train_folds <- vfold_cv(train, strata = profit)

rec <- recipe(profit ~., data = train) %>%
  step_rm(entry_name, i31) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_smote(profit)

xgb_tune <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                      
  sample_size = tune(), 
  mtry = tune(),
  learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_tune)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30)

doParallel::registerDoParallel()
set.seed(1234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = train_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE))

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc)

final_xgb %>%
  fit(data = train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "col")

last_xgb_fit <- last_fit(final_xgb, split)
last_xgb_fit %>% collect_metrics()

results <- last_xgb_fit %>% collect_predictions()
roc_curve(results, truth = profit,
          estimate = .pred_1) %>%
  autoplot()

tbl_conf <- last_xgb_fit %>% 
  collect_predictions() %>% 
  select(c(.pred_class, profit))


confm <- conf_mat(tbl_conf,
                  truth = profit, estimate =.pred_class)

confm %>% autoplot(type = 'heatmap')
