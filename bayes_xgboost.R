library(visdat)
library(tidyverse)
library(tidymodels)
library(patchwork)

theme_set(theme_bw())
ncores <- 4

train_data <- read_csv("tidymodels/house_prices_train.csv")
test_data <- read_csv("tidymodels/house_prices_test.csv")

vis_dat(train_data)
DataExplorer::plot_intro(train_data)
p <- DataExplorer::plot_missing(train_data, missing_only = T)
cols_to_remove <- p$data %>% filter(Band %in% c("Remove", "Bad")) %>% pull(feature)

#Target variable
# hist
g1 <- ggplot(train_data, aes(x=SalePrice)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ 
  labs(x = "", y = "")

# boxplot
g2 <- ggplot(train_data, aes(y=SalePrice)) + 
  geom_boxplot(aes(x=""), colour="black", fill="white")+
  coord_flip()+ 
  labs(x = "", y = "")

# qqplot
g3 <- ggplot(train_data, aes(sample = SalePrice))+ 
  stat_qq()+
  stat_qq_line()+ 
  labs(x = "", y = "")
g3 | g1 / g2 

train_data <- train_data %>% mutate(SalePrice = log(SalePrice))
# hist
g1 <- ggplot(train_data, aes(x=SalePrice)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ 
  labs(x = "", y = "")

# boxplot
g2 <- ggplot(train_data, aes(y=SalePrice)) + 
  geom_boxplot(aes(x=""), colour="black", fill="white")+
  coord_flip()+ 
  labs(x = "", y = "")

# qqplot
g3 <- ggplot(train_data, aes(sample = SalePrice))+ 
  stat_qq()+
  stat_qq_line()+ 
  labs(x = "", y = "")
g3 | g1 / g2 

#Recipe
SalePrice_recipe <- recipe(train_data, SalePrice ~ .) %>%
  step_rm(Id, Street, Utilities) %>% 
  step_rm(one_of(cols_to_remove)) %>%
  step_log(all_numeric(),-all_outcomes(), offset = 1) %>%
  step_normalize(all_numeric(),-all_outcomes()) %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
  step_novel(all_predictors(), -all_numeric()) %>%
  step_impute_knn(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes())

#Model definition
SalePrice_xgb_model <- 
  boost_tree(
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(), 
    sample_size = tune(),
    mtry = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost", nthread = ncores)

#Workflow
SalePrice_workflow <- workflow() %>% add_recipe(SalePrice_recipe)
SalePrice_xgb_workflow <-SalePrice_workflow %>% add_model(SalePrice_xgb_model)

#Parameter definition
xgboost_params <- parameters(
  trees(),
  learn_rate(),
  tree_depth(),
  min_n(), 
  loss_reduction(),
  sample_size = sample_prop(), finalize(mtry(), train_data))
xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 500))) 

#Cross-validation
set.seed(123)
SalePrice_vfold <- vfold_cv(train_data, v = 5, strata = SalePrice)

workflow_SalePrice_xgb_model <- 
  workflow() %>% 
  add_model(SalePrice_xgb_model) %>% 
  add_recipe(SalePrice_recipe)

set.seed(321)
xgboost_tune <-
  workflow_SalePrice_xgb_model %>%
  tune_bayes(
    resamples = SalePrice_vfold,
    param_info = xgboost_params,
    # initial = ?,
    iter = 5, 
    metrics = metric_set(rmse, mape),
    control = control_bayes(no_improve = 10, save_pred = T, verbose = T))

doParallel::stopImplicitCluster()
autoplot(xgboost_tune)

#Checking results
SalePrice_best_model <- select_best(xgboost_tune, "rmse")
print(SalePrice_best_model)

SalePrice_final_model <- finalize_model(SalePrice_xgb_model, SalePrice_best_model)
SalePrice_workflow    <- workflow_SalePrice_xgb_model %>% update_model(SalePrice_final_model)
SalePrice_xgb_fit     <- fit(SalePrice_workflow, data = train_data)

pred <- 
  predict(SalePrice_xgb_fit, test_data) %>% 
  mutate(modelo = "XGBoost", .pred = exp(.pred)) %>% 
  bind_cols(read_csv("../input/saleprice-full-score/full-score.csv") %>% select(SalePrice))

g1 <- 
  pred %>% 
  ggplot(aes(x = .pred, y = SalePrice))+
  geom_point()+ 
  geom_abline(intercept = 0, col = "red")
g2 <- 
  pred %>% 
  select(.pred, SalePrice) %>% 
  gather(key, value) %>% 
  ggplot(aes(x=value, volor = key, fill = key)) + 
  geom_density(alpha=.2)+ 
  labs(x = "", y = "")
g1 / g2

read_csv("../input/house-prices-advanced-regression-techniques/sample_submission.csv") %>%
  select(-SalePrice) %>% 
  bind_cols(pred %>% transmute(SalePrice = .pred)) %>% 
  write_csv("submission.csv")
