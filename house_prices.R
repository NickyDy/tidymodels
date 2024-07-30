library(tidyverse)
library(tidymodels)
library(GGally)
library(broom)
library(patchwork)
library(splines)
library(rules)
library(baguette)
library(finetune)
library(kernlab)
library(ranger)
library(ggrepel)
library(ggridges)
library(skimr)
library(earth)
library(doParallel)
library(workflowsets)

options(scipen = 25, tidymodels.dark = TRUE)
theme_set(theme_bw(base_size = 14))
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.align = "center", fig.width = 8)

train <- read_csv("tidymodels/house_prices_train.csv") %>% slice_sample(n = 500)
test <- read_csv("tidymodels/house_prices_test.csv") %>% slice_sample(n = 200)

train %>% 
  select(where(is.numeric)) %>% 
  skim() %>% 
  as_tibble() %>% 
  select(2:3) %>% 
  filter(skim_variable != "SalePrice") %>% 
  bind_cols(
    test %>% 
      select(where(is.numeric)) %>% 
      skim() %>% 
      as_tibble() %>% 
      select(n_missing_test = n_missing)) %>% 
  filter(!(n_missing == 0 & n_missing_test == 0))

train %>% 
  select(where(is.character)) %>% 
  skim() %>% 
  as_tibble() %>% 
  select(skim_variable, n_missing) %>% 
  bind_cols(
    test %>% 
      select(where(is.character)) %>% 
      skim() %>% 
      as_tibble() %>% 
      select(n_missing_test = n_missing)) %>% 
  filter(!(n_missing == 0 & n_missing_test == 0))

train %>% 
  select(where(is.character)) %>% 
  skim() %>% 
  as_tibble() %>% 
  select(skim_variable, n_unique = character.n_unique) %>% 
  bind_cols(
    test %>% 
      select(where(is.character)) %>% 
      skim() %>% 
      as_tibble() %>% 
      select(n_unique_test = character.n_unique)) %>% 
  filter(n_unique != n_unique_test)

train %>% 
  select(where(is.numeric)) %>% 
  skim()

train %>% 
  select(where(is.character)) %>% 
  mutate(across(.cols = everything(), as.factor)) %>% 
  skim()

p <-  train %>% 
  ggplot(aes(SalePrice))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)
p2 <- train %>% 
  ggplot(aes(log(SalePrice)))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)
p / p2 

train <- train %>% 
  mutate(SalePrice = log(SalePrice))

autoplot_category <- function(dataset, x, y) {
  
  summary_stats <- dataset %>% 
    group_by({{y}}) %>% 
    summarise(
      mean = mean({{x}}, na.rm = TRUE),
      sd = sd({{x}}, na.rm = TRUE),
      n = n(),
      sem = sd/sqrt(n),
      uppr = mean + 1.96*sem,
      lwr = mean - 1.96*sem)
  
  ggplot(data = dataset,aes(x = {{x}}, y ={{y}}, fill = {{y}}))+
    geom_vline(xintercept = mean(dataset %>% pull({{x}}), na.rm = TRUE), size = 1.5, alpha = 0.2)+
    geom_density_ridges(alpha = 0.2,color = "#0000001A")+
    geom_boxplot(width = 0.5, fill = NA, outlier.colour = "darkred", position = position_nudge(y = 0.5),
                 size = 0.5, outlier.size = 1)+
    geom_errorbar(data = summary_stats, aes(x = mean, xmin = lwr, xmax = uppr ), width = 0.5, color = "darkred",
                  position = position_nudge(y = 0.5), size = 0.7, alpha = 0.9)+
    geom_point(data = summary_stats, aes(x = mean), color = "darkred", shape = 18, size = 2,
               position = position_nudge(y = 0.5))+
    geom_label(data = summary_stats, aes(x = 10.2,label = n), fill = NA, position = position_nudge(y = 0.5))+
    ggtitle(paste(substitute(x) ,"versus", substitute(y)))+
    labs(x = NULL, y = NULL)+
    theme(legend.position = "none")
}

train %>% 
  mutate(Neighborhood = fct_reorder(Neighborhood, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Neighborhood)

train <- train %>% 
  mutate(Neighborhood = as.factor(Neighborhood),
         NeighborhoodMerged = fct_collapse(Neighborhood,
                                           "BluesteSawyer" = c("Blueste", "Sawyer"),
                                           "NPkVillMitchel" = c("NPkVill", "Mitchel")))
test <- test %>% 
  mutate(Neighborhood = as.factor(Neighborhood),
         NeighborhoodMerged = fct_collapse(Neighborhood,
                                           "BluesteSawyer" = c("Blueste", "Sawyer"),
                                           "NPkVillMitchel" = c("NPkVill", "Mitchel")))
train %>% 
  mutate(Condition1 = fct_reorder(Condition1, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Condition1)

train %>% 
  filter(Condition1 == "Artery" & SalePrice > 12.3) %>% 
  select(Id, LotArea, Neighborhood, OverallQual, GrLivArea)

train <- train %>% 
  mutate(Condition1 = as.factor(Condition1),
         Condition1_isPos_AN = if_else(Condition1 %in% c("PosA", "PosN"), 1, 0),
         Condition1_isRRAe_Feedr = if_else(Condition1 %in% c("RRAe", "Feedr"), 1, 0),
         Condition1_isArtery = if_else(Condition1 == "Artery", 1, 0))

test <- test %>% 
  mutate(Condition1 = as.factor(Condition1),
         Condition1_isPos_AN = if_else(Condition1 %in% c("PosA", "PosN"), 1, 0),
         Condition1_isRRAe_Feedr = if_else(Condition1 %in% c("RRAe", "Feedr"), 1, 0),
         Condition1_isArtery = if_else(Condition1 == "Artery", 1, 0))

train %>% 
  mutate(BldgType = fct_reorder(BldgType, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, BldgType)

train <- train %>% 
  mutate(BldgType = as.factor(BldgType),
         BldgType_isDuplex = if_else(BldgType == "Duplex", 1, 0),
         BldgType_is2fmCon = if_else(BldgType == "2fmCon", 1, 0))

test <- test %>% 
  mutate(BldgType = as.factor(BldgType),
         BldgType_isDuplex = if_else(BldgType == "Duplex", 1, 0),
         BldgType_is2fmCon = if_else(BldgType == "2fmCon", 1, 0))

train %>% 
  mutate(HouseStyle = fct_reorder(HouseStyle, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, HouseStyle)

test %>% distinct(HouseStyle)

train <- train %>% 
  mutate(HouseStyle = as.factor(HouseStyle),
         HouseStyleMerged = fct_collapse(HouseStyle,
                                         "2Story" = c("2Story", "2.5Fin"),
                                         "1.5Fin" = c("1.5Fin", "2.5Unf")))

test <- test %>% 
  mutate(HouseStyle = as.factor(HouseStyle),
         HouseStyleMerged = fct_collapse(HouseStyle,
                                         "1.5Fin" = c("1.5Fin", "2.5Unf")))

train %>% 
  mutate(OverallQual = fct_reorder(as.factor(OverallQual), SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, OverallQual)

train %>% 
  filter(OverallQual == 10 & SalePrice < 12.2)

train %>% 
  filter(OverallQual %in% c(3,4) & SalePrice < 11)

Ids_to_remove <- train %>% 
  filter(OverallQual == 10 & SalePrice < 12.2) %>% 
  pull(Id)

train <- train %>% 
  filter(!Id %in% Ids_to_remove)

train <- train %>% 
  mutate(OverallQual = as.factor(OverallQual),
         OverallQualMerged = fct_collapse(OverallQual,
                                          "123" = c("1", "2", "3")))

test <- test %>% 
  mutate(OverallQual = as.factor(OverallQual),
         OverallQualMerged = fct_collapse(OverallQual,
                                          "123" = c("1", "2", "3")))
train %>% 
  mutate(OverallCond = fct_reorder(as.factor(OverallCond), SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, OverallCond)

train %>% 
  filter(OverallCond == 2 & SalePrice > 12)

train <- train %>% 
  mutate(OverallCond = if_else(Id == 379, 5, OverallCond))

train <- train %>% 
  mutate(OverallCond = as.factor(OverallCond),
         OverallCond_is123 = if_else(OverallCond %in% c("1", "2", "3"), 1, 0),
         OverallCond_is4 = if_else(OverallCond == "4", 1, 0))

test <- test %>% 
  mutate(OverallCond = as.factor(OverallCond),
         OverallCond_is123 = if_else(OverallCond %in% c("1", "2", "3"), 1, 0),
         OverallCond_is4 = if_else(OverallCond == "4", 1, 0))

train %>% 
  mutate(YearRemodAdd = cut_interval(YearRemodAdd, length = 5, dig.lab = 10),
         YearRemodAdd = fct_reorder(YearRemodAdd, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, YearRemodAdd)

train <- train %>% 
  mutate(YearRemodAddBinned = cut_interval(YearRemodAdd, length = 5, dig.lab = 10))

test <- test %>% 
  mutate(YearRemodAddBinned = cut_interval(YearRemodAdd, length = 5, dig.lab = 10))

train %>% 
  mutate(Exterior1st = fct_reorder(Exterior1st, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Exterior1st)

setdiff(train %>% distinct(Exterior1st) %>% pull(Exterior1st),
        test %>% distinct(Exterior1st) %>% pull(Exterior1st))

train <- train %>% 
  mutate(Exterior1st = as.factor(Exterior1st),
         Exterior1st_isAsbShng = if_else(Exterior1st %in% c("CBlock", "AsbShng", "AsphShn", "BrkComm"), 1, 0))

test <- test %>% 
  mutate(Exterior1st = if_else(is.na(Exterior1st), "VinylSd", Exterior1st), 
         Exterior1st = as.factor(Exterior1st),
         Exterior1st_isAsbShng = if_else(Exterior1st %in% c("CBlock", "AsbShng", "AsphShn", "BrkComm"), 1, 0))

train %>% 
  mutate(MasVnrType = fct_reorder(MasVnrType, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, MasVnrType)

train <- train %>% 
  mutate(MasVnrType = if_else(is.na(MasVnrType), "BrkFace", MasVnrType),
         MasVnrType = as.factor(MasVnrType)) 

test <- test %>% 
  mutate(MasVnrType = if_else(is.na(MasVnrType), "BrkFace", MasVnrType),
         MasVnrType = as.factor(MasVnrType)) 

train %>% 
  mutate(ExterQual = fct_reorder(ExterQual, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, ExterQual)

train <- train %>% 
  mutate(ExterQual = as.factor(ExterQual))

test <- test %>% 
  mutate(ExterQual = as.factor(ExterQual))

train %>% 
  mutate(Foundation = fct_reorder(Foundation, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Foundation)

train <- train %>% 
  mutate(Foundation = as.factor(Foundation),
         FoundationMerged = fct_collapse(Foundation,
                                         "CBlockStone" = c("CBlock", "Stone"),
                                         "PConcWood" = c("PConc", "Wood"))) 

test <- test %>% 
  mutate(Foundation = as.factor(Foundation),
         FoundationMerged = fct_collapse(Foundation,
                                         "CBlockStone" = c("CBlock", "Stone"),
                                         "PConcWood" = c("PConc", "Wood"))) 

train %>% 
  mutate(BsmtQual = fct_reorder(BsmtQual, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, BsmtQual)

train <- train %>% 
  mutate(BsmtQual = if_else(is.na(BsmtQual), "NoBsmt", BsmtQual))

test <- test %>% 
  mutate(BsmtQual = if_else(is.na(BsmtQual), "NoBsmt", BsmtQual))

train %>% 
  mutate(HeatingQC = fct_reorder(HeatingQC, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, HeatingQC)

train <- train %>% 
  mutate(HeatingQC = as.factor(HeatingQC),
         HeatingQCMerged = fct_collapse(HeatingQC,
                                        "FaPo" = c("Fa", "Po")))

test <- test %>% 
  mutate(HeatingQC = as.factor(HeatingQC),
         HeatingQCMerged = fct_collapse(HeatingQC,
                                        "FaPo" = c("Fa", "Po")))

train %>% 
  mutate(Heating = fct_reorder(Heating, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Heating)

test %>% distinct(Heating)

train <- train %>% 
  mutate(Heating_isGravWall = if_else(Heating %in% c("Grav", "Wall"), 1, 0))

test <- test %>% 
  mutate(Heating_isGravWall = if_else(Heating %in% c("Grav", "Wall"), 1, 0))

train %>% 
  mutate(CentralAir = fct_reorder(CentralAir, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, CentralAir)

train <- train %>% 
  mutate(CentralAir = as.factor(CentralAir))

test <- test %>% 
  mutate(CentralAir = as.factor(CentralAir))

train %>% 
  mutate(KitchenQual = fct_reorder(KitchenQual, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, KitchenQual)

train <- train %>% 
  mutate(KitchenQual = as.factor(KitchenQual))

test <- test %>% 
  mutate(KitchenQual = if_else(is.na(KitchenQual), "TA", KitchenQual),
         KitchenQual = as.factor(KitchenQual)) 

train %>% 
  mutate(Fireplaces = fct_reorder(as.factor(Fireplaces), SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, Fireplaces)

train <- train %>% 
  mutate(Fireplaces = as.factor(Fireplaces),
         FireplacesMerged = fct_collapse(Fireplaces,
                                         "234" = c("2", "3"))) 
test <- test %>% 
  mutate(Fireplaces = as.factor(Fireplaces),
         FireplacesMerged = fct_collapse(Fireplaces,
                                         "234" = c("2", "3", "4"))) 

train %>% 
  mutate(GarageFinish = fct_reorder(GarageFinish, SalePrice, .fun = "mean")) %>% 
  autoplot_category(SalePrice, GarageFinish)

train <- train %>% 
  mutate(GarageFinish = if_else(is.na(GarageFinish), "NoGarage", GarageFinish),
         GarageFinish = as.factor(GarageFinish)) 

test <- test %>% 
  mutate(GarageFinish = if_else(is.na(GarageFinish), "NoGarage", GarageFinish),
         GarageFinish = as.factor(GarageFinish))

p1 <- train %>% 
  ggplot(aes(TotalBsmtSF))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)
p2 <- train %>% 
  ggplot(aes(log(TotalBsmtSF+1)))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)

p1 / p2

train %>% 
  ggplot(aes(TotalBsmtSF, SalePrice))+
  geom_point(alpha = 0.8)+
  geom_smooth()+
  scale_x_continuous(n.breaks = 6)

test <- test %>% 
  mutate(TotalBsmtSF = replace_na(TotalBsmtSF, median(TotalBsmtSF, na.rm = TRUE)))

p1 <- train %>% 
  ggplot(aes(GrLivArea))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)

p2 <- train %>% 
  ggplot(aes(log(GrLivArea)))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)

p1 / p2

train %>% 
  ggplot(aes(log(GrLivArea), SalePrice))+
  geom_point(alpha = 0.8)+
  geom_smooth()

p1 <- train %>% 
  mutate(TotalArea = GrLivArea + TotalBsmtSF) %>% 
  ggplot(aes(TotalArea))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)

p2 <- train %>% 
  mutate(TotalArea = GrLivArea + TotalBsmtSF) %>% 
  ggplot(aes(log(TotalArea)))+
  geom_histogram(fill = "steelblue", color = "white")+
  labs(y = NULL)

p1 / p2

train %>% 
  mutate(TotalArea = GrLivArea + TotalBsmtSF) %>%
  ggplot(aes(log(TotalArea), SalePrice))+
  geom_point()+
  geom_smooth()

train <- train %>% 
  mutate(TotalArea = GrLivArea + TotalBsmtSF)

test <- test %>% 
  mutate(TotalArea = GrLivArea + TotalBsmtSF)

p1 <- train %>%
  mutate(BuildingAge = YrSold - YearBuilt) %>% 
  ggplot(aes(BuildingAge))+
  geom_histogram(color = "white", fill = "steelblue")+
  scale_x_continuous(n.breaks = 15)

p2 <- train %>%
  mutate(BuildingAge = YrSold - YearBuilt) %>% 
  ggplot(aes(log(BuildingAge+1)))+
  geom_histogram(color = "white", fill = "steelblue")+
  scale_x_continuous(n.breaks = 15)

p1 / p2

train %>%
  mutate(BuildingAge = YrSold - YearBuilt) %>% 
  ggplot(aes(BuildingAge, SalePrice))+
  geom_point(alpha = 0.8, show.legend = FALSE)+
  geom_smooth(aes(color = as.factor(BuildingAge > 90), fill = as.factor(BuildingAge > 90)), show.legend = FALSE)+
  scale_x_continuous(n.breaks = 10)+
  scale_color_brewer(palette = "Set1")

train <- train %>% 
  mutate(BuildingAge = YrSold - YearBuilt,
         BuildingAge_isOver90 = as.double(BuildingAge > 90)) 

test <- test %>% 
  mutate(BuildingAge = YrSold - YearBuilt,
         BuildingAge_isOver90 = as.double(BuildingAge > 90)) 

train %>% 
  select(SalePrice, TotalBsmtSF , GrLivArea,  TotalArea, BuildingAge) %>% 
  ggcorr(label = TRUE, label_round = 2, geom = "circle", min_size = 10, max_size = 20)+
  theme(legend.key.height = unit(0.85, "inch"))

#Model Exploration--------------------------------------------------------------------
glimpse(train)
recipe_all <- train %>% 
  recipe(SalePrice ~ ., data = .) %>% 
  step_log(TotalArea) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

recipe_pars <- train %>% 
  recipe(SalePrice ~ TotalArea + BuildingAge + BuildingAge_isOver90 + NeighborhoodMerged +
           ExterQual + KitchenQual + FoundationMerged + CentralAir + BsmtQual +
           GarageFinish + OverallQualMerged + FireplacesMerged + OverallCond_is123 + OverallCond_is4 + 
           HouseStyleMerged + MasVnrType + HeatingQCMerged + YearRemodAddBinned,
         data = .) %>% 
  step_log(TotalArea) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

xgb <- boost_tree(mtry = tune(), trees = tune(), learn_rate = tune(),
                  tree_depth = tune(), min_n = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_param <- parameters(xgb) %>% 
  update(trees = trees(c(1000,3000)),
         mtry = mtry(c(4,18)),
         learn_rate = learn_rate(c(-5,-1)),
         tree_depth = tree_depth(c(1, 10)),
         min_n = min_n(c(2, 30)))

nn <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

nn_param <- parameters(nn) %>% 
  update(
    hidden_units = hidden_units(c(1,5)),
    penalty = penalty(c(-5,0)),
    epochs = epochs(c(10, 1000)))

rf <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_param <- parameters(rf) %>% 
  update(mtry = mtry(c(10,20)),
         trees = trees(c(1000, 2500)),
         min_n = min_n(c(2, 20)))

rad_svm <- svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

rad_svm_param <- parameters(rad_svm) %>% 
  update(
    cost = cost(c(-10, 5)),
    rbf_sigma = rbf_sigma(c(-5,0)),
    margin = svm_margin(c(0, 0.2)))

cub_rules <- cubist_rules(committees = tune(), neighbors = tune(), max_rules = tune()) %>% 
  set_engine("Cubist") %>% 
  set_mode("regression")

cub_rules_param <- parameters(cub_rules) %>% 
  update(
    committees = committees(c(1, 100)),
    neighbors = neighbors(c(1, 10)),
    max_rules = max_rules(c(1, 500)))

lin_reg <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

lin_reg_param <- parameters(lin_reg) %>% 
  update(
    penalty = penalty(c(-5, 0)),
    mixture = mixture(c(0,1)))

mars_fit <- mars(num_terms = tune(), prod_degree = tune()) %>% 
  set_engine("earth") %>% 
  set_mode("regression")

mars_fit_param <- parameters(mars_fit) %>% 
  update(
    num_terms = num_terms(c(5, 15)),
    prod_degree = prod_degree(c(1, 2)))

exploratory_workflowset <- workflow_set(
  preproc = list("recipe_all" = recipe_all, "recipe_pars" = recipe_pars),
  models = list("neural_net" = nn, "xgboost" = xgb,"rand_for" = rf, "rad_svm" = rad_svm,
                "cubist_rules" = cub_rules, "linear_reg" = lin_reg, "MARS" = mars_fit))

exploratory_workflowset <- exploratory_workflowset %>% 
  option_add(param_info = xgb_param, id = "recipe_all_xgboost") %>% 
  option_add(param_info = nn_param, id = "recipe_all_neural_net") %>% 
  option_add(param_info = rf_param, id = "recipe_all_rand_for") %>% 
  option_add(param_info = rad_svm_param, id = "recipe_all_rad_svm") %>% 
  option_add(param_info = cub_rules_param, id = "recipe_all_cubist_rules") %>% 
  option_add(param_info = lin_reg_param, id = "recipe_all_linear_reg") %>% 
  option_add(param_info = mars_fit_param, id = "recipe_all_MARS") %>% 
  option_add(param_info = xgb_param, id = "recipe_pars_xgboost") %>% 
  option_add(param_info = nn_param, id = "recipe_pars_neural_net") %>% 
  option_add(param_info = rf_param, id = "recipe_pars_rand_for") %>% 
  option_add(param_info = rad_svm_param, id = "recipe_pars_rad_svm") %>% 
  option_add(param_info = cub_rules_param, id = "recipe_pars_cubist_rules") %>% 
  option_add(param_info = lin_reg_param, id = "recipe_pars_linear_reg") %>% 
  option_add(param_info = mars_fit_param, id = "recipe_pars_MARS")  

set.seed(1993)
cv_folds <- vfold_cv(train, v = 5, repeats = 1)

grid_control <- control_grid(
  verbose = TRUE)

cl <- makeCluster(4)
registerDoParallel(cl)

exploratory_results <- exploratory_workflowset %>% 
  workflow_map(
    verbose = TRUE,
    seed = 1993,
    resamples = cv_folds,
    control = grid_control,
    grid = 60,
    metrics = metric_set(rmse))

autoplot(exploratory_results,
         select_best = TRUE)+
  geom_text(aes(label = wflow_id), angle = 90, nudge_x = -0.4)+
  scale_y_continuous(n.breaks = 10)+
  labs(y = NULL)+
  theme(legend.position = "none")

plot_compare <- function(id_all, id_pars) {
  
  p1 <- exploratory_results %>% 
    pull_workflow_set_result(id = id_all) %>%
    autoplot()+
    ggtitle("Recipe All")
  
  p2 <- exploratory_results %>% 
    pull_workflow_set_result(id = id_pars) %>%
    autoplot()+
    ggtitle("Recipe Parsimonious")
  
  return(p1 / p2) 
}

plot_compare("recipe_all_xgboost",
             "recipe_pars_xgboost")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_xgboost") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_xgboost") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

set.seed(1993)
best_model <- boost_tree(mtry = 9, trees = 2472, learn_rate = 0.05308297,
                         tree_depth = 2, min_n = 2) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("XGB_All_NULL.csv")

set.seed(1993)
best_model <- boost_tree(mtry = 11, trees = 1375, learn_rate = 0.01700219,
                         tree_depth = 5, min_n = 4) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_pars) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("XGB_Pars_NULL.csv")

plot_compare("recipe_all_neural_net",
             "recipe_pars_neural_net")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_neural_net") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_neural_net") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

set.seed(1993)
best_model <- mlp(hidden_units = 1, penalty = 0.0008246147, epochs = 692) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("NN_All_NULL.csv")

set.seed(1993)
best_model <- mlp(hidden_units = 1, penalty = 0.006460661, epochs = 470) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_pars) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("NN_Pars_NULL.csv")

plot_compare("recipe_all_rand_for",
             "recipe_pars_rand_for")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_rand_for") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_rand_for") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

plot_compare("recipe_all_rad_svm",
             "recipe_pars_rad_svm")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_rad_svm") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_rad_svm") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

set.seed(1993)
best_model <- svm_rbf(cost = 7.5946669, rbf_sigma = 0.00144719017, margin = 0.010112632) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("SVM_Rad_All_NULL.csv")

set.seed(1993)
best_model <- svm_rbf(cost = 7.5946669, rbf_sigma = 0.00144719017, margin = 0.010112632) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_pars) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("SVM_Rad_Pars_NULL.csv")

plot_compare("recipe_all_cubist_rules",
             "recipe_pars_cubist_rules")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_cubist_rules") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_cubist_rules") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_cubist_rules") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_cubist_rules") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

set.seed(1993)
best_model <- cubist_rules(committees = 91, neighbors = 9, max_rules = 365) %>% 
  set_engine("Cubist") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("Cubist_All_NULL.csv")

set.seed(1993)
best_model <- cubist_rules(committees = 67, neighbors = 10, max_rules = 442) %>% 
  set_engine("Cubist") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_pars) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("Cubist_Pars_NULL.csv")

plot_compare("recipe_all_linear_reg",
             "recipe_pars_linear_reg")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_linear_reg") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_linear_reg") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

set.seed(1993)
best_model <- linear_reg(penalty = 0.00001222794, mixture = 0.005317853) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("LinReg_All_NULL.csv")

set.seed(1993)
best_model <- linear_reg(penalty = 0.00001222794, mixture = 0.005317853) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

workflow() %>% 
  add_recipe(recipe_pars) %>% 
  add_model(best_model) %>% 
  fit(train) %>% 
  predict(new_data = test) %>% 
  bind_cols(Id = test$Id) %>% 
  mutate(SalePrice = exp(.pred)) %>% 
  select(-.pred) %>% 
  write_csv("LinReg_Pars_NULL.csv")

plot_compare("recipe_all_MARS",
             "recipe_pars_MARS")

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_all_MARS") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)

exploratory_results %>% 
  pull_workflow_set_result(id = "recipe_pars_MARS") %>% 
  collect_metrics() %>% 
  arrange(mean) %>% 
  slice_head(n = 10)
