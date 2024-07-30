library(tidyverse)

turbines <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/diss-turbine.csv")
turbines

turbines_df <- turbines %>%
  transmute(
    turbine_capacity = turbine_rated_capacity_k_w,
    rotor_diameter_m,
    hub_height_m,
    commissioning_date = parse_number(commissioning_date),
    province_territory = fct_lump_n(province_territory, 10),
    model = fct_lump_n(model, 10)) %>%
  filter(!is.na(turbine_capacity)) %>%
  mutate_if(is.character, factor)

turbines_df %>%
  select(turbine_capacity:commissioning_date) %>%
  pivot_longer(rotor_diameter_m:commissioning_date) %>%
  ggplot(aes(turbine_capacity, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free_y") +
  labs(y = NULL) +
  scale_fill_gradient(high = "cyan3")

library(tidymodels)
set.seed(123)
diss_split <- initial_split(turbines_df, strata = turbine_capacity)
diss_train <- training(diss_split)
diss_test <- testing(diss_split)

set.seed(234)
diss_folds <- vfold_cv(diss_train, strata = turbine_capacity)
diss_folds

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tree_spec

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid

doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tune_grid(
  tree_spec,
  turbine_capacity ~ .,
  resamples = diss_folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq, mae, mape))
tree_rs

collect_metrics(tree_rs)

autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")

show_best(tree_rs, "mape")

select_best(tree_rs, "rmse")

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "rmse"))

final_tree

final_fit <- fit(final_tree, turbine_capacity ~ ., diss_train)
final_rs <- last_fit(final_tree, turbine_capacity ~ ., diss_split)

library(vip)
final_fit %>%
  vip(geom = "col", aesthetics = list(fill = "#1303fc", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

library(parttree)
ex_fit <- fit(
  final_tree,
  turbine_capacity ~ rotor_diameter_m + commissioning_date,
  diss_train)

diss_train %>%
  ggplot(aes(rotor_diameter_m, commissioning_date)) +
  geom_parttree(data = ex_fit, aes(fill = turbine_capacity), alpha = 0.3) +
  geom_jitter(alpha = 0.7, width = 1, height = 0.5, aes(color = turbine_capacity)) +
  scale_colour_viridis_c(aesthetics = c("color", "fill"))

collect_metrics(final_rs)

final_rs %>%
  collect_predictions() %>%
  ggplot(aes(turbine_capacity, .pred)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  coord_fixed()
#===============================================================================
library(tidyverse)

load("diss.RData")

diss_df <- diss %>%
  select(exp, top, elev, slope, s_ha, herb_cover, shrub_cover) %>%
  mutate_if(is.character, as.factor)
glimpse(diss_df)

library(tidymodels)
set.seed(123)
diss_split <- initial_split(diss_df, strata = s_ha)
diss_train <- training(diss_split)
diss_test <- testing(diss_split)

set.seed(234)
diss_folds <- vfold_cv(diss_train, strata = s_ha)
diss_folds

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
tree_spec

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid

doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tune_grid(
  tree_spec,
  s_ha ~ .,
  resamples = diss_folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq, mae, mape))
tree_rs

collect_metrics(tree_rs)

autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")

show_best(tree_rs, "mape")

select_best(tree_rs, "rmse")

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "rmse"))

final_tree

final_fit <- fit(final_tree, s_ha ~ ., diss_train)
final_rs <- last_fit(final_tree, s_ha ~ ., diss_split)

library(vip)
final_fit %>%
  vip(geom = "col", aesthetics = list(fill = "#1303fc", alpha = 0.5)) +
  scale_y_continuous(expand = c(0, 0))

library(parttree)
ex_fit <- fit(
  final_tree,
  s_ha ~ herb_cover + exp,
  diss_train)

diss_train %>%
  ggplot(aes(herb_cover, exp)) +
  geom_parttree(data = ex_fit, aes(fill = s_ha), alpha = 0.3) +
  geom_jitter(alpha = 0.7, width = 1, height = 0.5, aes(color = s_ha)) +
  scale_colour_viridis_c(aesthetics = c("color", "fill"))

collect_metrics(final_rs)

final_rs %>%
  collect_predictions() %>%
  ggplot(aes(s_ha, .pred)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  coord_fixed()
