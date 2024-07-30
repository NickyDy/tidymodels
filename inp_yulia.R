library(tidyverse)
library(tidymodels)

df <- read_csv("yulia.csv") %>% mutate(school = as.factor(school))
glimpse(mis)

df_trees %>% map_dfr( ~ sum(is.na(.))) %>% view
df_trees %>% count(school)

first_df <- df %>% filter(is.na(school) | school %in% c("1", "2"))
second_df <- df %>% filter(school %in% c("3", "4"))

rec <- recipe(school ~ ., data = second_df)

# Impute via bagged trees
impute_trees <- rec %>%
  step_impute_bag(everything(),
                  impute_with = imp_vars(everything()),
                  options = list(nbagg = 5, keepX = FALSE))

imp_models_trees <- prep(impute_trees)

imputed_trees <- bake(imp_models_trees, new_data = NULL)

second_t <- imputed_trees %>% 
  mutate(school = as.integer(school)) %>% 
  map_dfr(~ round(., 0))

df_trees <- bind_rows(first_t, second_t)

write_csv(df_trees, "yulia_bagged_tree.csv")

# Impute via k-nearest neighbors
impute_knn <- rec %>%
  step_impute_knn(everything(), 
                  impute_with = imp_vars(everything()),
                  neighbors = 5)

imp_models_knn <- prep(impute_knn)

imputed_knn <- bake(imp_models_knn, new_data = NULL)

imputed_knn <- imputed_knn %>% 
  mutate(school = as.integer(school)) %>% 
  map_dfr(~ round(., 0))

write_csv(imputed_knn, "yulia_k_nearest.csv")
