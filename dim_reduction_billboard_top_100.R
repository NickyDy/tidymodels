library(tidyverse)
billboard <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv")
audio_features <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv")

max_weeks <-
  billboard %>%
  group_by(song_id) %>%
  summarise(weeks_on_chart = max(weeks_on_chart), .groups = "drop")
max_weeks

billboard_joined <-
  audio_features %>%
  filter(!is.na(spotify_track_popularity)) %>%
  inner_join(max_weeks)
billboard_joined

billboard_joined %>%
  filter(tempo > 0, time_signature > 1) %>%
  ggplot(aes(tempo, fill = factor(time_signature))) +
  geom_histogram(alpha = 0.5, position = "identity") +
  labs(fill = "time signature")

library(corrr)
billboard_joined %>%
  select(danceability:weeks_on_chart) %>%
  na.omit() %>%
  correlate() %>%
  rearrange() %>%
  network_plot(colours = c("orange", "white", "midnightblue"))

library(tidymodels)
set.seed(123)
billboard_split <- billboard_joined %>%
  select(danceability:weeks_on_chart) %>%
  mutate(weeks_on_chart = log(weeks_on_chart)) %>%
  na.omit() %>%
  initial_split(strata = weeks_on_chart)
billboard_split

billboard_train <- training(billboard_split)
billboard_test <- testing(billboard_split)

billboard_rec <-
  recipe(weeks_on_chart ~ ., data = billboard_train) %>%
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
rec_trained <- prep(billboard_rec)
rec_trained

library(ggforce)
plot_test_results <- function(recipe, dat = billboard_test) {
  recipe %>%
    prep() %>%
    bake(new_data = dat) %>%
    ggplot() +
    geom_autopoint(aes(color = weeks_on_chart), alpha = 0.4, size = 0.5) +
    geom_autodensity(alpha = .3) +
    facet_matrix(vars(-weeks_on_chart), layer.diag = 2) +
    scale_color_distiller(palette = "BuPu", direction = 1) +
    labs(color = "weeks (log)")
}

# PCA
rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  plot_test_results() +
  ggtitle("Principal Component Analysis")

rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  slice_max(abs(value), n = 5) %>%
  ungroup() %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component), scales = "free_y") +
  labs(x = "Contribution to principal component", y = NULL, fill = "Positive?")

# PLS
rec_trained %>%
  step_pls(all_numeric_predictors(), outcome = "weeks_on_chart", num_comp = 4) %>%
  plot_test_results() +
  ggtitle("Partial Least Squares")

rec_trained %>%
  step_pls(all_numeric_predictors(), outcome = "weeks_on_chart", num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PLS", 1:4)) %>%
  group_by(component) %>%
  slice_max(abs(value), n = 5) %>%
  ungroup() %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component), scales = "free_y") +
  labs(x = "Contribution to PLS component", y = NULL, fill = "Positive?")

# UMAP
library(embed)
rec_trained %>%
  step_umap(all_numeric_predictors(), num_comp = 4) %>%
  plot_test_results() +
  ggtitle("UMAP")
#===============================================================================
library(tidyverse)
load("diss.RData")

library(corrr)
diss %>%
  select(elev:herb_cover, -rare) %>%
  na.omit() %>%
  correlate(method = "spearman") %>%
  rearrange() %>%
  network_plot(colours = c("orange", "white", "midnightblue"))

library(tidymodels)
set.seed(123)
diss_split <- diss %>%
  select(elev:herb_cover, -rare) %>%
  mutate(invasive = log(invasive)) %>%
  na.omit() %>%
  initial_split(strata = invasive)
diss_split

diss_train <- training(diss_split)
diss_test <- testing(diss_split)

diss_rec <-
  recipe(invasive ~ ., data = diss_train) %>%
  step_zv(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())
rec_trained <- prep(diss_rec)
rec_trained

library(ggforce)
plot_test_results <- function(recipe, dat = diss_test) {
  recipe %>%
    prep() %>%
    bake(new_data = dat) %>%
    ggplot() +
    geom_autopoint(aes(color = invasive), alpha = 0.6, size = 0.9) +
    geom_autodensity(alpha = .3) +
    facet_matrix(vars(-invasive), layer.diag = 2) +
    scale_color_distiller(palette = "BuPu", direction = 1) +
    labs(color = "invasive (log)")
}

# PCA
rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  plot_test_results() +
  ggtitle("Principal Component Analysis")

rec_trained %>%
  step_pca(all_numeric_predictors(), num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  slice_max(abs(value), n = 5) %>%
  ungroup() %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component), scales = "free_y") +
  labs(x = "Contribution to principal component", y = NULL, fill = "Positive?")

# PLS
rec_trained %>%
  step_pls(all_numeric_predictors(), outcome = "invasive", num_comp = 4) %>%
  plot_test_results() +
  ggtitle("Partial Least Squares")

rec_trained %>%
  step_pls(all_numeric_predictors(), outcome = "invasive", num_comp = 4) %>%
  prep() %>%
  tidy(number = 3) %>%
  filter(component %in% paste0("PLS", 1:4)) %>%
  group_by(component) %>%
  slice_max(abs(value), n = 5) %>%
  ungroup() %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col(alpha = 0.8) +
  facet_wrap(vars(component), scales = "free_y") +
  labs(x = "Contribution to PLS component", y = NULL, fill = "Positive?")

# UMAP
library(embed)
rec_trained %>%
  step_umap(all_numeric_predictors(), num_comp = 4) %>%
  plot_test_results() +
  ggtitle("UMAP")
