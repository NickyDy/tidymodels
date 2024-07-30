library(tidyverse)
tournament <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv")

tournament

tournament %>%
  group_by(seed) %>%
  summarise(exp_wins = mean(tourney_w, na.rm = TRUE)) %>%
  ggplot(aes(seed, exp_wins)) +
  geom_point(alpha = 0.8, size = 3) +
  labs(y = "tournament wins (mean)")

tournament %>%
  ggplot(aes(seed, tourney_w)) +
  geom_bin2d(binwidth = c(1, 1), alpha = 0.8) +
  scale_fill_gradient(low = "gray85", high = "midnightblue") +
  labs(fill = "number of\nteams", y = "tournament wins")

library(splines)
plot_smoother <- function(deg_free) {
  p <- ggplot(tournament, aes(seed, tourney_w)) +
    geom_bin2d(binwidth = c(1, 1), alpha = 0.8) +
    scale_fill_gradient(low = "gray85", high = "midnightblue") +
    geom_smooth(
      method = lm, se = FALSE, color = "black",
      formula = y ~ ns(x, df = deg_free)
    ) +
    labs(
      fill = "number of\nteams", y = "tournament wins",
      title = paste(deg_free, "spline terms"))
  
  print(p)
}

walk(c(2, 4, 6, 8, 10, 15), plot_smoother)

library(tidymodels)
set.seed(123)
tourney_split <- tournament %>%
  filter(!is.na(seed)) %>%
  initial_split(strata = seed)

tourney_train <- training(tourney_split)
tourney_test <- testing(tourney_split)

set.seed(234)
tourney_folds <- bootstraps(tourney_train)
tourney_folds

tourney_rec <- recipe(tourney_w ~ seed, data = tourney_train) %>%
  step_ns(seed, deg_free = tune("seed_splines"))
tourney_rec

lm_spec <- linear_reg() %>% set_engine("lm")
tourney_wf <- workflow() %>%
  add_recipe(tourney_rec) %>%
  add_model(lm_spec)
tourney_wf

spline_grid <- tibble(seed_splines = c(1:3, 5, 7, 10))
spline_grid

doParallel::registerDoParallel()
save_preds <- control_grid(save_pred = TRUE)
spline_rs <-
  tune_grid(
    tourney_wf,
    resamples = tourney_folds,
    grid = spline_grid,
    control = save_preds)
spline_rs

collect_metrics(spline_rs)

collect_metrics(spline_rs) %>%
  ggplot(aes(seed_splines, mean, color = .metric)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~.metric, ncol = 1, scales = "free_y") +
  labs(x = "degrees of freedom", y = NULL) +
  theme(legend.position = "none")

select_by_pct_loss(spline_rs, metric = "rmse", limit = 5, seed_splines)

select_by_one_std_err(spline_rs, metric = "rmse", seed_splines)

final_wf <- finalize_workflow(tourney_wf, tibble(seed_splines = 3))
tourney_fit <- fit(final_wf, tourney_train)
tourney_fit

tourney_test %>%
  bind_cols(predict(tourney_fit, tourney_test)) %>%
  metrics(tourney_w, .pred)

predict(tourney_fit, new_data = tibble(seed = 1:16))
