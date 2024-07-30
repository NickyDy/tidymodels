library(tidyverse)
papers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv")
programs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv")
paper_authors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv")
paper_programs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv")

papers_joined <-
  paper_programs %>%
  left_join(programs) %>%
  left_join(papers) %>%
  filter(!is.na(program_category)) %>%
  distinct(paper, program_category, year, title)
papers_joined %>%
  count(program_category)

library(tidytext)
library(tidylo)
title_log_odds <-
  papers_joined %>%
  unnest_tokens(word, title) %>%
  filter(!is.na(program_category)) %>%
  count(program_category, word, sort = TRUE) %>%
  bind_log_odds(program_category, word, n)
title_log_odds %>%
  group_by(program_category) %>%
  slice_max(log_odds_weighted, n = 10) %>%
  ungroup() %>%
  ggplot(aes(log_odds_weighted,
             fct_reorder(word, log_odds_weighted),
             fill = program_category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(program_category), scales = "free_y") +
  labs(x = "Log odds (weighted)", y = NULL)

#SPLITTING THE DATA (DATA BUDGET)
library(tidymodels)
set.seed(123)
nber_split <- initial_split(papers_joined, strata = program_category)
nber_train <- training(nber_split)
nber_test <- testing(nber_split)

#SETTING UP THE CROSSVALIDATION
set.seed(234)
nber_folds <- vfold_cv(nber_train, strata = program_category)
nber_folds

#FEATURE ENGINEERING
library(themis)
library(textrecipes)
nber_rec <-
  recipe(program_category ~ year + title, data = nber_train) %>%
  step_tokenize(title) %>%
  step_tokenfilter(title, max_tokens = 200) %>%
  step_tfidf(title) %>%
  step_downsample(program_category)
nber_rec

#MODEL SPECIFICATION
multi_spec <-
  multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
multi_spec

#CREATING A WORKFLOW
nber_wf <- workflow(nber_rec, multi_spec)
nber_wf

#TUNING THE GRID OF PARAMETERS
nber_grid <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

doParallel::registerDoParallel()
set.seed(2021)
nber_rs <-
  tune_grid(
    nber_wf,
    nber_folds,
    grid = nber_grid)
nber_rs

autoplot(nber_rs)

show_best(nber_rs)

#CHOOSE AND EVALUATE THE FINAL MODEL
final_penalty <-
  nber_rs %>%
  select_by_one_std_err(metric = "roc_auc", desc(penalty))
final_penalty

final_rs <-
  nber_wf %>%
  finalize_workflow(final_penalty) %>%
  last_fit(nber_split)
final_rs

collect_metrics(final_rs)

#CONFUSION MATRIX
collect_predictions(final_rs) %>%
  conf_mat(program_category, .pred_class) %>%
  autoplot()

#ROC CURVE
collect_predictions(final_rs) %>%
  roc_curve(truth = program_category, .pred_Finance:.pred_Micro) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(slope = 1, color = "gray50", lty = 2, alpha = 0.8) +
  geom_path(size = 1.5, alpha = 0.7) +
  labs(color = NULL) +
  coord_fixed()

#EXTRACT AND SAVE FITTED WORKFLOW
final_fitted <- extract_workflow(final_rs)
readr::write_rds()

#TESTING THE PREDICTION
predict(final_fitted, nber_test[111, ], type = "prob")

predict(final_fitted, tibble(year = 2021, title = "Pricing Models for Corporate Responsibility"), type = "prob")

predict(final_fitted, tibble(year = 2021, title = "Teacher Health and Medicaid Expansion"), type = "prob")