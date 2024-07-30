library(tidyverse)
bigfoot_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')

bigfoot_raw %>%
	count(classification)

bigfoot <-
	bigfoot_raw %>%
	filter(classification != "Class C", !is.na(observed)) %>%
	mutate(
		classification = case_when(
			classification == "Class A" ~ "sighting",
			classification == "Class B" ~ "possible"
		)
	)

bigfoot

library(tidytext)
library(tidylo)

bigfoot %>%
	unnest_tokens(word, observed) %>%
	count(classification, word) %>%
	filter(n > 100) %>%
	bind_log_odds(classification, word, n) %>%
	arrange(-log_odds_weighted)

library(tidymodels)

set.seed(123)
bigfoot_split <-
	bigfoot %>%
	select(observed, classification) %>%
	initial_split(strata = classification)

bigfoot_train <- training(bigfoot_split)
bigfoot_test <- testing(bigfoot_split)

set.seed(234)
bigfoot_folds <- vfold_cv(bigfoot_train, v = 5, strata = classification)
bigfoot_folds

library(textrecipes)

bigfoot_rec <-
	recipe(classification ~ observed, data = bigfoot_train) %>%
	step_tokenize(observed) %>%
	step_tokenfilter(observed, max_tokens = 2e3) %>%
	step_tfidf(observed)

bigfoot_rec

glmnet_spec <- 
	logistic_reg(mixture = 1, penalty = tune()) %>%
	set_engine("glmnet")

bigfoot_wf <- workflow(bigfoot_rec, glmnet_spec)

doParallel::registerDoParallel()
set.seed(123)
bigfoot_res <- 
	tune_grid(
		bigfoot_wf, 
		bigfoot_folds, 
		grid = tibble(penalty = 10 ^ seq(-3, 0, by = 0.3))
	)

autoplot(bigfoot_res)

show_best(bigfoot_res)

select_by_pct_loss(bigfoot_res, desc(penalty), metric = "roc_auc")

bigfoot_final <-
	bigfoot_wf %>%
	finalize_workflow(
		select_by_pct_loss(bigfoot_res, desc(penalty), metric = "roc_auc")
	) %>%
	last_fit(bigfoot_split)

bigfoot_final

collect_metrics(bigfoot_final)

collect_predictions(bigfoot_final) %>%
	conf_mat(classification, .pred_class)

library(vip)
bigfoot_final %>%
	extract_fit_engine() %>%
	vi() 

library(vetiver)
v <- bigfoot_final %>%
	extract_workflow() %>%
	vetiver_model("bigfoot")
v

augment(v, slice_sample(bigfoot_test, n = 10))

library(plumber)
pr() %>% 
	vetiver_api(v) %>% 
	pr_run()

pr() %>% 
	vetiver_api(v, type = "prob") %>% 
	pr_run()
