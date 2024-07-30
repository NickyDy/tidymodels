library(tidyverse) # import, transform and visualize data
library(tidymodels) # modeling
library(vip) # variable importance plots
library(hrbrthemes) # great looking themes for ggplot2
library(patchwork) # combine ggplots into the same graphic
library(glue) # insert strings in some plot titles

train <- read_csv(file = "tidymodels/titanic_train.csv")
test <- read_csv(file = "tidymodels/titanic_test.csv")
titanic_orig <- bind_rows(train, test)

glimpse(titanic_orig)

train %>% 
  count(Survived) %>%
  mutate(frac = n / sum(n),
         Survived = as.factor(Survived)) %>% 
  ggplot(aes(x = reorder(Survived, frac),
             y = frac,
             fill = Survived)) +
  geom_col(show.legend = FALSE,
           width = .7) +
  geom_text(aes(label = paste0(round(100 * frac), "%")),
            fontface = "bold",
            col = "white",
            hjust = 1.1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentege of passengers that survived\non Titanic",
       subtitle = "0: Didn't survive\n1: Survived",
       x = "",
       y = "",
       caption = glue("Based on training data (N = {nrow(train)})")) +
  theme_ipsum_tw(grid = "X") +
  coord_flip()

name_title <-
  str_extract(titanic_orig$Name, "(,) [A-Za-z]*") %>%
  sub(pattern = ", ", replacement = "")

common_titles <- c("Master", "Miss", "Mr", "Mrs")

sum(name_title %in% common_titles)/sum(!is.na(name_title))

name_title[name_title == "Ms"] <- "Miss"
name_title[name_title == "Mlle"] <- "Miss"
name_title[name_title == "Mme"] <- "Mrs"

deck <- str_sub(titanic_orig$Cabin, 1, 1)
mean(is.na(deck))

knitr::include_graphics("https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Titanic_cutaway_diagram.png/800px-Titanic_cutaway_diagram.png")

family_size <- titanic_orig$SibSp + titanic_orig$Parch + 1
summary(family_size)

titanic_mut <- titanic_orig %>%
  select(PassengerId, Survived, Pclass, Sex, Age, Fare, Embarked) %>%
  mutate(Deck = deck,
         Title = name_title,
         Title = if_else(condition = Title %in% common_titles,
                         true = Title,
                         false = "Other"),
         FamilySize = case_when(family_size == 1 ~ "Single",
                                family_size > 4 ~ "Large",
                                TRUE ~ "Small")) %>% 
  mutate_at(vars(-one_of(c("Age", "Fare"))), as.factor)

titanic_train <- titanic_mut[1:891, ]
titanic_test <- titanic_mut[892:1309, ]

titanic_mut %>%
  select(-Survived) %>% 
  map_dfr(~sum(is.na(.)))

ggplot(titanic_train) +
  geom_histogram(aes(x = Age, fill = Survived),
                 alpha = .4, col = NA,
                 bins = 30) +
  facet_wrap(Sex~Pclass) +
  labs(x = "Age in years",
       y = "",
       title = "Survivors by sex, age and ticket class",
       subtitle = "0: Didn't survive\n1: Survived") +
  theme_ipsum_tw(grid = "XY")

ggplot(titanic_train, aes(x = Pclass, y = Fare)) +
  geom_violin(col = "darkgray") +
  geom_jitter(shape = 21,
              width = .1,
              alpha = .2,
              col = "darkgray",
              aes(fill =
                    ifelse(Fare < mean(Fare),
                           "Bellow the mean",
                           "Above the mean"))) +
  geom_hline(yintercept = mean(titanic_train$Fare),
             lty = 2,
             col = "black") +
  scale_y_log10() +
  labs(x = "Ticket class",
       y = "Passenger fare",
       title = "Distribution of fare according to\nticket class",
       subtitle = "Compared to the mean fare (32.20)",
       fill = "Fare") +
  scale_fill_brewer(palette = "BuPu",
                    guide = guide_legend(override.aes =
                                           list(size = 3,
                                                alpha = 1))) +
  theme_ipsum_rc(grid = "Y")

ggplot(titanic_train) +
  geom_density_2d_filled(aes(x = Age, y = Fare))+
  facet_wrap(Sex ~ Survived) +
  scale_y_log10() +
  labs(x = "Age in years",
       y = "Fare",
       title = "Density of survivors by sex, age and fare",
       subtitle = "0: Didn't survive\n1: Survived") +
  scale_fill_brewer() +
  theme_ipsum_tw(grid = "")

plot_title <-
  titanic_train %>% 
  group_by(Survived, Title) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  mutate(n = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(Title, n),
               y = n, fill = Survived),
           position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum_tw(grid = "X") +
  labs(x = "",
       y = "",
       title = "Percentage of passengers\nthat survived according\nto their titles",
       subtitle = "1: Survived\n0: Didn't survive")

plot_family <-
  titanic_train %>% 
  group_by(FamilySize, Survived) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  mutate(n = n / sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(FamilySize, n),
               y = n, fill = Survived),
           position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum_tw(grid = "X") +
  labs(x = "",
       y = "",
       title = "Percentage of passengers\nthat survived according\nto their family size",
       subtitle = "1: Survived\n0: Didn't survive")


plot_title / plot_family + plot_layout(guides = "collect")

titanic_rec <-
  # define the predictors and outcome
  recipe(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize,
         data = titanic_train) %>%
  # imputation for Fare, Embarked and Age
  step_impute_knn(neighbors = 5,
                 Fare, Embarked, Age)

titanic_prep <- prep(titanic_rec, new_data = NULL)
titanic_train_prep <- juice(titanic_prep)

titanic_train_prep %>% 
  map_dbl(~sum(is.na(.)))

beforePlot <- ggplot(filter(titanic_train, !is.na(Age)),
                     aes(x = Age)) +
  geom_histogram(fill = "Navyblue",
                 bins = 30) +
  labs(x = "Age, in years",
       y = "Count",
       title = "Before KNN\nimputation",
       subtitle = glue("Mean of {round(mean(titanic_train$Age, na.rm = TRUE), 2)} years")) +
  theme_ipsum_rc()

afterPlot <- ggplot(filter(titanic_train_prep, !is.na(Age)),
                    aes(x = Age)) +
  geom_histogram(fill = "Navyblue",
                 bins = 30) +
  labs(x = "Age, in years",
       y = "Count",
       title = "After KNN\nimputation",
       subtitle = glue("Mean of {round(mean(titanic_train_prep$Age), 2)} years")) +
  theme_ipsum_rc()

beforePlot + afterPlot

rf_model <-
  # Specify a random forest model
  rand_forest() %>% 
  # Tune arguments
  set_args(mtry = tune(), trees = tune(), importance = TRUE) %>%
  # Package {ranger}
  # The 'impurity' measure is the Gini index for classification
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_wflow <- 
  workflow() %>% 
  add_recipe(titanic_rec) %>% 
  add_model(rf_model)
rf_wflow

set.seed(123)
titanic_cv <- vfold_cv(titanic_train, v = 10)

rf_grid <- expand.grid(mtry = c(2, 3, 4, 5), trees = c(100, 300, 500, 1000, 3000, 5000))

rf_tune_results <-
  rf_wflow %>% 
  tune_grid(
    resamples = titanic_cv,
    grid = rf_grid,
    metrics = metric_set(accuracy, roc_auc))

autoplot(rf_tune_results) +
  theme_ipsum_rc() +
  scale_color_ft()

rf_tune_results %>% 
  show_best("roc_auc")

rf_tune_results %>%
  show_best("accuracy")

param_final <-
  rf_tune_results %>%
  select_best(metric = "roc_auc")
param_final

rf_fit <- rf_wflow %>%
  finalize_workflow(param_final) %>%
  fit(titanic_train)
rf_fit

predict(rf_fit,
        new_data = titanic_train,
        type = "prob") %>%
  bind_cols(select(titanic_train, Survived)) %>%
  roc_curve(Survived, .pred_0) %>% 
  autoplot() +
  labs(title = "ROC AUC with training data") +
  theme_ipsum_rc()

rf_fit_cv <- rf_wflow %>%
  finalize_workflow(param_final) %>%
  fit_resamples(titanic_cv)

collect_metrics(rf_fit_cv)

rf_fit %>%
  extract_fit_parsnip() %>% 
  vip() +
  labs(title = "What are the most important\nvariables?") +
  theme_ipsum_rc()

pred <- predict(rf_fit, new_data = titanic_test)

file <-
  titanic_test %>% 
  mutate(Survived = pred$.pred_class) %>% 
  select(PassengerId, Survived)

write.csv(file, "result.csv", row.names = FALSE)
