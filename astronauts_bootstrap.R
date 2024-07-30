library(tidyverse)
astronauts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv")

astronauts %>%
  count(in_orbit, sort = TRUE)

astronauts %>%
  mutate(
    year_of_mission = 10 * (year_of_mission %/% 10),
    year_of_mission = factor(year_of_mission)) %>%
  ggplot(aes(year_of_mission, hours_mission,
             fill = year_of_mission, color = year_of_mission)) +
  geom_boxplot(alpha = 0.2, size = 1.5, show.legend = FALSE) +
  scale_y_log10() +
  labs(x = NULL, y = "Duration of mission in hours")

astronauts_df <- astronauts %>%
  select(name, mission_title, hours_mission,
    military_civilian, occupation, year_of_mission, in_orbit) %>%
  mutate(in_orbit = case_when(
      str_detect(in_orbit, "^Salyut") ~ "Salyut",
      str_detect(in_orbit, "^STS") ~ "STS",
      TRUE ~ in_orbit),
    occupation = str_to_lower(occupation)) %>%
  filter(hours_mission > 0) %>%
  mutate(hours_mission = log(hours_mission)) %>%
  na.omit()

library(tidymodels)
set.seed(123)
astro_split <- initial_split(astronauts_df, strata = hours_mission)
astro_train <- training(astro_split)
astro_test <- testing(astro_split)

astro_recipe <- recipe(hours_mission ~ ., data = astro_train) %>%
  update_role(name, mission_title, new_role = "id") %>%
  step_other(occupation, in_orbit, threshold = 0.005, other = "Other") %>%
  step_dummy(all_nominal(), -has_role("id"))

astro_rec<-astro_recipe %>% prep() %>% juice()

astro_wf <- workflow() %>%
  add_recipe(astro_recipe)
astro_wf

library(baguette)
tree_spec <- bag_tree() %>%
  set_engine("rpart", times = 25) %>%
  set_mode("regression")
tree_spec

mars_spec <- bag_mars() %>%
  set_engine("earth", times = 25) %>%
  set_mode("regression")
mars_spec

tree_rs <- astro_wf %>%
  add_model(tree_spec) %>%
  fit(astro_train)
tree_rs

mars_rs <- astro_wf %>%
  add_model(mars_spec) %>%
  fit(astro_train)
mars_rs

test_rs <- astro_test %>%
  bind_cols(predict(tree_rs, astro_test)) %>%
  rename(.pred_tree = .pred) %>%
  bind_cols(predict(mars_rs, astro_test)) %>%
  rename(.pred_mars = .pred)
test_rs

test_rs %>%
  metrics(hours_mission, .pred_tree)

test_rs %>%
  metrics(hours_mission, .pred_mars)

new_astronauts <- crossing(
  in_orbit = fct_inorder(c("ISS", "STS", "Mir", "Other")),
  military_civilian = "civilian",
  occupation = "Other",
  year_of_mission = seq(1960, 2020, by = 10),
  name = "id", mission_title = "id") %>%
  filter(
    !(in_orbit == "ISS" & year_of_mission < 2000),
    !(in_orbit == "Mir" & year_of_mission < 1990),
    !(in_orbit == "STS" & year_of_mission > 2010),
    !(in_orbit == "STS" & year_of_mission < 1980))
new_astronauts

new_astronauts %>%
  bind_cols(predict(tree_rs, new_astronauts)) %>%
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    x = NULL, y = "Duration of mission in hours (predicted, on log scale)",
    color = NULL, title = "How did the duration of astronauts' missions change over time?",
    subtitle = "Predicted using bagged decision tree model")

new_astronauts %>%
  bind_cols(predict(mars_rs, new_astronauts)) %>%
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    x = NULL, y = "Duration of mission in hours (predicted, on log scale)",
    color = NULL, title = "How did the duration of astronauts' missions change over time?",
    subtitle = "Predicted using bagged MARS model")
