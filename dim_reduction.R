
library(tidyverse)
unvotes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv")
issues <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv")

unvotes_df <- unvotes %>%
  select(country, rcid, vote) %>%
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid)
  ) %>%
  pivot_wider(names_from = "rcid", values_from = "vote", values_fill = 2)

library(recipes)
pca_rec <- recipe(~., data = unvotes_df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)
pca_prep <- prep(pca_rec)
pca_prep

bake(pca_prep, new_data = NULL) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point(color = "midnightblue", alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

pca_comps <- tidy(pca_prep, 2) %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  left_join(issues %>% mutate(terms = paste0("rcid_", rcid))) %>%
  filter(!is.na(issue)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup()

pca_comps %>%
  mutate(value = abs(value)) %>%
  ggplot(aes(value, fct_reorder(terms, value), fill = issue)) +
  geom_col(position = "dodge") +
  facet_wrap(~component, scales = "free_y") +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = NULL,
    title = "What issues are most important in UN voting country differences?",
    subtitle = "Human rights and economic development votes account for the most variation"
  )

library(embed)
umap_rec <- recipe(~., data = unvotes_df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)
umap_prep

bake(umap_prep, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country)) +
  geom_point(color = "midnightblue", alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)
