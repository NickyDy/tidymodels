library(tidyverse)
employed <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv")

employed_tidy <- employed %>%
  filter(!is.na(employ_n)) %>%
  group_by(occupation = paste(industry, minor_occupation), race_gender) %>%
  summarise(n = mean(employ_n)) %>%
  ungroup()

employment_demo <- employed_tidy %>%
  filter(race_gender %in% c("Women", "Black or African American", "Asian")) %>%
  pivot_wider(names_from = race_gender, values_from = n, values_fill = 0) %>%
  janitor::clean_names() %>%
  left_join(employed_tidy %>%
              filter(race_gender == "TOTAL") %>%
              select(-race_gender) %>%
              rename(total = n)) %>%
  filter(total > 1e3) %>%
  mutate(across(c(asian, black_or_african_american, women), ~ . / (total)),
         total = log(total),
         across(where(is.numeric), ~ as.numeric(scale(.)))
  ) %>%
  mutate(occupation = snakecase::to_snake_case(occupation))
employment_demo

# Implement k-means clustering
employment_clust <- kmeans(select(employment_demo, -occupation), centers = 3)
summary(employment_clust)

library(broom)
tidy(employment_clust)

augment(employment_clust, employment_demo) %>%
  ggplot(aes(total, black_or_african_american, color = .cluster)) +
  geom_point()

# Choosing k
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(select(employment_demo, -occupation), .x)),
    glanced = map(kclust, glance),
  )

kclusts %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")

final_clust <- kmeans(select(employment_demo, -occupation), centers = 5)

library(plotly)
p <- augment(final_clust, employment_demo) %>%
  ggplot(aes(total, women, color = .cluster, name = occupation)) +
  geom_point()

ggplotly(p, height = 500)
