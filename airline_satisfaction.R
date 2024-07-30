library(tidyverse)
library(tidytext)
library(janitor)
library(corrplot)
library(treemap)
library(gridExtra)
library(scales)
library(glue)
library(patchwork)
library(skimr)
library(RColorBrewer)
library(tidymodels)
library(themis)
library(vip)

theme_set(theme_light())
airline_satisfaction <- read_csv("tidymodels/airline_clnd.csv", show_col_types = FALSE) %>% clean_names()

skim(airline_satisfaction)

airline_satisfaction <- airline_satisfaction %>%
  mutate(across(where(is.character), ~ as.factor(str_squish(str_to_title(.))))) %>%
  mutate(
    satisfaction = str_replace_all(satisfaction, "Neutral Or Dissatisfied", replacement = "No"),
    satisfaction = str_replace_all(satisfaction, "Satisfied", replacement = "Yes"),
    satisfaction = factor(satisfaction, levels = c("Yes", "No")),
    arrival_delay_in_minutes = as.numeric(str_replace_na(
      arrival_delay_in_minutes,
      mean(arrival_delay_in_minutes,
           na.rm = TRUE))))
glimpse(airline_satisfaction)

tree_plot <- function(var_to, pal = "Set1") {
  
  for_title <- as_label(enquo(var_to))
  for_title <- str_to_title(str_replace_all(for_title, "_", " "))
  airline_satisfaction %>% 
    count({{var_to}}, sort = T) %>%
    mutate(prop = str_c(round(n / sum(n) * 100, 0), "%"),
           label = str_c({{var_to}}, " ", prop)) %>%
    treemap(
      index = "label",
      vSize = "n",
      type = "index",
      title = glue(for_title," Proportions"),
      palette = pal,
      border.col = c("black"),
      border.lwds = 1,
      fontsize.labels = 18
    )
}

tree_plot(satisfaction, pal = "Dark2")

cor_mat <-
  airline_satisfaction %>% 
  select(where(is.numeric), -c(id, sr)) %>% 
  cor(use = "pairwise.complete.obs") 

corrplot(
  title = "\n\nCorrelation Matrix",
  cor_mat,
  method = "number",
  order = "alphabet",
  type = "lower",
  diag = FALSE,
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "darkgreen",
  addgrid.col = "gray")

airline_satisfaction %>% 
  group_by(gender) %>%
  summarize(counts = n()) %>%
  mutate(perc = (counts / sum(counts)) * 100) %>%
  arrange(desc(perc)) %>% 
  ggplot(aes("", counts)) +
  geom_col(
    position = "fill",
    color = "black",
    width = 1,
    aes(fill = factor(gender))
  ) +
  geom_text(
    aes(label = str_c(round(perc), "%"), group = factor(gender)),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 6,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual (values = c("#0F7078", "#42132B")) +
  theme_void() +
  labs(
    title = "Proportion of Men to Women",
    subtitle = "Pie Plot, proportion of Men to Women in Gender Var",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    fill = "")

tree_plot(class, pal = "RdYlBu")
tree_plot(type_of_travel, pal = c("#1B2B34", "#0B86C6"))
tree_plot(customer_type, pal = c("#F7495D", "#5FB9E7"))

airline_satisfaction %>% 
  select(flight_distance) %>% 
  ggplot(aes(flight_distance)) +
  geom_histogram(color = "#0E1856", fill = "#0E1856", bins = 40, alpha = 0.5) +
  geom_vline(aes(xintercept = mean(flight_distance)), color = "#FD6041", size = 1) +
  labs(
    title = "Flight distance Distributions",
    subtitle = "Histogram Plot",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Flight Distance",
    y = "Count")

airline_satisfaction %>%
  select(flight_distance, age) %>%
  ggplot(aes(x = age, y = flight_distance)) +
  geom_point(color = "#2A9C5B") +
  geom_boxplot(
    aes(y = flight_distance, group = age),
    fill = "orange2",
    alpha = 0.3,
    outlier.colour = "darkred") +
  geom_vline(xintercept = mean(airline_satisfaction$age), color = "black", size = 1) +
  theme(panel.background = element_rect(fill = "#EDECE9"),
        panel.grid = element_line(color = "white"))+
  labs(
    title = "Flight Distance and Age",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Age",
    y = "Flight Distance")

airline_satisfaction %>%
  select(gender, age, satisfaction) %>% 
  count(gender, age, satisfaction) %>% 
  arrange(desc(age)) %>% 
  ggplot(aes(x = age, y = n)) +
  geom_line(aes(color = gender), size = 1) +
  facet_wrap(vars(satisfaction))+
  geom_rug(color = "#730185") +
  scale_color_manual(values = c( "Female" = "#E60088", "Male" = "#0074BB"))+
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Salisfaction by Flight Distance and Age",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Age",
    y = "Count",
    color = NULL)
summary(airline_satisfaction$age)

airline_satisfaction %>%
  select(age, gender) %>%  
  arrange(desc(age)) %>% 
  mutate(age_group = case_when(
    age >= min(age) & age <= 10 ~ glue(min(age)," -10"),
    age > 10 & age <= 18 ~ "11-18",
    age > 18 & age <= 25 ~ "18-25",
    age > 25 & age <= 35 ~ "26-35",
    age > 35 & age <= 45 ~ "36-45",
    age > 45 & age <= 50 ~ "46-50",
    age > 50 & age <= 64 ~ "51-64",        
    age > 64 ~ "> 64"),
  age_group = factor(age_group,
    level = c(glue(min(age)," -10"), "11-18","18-25", "26-35", "36-45", "46-50", "51-64", "> 64"))) %>% 
  count(age_group, gender, name = "counts") %>% 
  mutate(perct = round(counts/sum(counts),2)) %>% 
  ggplot(aes(x = age_group, y = counts)) +
  geom_col(aes(fill = gender), size = 1) +
  scale_fill_manual(values = c( "Female" = "#E60088", "Male" = "#0074BB"))+
  labs(
    title = "Salisfaction by Flight Distance and Age",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Age",
    y = "Count",
    fill = NULL)

gruop_treemap_plot <- function(var_1, var_2 = satisfaction, var_col = "Set1"){
  
  for_title_1 <- as_label(enquo(var_1))
  for_title_1 <- str_to_title(str_replace_all(for_title_1, "_", " "))
  
  for_title_2 <- as_label(enquo(var_2))
  for_title_2 <- str_to_title(str_replace_all(for_title_2, "_", " "))
  
  
  airline_satisfaction %>%
    select({{var_1}}, {{var_2}}) %>% 
    mutate(var_1_ex = {{var_1}},
           var_2_ex = {{var_2}}) %>% 
    count(var_1_ex, var_2_ex) %>% 
    arrange(desc(n)) %>%
    treemap(title = glue(for_title_2, " proportion by ", for_title_1),
            index = c("var_1_ex", "var_2_ex"),
            vSize = "n",
            type = "index",
            palette = var_col,
            fontsize.labels = c(15, 12),
            fontcolor.labels = c("black","white"),
            fontface.labels = c(2, 2),
            align.labels = list(c("center", "center"),
                                c("left", "bottom")),
            overlap.labels = 0.5,
            border.col=c("white","gray"),
            border.lwds=c(6,1),
            inflate.labels = F
    )
  
}

gruop_treemap_plot(class)
gruop_treemap_plot(customer_type, var_col = "Accent")
gruop_treemap_plot(customer_type, var_2 = class, var_col = "Set2")
gruop_treemap_plot(type_of_travel, var_col = "Paired")
gruop_treemap_plot(type_of_travel, var_2 = class, var_col = "Set2")

lm_mod_arr_flight_departure <- lm(arrival_delay_in_minutes ~ departure_delay_in_minutes, data = airline_satisfaction)
summary(lm_mod_arr_flight_departure)

lm_mod_arr_flight_distance <- lm(arrival_delay_in_minutes ~ flight_distance, data = airline_satisfaction)
summary(lm_mod_arr_flight_distance)

airline_satisfaction_index <- initial_split( airline_satisfaction, prop = 2/3, strata = satisfaction)
arrival_depart_delay_for_plot <- testing(airline_satisfaction_index)

arrival_delay <- arrival_depart_delay_for_plot %>%
  select(flight_distance, arrival_delay_in_minutes) %>% 
  ggplot(aes(x = flight_distance, y = arrival_delay_in_minutes)) +
  geom_point( size = 1) +
  geom_rug(color = "#730185") +
  scale_x_log10() +
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Filght distance and Arrival Delays",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = " Flight Distance" ,
    y = "Arrival Delays/min",
    fill = "")
depart_delay <- arrival_depart_delay_for_plot %>%
  select(flight_distance, departure_delay_in_minutes) %>% 
  ggplot(aes(x = flight_distance, y = departure_delay_in_minutes)) +
  geom_point(size = 1) +
  geom_rug(color = "#730185") +
  scale_x_log10() +
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Filght distance and Departure Delays",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = " Flight Distance" ,
    y = "Departure Delays/min",
    fill = "")
arrival_delay + depart_delay

table_gate <- tableGrob(airline_satisfaction %>% 
                          select(gate_location, satisfaction) %>%  
                          count(gate_location, satisfaction), theme = ttheme_minimal())
airline_satisfaction %>% 
  select(gate_location, satisfaction) %>%  
  count(gate_location, satisfaction) %>% 
  ggplot(aes(x = gate_location, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Gate Location vs Satisfaction",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Gate Location" ,
    y = "Count",
    fill = "") + table_gate

table_boarding <- tableGrob(airline_satisfaction %>% 
                              select(online_boarding, satisfaction) %>%  
                              count(online_boarding, satisfaction), theme = ttheme_minimal())
airline_satisfaction %>% 
  select(online_boarding, satisfaction) %>%  
  count(online_boarding, satisfaction) %>% 
  ggplot(aes(x = online_boarding, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Online boarding vs Satisfaction",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Online boarding" ,
    y = "Count",
    fill = ""
  ) + table_boarding

airline_satisfaction %>% 
  select(seat_comfort, satisfaction, class) %>%  
  count(seat_comfort, satisfaction, class) %>% 
  ggplot(aes(x = seat_comfort, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  facet_wrap(vars(class))+
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Satisfaction by Seat Comfort and Class",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Seat Comfort" ,
    y = "Count",
    fill = "") 
airline_satisfaction %>% 
  select(departure_arrival_time_convenient, satisfaction, class) %>%  
  count(departure_arrival_time_convenient, satisfaction, class) %>% 
  ggplot(aes(x = departure_arrival_time_convenient, y = n, fill = satisfaction)) +
  geom_col(size = 1) +
  scale_fill_manual(values = c("#13AD00", "#EA0037"))+
  facet_wrap(vars(class))+
  theme(strip.background = element_rect(fill = "#673B38"),
        strip.text = element_text(face = "bold")) +
  labs(
    title = "Satisfaction by Departure Arrival Time Cconvenient and Class",
    caption = "Data Source: Airline Passenger Satisfaction Predictive Analysis",
    x = "Seat Comfort" ,
    y = "Count",
    fill = "")

airline_satisfaction_for_model <- airline_satisfaction %>%
  select(-c(id, sr))

#airline_satisfaction_for_model<-airline_satisfaction_for_model %>% slice_sample(n = 1000)

set.seed(31967)
airline_satisfaction_split <- initial_split( airline_satisfaction_for_model, prop = 3/4, strata = satisfaction)
train_data <- training(airline_satisfaction_split)
test_data  <- testing(airline_satisfaction_split)

train_data %>%
  count(satisfaction) %>% 
  mutate(prop = n/sum(n))

test_data  %>%
  count(satisfaction) %>% 
  mutate(prop = n/sum(n))

set.seed(31967)
fold_cv <- vfold_cv(train_data, times = 10, apparent = TRUE)

#First Model: Penalized Logistic Regression--------------------------------
logis_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

logis_recipe <-
  recipe(satisfaction ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>% 
  step_corr(all_numeric_predictors(), threshold = .7)
#summary(logis_recipe)

logis_workflow <- 
  workflow() %>% 
  add_model(logis_mod) %>% 
  add_recipe(logis_recipe)

logis_reg_grid <- tibble(penalty = 10^seq(-3, -1, length.out = 60))
logis_reg_grid %>% top_n(-5) 
logis_reg_grid %>% top_n(5)

doParallel::registerDoParallel()
set.seed(31967)
logis_res <- 
  logis_workflow %>% 
  tune_grid(fold_cv,
            grid = logis_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

logis_plot <- 
  logis_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  scale_x_log10(labels = scales::label_number()) +
  theme(
    plot.background = element_rect(fill = "#FFF1B2")) +
  labs(
    title = "Area under the ROC Curve",
    x= "Penalty",
    y = "Area under the ROC Curve")
logis_plot 

top20_models <-
  logis_res %>% 
  show_best("roc_auc", n = 40) %>% 
  arrange(desc(mean))
top20_models

logis_best <- 
  logis_res %>% 
  show_best() %>% 
  arrange(desc(mean)) %>%
  dplyr::slice(2)
logis_best

logis_auc <- 
  logis_res %>% 
  collect_predictions(parameters = logis_best) %>% 
  roc_curve(satisfaction, .pred_Yes) %>% 
  mutate(model = "Logistic Regression")
autoplot(logis_auc)

set.seed(31967)
final_logis_res <-
  logis_workflow %>%
  finalize_workflow(logis_best) %>%
  last_fit(airline_satisfaction_split)
final_logis_res

collect_metrics(final_logis_res)
confusion_matrix_logis <- collect_predictions(final_logis_res) %>%
  conf_mat(satisfaction, .pred_class)
collect_predictions(final_logis_res) %>%
  conf_mat(satisfaction, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Truth, Prediction, alpha = n)) +
  geom_tile(show.legend = FALSE, fill = "orange") +
  geom_text(aes(label = n), colour = "#2F423D", alpha = 1, size = 7) +
  scale_x_discrete(position = "top", limits = c("Yes","No"))

collect_predictions(final_logis_res) %>%
  metrics(satisfaction, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")
summary(confusion_matrix_logis)

#Second Model: Tree-Based Random Forest------------------------------
rf_rec <- 
  recipe(satisfaction ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric())

rf_rec %>% prep() %>%
juice() %>% select(satisfaction) %>% table()
summary(rf_rec)

rf_mod <- 
  rand_forest(mtry = tune(), 
              min_n = tune(),
              trees = tune()) %>% 
  set_engine("ranger",  importance = "impurity") %>% 
  set_mode("classification")

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_rec)

doParallel::registerDoParallel()
set.seed(31967)
rf_res <- 
  rf_workflow %>% 
  tune_grid(fold_cv,
            grid = 10,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
unnest(.metrics) %>% 
arrange(desc(.estimate))

rf_res %>% 
  show_best(metric = "roc_auc")

rf_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry, trees) %>%
  pivot_longer(cols = c(mtry, min_n,trees),
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE, size = 2) +
  facet_wrap(~parameter, scales = "free_x") +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "#483248"),
    strip.text = element_text(color = "white", face = "bold", size = 10),
    plot.background = element_rect(colour = "gray"),
    plot.title.position = "panel",
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.subtitle = element_text(size = 8),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 6, color = "grey"),
    axis.text.y = element_text(size = 6)) +
  labs(title = "The metrics for all models",
       x = NULL,
       y = "AUC")

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

finalize_workflow(
  rf_workflow,
  rf_best)

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(satisfaction, .pred_Yes) %>% 
  mutate(model = "Random Forest")

bind_rows(rf_auc, logis_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)

set.seed(31967)
final_rf_fit <-
  rf_workflow %>%
  finalize_workflow(rf_best) %>%
  last_fit(airline_satisfaction_split)
final_rf_fit

collect_metrics(final_rf_fit)

cunfusion_matrix_rf <- collect_predictions(final_rf_fit) %>%
  conf_mat(satisfaction, .pred_class)

collect_predictions(final_rf_fit) %>%
  conf_mat(satisfaction, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Truth, Prediction, alpha = n)) +
  geom_tile(show.legend = FALSE, fill = "orange") +
  geom_text(aes(label = n), colour = "#2F423D", alpha = 1, size = 7) +
  scale_x_discrete(position = "top", limits = c("Yes","No"))

collect_predictions(final_rf_fit) %>%
  metrics(satisfaction, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == c("accuracy"))

final_rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(satisfaction, .pred_Yes)

final_rf_auc %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(lwd = 1.5, alpha = 0.8, color = "#263F75") +
  geom_abline(lty = 3) +
  coord_equal() +
  theme(
    legend.position = "top",
    plot.background = element_rect(colour = "steelblue", fill = "#B6C99B"),
    panel.background = element_rect(colour = "steelblue", fill = "#E8F0DC"),
    plot.title.position = "panel",
    plot.title = element_text(
      size = 18,
      hjust = 0.5,
      color = "#010FCC"),
    plot.subtitle = element_text(size = 8),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 10, color = "grey"),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 10, color = "black")) +
  labs(
    title = "Final ROC Curve (Random Forest)",
    x = "False Positive Rate (1-Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = NULL)

summary(cunfusion_matrix_rf)

final_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "col", horizontal = TRUE, 
      aesthetics = list(color = "black", fill = "#1F6CA2", size = 0.5)) +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.title.position = "panel",
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "grey"),
    panel.background = element_rect(fill = "#C9D4C7"),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)) +
  labs(
    title = "Variable Importance",
    caption = "Data Source: Kaggle IBM HR Employee Attrition",
    x = "Variables",
    y = "Importance")

final_rf_fit %>%
  pluck(".workflow", 1) %>%
  extract_fit_parsnip() %>% vi(num_features = 20)
