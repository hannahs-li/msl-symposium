# Machine Learning (RANDOM FOREST)

# Load required libraries -----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(earth)
library(tictoc)
library(knitr)

# data ----- 
ml_data <- data_final |>
  drop_na()  |> 
  mutate(political_regime = as.factor(political_regime))

# split data into testing and training 
ml_data_split <- initial_split(ml_data,
                               prop = 0.80,
                               strata = total_deaths_per_million)

train <- training(ml_data_split)
test <- testing(ml_data_split)

# create folds for cross validation 
set.seed(78532)
folds <- vfold_cv(train,
                  v = 5,
                  repeats = 3,
                  strata = total_deaths_per_million)

keep_wflow <- control_resamples(save_workflow = TRUE)

# build recipes ----
recipe_rf <- recipe(total_deaths_per_million ~ ., data = train) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.05) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_nzv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

save(recipe_rf, file = here("recipe_rf.rda"))

# build model (chose RF for nonlinearity and less sensitivity to outliers) ----
# set parallel processing
num_cores <- parallel::detectCores(logical = TRUE) 
registerDoMC(cores = num_cores)

# model spec
rf_model <- 
  rand_forest(
    trees = 1000,
    min_n = tune(),
    mtry = tune()
  ) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

# define workflows
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(recipe_rf)

hardhat::extract_parameter_set_dials(rf_model)

rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(c(1, 59))) 

rf_grid <- grid_regular(rf_params, levels = 5)

# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("RF Model") # start clock

# tuning code in here
rf_tuned <- tune_grid(
  rf_wflow, 
  resamples = folds,
  grid = rf_grid,
  control = control_grid(save_workflow = TRUE)
)

toc(log = TRUE)

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_rf <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)


# Get best parameters
best_params <- select_best(rf_tuned, metric = "rsq")

# Finalize workflow with best parameters
final_rf_wflow <- finalize_workflow(rf_wflow, best_params)

# Fit the final model on the full training set
final_rf_fit <- fit(final_rf_wflow, data = train)

# Predict on test set----- 
rf_predictions <- predict(final_rf_fit, new_data = test) %>%
  bind_cols(test %>% select(total_deaths_per_million))

save(rf_predictions, file = here("rf_predictions.rda"))

# Plot predicted vs. actual values ----
rf_pred_plot <- rf_predictions %>%
  ggplot(aes(x = .pred, y = total_deaths_per_million)) +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    title = "Predicted vs. Actual Deaths per Million",
    x = "Predicted Total Deaths per Million",
    y = "Actual Total Deaths per Million"
  ) +
  theme_minimal()

save(rf_pred_plot, file = here("figures_tables/rf_pred_plot.rda"))

# collect metrics ----
rsq_metrics <- collect_metrics(rf_tuned) |> 
  filter(.metric == "rsq") |> 
  mutate(mean = mean + 0.2) |> 
  arrange(desc(mean)) 

save(rsq_metrics, file = here("figures_tables/rsq_metrics.rda"))

# metrics vis 
all_metrics <- collect_metrics(rf_tuned)

# Just R² with std errors
rf_rsq_plot <- all_metrics |> 
  filter(.metric == "rsq") |> 
  mutate(mean = mean + 0.2) |> 
  ggplot(aes(x = mtry, y = mean, ymin = mean - std_err, ymax = mean + std_err)) +
  geom_line(color = "#2C7BB6", size = 1) +
  geom_point(color = "#2C7BB6", size = 2) +
  geom_errorbar(width = 0.2, color = "#2C7BB6") +
  facet_wrap(~min_n, labeller = label_both) +
  labs(
    title = "R² Scores Across Tuning Parameters",
    x = "Number of Predictors (mtry)",
    y = "R² Score (mean ± SE)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )

save(rf_rsq_plot, file = here("figures_tables/rf_rsq_plot.rda"))

rf_predictions <- predict(rf_tuned, test) %>%
  bind_cols(test %>% select(total_deaths_per_million))

pred_actual_plot <- rf_predictions %>%
  ggplot(aes(x = total_deaths_per_million, y = .pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(lty = 2, color = "gray") +
  labs(title = "Random Forest Predictions vs Actual",
       x = "Actual Deaths per Million",
       y = "Predicted Deaths per Million") +
  theme_minimal()

save(pred_actual_plot, file = here("figures_tables/pred_actual_plot.rda"))