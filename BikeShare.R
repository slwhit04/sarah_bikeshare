# load libraries

library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(ggplot2)
library(patchwork)
library(skimr)


# load data
train <- vroom("C:/Users/19132/Downloads/train.csv")
test <- vroom("C:/Users/19132/Downloads/test.csv")

View(train)
View(test)


### eda

# Skim the dataset
skim(train)

### ggplot thing

plot_weather <- ggplot(train, aes(x = factor(weather))) +
  geom_bar(fill = "pink") +
  labs(title = "Weather Distribution", x = "Weather", y = "Count")

plot_temp <- ggplot(train, aes(x = temp)) +
  geom_histogram(fill = "pink", bins = 30) +
  labs(title = "Temperature Distribution", x = "Temperature", y = "Frequency")

plot_holiday <- ggplot(train, aes(x = factor(holiday), y = count)) +
  geom_boxplot(fill = "pink", alpha = 0.6) +
  labs(title = "Bike Count by Holiday", x = "Holiday (0 = No, 1 = Yes)", y = "Count")

plot_season <- ggplot(train, aes(x = factor(season), y = count)) +
  geom_boxplot(fill = "pink", alpha = 0.6) +
  labs(title = "Bike Count by Season", x = "Season", y = "Count")

# Combine all panels
combined_plot <- (plot_weather | plot_temp) / (plot_holiday | plot_season)

# Show combined plot
print(combined_plot)

### cleaning

train <- train |> 
  select(-casual, -registered) |> 
  mutate(count = log1p(count))

bike_recipe <- recipe(count ~ ., data = train) |> 
  # extract features from datetime
  step_mutate(hour = hour(datetime),
              wday = wday(datetime),
              month = month(datetime),
              year = year(datetime)) |> 
  
  # cyclic encodings for hour
  step_mutate(
    sin_hour = sin(2 * pi * hour / 24),
    cos_hour = cos(2 * pi * hour / 24)
  ) |> 
  
  # cyclic encodings for day of week
  step_mutate(
    sin_wday = sin(2 * pi * wday / 7),
    cos_wday = cos(2 * pi * wday / 7)
  ) |> 
  
  # cyclic encodings for month
  step_mutate(
    sin_month = sin(2 * pi * month / 12),
    cos_month = cos(2 * pi * month / 12)
  ) |> 
  
  # remove raw datetime + raw hour/wday/month
  step_rm(datetime, hour, wday, month) |> 
  
  # handle categoricals & scaling
  step_dummy(all_nominal_predictors()) |>   
  step_normalize(all_numeric_predictors())


  

### penalized regression

# --- OLD SINGLE FIT (commented out now) ---
# my_model <- linear_reg(
#   penalty = 0.01,   # chosen penalty
#   mixture = 0.5     # chosen mixture
# ) |> 
#   set_engine("glmnet")
#
# wf <- workflow() |> 
#   add_model(my_model) |> 
#   add_recipe(bike_recipe)
#
# final_fit <- wf |> 
#   fit(data = train)
#
# ### Predictions (old single fit)
# bike_predictions <- predict(final_fit, new_data = test)

# --- NEW: TUNED PENALIZED REGRESSION ---

## Define model with tunable penalty & mixture
preg_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_engine("glmnet")

## Workflow
preg_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(preg_model)

## Grid of tuning parameters
grid_of_tuning_params <- grid_regular(
  penalty(range = c(-4, 1)),  
  mixture(),                  
  levels = 10                 
)


## Cross-validation folds
set.seed(4)  # for reproducibility
folds <- vfold_cv(train, v = 5)

## Run CV
CV_results <- preg_wf |> 
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae)
  )

## Plot RMSE vs penalty (optional visualization)
collect_metrics(CV_results) |> 
  filter(.metric == "rmse") |> 
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

## Find best tuning parameters
bestTune <- CV_results |> 
  select_best(metric = "rmse")

## Finalize workflow
final_wf <- preg_wf |> 
  finalize_workflow(bestTune) |> 
  fit(data = train)

## Predictions with tuned model
bike_predictions <- predict(final_wf, new_data = test)

### Format the Predictions for Kaggle
kaggle_submission <- bike_predictions |> 
  bind_cols(test) |>                                   
  mutate(count = exp(.pred) - 1) |>                        
  mutate(count = round(pmax(0, count))) |>                
  select(datetime, count) |>                             
  mutate(datetime = as.character(format(datetime))) 

### Write out the file
vroom_write(x = kaggle_submission, file = "./kagglesubmission3.csv", delim = ",")
