# sin and cosine hour and time?
# break hour into factor by hour in the week so there is a whooole bunch of 0's and 1's


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
  
  # (optional) cyclic encodings for day of week (7 days)
  step_mutate(
    sin_wday = sin(2 * pi * wday / 7),
    cos_wday = cos(2 * pi * wday / 7)
  ) |> 
  
  # remove raw datetime + raw hour/wday if you don’t want them duplicated
  step_rm(datetime, hour, wday) |> 
  
  # handle categoricals & scaling
  step_dummy(all_nominal_predictors()) |>   
  step_normalize(all_numeric_predictors())


### penalized regresssion
### Fit penalized regression with one penalty/mixture combo
my_model <- linear_reg(
  penalty = 0.001,   # chosen penalty
  mixture = 0.5     # chosen mixture
) |> 
  set_engine("glmnet")

wf <- workflow() |> 
  add_model(my_model) |> 
  add_recipe(bike_recipe)

final_fit <- wf |> 
  fit(data = train)

### Predictions
bike_predictions <- predict(final_fit, new_data = test)

### Format the Predictions for Kaggle
kaggle_submission <- bike_predictions |> 
  bind_cols(test) |>                                   
  mutate(count = exp(.pred) - 1) |>                        
  mutate(count = round(pmax(0, count))) |>                
  select(datetime, count) |>                             
  mutate(datetime = as.character(format(datetime))) 

### Prep data (optional: to inspect)
data <- prep(bike_recipe) |> 
  bake(new_data = train)
head(data, 5)

### Write out the file
vroom_write(x = kaggle_submission, file = "./kagglesubmission3.csv", delim = ",")
