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
  step_mutate(hour = hour(datetime)) |> 
  step_mutate(season = factor(season)) |> 
  step_mutate(month = month(datetime)) |> 
  step_rm(datetime)



# # train
# 
# train <- train |> 
#   mutate(
#     hour = hour(datetime),
#     wday = wday(datetime, label = TRUE),
#     month = month(datetime),
#     year = year(datetime),
#     log_count = log1p(count)  
#   )
# 
# 
# ### test
# 
# test <- test |> 
#   mutate(
#     hour = hour(datetime),
#     wday = wday(datetime, label = TRUE),
#     month = month(datetime),
#     year = year(datetime)
#   )



### Linear Regression Model
my_linear_model <- workflow() |> 
  add_model(
    linear_reg() |> 
      set_engine("lm") |> 
      set_mode("regression")) |> 
  add_recipe(bike_recipe) |> 
  fit(data = train)


# my_linear_model <- linear_reg() |>                       
#   set_engine("lm") |>                                    
#   set_mode("regression") |>                              
#   fit(
#     formula = log_count ~ season + holiday + workingday + weather +
#       temp + atemp + humidity + windspeed + hour + wday + month + year,
#     data = train
#   )

### Predictions Using Linear Model
bike_predictions <- predict(my_linear_model, new_data = test)  
bike_predictions  

### Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions |> 
  bind_cols(test) |>                                   
  mutate(count = exp(.pred) - 1) |>                        
  mutate(count = round(pmax(0, count))) |>                
  select(datetime, count) |>                             
  mutate(datetime = as.character(format(datetime))) 

data <- prep(bike_recipe) |> 
  bake(new_data = train)
head(data, 5)

## Write out the file
vroom_write(x = kaggle_submission, file = "./kagglesubmission2.csv", delim = ",")
