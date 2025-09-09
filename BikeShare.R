# load libraries

library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
library(skimr)


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
