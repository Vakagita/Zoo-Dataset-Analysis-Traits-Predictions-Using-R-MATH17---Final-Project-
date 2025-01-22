# Final Project

library(dplyr)
library(ggplot2)
setwd('C:/Vaishu/Math17/Final Project/archive (2)/')

zoo <- read.csv("zoo2.csv")

# Animals with more than 4 legs
more_than_4_legs <- subset(zoo, legs > 4)
print("Animals with more than 4 legs:")
print(more_than_4_legs)

# Filter to see only aquatic animals
aquatic_animals <- zoo %>% filter(aquatic == 1)
print("Subset of aquatic animals:")
print(aquatic_animals)

# Grouping by animal class type and then summarizing traits
class_summary <- zoo %>%
  group_by(class_type) %>%
  summarise(
    Total_Animals = n(),
    Avg_Legs = mean(legs, na.rm = TRUE),
    Aquatic_Count = sum(aquatic),
    Hair_Count = sum(hair),
    Feather_Count = sum(feathers)
  )

print("Summary of traits by class type:")
print(class_summary)

# Proportion of animals that are listed as domestic
domestic_animals <- sum(zoo$domestic == 1)
total_animals <- nrow(zoo)
prop <- domestic_animals / total_animals
prop

# The 95% confidence interval for the proportion
prop.test(domestic_animals, total_animals, conf.level = 0.95)

# Linear model to Predict number of legs based on traits
lm_model <- lm(legs ~ hair + feathers + backbone, data = zoo)
summary(lm_model)

# Plotting the regression line for legs and backbone
ggplot(zoo, aes(x = backbone, y = legs)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regression: Legs vs Backbone",
       x = "Backbone (0 = No, 1 = Yes)", y = "Number of Legs")

# Adding a binary column for mammals
zoo$mammal <- ifelse(zoo$class_type == 1, 1, 0)

# Logistic regression model
logistic_model <- glm(mammal ~ hair + milk + backbone, 
                      family = "binomial", data = zoo)
summary(logistic_model)

# Prediction
zoo$predicted_mammal <- ifelse(predict(logistic_model, type = "response") > 0.5, 1, 0)

# Accuracy
accuracy <- mean(zoo$mammal == zoo$predicted_mammal)
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))

# Function to predict if an animal is a mammal
predict_mammal <- function(hair, milk, backbone) {
  probability <- exp(-2.5 + 1.5 * hair + 2.0 * milk + 1.2 * backbone) / 
    (1 + exp(-2.5 + 1.5 * hair + 2.0 * milk + 1.2 * backbone))
  return(ifelse(probability > 0.5, 1, 0))
}


predict_mammal(hair = 1, milk = 1, backbone = 1)

# Bar plot of Animals producing milk
ggplot(zoo, aes(x = factor(milk))) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Animals Producing Milk",
       x = "Produces Milk (0 = No, 1 = Yes)", y = "Count")

ggplot(zoo, aes(x = factor(class_type), y = legs)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Number of Legs Across Class Types",
       x = "Class Type", y = "Number of Legs")

ggplot(zoo, aes(x = fins, y = aquatic)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot: Fins vs Aquatic",
       x = "Number of Fins", y = "Aquatic (0 = No, 1 = Yes)")

ggplot(zoo, aes(x = legs)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Density Plot of Number of Legs",
       x = "Number of Legs", y = "Density")

# Facet plot for binary traits
ggplot(zoo, aes(x = factor(hair), fill = factor(milk))) +
  geom_bar() +
  facet_wrap(~ class_type) +
  labs(title = "Hair and Milk Traits by Class Type",
       x = "Hair (0 = No, 1 = Yes)", fill = "Milk")

# Final summarized table of traits
final_summary <- zoo %>%
  group_by(class_type) %>%
  summarise(
    Total_Animals = n(),
    Avg_Legs = mean(legs),
    Milk_Count = sum(milk),
    Aquatic_Count = sum(aquatic)
  )
print(final_summary)

