## loading packages
library("data.table")
library("tidyverse")
library("gridExtra")
library("rpart")
install.packages("rpart.plot")
library(rpart.plot)
install.packages("FactoMineR")
library(FactoMineR)
library(ggplot2)
library(corrplot)
library(dplyr)
library(lubridate)
library(caret)

# Set working directory
setwd("/Users/anushasathravada/Desktop/UTD/SEM1/Ba With R")

# Loading the dataset
rain <- read.csv("weatherAUS.csv")  # Assuming the dataset is in the working directory
summary(rain)

# Data Exploration

# Display the structure of the dataset
str(rain)

# Summary statistics for numerical variables
summary(rain[, c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine", 
                 "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                 "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                 "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")])

# Check for missing values
colSums(is.na(rain))

# Explore unique values and frequency for categorical variables
table(rain$Location)
table(rain$WindGustDir)
table(rain$WindDir9am)
table(rain$WindDir3pm)
table(rain$RainToday)
table(rain$RainTomorrow)

# Data Cleaning

# Remove rows with missing values
rain <- na.omit(rain)

# Convert Date to a date object
rain$Date <- as.Date(rain$Date)

# Convert RainToday and RainTomorrow to factors
rain$RainToday <- as.factor(rain$RainToday)
rain$RainTomorrow <- as.factor(rain$RainTomorrow)

# Explore and address outliers if necessary

# Additional cleaning steps based on exploration results

# Confirm changes
str(rain)
summary(rain)

# EDA - Visualize Relationships
# Boxplot for Rainfall by Location(for few cities)
ggplot(rain, aes(x = Location, y = Rainfall)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Rainfall by Location")

# Correlation matrix and plot
cor_matrix <- cor(rain[, c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                           "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                           "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                           "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")])
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

# Bar plot for RainTomorrow
ggplot(rain, aes(x = RainTomorrow)) +
  geom_bar() +
  ggtitle("Distribution of RainTomorrow")

# Bar plot for RainToday
ggplot(rain, aes(x = RainToday)) +
  geom_bar() +
  ggtitle("Distribution of RainToday")

# Extract month and year from the 'Date' column
rain <- rain %>%
  mutate(month = month(Date, label = TRUE), year = year(Date))

# Count occurrences of RainTomorrow for each Location, month, and RainTomorrow value
rain_counts <- rain %>%
  count(Location, month, RainTomorrow) %>%
  drop_na() %>%
  group_by(Location, month) %>%
  mutate(Rain_percent = n / sum(n))

# Plot the monthly rain
ggplot(rain_counts, aes(x = month, y = Rain_percent, fill = RainTomorrow)) +
  geom_col(position = "stack") +
  facet_wrap(~Location, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Monthly Rain Distribution by Location") +
  xlab("Month") +
  ylab("Percentage of RainTomorrow")

# Density plot for afternoon temperature by RainTomorrow
ggplot(rain, aes(x = Temp3pm, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Afternoon Temperature by Rain Tomorrow") +
  xlab("Afternoon Temperature (3pm)") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()


# Density plot for morning temperature by RainTomorrow
ggplot(rain, aes(x = Temp9am, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Morning Temperature by Rain Tomorrow") +
  xlab("Morning Temperature (9am)") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Density plot for pressure at 3 pm by RainTomorrow
ggplot(rain, aes(x = Pressure3pm, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Pressure at 3pm by Rain Tomorrow") +
  xlab("Pressure at 3pm") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Density plot for humidity at 3 pm by RainTomorrow
ggplot(rain, aes(x = Humidity3pm, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Humidity at 3pm by Rain Tomorrow") +
  xlab("Humidity at 3pm") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Density plot for pressure at 9 am by RainTomorrow
ggplot(rain, aes(x = Pressure9am, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Pressure at 9am by Rain Tomorrow") +
  xlab("Pressure at 9am") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Density plot for humidity at 9 am by RainTomorrow
ggplot(rain, aes(x = Humidity9am, fill = RainTomorrow, color = RainTomorrow)) +
  geom_density(alpha = 0.7) +
  ggtitle("Density Plot of Humidity at 9am by Rain Tomorrow") +
  xlab("Humidity at 9am") +
  ylab("Density") +
  scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
  scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
  theme_minimal()

# Filter out missing values for WindGustSpeed and WindGustDir
filtered_data <- na.omit(rain[, c("WindGustSpeed", "WindGustDir")])

# Create a 2D histogram (square plot) for WindGustSpeed and WindGustDir
ggplot(filtered_data, aes(x = WindGustDir, y = WindGustSpeed)) +
  geom_bin2d(bins = 20, square = TRUE) +
  scale_fill_viridis_c() +
  ggtitle("2D Histogram of Wind Gust Speed and Direction") +
  theme_minimal() +
  labs(x = "Wind Direction", y = "Wind Gust Speed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter out missing values for Pressure9am and Pressure3pm
filtered_data <- na.omit(rain[, c("Pressure9am", "Pressure3pm")])

# Scatter plot for Pressure9am and Pressure3pm
ggplot(filtered_data, aes(x = Pressure9am, y = Pressure3pm)) +
  geom_point() +
  ggtitle("Scatter Plot of Pressure at 9 am vs. Pressure at 3 pm") +
  xlab("Pressure at 9 am") +
  ylab("Pressure at 3 pm") +
  theme_minimal()

# Load necessary libraries for visualization
library(ggplot2)

# Assuming 'rain' is your dataset

# Filter out missing values for Humidity9am and Humidity3pm
filtered_data <- na.omit(rain[, c("Humidity9am", "Humidity3pm")])

# Scatter plot for Humidity9am and Humidity3pm
ggplot(filtered_data, aes(x = Humidity9am, y = Humidity3pm)) +
  geom_point() +
  ggtitle("Scatter Plot of Humidity at 9 am vs. Humidity at 3 pm") +
  xlab("Humidity at 9 am") +
  ylab("Humidity at 3 pm") +
  theme_minimal()

# Load necessary libraries for feature engineering
library(dplyr)
library(lubridate)

# Assuming 'rain' is your dataset

# Extracting relevant information from dates
rain <- rain %>%
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         Weekday = weekdays(Date),
         Season = case_when(
           month(Date) %in% c(12, 1, 2) ~ "Summer",
           month(Date) %in% c(3, 4, 5) ~ "Autumn",
           month(Date) %in% c(6, 7, 8) ~ "Winter",
           month(Date) %in% c(9, 10, 11) ~ "Spring"
         ))

# Convert categorical variables to factors
rain$Location <- as.factor(rain$Location)
rain$WindGustDir <- as.factor(rain$WindGustDir)
rain$WindDir9am <- as.factor(rain$WindDir9am)
rain$WindDir3pm <- as.factor(rain$WindDir3pm)
rain$RainToday <- as.factor(rain$RainToday)
rain$RainTomorrow <- as.factor(rain$RainTomorrow)
rain$Weekday <- as.factor(rain$Weekday)
rain$Season <- as.factor(rain$Season)

# Display the structure of the updated dataset
str(rain)

# Model Building
# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- createDataPartition(rain$RainTomorrow, p = 0.7, list = FALSE)
train_data <- rain[split_index, ]
test_data <- rain[-split_index, ]


# Load necessary libraries for Random Forest
library(randomForest)

# Assuming 'rain' is your dataset
# Assuming 'RainTomorrow' is the target variable
# Convert RainTomorrow to a factor (if not done during data cleaning)
rain$RainTomorrow <- as.factor(rain$RainTomorrow)
# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(rain$RainTomorrow, p = 0.7, list = FALSE)
train_data <- rain[split_index, ]
test_data <- rain[-split_index, ]
# Build a Random Forest model
model <- randomForest(RainTomorrow ~ ., data = train_data, ntree = 100, mtry = 5)
# Experiment with different values for ntree and mtry
# Make predictions on the test set
predictions <- predict(model, newdata = test_data)
# Evaluate the model
confusion_matrix1 <- confusionMatrix(predictions, test_data$RainTomorrow)
print(confusion_matrix1)



# Decision Tree model
rain_1<-data.frame(rain)
sample_size1 = dim(rain_1)[1]
set.seed(123)
sample_1 <- sample(1:sample_size1, 0.8 * sample_size1, replace=FALSE)
training_Set1 <- rain_1[sample_1, ]
testing_Set1 <- rain_1[-sample_1, ]
# Decision tree model with training data set
Model1 <- rpart(RainTomorrow ~ .,data=training_Set1, method = "class")
# decision tree model plot
rpart.plot(Model1,type = 4, box.palette = c("red", "green"), fallen.leaves = TRUE,extra = 101, split.font =1, varlen = -10)
#  cp value for removing overfitting
plotcp(Model1)
#cp value =0.018, pruned model
pruned_Model <- prune(Model1, cp = 0.018)
# pruned decision tree model plot
rpart.plot(pruned_Model)
# Perform predictions from the testing set
predictedvalues_1<-predict(pruned_Model, testing_Set1, type = "class")
#  testing set size
size_Test1 = dim(testing_Set1)[1]
# misclassification
error1 = sum(predictedvalues_1 != testing_Set1$RainTomorrow)
# Calculate the misclassification rate
misclassification_rate1 = error1/size_Test1
misclassification_rate1
#  misclassified points
Wrong = (predictedvalues_1 != testing_Set1$RainTomorrow)
# Yes/No values 
Yes_values = (predictedvalues_1 == 'Yes')
No_values = (predictedvalues_1 == 'No')
# Get the data points that are misclassified and are classified as yes
WrongAndYes = (Wrong & Yes_values)
errorYes = sum(WrongAndYes)
WrongAndNo = (Wrong & No_values)
errorNo = sum(WrongAndNo)
#misclassification rate
misclassification_rate1 = (errorYes+errorNo)/size_Test1
misclassification_rate1
# confusion matrix
confusion_matrix2 = confusionMatrix(predictedvalues_1, testing_Set1$RainTomorrow)
confusion_matrix2



#KNN
library(class)
# Assuming 'RainTomorrow' is the target variable
# Convert RainTomorrow to a factor (if not done during data cleaning)
rain$RainTomorrow <- as.factor(rain$RainTomorrow)
# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(rain$RainTomorrow, p = 0.7, list = FALSE)
train_data <- rain[split_index, ]
test_data <- rain[-split_index, ]
# Perform KNN on the numeric variables
numeric_data <- train_data[, c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                               "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                               "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                               "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")]
# Standardize the numeric variables
standardized_data <- scale(numeric_data)
# Train the KNN model
knn_model <- knn(train = standardized_data, test = standardized_data, cl = train_data$RainTomorrow, k = 5)
# Make predictions on the test set
predictions <- knn(train = standardized_data, test = scale(test_data[, c("MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine",
                                                                         "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                                                                         "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                                                                         "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")]),
                   cl = train_data$RainTomorrow, k = 5)
# Generate a confusion matrix
confusion_matrix3 <- confusionMatrix(predictions, test_data$RainTomorrow)
print(confusion_matrix3)


#Based on the above three models we get the Higher accuracy for the KNN











