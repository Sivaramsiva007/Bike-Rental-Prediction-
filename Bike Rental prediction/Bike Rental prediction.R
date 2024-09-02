library(readxl)
data = read_excel("C:/Users/Sivaram/Downloads/1657875746_day.xlsx")
View(data)
# 1.Exploratory data analysis
summary(data)
str(data)

data$season <- as.factor(data$season)
data$yr <- as.factor(data$yr)
data$mnth <- as.factor(data$mnth)
data$holiday <- as.factor(data$holiday)
data$weekday <- as.factor(data$weekday)
data$workingday <- as.factor(data$workingday)
data$weathersit <- as.factor(data$weathersit)

colSums(is.na(data))

# 2.Attributes distributions and trends
# Plot monthly distribution of the total number of bikes rented.
library(plotly)
monthly_data <- data %>%
  group_by(mnth) %>%
  summarize(total_rented = sum(cnt))


plot_ly(monthly_data, x = ~mnth, y = ~total_rented, type = 'bar') %>%
  layout(title = "Monthly Distribution of Total Bikes Rented",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Total Count of Bikes Rented"))


# Plot yearly distribution of the total number of bikes rented

yearly_data <- data %>%
  group_by(yr) %>%
  summarize(total_cnt = sum(cnt))

# Create the bar plot with Plotly
plot_ly(yearly_data, x = ~yr, y = ~total_cnt, type = 'bar') %>%
  layout(title = "Yearly Distribution of Total Bikes Rented",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Total Count of Bikes Rented"))

# Plot boxplot for outliers analysis
boxplot(data[, c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt')], main="Boxplot for Outliers Analysis")

# Create a list of box plots for each variable
p <- plot_ly() %>%
  add_trace(y = ~data$temp, type = 'box', name = 'Temperature') %>%
  add_trace(y = ~data$atemp, type = 'box', name = 'Feels-Like Temperature') %>%
  add_trace(y = ~data$hum, type = 'box', name = 'Humidity') %>%
  add_trace(y = ~data$windspeed, type = 'box', name = 'Wind Speed') %>%
  add_trace(y = ~data$casual, type = 'box', name = 'Casual Users') %>%
  add_trace(y = ~data$registered, type = 'box', name = 'Registered Users') %>%
  add_trace(y = ~data$cnt, type = 'box', name = 'Total Count')

p <- p %>% layout(title = "Boxplot for Outliers Analysis",
                  yaxis = list(title = "Values"))

p

# 3. Split the dataset into train and test dataset
library(caret)

set.seed(42)
splitIndex <- createDataPartition(data$cnt, p = 0.8, list = FALSE)
train_data <- data[splitIndex,]
test_data <- data[-splitIndex,]
View(test_data)
View(train_data)


# 4. Create a model using the random forest algorithm
#install.packages('randomForest')
library(randomForest)
rf_model <- randomForest(cnt ~ ., data=train_data, ntree=500, mtry=3, importance=TRUE)
summary(rf_model)
?randomForest
# 5. Predict the performance of the model on the test dataset
y_pred <- predict(rf_model, test_data)

show_test = test_data
show_test['Predicted Cnt'] = y_pred
View(show_test)

# Calculate R^2
rsq <- cor(test_data$cnt, y_pred)^2
rsq

# Calculate Adjusted R^2
n <- nrow(test_data)  # Number of observations
k <- ncol(test_data) - 1  # Number of predictors (excluding the target variable 'cnt')
adj_rsq <- 1 - ((1 - rsq) * (n - 1) / (n - k - 1))
adj_rsq


library(Metrics)
# Calculate RMSE
rmse_val <- rmse(test_data$cnt, y_pred)
print(paste("RMSE: ", rmse_val))

# Calculate and plot feature importance
importance_values <- importance(rf_model)
importance_df <- as.data.frame(importance_values)
importance_df$Feature <- rownames(importance_df)

# Plot feature importance
plot_ly(importance_df, x = ~Feature, y = ~IncNodePurity, type = 'bar') %>%
  layout(title = "Feature Importance",
         xaxis = list(title = "Features"),
         yaxis = list(title = "Importance (Increase in Node Purity)"))

# Display the importance values
importance_df

# Additional Gold Stock Price
gold_data = read.csv('C:/Users/goyal/OneDrive/Desktop/Desktop/goldstock.csv')
View(gold_data)
gold_data = gold_data[,-1]

colSums(is.na(gold_data))

# Preprocess the data: Convert Date to Date type and drop unnecessary column
gold_data <- gold_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))


str(gold_data)

# Time Series Plot of Closing Price

plot_ly(data = gold_data, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', name = 'Closing Price') %>%
  layout(title = 'Time Series Plot of Gold Closing Price',
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Closing Price'))


# Histogram of Closing Prices

plot_ly(data = gold_data, x = ~Close, type = 'histogram') %>%
  layout(title = 'Histogram of Gold Closing Prices',
         xaxis = list(title = 'Closing Price'),
         yaxis = list(title = 'Frequency'))

library(readr)
library(tidyr)

# Box Plot for Each Numeric Variable
gold_data_long <- gold_data %>%
  select(Date, Close, Volume, Open, High, Low) %>%
  gather(key = "Variable", value = "Value", -Date)


plot_ly(data = gold_data_long, x = ~Variable, y = ~Value, type = 'box') %>%
  layout(title = 'Box Plot of Gold Price Variables',
         xaxis = list(title = 'Variable'),
         yaxis = list(title = 'Value'))


# Candlestick Chart
plot_ly(x = gold_data$Date, type = "candlestick",
        open = ~gold_data$Open, close = ~gold_data$Close,
        high = ~gold_data$High, low = ~gold_data$Low) %>%
  layout(title = "Candlestick Chart for Gold Prices",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Price"))