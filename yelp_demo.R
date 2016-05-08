library(jsonlite)
# Set working directory
# setwd("C:/Users/w5335/Documents/ToshibaDocuments/datasets/yelp")

##### Load the data #####
training <- stream_in(file("yelp_training_restaurants_processed.json"))
test <- stream_in(file("yelp_test_restaurants_processed.json"))

training.clean <- training[, c(-(2:7), -9, -10, -11, -13, -14)]
test.clean <- test[, c(-(2:7), -9, -10, -11, -13, -14)]
# Fit linear model
lm.fit <- lm(data = training.clean, formula(stars ~ . -business_id - name))