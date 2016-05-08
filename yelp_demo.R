library(jsonlite)
library(glmnet)
# Set working directory
# setwd("C:/Users/w5335/Documents/ToshibaDocuments/datasets/yelp")

##### Load the data #####
training <- stream_in(file("yelp_training_restaurants_processed.json"))
test <- stream_in(file("yelp_test_restaurants_processed.json"))
# Training: 20000, test: 5078
# Without NA's: Training: 19910, test: 5062
training.clean <- training[, c(-(2:7), -9, -10, -11, -13, -14)]
test.clean <- test[, c(-(2:7), -9, -10, -11, -13, -14)]

# Clean the NA's
training.indices <- complete.cases(training.clean[, c(-1, -2, -3)])
test.indices <- complete.cases(test.clean[, c(-1, -2, -3)])

training.clean <- training.clean[training.indices,]
test.clean <- test.clean[test.indices,]

training.features <- training.clean[, c(-1, -2, -3)]
test.features <- test.clean[, c(-1, -2, -3)]

training.response <- training.clean$stars[training.indices]
test.response <- test.clean$stars[test.indices]

# Fit linear model
lm.fit <- lm(data = training.clean, formula(stars ~ . -business_id - name))
# Fit LASSO linear model
glm.fit <- glmnet(x = as.matrix(training.features), y = training.response, family = "gaussian", alpha = 1, standardize = F)
