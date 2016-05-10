library(jsonlite)
library(glmnet)
# Set working directory
# setwd("C:/Users/w5335/Documents/ToshibaDocuments/datasets/yelp")

##### Load the data #####
drops <- c("business_id", "full_address", "hours", "open", "categories", "city",
           "review_count", "name", "neighborhoods", "longitude", "state", "latitude", "attributes"  )
training <- stream_in(file("yelp_training_restaurants_processed.json"))
test <- stream_in(file("yelp_test_restaurants_processed.json"))
training.clean <- training[, !(names(training) %in% drops)]
test.clean <- test[, !(names(test) %in% drops)]

# calculate percentage of missing values in rows and columns
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(training.clean,2,pMiss))
sort(apply(test.clean,2,pMiss))

# Clean NAs
clean_NA <- function(data) {
  for (i in 1:ncol(data)) {
    data[is.na(data[ , i]), i] <- mean(data[ , i], na.rm = TRUE)
  }
  return(data)
}
training.clean <- clean_NA(training.clean)
test.clean <- clean_NA(test.clean)

# Fit linear model
lm.fit <- lm(stars ~ ., data = training.clean)

# Clean the NA's
# training.indices <- complete.cases(training.clean[, c(-1, -2, -3)])
# test.indices <- complete.cases(test.clean[, c(-1, -2, -3)])
# 
# training.clean <- training.clean[training.indices,]
# test.clean <- test.clean[test.indices,]
# 
training.features <- training.clean[, -1]
test.features <- test.clean[, -1]

training.response <- training.clean$stars
test.response <- test.clean$stars

# Fit LASSO linear model
fit <- glmnet(as.matrix(training.features), training.response)
glm.fit <- glmnet(x = as.matrix(training.features), y = training.response, family = "gaussian", alpha = 1, standardize = F)
