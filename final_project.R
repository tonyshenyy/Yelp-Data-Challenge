# set working directory to the correct folder
setwd("../Desktop/stat151a_final_project/Yelp-Data-Challenge/")

# load the dataset
library(jsonlite)
library(randomForest)
business <- stream_in(file("yelp_academic_dataset_business.json"))
review_train1 <- stream_in(file("yelp_clean_training1.json"))
review_test <- stream_in(file("yelp_clean_training2.json"))
user <- stream_in(file("yelp_user_clean.json"))

#flatten_business <- function(bus) {
#  for (i in 1:length(names(bus))) {
#    if (class(bus[ , i]) == "data.frame") {
#      fea <- bus[ , i]
#      bus[ , i] <- NULL
#      bus <- cbind(bus, fea)
#    } else if (class(bus[ , i]) == "list") {
#      bus[ , i] <- NULL
#    }
#  }
#  return(bus)
#}

# business_cleaned_first_iter <- flatten_business(business)
# business_cleaned_final <- flatten_business(business_cleaned_first_iter)

drops <- c("attributes", "categories", "neighborhoods", "hours")
business_temp <- business[ , !(names(business) %in% drops)]
train_data <- merge(business_temp, review_train1, by = "business_id")
train_data <- merge(train_data, user, by = "user_id")

test_data <- merge(business_temp, review_test, by = "business_id")
test_data <- merge(test_data, user, by = "user_id")

drops2 <- c("user_id", "business_id", "full_address", "review_id", "date", "yelping_since", "funny", "useful", "cool", "name.x", "name.y")
train_data <- train_data[ , !(names(train_data) %in% drops2)]
names(train_data)[3] <- "review_count_business"
names(train_data)[10] <- "review_count_reviewer"
names(train_data)[6] <- "stars_business"
names(train_data)[9] <- "stars_reviewer"

test_data <- test_data[ , !(names(test_data) %in% drops2)]
names(test_data)[3] <- "review_count_business"
names(test_data)[10] <- "review_count_reviewer"
names(test_data)[6] <- "stars_business"
names(test_data)[9] <- "stars_reviewer"

# dd0 <- subset(train_data, select=c(city, state))
# new_train_data <- data.frame(model.matrix(~.-1, dd0), review_count_business=train_data$review_count_business, longitude=train_data$longitude, stars_business=train_data$stars_business, latitude=train_data$latitude, stars_reviewer=as.numeric(train_data$stars_reviewer), review_count_reviewer=train_data$review_count_reviewer, fans=train_data$fans, average_stars=train_data$average_stars, compliments=train_data$compliments, elite=train_data$elite)

drop3 <- c("open", "city", "state", "type")
temp_train_data <- train_data[ , !(names(train_data) %in% drop3)]
temp_train_data$stars_reviewer <- as.numeric(temp_train_data$stars_reviewer)

temp_test_data <- test_data[ , !(names(test_data) %in% drop3)]
temp_test_data$stars_reviewer <- as.numeric(temp_test_data$stars_reviewer)

yelp_lm <- lm(stars_reviewer ~ ., data = temp_train_data)
test_labels <- temp_test_data$stars_reviewer
temp_test_data <- subset(temp_test_data, select = -stars_reviewer)
lm.prediction <- predict(yelp_lm, temp_test_data)

# RMSE loss function
rmse <- function(predicted, observed){
  return(sqrt(sum((predicted - observed)^2)))
}

# Zero one error loss: returns percent misclassified
zero.one <- function(predicted, observed) {
  return(length(which(predicted != observed)) / length(observed))
}



# merge_data <- function(bus, rev, usr) {
#   temp_df <- merge(bus, rev, by = "business_id")
#   return(merge(temp_df, usr, by = "user_id"))
# }
