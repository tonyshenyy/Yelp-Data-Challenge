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

attr <- business$attributes
good_for <- attr$`Good For`
ambience <- attr$Ambience
parking <- attr$Parking
music <- attr$Music
hair_types <- attr$`Hair Types Specialized In`
dietary <- attr$`Dietary Restrictions`
drops_attr <- c("Good For", "Ambience", "Parking", "Music", "Hair Types Specialized In",
                "Dietary Restrictions")
attr <- attr[ , !(names(attr) %in% drops_attr)]

drops <- c("attributes", "categories", "neighborhoods", "hours")
business_temp <- business[ , !(names(business) %in% drops)]
business_temp <- cbind(business_temp, attr)
business_temp <- cbind(business_temp, good_for)
business_temp <- cbind(business_temp, ambience)
business_temp <- cbind(business_temp, parking)
business_temp <- cbind(business_temp, music)
business_temp <- cbind(business_temp, hair_types)
business_temp <- cbind(business_temp, dietary)
train_data <- merge(business_temp, review_train1, by = "business_id")
train_data <- merge(train_data, user, by = "user_id")

test_data <- merge(business_temp, review_test, by = "business_id")
test_data <- merge(test_data, user, by = "user_id")

drops2 <- c("user_id", "business_id", "full_address", "review_id", "date", "yelping_since", "funny", "useful", "cool", "name.x", "name.y")
train_data <- train_data[ , !(names(train_data) %in% drops2)]
names(train_data)[3] <- "review_count_business"
names(train_data)[81] <- "review_count_reviewer"
names(train_data)[6] <- "stars_business"
names(train_data)[80] <- "stars_reviewer"

test_data <- test_data[ , !(names(test_data) %in% drops2)]
names(test_data)[3] <- "review_count_business"
names(test_data)[81] <- "review_count_reviewer"
names(test_data)[6] <- "stars_business"
names(test_data)[80] <- "stars_reviewer"

# dd0 <- subset(train_data, select=c(city, state))
# new_train_data <- data.frame(model.matrix(~.-1, dd0), review_count_business=train_data$review_count_business, longitude=train_data$longitude, stars_business=train_data$stars_business, latitude=train_data$latitude, stars_reviewer=as.numeric(train_data$stars_reviewer), review_count_reviewer=train_data$review_count_reviewer, fans=train_data$fans, average_stars=train_data$average_stars, compliments=train_data$compliments, elite=train_data$elite)

drop3 <- c("open", "city", "state", "type")
temp_train_data <- train_data[ , !(names(train_data) %in% drop3)]
temp_train_data$stars_reviewer <- as.numeric(temp_train_data$stars_reviewer)

temp_test_data <- test_data[ , !(names(test_data) %in% drop3)]
temp_test_data$stars_reviewer <- as.numeric(temp_test_data$stars_reviewer)

# d <- c("latitude", "longitude")
# temp_train_data <- temp_train_data[, !(names(temp_train_data) %in% d)]
# temp_test_data <- temp_test_data[, !(names(temp_test_data) %in% d)]

dd0_train <- subset(temp_train_data, select=c("Noise Level", "Attire", "Alcohol",
                                              "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed"))
dd0_train[is.na(dd0_train)] <- "na"
# for (i in 1:length(names(temp_train_data))) {
#   if (class(temp_train_data[ , i]) == "logical") {
#     temp_train_data[ , i] <- as.numeric(temp_train_data[ , i])
#   }
# }
cleaned_train_data <- data.frame(model.matrix(~.-1, dd0_train),
                                 review_count_business = temp_train_data$review_count_business,
                                 longitude = temp_train_data$longitude,
                                 stars_business = temp_train_data$stars_business,
                                 latitude = temp_train_data$latitude,
                                 take_out = temp_train_data$"Take-out",
                                 drive_thru = temp_train_data$"Drive-Thru",
                                 caters = temp_train_data$Caters,
                                 reservation = temp_train_data$"Takes Reservations",
                                 delivery = temp_train_data$Delivery,
                                 tv = temp_train_data$"Has TV",
                                 outdoor = temp_train_data$"Outdoor Seating",
                                 waiter = temp_train_data$"Waiter Service",
                                 credit_card = temp_train_data$"Accepts Credit Cards",                                 
                                 good_for_kids = temp_train_data$"Good for Kids",
                                 group = temp_train_data$"Good For Groups",
                                 price_range = temp_train_data$"Price Range",
                                 happy_hour = temp_train_data$"Happy Hour",
                                 dancing = temp_train_data$"Good For Dancing",
                                 coat_check = temp_train_data$"Coat Check",
                                 wheelchair = temp_train_data$"Wheelchair Accessible",
                                 dogs = temp_train_data$"Dogs Allowed",
                                 byob = temp_train_data$BYOB, corkage = temp_train_data$Corkage,
                                 order_at_counter = temp_train_data$"Order at Counter",
                                 by_appt = temp_train_data$"By Appointment Only",
                                 open_24_hrs = temp_train_data$"Open 24 Hours",
                                 insurance = temp_train_data$"Accepts Insurance",
                                 dessert = temp_train_data$dessert,
                                 latenight = temp_train_data$latenight,
                                 lunch = temp_train_data$lunch, dinner = temp_train_data$dinner,
                                 brunch = temp_train_data$brunch, breakfast = temp_train_data$breakfast,
                                 romantic = temp_train_data$romantic, intimate = temp_train_data$intimate,
                                 classy = temp_train_data$classy, hipster = temp_train_data$hipster,
                                 divey = temp_train_data$divey, touristy = temp_train_data$touristy,
                                 trendy = temp_train_data$trendy, upscale = temp_train_data$upscale,
                                 casual = temp_train_data$casual, garage = temp_train_data$garage,
                                 street = temp_train_data$street, validated = temp_train_data$validated,
                                 lot = temp_train_data$lot, valet = temp_train_data$valet,
                                 dj = temp_train_data$dj, background_music = temp_train_data$background_music,
                                 jukebox = temp_train_data$jukebox, live = temp_train_data$live,
                                 video = temp_train_data$video, karaoke = temp_train_data$karaoke,
                                 coloring = temp_train_data$coloring, africanamerican = temp_train_data$africanamerican,
                                 curly = temp_train_data$curly, perms = temp_train_data$perms,
                                 kids = temp_train_data$kids, extensions = temp_train_data$extensions,
                                 asian = temp_train_data$asian, straightperms = temp_train_data$straightperms,
                                 dairy_free = temp_train_data$"dairy-free", gluten_free = temp_train_data$"gluten-free",
                                 vegan = temp_train_data$vegan, kosher = temp_train_data$kosher,
                                 halal = temp_train_data$halal, soy_free = temp_train_data$"soy-free",
                                 vegetarian = temp_train_data$"vegetarian", 
                                 stars_reviewer = temp_train_data$"stars_reviewer",
                                 review_count_reviewer = temp_train_data$"review_count_reviewer", 
                                 fans = temp_train_data$fans, average_stars = temp_train_data$"average_stars",
                                 compliments = temp_train_data$compliments, elite = temp_train_data$elite)

cleaned_train_data <- as.data.frame(apply(cleaned_train_data, 2, as.numeric))
cleaned_train_data[is.na(cleaned_train_data)] <- 0

yelp_lm <- lm(stars_reviewer ~ ., data = cleaned_train_data, na.action = na.pass)
test_labels <- temp_test_data$stars_reviewer
temp_test_data <- subset(temp_test_data, select = -stars_reviewer)
lm.prediction <- predict(yelp_lm, temp_test_data)

# RMSE loss function
rmse <- function(predicted, observed){
  return(sqrt(mean((predicted - observed)^2)))
}

# Zero one error loss: returns percent misclassified
zero.one <- function(predicted, observed) {
  return(length(which(predicted != observed)) / length(observed))
}

lm.rmse <- rmse(lm.prediction, test_labels)
lm.error_rate <- zero.one(lm.prediction, test_labels)

rf <- randomForest(x = cleaned_train_data[ , -93], y = as.factor(cleaned_train_data$stars_reviewer))
# rf <- randomForest(x = temp_train_data[ , -5], y = as.factor(temp_train_data[, 5]), xtest = temp_test_data, ytest = as.factor(test_labels))

# merge_data <- function(bus, rev, usr) {
#   temp_df <- merge(bus, rev, by = "business_id")
#   return(merge(temp_df, usr, by = "user_id"))
# }
