setwd("../Desktop/stat151a_final_project/Yelp-Data-Challenge/")

library(jsonlite)
library(randomForest)
business <- stream_in(file("yelp_academic_dataset_business.json"))
review_train1 <- stream_in(file("yelp_clean_training1.json"))
review_test <- stream_in(file("yelp_clean_training2.json"))
user <- stream_in(file("yelp_user_clean.json"))

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

categories <- business$categories
neighborhoods <- business$neighborhoods
drops <- c("attributes", "categories", "neighborhoods", "hours")
business_temp <- business[ , !(names(business) %in% drops)]
business_temp <- cbind(business_temp, attr)
business_temp <- cbind(business_temp, good_for)
business_temp <- cbind(business_temp, ambience)
business_temp <- cbind(business_temp, parking)
business_temp <- cbind(business_temp, music)
business_temp <- cbind(business_temp, hair_types)
business_temp <- cbind(business_temp, dietary)

drops2 <- c("business_id", "full_address", "name", "open", "city", "state", "type")
business_temp <- business_temp[ , !(names(business_temp) %in% drops2)]
dd0_train <- subset(business_temp, select=c("Noise Level", "Attire", "Alcohol",
                                              "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed"))
dd0_train[is.na(dd0_train)] <- "na"
business_data <- data.frame(model.matrix(~.-1, dd0_train))

drops3 <- c("Noise Level", "Attire", "Alcohol",
            "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed")
business_temp <- business_temp[ , !(names(business_temp) %in% drops3)]
business_data <- cbind(business_data, business_temp)
business_data <- data.frame(apply(business_data, 2, as.numeric))

for(i in 1:ncol(business_data)){
  business_data[is.na(business_data[,i]), i] <- mean(business_data[,i], na.rm = TRUE)
}

sapply(unlist(myList, recursive=FALSE, use.names=FALSE), "[", 1)

yelp_lm <- lm(stars ~ ., data = business_data)

rf <- randomForest(x = x, y = asy, ntree = 100)
df[, -grep("na$", colnames(df))]