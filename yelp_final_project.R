setwd("../Desktop/stat151a_final_project/Yelp-Data-Challenge/")

library(jsonlite)
library(randomForest)
business <- stream_in(file("yelp_business_restaurants.json"))
#review_train1 <- stream_in(file("yelp_clean_training1.json"))
#review_test <- stream_in(file("yelp_clean_training2.json"))
#user <- stream_in(file("yelp_user_clean.json"))

# combine all the features and subset categorical features
attr <- business$attributes
good_for <- attr$`Good For`
ambience <- attr$Ambience
parking <- attr$Parking
music <- attr$Music
#hair_types <- attr$`Hair Types Specialized In`
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
#business_temp <- cbind(business_temp, hair_types)
business_temp <- cbind(business_temp, dietary)

drops2 <- c("business_id", "full_address", "name", "type", "city", "state")
business_temp <- business_temp[ , !(names(business_temp) %in% drops2)]
dd0_train <- subset(business_temp, select=c("Noise Level", "Attire", "Alcohol",
                                              "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed"))
drops3 <- c("Noise Level", "Attire", "Alcohol",
            "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed")
business_temp <- business_temp[ , !(names(business_temp) %in% drops3)]

# calculate percentage of missing values in rows and columns
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(business_temp,2,pMiss))
sort(apply(dd0_train,2,pMiss))

# library(mice)
# md.pattern(dd0_train)
# 
# library(zoo)
# library(VIM)
# aggr_plot <- aggr(business_temp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(business_temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# aggr_plot <- aggr(dd0_train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dd0_train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Mode function
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- sample(xmode, size = 1)
  return(xmode)
}

tempData <- mice(business_temp,m=5,maxit=50,meth='pmm',seed=500)
business_data <- complete(tempData,1)

# fill the NAs for categorical data with modes
# for (i in 1:ncol(dd0_train)) {
#   dd0_train[is.na(dd0_train[ , i]), i] <- Mode(dd0_train[, i], na.rm = TRUE)
# }

# one hot encoding for categorical data
business_data <- data.frame(model.matrix(~.-1, dd0_train), stringsAsFactors = FALSE)

# fill in NAs with either mean or mode
for (j in 1:ncol(business_temp)) {
  if ((class(business_temp[ , j]) == "numeric") || 
    (names(business_temp)[j] == "review_count")) {
    business_temp[is.na(business_temp[ , j]), j] <- mean(business_temp[ , j], na.rm = TRUE)
  } else if ((class(business_temp[ , j]) == "logical") || 
    (names(business_temp)[j] == "Price Range")) {
    business_temp[ , j] <- as.numeric(business_temp[ , j])
    business_temp[is.na(business_temp[ , j]), j] <- Mode(business_temp[ , j], na.rm = TRUE)
  }
}

business_data <- cbind(business_data, business_temp)
business_data <- data.frame(apply(business_data, 2, as.numeric), stringsAsFactors = FALSE)
business_data <- business_data[ , !(names(business_data) %in% c("Accepts.Insurance"))]
business_data <- business_data[ , !(names(business_data) %in% "X.Noise.Level.very_loud")]

# for(i in 1:ncol(business_data)){
#   business_data[is.na(business_data[,i]), i] <- mean(business_data[,i], na.rm = TRUE)
# }

#cate <- unname(rapply(categories, function(x) x[1], how = "unlist"))

# business_data <- business_data[, -grep("na$", colnames(business_data))]
yelp_lm <- lm(stars ~ ., data = business_data)

rf <- randomForest(x = business_data[ , -20], y = as.factor(business_data$stars))



# ======================================================
# attr <- cbind(attr, good_for)
# attr <- cbind(attr, ambience)
# attr <- cbind(attr, parking)
# attr <- cbind(attr, music)
# attr <- cbind(attr, dietary)
# attr$stars <- business$stars
# dd0_train <- subset(attr, select=c("Noise Level", "Attire", "Alcohol",
#                                             "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed"))
# 
# Mode <- function (x, na.rm) {
#   xtab <- table(x)
#   xmode <- names(which(xtab == max(xtab)))
#   if (length(xmode) > 1) xmode <- sample(xmode, size = 1)
#   return(xmode)
# }
# 
# for (i in 1:ncol(dd0_train)) {
#   dd0_train[is.na(dd0_train[ , i]), i] <- Mode(dd0_train[, i], na.rm = TRUE)
# }
# 
# business_data <- data.frame(model.matrix(~.-1, dd0_train), stringsAsFactors = FALSE)
# 
# drops3 <- c("Noise Level", "Attire", "Alcohol",
#             "Smoking", "Wi-Fi", "BYOB/Corkage", "Ages Allowed", "city", "state")
# attr <- attr[ , !(names(attr) %in% drops3)]
# for (j in 1:ncol(attr)) {
#   if ((class(attr[ , j]) == "numeric") || 
#       (names(attr)[j] == "review_count")) {
#     attr[is.na(attr[ , j]), j] <- mean(attr[ , j], na.rm = TRUE)
#   } else if ((class(attr[ , j]) == "logical") || 
#              (names(attr)[j] == "Price Range")) {
#     attr[ , j] <- as.numeric(attr[ , j])
#     attr[is.na(attr[ , j]), j] <- Mode(attr[ , j], na.rm = TRUE)
#   }
# }
# business_data <- cbind(business_data, attr)
# business_data <- data.frame(apply(business_data, 2, as.numeric), stringsAsFactors = FALSE)
# business_data <- business_data[ , !(names(business_data) %in% c("Accepts.Insurance"))]
# business_data <- business_data[ , !(names(business_data) %in% "X.Noise.Level.very_loud")]
# yelp_lm <- lm(stars ~ ., data = business_data)
