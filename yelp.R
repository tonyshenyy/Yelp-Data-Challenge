setwd("C:/Users/w5335/Documents/ToshibaDocuments/datasets/yelp")
library(jsonlite)
library(nnet)
library(stringr)
library(ggplot2)
library(plyr)
library(DAAG)

business <- stream_in(file("yelp_academic_dataset_business.json"))
review <- stream_in(file("yelp_clean_training1.json"))
user <- stream_in(file("yelp_user_clean.json"))

###### Pre-processing, scrambling, partitioning into chunks #######
set.seed(257)
# For reference, total dataset is 2225213 reviews
scrambled <- review[sample(1:nrow(review), nrow(review), replace = F),]
con.out <- file("yelp_clean_training1.json", open = "wb")
stream_out(scrambled[1:500000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_clean_training2.json", open = "wb")
stream_out(scrambled[500001:1000000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_clean_training3.json", open = "wb")
stream_out(scrambled[1000001:1500000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_clean_test.json", open = "wb")
stream_out(scrambled[1500001:2225213,], con.out, pagesize = 1000)
close(con.out)

con.out <- file("yelp_training1.json", open = "wb")
stream_out(scrambled[1:500000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_training2.json", open = "wb")
stream_out(scrambled[500001:1000000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_training3.json", open = "wb")
stream_out(scrambled[1000001:1500000,], con.out, pagesize = 1000)
close(con.out)
con.out <- file("yelp_test.json", open = "wb")
stream_out(scrambled[1500001:2225213,], con.out, pagesize = 1000)
close(con.out)
review$stars <- as.factor(review$stars)
hist(business$stars, breaks = 6)

# More preprocessing
flatten_business <- function(bus) {
  attr <- bus$attributes
  h <- bus$hours
  cat <- bus$categories
  nei <- bus$neighborhoods
  drops <- c("attributes", "hours", "categories", "neighborhoods")
  bus <- bus[ , !(names(bus) %in% drops)]
  bus <- cbind(bus, attr)
  return(cbind(bus, h))
}

business_cleaned <- flatten_business(business)

merge_data <- function(bus, rev, usr) {
  temp_df <- merge(bus, rev, by = "business_id")
  return(merge(temp_df, usr, by = "user_id"))
}

data <- merge_data(business_cleaned, review, user)

# Change the names that were in both joined datasets
change.name <- which(grepl(".*\\.[xy]$", names(data)))
renaming.function <- function(name) {
  if (grepl(".*\\.x", name)) {
    return(gsub("(.*)\\.x", "business_\\1", name))
  } else if (grepl(".*\\.y", name)) {
    return(gsub("(.*)\\.y", "user_\\1", name))
  }
}
new.names <- sapply(names(data)[change.name], renaming.function)
data <- rename(data, new.names)
con.out <- file("yelp_clean_complete.json", open = "wb")
stream_out(data, con.out, pagesize = 1000)
close(con.out)



# RMSE loss function
rmse <- function(predicted, observed){
  return(sqrt(sum((predicted - observed)^2)))
}

# Zero one error loss: returns percent misclassified
zero.one <- function(predicted, observed) {
  return(length(which(predicted != observed)) / length(observed))
}

# Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}
#### Create the model, train, predict ########

# Feature Selection
# LASSO, possibly F-B regr



# Model Training + CV
# Don't use this, doesn't terminate
#cv.model <- CVlm(data = data, m = 10, form.lm = formula(user_stars ~ average_stars))

# Perform k-fold CV, plot results of loss functions
cv.lm <- function(data, form = formula(user_stars ~ average_stars), k = 10) {
  id <- sample(1:k, nrow(data), replace = T)
  lst <- 1:k
  rmse.vector <- vector()
  zero.one.vector <- vector()
  for (i in 1:k) {
    training <- subset(data, id %in% lst[-i])
    test <- subset(data, id %in% c(i))
    
    fit.lm <- lm(form, data = training)
    pred <- round(predict(fit.lm, newdata = test))
    actual <- test$user_stars
    
    rmse.vector[i] <- rmse(pred, actual)
    zero.one.vector[i] <- zero.one(pred, actual)
    print(paste("Fold",i,"complete", sep = " "))
  }
  coeff.variation.rmse <- var(rmse.vector)/mean(rmse.vector)
  print(paste("Coefficient of variation for RMSE:", coeff.variation.rmse, sep = " "))
  coeff.variation.zero.one <- var(zero.one.vector)/mean(zero.one.vector)
  print(paste("Coefficient of variation for 0-1 loss:", coeff.variation.zero.one, sep = " "))
  par(mfrow = c(1,2))
  hist(rmse.vector)
  hist(zero.one.vector)
  par(mfrow = c(1,1))
  return(list(rmse.vector, zero.one.vector))
}

cv.results <- cv.lm(data, form = formula(user_stars ~ average_stars + business_stars))

# Prediction



# Old code
# Predict ratings for individual users
review.sub <- review[sample(1:nrow(review), 100000),]
test.sub <- review[sample(1:nrow(review), 50000),]

positive.count <- function(x) {
  regex.result <- gregexpr("[gG]ood|[gG]reat|[Ee]njoy", x["text"])
  return(length(regex.result[[1]]))
}
review.sub.clean <- review.sub[,-1]
positive.feature <- apply(review.sub.clean, 1, positive.count)
review.sub$positive <- positive.feature

test.sub.clean <- test.sub[,-1]
positive.feature <- apply(test.sub.clean, 1, positive.count)
test.sub$positive <- positive.feature

multi.fit <- multinom(stars ~ positive, data = review.sub)
lm.fit <- lm(as.numeric(stars) ~ positive, data = review.sub)
lm.predict <- predict(lm.fit, newdata = test.sub)

lm.classification <- round(lm.predict)
lm.rmse <- rmse(lm.classification, as.integer(test.sub$stars))
lm.rmse.continuous <- rmse(lm.predict, as.integer(test.sub$stars))
lm.zero.one <- zero.one(lm.classification, as.integer(test.sub$stars))

multi.predict <- predictMNL(multi.fit, newdata = test.sub)
multi.rmse <- rmse(multi.predict, as.integer(test.sub$stars))
multi.zero.one <- zero.one(multi.predict, as.integer(test.sub$stars))
