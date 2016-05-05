# set working directory to the correct folder
setwd("../Desktop/stat151a_final_project/Yelp-Data-Challenge/")

# load the dataset
library(jsonlite)
business <- stream_in(file("yelp_academic_dataset_business.json"))
review <- stream_in(file("yelp_clean_training1.json"))

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
