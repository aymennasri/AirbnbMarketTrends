library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(lubridate)
library(anytime)
#Objectives :
#What are the dates of the earliest and most recent reviews? Store these 
#values as two separate variables with your preferred names.
#How many of the listings are private rooms? Save this into any variable.
#What is the average listing price? Round to the nearest penny and save into 
#a variable.
#Combine the new variables into one tibble called review_dates with four columns 
#in the following order: first_reviewed, last_reviewed, nb_private_rooms, 
#and avg_price. The tibble should only contain one row of values.

price_data <- read_csv("airbnb_price.csv")
room_type_data <- read_excel("airbnb_room_type.xlsx")
last_review_data <- read_tsv("airbnb_last_review.tsv")

# Checking whether the dates are correctly written or not
head(last_review_data, n = 10)
last_review_data %>% filter(last_review == is.Date(last_review_data$last_review))
last_review_date <- anydate(last_review_data$last_review)
n_rev_data <- cbind(last_review_data, last_review_date)
is.Date(last_review_date)
# Storing the earliest and the most recent reviews
earliest_review <- min(n_rev_data$last_review_date)
most_recent_review <- max(n_rev_data$last_review_date)

# Checking the room_type variable
head(room_type_data, n = 10)
# The private room value is written differently each time, we have to change it
# to one value
lower_room_type <- str_to_lower(room_type_data$room_type)
head(lower_room_type, n = 10)
n_room_data <- cbind(room_type_data, lower_room_type)

# Counting the listings of private rooms
priv_room_count <- n_room_data %>% filter(lower_room_type == "private room") %>%
  count()
priv_room_count <- as.numeric(priv_room_count)

# Average listing price
head(price_data, n = 10)
num_price <- price_data$price %>% str_remove("dollars")
num_price <- as.numeric(num_price)
summary(is.na(num_price))
cbind(price_data, num_price)
avg_price <- round(mean(num_price), digits = 2)
avg_price
# Tibble containing the values we extracted
review_dates <- tibble(earliest_review, most_recent_review,
                       priv_room_count, avg_price,
                       .name_repair = ~ c("first_reviewed", "last_reviewed", 
                                          "nb_private_rooms", "avg_price"))
review_dates
