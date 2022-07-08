library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)

#Load datasets
X202103_divvy_tripdata <- read_csv("Google project/202103-divvy-tripdata.csv")
X202104_divvy_tripdata <- read_csv("Google project/202104-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("Google project/202105-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("Google project/202106-divvy-tripdata.csv")
X202107_divvy_tripdata <- read_csv("Google project/202107-divvy-tripdata.csv")
X202108_divvy_tripdata <- read_csv("Google project/202108-divvy-tripdata.csv")
X202109_divvy_tripdata <- read_csv("Google project/202109-divvy-tripdata.csv")
X202110_divvy_tripdata <- read_csv("Google project/202110-divvy-tripdata.csv")
X202111_divvy_tripdata <- read_csv("Google project/202111-divvy-tripdata.csv")
X202112_divvy_tripdata <- read_csv("Google project/202112-divvy-tripdata.csv")
X202201_divvy_tripdata <- read_csv("Google project/202201-divvy-tripdata.csv")
X202202_divvy_tripdata <- read_csv("Google project/202202-divvy-tripdata.csv")

#Check column names of each dataset for consistency
colnames(X202103_divvy_tripdata)
colnames(X202104_divvy_tripdata)
colnames(X202105_divvy_tripdata)
colnames(X202106_divvy_tripdata)
colnames(X202107_divvy_tripdata)
colnames(X202108_divvy_tripdata)
colnames(X202109_divvy_tripdata)
colnames(X202110_divvy_tripdata)
colnames(X202111_divvy_tripdata)
colnames(X202112_divvy_tripdata)
colnames(X202201_divvy_tripdata)
colnames(X202202_divvy_tripdata)

#Check data structures and data types for all data frames
str(X202103_divvy_tripdata)
str(X202104_divvy_tripdata)
str(X202105_divvy_tripdata)
str(X202106_divvy_tripdata)
str(X202107_divvy_tripdata)
str(X202108_divvy_tripdata)
str(X202109_divvy_tripdata)
str(X202110_divvy_tripdata)
str(X202111_divvy_tripdata)
str(X202112_divvy_tripdata)
str(X202201_divvy_tripdata)
str(X202202_divvy_tripdata)



# Removing  empty rows & columns & checking for missing values
all_trips <- bind_rows(X202103_divvy_tripdata, X202104_divvy_tripdata, X202105_divvy_tripdata, X202106_divvy_tripdata, X202107_divvy_tripdata, X202108_divvy_tripdata, X202109_divvy_tripdata, X202110_divvy_tripdata, X202111_divvy_tripdata, X202112_divvy_tripdata, X202201_divvy_tripdata, X202202_divvy_tripdata)
str(all_trips)

# remove NA values in the entire data frame
new_all_trips <- na.omit(all_trips)

#emoving any duplicate
new_all_trips_no_dups <- new_all_trips[!duplicated(new_all_trips$ride_id), ]


clean_all_trips <- new_all_trips_no_dups
#Remove columns not required in the project
clean_all_trips <- clean_all_trips %>%
  select(-c(start_lat:end_lng))
glimpse(clean_all_trips)

#Rename columns for better readability
clean_all_trips <- clean_all_trips %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(clean_all_trips)


#column for day of the week the trip started
clean_all_trips $day_of_the_week <- format(as.Date(clean_all_trips $start_time),'%a')

#column for month when the trip started
clean_all_trips $month <- format(as.Date(clean_all_trips $start_time),'%b_%y')

#Dates are not needed for extracting the duration of each trip, but only hours, minutes and seconds.
clean_all_trips $time <- format(clean_all_trips $start_time, format = "%H:%M")
clean_all_trips $time <- as.POSIXct(clean_all_trips $time, format = "%H:%M")

#column for trip duration in min
clean_all_trips $trip_duration <- (as.double(difftime(clean_all_trips $end_time, clean_all_trips $start_time)))/60

glimpse(clean_all_trips )

# checking for trip lengths less than 0
nrow(subset(clean_all_trips ,trip_duration < 0))

#checking for testrides that were made by company for quality checks
nrow(subset(clean_all_trips , start_station_name %like% "TEST"))
nrow(subset(clean_all_trips , start_station_name %like% "test"))
nrow(subset(clean_all_trips , start_station_name %like% "Test"))

# remove negative trip durations 
clean_all_trips_v2 <- clean_all_trips [!(clean_all_trips$trip_duration < 0),]

#remove test rides
clean_all_trips_v2<- clean_all_trips_v2[!((clean_all_trips_v2$start_station_name %like% "TEST" | clean_all_trips_v2$start_station_name %like% "test")),]

#check dataframe
glimpse(clean_all_trips_v2)


##Analyze the data
#summary of trip_duration for all trips
summary(clean_all_trips_v2$trip_duration)

#summary of trip_duration by customer_type
clean_all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))
#the mean of trip duration of casual riders is higher the the mean of trip duration of member. This tells us casual riders bike longer than member each trip.


#######
# fix the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations
clean_all_trips_v2$day_of_the_week <- ordered(clean_all_trips_v2$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
clean_all_trips_v2$month <- ordered(clean_all_trips_v2$month, levels=c("Mar_21", "Apr_21", "May_21", "Jun_21", "Jul_21", "Aug_21", "Sep_21",
                                                                       "Oct_21", "Nov_21", "Dec_21", "Jan_22", "Feb_22"))

clean_all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))

#######
clean_all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# The table tells us causual members bike more on the weekends than the weekdays. Also causual members bike more than annual members on weekends. 
# while the annual members is the opposite. annual members bike more on the weekdays than weekends. Also annual members bike less than causual members on weekends.

#Visualization:
#Total Trips by Customer Type V.S. Month

unique(all_trips$month)

clean_all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))

clean_all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total Trips by Customer Type V.S. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#The graph shows that the months of July, August and September are the most busy time of the year among both members and casual riders.
#This is probably due to external factor such as cold weather that less people bike during winter.
#The number of trips made by members is always higher than the casual riders across all months of the year except summer (July & Auguest)

#average trip duration by customer type on each day of the week

clean_all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type V.S. Day of the week")

#The average trip duration of a casual rider is more than twice that of a member.  Also, weekends not only contribute to more number of trips but also longer trips on average when compared to weekdays.

# average trip duration by customer type V.S. month
clean_all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type V.S. Month") +
  theme(axis.text.x = element_text(angle = 30))

#members ride between 10-15mins each trip on average throughout the year, while casual riders bikes at 20mins each trip and close to 40mins depends on the time of the year


#bike demand over 24 hr period per day:
  
clean_all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#For members, two peak demand hours: 7-9 AM and 4-7 PM, and 11am-3pm is still high in demand before 5-7pm peak hour.
#for casual riders, only peak hours is 4-7pm
# one reason for the high demand during 4-7pm is people get off from work & use bike to communite after work. but we need more data to substabtiate this assumption.

#ride type V.S. number of trips by customer type
clean_all_trips_v2 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type V.S. Number of trips")

# classic bike & electri bike are used more by members. only casual riders use docked bike.

#Creating a csv file of the clean data for futher analysis or visualizations 
clean_data <- aggregate(clean_all_trips_v2$trip_duration ~ clean_all_trips_v2$customer_type + clean_all_trips_v2$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)

#Summary
#1. Casual riders use bikeshare more during weekends, while members use them consistently over the entire week.
#2. Average trip duration of casual riders is more than twice that of member rider over any given day of the week cumulatively.
#3. Casual riders bike longer during spring & summer, while members relatively similar average trip duration month over month.
#4. Only Casual riders use docked bike. Member prefers classical bikes.


#Recommendation 
#1. Provide attractive promotions for casual riders on weekdays so that casual members use the bikeshare services ore uniformly across the entire week.
#2. Offer discounted pricing during non-busy hours so that casual riders might choose to use bikes more often and level out demand over the day.
#3. Provide discounted membership fee for renewals after the first year. It might attract casual riders to take up membership.





