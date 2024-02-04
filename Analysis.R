install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)

jan01_df <- read_csv("202301-divvy-tripdata.csv")
feb02_df <- read_csv("202302-divvy-tripdata.csv")
mar03_df <- read_csv("202303-divvy-tripdata.csv")
apr04_df <- read_csv("202304-divvy-tripdata.csv")
may05_df <- read_csv("202305-divvy-tripdata.csv")
june06_df <- read_csv("202306-divvy-tripdata.csv")
july07_df <- read_csv("202307-divvy-tripdata.csv")
aug08_df <- read_csv("202308-divvy-tripdata.csv")
sept09_df <- read_csv("202309-divvy-tripdata.csv")
oct10_df <- read_csv("202310-divvy-tripdata.csv")
nov11_df <- read_csv("202311-divvy-tripdata.csv")
dec12_df <- read_csv("202312-divvy-tripdata.csv")

#binding data together
cyclistic_full_df <- rbind(jan01_df, feb02_df, mar03_df, apr04_df, may05_df, june06_df, july07_df, aug08_df, sept09_df, oct10_df, nov11_df, dec12_df)

#creating a new column - months
cyclistic_full_df$months <- format(as.Date(cyclistic_full_df$started_at, format = "%m/%d/%Y"), "%m")

#creating a new column - quarter
cyclistic_full_df <- cyclistic_full_df %>% mutate(quarter =
                                                    case_when(months == "01" ~ "Q1",
                                                              months == "02" ~ "Q1",
                                                              months == "03" ~ "Q1",
                                                              months == "04" ~ "Q2",
                                                              months == "05" ~ "Q2",
                                                              months == "06" ~ "Q2",
                                                              months == "07" ~ "Q3",
                                                              months == "08" ~ "Q3",
                                                              months == "09" ~ "Q3",
                                                              months == "10" ~ "Q4",
                                                              months == "11" ~ "Q4",
                                                              months == "12" ~ "Q4")
                                                  )

#data cleaning
cyclistic_full_df <- na.omit(cyclistic_full_df)
cyclistic_full_df <- distinct(cyclistic_full_df)
cyclistic_full_df <- cyclistic_full_df[!(cyclistic_full_df$ride_length <= 0),]
cyclistic_full_df <- cyclistic_full_df %>%
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))

View(cyclistic_full_df)

#----TOTAL RIDES----

#total no of rides
total_no_of_rides <- nrow(cyclistic_full_df)
print(total_no_of_rides)

#total rides based on member type
cyclistic_full_df %>%
  count(member_casual)

#total rides based on member type and bike type
cyclistic_full_df %>%
  group_by(member_casual, rideable_type) %>%
  count(rideable_type)

#total rides based on bike type
cyclistic_full_df %>%
  count(rideable_type)

#total rides of member type based on month
mem_cas_months <- cyclistic_full_df %>%
  group_by(member_casual) %>%
  count(months)
View(mem_cas_months)

#total rides based on months
cyclistic_full_df %>%
  count(months)

#total rides of member type on each quarter
cyclistic_full_df %>%
  group_by(member_casual) %>%
  count(quarter)

#total rides based on each quarter
cyclistic_full_df %>%
  count(quarter)

#total rides of member type on each day of the week
cyclistic_full_df %>%
  group_by(member_casual) %>%
  count(day_of_week)

#total rides on each day of the week throughout the year
cyclistic_full_df %>%
  count(day_of_week)

#total rides on each day of the week in Jan month
cyclistic_full_df %>%
  group_by(months, day_of_week) %>%
  filter(months == "01") %>%
  count(months)

#total rides of member type on each day of the week for every month
mem_cas_each_day <- cyclistic_full_df %>%
  group_by(member_casual,months, day_of_week) %>%
  count(months)
View(mem_cas_each_day)

#total rides of member type in the first quarter
cyclistic_full_df %>%
  group_by(member_casual) %>%
  filter(quarter == "Q1") %>%
  count(quarter)

#total rides of member type in the second quarter
cyclistic_full_df %>%
  group_by(member_casual) %>%
  filter(quarter == "Q2") %>%
  count(quarter)

#total rides of member type in the third quarter
cyclistic_full_df %>%
  group_by(member_casual) %>%
  filter(quarter == "Q3") %>%
  count(quarter)

#total rides of member type in the fourth quarter
cyclistic_full_df %>%
  group_by(member_casual) %>%
  filter(quarter == "Q4") %>%
  count(quarter)

#----RIDE LENGTH----

#average ride length
avg_ride_length <- mean(cyclistic_full_df$ride_length)
print(avg_ride_length)

#average ride length based on member type
cyclistic_full_df %>%
  group_by(member_casual) %>%
  summarise(mean(ride_length))

#average ride length based on member type and bike type
cyclistic_full_df %>%
  group_by(member_casual,rideable_type) %>%
  summarise(mean(ride_length))

#average ride length based on bike type
cyclistic_full_df %>%
  group_by(rideable_type) %>%
  summarise(mean(ride_length))

#average ride length of member type based on month
mem_case_avg_month <- cyclistic_full_df %>%
  group_by(member_casual, months) %>%
  summarise(mean(ride_length))
View(mem_case_avg_month)

#average ride length based on month
cyclistic_full_df %>%
  group_by(months) %>%
  summarise(mean(ride_length))

#average ride length of member type in each quarter
cyclistic_full_df %>%
  group_by(member_casual, quarter) %>%
  summarise(mean(ride_length))

#average ride length in each quarter
cyclistic_full_df %>%
  group_by(quarter) %>%
  summarise(mean(ride_length))

#average ride length of member type on each day
cyclistic_full_df %>%
  group_by(member_casual, day_of_week) %>%
  summarise(mean(ride_length))

#average ride length on each day
cyclistic_full_df %>%
  group_by(day_of_week) %>%
  summarise(mean(ride_length))

#average ride length on each day of the week in Jan
cyclistic_full_df %>%
  group_by(months, day_of_week) %>%
  filter(months == "01") %>%
  summarise(mean(ride_length))

#average ride length of member type on each day of the week for every month
mem_cas_avg_day <- cyclistic_full_df %>%
  group_by(member_casual,months, day_of_week) %>%
  summarise(mean(ride_length))
View(mem_cas_avg_day)

#average ride length of member type in the first quarter
cyclistic_full_df %>%
  group_by(member_casual, quarter) %>%
  filter(quarter == "Q1") %>%
  summarise(mean(ride_length))

#average ride length of member type in the second quarter
cyclistic_full_df %>%
  group_by(member_casual, quarter) %>%
  filter(quarter == "Q2") %>%
  summarise(mean(ride_length))

#average ride length of member type in the third quarter
cyclistic_full_df %>%
  group_by(member_casual, quarter) %>%
  filter(quarter == "Q3") %>%
  summarise(mean(ride_length))

#average ride length of member type in the fourth quarter
cyclistic_full_df %>%
  group_by(member_casual, quarter) %>%
  filter(quarter == "Q4") %>%
  summarise(mean(ride_length))

#----END----
