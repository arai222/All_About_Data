#loading all the libraries we might need

library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)

#loading the all the data we need to R

divvy_tripdata_01_2021 <- read.csv("202101-divvy-tripdata.csv")
divvy_tripdata_02_2021 <- read.csv("202102-divvy-tripdata.csv")
divvy_tripdata_03_2021 <- read.csv("202103-divvy-tripdata.csv")
divvy_tripdata_04_2021 <- read.csv("202104-divvy-tripdata.csv")
divvy_tripdata_05_2021 <- read.csv("202105-divvy-tripdata.csv")
divvy_tripdata_06_2021 <- read.csv("202106-divvy-tripdata.csv")
divvy_tripdata_07_2021 <- read.csv("202107-divvy-tripdata.csv")
divvy_tripdata_08_2021 <- read.csv("202108-divvy-tripdata.csv")
divvy_tripdata_09_2021 <- read.csv("202109-divvy-tripdata.csv")
divvy_tripdata_10_2021 <- read.csv("202110-divvy-tripdata.csv")
divvy_tripdata_11_2021 <- read.csv("202111-divvy-tripdata.csv")
divvy_tripdata_12_2021 <- read.csv("202112-divvy-tripdata.csv")

# looking the internal structure of the data 
str(divvy_tripdata_01_2021)
colnames(divvy_tripdata_01_2021)


# now comparing with other datasets using Janitor library which shows a comparison of the columns in data frames being compared
compare_df_cols(divvy_tripdata_01_2021, divvy_tripdata_02_2021, divvy_tripdata_03_2021, divvy_tripdata_04_2021, divvy_tripdata_05_2021, divvy_tripdata_06_2021, divvy_tripdata_07_2021, divvy_tripdata_08_2021, divvy_tripdata_09_2021, divvy_tripdata_10_2021, divvy_tripdata_11_2021, divvy_tripdata_12_2021)


# binding all the data together using rbind 
trip2021 <- rbind(divvy_tripdata_01_2021, divvy_tripdata_02_2021, divvy_tripdata_03_2021, divvy_tripdata_04_2021, divvy_tripdata_05_2021, divvy_tripdata_06_2021, divvy_tripdata_07_2021, divvy_tripdata_08_2021, divvy_tripdata_09_2021, divvy_tripdata_10_2021, divvy_tripdata_11_2021, divvy_tripdata_12_2021)

view(trip2021)

str(trip2021)

dim(trip2021)

summary(trip2021)

#changing the data type of started_at and ended_at

trip2021$started_at = strptime(trip2021$started_at, "%Y-%m-%d %H:%M:%S")
trip2021$ended_at = strptime(trip2021$ended_at, "%Y-%m-%d %H:%M:%S")

str(trip2021)

#let us create a different column for month, day,  and date separately

trip2021$date <- as.Date(trip2021$started_at)
trip2021$year <- format(as.Date(trip2021$date), "%Y")
trip2021$month <- format(as.Date(trip2021$date), "%m")
trip2021$day <- format(as.Date(trip2021$date), "%d")
trip2021$day_of_week <- format(as.Date(trip2021$date), "%A")

# let us check the difference in time between started_at and ended_at and saving them at ride_length

trip2021$ride_length <- difftime(trip2021$ended_at, trip2021$started_at)

colnames(trip2021)
str(trip2021)

# changing the data type of the ride_length into numeric. 
trip2021$ride_length <- as.numeric(as.character(trip2021$ride_length))

# Analyzing the data 
view(trip2021)

str(trip2021)

# there is a high probability that some of the data in ride_length are less than 0 so we will filter them and see if there are any such data
# if so, we will remove them 

filter(trip2021, ride_length < 0)

#removing the ride_length less than 0

trip2021 <- trip2021[!(trip2021$ride_length < 0), ]
dim(trip2021)

# let us find mean, median, max, and min of ride_length 

max(trip2021$ride_length)
min(trip2021$ride_length)
mean(trip2021$ride_length)
median(trip2021$ride_length)

#mode of the day_of_week
mode = function()
{
  return(sort(-table(trip2021$day_of_week))[1])
}
mode()

# file extraction


write.csv(trip2021, "C:\\Users\\Ashma Rai\\Documents\\Cyclist_trip2021.csv")

