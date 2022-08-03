---
title: 'Google Data Analytics: Case Study 1 (Cyclistic)'
author: "Ashma"
date: '2022-07-30'
output:
html_document: Default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Overview 

The company named **Cyclistic** wants to know if the Company’s future success depends on maximizing the number of annual memberships. As a result, I am using Cyclistic's historical data to analyze and identify trends. The [datasets](https://divvy-tripdata.s3.amazonaws.com/index.html) consists of millions of data from all kinds of users. The company has two kinds of users and they are casual riders and Cyclistic members where casual members are the customers who purchase single-ride or full-day passes and Cyclistic members are the customers who purchase annual memberships. 

 
## Objectives 

1. Company wants to increase the annual number of members as they are much more profitable for the company.
2. Company wants a new marketing strategy to convert casual riders into members. 


# Preparing Data

I will be using the data from January 2021 to December 2021 for the analysis. The data is reliable, up-to-date, and cited as we will be using the data directly from the company. 

First, we will load all the libraries we need. 
```
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
```

Then we will load the data we downloaded in R using read.csv.
```{r }
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
```
As the data is loaded, let us check whether all the data have the same column name and the data frame. 
We can compare everything individually or by using the 'compare_df_cols()'.

* Comparing each data Individually 
```{r }
str(divvy_tripdata_01_2021)
colnames(divvy_tripdata_01_2021)
```

* Comparing using compare_df_cols() with other data using Janitor library which shows a comparison of the columns in data frames being compared and will show if the data can be bound together or not. 

```
compare_df_cols(divvy_tripdata_01_2021, divvy_tripdata_02_2021, divvy_tripdata_03_2021, divvy_tripdata_04_2021, divvy_tripdata_05_2021, divvy_tripdata_06_2021, divvy_tripdata_07_2021, divvy_tripdata_08_2021, divvy_tripdata_09_2021, divvy_tripdata_10_2021, divvy_tripdata_11_2021, divvy_tripdata_12_2021)
```

# Using rbind

Rbind is a function in R that is used to combine vectors and data frames by rows. 
Since all the data frames are compatible with each other, we will bind them together into one using the rbind function in R. 

```{r }
trip2021 <- rbind(divvy_tripdata_01_2021, divvy_tripdata_02_2021, divvy_tripdata_03_2021, divvy_tripdata_04_2021, divvy_tripdata_05_2021, divvy_tripdata_06_2021, divvy_tripdata_07_2021, divvy_tripdata_08_2021, divvy_tripdata_09_2021, divvy_tripdata_10_2021, divvy_tripdata_11_2021, divvy_tripdata_12_2021)

```

# Processing Data 
You can view the data using view() and also check whether the data frames have changed or not using str().
```
view(trip2021)
str(trip2021)
colnames(trip2021)
```
We can check the dimensions of the data using dim().
```{r }
dim(trip2021)
```
let us see the summary of the data.
``` {r }
summary(trip2021)
```

# Changing the data types

Here, we can see that the data type for the started_at and ended_at are saved as character data types so let us change the data type to DATE and TIME. 

```
trip2021$started_at = strptime(trip2021$started_at, "%Y-%m-%d %H:%M:%S")
trip2021$ended_at = strptime(trip2021$ended_at, "%Y-%m-%d %H:%M:%S")
```
Let us check whether the data type has changed or not.
```{r }
str(trip2021)
```
To understand the data better, let us separate the date into the day, month, and year. 
```{r }
trip2021$date <- as.Date(trip2021$started_at)
trip2021$year <- format(as.Date(trip2021$date), "%Y")
trip2021$month <- format(as.Date(trip2021$date), "%m")
trip2021$day <- format(as.Date(trip2021$date), "%d")
trip2021$day_of_week <- format(as.Date(trip2021$date), "%A")
```
Let us also check the difference in time between started_at and ended_at and save them at ride_length.
```
trip2021$ride_length <- difftime(trip2021$ended_at, trip2021$started_at)
```

Checking whether the data has been updated as we desired:
```{r }
colnames(trip2021)
str(trip2021)
```
All the data has been updated as we desired but the ride_length has to be numeric for the calculation so let us change the data type of ride_length to numeric.
```
trip2021$ride_length <- as.numeric(as.character(trip2021$ride_length))
```

# Analyzing the data 

When we look at the data in ride_length, some data are less than 0 which is unnecessary data that we don't need. So, let us filter the data that are less than 0 and remove them from the data. 

```{r }
filter(trip2021, ride_length < 0) 
trip2021 <- trip2021[!(trip2021$ride_length < 0), ]
```

Now the data is clean, we will calculate the mean, median, max, and min of the ride_length for further analysis. we will also calculate the mode of day_of_week for further analysis. 

```{r }
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

```

Now, we will extract the data and do further analysis in TABLEAU since it's much easier and more interactive. 

```
write.csv(trip2021, "C:\\Users\\Ashma Rai\\Documents\\Cyclist_trip2021.csv")

```
