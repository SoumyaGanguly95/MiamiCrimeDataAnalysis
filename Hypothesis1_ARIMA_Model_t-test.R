# Load the necessary libraries and the dataset
library(tidyverse)
library(lubridate)
library(forecast)


# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load Geo package and read the required layers and working on the hypothesis
miami_data <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22") %>% 
  st_transform(4326) %>% 
  st_make_valid() # Make geometries valid

# Filter the dataset to only include the crime type and area of interest
crime_name <- "SIMPLE ASSAULT"
area <- "City of Miami"
miami_data_filtered <- miami_data %>%
  filter(crime_type == crime_name & county == area)

colnames(miami_data_filtered)

# Calculating incident count every week
miami_data_filtered <- miami_data_filtered %>%
  group_by(week) %>%
  mutate(incident_count = n()) %>%
  ungroup()
# Converting the data frame into a time-series object
miami_df_ts <- ts(miami_data_filtered$incident_count, frequency = 7) # assuming weekly data

# Create a time series plot of the crime counts
plot(miami_df_ts, main = paste0("Crime Counts for ", crime_name, " in ", area))

# Fit an ARIMA model to the time series data
arima_model <- auto.arima(miami_df_ts)

# Check the model summary to see the coefficients and significance levels
summary(arima_model)

# Use the model to make predictions for the next 4 weeks
predictions <- forecast(arima_model, h = 4)

# Use hypothesis testing to compare the mean crime count during periods with a high previous crime history to the mean crime count during periods with a low previous crime history
# For example, we could split the time series data into two groups based on whether the crime count in the previous time period was above or below the median, and compare the mean crime count for the two groups
prev_crime_history <- miami_df_ts[-1]
prev_crime_median <- median(prev_crime_history)
low_prev_crime <- prev_crime_history[prev_crime_history <= prev_crime_median]
high_prev_crime <- prev_crime_history[prev_crime_history > prev_crime_median]
mean(low_prev_crime)
mean(high_prev_crime)
t.test(low_prev_crime, high_prev_crime)


