# Point pattern analysis could potentially be used to study whether previous 
# crime history of a certain crime type influences subsequent crimes in 
# an area. Point pattern analysis is a spatial statistics technique that 
# involves analyzing the locations of point events (in this case, crimes) 
# within a study area.
# 
# One approach could be to use kernel density estimation (KDE) to estimate 
# the underlying crime intensity surface in the area of interest, and then 
# compare the intensity surface between time periods to see if there are any
# changes or patterns. For example, we could compare the intensity surface 
# during time periods with a high previous crime history to the intensity 
# surface during time periods with a low previous crime history.
# Load the necessary libraries and the data set


# Install Packages
install.packages("sf")
install.packages("spatstat")
install.packages("stars")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("dplyr")
install.packages("RSQLite")
install.packages("tidyverse")

# Load the libraries
library(sf) |> suppressPackageStartupMessages()
library(spdep) |> suppressPackageStartupMessages()
library(spatstat) |> suppressPackageStartupMessages()
library(stars)
library(ggplot2)
library(rgdal)
library(dplyr)
library(RSQLite)
library(tidyverse)

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load Geo package
miamiCrimeData <- st_read("com_police_data.gpkg")
# head(miamiCrimeData) # View the first few rows of the dataset
# summary(miamiCrimeData) # View a summary of the dataset
# unique_crime_types <- unique(miamiCrimeData$crime_type) # Check unique values in column "crime_type"
# print(unique_crime_types)

# Explore the layers available
layers <- st_layers("com_police_data.gpkg")
ogrListLayers("com_police_data.gpkg")


# Reading  the required layers and working on the hypothesis
layer_violent_crime_2021_22 <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22")
colnames(layer_violent_crime_2021_22)

# For a specific crime type calculating the crime counts
crime_counts_violent_crime <- layer_violent_crime_2021_22 %>%
  group_by(crime_type) %>%
  mutate(crime_type_count = n()) %>%
  ungroup()


crimes <- unique(crime_counts_violent_crime$crime_type)
print(crimes)
crime_name = "ROBBERY - ARMED"
area = "City of Miami"
violent_crime_filtered <- crime_counts_violent_crime %>%
  filter(crime_type == crime_name & county == area )


# Create a spatial point pattern object from the crime locations
df_points_violent_crime_filtered <- violent_crime_filtered %>%
  select(longitude, latitude) %>%
  as.data.frame()

# Renaming longitude and latitude data
colnames(df_points_violent_crime_filtered) <- c("x", "y")

ppp_points_violent_crime <- ppp(
  x = df_points_violent_crime$x,
  y = df_points_violent_crime$y)

# Plot the intensity surface for the entire time period
plot(kde, main = paste0("Crime Intensity Surface for ", crime_name, " in ", area))


# Split the time series data into two groups based on whether the crime count in the previous time period was above or below the median
prev_crime_history <- df_filtered$incident_count[-1]
prev_crime_median <- median(prev_crime_history)
df_filtered_low <- df_filtered[prev_crime_history <= prev_crime_median, ]
df_filtered_high <- df_filtered[prev_crime_history > prev_crime_median, ]

# Create spatial point pattern objects from the crime locations for the low and high previous crime history groups
df_points_low <- df_filtered_low %>%
  select(lon, lat) %>%
  as.data.frame() %>%
  ppp()
df_points_high <- df_filtered_high %>%
  select(lon, lat) %>%
  as.data.frame() %>%
  ppp()

# Perform a nearest neighbor analysis for each group and compare the results
nn_low <- nncross(df_points_low)
nn_high <- nncross(df_points_high)

# Plot the empirical cumulative distribution functions (ECDFs) of nearest neighbor distances for the low and high previous crime history groups
plot(ecdf(nn_low$dist), main = paste0("Nearest Neighbor Analysis for ", crime_type, " in ", area, ": Low Previous Crime History"))
plot(ecdf(nn_high$dist), main = paste0("Nearest Neighbor Analysis for ", crime_type, " in ", area, ": High Previous Crime History"))
