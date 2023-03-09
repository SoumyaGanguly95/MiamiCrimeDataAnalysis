# Load the libraries
library(sf) |> suppressPackageStartupMessages()
library(spdep) |> suppressPackageStartupMessages()
library(spatstat) |> suppressPackageStartupMessages()
library(stars)
library(ggplot2)
library(rgdal)
library(tidyverse)
library(sf)
library(dplyr)

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Explore the layers available
layers <- st_layers("com_police_data.gpkg")
ogrListLayers("com_police_data.gpkg")

# Load the libraries
library(sf)
library(dplyr)

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load the crime dataset
miami_data <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22") %>% 
  st_transform(4326) %>% 
  st_make_valid()

# Load the neighbourhood dataset
layer_nhoods <- st_read("com_police_data.gpkg", layer = "com_nhoods") %>% 
  st_transform(4326) %>% 
  st_make_valid()

# Join the two datasets based on spatial intersection
miami_data_with_neighbourhood <- st_join(layer_nhoods, miami_data)

# Renaming LABEL column in miami_data_with_neighbourhood to neighbourhood
miami_data_with_neighbourhood <- miami_data_with_neighbourhood %>% 
  rename(neighbourhood = LABEL)

# Load the aggregated layer dataset
layer_aggregated_data <- read_sf("com_police_data.gpkg", layer = "com_aggregated_data", quiet = TRUE)

# Join the two datasets based on the common attribute neighbourhood
miami_crime_data_with_nhoods_prox <- left_join(miami_data_with_neighbourhood, layer_aggregated_data, by = "neighbourhood")

# View the resulting dataset
head(miami_crime_data_with_nhoods_prox)

crimes <- unique(miami_crime_data_with_nhoods_prox$crime_type)
print(crimes)

# Filter dataset for a specific crime (e.g., "Robbery")
specific_crime <- "ROBBERY / ARMED W OTHER THAN DEADLY WEAPON"
filtered_data <- miami_crime_data_with_nhoods_prox %>%
  filter(crime_type == specific_crime)


# Create a new column "incident_count" based on number of crimes per neighborhood
miami_crime_data_optimized <- filtered_data %>%
  group_by(neighbourhood, crime_type, county, geom, dist_bank, dist_convenience, dist_bars, dist450m_bank) %>%
  summarise(incident_count = n())

# View the resulting dataset
head(miami_crime_data_optimized)

# Remove any missing data
miami_crime_data_optimized <- na.omit(miami_crime_data_optimized)

# Fit the regression model
model <- lm(incident_count ~ dist_bank + dist_bars + dist_convenience, dist450m_bank, data = miami_crime_data_optimized)

# Print the model summary
summary(model)

# Test the hypothesis using ANOVA
anova(model)

# Perform the Wilcoxon rank-sum test
wilcox.test(incident_count ~ dist450m_bank, data = miami_crime_data_optimized)

# Visualize the results
ggplot(crime_data, aes(x = dist_bank, y = violent_crime_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Relationship between crime rate and distance to nearest bank")