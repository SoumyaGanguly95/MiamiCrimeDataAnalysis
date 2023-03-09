
# Load the necessary libraries and the dataset
library(tidyverse)
library(caret)
library(car)
library(nortest)


# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load Geo package and read the required layers and working on the hypothesis
miami_data <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22") %>% 
  st_transform(4326) %>% 
  st_make_valid() # Make geometries valid

# Check the structure and summary statistics of the dataset
str(miami_data)
summary(miami_data)

miami_data <- miami_data %>%
  group_by(date_eu) %>%
  mutate(incident_count = n()) %>%
  ungroup()

# Create a linear regression model to predict incident_count based on other variables
model <- lm(incident_count ~ weekday + county + hour + latitude + longitude, data = miami_data)

# Check the model summary to see the coefficients and significance levels
summary(model)

# Check the normality of residuals using a Q-Q plot and a Anderson Darling test
qqPlot(model, main="Q-Q Plot") # requires the car library
ad.test(residuals(model))

# Check the linearity of the model using a partial residual plot
crPlots(model) # requires the car library
