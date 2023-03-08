# Install Packages
install.packages("sf")
install.packages("spatstat")
install.packages("stars")
install.packages("tidyverse")
install.packages("caret")
install.packages("car")


# Load the necessary libraries and the dataset
library(sf)
library(spatstat)
library(tidyverse)
library(caret)
library(car)

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load Geo package
miamiCrimeData <- st_read("com_police_data.gpkg")
head(miamiCrimeData) # View the first few rows of the dataset
summary(miamiCrimeData) # View a summary of the dataset
unique_crime_types <- unique(miamiCrimeData$crime_type) # Check unique values in column "crime_type"
print(unique_crime_types)

# Check the structure and summary statistics of the dataset
str(df)
summary(df)

# Create a linear regression model to predict incident_count based on other variables
model <- lm(incident_count ~ day_of_week + beat + neighborhood + lat + lon, data = df)

# Check the model summary to see the coefficients and significance levels
summary(model)

# Check the normality of residuals using a Q-Q plot and a Shapiro-Wilk test
qqPlot(model, main="Q-Q Plot") # requires the car library
shapiro.test(residuals(model))

# Check the linearity of the model using a partial residual plot
crPlots(model) # requires the car library
