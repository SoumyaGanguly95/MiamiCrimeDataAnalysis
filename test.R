# Install and load the necessary packages: sf and dplyr.
install.packages(c("sf", "dplyr"))
library(sf)
library(dplyr)

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

st_layers("com_police_data.gpkg")
miami_data <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22")
head(miami_data)
summary(miami_data)

colnames(miami_data)

# Group the data by hours and calculate the total number of incidents
miami_data %>%
  group_by(hour) %>%
  summarise(incident_count = n())

