# Load required packages
library(sf)
library(tidyverse)

# Setting current working directory
setwd("D:/MyWorkspace/AOSD-Course/R-Workspace/MiamiCrimeDataAnalysis/data")

# Load Geo package
miami_data <- st_read("com_police_data.gpkg") %>% 
  st_transform(4326) %>% 
  st_make_valid() # Make geometries valid

# List all layer names and their type in the data source
st_layers("com_police_data.gpkg")

# Load Geo package and read the required layers and working on the hypothesis
miami_data_violent_crimes <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22") %>% 
  st_transform(4326) %>% 
  st_make_valid() # Make geometries valid

colnames(miami_data_violent_crimes)

# Get bounding box for entire dataset
bbox_violent_crimes <- st_bbox(miami_data_violent_crimes)

# Print bounding box
bbox_violent_crimes

crimes <- unique(miami_data_violent_crimes$crime_type)
print(crimes)

# Filter data to focus on a specific region and crime type 
crime_name <- "SIMPLE ASSAULT" # Change this to the specific crime type you want to analyze
region <- st_bbox(c(xmin=-80.31578, ymin=25.70949, xmax=-80.15572, ymax=25.85503), crs=4326) # Change this to the specific region you want to analyze

# Create a spatial object from the bounding box
region_sf <- st_as_sfc(region)
region_buffered <- region_sf %>% 
  st_buffer(0.01) # Adjust the buffer distance as needed

# Filter data and perform intersection
miami_filtered_violent_crimes <- miami_data_violent_crimes %>% 
  filter(crime_type==crime_name) %>% 
  st_intersection(region_buffered)


# Plot crime locations to visualize the data
ggplot(miami_filtered_violent_crimes) +
  geom_sf()

# Calculate crime counts for different time periods and counties
time_periods <- c("2021-11-28/2021-12-28", "2022-01-23/2022-03-31") # Change these to the specific time periods you want to analyze
time_period_breaks <- as.POSIXct(unlist(strsplit(time_periods, "/")), format="%Y-%m-%d") # Convert each element of time_periods to a POSIXct object
crime_counts <- miami_filtered_violent_crimes %>% 
  group_by(year=as.numeric(substr(date_eu, 1, 4)), county=as.factor(county), time_period=cut(as.POSIXct(date_eu), breaks=time_period_breaks)) %>% 
  summarize(count=n())

# Plot crime counts over time to visualize trends
ggplot(crime_counts, aes(x=time_period, y=count, color=as.factor(year))) +
  geom_point() +
  geom_line() +
  labs(x="Time Period", y="Crime Count", color="Year")

# Create a contingency table of the counts of the two variables

# To find correlation between yearly crime counts with respect to area
cont_table_year_county <- table(crime_counts$year, crime_counts$county)
chisq_result_year_county <- chisq.test(cont_table_year_county) # Perform the chi-square test on the contingency table
chisq_result_year_county
chisq_result_year_county$p.value # Check the p-value to determine whether to reject or fail to reject the null hypothesis

# To find correlation between crime counts in an area with respect to time period
cont_table_year_period <- table(crime_counts$time_period, crime_counts$county)
chisq_result_year_period <- chisq.test(cont_table_year_period) # Perform the chi-square test on the contingency table
chisq_result_year_period
chisq_result_year_period$p.value # Check the p-value to determine whether to reject or fail to reject the null hypothesis
