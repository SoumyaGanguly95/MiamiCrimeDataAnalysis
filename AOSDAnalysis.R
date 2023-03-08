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

# Getting current working directory
getwd()

# Setting current working directory
setwd("D://MyWorkspace//AOSD-Course//R-Workspace//AOSDFinalAssignment//data")

# Load Geo Package
miamiCrimeData = "com_police_data.gpkg"

# Explore the layers available
layers <- st_layers("com_police_data.gpkg")
ogrListLayers("com_police_data.gpkg")


# Reading all the layers

layer_burg_epsg <- st_read("com_police_data.gpkg", layer = "com_burg_epsg3511")

layer_nhoods <- st_read("com_police_data.gpkg", layer = "com_nhoods")

layer_violent_crime_2021_22 <- st_read("com_police_data.gpkg", layer = "com_violent_crime_2021_22")

layer_voronoi_points <- st_read("com_police_data.gpkg", layer = "com_voronoi_points")

layer_voronoi_polygons <- st_read("com_police_data.gpkg", layer = "com_voronoi_polygons")

layer_env_crime2021 <- st_read("com_police_data.gpkg", layer = "com311_env_crime2021")

layer_env_crime2022 <- st_read("com_police_data.gpkg", layer = "com311_env_crime2022")

layer_mpd_districts <- st_read("com_police_data.gpkg", layer = "mpd_districts")

layer_police_data <- st_read("com_police_data.gpkg", layer = "com_police_data")

layer_layer_styles <- st_read("com_police_data.gpkg", layer = "layer_styles")

layer_aggregated_data <- st_read("com_police_data.gpkg", layer = "com_aggregated_data")

