# Script to calculate the zonal histogram from annual and monthly rasters over a series of Polygon shapefiles

# remotes::install_github("paleolimbot/qgisprocess")
library(qgisprocess)
library(raster)
library(tidyverse)
library(janitor)
library(foreign)
library(magrittr)
library(rgdal)
library(here)
library(sf)
library(sp)
library(fuzzyjoin)


source("./code/R/functions/get_emerged_area.R")


# Filter and prepare data for analysis in QGIS ----------------------------


# Load the data
tshr_polys <- readOGR(here::here("data/raw/shp", "Tishreen_Poly.shp"),
                      stringsAsFactors = FALSE, verbose = FALSE
)

tshr_sites <- readOGR(here::here("data/raw/shp", "Tishreen_Points.shp"),
                      stringsAsFactors = FALSE, verbose = FALSE
)


# Attach certainty fields from points to the polys ------------------------


# # For the geometries to be "joined" correctly we need to first convert to an sf object
# # dplyr::inner_join expects a data frame as y variable (tshr_sites@data)
# # Since more than one point can be included in one polygon, use fuzzy_inner_join
# # from the {fuzzyjoin} package to match partial string
# # distinct remove duplicate rows

# tshr_polys_filtered <- st_as_sf(tshr_polys) %>%
#   # fuzzy_anti_join(., tshr_sites@data[, 2:3], by = c("Name" = "Name"), match_fun = str_detect)
#   fuzzy_inner_join(., tshr_sites@data[, 2:3], by = c("Name" = "Name"), match_fun = str_detect) %>%
#   select(-Name.y) %>%
#   rename(Name = Name.x) %>%
#   distinct(.keep_all = TRUE) # remove duplicate rows (all elements of the row must be identical!)


# # Keep the duplicate with the highest certainty (identified by the function below)
# # Different numbers in the certainty column prevent distinct() to eliminate one of the rows
# # Use the | operator to combine the results from the two functions
# dupl <- tshr_polys_filtered[which(duplicated(tshr_polys_filtered$Name) | duplicated(tshr_polys_filtered$Name, fromLast = TRUE)), ]
# dupl

# # Retrieve the row index of the point with lowest certainty and use it later to remove it
# index <- c(102, 247, 258) # as.integer(rownames(dupl[which(dupl$Certainty == 2), ]))

# # Transform the dataframe into SpatialPolygonDataFrame
# tshr_polys_filtered <- tshr_polys_filtered[-index, ] %>%
#   mutate(Name = str_replace(Name, "T. Rownak_exc", "T. Rownak (exc.)")) %>%  # Reformat to the original name
#   st_as_sf() %>%
#   as(., "Spatial")

# # # Replace T. Rownak (exc) with T. Rownak_exc otherwise some join function do not detect it
# # tshr_polys@data$Name[grep("T. Rownak .*", tshr_polys@data$Name)] <- "T. Rownak_exc"

# # Turn all the column names to lowercase for easier referencing
# colnames(tshr_polys_filtered@data) <- tolower(colnames(tshr_polys_filtered@data))

# # Keep only the selected fields and remove the id column
# tshr_polys_filtered@data <- subset(tshr_polys_filtered@data, select = -id)


# List the reclassified rasters
r_raster_files <- list.files(here::here("data/processed/raster/time_series_1999_2023_reclassified/"),
                             pattern = "*.tif$",
                             recursive = TRUE, full.names = FALSE
)


# # Get details on the qgisprocess path
qgis_configure()

# Get information on the desired algorithm
# note how the algorithm takes the PATH to a raster file as raster input 
qgis_show_help("native:zonalhistogram")


# Create an R function from the qgis function (see https://github.com/paleolimbot/qgisprocess/issues/15)
zonal_histogram <- qgis_function("native:zonalhistogram")



# Apply the function to generate new shapefiles with the zonal histogram algorithm

get_zonal_histo(
  polys = tshr_polys,
  raster_files = r_raster_files,
  raster_path = here::here("data/processed/raster/time_series_1999_2023_reclassified/"),
  out_path = here::here("output/shp/zonal_histogram_output")
)

#  Load output data -----------------------------------------------------------------------


site_type <- tshr_sites@data[,c(3:4)]

# Merge results from QGIS and compute percentage of emerged site area --------


poly_names_min <- list.files(here::here("output/shp/zonal_histogram_output"),
                             pattern = "*min.dbf",
                             ignore.case = FALSE
)

poly_names_max <- list.files(here::here("output/shp/zonal_histogram_output"),
                             pattern = "*max.dbf",
                             ignore.case = FALSE
)


# read.dbf is from the "foreign" package
polys_year_list_min <- lapply(poly_names_min, function(fn) {
  read.dbf(here::here("output/shp/zonal_histogram_output", fn), as.is = TRUE)
})


polys_year_list_max <- lapply(poly_names_max, function(fn) {
  read.dbf(here::here("output/shp/zonal_histogram_output", fn), as.is = TRUE)
})


# Data manipulation -----------------------------------------------------------------------


# Define fields that will not be subject to percentage conversion
# This will be all the fields except those added by the zonal histogram
fields_to_not_adorn <- c("fid", "Shape_Leng", "Shape_Area", "Name", "Certainty", "Note")

# Define numeric fields with site measurements
# This will avoid errors when transforming percentage fields in integer
# the mutate() function will not touch these fields
measurement_fields <- c("Shape_Leng", "Shape_Area")

# Apply the function to get a data frame with columns containing zonal percentages of all the loaded dbf
# This represent the percentage of site area emerged and submerged each year
# Apply the function on the datasets and replace the original shp dataframe
# to have GIS-ready shapefile to export.
#  Min Water Level
tshr_polys@data <- get_zonal_pct(polys_year_list_min)

tshr_polys@data <- tshr_polys@data %>% 
  full_join(., select(site_type, c("Name", "Type")), by = "Name") %>% 
  relocate("Type", .after = "Shape_Area") %>%
  as.data.frame()

# Export the results

writeOGR(
  obj = tshr_polys,
  dsn = here::here("output/shp", "tshr_polys_area_pct_min.shp"),
  layer = "tshr_polys_area_pct_min",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys@data, here::here("output/csv", "tshr_polys_area_pct_min.csv"))



#  Max Water level
tshr_polys@data <- get_zonal_pct(polys_year_list_max)

tshr_polys@data <-tshr_polys@data %>% 
  full_join(., select(site_type, c("Name", "Type")), by = "Name") %>% 
  relocate("Type", .after = "Shape_Area") %>%
  as.data.frame()

# Export the results

writeOGR(
  obj = tshr_polys,
  dsn = here::here("output/shp", "tshr_polys_area_pct_max.shp"),
  layer = "tshr_polys_area_pct_max",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys@data, here::here("output/csv", "tshr_polys_area_pct_max.csv"))

# column_names <- colnames(tshr_polys@data[1:7]) # save it for later
