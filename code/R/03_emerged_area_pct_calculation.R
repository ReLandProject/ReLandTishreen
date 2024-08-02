# Script to calculate the zonal histogram from annual and monthly rasters over a series of Polygon shapefiles

library(qgisprocess)
library(raster)
library(dplyr)
library(janitor)
library(foreign)
library(magrittr)
library(here)
library(sf)
library(fuzzyjoin)

source("./code/R/functions/get_emerged_area.R")

# Filter and prepare data for analysis in QGIS ----------------------------

# Load the data
tshr_polys <- st_read(here::here("data/raw/shp", "Tishreen_Poly.shp"),
  stringsAsFactors = FALSE
)

tshr_sites <- st_read(here::here("data/raw/shp", "Tishreen_Points.shp"),
  stringsAsFactors = FALSE
)

# Load Rasters -----------------------------------------------------------------------
# List the reclassified rasters

r_raster_files <- list.files(here::here("data/processed/raster/time_series_1999_2023_reclassified/"),
  pattern = "*.tif$",
  recursive = TRUE, full.names = FALSE
)

# Get details on the qgisprocess path
qgis_configure()
# Use the function below if qgis_configure() was already run once on your system
# qgis_configure(use_cached_data = TRUE)

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

site_type <- tshr_sites[, c(3:4)]

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
tshr_polys <- get_zonal_pct(polys_year_list_min) %>% st_set_geometry(., st_geometry(tshr_polys))

tshr_polys <- tshr_polys %>%
  full_join(., select(site_type, c("Name", "Type")), by = "Name") %>%
  relocate("Type", .after = "Shape_Area") %>%
  as.data.frame()

# Export data -----------------------------------------------------------------------

st_write(
  obj = tshr_polys,
  dsn = here::here("output/shp", "tshr_polys_area_pct_min.shp"),
  layer = "tshr_polys_area_pct_min",
  driver = "ESRI Shapefile", overwrite_layer = TRUE,
  append = FALSE
)

write.csv(tshr_polys, here::here("output/csv", "tshr_polys_area_pct_min.csv"), row.names = FALSE)


#  Max Water level
tshr_polys <- get_zonal_pct(polys_year_list_max)

tshr_polys <- tshr_polys %>%
  full_join(., select(site_type, c("Name", "Type")), by = "Name") %>%
  relocate("Type", .after = "Shape_Area") %>%
  as.data.frame()

# Export the results

st_write(
  obj = tshr_polys,
  dsn = here::here("output/shp", "tshr_polys_area_pct_max.shp"),
  layer = "tshr_polys_area_pct_max",
  driver = "ESRI Shapefile", overwrite_layer = TRUE,
  append = FALSE
)

write.csv(tshr_polys, here::here("output/csv", "tshr_polys_area_pct_max.csv"), row.names = FALSE)