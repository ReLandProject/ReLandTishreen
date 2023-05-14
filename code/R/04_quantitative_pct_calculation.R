# remotes::install_github("paleolimbot/qgisprocess")
# library(qgisprocess)
# library(raster)
library(tidyverse)
# library(janitor)
# library(foreign)
library(magrittr)
# library(rgdal)
library(here)
library(sf)
library(sp)
# library(fuzzyjoin)

source("./code/R/functions/get_emerged_area.R")

# Get quantitative info about emerged area --------------------------------

# Function to use with across in place of filter_at + any_vars
# # Find all rows where ANY numeric variable is greater than zero
# taken from vignette("colwise")
rowAny <- function(x) rowSums(x, na.rm = TRUE) > 0

tshr_polys_pct_min <- readOGR(here::here("output/shp", "tshr_polys_area_pct_min.shp"),
                              stringsAsFactors = FALSE, verbose = TRUE
)

tshr_polys_pct_max <- readOGR(here::here("output/shp", "tshr_polys_area_pct_max.shp"),
                              stringsAsFactors = FALSE, verbose = TRUE
)

# Apply the function after converting to simple feature, then convert it back to spatial object

# Select which field you want to keep in the new spatial object
fields_to_select <- c("Shape_Leng", "Shape_Area", "Type", "Name", "Certainty", "Note")


# Define a field where the numeric area is stored
# it will be rounded inside the function
area_field <- c("Shape_Area")

# Calculate the starting column for the calculations
# The first percentage field will start after (+1) the fields we selected before
starting_col <- length(fields_to_select) +1

tshr_polys_pct_info_min <- st_as_sf(tshr_polys_pct_min) %>%
  get_emersion_data(column_index = starting_col) %>%
  as(., "Spatial")


tshr_polys_pct_info_min@data <- tshr_polys_pct_info_min@data %>% 
  mutate(across(where(is.numeric), as.integer)) %>% 
  set_colnames(sub("X", "", colnames(.), fixed = TRUE))


tshr_polys_pct_info_max <- st_as_sf(tshr_polys_pct_max) %>%
  get_emersion_data(column_index = starting_col) %>%
  as(., "Spatial")

tshr_polys_pct_info_max@data <- tshr_polys_pct_info_max@data %>% 
  mutate(across(where(is.numeric), as.integer)) %>% 
  set_colnames(sub("X", "", colnames(.), fixed = TRUE))


# Get info on never aff and always sub sites
# Isolate sites which are not touched by the lake in periods of max water
# and those that are not outside of the water in periods of min water
names_sites <- tshr_polys_pct_info_max@data[which(tshr_polys_pct_info_max@data$NeverAff == 1),] %>% 
  bind_rows(tshr_polys_pct_info_min@data[which(tshr_polys_pct_info_min$ComplSub == 1),]) %>% 
  select(starts_with("Name"))

tshr_polys_pct_info_max@data  <- tshr_polys_pct_info_max@data %>% 
  mutate(NeverSub = ifelse(NeverAff == 1 & tshr_polys_pct_info_max@data$Name %in% names_sites$Name, 1, 0)) %>% 
  mutate(NeverEm = ifelse(ComplSub == 1 & tshr_polys_pct_info_max@data$Name %in% names_sites$Name, 1, 0))

tshr_polys_pct_info_min@data  <- tshr_polys_pct_info_min@data %>% 
  mutate(NeverSub = ifelse(NeverAff == 1 & tshr_polys_pct_info_min@data$Name %in% names_sites$Name, 1, 0)) %>% 
  mutate(NeverEm = ifelse(ComplSub == 1 & tshr_polys_pct_info_min@data$Name %in% names_sites$Name, 1, 0))

# Export data

writeOGR(
  obj = tshr_polys_pct_info_min,
  dsn = here::here("output/shp", "tshr_polys_area_pct_info_min.shp"),
  layer = "tshr_polys_area_pct_min",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys_pct_info_min@data, here::here("output/csv", "tshr_polys_area_pct_info_min.csv"))

writeOGR(
  obj = tshr_polys_pct_info_max,
  dsn = here::here("output/shp", "tshr_polys_area_pct_info_max.shp"),
  layer = "tshr_polys_area_pct_max",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys_pct_info_max@data, here::here("output/csv", "tshr_polys_area_pct_info_max.csv"))






# sites_pct_area_min <- openxlsx::read.xlsx(here::here("output/csv", "tshr_polys_area_pct_info_min.xlsx")) 

# sites_pct_area_max <- openxlsx::read.xlsx(here::here("output/csv", "tshr_polys_area_pct_info_max.xlsx"))

# sites_pct_area_min_never <- sites_pct_area_min[which(sites_pct_area_min$NeverAff==1),]

# sites_pct_area_max_never <- sites_pct_area_max[which(sites_pct_area_max$NeverAff==1),]

# never_ever_aff <- sites_pct_area_max_never %>%
#   fuzzyjoin::fuzzy_anti_join(.,sites_pct_area_min_never, by = c("name" = "name"), match_fun = str_detect)
