library(dplyr)
library(magrittr)
library(here)
library(sf)

source("./code/R/functions/get_emerged_area.R")

# Get quantitative info about emerged area --------------------------------

# Function to use with across in place of filter_at + any_vars
# # Find all rows where ANY numeric variable is greater than zero
# taken from vignette("colwise")
rowAny <- function(x) rowSums(x, na.rm = TRUE) > 0

tshr_polys_pct_min <- st_read(here::here("output/shp", "tshr_polys_area_pct_min.shp"),
  stringsAsFactors = FALSE
)

tshr_polys_pct_max <- st_read(here::here("output/shp", "tshr_polys_area_pct_max.shp"),
  stringsAsFactors = FALSE
)

# Load also the points to export a point layer later
tshr_sites <- st_read(here::here("data/raw/shp", "Tishreen_Points.shp"),
  stringsAsFactors = FALSE
)

# Apply the function after converting to simple feature, then convert it back to spatial object

# Select which field you want to keep in the new spatial object
fields_to_select <- c("Shape_Leng", "Shape_Area", "Type", "Name", "Certainty", "Note")


# Define a field where the numeric area is stored
# it will be rounded inside the function
area_field <- c("Shape_Area")

# Calculate the starting column for the calculations
# The first percentage field will start after (+1) the fields we selected before
starting_col <- length(fields_to_select) + 1

tshr_polys_pct_info_min <- tshr_polys_pct_min %>%
  get_emersion_data(column_index = starting_col) %>%
  mutate(across(where(is.numeric), as.integer))

tshr_polys_pct_info_min <- tshr_polys_pct_info_min[order(tshr_polys_pct_info_min$Name), ] %>%
  as(., "Spatial")

tshr_polys_pct_info_min <- tshr_polys_pct_info_min %>%
  set_colnames(sub("X", "", colnames(.), fixed = TRUE))


tshr_polys_pct_info_max <- tshr_polys_pct_max %>%
  get_emersion_data(column_index = starting_col) %>%
  mutate(across(where(is.numeric), as.integer))

tshr_polys_pct_info_max <- tshr_polys_pct_info_max[order(tshr_polys_pct_info_max$Name), ] %>%
  as(., "Spatial")

tshr_polys_pct_info_max <- tshr_polys_pct_info_max %>%
  set_colnames(sub("X", "", colnames(.), fixed = TRUE))


# Evaluate whether the three operations below should go inside the function -----------------------------------------------------------------------
# Get info on NeverEm and AlwaysSub sites
# Isolate sites which are not touched by the lake in periods of max water
# and those that are not outside the water in periods of min water
names_sites <- tshr_polys_pct_info_max[which(tshr_polys_pct_info_max$AlwaysEm == 1), ] %>%
  bind_rows(tshr_polys_pct_info_min[which(tshr_polys_pct_info_min$AlwaysSub == 1), ]) %>%
  select(starts_with("Name"))

tshr_polys_pct_info_max <- tshr_polys_pct_info_max %>%
  mutate(NeverSub = ifelse(AlwaysEm == 1 & tshr_polys_pct_info_max$Name %in% names_sites$Name, 1, 0)) %>%
  mutate(NeverEm = ifelse(AlwaysSub == 1 & tshr_polys_pct_info_max$Name %in% names_sites$Name, 1, 0)) %>%
  mutate(across(starts_with("Never"), as.integer))

tshr_polys_pct_info_min <- tshr_polys_pct_info_min %>%
  mutate(NeverSub = ifelse(AlwaysEm == 1 & tshr_polys_pct_info_min$Name %in% names_sites$Name, 1, 0)) %>%
  mutate(NeverEm = ifelse(AlwaysSub == 1 & tshr_polys_pct_info_min$Name %in% names_sites$Name, 1, 0)) %>%
  mutate(across(starts_with("Never"), as.integer))

# Get some insights into the data just created
# Note: Fields AlwaysEm and AlwaysSub are relative to the water level period
# Fields NeverSub and NeverEm are independent of the water level
# This means that NeverSub and NeverEm should be the same in both layers
# For the same reason, AlwaysEm and NeverSub will be the same in max water level
# And AlwaysSub and NeverEm will be the same in min water level

# First let's check if NeverSub and NeverEm are  the same in min and max water level
length(which(tshr_polys_pct_info_min$NeverSub == 1)) == length(which(tshr_polys_pct_info_max$NeverSub == 1))
length(which(tshr_polys_pct_info_min$NeverEm == 1)) == length(which(tshr_polys_pct_info_max$NeverEm == 1))

# Print a table in the console
knitr::kable(data.frame(
  "Always Submerged at h.w.l." = length(which(tshr_polys_pct_info_max$AlwaysSub == 1)),
  "Always Emerged at h.w.l." = length(which(tshr_polys_pct_info_max$AlwaysEm == 1)),
  "Always Submerged at l.w.l." = length(which(tshr_polys_pct_info_min$AlwaysSub == 1)),
  "Always Emerged at l.w.l." = length(which(tshr_polys_pct_info_min$AlwaysEm == 1)),
  "Never Emerged" = length(which(tshr_polys_pct_info_min$NeverEm == 1)),
  "Never Submerged" = length(which(tshr_polys_pct_info_min$NeverSub == 1)),
  "Affected in h.w.l." = length(which(tshr_polys_pct_info_max$Affected == 1)),
  "Affected in l.w.l." = length(which(tshr_polys_pct_info_min$Affected == 1))
))

# Export data

writeOGR(
  obj = tshr_polys_pct_info_min,
  dsn = here::here("output/shp", "tshr_polys_area_pct_info_min.shp"),
  layer = "tshr_polys_area_pct_min",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys_pct_info_min, here::here("output/csv", "tshr_polys_area_pct_info_min.csv"), row.names = FALSE)

writeOGR(
  obj = tshr_polys_pct_info_max,
  dsn = here::here("output/shp", "tshr_polys_area_pct_info_max.shp"),
  layer = "tshr_polys_area_pct_max",
  driver = "ESRI Shapefile", overwrite_layer = TRUE
)

write.csv(tshr_polys_pct_info_max, here::here("output/csv", "tshr_polys_area_pct_info_max.csv"), row.names = FALSE)


# Obtain point layers and export them instead of creating centroids later

tshr_sites <- tshr_sites[order(tshr_sites$Name), ]

tshr_points_pct_info_min <- tshr_sites
tshr_points_pct_info_max <- tshr_sites

tshr_points_pct_info_min <- tshr_polys_pct_info_min
tshr_points_pct_info_max <- tshr_polys_pct_info_max

st_write(
  obj = tshr_points_pct_info_min,
  dsn = here::here("output/shp", "tshr_points_pct_info_min.shp"),
  layer = "tshr_points_pct_info_min",
  driver = "ESRI Shapefile",
  append = FALSE
)

st_write(
  obj = tshr_points_pct_info_max,
  dsn = here::here("output/shp", "tshr_points_pct_info_max.shp"),
  layer = "tshr_points_pct_info_max",
  driver = "ESRI Shapefile",
  append = FALSE
)