# THESE FUNCTIONS ARE SUPPOSED TO BE USED WITH THE 02_zonal_histogram.R SCRIPT.

# batch_reclass_and_save function -----------------------------------------

# This function was adapted from
# https://stackoverflow.com/questions/52750561/how-to-reclassify-a-batch-of-rasters-in-r
# It reads raster files from a supplied directory and reclassify them, then save them to a local folder
# Arguments:
# raslist is a is a character vector containing file names of the rasters that needs to be reclassified.
# raster_path s a character vector indicating the folder in which the raster files
# are stored, accepts relative paths.
# rclmat is a reclassification matrix for the reclassify function to work
# outpath is a character vector indicating the target folder where to save
# the reclassified rasters, accepts relative paths.

batch_reclass_and_save <- function(raslist, raster_path, rclmat, outpath) {
  for (i in 1:length(raslist)) {
    # read in raster
    r <- raster(file.path(raster_path, raslist[i]))

    # perform the reclassifcation
    rc <- reclassify(r, rclmat)

    # values outside 0 and 1 will be NA
    rc_clamped <- clamp(rc, lower = 0, upper = 1, useValues = FALSE)

    # w rite each reclass to a new file
    writeRaster(rc_clamped, filename = paste0(
      outpath, "r_",
      raslist[i]
    ), format = "GTiff", overwrite = TRUE)

    print(rc_clamped)
  }

  cat("====== DONE ======")
}


# get_zonal_histogram function --------------------------------------------

# This function calculates zonal histogram algorithm from monthly and annual rasters
# The aim of this function is to loop through a list of rasters and apply the zonal histogram algorithm
# The zonal histogram is a QGIS algorithm accesed through the {qgisprocess} package

# Arguments:
# polys is a polygon shapefile (class SpatialPolygonsDataframe) containing one ore more polygons
# raster_files is a character vector containing file names of reclassified rasters
# raster_path is a character vector indicating the folder in which the raster files
# are stored, it accepts relative paths.
# area_filter is a character vector indicating the abbreviation for the study area
# to which the rasters and the polygons pertain.
# out_path is a character vector indicating the target folder where to save
# the reclassified rasters, accepts relative paths.

# The function returns a series of shapefiles, one for each raster used,
# containing the count of unique pixel values inside one or more polygons.

get_zonal_histo <- function(polys, raster_files, raster_path, out_path) {
  # character vector for programmatically naming later
  area <- substring(raster_files[[1]], first = 3, last = 6)

  # obtain the dates from the file names
  dates <- paste0(substring(raster_files, first = 13, last = 19))

  # obtain longer names for the output filename (shpo has 10 digit limit)
  date_period <- paste0(substring(raster_files, first = 13, last = 23))

  # convert the SpatialPolygonDataFrame to an sf object for qgisprocess to work properly
  polys_sf <- sf::st_as_sf(polys)
  shp_list <- list() # empty list where to store the outcome of the loop

  for (i in 1:length(raster_files)) {
    ras_files <- list.files(raster_path,
      pattern = "*tif$",
      full.names = TRUE, recursive = FALSE
    ) # INPUT_RASTER needs the entire path to the files
    shp_list[[i]] <- zonal_histogram(
      INPUT_RASTER = ras_files[[i]],
      RASTER_BAND = 1,
      INPUT_VECTOR = polys_sf,
      COLUMN_PREFIX = paste0(dates[i], "_"),
      OUTPUT = file.path(out_path, paste0(area, "_", date_period[[i]], ".shp"))
    )
    print(shp_list[[i]])
  }

  cat("====== DONE ======")
}

# get_zonal_pct function --------------------------------------------------

# This function is aimed at manipulating data resulted from the zonal histogram

# The aim is to merge all the files  from the zonal histogram in a long, format
# This function uses some different packages to achieve its result,
# the needed libraries are loaded in the main script (janitor, dplyr, magrittr).
# The function takes one argument, which has to be a list of dataframes
# The first four pipes convert the numerical fields of all the data frame in the
# list into percentages and then it merge them column-wise.
# The filter with "select()" remove duplicate fields, e.g.  FID and NODATA
# The last two pipes are meant to rename column names in a more readable way,
# but still within the limit of the shapefile fields length.

# .name_repair = minimal to ensure columns are not renamed in a unique way
# adorn_rounding() automatically applies on numerical fields only

get_zonal_pct <- function(x) {
  a <- x %>%
    adorn_percentages(... = -starts_with(fields_to_not_adorn)) %>% # make sure to have one column only to skip! all columns after the third are counted for the pct
    bind_cols(.name_repair = "minimal") %>%
    .[!duplicated(as.list(.))] %>% # Remove duplicated columns
    mutate(across(where(is.numeric) & -starts_with(fields_to_not_adorn), ~ . * 100)) %>%
    adorn_rounding(digits = 0, ... = -starts_with(fields_to_not_adorn)) %>%
    mutate(across(where(is.numeric) & -starts_with(measurement_fields), as.integer)) %>%
    set_colnames(gsub(
      x = sub("X", "", colnames(.), fixed = TRUE),
      pattern = "*_0$", replacement = "_em"
    )) %>%
    set_colnames(gsub(
      x = colnames(.),
      pattern = "*_1$", replacement = "_sb"
    )) %>%
    set_colnames(gsub(
      x = colnames(.),
      pattern = "*_NO", replacement = "_NA"
    )) %>%
    as.data.frame()


  print(a)
}



# get_emersion_data function ----------------------------------------------

# Function to get information about emerged area polygons
# It takes a data frame as only input and it outputs a new dataframe with 7 new columns
# The first part remove fields with submerged info and round the area to two decimals
# The first two new columns are Polygons that have always been out of the water and
# polygons never out of water (in the observed time frame)
# The other 5 fields created are polygons that emerged for more than the amount indicated
# at least once during the observed timespan
# To avoid repetitions, new fields are calculated excluding those created before

get_emersion_data <- function(a, column_index) {
  a %>%
    select(all_of(fields_to_select), ends_with("_em")) %>% # Select only the columns with emerged percentage
    mutate(across(starts_with(area_field), ~ round(.x, 2))) %>% # Round the area to two decimals
    mutate(across(where(is.integer), as.numeric)) -> aa # Rowsums needs numeric fields

  if (is(aa, "sf")) { # check wether we need to account for the geometry column added by the sf format
    tot_cols <- length(aa) - 1 # -1 to account for the geometry column that can't be easily removed
    AlwaysEm_threshold <- (ncol(select(aa, ends_with("_em"))) - 1) * 100 # The threshold for calculating the AlwaysEm field is equal to the number of years multiplied by 100 (the pct of emerged area)
  } else {
    tot_cols <- length(aa) # -1 to account for the geometry column that can't be easily removed
    AlwaysEm_threshold <- (ncol(select(aa, ends_with("_em")))) * 100 # The threshold for calculating the AlwaysEm field is equal to the number of years multiplied by 100 (the pct of emerged area)
  }

  aa$AlwaysEm <- ifelse(rowSums(aa[, column_index:tot_cols, drop = TRUE], na.rm = TRUE) >= AlwaysEm_threshold, 1, 0)
  aa$AlwaysSub <- ifelse(aa$AlwaysEm == 0 & rowSums(aa[, column_index:tot_cols, drop = TRUE], na.rm = TRUE) == 0, 1, 0)
  aa$Affected <- ifelse(aa$AlwaysEm == 1 | aa$AlwaysSub == 1, 0, 1)

  # aa <- aa %>%
  #   mutate(Area100 = ifelse(AlwaysEm == 0 & rowAny(across(column_index:all_of(tot_cols), ~ .x == 100)), 1, 0)) %>%
  #   mutate(Area75 = ifelse(AlwaysEm == 0 & rowAny(across(column_index:all_of(tot_cols), ~ .x >= 75)), 1, 0)) %>%
  #   mutate(Area50 = ifelse(AlwaysEm == 0 & rowAny(across(column_index:all_of(tot_cols), ~ .x >= 50)), 1, 0)) %>%
  #   mutate(Area25 = ifelse(AlwaysEm == 0 & rowAny(across(column_index:all_of(tot_cols), ~ .x >= 25)), 1, 0)) %>%
  #   mutate(AreaLt25 = ifelse(AlwaysEm == 0 & ComplSub == 0 & Area100 == 0 &
  #     Area75 == 0 & Area50 == 0 & Area25 == 0, 1, 0))

  print(aa)
}
