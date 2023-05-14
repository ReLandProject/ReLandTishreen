
# Description -------------------------------------------------------------


# The aim of the script is to produce an ImageCollection of NDWI Satellite
# images, with one image mosaic per year or one image mosaic per month. 
# The script will also export the resulting ImageCollection, letting the user
# choose whether to export the images on google drive or to a local folder.
# Each image will be named in a machine readable format with the study area,
# the type of image and the date. 
# This script uses the {rgee} package (Aybar et al., (2020) https://doi.org/10.21105/joss.02272)
# please see the instruction to install and use rgee: https://github.com/r spatial/rgee

# In the "Initial Configuration section, the user will find all the options to
# edit, in this way the other sections can be run without any further edits.
# If more in depth changes are needed, be sure to look at the other sections as well.

# Change also the email inside ee_intialize() accordingly.


# All the images are exported following a naming convention: "area_image_type
# YYYY_satellite" or "area_image_type YYYY-MM_satellite" for the monthly
# composites.
# Once the export task is completed, it will take some minutes for the images to show up in GDrive
# The local download option will take longer depending on the image size
# make sure to have some space on Google Drive or the process might not complete

# The function for generating list of dates and the one for acquiring one
# image per year is adapted from
# https://gis.stackexchange.com/questions/269313/mapping-over-list-of-dates-in-google-earth-engine


# Technical notes -----------------------------------------------------------------------

# While there is a convenient Normalized Difference Formula 
# https://developers.google.com/earth-engine/tutorials/tutorial_api_06
# we decided to use the manual calculation, as we saw that the above
# resulted in some nan values in water areas

# Initial Configuration ---------------------------------------------------

library(rgee)
library(reticulate)
# Initialize rgee
ee_Initialize(email = "titoloandrea@gmail.com", drive = TRUE, gcs = FALSE)


# ee_check()
# ee_check_credentials()
# ee_user_info()

# Variable Definition -----------------------------------------------------

# Choose which satellite we want to use
# Select only one at a time.
# satellite <- "L5"
# satellite <- "L7"
satellite <- "L8"
# satellite <- "S2"

# Select exporting CRS for satellite images
img_crs = "EPSG: 32637"

# Select a starting date for the analysis
# Operational years:
  # - L5: 1984-2012
  # - L7: 1999-2022
  # - L8: 2013-present
  # - S2: 2015-present

start  <- ee$Date$fromYMD(2013, 01, 01)

end <- ee$List$sequence(0, 64)

# Choose which method to use for exporting the images (DRIVE, LOCAL)
# see https://r-spatial.github.io/rgee/reference/ee_imagecollection_to_local.html#details
drive <- TRUE
local_drive <- FALSE # Slower, depending on the images.

# Set the desired cloud filter value
# Careful, very low cloud filter values obviously lead to fewer images
cloud_filter_value <- 20

# Choose the name of the folder to store the images exported when drive = TRUE
# Local folder is for when local_drive is set to TRUE
gdrive_folder <- "rgeeRelandTishreen"

# Do you want to limit the collection to a certain amount of images?
# Might be useful for testing purpose
limit <- FALSE
limit_num <- 5

# Display the imageCollection in an interactive map? 
# This may be slower depending on the number of images
display <- TRUE


area <- ee$String("tshr_NDWI_")

# Generate geometries according to the chosen area
# Generate also a string useful in naming the images later

# study_region <- ee$FeatureCollection("projects/ee-titoloandrea/assets/tishreen_study_area")

geometry_tishreen <- ee$Geometry$Polygon(
  coords = list(
    c(37.99069550372825,36.31033474346237),
    c(38.335048225896216,36.31033474346237),
    c(38.335048225896216,36.85151712320516),
    c(37.99069550372825,36.85151712320516)
  )
)


study_region <- geometry_tishreen

# Variable definition according to the chosen satellite
# For each choice we define the ImageCollection, the bands to use to generate the NDWI,
# a string identifying the satellite (useful in the image naming later)
# and start and end dates (unique to each satellite).
# Please refer to https://www.usgs.gov/media/images/landsat-missions-timeline
# for information on periods of activity and decommision of each satellite.
# The start variable set the starting point and the end variable determine how much
# the function needs to advance from the start variable.

if (satellite == "L5") {
     
  BaseColl <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")
  
  # ndwi_bands <- c("B2", "B5")
  green  <- "SR_B2"
  swir <- "SR_B5"
  
  sensor <- ee$String("_L5")

  px_scale <- 30
  
} else if (satellite == "L7") {
  
  BaseColl <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")
  
  # ndwi_bands <- c("B2", "B5")
  green  <- "SR_B2"
  swir <- "SR_B5"
  
  sensor <- ee$String("_L7")

  px_scale <- 30

} else if (satellite == "L8") {
   
  BaseColl <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")
  
  # ndwi_bands <- c("B2", "B11")
  green  <- "SR_B3"
  swir <- "SR_B6"
  
  sensor <- ee$String("_L8")

  px_scale <- 30

} else if (satellite == "S2") {

  BaseColl <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

  # ndwi_bands <- c("B3", "B11")
  green  <- "B3"
  swir <- "B11"

  sensor <- ee$String("_S2")

  px_scale <- 20

}


# Functions Definition -----------------------------------------------------

# Function to generate a list of dates over which we will map the image collection function
# Use ee_utils_pyfunc when mapping over a ee_List object
# See https://r-spatial.github.io/rgee/articles/considerations.html#the-map-message-error
# The monthly image collection require a different variable inside the "advance" function
# thus it is separated from the main one and will run only if monthly_S2 == TRUE

startDates <- end$map(ee_utils_pyfunc(
  function(x) start$advance(x, "month")
))

if(satellite == "L5" || satellite == "L7") {

    cloudfunction <- function(image) {
    # Bit 0 - Fill
    # Bit 2 - Unused
    # Bit 1 - Dilated Cloud
    # Bit 3 - Cloud
    # Bit 4 - Cloud Shadow
    qaMask <- image$select('QA_PIXEL')$bitwiseAnd(ee$Number$parse('11111', 2))$eq(0)
    saturationMask <- image$select('QA_RADSAT')$eq(0)

    # Apply the scaling factors to the appropriate bands.
    opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBand <- image$select('ST_B6')$multiply(0.00341802)$add(149.0)

    # Replace the original bands with the scaled ones and apply the masks.
    return(image$addBands(opticalBands, names = NULL, overwrite = TRUE)$
        addBands(thermalBand, names = NULL, overwrite = TRUE)$
        updateMask(qaMask)$
        updateMask(saturationMask))
    }

  cloud_filter <- ee$Filter$lt("CLOUD_COVER", cloud_filter_value)

} else if (satellite == "L8") {

  cloudfunction <- function(image) {
    # Bit 0 - Fill
    # Bit 1 - Dilated Cloud
    # Bit 2 - Cirrus
    # Bit 3 - Cloud
    # Bit 4 - Cloud Shadow
    qaMask <- image$select('QA_PIXEL')$bitwiseAnd(ee$Number$parse('11111', 2))$eq(0)
    saturationMask <- image$select('QA_RADSAT')$eq(0)

    # Apply the scaling factors to the appropriate bands.
    opticalBands <- image$select('SR_B.')$multiply(0.0000275)$add(-0.2)
    thermalBands <- image$select('ST_B.*')$multiply(0.00341802)$add(149.0)

    # Replace the original bands with the scaled ones and apply the masks.
    return(image$addBands(opticalBands, names = NULL, overwrite = TRUE)$
        addBands(thermalBands, names = NULL, overwrite = TRUE)$
        updateMask(qaMask)$
        updateMask(saturationMask))
  }

  cloud_filter <- ee$Filter$lt("CLOUD_COVER", cloud_filter_value)

} else if (satellite == "S2") {

  # Function to mask clouds from the QA band in Sentinel images BOA
  cloudfunction <- function(image) {
    qa <- image$select("QA60")
    # Bits 10 and 11 are clouds and cirrus, respectively.
    cloudBitMask <- bitwShiftL(1, 10)
    cirrusBitMask <- bitwShiftL(1, 11)

    # Both flags should be set to zero, indicating clear conditions.
    mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$And(
      qa$bitwiseAnd(cirrusBitMask)$eq(0)
    )

    # Return the masked and scaled data, without the QA bands.
    image$updateMask(mask)$
      divide(10000)$
      select("B.*")$
      copyProperties(image, list("system:time_start"))
  }

  cloud_filter <- ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", cloud_filter_value)

}

# Function to get image NDWI
getNDWI = function(img){
  greenBand <- img$select(green);
  swirBand <- img$select(swir);
  NDWI <- greenBand$subtract(swirBand)$divide(greenBand$add(swirBand))$rename('NDWI')
  return(NDWI)
}

# Function to generate and NDWI band using the shortcut function
# see https://developers.google.com/earth-engine/tutorials/tutorial_api_06
# getNDWI <- function(img) {
#   NDWI <- img$normalizedDifference(ndwi_bands)$rename("NDWI")
#   return(NDWI)
# }

# Function to generate NDWI mosaics for each year, steps:
# Get a starting date from the object that we are going to pass to the function
# Get an end range for each step advancing by: 1 year
# Creates a date range, according to the start and end layer
# Create a name property for each image from the variables we set before and the starting year date 
# Select the dataset as ImageCollection
# Filter the collection for images within our date ranges
# Filter the collection for images within our study region
# Filter for the percentage of cloud cover set before
# Map the cloud function chosen before
# Map the NDWI function created before, this will return the NDWI band of each image in the collection
# Clip the images to our study region
# Apply the median reducer to mosaic the images of each year and set the variable name as a property of the images.

# This is a slightly modified version of the above
# Date ranges are within a month interval
# The name property will show the month and nnot only the year

yearmap <- function(m){
  start <- ee$Date(m)
  end <- ee$Date(m)$advance(1,"month")
  date_range <- ee$DateRange(start,end)
  name <- area$cat(start$format("YYYY-MM"))$cat(sensor)
  ImgYear <- BaseColl$
    filterDate(date_range)$
    filterBounds(study_region)$
    filter(cloud_filter)$ # Pre-filter to get less cloudy granules.
    map(cloudfunction)$
    map(getNDWI)$
    map(function(img){return(img$clip(study_region))})
  return(ImgYear$median()$set("name",name))
}


# Processing --------------------------------------------------------------

# Map the function over the list of dates created above
list_of_images <- startDates$map(ee_utils_pyfunc(yearmap))


# Transform the list of images to an ImageCollection to batch export them
ImgColl <- ee$ImageCollection(list_of_images)

# # If needed, get information on the first image
# ee_print(ImgColl$first())


# Visualisation -----------------------------------------------------------


if (display) {
  
  # ndwiParams = list(min = -1, max = 1, palette = c('brown', 'white', 'blue'))
  ndwiParams = list(min = -1, max = 1, palette = c('#a52a2a', '#ffffff', '#0000ff'))
  img_name <- ImgColl$aggregate_array('name')$getInfo()
  
  Map$centerObject(study_region, zoom = 10)
  Map$addLayers(eeObject = ImgColl, visParams = ndwiParams, name = img_name, shown = FALSE)
  
}

# Export ------------------------------------------------------------------

# OPTIONAL: limit the Collection size and print info on its size
if (limit) {
  
  ExpColl <- ImgColl$limit(limit_num)
  cat("Limited Collection Size: ", ExpColl$size()$getInfo())
  
} else {
  
  ExpColl <- ImgColl
  cat("Collection Size: ", ExpColl$size()$getInfo())
  
}

# Export to Drive ---------------------------------------------------------

if (drive) {
  
# Count the number of images in the collection and convert them to a list
# the list will make possible to extract single images from the collection during the loop
  count <- ExpColl$size()$getInfo()
  ExpColl_list <- ExpColl$toList(count)
  
  # Loop to output each image
  for (index in seq_len(count)) {
    image <- ee$Image(ExpColl_list$get(index - 1))
    date <- ee_get_date_img(image)
    name <- ee$String(image$get('name'))$getInfo()
    print(name)
    task <- ee$batch$Export$image$toDrive(
      image,
      name,
      scale = px_scale,
      maxPixels = 1e9,
      folder = gdrive_folder,
      region = study_region,
      crs = img_crs
    )
    task$start()
    # ee_monitoring_test()
  }
  task$status()
}