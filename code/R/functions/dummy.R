# Initial Configuration ---------------------------------------------------

library(rgee)
# Initialize rgee
ee_Initialize(email = "myemail@gmail.com", drive = TRUE, gcs = FALSE)

study_region <- ee$Geometry$Polygon(
  coords = list(
    c(37.99069550372825,36.31033474346237),
    c(38.335048225896216,36.31033474346237),
    c(38.335048225896216,36.85151712320516),
    c(37.99069550372825,36.85151712320516)
  )
)

# Functions and variables Definition -----------------------------------------------------

area <- ee$String("tsh_NDWI_")

ndwi_bands <- c("B3", "B11")

sensor <- ee$String("_S2")

start <- ee$Date$fromYMD(2021, 01, 01)
end <- ee$List$sequence(0, 11)


# Function to generate a list of dates over which we will map the image collection function
# Use ee_utils_pyfunc when mapping over a ee_List object

startDates <- end$map(ee_utils_pyfunc(
  function(x) start$advance(x, "month")
))

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

# Function to generate and NDWI band using the shortcut function
getNDWI <- function(img) {
  NDWI <- img$normalizedDifference(ndwi_bands)$rename("NDWI")
  return(NDWI)
}


monthmap <- function(m){
  start <- ee$Date(m)
  end <- ee$Date(m)$advance(1,"month")
  date_range <- ee$DateRange(start,end)
  name <- area$cat(start$format("YYYY-MM"))$cat(sensor)
  ImgYear <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterDate(date_range)$
    filterBounds(study_region)$
    filter( ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))$ # Pre-filter to get less cloudy granules.
    map(cloudfunction)$
    map(getNDWI)$
    map(function(img){return(img$clip(study_region))})
  return(ImgYear$median()$set("name",name))
}

# Processing --------------------------------------------------------------

# Map the function over the list of dates created above
list_of_images <- startDates$map(ee_utils_pyfunc(monthmap))

# Transform the list of images to an ImageCollection to batch export them
ImgColl <- ee$ImageCollection(list_of_images)


# Export ------------------------------------------------------------------

# Collection of 12 images
cat("Collection Size: ", ImgColl$size()$getInfo())

## Filter selected months -----------------------------------------------------------------------
filterSelected <- ee$Filter$Or(
    ee$Filter$date('2021-01-01', '2021-01-31'),
    ee$Filter$date('2021-04-01', '2021-04-30'),
    ee$Filter$date('2022-06-01', '2022-06-30'),
    ee$Filter$date('2021-12-01', '2021-12-31')
)

Test <- ImgColl$filter(filterSelected)

# Empty collection
cat("Collection Size: ", Test$size()$getInfo())





# Filter selected months -----------------------------------------------------------------------
filterSelected = ee$Filter$Or(
    FilterDate('2021-04-01', '2021-04-28'),
    ee$Filter$date('2022-06-01', '2022-06-30'),
    ee$Filter$date('2021-12-01', '2021-12-31'),        
    ee$Filter$date('2022-12-01', '2022-12-31')                
)

ExpCollFiltered <- ExpColl$
    FilterOr(
    filter(ee$Filter$eq('name', 'ReL_NDWI_2023-01_S2')),
    filter(ee$Filter$eq('name', 'ReL_NDWI_2023-03_S2'))
    )

ExpCollFiltered <- ExpColl$
  filterSelected