// Import the geometry 

// var geometry_tishreen = /* color: #0B4A8B */ee.Geometry.Polygon(
//     [[[37.99069550372825, 36.31033474346237],
//       [38.335048225896216, 36.31033474346237],
//       [38.335048225896216, 36.85151712320516],
//       [37.99069550372825, 36.85151712320516]]], null, false);

//Define functions and variables to use later

// STUDY REGIONS, remove the "//" from the one you need to use and put the "//" to those that are not needed.

var study_region = geometry_tishreen;
Map.centerObject(study_region, 9); //center and zoom to the kurgan area


// Define parameters for the NDWI visualisation in Google Earth Engine
var ndwiParams = { min: -1, max: 1, palette: ['brown', 'white', 'blue'] };

// This is not really necessary, but it might ease the identification of images if exported to a single folder
// it will be used to generate a more consistent image name

var area = ee.String('tshr_NDWI_');

var sensor = ee.String("_S2");


var exp_folder = 'rgeeRelandTishreen'; // Here you can set the name of the new folder to be created in GDrive
var px_scale = 20;


// SELECT THE IMAGES TO USE
var BaseColl = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED');         // Sentinel 2, available


// CREATE START AND END LAYER
var start = ee.Date.fromYMD(2018,1,1);    // Start date for Sentinel 2
var months = ee.List.sequence(0, 11);     // Number of years for Sentinel2



// ----- Landsat and Sentinel Specific Functions -----

// Cloud masking function for Sentinel SR images
var maskS2clouds_sr = function (image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask).divide(10000);
}


var cloudfunction = maskS2clouds_sr;

// set the cloud coverage percentage
var cloud_filter = ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 0.1); // Set the maximum percentage of cloud cover by tweaking this number
                                                                 // Careful, lower percentages might result in missing images when using it with a short time-period filter



// ----- Processing: Landsat monthly images -----


// Function to get image NDWI
var getNDWI = function(img){
  var green = img.select('B3');
  var swir = img.select('B11');
  var NDWI = green.subtract(swir).divide(green.add(swir)).rename('NDWI');
  return NDWI;
};
// ----- Processing: Monthly S2 Images -----


// Make start and end layers
var S2Monthstart = ee.Date.fromYMD(2022,01,1);
var S2months = ee.List.sequence(0, 11);
var S2MonthstartDates = S2months.map(function(d) {
  return S2Monthstart.advance(d, 'month');
});
print("Sentinel2 Month Start dates",S2MonthstartDates);

// Collect imagery by month
var S2monthmap = function(m){
  var start = ee.Date(m);
  var end = ee.Date(m).advance(1,'month');
  var date_range = ee.DateRange(start,end);
  var name = area.cat(start.format('YYYY-MM')).cat(sensor);
  var ImgMonth = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
    .filterDate(date_range)
    .filterBounds(study_region)
    .filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 20))
    .map(maskS2clouds_sr)
    .map(getNDWI)
    .map(function(img){return img.clip(study_region)});
  return(ImgMonth.median().set({name: name}));
};
print("S2monthmap",S2monthmap);

var list_of_S2monthly_images = S2MonthstartDates.map(S2monthmap);
print('list_of_S2monthly_images', list_of_S2monthly_images);
var S2MonthlyColl = ee.ImageCollection(list_of_S2monthly_images);
print("Monthly NDWI", S2MonthlyColl);


/// ----- Images Visualisation -----

//Map.addLayer(ImgColl,ndwiParams,"NDWI"); // this add the last image of the collection (the more recent)
//select the number of images to display, set to 1 to display all of them (the value should not exceed the total generated images for each collection)

var images_to_not_display = 1; //this will remove n images from the collection and show all the others


var imageSetCollection = S2MonthlyColl.toList(S2MonthlyColl.size());

print(imageSetCollection);

var test= ee.List.sequence(0,ee.Number(S2MonthlyColl.size().subtract(images_to_not_display))).getInfo();
var fun = function(img){

  var image = ee.Image(imageSetCollection.get(img));
  var names = ee.String(image.get('name')).getInfo();
  var label = img + '_' +names;

  Map.addLayer(image,ndwiParams, label, false);
};

test.map(fun); //this is disabled by default, as it can slow down the process.
Map.centerObject(study_region, 9); // We center the map on our study area with a set zoom

// ----- Export all the Images to Google Drive -----
// Since there is no native way to export all the images in a Collection, we need to import an external module to do that
// The module was developed by Rodrigo E. Principe and is available at: https://github.com/fitoprincipe/geetools-code-editor
// This tool will create a folder in your Google Drive, where all the images in the collection will be exported.

var batch = require('users/fitoprincipe/geetools:batch'); // Here we load the module
batch.Download.ImageCollection.toDrive(S2MonthlyColl, exp_folder, {
  name: '{name}',
  scale: px_scale,
  region: study_region,
  type: 'float',
  crs: 'EPSG: 32637'
});
