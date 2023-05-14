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

// Define bands to use in the NDWI function.

var ndwi_bands = ['B2', 'B5']; // L5band
// ndwi_bandsvar = ['B2', 'B5']; // L7
// ndwi_bandsvar = ['B3', 'B6']; // L8
// ndwi_bands = ['B3', 'B11']; // S2

//var ndwi_bands = L7bands;

// This is not really necessary, but it might ease the identification of images if exported to a single folder
// it will be used to generate a more consistent image name

var area = ee.String('tshr_NDWI_');

var sensor = ee.String("_L5");
//var sensor = ee.String("_L7");
//var sensor = ee.String("_L8");


var exp_folder = 'rgeeRelandTishreen'; // Here you can set the name of the new folder to be created in GDrive
var px_scale = 30; // Set the pixel scale, REMEMBER to set it to 30 for Landsat images


// SELECT THE IMAGES TO USE
var BaseColl = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR'); // Landsat 5, available
//var BaseColl = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR'); // Landsat 7, available
//var BaseColl = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR'); // Landsat 8, available

//var rgb_bands = ['B3', 'B2', 'B1']
// var rgb_bands = ['B4', 'B3', 'B2']
// CREATE START AND END LAYER

var start = ee.Date.fromYMD(1999,1,1); // Start date for Landsat 5
//var start = ee.Date.fromYMD(1999,1,1); // Start date for Landsat7
//var start = ee.Date.fromYMD(2013,1,1); // Start date for Landsat 8


var months = ee.List.sequence(0, 11);  // Number of months


// ----- Landsat and Sentinel Specific Functions -----

var maskL8sr = function (image) {
  // Bits 3 and 5 are cloud shadow and cloud, respectively.
  var cloudShadowBitMask = (1 << 3);
  var cloudsBitMask = (1 << 5);
  // Get the pixel QA band.
  var qa = image.select('pixel_qa');
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
                 .and(qa.bitwiseAnd(cloudsBitMask).eq(0));
  return image.updateMask(mask);
}

// cloudmask function for landsat 5-7 SR
var cloudMaskL457 = function(image) {
  var qa = image.select('pixel_qa');
  // If the cloud bit (5) is set and the cloud confidence (7) is high
  // or the cloud shadow bit is set (3), then it's a bad pixel.
  var cloud = qa.bitwiseAnd(1 << 5)
                  .and(qa.bitwiseAnd(1 << 7))
                  .or(qa.bitwiseAnd(1 << 3));
  // Remove edge pixels that don't occur in all bands
  var mask2 = image.mask().reduce(ee.Reducer.min());
  return image.updateMask(cloud.not()).updateMask(mask2);
};




// ----- Processing: Landsat monthly images -----


// Function to get image NDWI
var getNDWI = function(img){
  var green = img.select('B2');
  var swir = img.select('B5');
  var NDWI = green.subtract(swir).divide(green.add(swir)).rename('NDWI');
  return NDWI;
};

// var getNDWI = function(img) {
//     var NDWI = img.normalizedDifference(ndwi_bands).rename("NDWI");
//     return(NDWI);
//   };
  

// Make start and end layers
var monthStart = start;
var totMonths = months;
var MonthstartDates = totMonths.map(function(d) {
  return monthStart.advance(d, 'month');
});
print("Sentinel2 Month Start dates",MonthstartDates);

// Collect imagery by month
var monthMap = function(m){
  var start = ee.Date(m);
  var end = ee.Date(m).advance(1,'month');
  var date_range = ee.DateRange(start,end);
  var name = area.cat(start.format('YYYY-MM')).cat(sensor);
  var ImgMonth = BaseColl
    .filterDate(date_range)
    .filterBounds(study_region)
    .filter(ee.Filter.lt("CLOUD_COVER", 20))
    //.map(cloudMaskL457)
    .map(getNDWI)
    //.select(rgb_bands)
    .map(function(img){return img.clip(study_region)});
  return(ImgMonth.median().set({name: name}));
};
print("monthMap",monthMap);

var list_of_monthly_images = MonthstartDates.map(monthMap);
print('list_of_monthly_images', list_of_monthly_images);
var monthlyColl = ee.ImageCollection(list_of_monthly_images);
print("Monthly NDWI", monthlyColl);


/// ----- Images Visualisation -----

//Map.addLayer(ImgColl,ndwiParams,"NDWI"); // this add the last image of the collection (the more recent)
//select the number of images to display, set to 1 to display all of them (the value should not exceed the total generated images for each collection)
var trueColor321Vis = {
  min: 0.0,
  max: 0.4,
  gamma: 1.2,
};
var images_to_not_display = 1; //this will remove n images from the collection and show all the others


var imageSetCollection = monthlyColl.toList(monthlyColl.size());

print(imageSetCollection);

var test= ee.List.sequence(0,ee.Number(monthlyColl.size().subtract(images_to_not_display))).getInfo();
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
batch.Download.ImageCollection.toDrive(monthlyColl, exp_folder, {
  name: '{name}',
  scale: px_scale,
  region: study_region,
  type: 'float',
  crs: 'EPSG: 32637'
});