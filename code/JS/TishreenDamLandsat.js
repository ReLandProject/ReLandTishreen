// ------------------------------
// Description
// ------------------------------

// The aim of the script is to produce an ImageCollection of NDWI Satellite
// images, with one image mosaic per year or one image mosaic per month.
// The script will also export the resulting ImageCollection to Google Drive.
// Each image will be named in a machine readable format with the study area, the type of image and the date.

// In the "Variable Definition" section, the user will find all the options to edit,
// in this way most of the other sections can be run without any further edits.
// If more in depth changes are needed, be sure to look at the other sections as well.
// The "Variable Definition" section is designed to be used with comments ("//")
// Comment variables or uncomment them depending on the things you want to obtain.
// Note that all the variables are mandatory (except crs, which if not specified will default to EPSG: 4326, and cloudfunction, which is only one in this script).

// At lines 54 to 58 there is a geometry expressed by a series of coordinates. You can leave as it is or import it in your GEE interface.
// At line 199, inside the monthMap function, comment out or uncomment lines 198 or 199 in order to switch between NDWI and RGB output (AFTER changing the other variables accordingly) 

// ------------------------------
// List of editable variables
// ------------------------------

// More info on each of them are found at the specified line.
// - Lines 67-69, baseColl: satellite to choose for the analysis. All are surface reflectance but use TOA for obtain pansharpen data.
// - Lines 78, start: start date for the analysis
// - Lines 81, months: for how many months we need to gather images
// - Line 85, cloud_filter_value: do not select images with a higher cloudy percentage than this
// - Line 91;93, greenBand: first band for NDWI image generation
// - Line 92;94, swirBand: second band for NDWI image generation
// - Line 99-100, rbgBands: define the red, green, and blue band according to the satellite
// - Line 103-104, area: String for the image names
// - Line 107-109, sensor: the name of the satellite to attach to the image name
// - Lines 112, exp_folder: the name of the folder in Google Drive where the images will be saved
// - Line 115-116, px_scale: spatial resolution of the output images once saved to Google Drive
// - Lines 119, crs: CRS of the images once exported to Google Drive
// - Lines 124-125, vizParams: visualisation parameters to be changed accordingly to the desired output. IT DOES NOT influence the saved images.
// - Lines 167, cloudfunction: choose the appropriate cloud masking function to be applied.

// All the images are exported following a naming convention: "area_image_type_YYYY-MM_satellite"
// Once the export task is completed, it will take some minutes for the images to show up in Google Drive
// Make sure to have some space on Google Drive or the process might not complete

// The function for generating list of dates and the one for acquiring one
// image per year is adapted from
// https://gis.stackexchange.com/questions/269313/mapping-over-list-of-dates-in-google-earth-engine


// ------------------------------
// Variable definition
// ------------------------------

// When pasting the code in GEE, hover with the mouse on "geometry_mosul" and then select "Convert" in the popup to convert the geometry to an import record.
var geometry_tishreen = /* color: #0B4A8B */ee.Geometry.Polygon(
    [[[37.99069550372825, 36.31033474346237],
      [38.335048225896216, 36.31033474346237],
      [38.335048225896216, 36.85151712320516],
      [37.99069550372825, 36.85151712320516]]], null, false);

// Study region
var study_region = geometry_tishreen;
Map.centerObject(study_region, 9); //center and zoom to the study area

// -------- Variables for processing --------

// Select the collection to use
var BaseColl = ee.ImageCollection('LANDSAT/LT05/C02/T1_L2'); // Landsat 5, available
// var BaseColl = ee.ImageCollection('LANDSAT/LE07/C02/T1_L2'); // Landsat 7, available
//var BaseColl = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2'); // Landsat 8, available

// The start variable set the starting point and the months variable determine how much
// the function needs to advance from the start variable.
// the sequence starts from 0 (the month defined in "start") and advance for eleven units, in our case months.
// You can potentially cover more than one year by defining e.g.: 
// - ee.List.sequence(0, 23) two years
// - ee.List.sequence(0, 35) three years and so on
// CREATE START AND END LAYER
var start = ee.Date.fromYMD(1999,1,1); // Start date for Landsat 5 (the dam was completed in 1999)
// var start = ee.Date.fromYMD(1999,1,1); // Start date for Landsat7
//var start = ee.Date.fromYMD(2013,1,1); // Start date for Landsat 8
var months = ee.List.sequence(0, 11);  // Number of months

// Set the desired cloud filter value
// Careful, very low cloud filter values obviously lead to fewer images
var cloud_filter_value = ee.Filter.lt("CLOUD_COVER", 20)

// Define bands to use in the NDWI function. Uncomment the appropriate one and comment out the other pair.
// Landsat 5 and Landsat 7
var greenBand = 'SR_B2';
var swirBand = 'SR_B5';

// Landsat 8
// var greenBand = 'SR_B3';
// var swirBand = 'SR_B6';

// Define RGB bands if we want to export rgb composites
var rbgBands = ['SR_B3', 'SR_B2', 'SR_B1'] // Landsat 5-7
//var rbgBands = ['SR_B4', 'SR_B3', 'SR_B2'] // landsat 8

// -------- Variables for export --------

// Set the string for the area name to be used for file naming
var area = ee.String('tshr_NDWI_');
// var area = ee.String('tshr_RGBA_');

// A string identifying the satellite (useful in the image naming later)
var sensor = ee.String("_L5");
//var sensor = ee.String("_L7");
//var sensor = ee.String("_L8");

// Choose the name of the folder to store the images exported
var exp_folder = 'rgeeRelandTishreen'; // Here you can set the name of the new folder to be created in GDrive

// Spatial resolution of the output images
var px_scale = 30; // anything other that the original band scale will result in a badly generated image
//var px_scale = 15; // uncomment if doing pansharpening of Landsat 8 images

// Select exporting CRS for satellite images
var crs = 'EPSG: 32637'

// -------- Variables for visualization --------

// Define parameters for the visualisation in Google Earth Engine
var vizParams = { min: -1, max: 1, palette: ['brown', 'white', 'blue'] };  // NDWI
// var vizParams = { min: 0.0, max: 0.4, gamma: 1.2 }; // True Color

// ------------------------------
// Functions and processing variables
// ------------------------------


// ----- Landsat-specific Functions -----

// Cloudmask function for landsat 5-7 Surface reflectance
function maskL457sr(image) {
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
      .addBands(thermalBand, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}

// Cloud masking landsat 8 Surface reflectance
function maskL8sr(image) {
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
      .addBands(thermalBands, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}

// Select the appropriate cloudfunction from the above
var cloudfunction = maskL457sr;

// ----- Processing: Landsat monthly images -----

// Function to get image NDWI
var getNDWI = function(img){
  var green = img.select(greenBand);
  var swir = img.select(swirBand);
  var NDWI = green.subtract(swir).divide(green.add(swir)).rename('NDWI');
  return NDWI;
};

// Make start and end layers
var monthStart = start;
var totMonths = months;
var MonthstartDates = totMonths.map(function(d) {
  return monthStart.advance(d, 'month');
});
print("Landsat Month Start dates",MonthstartDates);

// Collect imagery by month
var monthMap = function(m){
  var start = ee.Date(m);
  var end = ee.Date(m).advance(1,'month');
  var date_range = ee.DateRange(start,end);
  var name = area.cat(start.format('YYYY-MM')).cat(sensor);
  var ImgMonth = BaseColl
    .filterDate(date_range)
    .filterBounds(study_region)
    .filter(cloud_filter_value)
    .map(cloudfunction)
    .map(getNDWI)  // comment out this line when the line below is uncommented
    //.select(rgb_bands)  // uncomment this to get rgb composites
    .map(function(img){return img.clip(study_region)});
  return(ImgMonth.median().set({name: name}));
};
print("monthMap",monthMap);

var list_of_monthly_images = MonthstartDates.map(monthMap);
print('list_of_monthly_images', list_of_monthly_images);
var monthlyColl = ee.ImageCollection(list_of_monthly_images);
print("Monthly NDWI", monthlyColl);


// ----- Images Visualisation -----

// Map.addLayer(ImgColl,ndwiParams,"NDWI"); // this add the last image of the collection (the more recent)

// Select the number of images to display, set to 1 to display all of them (the value should not exceed the total generated images for each collection)
var images_to_not_display = 1; //this will remove n images from the collection and show all the others


var imageSetCollection = monthlyColl.toList(monthlyColl.size());

print(imageSetCollection);

var test= ee.List.sequence(0,ee.Number(monthlyColl.size().subtract(images_to_not_display))).getInfo();
var fun = function(img){

  var image = ee.Image(imageSetCollection.get(img));
  var names = ee.String(image.get('name')).getInfo();
  var label = img + '_' +names;

  Map.addLayer(image,vizParams, label, false);
};

test.map(fun); //this is disabled by default, as it can slow down the process.
Map.centerObject(study_region, 9); // We center the map on our study area with a set zoom

// ------------------------------
// Export images to Google Drive
// ------------------------------

// Since there is no native way to export all the images in a Collection, we need to import an external module to do that
// The module was developed by Rodrigo E. Principe and is available at: https://github.com/fitoprincipe/geetools-code-editor
// This tool will create a folder in your Google Drive, where all the images in the collection will be exported.

var batch = require('users/fitoprincipe/geetools:batch'); // Here we load the module
batch.Download.ImageCollection.toDrive(monthlyColl, exp_folder, {
  name: '{name}',
  scale: px_scale,
  region: study_region,
  type: 'float',
  crs: crs
});