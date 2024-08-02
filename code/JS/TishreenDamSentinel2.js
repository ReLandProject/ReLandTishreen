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
// Note that all the variables are mandatory (except crs, which if not specified will default to EPSG: 4326).

// At lines 53 to 57 there is a geometry expressed by a series of coordinates. You can leave as it is or import it in your GEE interface.
// At lines 157, inside the monthMap function, comment out or uncomment lines XX and XX in order to produce rgb or pansharpened rgb output 

// ------------------------------
// List of editable variables
// ------------------------------

// More info on each of them are found at the specified line.
// - Lines 66, baseColl: satellite to choose for the analysis. All are surface reflectance but use TOA for obtain pansharpen data.
// - Lines 75, start: start date for the analysis
// - Lines 76, month: for how many months we need to gather images
// - Line 80, cloud_filter_value: do not select images with a higher cloudy percentage than this
// - Line 83, greenBand: first band for NDWI image generation
// - Line 84, swirBand: second band for NDWI image generation
// - Line 87, rbgBands: define the red, green, and blue band according to the satellite
// - Line 92; 93, area: String for the image names
// - Line 96, sensor: the name of the satellite to attach to the image name
// - Lines 99, exp_folder: the name of the folder in Google Drive where the images will be saved
// - Line 102-103, px_scale: spatial resolution of the output images once saved to Google Drive
// - Lines 106, crs: CRS of the images once exported to Google Drive
// - Lines 111-112, vizParams: visualisation parameters to be changed accordingly to the desired output. IT DOES NOT influence the saved images.
// - Lines 136, cloudfunction: choose the appropriate cloud masking function to be applied.

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
var baseColl = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED');         // Sentinel 2, available

// The start variable set the starting point and the months variable determine how much
// the function needs to advance from the start variable.
// the sequence starts from 0 (the month defined in "start") and advance for eleven units, in our case months.
// You can potentially cover more than one year by defining e.g.: 
// - ee.List.sequence(0, 23) two years
// - ee.List.sequence(0, 35) three years and so on
// CREATE START AND END LAYER
var start = ee.Date.fromYMD(2018,1,1);    // Start date for Sentinel 2
var months = ee.List.sequence(0, 11);     // Number of years for Sentinel2

// Set the desired cloud filter value
// Careful, very low cloud filter values obviously lead to fewer images
var cloud_filter_value = ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 20)

// Define bands to use in the NDWI function. Uncomment the appropriate one and comment out the other pair.
var greenBand = 'B3';
var swirBand = 'B11';

// Define RGB bands if we want to export rgb composites
var rgbBands = ['B4', 'B3', 'B2'];

// -------- Variables for export --------

// Set the string for the area name to be used for file naming
var area = ee.String('tshr_NDWI_');
// var area = ee.String('tshr_RGBA_');

// A string identifying the satellite (useful in the image naming later)
var sensor = ee.String("_S2");

// Choose the name of the folder to store the images exported
var exp_folder = 'rgeeRelandTishreen'; // Here you can set the name of the new folder to be created in GDrive

// Spatial resolution of the output images
var px_scale = 20; // anything other that the original band scale will result in a badly generated image
//var px_scale = 10; // uncomment if generating RGB composites

// Select exporting CRS for satellite images
var crs = 'EPSG: 32637'

// -------- Variables for visualization --------

// Define parameters for the visualisation in Google Earth Engine
var vizParams = { min: -1, max: 1, palette: ['brown', 'white', 'blue'] };  // NDWI
//var vizParams = { min: 0, max: 0.3, bands: rgbBands }; // for visualizing rgb images

// ------------------------------
// Functions and processing variables
// ------------------------------

// ----- Sentinel-specific Functions -----

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

// Select the appropriate cloudfunction from the above
var cloudfunction = maskS2clouds_sr;

// ----- Processing: Sentinel monthly images -----

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
var monthStartDates = totMonths.map(function(d) {
  return monthStart.advance(d, 'month');
});
print("Sentinel2 Month Start dates",monthStartDates);

// Collect imagery by month
var monthMap = function(m){
  var start = ee.Date(m);
  var end = ee.Date(m).advance(1,'month');
  var date_range = ee.DateRange(start,end);
  var name = area.cat(start.format('YYYY-MM')).cat(sensor);
  var ImgMonth = baseColl
    .filterDate(date_range)
    .filterBounds(study_region)
    .filter(cloud_filter_value)
    .map(cloudfunction)
    .map(getNDWI) // comment out this line when the line below is uncommented
    //.select(rgbBands) // uncomment this to get rgb composites
    .map(function(img){return img.clip(study_region)});
  return(ImgMonth.median().set({name: name}));
};
print("monthMap",monthMap);

var list_of_S2monthly_images = monthStartDates.map(monthMap);
print('list_of_S2monthly_images', list_of_S2monthly_images);
var S2MonthlyColl = ee.ImageCollection(list_of_S2monthly_images);
print("Monthly NDWI", S2MonthlyColl);

// ----- Images Visualisation -----

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
batch.Download.ImageCollection.toDrive(S2MonthlyColl, exp_folder, {
  name: '{name}',
  scale: px_scale,
  region: study_region,
  type: 'float',
  crs: crs
});
