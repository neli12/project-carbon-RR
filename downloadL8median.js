Link: https://code.earthengine.google.com/363950ac45b704cdfdfb2e7f8e36fe4c
// Load collection
var L8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
           .filterDate('2013-01-01', '2013-12-31')
           .filterMetadata("CLOUD_COVER", 'less_than', 20)
           .select(['SR_B2','SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'])
           .filterBounds(geometry);
   
print(L8);
/***********************************
Calculate NDVI
***********************************/ 

var addNDVI = function(img) {
  var ndvi = img.expression(
  '(nir-red)/(nir+red)', {
    'nir': img.select('SR_B5').divide(10000),
    'red': img.select('SR_B4').divide(10000)}).rename('NDVI');
  return img.addBands(ndvi);
}

/***********************************
Calculate GNDVI
***********************************/ 

var addGNDVI = function(img) {
  var ndvi = img.expression(
  '(nir-green)/(nir+green)', {
    'nir': img.select('SR_B5').divide(10000),
    'green': img.select('SR_B3').divide(10000)}).rename('GNDVI');
  return img.addBands(ndvi);
}

/***********************************
Calculate NDWI
***********************************/ 

var addNDMI = function(img) {
  var ndmi = img.expression(
  '(nir-swir1)/(nir+swir1)', {
    'nir': img.select('SR_B5').divide(10000),
    'swir1': img.select('SR_B6').divide(10000)}).rename('NDMI');
  return img.addBands(ndmi);
}

/***********************************
Calculate EVI
***********************************/ 
var addEVI = function(img) {
  var evi = img.expression(
    '2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))', {
      'nir': img.select('SR_B5').divide(10000),
      'red': img.select('SR_B4').divide(10000),
      'blue': img.select('SR_B2').divide(10000)
    }).rename('EVI');
  return img.addBands(evi);
}

/***********************************
Calculate BSI
***********************************/ 
var addBSI = function(img){
  var bsi = img.expression(
    '((swir1+red) - (nir + blue))/((swir1+red) + (nir + blue))', {
    'blue' : img.select('SR_B2').divide(10000),
    'nir': img.select('SR_B5').divide(10000),
    'red' : img.select('SR_B4').divide(10000),
    'swir1': img.select('SR_B6').divide(10000)
    }).rename('BSI');
  return img.addBands(bsi);
}

    
/****************************************
Map over the collection
****************************************/ 
var L8withindices = L8.map(addNDVI)
                            .map(addGNDVI)
                            .map(addNDMI)
                            .map(addEVI)
                            .map(addBSI);
print(L8withindices);


// Calculate the median and clip the image
var L8_median = L8withindices.median().clip(geometry).toFloat()

// Add image to layer
Map.addLayer(L8_median, 
            {bands: ['SR_B4', 'SR_B3', 'SR_B2'], 
            min: 7811, max: 11953, gamma: 0.75}, 'median');
            

//Export image
Export.image.toDrive({image: L8_median,
                      description: 'L8Median2013_27700',
                      scale: 30, 
                      crs: 'EPSG:27700'})
