Link: https://code.earthengine.google.com/3e9f011367fcdfb5426a7593538f4198
// Load collection
var L8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
           .filterDate('2021-07-01', '2021-07-30')
           .filterMetadata("CLOUD_COVER", 'less_than', 10)
           .select(['SR_B2','SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'])
           .filterBounds(geometry)
           .sort('CLOUD_COVER')
           .first();
 
print(L8);
/***********************************
Calculate NDVI
***********************************/ 
  var ndvi = L8.expression(
  '(nir-red)/(nir+red)', {
    'nir': L8.select('SR_B5').divide(10000),
    'red': L8.select('SR_B4').divide(10000)}).rename('NDVI');

/***********************************
Calculate GNDVI
***********************************/ 
  var gndvi = L8.expression(
  '(nir-green)/(nir+green)', {
    'nir': L8.select('SR_B5').divide(10000),
    'green': L8.select('SR_B3').divide(10000)}).rename('GNDVI');

/***********************************
Calculate NDWI
***********************************/ 
  var ndmi = L8.expression(
  '(nir-swir1)/(nir+swir1)', {
    'nir': L8.select('SR_B5').divide(10000),
    'swir1': L8.select('SR_B6').divide(10000)}).rename('NDMI');


/***********************************
Calculate EVI
***********************************/ 
  var evi = L8.expression(
    '2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))', {
      'nir': L8.select('SR_B5').divide(10000),
      'red': L8.select('SR_B4').divide(10000),
      'blue': L8.select('SR_B2').divide(10000)
    }).rename('EVI');

/***********************************
Calculate BSI
***********************************/ 
  var bsi = L8.expression(
    '((swir1+red) - (nir + blue))/((swir1+red) + (nir + blue))', {
    'blue' : L8.select('SR_B2').divide(10000),
    'nir': L8.select('SR_B5').divide(10000),
    'red' : L8.select('SR_B4').divide(10000),
    'swir1': L8.select('SR_B6').divide(10000)
    }).rename('BSI');

    
/****************************************
Map over the collection
****************************************/ 
var L8withindices = L8.addBands(ndvi)
                      .addBands(gndvi)
                      .addBands(ndmi)
                      .addBands(evi)
                      .addBands(bsi);
print(L8withindices);


// Calculate the median and clip the image
var L8_clip = L8withindices.clip(geometry).toFloat()
print(L8_clip)

// Add image to layer
Map.addLayer(L8_clip, 
            {bands: ['SR_B4', 'SR_B3', 'SR_B2'], 
            min: 7811, max: 11953, gamma: 0.75}, 'median');
            

//Export image
Export.image.toDrive({image: L8_clip,
                      description: 'L8_2021',
                      scale: 30, 
                      crs: 'EPSG:27700'})
