//============================================================================================
// TERRAMAZ — FOREST REGROWTH STOCK & FLOW CALCULATOR (TMF + MAPBIOMAS ATBD)
//============================================================================================
/**
 * Title: TerrAmaz — Forest Regrowth Stock & Flow Calculator
 * Purpose: Calculate annual forest regrowth stock and flow following ATBD methodology for Mapbiomas
 * Author: Lucas Lima
 * 
 *
 * OVERVIEW:
 * This script calculates two types of metrics:
 * 
 * 1. STOCK (Estoque): Total secondary forest area in each year
 *    - All pixels that are regrowth in year t (regardless of when regeneration started)
 * 
 * 2. FLOW (Fluxo): New/recurrent regeneration events per year
 *    - Pixels that BECAME regrowth in year t (were NOT regrowth in t-1)
 *    - Allows recounting if pixel was cleared and regenerated again
 *
 * OUTPUT:
 * For each zone, 2 CSV files:
 * - {zone}_regrowth_stock_tmf_mb_1987_2024.csv
 * - {zone}_regrowth_stock_tmf_mb_1987_2024.csv
 *
 * Columns: area_id, year, tmf_px, tmf_area_ha, mb_px, mb_area_ha
 */

//============================================================================================
// 1) CONFIGURATION
//============================================================================================

var YEARS_EXPORT = ee.List.sequence(1987, 2024);
var TMF_YEAR_MIN = 1990;
var TMF_YEAR_MAX = 2024;
var MB_YEAR_MIN = 1987;
var MB_YEAR_MAX = 2024;

// Spatial filter threshold (hectares)
var MIN_PATCH_HA = 0.5;

// Processing parameters
var MAX_PIXELS = 1e13;
var TILE_SCALE = 4;

//============================================================================================
// 2) DATA SOURCES
//============================================================================================

var ZONES = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');
var TMF_ACC = ee.Image('projects/JRC/TMF/v1_2024/AnnualChanges/SAM');
var TMF_PROJ = TMF_ACC.projection();
var MB_AMZ = ee.Image('projects/mapbiomas-public/assets/amazon/lulc/collection6/mapbiomas_collection60_integration_v1');
var MB_PROJ = MB_AMZ.projection();

//============================================================================================
// 3) UTILITY FUNCTIONS
//============================================================================================

function calculatePixelCount(mask, geometry, projection) {
  var totalPixels = mask.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: geometry,
    crs: projection.crs(),
    scale: projection.nominalScale(),
    maxPixels: MAX_PIXELS,
    bestEffort: true,
    tileScale: TILE_SCALE
  });
  var bandName = ee.String(mask.bandNames().get(0));
  var pixels = ee.Number(totalPixels.get(bandName));
  return ee.Number(ee.Algorithms.If(pixels, pixels, 0));
}

function calculateAreaHa(mask, geometry, projection) {
  var pixelArea = ee.Image.pixelArea().reproject(projection).divide(10000);
  var totalArea = mask.multiply(pixelArea).reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: geometry,
    crs: projection.crs(),
    scale: projection.nominalScale(),
    maxPixels: MAX_PIXELS,
    bestEffort: true,
    tileScale: TILE_SCALE
  });
  var bandName = ee.String(mask.bandNames().get(0));
  var area = ee.Number(totalArea.get(bandName));
  return ee.Number(ee.Algorithms.If(area, area, 0));
}

function createMembershipMask(image, valuesList) {
  var mask = image.eq(ee.Number(valuesList.get(0)));
  return ee.Image(valuesList.slice(1).iterate(function(value, acc) {
    return ee.Image(acc).or(image.eq(ee.Number(value)));
  }, mask));
}

function applySpatialFilter(binaryMask, projection) {
  var pixelAreaM2 = ee.Number(projection.nominalScale()).pow(2);
  var minPixels = ee.Number(MIN_PATCH_HA).multiply(10000).divide(pixelAreaM2).round();
  var connected = binaryMask.selfMask().connectedPixelCount(512, true);
  return binaryMask.updateMask(connected.gte(minPixels));
}

//============================================================================================
// 4) TMF STOCK & FLOW
//============================================================================================

/**
 * Get TMF STOCK mask for a given year (all pixels that are class 4 in that year)
 */
function getTMFStockMask(year) {
  year = ee.Number(year);
  var isValidYear = year.gte(TMF_YEAR_MIN).and(year.lte(TMF_YEAR_MAX));
  
  return ee.Image(ee.Algorithms.If(isValidYear, function() {
    var bandName = ee.String('Dec').cat(year.format('%d'));
    var bandExists = TMF_ACC.bandNames().indexOf(bandName).gte(0);
    
    return ee.Image(ee.Algorithms.If(
      bandExists,
      TMF_ACC.select(bandName).eq(4).selfMask().rename('tmf_stock'),
      ee.Image(0).updateMask(ee.Image(0))
    ));
  }(), ee.Image(0).updateMask(ee.Image(0))));
}

/**
 * Get TMF FLOW mask for a given year (pixels that became class 4, were NOT class 4 in t-1)
 */
function getTMFFlowMask(year) {
  year = ee.Number(year);
  var isValidYear = year.gte(TMF_YEAR_MIN).and(year.lte(TMF_YEAR_MAX));
  
  return ee.Image(ee.Algorithms.If(isValidYear, function() {
    var stockT = getTMFStockMask(year);
    var stockT1 = getTMFStockMask(year.subtract(1));
    
    // Flow = regrowth in t AND NOT regrowth in t-1
    var flow = stockT.unmask(0).and(stockT1.unmask(0).not());
    
    return flow.selfMask().rename('tmf_flow');
  }(), ee.Image(0).updateMask(ee.Image(0))));
}

//============================================================================================
// 5) MAPBIOMAS STOCK & FLOW
//============================================================================================

/**
 * Aggregate MapBiomas classes
 */
function aggregateMapBiomasClasses(yearImage) {
  var anthropicCodes = ee.List([15, 18, 9, 35, 21, 24, 30, 25]);
  var naturalCodes = ee.List([3, 4, 5, 6, 11, 12, 29, 13]);
  var notIncludedCodes = ee.List([23, 33, 34, 27, 68]);
  
  var anthropicMask = createMembershipMask(yearImage, anthropicCodes);
  var naturalMask = createMembershipMask(yearImage, naturalCodes);
  var notIncludedMask = createMembershipMask(yearImage, notIncludedCodes);
  
  return ee.Image(0)
    .where(anthropicMask, 1)
    .where(naturalMask, 2)
    .where(notIncludedMask, 7)
    .updateMask(anthropicMask.or(naturalMask).or(notIncludedMask))
    .reproject(MB_PROJ);
}

/**
 * Create time series of aggregated MapBiomas classifications (cached)
 */
function createMapBiomasTimeSeries() {
  var yearsList = ee.List.sequence(MB_YEAR_MIN - 2, MB_YEAR_MAX + 2);
  
  return ee.ImageCollection(yearsList.map(function(year) {
    year = ee.Number(year);
    var bandName = ee.String('classification_').cat(year.format('%d'));
    var bandExists = MB_AMZ.bandNames().indexOf(bandName).gte(0);
    
    var aggregated = ee.Image(ee.Algorithms.If(
      bandExists,
      aggregateMapBiomasClasses(MB_AMZ.select(bandName)),
      ee.Image(0).updateMask(ee.Image(0))
    ));
    
    return aggregated.set('year', year);
  }));
}

var MB_TIME_SERIES = createMapBiomasTimeSeries();

/**
 * PRE-CALCULATE all MapBiomas regrowth events (optimization for STOCK calculation)
 * Returns ImageCollection where each image = regrowth events detected in that year
 */
function createMBRegrowthEventsCollection() {
  // Only calculate events where we can detect them (year + 2 <= MB_YEAR_MAX)
  var validEventYears = ee.List.sequence(MB_YEAR_MIN, MB_YEAR_MAX - 2);
  
  return ee.ImageCollection(validEventYears.map(function(eventYear) {
    eventYear = ee.Number(eventYear);
    
    var years = [eventYear.subtract(2), eventYear.subtract(1), eventYear,
                 eventYear.add(1), eventYear.add(2)];
    
    var imgs = years.map(function(y) {
      return ee.Image(MB_TIME_SERIES.filter(ee.Filter.eq('year', y)).first());
    });
    
    var img_t2 = ee.Image(imgs[0]);
    var img_t1 = ee.Image(imgs[1]);
    var img_t0 = ee.Image(imgs[2]);
    var img_tp1 = ee.Image(imgs[3]);
    var img_tp2 = ee.Image(imgs[4]);
    
    // Apply ATBD trajectory rules
    var anthropicBefore = img_t2.eq(1).and(img_t1.eq(1));
    var naturalAfter = img_t0.eq(2).and(img_tp1.eq(2)).and(img_tp2.eq(2));
    var noNotIncluded = img_t2.neq(7).and(img_t1.neq(7)).and(img_t0.neq(7))
                        .and(img_tp1.neq(7)).and(img_tp2.neq(7));
    
    var regrowthEvent = anthropicBefore.and(naturalAfter).and(noNotIncluded);
    
    // Apply spatial filter ONCE per event
    var filtered = applySpatialFilter(regrowthEvent, MB_PROJ);
    
    return filtered.unmask(0).rename('event').set('event_year', eventYear);
  }));
}

// Pre-calculate all regrowth events (called once)
var MB_REGROWTH_EVENTS = createMBRegrowthEventsCollection();

/**
 * Get MapBiomas STOCK mask for a given year (OPTIMIZED VERSION)
 * Stock = Union of all events (year <= t) that are still natural in year t
 */
function getMBStockMask(year) {
  year = ee.Number(year);
  var isValidYear = year.gte(MB_YEAR_MIN).and(year.lte(MB_YEAR_MAX));
  
  return ee.Image(ee.Algorithms.If(isValidYear, function() {
    // Get current year classification
    var currentYear = ee.Image(MB_TIME_SERIES.filter(ee.Filter.eq('year', year)).first());
    var isNaturalNow = currentYear.eq(2);
    
    // Get all events that happened at or before year t
    var eventsBeforeT = MB_REGROWTH_EVENTS
      .filter(ee.Filter.lte('event_year', year));
    
    // Union of all events (max = logical OR for binary images)
    var hadRegrowthBefore = eventsBeforeT.max();
    
    // Stock = had regrowth event before AND is still natural now
    var stock = hadRegrowthBefore.and(isNaturalNow);
    
    return stock.selfMask().rename('mb_stock');
  }(), ee.Image(0).updateMask(ee.Image(0))));
}

/**
 * Get MapBiomas FLOW mask for a given year (OPTIMIZED VERSION)
 * Uses pre-calculated events collection
 */
function getMBFlowMask(year) {
  year = ee.Number(year);
  
  // For flow, we just get the event from the pre-calculated collection
  var isValidYear = year.gte(MB_YEAR_MIN).and(year.add(2).lte(MB_YEAR_MAX));
  
  return ee.Image(ee.Algorithms.If(isValidYear, function() {
    var eventMask = MB_REGROWTH_EVENTS
      .filter(ee.Filter.eq('event_year', year))
      .first();
    
    return eventMask.selfMask().rename('mb_flow');
  }(), ee.Image(0).updateMask(ee.Image(0))));
}

//============================================================================================
// 6) AREA CALCULATION PER ZONE
//============================================================================================

function calculateStockForZone(feature) {
  feature = ee.Feature(feature);
  var zoneId = feature.get('zone');
  var geometry = feature.geometry();
  
  var tmfGeom = geometry.transform(TMF_PROJ, 1);
  var mbGeom = geometry.transform(MB_PROJ, 1);
  
  var annualResults = YEARS_EXPORT.map(function(year) {
    year = ee.Number(year);
    
    // TMF Stock
    var tmfStockMask = getTMFStockMask(year);
    var tmfPixels = calculatePixelCount(tmfStockMask, tmfGeom, TMF_PROJ);
    var tmfArea = calculateAreaHa(tmfStockMask, tmfGeom, TMF_PROJ);
    
    // MapBiomas Stock
    var mbStockMask = getMBStockMask(year);
    var mbPixels = calculatePixelCount(mbStockMask, mbGeom, MB_PROJ);
    var mbArea = calculateAreaHa(mbStockMask, mbGeom, MB_PROJ);
    
    return ee.Feature(null, {
      'area_id': zoneId,
      'year': year,
      'tmf_px': tmfPixels,
      'tmf_area_ha': tmfArea,
      'mb_px': mbPixels,
      'mb_area_ha': mbArea
    });
  });
  
  return ee.FeatureCollection(annualResults);
}

function calculateFlowForZone(feature) {
  feature = ee.Feature(feature);
  var zoneId = feature.get('zone');
  var geometry = feature.geometry();
  
  var tmfGeom = geometry.transform(TMF_PROJ, 1);
  var mbGeom = geometry.transform(MB_PROJ, 1);
  
  var annualResults = YEARS_EXPORT.map(function(year) {
    year = ee.Number(year);
    
    // TMF Flow
    var tmfFlowMask = getTMFFlowMask(year);
    var tmfPixels = calculatePixelCount(tmfFlowMask, tmfGeom, TMF_PROJ);
    var tmfArea = calculateAreaHa(tmfFlowMask, tmfGeom, TMF_PROJ);
    
    // MapBiomas Flow
    var mbFlowMask = getMBFlowMask(year);
    var mbPixels = calculatePixelCount(mbFlowMask, mbGeom, MB_PROJ);
    var mbArea = calculateAreaHa(mbFlowMask, mbGeom, MB_PROJ);
    
    return ee.Feature(null, {
      'area_id': zoneId,
      'year': year,
      'tmf_px': tmfPixels,
      'tmf_area_ha': tmfArea,
      'mb_px': mbPixels,
      'mb_area_ha': mbArea
    });
  });
  
  return ee.FeatureCollection(annualResults);
}

//============================================================================================
// 7) BATCH EXPORT
//============================================================================================

function exportResults() {
  print('================================================================================');
  print('TERRAMAZ - STOCK & FLOW CALCULATOR');
  print('================================================================================');
  print('TMF coverage:', TMF_YEAR_MIN, '-', TMF_YEAR_MAX);
  print('MapBiomas coverage:', MB_YEAR_MIN, '-', MB_YEAR_MAX);
  print('Minimum patch size:', MIN_PATCH_HA, 'ha');
  print('================================================================================');
  
  ZONES.evaluate(function(featureCollection) {
    featureCollection.features.forEach(function(feature) {
      var areaId = feature.properties.zone;
      
      print('\n📍 Processing zone:', areaId);
      
      // STOCK Export
      var stockTable = calculateStockForZone(ee.Feature(feature));
      Export.table.toDrive({
        collection: stockTable,
        description: areaId + '_regrowth_stock_tmf_mb_1987_2024',
        folder: areaId + '_Terramaz_metrics',
        fileNamePrefix: areaId + '_regrowth_stock_tmf_mb_1987_2024',
        fileFormat: 'CSV'
      });
      print('  ✓ STOCK export queued');
      
      // FLOW Export
      var flowTable = calculateFlowForZone(ee.Feature(feature));
      Export.table.toDrive({
        collection: flowTable,
        description: areaId + '_regrowth_flow_tmf_mb_1987_2024',
        folder: areaId + '_Terramaz_metrics',
        fileNamePrefix: areaId + '_regrowth_flow_tmf_mb_1987_2024',
        fileFormat: 'CSV'
      });
      print('  ✓ FLOW export queued');
    });
    
    print('\n' + '================================================================================');
    print('✓ ALL EXPORT TASKS QUEUED SUCCESSFULLY!');
    print('Total exports: ' + (featureCollection.features.length * 2));
    print('================================================================================');
  });
}

//============================================================================================
// 8) EXECUTION
//============================================================================================

exportResults();

//============================================================================================
// 9) DEBUGGING (Optional)
//============================================================================================

// // Test for a specific zone and year
// var testZone = ZONES.first();
// var testYear = 2010;

// // Visualize STOCK
// var tmfStock = getTMFStockMask(testYear);
// var mbStock = getMBStockMask(testYear);
// Map.addLayer(tmfStock, {palette: ['blue']}, 'TMF Stock ' + testYear);
// Map.addLayer(mbStock, {palette: ['green']}, 'MB Stock ' + testYear);

// // Visualize FLOW
// var tmfFlow = getTMFFlowMask(testYear);
// var mbFlow = getMBFlowMask(testYear);
// Map.addLayer(tmfFlow, {palette: ['red']}, 'TMF Flow ' + testYear);
// Map.addLayer(mbFlow, {palette: ['yellow']}, 'MB Flow ' + testYear);

// Map.addLayer(ZONES, {color: 'white'}, 'Zones');
// Map.centerObject(ZONES, 8);