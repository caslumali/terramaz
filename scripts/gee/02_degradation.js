//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz — Degradation CSV Exporter
 * Purpose: Export one CSV per region with yearly degradation statistics using TMF data.
 * Author: Lucas Lima
 *
 * OUTPUT:
 *   - One CSV per ROI (zone), with one row per year (1990–2024).
 *   - Each row contains:
 *       area_id, year,
 *       tmf_deg_px_total: pixel count of degraded pixels in that year,
 *       tmf_deg_area_ha : area (ha) of degraded pixels.
 *
 * DATASET:
 *   - TMF DegradationYear (v2024): single-band image where each pixel contains a year
 *     when degradation was detected (DecYYYY).
 *     Binary event per year: event in year Y ⇢ TMF_DEG.eq(Y)
 *
 * NOTES:
 *   - Valid year range: 1982–2024.
 *   - For years outside this range, outputs are masked (null/NA in CSV).
 *   - Native projection and scale are preserved for highest spatial accuracy.
 */


//============================================================================================
// 1) CONFIGURATION
//============================================================================================
var YEARS_ALL = ee.List.sequence(1990, 2024);

var ROIS     = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');
var TMF_DEG  = ee.Image('projects/JRC/TMF/v1_2024/DegradationYear/SAM');  // Dec1982..Dec2024

var PROJ       = TMF_DEG.projection();     // native projection
var SCALE      = PROJ.nominalScale();      // native scale
var MAX_PIXELS = 1e13;


//============================================================================================
// 2) UTILITY FUNCTIONS
//============================================================================================

/** Pixel count: sum of 1's in a binary mask within the region. */
function reduceCount(mask, region) {
  var b = ee.String(mask.bandNames().get(0));
  var sum = mask.reduceRegion({
    reducer   : ee.Reducer.sum(),
    geometry  : region,
    scale     : SCALE,
    maxPixels : MAX_PIXELS,
    bestEffort: true
  }).get(b);
  return ee.Number(sum);
}

/** Area (in hectares) of 1's in a binary mask. */
function reduceAreaHa(mask, region) {
  var b = ee.String(mask.bandNames().get(0));
  var ha = ee.Image.pixelArea().divide(10000);
  var sum = mask.multiply(ha).reduceRegion({
    reducer   : ee.Reducer.sum(),
    geometry  : region,
    scale     : SCALE,
    maxPixels : MAX_PIXELS,
    bestEffort: true
  }).get(b);
  return ee.Number(sum);
}


//============================================================================================
// 3) ANNUAL MASK — TMF DegradationYear
//============================================================================================

/**
 * Returns binary mask of degradation events in year Y.
 * Valid only for years 1982–2024. Outside this range: fully masked image.
 */
function tmfDegMaskForYear(y) {
  y = ee.Number(y);
  var computable = y.gte(1982).and(y.lte(2024));

  var event = TMF_DEG.eq(y);
  event = ee.Image(ee.Algorithms.If(
    computable,
    event,
    ee.Image(0).updateMask(ee.Image(0))  // fully masked image for non-computable years
  ));

  return event.rename(ee.String('tmf_deg_').cat(y.format('%d'))).selfMask();
}


//============================================================================================
// 4) TABLE BUILDER — one row per year, one file per area
//============================================================================================
function buildTableForArea(feat) {
  feat = ee.Feature(feat);
  var geom    = feat.geometry().transform(PROJ, 1);
  var areaId  = ee.String(feat.get('zone'));

  var rows = YEARS_ALL.map(function(y) {
    y = ee.Number(y);

    var dm  = tmfDegMaskForYear(y);
    var px  = reduceCount(dm, geom);  // leave null if not computable
    var ha  = reduceAreaHa(dm, geom);

    return ee.Feature(null, {
      area_id         : areaId,
      year            : y,
      tmf_deg_px_total: px,
      tmf_deg_area_ha : ha
    });
  });

  return ee.FeatureCollection(rows);
}


//============================================================================================
// 5) EXPORT — one CSV file per area, inside its own folder
//============================================================================================
ROIS.evaluate(function(fc) {
  fc.features.forEach(function(f) {
    var areaId = f.properties.zone;
    var table  = buildTableForArea(ee.Feature(f));

    print('✓ Task queued for area (degradation):', areaId);

    Export.table.toDrive({
      collection    : table,
      description   : areaId + '_degradation_tmf_1990_2024',
      folder        : areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_degradation_tmf_1990_2024',
      fileFormat    : 'CSV'
    });
  });

  print('✓ All degradation export tasks have been queued.');
});
