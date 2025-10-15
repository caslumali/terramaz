//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz — Forest & Non-Forest Mask Generator from TMF AnnualChanges (COMPLETE)
 * Author: Lucas Lima + GEE Assistant
 *
 * Goal:
 *   Generate annual forest and non-forest masks using TMF AnnualChanges dataset (1990–2024).
 *   Forest classes: 1=Undisturbed, 2=Degraded, 4=Regrowth
 *   Non-forest classes: 3=Deforested, 6=Other land cover (excludes 5=Water)
 *
 * Inputs:
 *   • TMF AnnualChanges: 'projects/JRC/TMF/v1_2024/AnnualChanges/SAM'
 *     - Bands: Dec1990..Dec2024 (values: 1–6)
 *   • Amazon limits: 'projects/terramaz/assets/bases/lim_amazon'
 *   • Zonal limits: 'projects/terramaz/assets/bases/zones_terramaz'
 *
 * Outputs:
 *   • Forest mask (multi-band or single year): Asset and/or Drive
 *   • Non-forest mask (multi-band or single year): Asset and/or Drive
 */

//============================================================================================
// 1) CONFIGURATION
//============================================================================================
var TMF_PATH = 'projects/JRC/TMF/v1_2024/AnnualChanges/SAM';
var AMAZON_PATH = 'projects/terramaz/assets/bases/lim_amazon_eva';
var ZONES_PATH = 'projects/terramaz/assets/bases/zones_terramaz';

var OUTPUT_FOREST_ASSET = 'projects/terramaz/assets/bases/forest_mask_annual_1990_2024';
var OUTPUT_NONFOREST_ASSET = 'projects/terramaz/assets/bases/nonforest_mask_annual_1990_2024';
var OUTPUT_SINGLE_ASSET_PREFIX = 'projects/terramaz/assets/bases/mask_single_';

var EXPORT_SCALE = 30;
var MAX_PIXELS = 1e13;

// Export flags
var EXPORT_ALL_YEARS = true;     // true = export full 1990–2024 masks
var EXPORT_SINGLE_YEAR = true;    // true = export only one year

var TARGET_YEAR = 2024;           // year to export if EXPORT_SINGLE_YEAR is true
var EXPORT_TO_ASSET = true;
var EXPORT_TO_DRIVE = true;

//============================================================================================
// 2) LOAD DATA
//============================================================================================
var TMF = ee.Image(TMF_PATH);
var AMAZON_LIM = ee.FeatureCollection(AMAZON_PATH);
var ZONES = ee.FeatureCollection(ZONES_PATH);
var PROJ = TMF.projection();

print('✓ TMF bands:', TMF.bandNames().size());
print('✓ Amazon limits loaded');
print('✓ Zonal divisions loaded');

//============================================================================================
// 3) MASK GENERATION
//============================================================================================

// Forest: 1=undisturbed, 2=degraded, 4=regrowth
var forestMaskAnnual = TMF.eq(1).or(TMF.eq(2)).or(TMF.eq(4))
  .rename(TMF.bandNames())
  .unmask(0).toByte()
  .clip(AMAZON_LIM)
  .set({
    'type': 'forest',
    'classes_included': '1,2,4',
    'pixel': '1=forest, 0=non-forest',
    'source': 'JRC TMF AnnualChanges v2024',
    'scale_m': EXPORT_SCALE,
    'created_by': 'TerrAmaz_project',
    'created_date': ee.Date(Date.now()).format('YYYY-MM-dd'),
    'bands_count': TMF.bandNames().size()
  });

// Non-Forest: 3=deforested, 6=other land (excluding water=5)
var nonForestMaskAnnual = TMF.eq(3).or(TMF.eq(6))
  .rename(TMF.bandNames())
  .unmask(0).toByte()
  .clip(AMAZON_LIM)
  .set({
    'type': 'non_forest',
    'classes_included': '3,6',
    'pixel': '1=non-forest, 0=other',
    'note': 'Water (class 5) excluded',
    'source': 'JRC TMF AnnualChanges v2024',
    'scale_m': EXPORT_SCALE,
    'created_by': 'TerrAmaz_project',
    'created_date': ee.Date(Date.now()).format('YYYY-MM-dd'),
    'bands_count': TMF.bandNames().size()
  });

//============================================================================================
// 4) VISUALIZATION
//============================================================================================
var vizParams = {
  min: 0,
  max: 1,
  palette: ['white', 'darkgreen'],
  opacity: 0.8
};

Map.centerObject(AMAZON_LIM, 5);
Map.addLayer(AMAZON_LIM, {color: 'red', fillColor: '00000000'}, 'Amazon Limits', false);
Map.addLayer(ZONES, {color: 'blue'}, 'Zones TerrAmaz', false);
Map.addLayer(forestMaskAnnual.select('Dec2020'), vizParams, 'Forest Mask 2020', false);
Map.addLayer(nonForestMaskAnnual.select('Dec2020'), {palette: ['white', 'orange'], min: 0, max: 1}, 'Non-Forest Mask 2020', false);
//============================================================================================
// 5) EXPORT MULTI-BAND MASKS (1990–2024)
//============================================================================================
if (EXPORT_ALL_YEARS) {
  Export.image.toAsset({
    image: forestMaskAnnual,
    description: 'forest_mask_annual_1990_2024',
    assetId: OUTPUT_FOREST_ASSET,
    region: AMAZON_LIM,
    scale: EXPORT_SCALE,
    maxPixels: MAX_PIXELS,
    crs: PROJ.getInfo().crs,
    pyramidingPolicy: { '.default': 'mode' }
  });

  Export.image.toAsset({
    image: nonForestMaskAnnual,
    description: 'nonforest_mask_annual_1990_2024',
    assetId: OUTPUT_NONFOREST_ASSET,
    region: AMAZON_LIM,
    scale: EXPORT_SCALE,
    maxPixels: MAX_PIXELS,
    crs: PROJ.getInfo().crs,
    pyramidingPolicy: { '.default': 'mode' }
  });

  print('✓ Multiband export tasks (forest and non-forest) queued.');
}

//============================================================================================
// 6) EXPORT SINGLE-YEAR MASKS (to ASSET & DRIVE)
//============================================================================================
if (EXPORT_SINGLE_YEAR) {
  var bandName = 'Dec' + TARGET_YEAR;
  var forestSingle = forestMaskAnnual.select(bandName).set({year: TARGET_YEAR, type: 'forest'});
  var nonForestSingle = nonForestMaskAnnual.select(bandName).set({year: TARGET_YEAR, type: 'non_forest'});

  if (EXPORT_TO_ASSET) {
    Export.image.toAsset({
      image: forestSingle,
      description: 'forest_mask_' + TARGET_YEAR,
      assetId: OUTPUT_SINGLE_ASSET_PREFIX + 'forest_' + TARGET_YEAR,
      region: AMAZON_LIM,
      scale: EXPORT_SCALE,
      maxPixels: MAX_PIXELS,
      crs: PROJ.getInfo().crs,
      pyramidingPolicy: { '.default': 'mode' }
    });

    Export.image.toAsset({
      image: nonForestSingle,
      description: 'nonforest_mask_' + TARGET_YEAR,
      assetId: OUTPUT_SINGLE_ASSET_PREFIX + 'nonforest_' + TARGET_YEAR,
      region: AMAZON_LIM,
      scale: EXPORT_SCALE,
      maxPixels: MAX_PIXELS,
      crs: PROJ.getInfo().crs,
      pyramidingPolicy: { '.default': 'mode' }
    });
  }

  if (EXPORT_TO_DRIVE) {
    Export.image.toDrive({
      image: forestSingle,
      description: 'forest_mask_' + TARGET_YEAR + '_drive',
      folder: 'TerrAmaz_rasters',
      fileNamePrefix: 'forest_mask_' + TARGET_YEAR,
      region: AMAZON_LIM,
      scale: EXPORT_SCALE,
      maxPixels: MAX_PIXELS,
      crs: PROJ.getInfo().crs
    });

    Export.image.toDrive({
      image: nonForestSingle,
      description: 'nonforest_mask_' + TARGET_YEAR + '_drive',
      folder: 'TerrAmaz_rasters',
      fileNamePrefix: 'nonforest_mask_' + TARGET_YEAR,
      region: AMAZON_LIM,
      scale: EXPORT_SCALE,
      maxPixels: MAX_PIXELS,
      crs: PROJ.getInfo().crs
    });
  }

  print('✓ Single year export (' + TARGET_YEAR + ') queued (asset/drive).');
}
