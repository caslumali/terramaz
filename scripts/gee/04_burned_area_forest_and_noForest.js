//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz — Forest & Non-Forest Fire CSV Exporter (MB Fire + GLAD Fire + MODIS BA)
 * Author: Lucas Lima
 *
  * Goal
 *   Export annual burned area (ha) per territory (ROI) for both forest-only (fo) and non-forest (nf):
 *     • Brazil (country == "BR"):   MapBiomas Fire  OR GLAD Fire
 *     • Other countries (e.g., CO, PE): MODIS Burned BA OR GLAD Fire
 *   Masks from TMF ACC:
 *     • Forest (fo)      = {1 Undisturbed, 2 Degraded, 4 Regrowth}
 *     • Non-forest (nf)  = {3 Deforested, 6 Other land cover}  (water=5 excluded)
 *
 *
 * Output (one CSV per ROI)
 *   Rows: years 2001–2024
 *   Columns:
 *     area_id, year,
 * 
 *     -- INSIDE FOREST (masked by forest):
 *     mapbiomas_fire_fo_px, mapbiomas_fire_fo_ha,
 *     glad_fire_fo_px,      glad_fire_fo_ha,
 *     modis_fire_fo_px,     modis_fire_fo_ha,
 *     combined_fire_fo_px,  combined_fire_fo_ha,     // BR: MB∨GLAD ; Others: MODIS∨GLAD
 * 
 *     -- OUTSIDE FOREST (masked by non-forest):
 *     mapbiomas_fire_nf_px, mapbiomas_fire_nf_ha,
 *     glad_fire_nf_px,      glad_fire_nf_ha,
 *     modis_fire_nf_px,     modis_fire_nf_ha,
 *     combined_fire_nf_px,  combined_fire_nf_ha,
 *     source_used
 *
 * Notes
 *  • Pixel counts (px) are diagnostic; area_ha is the comparable metric across products.
 *  • All reductions are evaluated at the forest mask native grid (30 m) via crs/scale in reduceRegion.
 *  • One reduceRegion per year per ROI (stacked bands) to avoid timeouts.
 */

//============================================================================================
// 1) CONFIG
//============================================================================================
var YEARS = ee.List.sequence(2001, 2024);
var ROIS  = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');

// Core datasets
var FOREST_MASK    = ee.Image('projects/terramaz/assets/bases/forest_mask_annual_1990_2024');     // 1=forest
var NONFOREST_MASK = ee.Image('projects/terramaz/assets/bases/nonforest_mask_annual_1990_2024');  // 1=non-forest (3,6)
var MAPBIOMAS_FIRE = ee.Image('projects/mapbiomas-public/assets/brazil/fire/collection4/mapbiomas_fire_collection4_annual_burned_v1');
var GLAD_FIRE_YEARLY = ee.Image('users/sashatyu/2001-2024_fire_forest_loss_annual/LAM_fire_forest_loss_2001-24_annual');
var GLAD_FIRE_CONF   = ee.Image('users/sashatyu/2001-2024_fire_forest_loss/LAM_fire_forest_loss_2001-24');
var MODIS_FIRE       = ee.ImageCollection('MODIS/061/MCD64A1');

// Use mask projection/scale to keep areas consistent
var PROJ       = FOREST_MASK.projection();
var SCALE      = PROJ.nominalScale();
var MAX_PIXELS = 1e13;

// single-area constants
var TILE_SCALE = 6;            

// Keep area image on the mask grid without forcing global resample up front.
// It will still be evaluated on-the-fly at reduceRegion with crs/scale.
var HA = ee.Image.pixelArea().divide(1e4).setDefaultProjection(PROJ, null, SCALE);


// Sanity prints
print('✓ Forest mask bands:', FOREST_MASK.bandNames());
print('✓ Non-forest mask bands:', NONFOREST_MASK.bandNames());
print('✓ ROIs loaded:', ROIS.size());
print('✓ Projection:', PROJ);

//============================================================================================
// 2) UTILITIES
//============================================================================================
function isBrazil(country){ return ee.String(country).compareTo('BR').eq(0); }

//============================================================================================
// 3) FIRE MASKS (annual, binary 0/1)
//============================================================================================
// Function to make MapBiomas Fire Mask
function getMapBiomasFireMask(year){
  year = ee.Number(year);
  var band = ee.String('burned_area_').cat(year.format('%d'));
  var has  = MAPBIOMAS_FIRE.bandNames().contains(band);
  var img  = ee.Image(ee.Algorithms.If(has, MAPBIOMAS_FIRE.select(band), ee.Image(0)));
  return img.eq(1).rename('mapbiomas_fire').unmask(0).toByte();
}

// Function to make Glad Fire Mask
function getGladFireMask(year){
  var code   = ee.Number(year).subtract(2000); // 2001→1 .. 2024→24
  var annual = GLAD_FIRE_YEARLY.eq(code).unmask(0);
  var conf   = GLAD_FIRE_CONF.gte(2).unmask(0);
  return annual.and(conf).rename('glad_fire').toByte();
}

// Robust QA using bitmask: (bit0==1 land) AND (bit1==1 valid data)
function getModisFireMask(year){
  year = ee.Number(year);
  var col = MODIS_FIRE
    .filter(ee.Filter.calendarRange(year, year, 'year'))
    .select(['BurnDate','QA']);

  var has = col.size().gt(0);

  var yearly = ee.Image(ee.Algorithms.If(has,
    col.map(function(img){
      var bd = img.select('BurnDate').gt(0);      // burned this month
      var qa = img.select('QA');
      var ok = qa.bitwiseAnd(3).eq(3);            // bits (0..1) == 11b
      return bd.and(ok).toByte();
    }).max(),                                     // OR across months of the year
    ee.Image(0).toByte()
  ));

  return yearly.rename('modis_fire').unmask(0);
}

function getCombinedFireMask(year, country){
  var glad = getGladFireMask(year);
  var combined = ee.Image(ee.Algorithms.If(
    isBrazil(country), getMapBiomasFireMask(year).or(glad), getModisFireMask(year).or(glad)
  ));
  return combined.rename('combined_fire').toByte();
}

//============================================================================================
// 4) BUILD STACK 
//============================================================================================

// build one stacked image (px + ha) and do a single reduceRegion per year
function stackYearBands(y, country, forestMask, nonForestMask, geom){
  // clip early to ROI to reduce reprojection cost
  var fo = forestMask.clip(geom);
  var nf = nonForestMask.clip(geom);

  var mb = getMapBiomasFireMask(y).clip(geom);
  var gd = getGladFireMask(y).clip(geom);
  var md = getModisFireMask(y).clip(geom);
  var cb = getCombinedFireMask(y, country).clip(geom);

  var mb_fo = mb.updateMask(fo);
  var gd_fo = gd.updateMask(fo);
  var md_fo = md.updateMask(fo);
  var cb_fo = cb.updateMask(fo);

  var mb_nf = mb.updateMask(nf);
  var gd_nf = gd.updateMask(nf);
  var md_nf = md.updateMask(nf);
  var cb_nf = cb.updateMask(nf);

  var pxNames = [
    'mapbiomas_fire_fo_px','glad_fire_fo_px','modis_fire_fo_px','combined_fire_fo_px',
    'mapbiomas_fire_nf_px','glad_fire_nf_px','modis_fire_nf_px','combined_fire_nf_px'
  ];
  
  var pxImg = ee.Image.cat([mb_fo, gd_fo, md_fo, cb_fo, mb_nf, gd_nf, md_nf, cb_nf])
    .rename(pxNames).uint16();
 
  // Ensure float precision for area sums (avoids any integer reducer fallback).
  var haImg = pxImg.toFloat().multiply(HA)
    .rename(pxNames.map(function(n){ return n.replace('_px','_ha'); }));

  // Ensure empty bands reduce to 0 instead of null
  return pxImg.addBands(haImg).unmask(0);
}

//============================================================================================
// 5) TABLE BUILDER (per ROI)
//============================================================================================
function buildTableForArea(feat){
  feat = ee.Feature(feat);
  var areaId  = ee.String(feat.get('zone'));
  var country = ee.String(feat.get('country'));
  var geom    = ee.Geometry(feat.geometry());      // no transform here

  var rows = YEARS.map(function(y){
    y = ee.Number(y);
    var bandName      = ee.String('Dec').cat(y.format('%d'));
    var forestMask    = FOREST_MASK.select(bandName);
    var nonForestMask = NONFOREST_MASK.select(bandName);

    // Extra safety: keep zeros (no nulls) at reduction time
    var img = stackYearBands(y, country, forestMask, nonForestMask, geom).unmask(0);

    // One-output reducer over a multi-band image already returns one value per band (keys = band names).
    var dict = img.reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: geom,
      crs: PROJ,
      scale: SCALE,
      maxPixels: MAX_PIXELS,
      bestEffort: false,
      tileScale: TILE_SCALE
    });

    return ee.Feature(null, ee.Dictionary(dict).combine({
      area_id: areaId,
      year: y.int(),
      source_used: ee.String(ee.Algorithms.If(isBrazil(country), 'MB+GLAD', 'MODIS+GLAD'))
    }, true));
  });

  return ee.FeatureCollection(rows);
}


//============================================================================================
// 6) EXPORT — one CSV per area (Drive)
//============================================================================================

ROIS.evaluate(function(fc){
  fc.features.forEach(function(f){
    var areaId  = f.properties.zone;
    var country = f.properties.country;
    var table   = buildTableForArea(ee.Feature(f));

    print('✓ Queuing burned area CSV (fo & nf):', areaId, '(' + country + ')');

    // Lock a stable column order across tasks/ROIs.
    var SELECTORS = [
      'area_id','year',
      'mapbiomas_fire_fo_px','mapbiomas_fire_fo_ha',
      'glad_fire_fo_px','glad_fire_fo_ha',
      'modis_fire_fo_px','modis_fire_fo_ha',
      'combined_fire_fo_px','combined_fire_fo_ha',
      'mapbiomas_fire_nf_px','mapbiomas_fire_nf_ha',
      'glad_fire_nf_px','glad_fire_nf_ha',
      'modis_fire_nf_px','modis_fire_nf_ha',
      'combined_fire_nf_px','combined_fire_nf_ha',
      'source_used'
    ];
    
    Export.table.toDrive({
      collection    : table.select(SELECTORS, null, false),
      description   : areaId + '_burned_fo_nf_2001_2024',
      folder        : areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_burned_fo_nf_2001_2024',
      fileFormat    : 'CSV',
      selectors     : SELECTORS
    });
  });

  print('✓ All CSV export tasks have been queued.');
  print('• Brazil: MapBiomas Fire ∨ GLAD Fire (conf ≥ 2)');
  print('• Others: MODIS BA ∨ GLAD Fire (MODIS QA: bits 0&1 == 1; BurnDate>0)');
  print('• fo = TMF {1,2,4}; nf = TMF {3,6} (water=5 excluded).');
});