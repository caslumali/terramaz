//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz — Deforestation CSV Exporter (MB-ATBD on Amazon C6 + TMF + PRODES)
 * Purpose: Export one CSV per region with annual deforestation statistics from
 *          MapBiomas (ATBD rules, Amazon Collection 6), TMF, and PRODES.
 * Author: Lucas Lima
 *
 * OUTPUT:
 *   - One CSV file per input zone (ROIS).
 *   - One row per year in 1987–2024.
 *   - Columns:
 *       area_id, year,
 *       tmf_def_px_total, tmf_def_area_ha,
 *       mapbiomas_px_total, mapbiomas_area_ha,
 *       prodes_px_total, prodes_area_ha
 *
 * DATA SOURCES:
 *   - TMF DeforestationYear v2024 (first deforestation year): 1982–2024
 *   - MapBiomas Amazon Collection 6 (integration v1), ATBD deforestation logic:
 *       * Years 1987–2023: deforestation in year Y if Natural at Y-2 & Y-1,
 *         and Anthropic at Y & Y+1; exclude Not-Included (water/not-observed).
 *       * Per-year spatial filter = 5 pixels (~0.5 ha), applied yearly (no accumulation).
 *       * Year 2024 is NOT available in Amazon C6 ⇒ return NA (masked).
 *   - PRODES (Brazil only): deforestation year code D_Y = (Y - 2000), report for Y ≥ 2008.
 *
 * NOTES:
 *   - NA years are exported as nulls in CSV (masked rasters).
 *   - Areas/counts are computed on each dataset’s native projection for accuracy.
 */


//============================================================================================
// 1) CONFIG
//============================================================================================

// Years exported: 1987–2024 (MapBiomas NA in 2024; TMF/PRODES up to 2024)
var YEARS_ALL = ee.List.sequence(1987, 2024);

// MapBiomas ATBD (Amazon C6) availability and rules
var MB_MIN_STRICT   = 1987;  // first ATBD strict year (needs t-2 & t-1)
var MB_MAX_STRICT   = 2023;  // last strict year (needs t+1; 2024 not available in C6)
var MB_MIN_PATCH_PX = 5;     // per-year spatial filter: 5 pixels (~0.5 ha), no accumulation

// PRODES reporting start
var PRODES_MIN = 2008;

// Generic compute params
var MAX_PIXELS = 1e13;


//============================================================================================
// 2) LOAD DATA
//============================================================================================

// Input zones (must contain properties: 'zone' and 'country')
var ROIS      = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');

// TMF — Deforestation year (first event), South America mosaic
var TMF_DEF   = ee.Image('projects/JRC/TMF/v1_2024/DeforestationYear/SAM');

// MapBiomas — Amazon Collection 6 integration (valid for BR, PE, CO)
var MAPBIOMAS = ee.Image('projects/mapbiomas-public/assets/amazon/lulc/collection6/mapbiomas_collection60_integration_v1');
var MB_PROJ   = MAPBIOMAS.projection();  // ensure 30 m MB native grid for MB steps

// PRODES — raster with D_Y code = Y - 2000 (Brazil-only)
var PRODES    = ee.Image('projects/terramaz/assets/bases/prodes_2024');


//============================================================================================
// 3) UTILITY FUNCTIONS
//============================================================================================

/** Pixel count: sum of 1's in a binary mask within region, using the mask's native grid. */
function reduceCount(mask, region) {
  var b   = ee.String(mask.bandNames().get(0));
  var PRJ = mask.projection();
  var sum = mask.reduceRegion({
    reducer   : ee.Reducer.sum(),
    geometry  : region,
    crs       : PRJ.crs(),
    scale     : PRJ.nominalScale(),
    maxPixels : MAX_PIXELS,
    tileScale : 2
  }).get(b);
  return ee.Number(sum);
}

/** Area (ha): sum pixelArea over 1's in the mask, using the mask's native grid. */
function reduceAreaHa(mask, region) {
  var b   = ee.String(mask.bandNames().get(0));
  var PRJ = mask.projection();
  var ha  = ee.Image.pixelArea().reproject(PRJ).divide(10000);
  var sum = mask.multiply(ha).reduceRegion({
    reducer   : ee.Reducer.sum(),
    geometry  : region,
    crs       : PRJ.crs(),
    scale     : PRJ.nominalScale(),
    maxPixels : MAX_PIXELS,
    tileScale : 2
  }).get(b);
  return ee.Number(sum);
}

/** Null-to-zero for numeric outputs (keep NA when necessary by not wrapping). */
function nz(x) { return ee.Number(ee.Algorithms.If(x, x, 0)); }

/** Membership test helper: returns binary image for img ∈ codes. */
function isOneOf(img, codes) {
  var first = ee.Number(codes.get(0));
  var mask = img.eq(first);
  var rest = ee.List(codes.slice(1));
  return ee.Image(rest.iterate(function(c, acc) {
    return ee.Image(acc).or(img.eq(ee.Number(c)));
  }, mask));
}

/** Per-year patch filter (8-neighborhood) with minimum size in pixels (MapBiomas only). */
function mbApplyMinPatchPx(binMask, minPx) {
  binMask = ee.Image(binMask).selfMask().reproject(MB_PROJ);
  var cpc = binMask.connectedPixelCount(1024, true); // 8-neighborhood
  return binMask.updateMask(cpc.gte(minPx));
}

/** Aggregate MB classes to ATBD groups for Amazon C6: Anthropic=1, Natural=2, NotIncluded=7. */
function aggregateToATBDGroups(imgY) {
  // Amazon C6 class sets (adjust if your legend diverges)
  var A  = ee.List([9,14,15,18,21,24,25,30,35]);        // Anthropic
  var N  = ee.List([3,4,5,6,10,11,12,13,29,23,68]);     // Natural
  var NI = ee.List([27,33,34]);                         // Not included: Not Observed, Water, Aquaculture

  var isA  = isOneOf(imgY, A);
  var isN  = isOneOf(imgY, N);
  var isNI = isOneOf(imgY, NI);

  return ee.Image(0)
    .where(isA, 1)
    .where(isN, 2)
    .where(isNI, 7)
    .updateMask(isA.or(isN).or(isNI))
    .rename('g')
    .reproject(MB_PROJ);
}


//============================================================================================
// 4) YEARLY EVENT MASKS
//============================================================================================

/** 4.1 TMF — Deforestation if first deforestation year equals Y (valid 1982–2024). */
function tmfDefMaskForYear(y) {
  y = ee.Number(y);
  var computable = y.gte(1982).and(y.lte(2024));
  var event = TMF_DEF.eq(y);
  event = ee.Image(ee.Algorithms.If(
    computable, event, ee.Image(0).updateMask(ee.Image(0))
  ));
  return event.rename(ee.String('tmf_def_').cat(y.format('%d'))).selfMask();
}

/** 4.2 MapBiomas (Amazon C6, ATBD):
 *     Strict ATBD for Y in 1987–2023:
 *       - Natural at Y-2 and Y-1; Anthropic at Y and Y+1; none of these years is NotIncluded.
 *       - Per-year spatial filter: 5 pixels (~0.5 ha), no accumulation.
 *     Year 2024: NOT available in Amazon C6 ⇒ NA (masked).
 */
function mbDefMaskStrictForYear(y) {
  y = ee.Number(y);

  var computable = y.gte(MB_MIN_STRICT).and(y.lte(MB_MAX_STRICT));

  var b0 = ee.String('classification_').cat(y.subtract(2).format('%d'));
  var b1 = ee.String('classification_').cat(y.subtract(1).format('%d'));
  var b2 = ee.String('classification_').cat(y.format('%d'));
  var b3 = ee.String('classification_').cat(y.add(1).format('%d'));

  // Use indexOf(...).gte(0) — returns Boolean; avoid List.contains (Number)
  var has0 = MAPBIOMAS.bandNames().indexOf(b0).gte(0);
  var has1 = MAPBIOMAS.bandNames().indexOf(b1).gte(0);
  var has2 = MAPBIOMAS.bandNames().indexOf(b2).gte(0);
  var has3 = MAPBIOMAS.bandNames().indexOf(b3).gte(0);

  var i0 = ee.Image(ee.Algorithms.If(has0, MAPBIOMAS.select(b0), ee.Image(0)));
  var i1 = ee.Image(ee.Algorithms.If(has1, MAPBIOMAS.select(b1), ee.Image(0)));
  var i2 = ee.Image(ee.Algorithms.If(has2, MAPBIOMAS.select(b2), ee.Image(0)));
  var i3 = ee.Image(ee.Algorithms.If(has3, MAPBIOMAS.select(b3), ee.Image(0)));

  var g0 = aggregateToATBDGroups(i0);
  var g1 = aggregateToATBDGroups(i1);
  var g2 = aggregateToATBDGroups(i2);
  var g3 = aggregateToATBDGroups(i3);

  var isN  = function(g){ return g.eq(2); };
  var isA  = function(g){ return g.eq(1); };
  var isNI = function(g){ return g.eq(7); };

  var base = isN(g0).and(isN(g1)).and(isA(g2)).and(isA(g3))
    .and(isNI(g0).not()).and(isNI(g1).not()).and(isNI(g2).not()).and(isNI(g3).not());

  // base = N(t-2)&N(t-1)&A(t)&A(t+1) (your strict rule)
  var lastYear = y.eq(2023);
  // fallback when there is no t+1: N(t-2)&N(t-1)&A(t), excluding NI
  var baseLast = isN(g0).and(isN(g1)).and(isA(g2))
    .and(isNI(g0).not()).and(isNI(g1).not()).and(isNI(g2).not());
  
  // choose which to use
  var chosen = ee.Image(ee.Algorithms.If(lastYear, baseLast, base));
  
  // keep only within computable Amazonia window
  var def = ee.Image(ee.Algorithms.If(
    computable, chosen, ee.Image(0).updateMask(ee.Image(0))
  ));
  
  // Apply ATBD spatial filter: Amazonia = 5 px (~0.5 ha)
  def = mbApplyMinPatchPx(def, MB_MIN_PATCH_PX);
  return def.rename(ee.String('mb_def_').cat(y.format('%d'))).selfMask();
}

/** Wrapper: returns MB deforestation mask for year Y.
 *  - 1987–2023: strict ATBD
 *  - 2024: NA (Amazon C6 has no 2024)
 */
function mbDefMaskForYear(y) {
  y = ee.Number(y);
  return ee.Image(ee.Algorithms.If(
    y.lte(MB_MAX_STRICT),
    mbDefMaskStrictForYear(y),
    ee.Image(0).updateMask(ee.Image(0))  // NA for 2024
  ));
}

/** 4.3 PRODES — Brazil-only, Y >= 2008; deforestation if code == (Y - 2000). */
function prodesMaskForYear(y, country) {
  y = ee.Number(y);
  country = ee.String(ee.Algorithms.If(country, country, ''));

  var isBR   = ee.String(country).trim().toUpperCase().compareTo('BR').eq(0);
  var usable = isBR.and(y.gte(PRODES_MIN));
  var codeD  = y.subtract(2000);

  var validD = PRODES.gte(8).and(PRODES.lte(24));
  var maskDyr = validD.and(PRODES.eq(codeD));

  var name = ee.String('pd_def_').cat(y.format('%d'));
  return ee.Image(ee.Algorithms.If(
    usable, maskDyr, ee.Image(0).updateMask(ee.Image(0))
  )).rename(name).selfMask();
}

//============================================================================================
// 5) BUILD TABLE PER AREA
//============================================================================================
function buildTableForArea(feat) {
  feat = ee.Feature(feat);
  
  var gMB  = feat.geometry().transform(MAPBIOMAS.projection(), 1);
  var gTMF = feat.geometry().transform(TMF_DEF.projection(), 1);
  var gPD  = feat.geometry().transform(PRODES.projection(), 1);
  
  var areaId  = ee.String(feat.get('zone'));
  var country = ee.String(feat.get('country'));

  var rows = YEARS_ALL.map(function(y) {
    y = ee.Number(y);

    // TMF
    var tmf = tmfDefMaskForYear(y);
    var tmfDefPx = nz(reduceCount(tmf, gTMF));
    var tmfDefHa = nz(reduceAreaHa(tmf, gTMF));

    // MapBiomas (ATBD on Amazon C6) — NA in 2024
    var mb = mbDefMaskForYear(y);
    var mbPx = reduceCount(mb, gMB);     // leave null for NA (masked)
    var mbHa = reduceAreaHa(mb, gMB);

    // PRODES — Brazil-only, Y >= 2008
    var pd = prodesMaskForYear(y, country);
    var pdPx = nz(reduceCount(pd, gPD));
    var pdHa = nz(reduceAreaHa(pd, gPD));

    return ee.Feature(null, {
      area_id           : areaId,
      year              : y,
      tmf_def_px_total  : tmfDefPx,
      tmf_def_area_ha   : tmfDefHa,
      mapbiomas_px_total: mbPx,
      mapbiomas_area_ha : mbHa,
      prodes_px_total   : pdPx,
      prodes_area_ha    : pdHa
    });
  });

  return ee.FeatureCollection(rows);
}


//============================================================================================
// 6) EXPORT — one CSV per area
//============================================================================================
ROIS.evaluate(function(fc) {
  var feats = fc.features;
  feats.forEach(function(f) {
    var areaId = f.properties.zone;
    var table  = buildTableForArea(ee.Feature(f));

    print('✓ Task queued for area:', areaId);

    Export.table.toDrive({
      collection    : table,
      description   : areaId + '_deforestation_all_datasets_1987_2024',
      folder        : areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_deforestation_all_datasets_1987_2024',
      fileFormat    : 'CSV'
    });
  });
  print('✓ All export tasks have been queued.');
});
