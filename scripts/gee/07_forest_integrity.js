//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz – Forest Integrity Index (2024)
 * Author: Lucas Lima
 *
 * Purpose:
 * Compute an intra-class structural integrity index (0–1) for three macroclasses
 * (Intacte, Dégradée, Régénérée) using TMF 2024 sub-types and MSPA 2024
 * with small morphological width (EdgeWidth=1 px) and ecological edge via 120 m buffer.
 *
 * Key choices (robustness & performance):
 * • Scoring is strictly intra-class (no direct comparison across macroclasses).
 * • MSPA penalties are small (|Δ| < 0.1) to avoid overriding class steps (0.1).
 * • Ecological edge threshold (120 m) applied in GEE (not baked into MSPA).
 * • Exports split policies: scores as 'mean', macroclass as 'mode' (per-band).
 * • Single NoData for all bands: -99; zeros in score bands mean “valid pixel, not this class”.
 *
 * Outputs (per territory, Google Drive → "<AREA>\_Terramaz\_metrics"):
 * • <AREA>\_forest\_integrity\_2024.tif   // 4 bands (3 scores + macroclass)
 *    [1] score_intacte   (Float32, 0–1, NoData=-99; outside-class=0)
 *    [2] score_degradee  (Float32, 0–1, NoData=-99; outside-class=0)
 *    [3] score_regeneree (Float32, 0–1, NoData=-99; outside-class=0)
 *    [4] macroclass      (Float32 with values 10/20/30, NoData=-99)
 *
 *  PyramidingPolicy: scores='mean'; macroclass='mode'
 *
 * • <AREA>\_hist\_forest\_integrity\_2024.csv
 *    Columns: area_id, macroclass (10/20/30), score_bin, count_px, area_ha
 *
 * Inputs:
 * • ROIs     : projects/terramaz/assets/bases/zones\_terramaz          (props: 'zone','country')
 * • MSPA 2024: projects/terramaz/assets/bases/amazon\_mspa\_2024        (band b1; codes below; EPSG:4326)
 * • TMF TransitionMap Subtypes (2024): projects/JRC/TMF/v1\_2024/TransitionMapSubtypes/SAM
 * • TMF Annual Changes (1990..2024):    projects/JRC/TMF/v1\_2024/AnnualChanges/SAM
 *
 * \------------------------  Legend — TMF 2024 Sub-types (macroclass → base score)  ------------------------
 * Undisturbed
 *   value 10. Undisturbed Tropical Moist Forest (TMF) • Score : 100
 *   value 11. Bamboo-dominated forest • Score : 100
 *   value 12. Undisturbed mangroves • Score : 100
 * 
 * Degraded
 *   value 21. Degraded forest with short-duration disturbance (started before 2015) • Score : 100
 *   value 22. Degraded forest with short-duration disturbance (started in 2015-2023) • Score : 90
 *   value 23. Degraded forest with long-duration disturbance (started before 2015) • Score : 80
 *   value 24. Degraded forest with long-duration disturbance (started in 2015-2023) • Score : 70
 *   value 25. Degraded forest with 2/3 degradation periods (last degradation started before 2015) • Score : 60
 *   value 26. Degraded forest with 2/3 degradation periods (last degradation started in 2015-2023) • Score : 50
 * 
 * Regrowth
 *   value 31. Old forest regrowth (disturbed before 2005) • Score : 100
 *   value 32. Young forest regrowth (disturbed in 2005-2014) • Score : 90
 *   value 33. Very young forest regrowth (disturbed in 2015-2021) • Score : 80
 *
 * \-------------------------------  MSPA → penalty Δ (intra-class)  ---------------------------------------
 * Codes (MSPA IntExt=1): Core=17; Islet=9; Perforation=105; Edge=3; Loop=169; Bridge=33; Branch=1; (Background=2; Missing=0)
 * Penalties (Δ):
 * Core (17)                          →  0
 * Perforation (105); Loop (169)      → -3
 * Edge (3)                           → -6
 * Islet (9); Branch (1); Bridge (33) → -9
 * 
 * Notes:
 * • Background (2) and Missing (0) are masked (NoData=-99).
 * • Ecological edge (<120 m) is enforced in GEE when needed, not by MSPA EdgeWidth.
 *
 * Visualization:
 * • score\_intacte   → darkgreen ramp  (light to dark)
 * • score\_degradee  → green ramp      (light to dark)
 * • score\_regeneree → lightgreen ramp (light to dark)
 * • Use macroclass band for selection/area summaries; style maps with the three score bands.
**/
//============================================================================================
// 1) CONFIG & CONSTANTS
//============================================================================================
var ROIS_PATH = 'projects/terramaz/assets/bases/zones_terramaz';
var MSPA_PATH = 'projects/terramaz/assets/bases/amazon_mspa_2024';
var TMF_PATH  = 'projects/JRC/TMF/v1_2024/TransitionMap_Subtypes/SAM';

var SCALE = 30;                      // nominal working scale (m) for reducers/exports
var CRS   = 'EPSG:4326';             // storage CRS
var MAX_PIXELS  = 1e13;

// Metric projection for pixel-accurate buffering (units in meters)
var METRIC_CRS  = 'EPSG:3857';
var METRIC_PROJ = ee.Projection(METRIC_CRS).atScale(SCALE);
var PROJ  = ee.Projection(CRS).atScale(SCALE);

// Ecological edge distance (meters) and equivalent pixels at 30 m
var EDGE_DISTANCE = 120;
var BUFFER_PX = Math.max(1, Math.round(EDGE_DISTANCE / SCALE));   // ≈ 4 px (120/30)

var DEBUG = false;

//============================================================================================
// 2) LOAD DATA
//============================================================================================
var ROIS = ee.FeatureCollection(ROIS_PATH);
var MSPA = ee.Image(MSPA_PATH);
var TMF  = ee.Image(TMF_PATH);

// print('✓ ROIs:', ROIS, ROIS.size());
// print('✓ MSPA bands:', MSPA, MSPA.projection(), MSPA.bandNames());
// print('✓ TMF band(s):',TMF, TMF.projection(), TMF.bandNames());

//============================================================================================
// 3) HELPER: Simple RGB→hex for palettes
//============================================================================================
function rgb(r, g, b) {
  var bin = (r << 16) | (g << 8) | b;
  return ('000000' + bin.toString(16).toUpperCase()).slice(-6);
}

var vizUND = {min:0, max:100, palette:[
  rgb(0,80,0), rgb(5,90,5), rgb(10,100,10), rgb(10,95,40), rgb(10,90,60)
]};
var vizDEG = {min:0, max:100, palette:[
  rgb(30,120,0), rgb(55,135,0), rgb(80,150,0), rgb(90,155,20), rgb(100,160,40), rgb(110,165,40), rgb(120,170,40)
]};
var vizREG = {min:0, max:100, palette:[
  rgb(185,200,60), rgb(192,215,60), rgb(200,230,60), rgb(205,240,60), rgb(210,250,60)
]};

// MSPA palette
var vizMSPA = [
  rgb(0, 0, 0),         // background
  rgb(0, 204, 0),       // core
  rgb(204, 102, 0),     // islet
  rgb(0, 0, 255),       // perforation
  rgb(0, 0, 0),         // unused
  rgb(255, 255, 0),     // loop
  rgb(255, 0, 0),       // bridge
  rgb(255, 178, 102)    // branch
];

// Transition palette
var vizTRANSITIONS = [
  rgb(0,80,0), rgb(10,100,10), rgb(10,90,60), rgb(0,0,0),rgb(0,0,0), rgb(0,0,0), rgb(0,0,0),  rgb(0,0,0), rgb(0,0,0),  rgb(0,0,0), rgb(0,0,0),
  rgb(30,120,0), rgb(80,150,0), rgb(100,160,40), rgb(120,170,40), rgb(100,160,40), rgb(120,170,40), rgb(0,0,0), rgb(0,0,0),rgb(0,0,0),rgb(0,0,0),
  rgb(185,200,60),  rgb(200,230,60), rgb(210,250,60), rgb(0,0,0), rgb(0,0,0),  rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0),
  rgb(255,240,160), rgb(255,150,8), rgb(0,0,0), rgb(0,0,0),rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0),                       
  rgb(250,60,10), rgb(170,80,10), rgb(140,100,30),  rgb(140,120,60), rgb(0,0,0), rgb(0,0,0),rgb(0,0,0),  rgb(0,0,0),   rgb(0,0,0), rgb(0,0,0),
  rgb(40,100,50), rgb(80,150,0),rgb(200,230,60),rgb(210,250,60),rgb(255,230,110), rgb(255,60,10), rgb(155,105,70),rgb(0,0,0), rgb(0,0,0), rgb(0,0,0),
  rgb(0,50,150),rgb(0,150,200), rgb(0,160,150), rgb(0,210,210), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0),  rgb(0,0,0), rgb(0,0,0),  
  rgb(51,99,51),rgb(98,161,80), rgb(188,209,105), rgb(255,228,148), rgb(250,180,150), rgb(204,163,163), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), rgb(0,0,0), 
  rgb(255,255,255), rgb(237,255,215), rgb(224,250,157), rgb(214,250,188)
];


//============================================================================================
// 4) TMF → Macroclass & Base Score (0–1), Forest Mask
//============================================================================================
/**
 * Map TMF sub-type codes (10..33) to:
 *   - macroclass (10/20/30)
 *   - base score (0–1) used intra-class (no cross-class comparison)
 */
var TMF_CODES   = [10,11,12, 21,22,23,24,25,26, 31,32,33];
var BASE_SCORES = [100, 100, 100,   100,90,80,70,60,50,   100,90,80];

var tmfSubtype = TMF.rename('tmf_subtype');

var baseScore = tmfSubtype
  .remap(TMF_CODES, BASE_SCORES, -99)
  .rename('base_score');

var macroclass = ee.Image(0)
  .where(tmfSubtype.eq(10).or(tmfSubtype.eq(11)).or(tmfSubtype.eq(12)), 10)     // Intacte
  .where(tmfSubtype.gte(21).and(tmfSubtype.lte(26)), 20)                         // Dégradée
  .where(tmfSubtype.gte(31).and(tmfSubtype.lte(33)), 30)                         // Régénérée
  .updateMask(baseScore.neq(-99))
  .rename('macroclass');

// Forest mask (valid TMF 10..33)
var forestMask = baseScore.neq(-99);

//============================================================================================
// 5) MSPA penalties with 120 m ecological buffers (fastDistanceTransform + meters)
//    - Apply Δ within 120 m of Edge or Perforation/Loop morphologies
//    - Precedence: Edge > Perf/Loop > Small morphologies (local only)
//============================================================================================

/**
 * Morphology-based penalties (Δ), applied strictly intra-class:
 *   • Edge buffer (≤120 m):              -6
 *   • Perforation/Loop buffer (≤120 m):  -3
 *   • Islet/Branch/Bridge (local pixel): -9
 *   • Core (and others):                  0
 */

var mspa = MSPA.select(0).rename('mspa_code');

// === Binary masks for each relevant MSPA morphology ===
var isCore   = mspa.eq(17);
var isIslet  = mspa.eq(9);
var isPerf   = mspa.eq(105);
var isEdge   = mspa.eq(3);
var isLoop   = mspa.eq(169);
var isBridge = mspa.eq(33);
var isBranch = mspa.eq(1);

// ========== Euclidean Buffers using fastDistanceTransform ========== //
// Each fastDistanceTransform returns squared pixel distance.
// The first sqrt() converts it to linear pixel distance.
// Multiplying by sqrt(pixelArea) converts pixels to meters.
// Reproject to METRIC_PROJ to ensure correct scaling in meters.

var edgeDist = isEdge
  .fastDistanceTransform()                 // squared distance (in pixels²)
  .sqrt()                                  // → linear distance (pixels)
  .multiply(ee.Image.pixelArea().sqrt())   // → convert pixels to meters
  .reproject(METRIC_PROJ);                 // enforce meter-based projection

var perfLoopDist = isPerf.or(isLoop)
  .fastDistanceTransform()
  .sqrt()
  .multiply(ee.Image.pixelArea().sqrt())
  .reproject(METRIC_PROJ);

// Create masks for pixels within 120 m of each morphology
// These will receive penalties if they are also forest pixels
var distToEdge     = edgeDist.lte(EDGE_DISTANCE).and(forestMask);
var distToPerfLoop = perfLoopDist.lte(EDGE_DISTANCE).and(forestMask);

// Small morphologies are penalized only at the pixel level (no buffer)
var smallLocal = isIslet.or(isBranch).or(isBridge).and(forestMask);

// ========== Combine penalties with precedence logic ========== //
// Higher penalty overrides: Edge > Perf/Loop > Small morphologies
function penalize(mask, value) {
  return mask.multiply(value).int8().rename('delta');
}

// Use .min() → because values are negative, "min" selects the strongest penalty
var delta_mspa = ee.ImageCollection([
  penalize(distToEdge, -6),
  penalize(distToPerfLoop, -3),
  penalize(smallLocal, -9)
]).min().rename('delta_mspa').int8();


  
//============================================================================================
// 6) Final Score (Int8) and Per-class Bands (outside-class = 0; NoData = -99)
//============================================================================================
/**
 * score_final = clamp(base_score + Δ, 0, 100), integer only.
 * Output 4 bands:
 *   - score_und       (Int8, 0–100; 0 = outside class; -99 = NoData)
 *   - score_deg       (Int8, 0–100; 0 = outside class; -99 = NoData)
 *   - score_reg       (Int8, 0–100; 0 = outside class; -99 = NoData)
 *   - macroclass      (Int8, 10/20/30; -99 = NoData)
 */
var scoreFinal = baseScore
  .add(delta_mspa)
  .clamp(0, 100)
  .toInt8()
  .updateMask(forestMask)
  .rename('score_final');

var score_und = scoreFinal
  .where(macroclass.neq(10), 0)
  .rename('score_und');

var score_deg = scoreFinal
  .where(macroclass.neq(20), 0)
  .rename('score_deg');

var score_reg = scoreFinal
  .where(macroclass.neq(30), 0)
  .rename('score_reg');

var macroclassBand = macroclass
  .toInt8()
  .updateMask(forestMask)
  .rename('macroclass');

// Final 4-band image: all Int8, masked for NoData
var integrity4B = score_und
  .addBands([score_deg, score_reg, macroclassBand])
  .toInt8()
  .updateMask(forestMask);

//============================================================================================
// 7) Export per ROI — One GeoTIFF + Aggregated CSV per zone
//============================================================================================
/**
* For each ROI (via client-side loop):
*   - Export 4-band GeoTIFF (Int8, NoData = -99)
*   - Export aggregated CSV with [macroclass, TMF class, score] breakdown
*/

var ROIS_T = ROIS.map(function (f) { return f.transform(PROJ); });
var roiList = ROIS_T.toList(ROIS_T.size()).getInfo();

roiList.forEach(function (roiDict) {
  var feature = ee.Feature(roiDict);
  var zone = feature.get('zone');

  var areaIdStr = ee.String(zone);
  var folderStr = areaIdStr.cat('_Terramaz_metrics');
  var geom      = feature.geometry();

  // ----------------------
  // 1. GeoTIFF Export
  // ----------------------
  var imageOut = integrity4B.clip(geom).unmask(-99);
  Export.image.toDrive({
    image: imageOut,
    description: areaIdStr.getInfo() + '_forest_integrity_2024',
    folder: folderStr.getInfo(),
    fileNamePrefix: areaIdStr.getInfo() + '_forest_integrity_2024',
    region: geom,
    crs: CRS,
    scale: SCALE,
    maxPixels: MAX_PIXELS,
    fileFormat: 'GeoTIFF'
  });

  // ----------------------
  // 2. Aggregated CSV Export (score × classe × macroclasse)
  // ----------------------
  var stack = ee.Image.cat([
    ee.Image.pixelArea().divide(1e4).rename('area_ha'),
    scoreFinal.rename('score_bin'),
    tmfSubtype.rename('classe'),
    macroclass.rename('macroclasse')
  ]).updateMask(forestMask);

  var histDict = stack.reduceRegion({
    reducer: ee.Reducer.sum()
      .group({groupField: 1, groupName: 'score_bin'})
      .group({groupField: 2, groupName: 'classe'})
      .group({groupField: 3, groupName: 'macroclasse'}),
    geometry: geom,
    scale: SCALE,
    maxPixels: MAX_PIXELS,
    tileScale: 4,
    bestEffort: true
  });

  var features = ee.List(histDict.get('groups')).map(function (mc) {
    mc = ee.Dictionary(mc);
    var macro = mc.get('macroclasse');
    return ee.List(mc.get('groups')).map(function (cl) {
      cl = ee.Dictionary(cl);
      var classe = cl.get('classe');
      return ee.List(cl.get('groups')).map(function (sc) {
        sc = ee.Dictionary(sc);
        return ee.Feature(null, {
          area_id: areaIdStr,
          macroclasse: macro,
          classe: classe,
          score_final: sc.get('score_bin'),
          area_ha: sc.get('sum')
        });
      });
    }).flatten();
  }).flatten();

  var tableOut = ee.FeatureCollection(features);

  Export.table.toDrive({
    collection: tableOut,
    description: areaIdStr.getInfo() + '_hist_forest_integrity_2024',
    folder: folderStr.getInfo(),
    fileNamePrefix: areaIdStr.getInfo() + '_hist_forest_integrity_2024',
    fileFormat: 'CSV'
  });
});

//============================================================================================
// 8) Quick QA — On-map visualization (optional)
//============================================================================================
if (DEBUG) {
  Map.addLayer(score_reg.updateMask(score_reg.gt(0)).clip(ROIS), vizREG,'Regrowth (0–1)', false);
  Map.addLayer(score_deg.updateMask(score_deg.gt(0)).clip(ROIS), vizDEG,'Degraded (0–1)', false);
  Map.addLayer(score_und.updateMask(score_und.gt(0)).clip(ROIS), vizUND, 'Undisturbed (0–1)', false );
  Map.addLayer(macroclassBand.clip(ROIS), {min:10, max:30, palette:['darkgreen','lightgreen','yellow']}, 'macroclass (10/20/30)', false)

  //============================================================================================
  // 9) SHOW TMF SubTypes and MSPA raster
  //============================================================================================
  var TransitionMap = ee.Image('projects/JRC/TMF/v1_2024/TransitionMap_Subtypes/SAM');
  Map.addLayer(
    TransitionMap.updateMask(TransitionMap),
    {min:10, max:94, palette: vizTRANSITIONS},
    'JRC - Transition Map – Sub types - v1 2024',
    false
  );
  
  //============================================================================================
  // 10) MSPA Visualization — palette + legend (UI)
  //============================================================================================
  /**
   * Display MSPA (IntExt=1) with a clear palette and a compact legend.
   * Codes in your raster: Core=17; Islet=9; Perforation=105; Edge=3; Loop=169; Bridge=33; Branch=1
   * Background=2 and Missing=0 are masked out.
   */
  
  // -- 11.1 Build a display image that remaps MSPA codes to 1..7 for palette indexing
  var mspa_class = ee.Image.constant(0)
    .where(MSPA.eq(17), 1)  // Core
    .where(MSPA.eq(9), 2)   // Islet
    .where(MSPA.eq(105), 3) // Perforation
    .where(MSPA.eq(3), 4)   // Edge
    .where(MSPA.eq(169), 5) // Loop
    .where(MSPA.eq(33), 6)  // Bridge
    .where(MSPA.eq(1), 7);  // Branch
  
  // -- 11.3 Add MSPA layer (turned off by default for a clean map toggle)
  Map.addLayer(mspa_class.updateMask(mspa_class).clip(ROIS),{min: 0, max: 7, palette: vizMSPA, opacity: 0.6}, 'MSPA', true);
  
  // -- 11.4 Compact legend (UI) for MSPA
  var legendMspa = ui.Panel({style: {position: 'bottom-left', padding: '8px'}});
  legendMspa.add(ui.Label({
    value: 'MSPA (IntExt=1)',
    style: {fontWeight: 'bold', fontSize: '12px', margin: '0 0 6px 0'}
  }));
  
  // Helper to add a legend row
  function addLegendRow(color, name) {
    var colorBox = ui.Label({
      style: {backgroundColor: '#' + color, padding: '8px', margin: '0 6px 4px 0'}
    });
    var desc = ui.Label({value: name, style: {fontSize: '12px'}});
    legendMspa.add(ui.Panel([colorBox, desc], ui.Panel.Layout.Flow('horizontal'), {margin: '0 0 2px 0'}));
  }
  
  // Entries (order consistent with palette indexing above)
  addLegendRow(rgb(0, 204, 0),   'Core (17)');
  addLegendRow(rgb(204, 102, 0), 'Islet (9)');
  addLegendRow(rgb(0, 0, 255),   'Perforation (105)');
  addLegendRow(rgb(255, 0, 0),   'Edge (3)');
  addLegendRow(rgb(255, 255, 0), 'Loop (169)');
  addLegendRow(rgb(255, 0, 0),   'Bridge (33)');
  addLegendRow(rgb(255, 178, 102),'Branch (1)');
  
  Map.add(legendMspa);
  
  var styledROIS = ROIS.style({color: '000000', fillColor: '00000000', width: 2});
  
  Map.addLayer(styledROIS, {}, 'TerrAmaz');
  
  Map.addLayer(distToPerfLoop.updateMask(distToPerfLoop), {palette:['red'],opacity: 0.5}, 'buffer perfLoop zone');
  Map.addLayer(distToEdge.updateMask(distToEdge), {palette:['orange'], opacity: 0.5}, 'buffer perfEdge zone');
  Map.addLayer(perfLoopDist, {min:0, max: 200}, 'perfLoopDist (m)', false);
  
  
  // Map.centerObject(paragominas, 9);
  // Map.centerObject(ROIS, 5);
  // Map.setCenter(-58.7206, -9.4434, 9)
}