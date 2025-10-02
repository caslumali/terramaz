//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz — Annual Sankey Exporter (TMF + MapBiomas Amazonia C6)
 * Purpose: Export per-region CSVs with year→year land-cover transitions (flows) to build Sankey.
 * Author: Lucas Lima
 *
 * OUTPUT:
 *   - One CSV per input zone (ROIS) + optional ALL_ROIS merged CSV.
 *   - One row per transition pair Y→Y+1 in 1990→1991 … 2022→2023.
 *   - Columns:
 *       area_id, year_from, year_to,
 *       from_code, to_code, from_name, to_name,
 *       area_ha
 *
 * 13-CLASS LEGEND (codes):
 *   1–3  Terra Firme (Undisturbed, Degraded, Regrowth)
 *   4–6  Flooded Forest (Undisturbed, Degraded, Regrowth)
 *   7    Natural Non-Forest
 *   8    Pasture
 *   9    Mosaic/Agriculture (incl. perennials, oil palm)
 *   10   Mining
 *   11   Urban
 *   12   Others (anthropic/non-veg catch-all)
 *   13   Rivers/Lakes
 *
 * DATA SOURCES:
 *   - TMF Annual Changes v2024 (bands Dec1990..Dec2024): use classes 1=Undisturbed, 2=Degraded, 4=Regrowth.
 *   - MapBiomas Amazonia Collection 6 (integration v1, per-year to 2023):
 *       TF/FF split and non-forest buckets (Pasture, Mosaic/Agric, Urban, Mining, Others, Water, Natural NF).
 *
 * METHOD (per year Y):
 *   1) Build class map (codes 1..13) from TMF DecY (forest condition U/D/R) × MB(Y) masks (TF vs FF and non-forest).
 *   2) For each pair Y→Y+1, intersect class(Y) × class(Y+1) and sum area (ha) by (from_code, to_code) within each ROI.
 *   3) Export rows with names via lookup table.
 *
 * NOTES:
 *   - Transition range ends at 2023 because MB Amazonia C6 ends in 2023.
 *   - Areas computed on each image’s native projection; NA/missing years remain masked (not exported).
 */


//============================================================================================
// 1) CONFIG
//============================================================================================
var YEAR_START   = 1990;
var YEAR_END     = 2023;   // inclusive (flows are YEAR→YEAR+1 up to 2023)
var MAX_PIXELS   = 1e13;

var ROIS = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');

var TMF_ACC = ee.Image('projects/JRC/TMF/v1_2024/AnnualChanges/SAM'); // Dec1990..Dec2024
var MB_AMZ      = ee.Image('projects/mapbiomas-public/assets/amazon/lulc/collection6/mapbiomas_collection60_integration_v1');
var MB_PROJ = MB_AMZ.projection();

//============================================================================================
// 2) CLASS LISTS (MapBiomas Amazonia C6)
//============================================================================================
// Forest types in MB (to split TF vs FF)
var MB_FOREST_TF = ee.List([3, 4]);
var MB_FOREST_FF = ee.List([6, 5]);

// Natural Non-Forest
var MB_NATURAL_NF = ee.List([11,12,13,29,68]);

// Water
var MB_WATER = ee.List([33, 34]);

// Anthropic buckets
var MB_PASTURE   = ee.List([15]);
var MB_MOSAIC_AG = ee.List([9,18,21,35]);
var MB_URBAN     = ee.List([24]);
var MB_MINING    = ee.List([30]);

// Others (catch-all anthro/non-veg)
var MB_OTHERS = ee.List([23, 25]);

//============================================================================================
// 3) HELPERS
//============================================================================================
function isOneOf(img, codes) {
  codes = ee.List(codes);
  var first = ee.Number(codes.get(0));
  var mask  = img.eq(first);
  var rest  = codes.slice(1);
  return ee.Image(rest.iterate(function(c, acc){
    return ee.Image(acc).or(img.eq(ee.Number(c)));
  }, mask));
}

// MB band by year (returns fully masked if band missing)
function mbBand(y) {
  var b = ee.String('classification_').cat(ee.Number(y).format('%d'));
  var has = MB_AMZ.bandNames().indexOf(b).gte(0);
  return ee.Image(ee.Algorithms.If(has, MB_AMZ.select(b), ee.Image(0).updateMask(ee.Image(0))));
}

// Area reducer (ha) — sums masked pixels area in region
function reduceAreaHa(mask, region){
  var b   = ee.String(mask.bandNames().get(0));
  var PRJ = mask.projection();
  var ha  = ee.Image.pixelArea().reproject(PRJ).divide(10000);
  var sum = mask.multiply(ha).reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: region,
    crs: PRJ.crs(),
    scale: PRJ.nominalScale(),
    maxPixels: MAX_PIXELS,
    tileScale: 2
  }).get(b);
  return ee.Number(sum);
}

// Class names dictionary
var CLASS_DICT = ee.Dictionary({
  '1':'Undisturbed TF',
  '2':'Degraded TF',
  '3':'Regrowth TF',
  '4':'Undisturbed FF',
  '5':'Degraded FF',
  '6':'Regrowth FF',
  '7':'Natural NF',
  '8':'Pasture',
  '9':'Agriculture',
 '10':'Mining',
 '11':'Urban',
 '12':'Others',
 '13':'Water'
});

//============================================================================================
// 4) PER-YEAR CLASS MAP (1..13) — TMF DecY + MB Y
//============================================================================================
function tmfBand(y){
  return TMF_ACC.select(ee.String('Dec').cat(ee.Number(y).format('%d')))
                .reproject(TMF_ACC.projection());
}

function classImage(y){
  var TMF = tmfBand(y);
  var TMF_UND = TMF.eq(1).selfMask();
  var TMF_DEG = TMF.eq(2).selfMask();
  var TMF_REG = TMF.eq(4).selfMask();

  var MBY = mbBand(y).reproject(MB_PROJ);

  var MB_FF    = isOneOf(MBY, MB_FOREST_FF).selfMask();
  var MB_TF    = isOneOf(MBY, MB_FOREST_TF).selfMask();
  var MB_natNF = isOneOf(MBY, MB_NATURAL_NF).selfMask();
  var MB_water = isOneOf(MBY, MB_WATER).selfMask();
  var MB_past  = isOneOf(MBY, MB_PASTURE).selfMask();
  var MB_mosag = isOneOf(MBY, MB_MOSAIC_AG).selfMask();
  var MB_urban = isOneOf(MBY, MB_URBAN).selfMask();
  var MB_mining= isOneOf(MBY, MB_MINING).selfMask();
  var MB_others= isOneOf(MBY, MB_OTHERS).selfMask();

  var UND_TF = TMF_UND.updateMask(MB_TF);
  var DEG_TF = TMF_DEG.updateMask(MB_TF);
  var REG_TF = TMF_REG.updateMask(MB_TF);

  var UND_FF = TMF_UND.updateMask(MB_FF);
  var DEG_FF = TMF_DEG.updateMask(MB_FF);
  var REG_FF = TMF_REG.updateMask(MB_FF);

  var img = ee.Image(0)
    .where(UND_TF,    1)
    .where(DEG_TF,    2)
    .where(REG_TF,    3)
    .where(UND_FF,    4)
    .where(DEG_FF,    5)
    .where(REG_FF,    6)
    .where(MB_natNF,  7)
    .where(MB_past,   8)
    .where(MB_mosag,  9)
    .where(MB_mining, 10)
    .where(MB_urban,  11)
    .where(MB_others, 12)
    .where(MB_water,  13)
    .updateMask(ee.Image(1))
    
    return img
      .updateMask(img.neq(0))
      .rename(ee.String('class_').cat(ee.Number(y).format('%d'))) 
      .reproject(TMF_ACC.projection());   
}


//============================================================================================
// 5) TRANSITIONS year→year+1 PER ROI (area ha)
//============================================================================================
var YEARS = ee.List.sequence(YEAR_START, YEAR_END);
var PAIRS = ee.List.sequence(YEAR_START, YEAR_END - 1);

// sum(ha) grouped by “pair code” (from*100 + to)
function transitionsForYears(y0, y1, region, areaId){
  y0 = ee.Number(y0); y1 = ee.Number(y1);
  var img0 = classImage(y0);
  var img1 = classImage(y1).reproject(img0.projection());

  var pair = img0.multiply(100).add(img1).rename('pair'); // 101..1313
  var PRJ  = img0.projection();

  var ha   = ee.Image.pixelArea().reproject(PRJ).divide(10000).rename('ha');
  var ones = ee.Image.constant(1).rename('ones').updateMask(pair.mask());

  var imgHa = ee.Image.cat([ha, pair]);
  var imgPx = ee.Image.cat([ones, pair]);

  var groupsHa = imgHa.reduceRegion({
    reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'pair'}),
    geometry: region, crs: PRJ.crs(), scale: PRJ.nominalScale(),
    maxPixels: MAX_PIXELS, tileScale: 2
  }).get('groups');

  var groupsPx = imgPx.reduceRegion({
    reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'pair'}),
    geometry: region, crs: PRJ.crs(), scale: PRJ.nominalScale(),
    maxPixels: MAX_PIXELS, tileScale: 2
  }).get('groups');

  var listHa = ee.List(ee.Algorithms.If(groupsHa, groupsHa, ee.List([])));
  var listPx = ee.List(ee.Algorithms.If(groupsPx, groupsPx, ee.List([])));

  var pxDict = ee.Dictionary(
    listPx.iterate(function(item, acc){
      item = ee.Dictionary(item);
      var k = item.get('pair');
      var v = item.get('sum'); // not 'count'
      return ee.Dictionary(acc).set(k, v);
    }, ee.Dictionary({}))
  );

  var fc = ee.FeatureCollection(listHa.map(function(d){
    d = ee.Dictionary(d);
    var p   = ee.Number(d.get('pair'));
    var ah  = ee.Number(d.get('sum'));
    var px  = ee.Number(pxDict.get(p.format(), 0));

    var from = p.divide(100).floor();
    var to   = p.mod(100);

    return ee.Feature(null, {
      area_id   : areaId,
      year      : y1,
      src       : from,
      dst       : to,
      src_label : ee.String(CLASS_DICT.get(from.format('%d'))),
      dst_label : ee.String(CLASS_DICT.get(to.format('%d'))),
      px_total  : px,
      area_ha   : ah
    });
  }));

  return fc;
}

function sankeyForROI(feat){
  feat = ee.Feature(feat);
  var geom   = feat.geometry();
  var areaId = ee.String(feat.get('zone'));
  var fcList = PAIRS.map(function(y0){
    y0 = ee.Number(y0);
    return transitionsForYears(y0, y0.add(1), geom, areaId);
  });
  return ee.FeatureCollection(fcList).flatten();
}

//============================================================================================
// 6) EXPORTS
//============================================================================================
// One CSV per ROI with all annual transitions (1990→1991 … 2022→2023)
ROIS.evaluate(function(fc){
  var allFCs = [];
  (fc.features || []).forEach(function(f){
    var areaId = f.properties.zone;
    var table  = sankeyForROI(ee.Feature(f));
    allFCs.push(table);
    Export.table.toDrive({
      collection    : table,
      description   : areaId + '_tmf_mb_transitions_1990_2023',
      folder        : areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_tmf_mb_transitions_1990_2023',
      fileFormat    : 'CSV'
    });
    print('✓ Sankey queued:', areaId);
  });
});

//============================================================================================
// 7) OPTIONAL — QUICK MAP FOR A GIVEN YEAR
//============================================================================================
var y = 2023;
Map.addLayer(classImage(y), {min:1, max:13, palette:[
  '#1a7f1a','#7fbf3f','#d1f57a',  // 1..3  TF
  '#145c3d','#4aa382','#9be3c3',  // 4..6  FF
  '#c2e699',                      // 7    Natural NF
  '#ffd37f','#ffb347',            // 8,9  Pasture, Mosaic/Agric
  '#b2182b',                      // 10   Mining
  '#d9d9d9',                      // 11   Urban
  '#2166ac',                      // 12   Others
  '#8c510a'                       // 13   Rivers/Lakes
]}, 'Classes '+y, false);
