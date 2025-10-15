//============================================================================================
// TerrAmaz — Sankey Transitions (TMF + MapBiomas Amazonia C6)
//============================================================================================
/**
 * Purpose
 * -------
 * Export, por território, as transições entre classes para os pares de anos
 * específicos usados nos gráficos Sankey (ex.: 1991 → 2008 → 2024).
 *
 * Saída
 * -----
 * Um CSV por território contendo linhas:
 *   area_id, year_from, year_to, src, dst, src_label, dst_label, px_total, area_ha
 *
 * Notas
 * -----
 * - TMF (DecYYYY) cobre até 2024. MapBiomas Amazônia C6 vai até 2023, então os
 *   anos acima de 2023 reutilizam a classificação de 2023 para categorias não
 *   florestais.
 * - As combinações de anos são definidas em STAGE_YEARS (por território).
 */

//============================================================================================
// 1) CONFIGURATION
//============================================================================================
var YEAR_START = 1990;
var MB_LAST_YEAR = 2023;
var MAX_PIXELS = 1e13;

var STAGE_YEARS = ee.Dictionary({
  cotriguacu: ee.List([1990, 2008, 2023]),
  paragominas: ee.List([1990, 2008, 2023]),
  guaviare: ee.List([1990, 2016, 2023]),
  madre_de_dios: ee.List([1990, 2010, 2023])
});

var ROIS   = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');
var TMF_ACC = ee.Image('projects/JRC/TMF/v1_2024/AnnualChanges/SAM'); // bandas DecYYYY
var MB_AMZ  = ee.Image('projects/mapbiomas-public/assets/amazon/lulc/collection6/mapbiomas_collection60_integration_v1');
var MB_PROJ = MB_AMZ.projection();

//============================================================================================
// 2) CLASS LISTS (MapBiomas)
//============================================================================================
var MB_FOREST_TF = ee.List([3, 4]);
var MB_FOREST_FF = ee.List([6, 5]);
var MB_NATURAL_NF = ee.List([11,12,13,29,68]);
var MB_WATER = ee.List([33, 34]);
var MB_PASTURE   = ee.List([15]);
var MB_MOSAIC = ee.List([9]);
var MB_AGRICULTURE = ee.List([18,21,35]);
var MB_URBAN     = ee.List([24]);
var MB_MINING    = ee.List([30]);
var MB_OTHERS    = ee.List([23, 25]);

var MB_OTHER_LULC = MB_URBAN.cat(MB_OTHERS).cat(MB_WATER);
var MB_PREV_CONVERSION = MB_MOSAIC
  .cat(MB_AGRICULTURE)
  .cat(MB_PASTURE)
  .cat(MB_URBAN)
  .cat(MB_OTHERS)
  .cat(MB_MINING)
  .cat(MB_WATER);

var CLASS_DICT = ee.Dictionary({
  '1':'Undisturbed TF',
  '2':'Degraded TF',
  '3':'Regrowth TF',
  '4':'Undisturbed FF',
  '5':'Degraded FF',
'6':'Regrowth FF',
'7':'Other natural vegetation',
'8':'Agriculture',
'9':'Pasture',
'10':'Mining',
'11':'Other LULC',
'12':'Mosaic of uses'
});

//============================================================================================
// 3) HELPERS
//============================================================================================
function isOneOf(image, codes) {
  codes = ee.List(codes);
  var first = ee.Number(codes.get(0));
  var mask  = image.eq(first);
  var rest  = codes.slice(1);
  return ee.Image(rest.iterate(function(c, acc){
    return ee.Image(acc).or(image.eq(ee.Number(c)));
  }, mask));
}

function mbBand(year) {
  var y = ee.Number(year).min(MB_LAST_YEAR);
  var band = ee.String('classification_').cat(y.format('%d'));
  var hasBand = MB_AMZ.bandNames().indexOf(band).gte(0);
  return ee.Image(ee.Algorithms.If(hasBand, MB_AMZ.select(band), ee.Image(0).updateMask(ee.Image(0))));
}

function tmfBand(year){
  return TMF_ACC.select(ee.String('Dec').cat(ee.Number(year).format('%d')))
                .reproject(TMF_ACC.projection());
}

//============================================================================================
// 4) CLASS MAP POR ANO (codes 1..11)
//============================================================================================
function classImage(year){
  year = ee.Number(year);
  var TMF = tmfBand(year);
  var MBY = mbBand(year).reproject(MB_PROJ);

  var TMF_UND = TMF.eq(1);
  var TMF_DEG = TMF.eq(2);
  var TMF_REG = TMF.eq(4);

  var MB_TF    = isOneOf(MBY, MB_FOREST_TF).gt(0);
  var MB_FF    = isOneOf(MBY, MB_FOREST_FF).gt(0);
  var MB_natNF = isOneOf(MBY, MB_NATURAL_NF).gt(0);
  var MB_agri  = isOneOf(MBY, MB_AGRICULTURE).gt(0);
  var MB_past  = isOneOf(MBY, MB_PASTURE).gt(0);
  var MB_mining= isOneOf(MBY, MB_MINING).gt(0);
  var MB_other = isOneOf(MBY, MB_OTHER_LULC).gt(0);
  var MB_mosaic= isOneOf(MBY, MB_MOSAIC).gt(0);

  var MB_prev = ee.Image(ee.Algorithms.If(
    year.gt(YEAR_START),
    mbBand(year.subtract(1)).reproject(MB_PROJ),
    ee.Image(0).updateMask(ee.Image(0))
  ));
  var MB_prev_conversion = isOneOf(MB_prev, MB_PREV_CONVERSION).gt(0);

  var img = ee.Image(0);

  img = img.where(TMF_UND.and(MB_TF), 1)
           .where(TMF_DEG.and(MB_TF), 2)
           .where(TMF_REG.and(MB_TF), 3)
           .where(TMF_UND.and(MB_FF), 4)
           .where(TMF_DEG.and(MB_FF), 5)
           .where(TMF_REG.and(MB_FF), 6);

  var assigned = img.neq(0);
  var forestTF_left = MB_TF.and(assigned.not());
  var forestFF_left = MB_FF.and(assigned.not());

  img = img.where(forestTF_left.and(MB_prev_conversion), 3)
           .where(forestFF_left.and(MB_prev_conversion), 6);

  assigned = img.neq(0);
  img = img.where(MB_TF.and(assigned.not()), 2)
           .where(MB_FF.and(assigned.not()), 5);

  assigned = img.neq(0);
  img = img.where(MB_agri.and(assigned.not()), 8);

  assigned = img.neq(0);
  img = img.where(MB_past.and(assigned.not()), 9);

  assigned = img.neq(0);
  img = img.where(MB_natNF.and(assigned.not()), 7);

  assigned = img.neq(0);
  img = img.where(MB_mining.and(assigned.not()), 10);

  assigned = img.neq(0);
  img = img.where(MB_mosaic.and(assigned.not()), 12);

  assigned = img.neq(0);
  img = img.where(MB_other.and(assigned.not()), 11);

  img = img.where(img.eq(0), 11);

  return img.toInt16()
    .rename(ee.String('class_').cat(year.format('%d')))
    .reproject(TMF_ACC.projection());
}

//============================================================================================
// 5) TRANSITIONS PARA UM PAR DE ANOS
//============================================================================================
function transitionsForPair(yearFrom, yearTo, region, areaId){
  var y0 = ee.Number(yearFrom);
  var y1 = ee.Number(yearTo);

  var img0 = classImage(y0);
  var img1 = classImage(y1).reproject(img0.projection());

  var pair = img0.multiply(100).add(img1).rename('pair');
  var PRJ  = img0.projection();

  var ha = ee.Image.pixelArea().reproject(PRJ).divide(10000).rename('ha');
  var ones = ee.Image.constant(1).updateMask(pair.mask()).rename('ones');

  var imgHa = ee.Image.cat([ha, pair]);
  var imgPx = ee.Image.cat([ones, pair]);

  var rawHa = imgHa.reduceRegion({
    reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'pair'}),
    geometry: region,
    crs: PRJ.crs(),
    scale: PRJ.nominalScale(),
    maxPixels: MAX_PIXELS,
    tileScale: 2
  }).get('groups');
  var groupsHa = ee.List(ee.Algorithms.If(rawHa, rawHa, ee.List([])));

  var rawPx = imgPx.reduceRegion({
    reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'pair'}),
    geometry: region,
    crs: PRJ.crs(),
    scale: PRJ.nominalScale(),
    maxPixels: MAX_PIXELS,
    tileScale: 2
  }).get('groups');
  var groupsPx = ee.List(ee.Algorithms.If(rawPx, rawPx, ee.List([])));

  var pxDict = ee.Dictionary(groupsPx.iterate(function(item, acc){
    item = ee.Dictionary(item);
    return ee.Dictionary(acc).set(item.get('pair'), item.get('sum'));
  }, ee.Dictionary({})));

  var fc = ee.FeatureCollection(groupsHa.map(function(item){
    item = ee.Dictionary(item);
    var p = ee.Number(item.get('pair'));
    var area = ee.Number(item.get('sum'));
    var px = ee.Number(pxDict.get(p.format(), 0));

    var from = p.divide(100).floor();
    var to   = p.mod(100);

    return ee.Feature(null, {
      area_id   : areaId,
      year_from : y0,
      year_to   : y1,
      src       : from,
      dst       : to,
      src_label : ee.String(CLASS_DICT.get(from.format('%d'))),
      dst_label : ee.String(CLASS_DICT.get(to.format('%d'))),
      px_total  : px,
      area_ha   : area
    });
  }));

  return fc;
}

//============================================================================================
// 6) TRANSITIONS POR TERRITÓRIO
//============================================================================================
function sankeyForROI(feat){
  feat = ee.Feature(feat);
  var areaId = ee.String(feat.get('zone'));
  var years = ee.List(STAGE_YEARS.get(areaId));

  return ee.Algorithms.If(
    years,
    (function(){
      var geom = feat.geometry();
      var fromYears = years.slice(0, years.length().subtract(1));
      var toYears = years.slice(1);
      var pairs = fromYears.zip(toYears);

      var fcList = pairs.map(function(pair){
        pair = ee.List(pair);
        return transitionsForPair(ee.Number(pair.get(0)), ee.Number(pair.get(1)), geom, areaId);
      });

      return ee.FeatureCollection(fcList).flatten();
    })(),
    ee.FeatureCollection([])
  );
}

//============================================================================================
// 7) EXPORT
//============================================================================================
ROIS.evaluate(function(collection){
  (collection.features || []).forEach(function(feature){
    var areaId = feature.properties.zone;
    var table = sankeyForROI(ee.Feature(feature));
    Export.table.toDrive({
      collection    : table,
      description   : areaId + '_tmf_mb_transitions_custom',
      folder        : areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_tmf_mb_transitions_custom',
      fileFormat    : 'CSV'
    });
    print('⤳ Sankey export queued for', areaId);
  });
});
