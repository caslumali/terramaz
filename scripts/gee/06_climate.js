//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: TerrAmaz – Climate CSV Exporter (Optimized & Robust) — MODIS LST (MYD11A2) + CHIRPS
 * Author: Lucas Lima
 *
 * Purpose:
 *   Export pixel-level climate metrics (temperature °C & precipitation mm) per TerrAmaz zone,
 *   for monthly and annual series (2003–2024). Outputs are shaped for robust boxplots in R.
 *
 * Key choices (robustness & performance):
 *   - Temperature: MODIS Aqua MYD11A2 (8-day, 1 km, Daytime) → long, stable time series.
 *   - Precipitation: CHIRPS Pentad (5-day) → monthly/annual sums (gauge-informed; long record).
 *   - TMF class: yearly TMF (DecYYYY), assigned via **median** in a **metric kernel (meters)**
 *                after reprojecting TMF to the MODIS LST grid; **skipMasked: true**.
 *   - Sampling: consistently on the **MODIS LST 1 km projection/scale** (sinusoidal).
 *   - Performance: cache TMF median per year; avoid unnecessary reprojects/clips; build
 *                  ImageCollections from lists explicitly (no fragile flatten); copy year/month
 *                  to feature properties after sample().
 *
 * Outputs (2 CSVs per zone, Drive folder "<AREA>_Terramaz_metrics"):
 *   • <AREA>_climate_annual_2003_2024.csv
 *   • <AREA>_climate_monthly_2003_2024.csv
 *
 * Columns:
 *   area_id, year, [month], tmf_class_median, tmf_class_label, pixel_lon, pixel_lat, temperature_c, precipitation_mm
 *
 * Inputs:
 *   • ROIs:       projects/terramaz/assets/bases/zones_terramaz   (props: 'zone','country')
 *   • TMF Annual: projects/JRC/TMF/v1_2024/AnnualChanges/SAM      (bands Dec1990..Dec2024)
 *   • MODIS LST:  MODIS/061/MYD11A2 (8-day, Daytime, 1 km)
 *   • CHIRPS:     UCSB-CHG/CHIRPS/PENTAD (5-day)
 *
 * Period: 2003–2024
 * TMF Classes: 1=Undisturbed, 2=Degraded, 3=Deforested, 4=Regrowth
 */
//============================================================================================
// 1) CONFIG & CONSTANTS
//============================================================================================
var ROIS_PATH = 'projects/terramaz/assets/bases/zones_terramaz';
var TMF_PATH  = 'projects/JRC/TMF/v1_2024/AnnualChanges/SAM';

var MODIS_LST = ee.ImageCollection('MODIS/061/MYD11A2');     // 8-day, Daytime, 1 km
var CHIRPS    = ee.ImageCollection('UCSB-CHG/CHIRPS/PENTAD'); // 5-day precip

var START_YEAR = 2003;
var END_YEAR   = 2024;
var YEARS      = ee.List.sequence(START_YEAR, END_YEAR);

var LST_PROJ  = MODIS_LST.first().select('LST_Day_1km').projection();
var LST_SCALE = LST_PROJ.nominalScale(); // ~1000 m

var TMF_KERNEL_METERS = 1000; // 1 km median window (meters)

var TMF_LABELS = ee.Dictionary({
  1: 'Undisturbed',
  2: 'Degraded',
  3: 'Deforested',
  4: 'Regrowth'
});

//============================================================================================
// 2) LOAD DATA
//============================================================================================
var ROIS = ee.FeatureCollection(ROIS_PATH);
var TMF  = ee.Image(TMF_PATH);

print('✓ ROIs:', ROIS.size(), '  ✓ TMF bands:', TMF.bandNames().size());
print('✓ Period:', START_YEAR, '-', END_YEAR);

//============================================================================================
// 3) HELPERS
//============================================================================================
// --- QA helpers for MYD11A2 Day LST -----------------------------------------
function bits(img, band, start, end) {
  var mask = ee.Number(1).leftShift(end - start + 1).subtract(1).int();
  return img.select(band).rightShift(start).bitwiseAnd(mask);
}

// Mode = 'balanced' (default) or 'strict'
function maskLST_Day(img, mode) {
  mode = mode || 'balanced';
  var lst   = img.select('LST_Day_1km');
  var qc    = img.select('QC_Day');

  // Valid range: 7500..65535 (scaled 0.02 K). Zero é fill -> invalida.
  var validRange = lst.gte(7500).and(lst.lte(65535));

  // QC bits (see product doc):
  var mandatory   = bits(img, 'QC_Day', 0, 1);  // 0 good, 1 unreliable, 2 cloud, 3 other
  var dataQual    = bits(img, 'QC_Day', 2, 3);  // 0 good, 1 other
  var emissErr    = bits(img, 'QC_Day', 4, 5);  // 0 <=0.01, 1 <=0.02, 2 <=0.04, 3 >0.04
  var lstErr      = bits(img, 'QC_Day', 6, 7);  // 0 <=1K, 1 <=2K, 2 <=3K, 3 >3K

  var qaMask;
  if (mode === 'strict') {
    qaMask = mandatory.eq(0)       // pixel produced, good quality
      .and(dataQual.eq(0))         // good data quality
      .and(emissErr.lte(1))        // emissivity error <= 0.02
      .and(lstErr.lte(0));         // LST error <= 1K
  } else { // balanced (recommended)
    qaMask = mandatory.lte(1)      // pixel produced (good OR unreliable)
      .and(dataQual.lte(1))        // keep 0 or 1
      .and(lstErr.lte(1));         // LST error <= 2K
  }

  return lst.updateMask(validRange.and(qaMask)).rename('LST_Day_1km');
}

// Function to convert Kevint to Celsius
function kelvinToCelsius(image) {
  return image.multiply(0.02).subtract(273.15);
}
function getTMFForYear(year) {
  var band = ee.String('Dec').cat(ee.Number(year).format('%d'));
  var has  = TMF.bandNames().contains(band);
  return ee.Image(ee.Algorithms.If(has, TMF.select(band), ee.Image().toByte()));
}
// Cache: TMF median per year on LST grid (byte band 'tmf_class_median')
var TMF_MEDIAN_CACHE = ee.Dictionary(
  YEARS.iterate(function(y, acc){
    y = ee.Number(y);
    acc = ee.Dictionary(acc);
    var tmfY = getTMFForYear(y).reproject({crs: LST_PROJ, scale: LST_SCALE});
    var tmfMed = tmfY.reduceNeighborhood({
      reducer: ee.Reducer.median(),
      kernel: ee.Kernel.circle({radius: TMF_KERNEL_METERS, units: 'meters'}),
      skipMasked: true
    }).rename('tmf_class_median').toByte();
    return acc.set(y.format(), tmfMed);
  }, ee.Dictionary({}))
);
function tmfMedianFor(year) {
  return ee.Image(TMF_MEDIAN_CACHE.get(ee.Number(year).format()));
}
function addPixelLonLatBands(img) {
  var ll = ee.Image.pixelLonLat().select(['longitude','latitude'], ['pixel_lon','pixel_lat']);
  return img.addBands(ll);
}

// CHIRPS monthly/annual (masked if empty). No client reproject needed; sampling enforces LST grid.
function chirpsMonthlySum(year, month) {
  var start = ee.Date.fromYMD(year, month, 1);
  var end   = start.advance(1, 'month');
  var col = CHIRPS.filterDate(start, end)
    .select('precipitation')
    .map(function(img){ return img.updateMask(img.gte(0)); });
  return ee.Image(ee.Algorithms.If(
    col.size().gt(0), col.sum().rename('precipitation_mm'), ee.Image().rename('precipitation_mm')
  ));
}
function chirpsAnnualSum(year) {
  var start = ee.Date.fromYMD(year, 1, 1);
  var end   = start.advance(1, 'year');
  var col   = CHIRPS.filterDate(start, end).select('precipitation');
  return ee.Image(ee.Algorithms.If(
    col.size().gt(0), col.sum().rename('precipitation_mm'), ee.Image().rename('precipitation_mm')
  ));
}

// LST monthly/annual °C (masked if empty)
function lstMonthlyMeanC(year, month) {
  var start = ee.Date.fromYMD(year, month, 1);
  var end   = start.advance(1, 'month');
  var col = MODIS_LST.filterDate(start, end)
    .map(function(img){ return maskLST_Day(img, 'balanced'); });  // or 'strict'
  return ee.Image(ee.Algorithms.If(
    col.size().gt(0),
    kelvinToCelsius(col.mean()).rename('temperature_c'),
    ee.Image().rename('temperature_c')
  ));
}
function lstAnnualMeanC(year) {
  var start = ee.Date.fromYMD(year, 1, 1);
  var end   = start.advance(1, 'year');
  var col = MODIS_LST.filterDate(start, end)
    .map(function(img){ return maskLST_Day(img, 'balanced'); });  // or 'strict'
  return ee.Image(ee.Algorithms.If(
    col.size().gt(0),
    kelvinToCelsius(col.mean()).rename('temperature_c'),
    ee.Image().rename('temperature_c')
  ));
}

//============================================================================================
// 4) COMBINED IMAGES (by year / year-month) — no clip; mask by TMF; add lon/lat
//============================================================================================
function processAnnualImage(year) {
  var lst    = lstAnnualMeanC(year);
  var chirps = chirpsAnnualSum(year);
  var tmfMed = tmfMedianFor(year);
  var img = ee.Image.cat([lst, chirps, tmfMed]);
  return addPixelLonLatBands(img)
    .set('year', ee.Number(year).int())
    .updateMask(tmfMed.gte(1).and(tmfMed.lte(4)));
}
function processMonthlyImage(year, month) {
  var lst    = lstMonthlyMeanC(year, month);
  var chirps = chirpsMonthlySum(year, month);
  var tmfMed = tmfMedianFor(year);
  var img = ee.Image.cat([lst, chirps, tmfMed]);
  return addPixelLonLatBands(img)
    .set({'year': ee.Number(year).int(), 'month': ee.Number(month).int()})
    .updateMask(tmfMed.gte(1).and(tmfMed.lte(4)));
}

//============================================================================================
// 5) BUILD IMAGECOLLECTIONS SAFELY (avoid fragile flatten on mixed lists)
//============================================================================================
// Annual: build ee.List of ee.Image, then ee.ImageCollection(list)
function annualImagesList(areaId) {
  return YEARS.map(function(y){
    return processAnnualImage(y).set('area_id', areaId);
  });
}
function monthlyImagesList(areaId) {
  // Build nested list and flatten it into a single ee.List of ee.Image
  var list = YEARS.iterate(function(y, acc){
    y = ee.Number(y);
    acc = ee.List(acc);
    var months = ee.List.sequence(1, 12);
    var perYear = months.map(function(m){
      return processMonthlyImage(y, m).set('area_id', areaId);
    });
    return acc.cat(perYear);
  }, ee.List([]));
  return ee.List(list);
}

//============================================================================================
// 6) TABLE BUILDERS (pixel-level sampling; copy year/month & labels to each feature)
//============================================================================================
function buildAnnualTableForArea(feat) {
  feat = ee.Feature(feat);
  var geom   = feat.geometry();
  var areaId = ee.String(feat.get('zone'));

  var imgs = ee.ImageCollection(annualImagesList(areaId));

  var samples = imgs.map(function(img){
    return img.sample({
      region: geom,
      projection: LST_PROJ,
      scale: LST_SCALE,
      geometries: false
    }).map(function(f){
      var cls = ee.Number(f.get('tmf_class_median'));
      return f.set({
        area_id: img.get('area_id'),
        year:    img.get('year'),                 // <-- ensure 'year' column
        tmf_class_label: TMF_LABELS.get(cls)
      });
    });
  }).flatten();

  return samples.select([
    'area_id','year','tmf_class_median','tmf_class_label',
    'pixel_lon','pixel_lat','temperature_c','precipitation_mm'
  ]);
}

function buildMonthlyTableForArea(feat) {
  feat = ee.Feature(feat);
  var geom   = feat.geometry();
  var areaId = ee.String(feat.get('zone'));

  var imgs = ee.ImageCollection(monthlyImagesList(areaId));

  var samples = imgs.map(function(img){
    return img.sample({
      region: geom,
      projection: LST_PROJ,
      scale: LST_SCALE,
      geometries: false
    }).map(function(f){
      var cls = ee.Number(f.get('tmf_class_median'));
      return f.set({
        area_id: img.get('area_id'),
        year:    img.get('year'),                 // <-- ensure 'year' column
        month:   img.get('month'),                // <-- ensure 'month' column
        tmf_class_label: TMF_LABELS.get(cls)
      });
    });
  }).flatten();

  return samples.select([
    'area_id','year','month','tmf_class_median','tmf_class_label',
    'pixel_lon','pixel_lat','temperature_c','precipitation_mm'
  ]);
}

//============================================================================================
// 7) EXPORTS
//============================================================================================
ROIS.evaluate(function(fc){
  fc.features.forEach(function(f){
    var areaId  = f.properties.zone;
    var country = f.properties.country;

    var annualTable  = buildAnnualTableForArea(ee.Feature(f));
    var monthlyTable = buildMonthlyTableForArea(ee.Feature(f));

    print('✓ Queuing ANNUAL:', areaId, '(' + country + ')');
    Export.table.toDrive({
      collection: annualTable,
      description: areaId + '_climate_annual_2003_2024',
      folder: areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_climate_annual_2003_2024',
      fileFormat: 'CSV',
      selectors: ['area_id','year','tmf_class_median','tmf_class_label','pixel_lon','pixel_lat','temperature_c','precipitation_mm']
    });

    print('✓ Queuing MONTHLY:', areaId, '(' + country + ')');
    Export.table.toDrive({
      collection: monthlyTable,
      description: areaId + '_climate_monthly_2003_2024',
      folder: areaId + '_Terramaz_metrics',
      fileNamePrefix: areaId + '_climate_monthly_2003_2024',
      fileFormat: 'CSV',
      selectors: ['area_id','year','month','tmf_class_median','tmf_class_label','pixel_lon','pixel_lat','temperature_c','precipitation_mm']
    });
  });

  print('✓ All export tasks queued.');
  print('• MODIS LST (MYD11A2) 8-day → monthly/annual means at 1 km; CHIRPS pentad → sums.');
  print('• TMF median (1 km radius, meters, skipMasked=true) cached per year on LST grid.');
  print('• Pixel-level sampling using LST projection/scale; explicit year/month copied to features.');
});