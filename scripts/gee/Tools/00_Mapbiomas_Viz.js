//============================================================================================
// 0) SCRIPT OVERVIEW
//============================================================================================
/**
 * Title: MapBiomas Amazônia C6 — Visualização sincronizada (código bruto = Inspector)
 * Author: Lucas Lima (layout), ajustes finais
 *
 * Goal:
 *   • Show MapBiomas classes with stable colors
 *   • Keep raw raster values untouched so the Inspector shows the true class code
 *   • Dynamic legend (optionally filtered by AOI)
 *
 * Key idea:
 *   • Add TWO layers from the SAME image:
 *       (1) Bottom: colorized copy (indexed for palette)  → visual styling only
 *       (2) Top   : raw class band (no viz params)       → Inspector reads true codes
 */

//============================================================================================
// 1) CONFIG
//============================================================================================
var LABEL_LANG   = 'pt';         // 'pt' | 'es' | 'en'
var DEFAULT_YEAR = 2015;         // Initial year
var YEARS = []; for (var y = 1985; y <= 2023; y++) YEARS.push(String(y));

// Optional AOI to filter legend to present classes (null = list all)
var AOI = null;  // e.g.: ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz').filter(ee.Filter.eq('zone','cotriguacu')).geometry();

// Data source
var MAPBIOMAS = ee.Image('projects/mapbiomas-public/assets/amazon/lulc/collection6/mapbiomas_collection60_integration_v1');

// Safety check
var CHECK_BAND = 'classification_' + DEFAULT_YEAR;
if (!MAPBIOMAS.bandNames().contains(CHECK_BAND)) {
  throw new Error('Missing band ' + CHECK_BAND + ' in MapBiomas C6 asset. Check the path.');
}

//============================================================================================
// 2) CLASS DICTIONARY (raw code → label + color)
//    IMPORTANT: Do NOT include macro code 1 (it is not a raster value).
//    Include 10, 22, 26 (these are actual raster codes that appear frequently).
//============================================================================================
var MB_CLASSES = [
  // --- Forest (raw codes) ---
  {code: 3,  label_pt: 'Formação florestal',                  label_es: 'Formación forestal',         label_en: 'Forest formation',         color: '#1f8d49'},
  {code: 4,  label_pt: 'Formação savânica / Floresta aberta', label_es: 'Formación sabánica/abierta', label_en: 'Savanna / Open forest',    color: '#7dc975'},
  {code: 5,  label_pt: 'Mangue',                              label_es: 'Manglar',                     label_en: 'Mangrove',                 color: '#04381d'},
  {code: 6,  label_pt: 'Floresta alagável',                   label_es: 'Bosque inundable',            label_en: 'Flooded forest',           color: '#026975'},

  // --- Non-forest natural formation ---
  {code: 11, label_pt: 'Campo alagado / Pantanosa',           label_es: 'Inundable / Pantanosa',       label_en: 'Wetland',                  color: '#519799'},
  {code: 12, label_pt: 'Formação campestre',                  label_es: 'Formación campestre',         label_en: 'Grassland',                color: '#d6bc74'},
  {code: 13, label_pt: 'Outra formação natural não florestal',label_es: 'Otra formación natural no forestal', label_en: 'Other non-forest natural', color: '#d89f5c'},
  {code: 29, label_pt: 'Afloramento rochoso',                 label_es: 'Afloramiento rocoso',         label_en: 'Rocky outcrop',            color: '#ffaa5f'},

  // --- Farming & silviculture ---
  {code: 9,  label_pt: 'Silvicultura',                        label_es: 'Silvicultura',                label_en: 'Silviculture',             color: '#7a5900'},
  {code: 15, label_pt: 'Pastagem',                            label_es: 'Pasto',                       label_en: 'Pasture',                  color: '#edde8e'},
  {code: 18, label_pt: 'Agricultura',                         label_es: 'Agricultura',                 label_en: 'Agriculture',              color: '#E974ED'},
  {code: 21, label_pt: 'Mosaico agri/pasto',                  label_es: 'Mosaico agricultura/pastos',  label_en: 'Mosaic of uses',           color: '#ffefc3'},
  {code: 35, label_pt: 'Palma (óleo)',                        label_es: 'Palma aceitera',              label_en: 'Oil palm',                 color: '#9065d0'},

  // --- Non-vegetated area ---
  {code: 23, label_pt: 'Praia / duna / areal',                label_es: 'Playa / duna / areal',        label_en: 'Beach / dune / sand',      color: '#ffa07a'},
  {code: 24, label_pt: 'Área urbanizada',                     label_es: 'Infraestructura urbana',      label_en: 'Urban infrastructure',     color: '#d4271e'},
  {code: 25, label_pt: 'Outra área antrópica sem vegetação',  label_es: 'Otra área antrópica sin vegetación', label_en: 'Other anthropic non-vegetated', color: '#db4d4f'},
  {code: 30, label_pt: 'Mineração',                           label_es: 'Minería',                     label_en: 'Mining',                   color: '#9c0027'},
  {code: 68, label_pt: 'Outra área natural sem vegetação',    label_es: 'Otra área natural sin vegetación', label_en: 'Other natural non-vegetated', color: '#e97a7a'},

  // --- Water & not observed ---
  {code: 33, label_pt: 'Rio / lago / oceano',                 label_es: 'Río / lago / océano',         label_en: 'River / lake / ocean',     color: '#2532e4'},
  {code: 34, label_pt: 'Geleira',                             label_es: 'Glaciar',                     label_en: 'Glacier',                  color: '#93dfe6'},
  {code: 27, label_pt: 'Não observado',                       label_es: 'No observado',                label_en: 'Not observed',             color: '#ffffff'}
];

// Derived arrays for palette indexing (visual layer only)
var CLASS_CODES  = MB_CLASSES.map(function(d){ return d.code; });
var CLASS_COLORS = MB_CLASSES.map(function(d){ return d.color; });

//============================================================================================
// 3) HELPERS (visual only; raw data untouched)
//============================================================================================

/** Return band name for a given year. */
function bandName(year) { return 'classification_' + String(year); }

/** Build an indexed image (0..N-1) used ONLY for coloring (keeps raw layer for Inspector). */
function toIndexed(image) {
  var indices = ee.List.sequence(0, CLASS_CODES.length - 1);
  return image.remap(CLASS_CODES, indices, 9999); // unmapped classes → masked
}

/** Legend utils */
function classLabel(entry) {
  return (LABEL_LANG === 'es') ? entry.label_es :
         (LABEL_LANG === 'en') ? entry.label_en : entry.label_pt;
}

//============================================================================================
// 4) RENDER (one dataset, two layers from the same image)
//============================================================================================

// Keep references to prevent duplicates
var currentRawLayer;    // TOP: raw codes (Inspector reads true values)
var currentColorLayer;  // BOTTOM: colorized (visual only)

function renderYear(year) {
  var bname    = bandName(year);
  var classImg = MAPBIOMAS.select(bname);

  // Remove previous layers if any
  if (currentRawLayer)   Map.layers().remove(currentRawLayer);
  if (currentColorLayer) Map.layers().remove(currentColorLayer);
  
  // TOP — raw class codes (no vis params): Inspector shows true class code
  currentRawLayer = ui.Map.Layer(classImg, {}, 'MapBiomas C6 — códigos (bruto): ' + year, false);
  Map.layers().add(currentRawLayer);

  // BOTTOM — colorized copy (visual only)
  var indexed = toIndexed(classImg);
  var visIdx  = {min: 0, max: CLASS_COLORS.length - 1, palette: CLASS_COLORS};
  currentColorLayer = ui.Map.Layer(indexed, visIdx, 'MapBiomas C6 — visual: ' + year, true);
  Map.layers().add(currentColorLayer);

  // Update legend synced to RAW
  updateLegend(year, classImg, AOI);
}

//============================================================================================
// 5) LEGEND (dynamic; filtered by AOI if provided)
//============================================================================================
var legendPanel = ui.Panel({
  style: {
    position: 'bottom-left',
    padding: '8px',
    backgroundColor: 'rgba(255,255,255,0.9)'
  }
});
Map.add(legendPanel);

function legendRow(entry) {
  var chip = ui.Label({
    style: {
      backgroundColor: entry.color,
      padding: '8px',
      margin: '0 8px 4px 0',
      border: '1px solid #777',
      width: '18px'
    }
  });
  var txt = ui.Label({
    value: entry.code + ' — ' + classLabel(entry),
    style: {margin: '0 0 4px 0', fontSize: '12px'}
  });
  return ui.Panel({
    widgets: [chip, txt],
    layout: ui.Panel.Layout.Flow('horizontal'),
    style: {margin: 0, padding: 0}
  });
}

function updateLegend(year, classImage, aoi) {
  legendPanel.clear();
  legendPanel.add(ui.Label({
    value: 'MapBiomas C6 — ' + year,
    style: {fontWeight: 'bold', fontSize: '13px', margin: '0 0 6px 0'}
  }));

  if (!aoi) {
    MB_CLASSES.forEach(function(cls){ legendPanel.add(legendRow(cls)); });
    return;
  }

  // Only list classes present in AOI
  var bname = bandName(year);
  classImage.reduceRegion({
    reducer   : ee.Reducer.frequencyHistogram(),
    geometry  : aoi,
    scale     : 30,
    maxPixels : 1e9,
    bestEffort: true
  }).get(bname).evaluate(function(hist) {
    var present = new Set(Object.keys(hist || {}).map(function(k){ return Number(k); }));
    MB_CLASSES.forEach(function(cls) {
      if (present.has(cls.code)) legendPanel.add(legendRow(cls));
    });
  });
}

//============================================================================================
// 6) UI — year selector
//============================================================================================
var uiPanel = ui.Panel({
  style: {
    position: 'top-left',
    padding: '8px',
    backgroundColor: 'rgba(255,255,255,0.9)'
  }
});
uiPanel.add(ui.Label({value: 'Ano (MapBiomas C6):', style: {fontWeight: 'bold', margin: '0 0 6px 0'}}));

var yearSelect = ui.Select({
  items: YEARS,
  value: String(DEFAULT_YEAR),
  onChange: function(v){ renderYear(Number(v)); },
  style: {stretch: 'horizontal'}
});
uiPanel.add(yearSelect);
Map.add(uiPanel);

//============================================================================================
// 7) TERRAMAZ ZONES — vector layer (black border, no fill) + checkbox in the panel
//============================================================================================

// Source polygons
var ZONES_FC = ee.FeatureCollection('projects/terramaz/assets/bases/zones_terramaz');

// Reference to the layer so we can remove/re-add it
var zonesLayer = null;

// Function to (re)build the styled vector layer
function buildZonesLayer() {
  var fc = AOI ? ZONES_FC.filterBounds(AOI) : ZONES_FC;
  var styled = fc.style({
    color: '000000',       // black border
    fillColor: '00000000', // transparent fill
    width: 2               // line thickness
  });
  return ui.Map.Layer(styled, {}, 'TerrAmaz zones', true);
}

// Checkbox in the existing uiPanel (from Section 6)
var zonesCheckbox = ui.Checkbox({
  label: 'Show TerrAmaz zones (black border)',
  value: false, // set true if you want it visible by default
  onChange: function(checked) {
    // Always rebuild the layer (so AOI filters apply if set)
    if (zonesLayer) {
      Map.layers().remove(zonesLayer);
      zonesLayer = null;
    }
    if (checked) {
      zonesLayer = buildZonesLayer();
      // Add last so it stays on top of raster layers
      Map.layers().add(zonesLayer);
    }
  },
  style: {margin: '8px 0 0 0'}
});

// Attach the checkbox to the existing UI panel
uiPanel.add(zonesCheckbox);


//============================================================================================
// 8) KICKOFF
//============================================================================================
renderYear(DEFAULT_YEAR);
