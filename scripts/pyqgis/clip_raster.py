import os
import processing
from qgis.core import (
    QgsProject,
    QgsVectorLayer,
    QgsRasterLayer,
    QgsFeature,
    QgsFields,
    QgsField,
    QgsVectorFileWriter,
    QgsWkbTypes,
    QgsCoordinateReferenceSystem,
    QgsVectorLayer,
    QgsVectorDataProvider,
    QgsVectorLayer,
    QgsVectorFileWriter,
    QgsVectorLayer,
    QgsFeature,
    QgsGeometry,
    QgsVectorLayer,
    QgsVectorDataProvider
)
from qgis.PyQt.QtCore import QVariant

# ==============================================================
# SETTINGS - adjust these paths and names
# ==============================================================

# Name of the big mask raster in the current project
MASK_RASTER_NAME = "non_forest_mask_2024"

# Name of the territory boundary layer in the current project
BOUNDARY_LAYER_NAME = "terramaz_boundary"  # ajuste se o nome for outro

# Field that contains the short "zone" name (cotriguacu, paragominas, etc.)
ZONE_FIELD = "zone"

# Output folder for the clipped masks (one per territory)
OUTPUT_DIR = r"C:/dados/SIG/Cirad/Terramaz/data_raw/raster/non_forest_mask_2024"
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Base name for the output files
OUTPUT_BASE = "non_forest_mask_2024"

# GDAL creation options for strong compression (0/1 data)
CREATION_OPTS = "COMPRESS=LZW|PREDICTOR=2"

# ==============================================================
# Helper: get layer by name
# ==============================================================

def get_layer_by_name(name):
    """Return first layer with given name in the current QGIS project."""
    layers = QgsProject.instance().mapLayersByName(name)
    if not layers:
        raise RuntimeError(f"Layer not found: {name}")
    return layers[0]

# ==============================================================
# 1) Get input layers
# ==============================================================

mask_layer = get_layer_by_name(MASK_RASTER_NAME)
boundary_layer = get_layer_by_name(BOUNDARY_LAYER_NAME)

if not isinstance(mask_layer, QgsRasterLayer):
    raise RuntimeError("MASK_RASTER_NAME is not a raster layer.")

if not isinstance(boundary_layer, QgsVectorLayer):
    raise RuntimeError("BOUNDARY_LAYER_NAME is not a vector layer.")

print("Input raster:", mask_layer.name())
print("Boundary layer:", boundary_layer.name())

# ==============================================================
# 2) Loop over territories and clip
# ==============================================================

for feat in boundary_layer.getFeatures():
    zone = feat[ZONE_FIELD]
    if zone is None or str(zone).strip() == "":
        continue

    zone_str = str(zone)
    print(f"Processing zone: {zone_str}")

    # Create an in-memory mask layer with a single feature (this territory)
    crs_authid = boundary_layer.crs().authid()
    mem_layer = QgsVectorLayer(f"Polygon?crs={crs_authid}", f"mask_{zone_str}", "memory")
    prov = mem_layer.dataProvider()

    new_feat = QgsFeature()
    new_feat.setGeometry(feat.geometry())
    prov.addFeatures([new_feat])
    mem_layer.updateExtents()

    # Output path
    out_path = os.path.join(
        OUTPUT_DIR,
        f"{OUTPUT_BASE}_{zone_str}.tif"
    )

    # Run GDAL clip by mask
    # DATA_TYPE=0 -> "use input layer data type" (already Byte)
    params = {
        "INPUT": mask_layer,
        "MASK": mem_layer,
        "SOURCE_CRS": None,
        "TARGET_CRS": None,
        "NODATA": None,  # assume 0 as nodata outside mask
        "ALPHA_BAND": False,
        "CROP_TO_CUTLINE": True,
        "KEEP_RESOLUTION": True,
        "OPTIONS": CREATION_OPTS,
        "DATA_TYPE": 0,
        "OUTPUT": out_path,
    }

    res = processing.run("gdal:cliprasterbymasklayer", params)

    # Use the path actually returned by the algorithm
    out_path_final = res["OUTPUT"]

    print(f"  -> saved: {out_path_final}")

    # Optionally add result to project with a nice name
    nice_name = f"{zone_str}_non_forest_mask_2024"
    out_layer = QgsRasterLayer(out_path_final, nice_name)
    
    if out_layer.isValid():
        QgsProject.instance().addMapLayer(out_layer)
    else:
        print(f"  [warning] output layer invalid for {zone_str}")

print("Done clipping masks for all territories.")
