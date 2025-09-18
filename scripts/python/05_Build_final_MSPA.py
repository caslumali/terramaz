#===============================================================================
# 0. OVERVIEW
#===============================================================================
"""
Mosaic MSPA-processed tiles and reproject to EPSG:4326, aligned to a reference raster.

Steps:
  1. Merge the 49 MSPA tiles into a seamless mosaic (Equal Earth)
  2. Reproject to EPSG:4326 using extent, resolution, and alignment of a 
     reference raster (e.g., the original GEE-derived forest mosaic)

Output: Two GeoTIFFs (Equal Earth and EPSG:4326), compressed with LZW.

Requires: GDAL with Python bindings (`osgeo.gdal`)
"""

#===============================================================================
# 1. IMPORTS AND CONFIGURATION
#===============================================================================
import os
from glob import glob
from osgeo import gdal
gdal.UseExceptions()

# Input/output directories
data_folder = "UE1001_StageM2/data/vegetation/veg3_gwb_mspa_2019"
tile_dir = os.path.join(data_folder, "veg3_mspa_outputs")

# Input reference raster in EPSG:4326 (used to guide reprojection)
reference_raster = os.path.join(data_folder, "veg3_binary_mosaic.tif")

# Output file paths
mosaic_eqearth = os.path.join(data_folder, "veg3_mspa_eqearth.tif")
mosaic_wgs84   = os.path.join(data_folder, "veg3_mspa_epsg4326.tif")

#===============================================================================
# 2. VALIDATION
#===============================================================================
tile_paths = sorted(glob(os.path.join(tile_dir, "*_mspa.tif")))
assert len(tile_paths) == 49, f"Expected 49 tiles, found {len(tile_paths)}"

# Set color interpretation to gray index for all tiles
print("Setting color interpretation to gray index for all tiles...")
for path in tile_paths:
    ds = gdal.Open(path, gdal.GA_Update)
    band = ds.GetRasterBand(1)
    band.SetColorTable(None)
    band.SetColorInterpretation(gdal.GCI_GrayIndex)
    ds = None
    
#===============================================================================
# 3. MOSAIC TILES IN EQUAL EARTH PROJECTION
#===============================================================================
print("Building seamless mosaic in Equal Earth projection...")

vrt_tmp = "mosaic_tmp.vrt"
gdal.BuildVRT(vrt_tmp, tile_paths)
gdal.Translate(mosaic_eqearth, vrt_tmp, creationOptions=["COMPRESS=LZW"])
os.remove(vrt_tmp)

print(f"Mosaic saved to: {mosaic_eqearth}\n")

#===============================================================================
# 4. REPROJECT TO EPSG:4326 USING REFERENCE
#===============================================================================
print("Reprojecting to EPSG:4326 using reference raster...")

ref = gdal.Open(reference_raster)
gt = ref.GetGeoTransform()
xmin = gt[0]
ymax = gt[3]
xres = gt[1]
yres = gt[5]
xmax = xmin + ref.RasterXSize * xres
ymin = ymax + ref.RasterYSize * yres

gdal.Warp(
    mosaic_wgs84,
    mosaic_eqearth,
    dstSRS="EPSG:4326",
    xRes=abs(xres),
    yRes=abs(yres),
    outputBounds=[xmin, ymin, xmax, ymax],
    outputBoundsSRS="EPSG:4326",
    targetAlignedPixels=True,
    resampleAlg="near",
    dstNodata=0,
    creationOptions=["COMPRESS=LZW"]
)

print(f"Reprojected mosaic saved to: {mosaic_wgs84}")
print("All processing complete.")
