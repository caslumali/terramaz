#===============================================================================
# 0. OVERVIEW
#===============================================================================
"""
Reprojects the binary mosaic raster into an equal-area CRS (EPSG:8857 - Equal Earth),
preserving 30m pixel size, using nearest neighbor resampling. Output is optimized for MSPA.
"""

#===============================================================================
# 1. IMPORTS AND PARAMETERS
#===============================================================================
import os
from osgeo import gdal

gdal.UseExceptions()

# Define working directory and input/output paths
data_folder = "data/raster/mspa_2024/"
input_raster = os.path.join(data_folder, "forest_mask_binary_mosaic.tif")
output_raster = os.path.join(data_folder, "forest_mask_binary_eqearth.tif")

# Confirm input raster exists
if not os.path.exists(input_raster):
    raise FileNotFoundError(f"Input raster not found: {input_raster}")

#===============================================================================
# 2. GDAL WARP OPTIONS FOR EQUAL EARTH REPROJECTION
#===============================================================================
print("Reprojecting mosaic raster to Equal Earth (EPSG:8857)...")

warp_options = gdal.WarpOptions(
    dstSRS="EPSG:8857",        # Equal Earth projection
    format="GTiff",
    xRes=30, yRes=30,          # 30m pixels
    resampleAlg="near",       # Nearest neighbor (preserves classes)
    creationOptions=[
        "COMPRESS=LZW",
        "TILED=YES",
        "BIGTIFF=YES",
        "NUM_THREADS=ALL_CPUS"
    ],
    dstNodata=0                # Optional: avoid unintended NoData
)

gdal.Warp(destNameOrDestDS=output_raster, srcDSOrSrcDSTab=input_raster, options=warp_options)

print(f"Reprojected raster saved to: {output_raster}")
