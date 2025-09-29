#===============================================================================
# 0. OVERVIEW
#===============================================================================
"""
Tile a large MSPA-ready raster into smaller overlapping tiles using GDAL Retile.

This script cuts the image into 15,000 x 15,000 pixel tiles with a 300 px overlap
to ensure connectivity of morphological structures across tiles.

Output tiles are compressed (LZW), tiled, and saved as BigTIFFs. 
Designed for use before MSPA batch processing with GWB_MSPA.

This script is built for execution on a Windows machine using a GDAL-enabled 
Python environment (e.g., Miniforge or Conda).
"""

#===============================================================================
# 1. IMPORTS AND CONFIGURATION
#===============================================================================
import os
import sys
import subprocess

# Path to the GDAL Retile script installed in your Python environment
gdal_retile_path = r"C:\Users\caslu\miniforge3\envs\pygeo\Scripts\gdal_retile.py"

# Define working directory and paths
data_folder = "data/raster/mspa_2023/"
input_raster = os.path.join(data_folder, "forest_mask_recode_mspa.tif")
output_folder = os.path.join(data_folder, "inputs")

# Tile parameters
tile_size = 15000  # in pixels
overlap = 300      # buffer to preserve edge connectivity (≈9 km)

# GDAL creation options
gdal_options = [
    "-co", "COMPRESS=LZW",
    "-co", "TILED=YES",
    "-co", "BIGTIFF=YES"
]

#===============================================================================
# 2. VALIDATION
#===============================================================================
if not os.path.exists(input_raster):
    raise FileNotFoundError(f" Input raster not found:\n{input_raster}")

os.makedirs(output_folder, exist_ok=True)

#===============================================================================
# 3. GDAL RETILE COMMAND
#===============================================================================
print("\n Starting tile generation using GDAL Retile...")
print(f"Input raster: {input_raster}")
print(f"Tile size   : {tile_size} x {tile_size} px")
print(f"Overlap     : {overlap} px")
print(f"Output dir  : {output_folder}\n")

cmd = [
    sys.executable, gdal_retile_path,
    "-ps", str(tile_size), str(tile_size),
    "-overlap", str(overlap),
    "-targetDir", output_folder,
    *gdal_options,
    input_raster
]

#===============================================================================
# 4. EXECUTION
#===============================================================================
subprocess.run(cmd, check=True)
print("\n Tiling complete.\n")
