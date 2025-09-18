#===============================================================================
# 0. OVERVIEW
#===============================================================================
"""
Mosaic multiple binary forest tiles into a single raster using GDAL.

Usage: Run in any Python environment where GDAL is installed (e.g., pygeo).
"""

#===============================================================================
# 1. IMPORTS AND PARAMETERSA
#===============================================================================
import os
from functions import mosaic_binary_for_gwb  # Import the function from function.py

# Define data folder and input/output paths
data_folder = "data/raster/mspa_2024/"
input_folder = os.path.join(data_folder, "gee_tiles")
output_path = os.path.join(data_folder, "forest_mask_binary_mosaic.tif")

# Call the function to create the mosaic
mosaic_binary_for_gwb(input_folder, output_path)

