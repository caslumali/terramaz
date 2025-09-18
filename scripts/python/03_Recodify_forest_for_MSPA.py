#===============================================================================
# 0. OVERVIEW
#===============================================================================
"""
Recodes binary forest raster (0/1) into MSPA format (1 = Background, 2 = Foreground),
removes overviews to ensure GWB compatibility, and saves LZW-compressed output.
"""

#===============================================================================
# 1. IMPORTS
#===============================================================================
import subprocess
import sys
import os

# GDAL calc script path
# Update this path to match your GDAL installation
gdal_calc_path = r"C:\Users\caslu\miniforge3\envs\pygeo\Scripts\gdal_calc.py"

#===============================================================================
# 2. PATHS
#===============================================================================
# Define the data folder and input raster path
data_folder = "data/raster/mspa_2024/"
input_raster  = os.path.join(data_folder, "forest_mask_binary_eqearth.tif")

# Ensure the input raster path is correct
if not os.path.exists(input_raster):
    raise FileNotFoundError(f"Input raster not found: {input_raster}")
print(f"Input raster found: {input_raster}")

# Output raster path and GDAL calc script path
output_raster = os.path.join(data_folder, "forest_mask_recode_mspa.tif")

#===============================================================================
# 3. RECODE RASTER TO MSPA FORMAT
#===============================================================================
print("Recoding binary raster to MSPA-compatible format...")

cmd_recode = [
    sys.executable, gdal_calc_path,
    "-A", input_raster,
    "--outfile", output_raster,
    "--calc=2*A + 1*(A==0)",
    "--type=Byte",
    "--NoDataValue=None",
    "--co=COMPRESS=LZW",
    "--co=TILED=YES",
    "--co=BIGTIFF=YES"
]

subprocess.run(cmd_recode, check=True)
print("Recoding complete.")

#===============================================================================
# 4. CLEAN OVERVIEWS (REQUIRED FOR GWB)
#===============================================================================
print("Cleaning overviews...")
subprocess.run(["gdaladdo", "-clean", output_raster], check=True)
print("Overviews cleaned. Raster is ready for GWB/MSPA.")
