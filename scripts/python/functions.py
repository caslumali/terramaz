import os
from osgeo import gdal
from tqdm import tqdm

gdal.UseExceptions()

#===============================================================================
# 1. FUNCTION FOR MOSAICKING BINARY RASTERS FOR GWB (1-band, Byte, LZW)
#===============================================================================
def mosaic_binary_for_gwb(input_folder, output_path):
    """
    Mosaics multiple binary rasters (0/1) into a single-band Byte GeoTIFF,
    optimized for use in GWB/MSPA (no NoData, LZW-compressed).
    
    Parameters:
        input_folder (str): Folder containing .tif binary tiles.
        output_path (str): Full path to the resulting GeoTIFF file.
    """
    print(f"Input folder: {input_folder}")
    file_list = sorted([
        os.path.join(input_folder, f)
        for f in os.listdir(input_folder)
        if f.lower().endswith(".tif")
    ])

    if not file_list:
        raise ValueError("No .tif files found in the input folder.")

    print(f"Found {len(file_list)} binary raster tiles.")

    print("Preview of files to mosaic:")
    for f in tqdm(file_list, desc="Listing files", unit="file"):
        print(f"  - {os.path.basename(f)}")

    if os.path.exists(output_path):
        print("Removing existing output file...")
        os.remove(output_path)

    print("Mosaicking tiles into single binary raster (Byte, LZW)...")
    warp_options = gdal.WarpOptions(
        format="GTiff",
        outputType=gdal.GDT_Byte,
        creationOptions=[
            "COMPRESS=LZW",
            "TILED=YES",
            "BIGTIFF=YES",
            "NUM_THREADS=ALL_CPUS",
            "BLOCKXSIZE=512",
            "BLOCKYSIZE=512"
        ],
        srcNodata=None,
        dstNodata=None
    )

    gdal.Warp(destNameOrDestDS=output_path, srcDSOrSrcDSTab=file_list, options=warp_options)

    print(f"Mosaic saved successfully at: {output_path}")

