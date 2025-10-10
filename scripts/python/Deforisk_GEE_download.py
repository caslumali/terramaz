# -*- coding: utf-8 -*-
"""
Exports binary forest maps (1 = forest, 0 = non-forest) from the JRC TMF dataset
for the four TerrAmaz territories. Each exported raster contains only three bands:
t0 (initial), t1 (key event), and t3 (final year).

Author: Lucas Lima
"""

import ee
ee.Initialize()

# ==========================================================
# 1. GENERAL SETTINGS
# ==========================================================
# Define each territory with its AOI path, projection (CRS), buffer,
# and the three reference years for the time series.
territories = {
    "cotriguacu": {
        "aoi": "projects/terramaz/assets/deforisk/cotriguacu",
        "crs": "EPSG:32721",   # UTM zone 21S
        "buffer_km": 20,       # buffer around the AOI in kilometers
        "years": [1990, 2008, 2024]
    },
    "paragominas": {
        "aoi": "projects/terramaz/assets/deforisk/paragominas",
        "crs": "EPSG:32722",   # UTM zone 22S
        "buffer_km": 20,
        "years": [1990, 2008, 2024]
    },
    "guaviare": {
        "aoi": "projects/terramaz/assets/deforisk/guaviare",
        "crs": "EPSG:32618",   # UTM zone 18N
        "buffer_km": 20,
        "years": [1990, 2016, 2024]
    },
    "madre_de_dios": {
        "aoi": "projects/terramaz/assets/deforisk/madre_de_dios",
        "crs": "EPSG:32719",   # UTM zone 19S
        "buffer_km": 20,
        "years": [1990, 2010, 2024]
    }
}

# ==========================================================
# 2. TMF ANNUAL PRODUCT
# ==========================================================
# Load and mosaic the TMF AnnualChanges dataset.
# Each "DecYYYY" band contains the forest class for that year.
tmf = ee.ImageCollection("projects/JRC/TMF/v1_2024/AnnualChanges").mosaic()

def forest_mask(year):
    """
    Returns a binary forest mask for a given year.
    Pixels with class 1 or 2 are forest → 1; all others → 0.
    """
    band = f"Dec{year}"
    img = tmf.select(band)
    return img.eq(1).Or(img.eq(2)).rename(str(year)).uint8()

# ==========================================================
# 3. MAIN LOOP OVER TERRITORIES
# ==========================================================
for terr, p in territories.items():
    print(f"\n Starting export for {terr}...")

    # Load the AOI and apply buffer (converted from km to meters)
    aoi_fc = ee.FeatureCollection(p["aoi"])
    aoi_geom = aoi_fc.geometry()
    region = aoi_geom.buffer(p["buffer_km"] * 1000).bounds()

    # Stack only three bands (t0, t1, t3) corresponding to selected years
    stack = ee.Image.cat([forest_mask(y) for y in p["years"]]).clip(region)

    # Export the multiband stack to Google Drive
    task = ee.batch.Export.image.toDrive(
        image=stack,
        description=f"fcc_{terr}_t0t1t3",
        folder="Deforisk",                # Target folder on Google Drive
        fileNamePrefix=f"fcc_{terr}_t0t1t3",
        region=region,
        scale=30,                         # Pixel resolution in meters
        maxPixels=1e13,                   # Allow large exports
        crs=p["crs"]                      # Projection (UTM)
    )

    # Start the export task
    task.start()
    print(f" Export task created for {terr}: {p['years']}")

print("\n All export tasks have been started. Check the Earth Engine Tasks panel for progress.")
