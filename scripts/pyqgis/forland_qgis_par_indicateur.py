# ================================================================
# TerrAmaz - Automatic QGIS project generator for Forland
# Author: Lucas + ChatGPT
#
# This script:
#   - Reads layers from the *current* QGIS project
#   - Copies data sources (rasters and vectors) to a clean folder
#   - Creates 12 encapsulated QGIS projects:
#       * 4 territories  ×  3 map types:
#         - Forest Integrity
#         - Fire Vulnerability
#         - Deforestation Risk (Deforisk)
#   - Applies QML styles from a common styles folder
#   - Copies QML styles next to each data file (same basename)
#   - Fills French metadata automatically for main rasters
#
# IMPORTANT:
#   - Run this inside the QGIS Python console, with your master
#     TerrAmaz project open and layers named exactly as defined below.
#   - Adjust BASE_OUTPUT_DIR and STYLES_DIR before running.
#   - The script uses ABSOLUTE paths for reliability. If you need
#     strictly relative paths in the final projects, you can open
#     each project, set "Save paths as relative" and resave.
# ================================================================

import os
import shutil

from qgis.core import (
    QgsProject,
    QgsRasterLayer,
    QgsVectorLayer,
    QgsLayerTreeLayer,
    QgsLayerTreeGroup,
    QgsLayerMetadata,
    QgsReferencedRectangle,
)

# ------------------------------------------------
# 0. USER SETTINGS - ADJUST THESE PATHS
# ------------------------------------------------

# Base folder where projects will be created
BASE_OUTPUT_DIR = os.path.abspath(
    "C:/dados/SIG/Cirad/Terramaz/qgis/forland/terramaz_qgis"
)

# Folder where your QML styles are stored
STYLES_DIR = os.path.abspath(
    "C:/dados/SIG/Cirad/Terramaz/qgis/forland/styles"
)

# Ensure base output directory exists
os.makedirs(BASE_OUTPUT_DIR, exist_ok=True)

# ------------------------------------------------
# 1. HELPER FUNCTIONS
# ------------------------------------------------

def get_layer_by_name(name):
    """
    Return the first layer with the given name from the current project.
    """
    proj = QgsProject.instance()
    layers = proj.mapLayersByName(name)
    if not layers:
        raise RuntimeError(f"Layer not found in current project: {name}")
    return layers[0]


def copy_datasource(layer, dest_folder):
    """
    Copy the underlying datasource of a layer (raster or vector) into dest_folder.
    For vectors, if it's a shapefile, all sidecar files (.shp, .shx, .dbf, .prj, etc.)
    are copied together.

    Returns the full path to the main data file in dest_folder.
    """
    os.makedirs(dest_folder, exist_ok=True)

    # Get raw datasource path (strip provider options if present, e.g. "|layerid=0")
    src = layer.dataProvider().dataSourceUri()
    if "|" in src:
        src = src.split("|")[0]

    if not os.path.isfile(src):
        raise RuntimeError(f"Datasource is not a file or not found: {src}")

    base_name = os.path.basename(src)
    name_no_ext, ext = os.path.splitext(base_name)

    # If vector/Shapefile: copy all sidecar files with same basename
    if isinstance(layer, QgsVectorLayer) and ext.lower() == ".shp":
        src_dir = os.path.dirname(src)
        dest_main = os.path.join(dest_folder, base_name)

        for f in os.listdir(src_dir):
            if f.startswith(name_no_ext + "."):
                shutil.copy2(
                    os.path.join(src_dir, f),
                    os.path.join(dest_folder, f)
                )
        return dest_main

    # Raster or non-shapefile vector: copy single file
    dest_path = os.path.join(dest_folder, base_name)
    shutil.copy2(src, dest_path)
    return dest_path


def apply_style(layer, style_filename):
    """
    Apply a QML style file (style_filename) from STYLES_DIR to the given layer.
    """
    style_path = os.path.join(STYLES_DIR, style_filename)
    if not os.path.isfile(style_path):
        raise RuntimeError(f"Style file not found: {style_path}")
    layer.loadNamedStyle(style_path)
    layer.triggerRepaint()


def copy_style_for_datasource(style_filename, data_path):
    """
    Copy the master QML style into the same folder as the data_path,
    renaming it to have the same basename as the data file.

    Example:
        data_path = ".../rasters/paragominas_riskmap.tif"
        style_filename = "03d_paragominas_riskmap.qml"
        => creates ".../rasters/paragominas_riskmap.qml"
    """
    style_src = os.path.join(STYLES_DIR, style_filename)
    if not os.path.isfile(style_src):
        raise RuntimeError(f"Style file not found: {style_src}")

    dest_dir = os.path.dirname(data_path)
    base_name, _ = os.path.splitext(os.path.basename(data_path))
    dest_style = os.path.join(dest_dir, base_name + ".qml")

    shutil.copy2(style_src, dest_style)


def add_layer_to_group(project, group, layer):
    """
    Add a layer to a given layer tree group in a project (without creating duplicate top-level entries).
    """
    project.addMapLayer(layer, False)  # Add to project, but not to root
    node = QgsLayerTreeLayer(layer.id())
    group.addChildNode(node)

def set_project_view_to_layer_extent(proj, layer):
    """
    Set the initial map view of the project to the extent of the given layer.

    This does NOT modify any data. It only affects how the QGIS project
    opens (zoom + center), ensuring that the project starts focused on the
    territory boundary instead of an arbitrary global extent.

    Behavior:
        - The project CRS is set to match the boundary layer CRS.
        - If available (QGIS ≥ 3.22), the project "default view extent"
          is updated so that the .qgz opens centered on the territory.

    Parameters
    ----------
    proj : QgsProject
        The QGIS project being constructed.

    layer : QgsMapLayer
        Typically the boundary layer for the territory.
        Its extent is used to define the initial view.
    """

    # Ensure the project CRS matches the boundary layer CRS
    proj.setCrs(layer.crs())

    # QGIS API compatibility: only set default view if available
    if hasattr(proj, "viewSettings"):
        view = proj.viewSettings()
        if hasattr(view, "setDefaultViewExtent"):
            # setDefaultViewExtent expects a QgsReferencedRectangle
            ref_extent = QgsReferencedRectangle(layer.extent(), layer.crs())
            view.setDefaultViewExtent(ref_extent)


# ------------------------------------------------
# 2. METADATA BUILDERS (FRENCH)
# ------------------------------------------------

def build_metadata_forest_integrity():
    """
    Build metadata for Forest Integrity rasters.
    The same metadata is used for all FI rasters (und/deg/reg) in all territories.
    """
    md = QgsLayerMetadata()
    md.setTitle("État de la forêt – Intégrité structurelle (TMF-JRC 2024)")

    abstract = (
        "Cette carte décrit l'état structurel des forêts tropicales humides en distinguant "
        "trois familles forestières (forêts intactes, dégradées et en régénération) et en "
        "attribuant à chaque pixel un score d'intégrité (0–100) intra-classe. Elle offre "
        "une lecture synthétique de la qualité écologique du couvert forestier au sein de "
        "chaque macro-classe."
    )

    lineage = (
        "Le produit Transition Subtypes du TMF-JRC (2024) a servi de base à la classification "
        "de l'état des forêts. Trois macro-classes sont distinguées : i) forêt non perturbée, "
        "ii) forêt dégradée, iii) forêt en régénération. À partir de ces trois familles, douze "
        "classes ont été établies en combinant la morphologie du couvert (cœur, perforation, "
        "bord/îlot) pour les forêts intactes, l'intensité et l'ancienneté des événements pour "
        "les forêts dégradées, et l'âge de la repousse pour les forêts en régénération. "
        "La morphologie du couvert forestier est dérivée du module MSPA du logiciel Guidos "
        "Toolbox Workbench, qui permet d'identifier les zones situées à moins de 120 m des "
        "lisières, où les effets de bord sont les plus marqués. Les séries TMF (1990–2024), "
        "harmonisées dans Google Earth Engine, permettent d'attribuer à chaque pixel un score "
        "d'intégrité compris entre 0 et 100, comparables uniquement à l'intérieur d'une même "
        "famille forestière."
    )
    abstract_full = abstract + "\n\nMéthode:\n" + lineage
    md.setAbstract(abstract_full)

    return md


def build_metadata_fire_vulnerability():
    """
    Build metadata for Fire Vulnerability rasters.
    Same metadata structure for all territories.
    """
    md = QgsLayerMetadata()
    md.setTitle("Vulnérabilité des forêts au feu")

    abstract = (
        "L'indicateur de vulnérabilité au feu mesure la propension du couvert forestier à subir "
        "des incendies, en intégrant l'aléa historique, l'exposition aux sources d'ignition et la "
        "sensibilité écologique du couvert. Les valeurs varient de 0 (risque minimal) à 1 "
        "(risque maximal), reclassées en cinq niveaux opérationnels de vulnérabilité."
    )
    md.setAbstract(abstract)

    lineage = (
        "La vulnérabilité des forêts au feu est calculée en combinant trois composantes A, E et S : "
        "A (Aléa) correspond à la fréquence historique des feux, dérivée des séries de surface "
        "brûlée (MapBiomas Fogo, MODIS Burned Area, GLAD Fire selon les territoires) sur la "
        "période 2003–2024. E (Exposition) mesure la proximité aux sources d'ignition à partir "
        "des distances aux routes (OSM) et aux zones de pâturage et d'agriculture (MapBiomas "
        "Amazonie), selon des seuils et scores définis par classe de distance. S (Sensibilité) "
        "repose sur l'état d'intégrité du couvert forestier (forêt intacte, dégradée, régénérée, "
        "issu du TMF) et sur sa position dans le paysage (cœur, bord, perforation, îlot) dérivée "
        "de l'analyse de morphologie MSPA. Le score final de vulnérabilité est la moyenne de "
        "ces trois dimensions et est ensuite regroupé en cinq classes de vulnérabilité "
        "(Très faible à Très élevée)."
    )
    abstract_full = abstract + "\n\nMéthode:\n" + lineage
    md.setAbstract(abstract_full)

    return md


def build_metadata_riskmap(territory_name_fr, model_name, r2, rmse):
    """
    Build metadata for Riskmap rasters, customised per territory with model, R² and RMSE.
    """
    md = QgsLayerMetadata()
    md.setTitle(f"Risque de déforestation – {territory_name_fr}")

    abstract = (
        "Cette carte représente la probabilité spatiale de déforestation à moyen terme, "
        "estimée à partir de modèles statistiques calibrés sur l'historique de déforestation "
        "du territoire. Elle met en évidence les zones où le risque de conversion du couvert "
        "forestier est le plus élevé."
    )
    md.setAbstract(abstract)

    lineage = (
        "La carte de risque de déforestation a été produite avec le plugin Deforisk dans QGIS, "
        "qui permet de comparer plusieurs modèles statistiques (iCAR, GLM, Random Forest, "
        "Moving Window) pour cartographier la probabilité de déforestation. Les modèles ont "
        "été calibrés à partir d'observations historiques de déforestation, en trois périodes "
        "correspondant aux mêmes intervalles que ceux utilisés pour l'indicateur de transition "
        "d'usage des sols. La couverture forestière TMF sert de base, tandis que les variables "
        "explicatives incluent la distance à la lisière de la forêt, l'altitude, la présence "
        "d'aires protégées, ainsi que la distance aux routes et aux cours d'eau."
    )

    result_riskmap = (
        "Le modèle "
        f"retenu pour {territory_name_fr} est {model_name}, avec des performances statistiques "
        f"de R² = {r2:.2f} et RMSE = {rmse:.2f}. "
        "Le raster résultant exprime, pour chaque pixel forestier, un niveau relatif de risque de déforestation."
    )
    abstract_full = abstract + "\n\nMéthode:\n" + lineage + "\n\nRésultat Riskmap:\n" + result_riskmap
    md.setAbstract(abstract_full)

    return md

# ------------------------------------------------
# 3. CONFIGURATION - TERRITORIES, LAYER NAMES, STYLES, MODELS
# ------------------------------------------------

# Per-territory boundary layers (single polygon per site)
BOUNDARY_LAYERS = {
    "cotriguacu": "cotriguacu_boundary",
    "paragominas": "paragominas_boundary",
    "guaviare": "guaviare_boundary",
    "madre_de_dios": "madre_de_dios_boundary",
}

# Forest Integrity raster layer names by territory
FI_LAYERS = {
    "cotriguacu": {
        "und": "cotriguacu_fi_und",
        "deg": "cotriguacu_fi_deg",
        "reg": "cotriguacu_fi_reg",
        "label": "Cotriguaçu",
    },
    "paragominas": {
        "und": "paragominas_fi_und",
        "deg": "paragominas_fi_deg",
        "reg": "paragominas_fi_reg",
        "label": "Paragominas",
    },
    "guaviare": {
        "und": "guaviare_fi_und",
        "deg": "guaviare_fi_deg",
        "reg": "guaviare_fi_reg",
        "label": "Guaviare",
    },
    "madre_de_dios": {
        "und": "madre_de_dios_fi_und",
        "deg": "madre_de_dios_fi_deg",
        "reg": "madre_de_dios_fi_reg",
        "label": "Madre de Dios",
    },
}

# Fire Vulnerability raster layer names
FIRE_LAYERS = {
    "cotriguacu": {
        "layer": "cotriguacu_fire_vulnerability",
        "style": "02a_cotriguacu_fire_vulnerability.qml",
        "label": "Cotriguaçu",
    },
    "guaviare": {
        "layer": "guaviare_fire_vulnerability",
        "style": "02b_guaviare_fire_vulnerability.qml",
        "label": "Guaviare",
    },
    "madre_de_dios": {
        "layer": "madre_de_dios_fire_vulnerability",
        "style": "02c_madre_de_dios_fire_vulnerability.qml",
        "label": "Madre de Dios",
    },
    "paragominas": {
        "layer": "paragominas_fire_vulnerability",
        "style": "02d_paragominas_fire_vulnerability.qml",
        "label": "Paragominas",
    },
}

# Riskmap raster layer names and model performances
RISKM_LAYERS = {
    "cotriguacu": {
        "layer": "cotriguacu_riskmap",
        "style": "03a_cotriguacu_riskmap.qml",
        "model": "iCAR",
        "r2": 0.30,
        "rmse": 28.82,
        "label": "Cotriguaçu",
    },
    "paragominas": {
        "layer": "paragominas_riskmap",
        "style": "03d_paragominas_riskmap.qml",
        "model": "Moving Window (11 pixels)",
        "r2": 0.18,
        "rmse": 30.02,
        "label": "Paragominas",
    },
    "guaviare": {
        "layer": "guaviare_riskmap",
        "style": "03b_guaviare_riskmap.qml",
        "model": "iCAR",
        "r2": 0.30,
        "rmse": 15.37,
        "label": "Guaviare",
    },
    "madre_de_dios": {
        "layer": "madre_de_dios_riskmap",
        "style": "03c_madre_de_dios_riskmap.qml",
        "model": "Moving Window (11 pixels)",
        "r2": 0.31,
        "rmse": 14.18,
        "label": "Madre de Dios",
    },
}

# Mask raster layer names per territory (already clipped and compressed)
MASK_LAYERS = {
    "cotriguacu": "cotriguacu_non_forest_mask_2024",
    "paragominas": "paragominas_non_forest_mask_2024",
    "guaviare": "guaviare_non_forest_mask_2024",
    "madre_de_dios": "madre_de_dios_non_forest_mask_2024",
}

# Style filenames for FI bands
FI_STYLES = {
    "und": "01a_forest_integrity_undisturbed.qml",
    "deg": "01b_forest_integrity_degraded.qml",
    "reg": "01c_forest_integrity_regrowth.qml",
}

# Common style filenames
COMMON_STYLES = {
    "mask_non_forest": "non_forest_mask_2024.qml",
    "boundary": "terramaz_boundaries.qml"
}

# ------------------------------------------------
# 4. PROJECT BUILDERS
# ------------------------------------------------

def build_forest_integrity_project(territory_key):
    """
    Build Forest Integrity project for a given territory.
    """
    fi_info = FI_LAYERS[territory_key]
    territory_label = fi_info["label"]

    print(f"Building Forest Integrity project for {territory_label}...")

    # Paths
    out_dir = os.path.join(BASE_OUTPUT_DIR, territory_key, "forest_integrity")
    rasters_dir = os.path.join(out_dir, "rasters")
    shapes_dir = os.path.join(out_dir, "shapes")
    os.makedirs(rasters_dir, exist_ok=True)
    os.makedirs(shapes_dir, exist_ok=True)

    project_path = os.path.join(out_dir, f"{territory_key}_ForestIntegrity.qgz")

    # Create a new standalone project
    proj = QgsProject()
    root = proj.layerTreeRoot()
    proj.setFileName(project_path)

    # Create groups
    grp_boundaries = root.addGroup("Boundary")
    grp_fi = root.addGroup("Forest Integrity")
    grp_masks = root.addGroup("Masks")

    # --- Add boundary (territory-specific) ---
    boundary_master = get_layer_by_name(BOUNDARY_LAYERS[territory_key])
    boundary_path = copy_datasource(boundary_master, shapes_dir)
    boundary_layer = QgsVectorLayer(
        boundary_path,
        BOUNDARY_LAYERS[territory_key],
        "ogr"
    )
    apply_style(boundary_layer, COMMON_STYLES["boundary"])
    copy_style_for_datasource(COMMON_STYLES["boundary"], boundary_path)
    add_layer_to_group(proj, grp_boundaries, boundary_layer)

    # Set initial project view centered on the boundary layer
    set_project_view_to_layer_extent(proj, boundary_layer)

    # --- Non-forest mask (territory-specific, already clipped and light) ---
    mask_master = get_layer_by_name(MASK_LAYERS[territory_key])
    mask_path = copy_datasource(mask_master, rasters_dir)
    mask_layer = QgsRasterLayer(mask_path, MASK_LAYERS[territory_key])
    apply_style(mask_layer, COMMON_STYLES["mask_non_forest"])
    copy_style_for_datasource(COMMON_STYLES["mask_non_forest"], mask_path)
    add_layer_to_group(proj, grp_masks, mask_layer)

    # --- Add FI rasters (und, deg, reg) ---
    md_fi = build_metadata_forest_integrity()

    for band_key in ["und", "deg", "reg"]:
        layer_name = fi_info[band_key]
        master_layer = get_layer_by_name(layer_name)
        raster_path = copy_datasource(master_layer, rasters_dir)
        new_layer = QgsRasterLayer(raster_path, layer_name)
        apply_style(new_layer, FI_STYLES[band_key])
        copy_style_for_datasource(FI_STYLES[band_key], raster_path)
        new_layer.setMetadata(md_fi)
        add_layer_to_group(proj, grp_fi, new_layer)

    # Save project
    proj.write()
    print(f"Saved Forest Integrity project: {project_path}")


def build_fire_vulnerability_project(territory_key):
    """
    Build Fire Vulnerability project for a given territory.
    """
    fire_info = FIRE_LAYERS[territory_key]
    territory_label = fire_info["label"]

    print(f"Building Fire Vulnerability project for {territory_label}...")

    out_dir = os.path.join(BASE_OUTPUT_DIR, territory_key, "fire_vulnerability")
    rasters_dir = os.path.join(out_dir, "rasters")
    shapes_dir = os.path.join(out_dir, "shapes")
    os.makedirs(rasters_dir, exist_ok=True)
    os.makedirs(shapes_dir, exist_ok=True)

    project_path = os.path.join(out_dir, f"{territory_key}_FireVulnerability.qgz")

    proj = QgsProject()
    root = proj.layerTreeRoot()
    proj.setFileName(project_path)

    grp_boundaries = root.addGroup("Boundaries")
    grp_fire = root.addGroup("Fire Vulnerability")
    grp_masks = root.addGroup("Masks")

    # Boundary (territory-specific)
    boundary_master = get_layer_by_name(BOUNDARY_LAYERS[territory_key])
    boundary_path = copy_datasource(boundary_master, shapes_dir)
    boundary_layer = QgsVectorLayer(
        boundary_path,
        BOUNDARY_LAYERS[territory_key],
        "ogr"
    )
    apply_style(boundary_layer, COMMON_STYLES["boundary"])
    copy_style_for_datasource(COMMON_STYLES["boundary"], boundary_path)
    add_layer_to_group(proj, grp_boundaries, boundary_layer)

    # Set initial project view centered on the boundary layer
    set_project_view_to_layer_extent(proj, boundary_layer)

    # Non-forest mask
    mask_master = get_layer_by_name(MASK_LAYERS[territory_key])
    mask_path = copy_datasource(mask_master, rasters_dir)
    mask_layer = QgsRasterLayer(mask_path, MASK_LAYERS[territory_key])
    apply_style(mask_layer, COMMON_STYLES["mask_non_forest"])
    copy_style_for_datasource(COMMON_STYLES["mask_non_forest"], mask_path)
    add_layer_to_group(proj, grp_masks, mask_layer)

    # Fire raster
    fire_master = get_layer_by_name(fire_info["layer"])
    fire_raster_path = copy_datasource(fire_master, rasters_dir)
    fire_layer = QgsRasterLayer(fire_raster_path, fire_info["layer"])
    apply_style(fire_layer, fire_info["style"])
    copy_style_for_datasource(fire_info["style"], fire_raster_path)
    fire_layer.setMetadata(build_metadata_fire_vulnerability())
    add_layer_to_group(proj, grp_fire, fire_layer)

    proj.write()
    print(f"Saved Fire Vulnerability project: {project_path}")


def build_riskmap_project(territory_key):
    """
    Build Deforisk Riskmap project for a given territory.
    """
    risk_info = RISKM_LAYERS[territory_key]
    territory_label = risk_info["label"]

    print(f"Building Riskmap project for {territory_label}...")

    out_dir = os.path.join(BASE_OUTPUT_DIR, territory_key, "riskmap")
    rasters_dir = os.path.join(out_dir, "rasters")
    shapes_dir = os.path.join(out_dir, "shapes")
    os.makedirs(rasters_dir, exist_ok=True)
    os.makedirs(shapes_dir, exist_ok=True)

    project_path = os.path.join(out_dir, f"{territory_key}_Riskmap.qgz")

    proj = QgsProject()
    root = proj.layerTreeRoot()
    proj.setFileName(project_path)

    grp_boundaries = root.addGroup("Boundaries")
    grp_risk = root.addGroup("Riskmap")
    grp_masks = root.addGroup("Masks")

    # Boundary (territory-specific)
    boundary_master = get_layer_by_name(BOUNDARY_LAYERS[territory_key])
    boundary_path = copy_datasource(boundary_master, shapes_dir)
    boundary_layer = QgsVectorLayer(
        boundary_path,
        BOUNDARY_LAYERS[territory_key],
        "ogr"
    )
    apply_style(boundary_layer, COMMON_STYLES["boundary"])
    copy_style_for_datasource(COMMON_STYLES["boundary"], boundary_path)
    add_layer_to_group(proj, grp_boundaries, boundary_layer)

    # Set initial project view centered on the boundary layer
    set_project_view_to_layer_extent(proj, boundary_layer)

    # Non-forest mask
    mask_master = get_layer_by_name(MASK_LAYERS[territory_key])
    mask_path = copy_datasource(mask_master, rasters_dir)
    mask_layer = QgsRasterLayer(mask_path, MASK_LAYERS[territory_key])
    apply_style(mask_layer, COMMON_STYLES["mask_non_forest"])
    copy_style_for_datasource(COMMON_STYLES["mask_non_forest"], mask_path)
    add_layer_to_group(proj, grp_masks, mask_layer)

    # Riskmap raster
    risk_master = get_layer_by_name(risk_info["layer"])
    risk_raster_path = copy_datasource(risk_master, rasters_dir)
    risk_layer = QgsRasterLayer(risk_raster_path, risk_info["layer"])
    apply_style(risk_layer, risk_info["style"])
    copy_style_for_datasource(risk_info["style"], risk_raster_path)

    md_risk = build_metadata_riskmap(
        risk_info["label"],
        risk_info["model"],
        risk_info["r2"],
        risk_info["rmse"],
    )
    risk_layer.setMetadata(md_risk)
    add_layer_to_group(proj, grp_risk, risk_layer)

    proj.write()
    print(f"Saved Riskmap project: {project_path}")

# ------------------------------------------------
# 5. MAIN - RUN ALL TERRITORIES
# ------------------------------------------------

def run_all():
    """
    Main entry point: build all 12 projects (4 territories × 3 map types).
    """
    territories = ["cotriguacu", "paragominas", "guaviare", "madre_de_dios"]

    for t in territories:
        build_forest_integrity_project(t)
        build_fire_vulnerability_project(t)
        build_riskmap_project(t)

    print("All TerrAmaz projects generated successfully.")

# Call main
run_all()
