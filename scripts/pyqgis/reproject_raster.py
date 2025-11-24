import os
import processing
from qgis.core import QgsProject, QgsRasterLayer, QgsCoordinateReferenceSystem

# ===========================
# 0) CONFIGURAÇÕES
# ===========================

# Nome das camadas de entrada no projeto QGIS
RISKMAP_NAMES = [
    "cotriguacu_riskmap",
    "guaviare_riskmap",
    "paragominas_riskmap",
    "madre_de_dios_riskmap",
]

# Pasta de saída
OUT_DIR = r"C:/dados/SIG/Cirad/Terramaz/data_raw/raster/deforisk"
os.makedirs(OUT_DIR, exist_ok=True)

# CRS alvo
TARGET_CRS = QgsCoordinateReferenceSystem("EPSG:4326")

# ===========================
# 1) Função helper
# ===========================

def get_layer_by_name(name):
    layers = QgsProject.instance().mapLayersByName(name)
    if not layers:
        raise RuntimeError(f"Layer not found in current project: {name}")
    return layers[0]

# ===========================
# 2) Loop nos riskmaps
# ===========================

for name in RISKMAP_NAMES:
    layer = get_layer_by_name(name)
    if not isinstance(layer, QgsRasterLayer):
        print(f"[ERRO] {name} não é raster, pulando.")
        continue

    print(f"Reprojetando {name} -> EPSG:4326 ...")

    out_path = os.path.join(OUT_DIR, f"{name}_epsg4326.tif")

    params = {
        "INPUT": layer,                 # raster de entrada
        "SOURCE_CRS": None,             # usa o CRS nativo do raster
        "TARGET_CRS": TARGET_CRS,       # EPSG:4326
        "RESAMPLING": 1,                # 0=Nearest, 1=Bilinear, 2=Cubic, ...
        "NODATA": 0,                    # NoData de saída
        "TARGET_RESOLUTION": None,      # mantém resolução (~30 m)
        "TARGET_EXTENT": None,          # usa extensão automática
        "TARGET_EXTENT_CRS": None,
        "MULTITHREADING": True,
        "OPTIONS": "COMPRESS=LZW",      # compressão LZW
        "DATA_TYPE": 0,                 # 0 = manter tipo de dado de entrada
        "EXTRA": "",                    # argumentos GDAL extras (se precisar)
        "OUTPUT": out_path,
    }

    res = processing.run("gdal:warpreproject", params)

    print(f"  -> salvo em: {out_path}")

    # (opcional) adicionar resultado ao projeto
    out_layer = QgsRasterLayer(out_path, f"{name}_epsg4326")
    if out_layer.isValid():
        QgsProject.instance().addMapLayer(out_layer)
    else:
        print(f"  [aviso] raster de saída inválido para {name}")

print("Finalizado reprojeção de todos os riskmaps.")
