#!/usr/bin/env bash
# MSPA batch – GWB 1.9.8  (system-wide)
set -Eeuo pipefail

echo "Startin at $(date) with $(nproc) CPUs visible"

# -------- user paths --------------------------------------------------------
TILES_DIR="/mnt/c/dados/sig/cirad/terramaz/data/raster/mspa_2024/inputs"
RESULTS_DIR="/mnt/c/dados/sig/cirad/terramaz/data/raster/mspa_2024/outputs"
LOG_DIR="$HOME/mspa_logs"
THREADS=4                 # Nº of cores to use
DISKFLAG=1                # Line 31 in mspa-parameters.txt
# ---------------------------------------------------------------------------

export OMP_NUM_THREADS=$THREADS    # controls IDL/OMP parallelism

GWB_BIN=$(command -v GWB_MSPA)     || { echo "GWB_MSPA not in PATH"; exit 1; }
mkdir -p "$RESULTS_DIR" "$LOG_DIR"

WORK=/tmp/mspa_work ; mkdir -p "$WORK"
PARAM="$WORK/mspa-parameters.txt"
if [[ ! -f $PARAM ]]; then
    cp /opt/GWB/input/mspa-parameters.txt "$PARAM"
    sed -i -e '27s/.*/8/'  -e '28s/.*/1/'  -e '29s/.*/1/' \
           -e '30s/.*/1/'  -e "31s/.*/$DISKFLAG/" -e '32s/.*/1/' "$PARAM"
fi

for TILE in "$TILES_DIR"/*.tif; do
    BASE=$(basename "$TILE" .tif)
    OUT="$RESULTS_DIR/${BASE}_mspa.tif"
    LOG="$LOG_DIR/${BASE}.log"
    [[ -f $OUT ]] && { echo "✓ Skip $BASE"; continue; }

    INDIR="$WORK/${BASE}_in"; OUTDIR="$WORK/${BASE}_out"
    rm -rf "$INDIR" "$OUTDIR"; mkdir -p "$INDIR" "$OUTDIR"
    cp "$TILE" "$INDIR/"; cp "$PARAM" "$INDIR/mspa-parameters.txt"

    echo "→ $BASE" | tee "$LOG"
    $GWB_BIN --nox -i="$INDIR" -o="$OUTDIR"           >>"$LOG" 2>&1

    RES=$(find "$OUTDIR" -name "${BASE}_*.tif" | head -n1)
    if [[ -f $RES ]]; then
        mv "$RES" "$OUT"; echo "   ✔ Saved $OUT" | tee -a "$LOG"
    else
        echo " ⚠️ empty – copying original tile" | tee -a "$LOG"
        gdal_calc.py -A "$TILE" --outfile="$OUT" --calc="0" --type=Byte --NoDataValue=0 --co COMPRESS=LZW
    fi
    rm -rf "$INDIR" "$OUTDIR"
done
echo "== batch done =="
