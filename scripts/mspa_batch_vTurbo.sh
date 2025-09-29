#!/usr/bin/env bash
#===============================================================================
# MSPA batch — GWB 1.9.8 (system-wide), fast I/O + parallel tiles
#===============================================================================
set -Eeuo pipefail

echo "Starting at $(date) with $(nproc) CPUs visible"

#===============================================================================
# 1) USER PATHS & KNOBS
#===============================================================================
# Input/output (WSL paths recommended for max I/O throughput)
TILES_DIR="/mnt/c/dados/sig/cirad/terramaz/data/raster/mspa_2023/inputs"
RESULTS_DIR="/mnt/c/dados/sig/cirad/terramaz/data/raster/mspa_2023/outputs"
LOG_DIR="$HOME/mspa_logs"

# Concurrency:
JOBS="${JOBS:-8}"          # number of tiles processed in parallel (adjust to your SSD/CPU)
THREADS_PER_JOB="${THREADS_PER_JOB:-4}"  # OpenMP threads *per* MSPA process
DISKFLAG=1                 # Line 31 (disk caching) in mspa-parameters.txt

#===============================================================================
# 2) ENV (THREADING & GDAL)
#===============================================================================
# OpenMP threads used by GWB/IDL libs (per process):
export OMP_NUM_THREADS="$THREADS_PER_JOB"

# GDAL hints (mainly relevant if fallback gdal_calc runs):
export GDAL_NUM_THREADS=ALL_CPUS
export GDAL_CACHEMAX=4096    # MB; bump if you tem RAM de sobra (ex.: 8192)

# Faster temp on RAM (Linux/WSL):
WORK="/dev/shm/mspa_work"
mkdir -p "$WORK"

#===============================================================================
# 3) BINARIES & PARAM TEMPLATE
#===============================================================================
GWB_BIN="$(command -v GWB_MSPA)" || { echo "GWB_MSPA not in PATH"; exit 1; }
mkdir -p "$RESULTS_DIR" "$LOG_DIR" "$WORK"

PARAM="$WORK/mspa-parameters.txt"
if [[ ! -f "$PARAM" ]]; then
  cp /opt/GWB/input/mspa-parameters.txt "$PARAM"
  # Lines 27..32 adjustment (8-connected; background=1; foreground=1; etc.)
  sed -i \
    -e '27s/.*/8/'  \
    -e '28s/.*/1/'  \
    -e '29s/.*/1/'  \
    -e '30s/.*/1/'  \
    -e "31s/.*/$DISKFLAG/" \
    -e '32s/.*/1/' "$PARAM"
fi

#===============================================================================
# 4) TILE PROCESSOR (single tile)
#===============================================================================
process_tile() {
  local TILE="$1"
  local BASE OUT LOG INDIR OUTDIR
  BASE="$(basename "$TILE" .tif)"
  OUT="$RESULTS_DIR/${BASE}_mspa.tif"
  LOG="$LOG_DIR/${BASE}.log"

  # Skip if already done
  if [[ -f "$OUT" ]]; then
    echo "✓ Skip $BASE"
    return 0
  fi

  INDIR="$WORK/${BASE}_in"
  OUTDIR="$WORK/${BASE}_out"
  rm -rf "$INDIR" "$OUTDIR"
  mkdir -p "$INDIR" "$OUTDIR"

  # Fast "copy": try hardlink (falls back to cp if FS differs)
  if ! ln "$TILE" "$INDIR/" 2>/dev/null; then
    cp -f "$TILE" "$INDIR/"
  fi
  cp -f "$PARAM" "$INDIR/mspa-parameters.txt"

  echo "→ $BASE (threads/job=${OMP_NUM_THREADS})" | tee "$LOG"

  # Run MSPA (quiet/no X)
  "$GWB_BIN" --nox -i="$INDIR" -o="$OUTDIR" >>"$LOG" 2>&1

  # Grab first MSPA output tif (GWB names ${BASE}_*_MSPA_Class.tif etc.)
  local RES
  RES="$(find "$OUTDIR" -maxdepth 1 -type f -name "${BASE}_*.tif" | head -n1 || true)"

  if [[ -f "$RES" ]]; then
    mv -f "$RES" "$OUT"
    echo "   ✔ Saved $OUT" | tee -a "$LOG"
  else
    echo "   ⚠ MSPA empty — copying original tile to keep pipeline consistent" | tee -a "$LOG"
    # Fallback: write empty byte raster matching size (requires gdal_calc.py in PATH)
    gdal_calc.py -A "$TILE" --outfile="$OUT" --calc="0" --type=Byte --NoDataValue=0 \
                 --co=COMPRESS=LZW --co=TILED=YES --co=BIGTIFF=YES >>"$LOG" 2>&1 || true
  fi

  rm -rf "$INDIR" "$OUTDIR"
}

export -f process_tile
export GWB_BIN WORK PARAM RESULTS_DIR LOG_DIR OMP_NUM_THREADS

#===============================================================================
# 5) PARALLEL DISPATCH
#===============================================================================
# Choose a sensible default for JOBS if not set:
if [[ "${JOBS}" -lt 1 ]]; then
  JOBS=1
fi

# Optional: cap per-job threads so total threads ~= nproc
TOTAL_THREADS=$(( JOBS * THREADS_PER_JOB ))
if (( TOTAL_THREADS > $(nproc) )); then
  echo "Note: total threads ($TOTAL_THREADS) > CPUs ($(nproc)); consider JOBS*THREADS_PER_JOB <= $(nproc)"
fi

echo "Running with JOBS=$JOBS, THREADS_PER_JOB=$THREADS_PER_JOB (OMP_NUM_THREADS=$OMP_NUM_THREADS)"
echo "Temp work dir: $WORK"

# Feed tiles to xargs (parallel). Robust to spaces via -0.
find "$TILES_DIR" -maxdepth 1 -type f -name '*.tif' -print0 \
  | xargs -0 -n1 -P "$JOBS" bash -c 'process_tile "$@"' _

echo "== batch done =="
