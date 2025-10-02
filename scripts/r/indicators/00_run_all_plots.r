##%###########################################################################%##
#                                                                               #
#                     MASTER SCRIPT – RUN ALL PLOTS                           ----
#                                                                               #
##%###########################################################################%##
# This script executes all plot scripts located directly in the "plots/" folder.
# It ignores any subfolders (e.g., "histograms_maps", "olds").
#
#  To tun this script put in the R console:
#  source("scripts/r/indicators/00_run_all_plots.R")
#
# Each script is sourced sequentially, so all plots will be generated and saved
# according to the export logic inside each individual script.
#                                                                               #
##%###########################################################################%##

# 1) Configuration ----
# ------------------------------------------------------------------------- - - -
suppressPackageStartupMessages({
  library(glue)
})

# Directory where the plot scripts are stored
plot_scripts_dir <- "scripts/r/indicators"

# 2) List scripts to execute ----
# ------------------------------------------------------------------------- - - -
# List only files ending in ".R" that are directly inside "plots/"
scripts <- list.files(
  path = plot_scripts_dir,
  pattern = "\\.r$",         # only .R files
  full.names = TRUE,
  recursive = FALSE          # do NOT go into subfolders
)

# Ensure scripts are sorted (by numeric/alphabetical order)
scripts <- sort(scripts)

# Optional: exclude this master script itself (to avoid recursion)
scripts <- scripts[!grepl("00_run_all_plots\\.r$", scripts, ignore.case = TRUE)]

# 3) Execute all scripts ----
# ------------------------------------------------------------------------- - - -
message("=== RUNNING ALL PLOT SCRIPTS ===")
for (s in scripts) {
  message(glue("▶ Running: {basename(s)}"))
  tryCatch(
    source(s),
    error = function(e) {
      message(glue("❌ Error in {basename(s)}: {e$message}"))
    }
  )
}
message("=== ALL PLOTS FINISHED ===")
