###############################################################################
#                                                                             #
#                          Temperature — Boxplots                             #
#                                                                             #
###############################################################################

## 1) Settings ----
# -----------------------------------------------------------------------------
WRITE_PLOT   <- FALSE
LANG         <- "pt"
TERRITORY    <- "cotriguacu"
INPUT_DIR    <- file.path("results/metrics", TERRITORY)
OUTPUT_DIR   <- file.path("results/plots",   TERRITORY)
FILENAME_STUB<- "temperature_box"
WIDTH        <- 160; HEIGHT <- 110; UNITS <- "mm"; DPI <- 300

## 2) Load config & libs ----
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(readr); library(dplyr); library(ggplot2) })
source("scripts/r/config/01_labels.r")
source("scripts/r/config/02_palettes.r")
source("scripts/r/config/03_themes.r")

## 3) Read data ----
# -----------------------------------------------------------------------------
csv_path <- list.files(INPUT_DIR, pattern = paste0("^", TERRITORY, "_climate_annual_.*\\.csv$"),
                       full.names = TRUE, ignore.case = TRUE)
if (length(csv_path) == 0) stop("CSV not found for climate_annual in: ", INPUT_DIR)
df <- suppressMessages(readr::read_csv(csv_path[1], show_col_types = FALSE))

## 4) Checks & grooming ----
# -----------------------------------------------------------------------------
# Expected columns: year, lst_c (or temperature column you use)
col_temp <- if ("lst_c" %in% names(df)) "lst_c" else "temperature_c"
stopifnot(all(c("year", col_temp) %in% names(df)))
df <- df %>% filter(!is.na(year), !is.na(.data[[col_temp]])) %>% arrange(year)

## 5) Plot ----
# -----------------------------------------------------------------------------
p <- ggplot(df, aes(x = factor(year), y = .data[[col_temp]])) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  labs(title = label("temperature_box_title", .LANG = LANG),
       x = label("axis_year", .LANG = LANG),
       y = label("axis_temperature_c", .LANG = LANG)) +
  theme_terramaz() + legend_top()

print(p)

## 6) Save ----
# -----------------------------------------------------------------------------
if (WRITE_PLOT) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  ggsave(file.path(OUTPUT_DIR, paste0(FILENAME_STUB, ".png")), p,
         width = WIDTH, height = HEIGHT, units = UNITS, dpi = DPI)
  ggsave(file.path(OUTPUT_DIR, paste0(FILENAME_STUB, ".svg")), p,
         width = WIDTH, height = HEIGHT, units = UNITS, dpi = 300)
  message("✓ saved: ", file.path(OUTPUT_DIR, paste0(FILENAME_STUB, ".{png,svg}")))
}
