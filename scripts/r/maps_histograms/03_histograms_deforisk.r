##%###########################################################################%##
#                                                                               #
#              Deforisk Risk Histograms & Tables (iCAR only)                ----
#                                                                               #
##%###########################################################################%##

# 1) Configuration ----
# ------------------------------------------------------------------------- - - -
suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(glue)
  library(scales)
  library(svglite)
  library(rlang)
  library(purrr)
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE     # EN: Export PNGs
WRITE_SVG  <- TRUE     # EN: Also export SVGs (vector)

# Territories to process
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

# Labels for pretty titles (if needed later)
TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Output parameters
UNITS       <- "mm"
DPI         <- 450
PLOT_WIDTH  <- 250
PLOT_HEIGHT <- 35
OUTPUT_DIR  <- file.path("results", "maps", "histograms", "deforisk_grouped")

CLASS_TABLE <- tibble::tribble(
  ~classe,       ~score_min, ~score_max,
  "Très faible", 0.00,       0.19,
  "Faible",      0.20,       0.39,
  "Modéré",      0.40,       0.59,
  "Élevé",       0.60,       0.79,
  "Très élevé",  0.80,       1.00
)
CLASS_NAMES <- factor(CLASS_TABLE$classe, levels = CLASS_TABLE$classe)

FILENAME_METRICS <- "10_{territory}_deforisk_risk_classes.csv"

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
# LANGS <- c("fr", "es", "pt", "en")   # "pt" | "es" | "fr" | "en"
LANGS <- c("en")

LABELS <- list(
  # Axes
  y_area_pct = c(
    fr = "Pourcentage de la surface",
    es = "Porcentaje del área",
    pt = "Porcentagem da área",
    en = "Percentage of area"
  ),
  x_score = c(
    fr = "Risque normalisé (0-1)",
    es = "Riesgo normalizado (0-1)",
    pt = "Risco normalizado (0-1)",
    en = "Normalized risk (0-1)"
  )
)

# Dynamic label function
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Helpers & theme ----
# ------------------------------------------------------------------------- - - -
format_number_fr <- function(x, digits = 0) {
  if (length(x) == 0) return(character(0))
  digits <- rep_len(digits, length.out = length(x))
  vapply(
    seq_along(x),
    function(i) {
      val <- x[i]
      dig <- digits[i]
      if (is.na(val) || !is.finite(val)) {
        "--"
      } else {
        trimws(formatC(
          round(val, dig),
          format = "f",
          big.mark = " ",
          decimal.mark = ",",
          digits = dig
        ))
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

format_percent_fr <- function(x, digits = 2) {
  if (length(x) == 0) return(character(0))
  vals <- format_number_fr(x * 100, digits = digits)
  ifelse(vals == "--", "--", paste0(vals, " %"))
}

format_score_range <- function(min_val, max_val) {
  min_txt <- format_number_fr(min_val, digits = 2)
  max_txt <- format_number_fr(max_val, digits = 2)
  glue::glue("{min_txt} - {max_txt}")
}

# EN: Base theme (ultra-clean, map-friendly, transparent background)
theme_histogram_base <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.text.y        = element_text(size = 9),
      axis.title.y       = element_text(size = 10, margin = margin(r = 4), colour = "#6a6a6aff"),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.title.x       = element_blank(),
      panel.grid         = element_blank(),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      legend.position    = "none",
      plot.margin        = margin(4, 4, 4, 4),
      plot.background    = element_rect(fill = NA, colour = NA),
      panel.background   = element_rect(fill = NA, colour = NA)
    )
}

# EN: Y axis always from 0 to 100 (%)
axis_y_percent <- function(show_labels = TRUE) {
  if (show_labels) {
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20),
      labels = function(v) paste0(v, "%"),
      expand = expansion(mult = c(0, 0.02))
    )
  } else {
    scale_y_continuous(
      limits = c(0, 100),
      breaks = NULL,
      labels = NULL,
      expand = expansion(mult = c(0, 0.02))
    )
  }
}

# EN: Normalize UInt16 raster values to [0, 1]
normalize_risk <- function(values) {
  norm <- (values - 1) / (65535 - 1)
  pmax(pmin(norm, 1), 0)
}

##%###########################################################################%##
#                                                                               #
#                         2) Utility Functions                               ----
#                                                                               #
##%###########################################################################%##

## 2.1 Build class table from raster ----
# ------------------------------------------------------------------------- - - -
build_class_table <- function(rast, territory) {
  values <- terra::values(rast, mat = FALSE)
  values <- values[!is.na(values) & values > 0]

  if (length(values) == 0) {
    return(NULL)
  }

  pixel_area <- abs(prod(terra::res(rast))) / 10000  # ha

  palette_vec <- if (territory %in% c("paragominas", "madre_de_dios")) {
    c("#196e19", "#298b21", "#c9b00e", "#ffa107", "#e31a1c")
  } else {
    c("#196e19", "#228b22", "#ffa500", "#e31a1c", "#000000")
  }

  class_defs <- CLASS_TABLE %>%
    mutate(
      couleur = palette_vec,
      classe = factor(classe, levels = CLASS_TABLE$classe)
    )

  values_norm <- normalize_risk(values)

  class_id <- cut(
    values_norm,
    breaks = c(class_defs$score_min, 1),
    include.lowest = TRUE,
    right = TRUE,
    labels = class_defs$classe
  )

  class_tbl <- tibble::tibble(
    Classe = class_id
  ) %>%
    count(Classe, name = "n_pixels") %>%
    mutate(
      Classe = factor(Classe, levels = levels(CLASS_NAMES)),
      Score = purrr::map_chr(Classe, function(cls) {
        row <- class_defs[class_defs$classe == cls, ]
        format_score_range(row$score_min, row$score_max)
      }),
      surface_ha = n_pixels * pixel_area,
      part = surface_ha / sum(surface_ha, na.rm = TRUE)
    ) %>%
    arrange(Classe)

  list(
    class_tbl = class_tbl,
    class_defs = class_defs
  )
}

##%###########################################################################%##
#                                                                               #
#                         3) Main Processing Loop                            ----
#                                                                               #
##%###########################################################################%##

for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))

  for (TERRITORY in TERRITORIES) {
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")
    cat(glue("PROCESSING DEFORISK: {toupper(TERRITORY)} ({LANG})"))
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")

    # --- Input raster ------------------------------------------------------
    raster_path <- file.path(
      "deforisk", "territorios", TERRITORY,
      "outputs", "far_models", "forecast",
      "prob_icar_t3.tif"
    )

    if (!file.exists(raster_path)) {
      message(glue("⚠ Raster not found: {raster_path}"))
      next
    }

    rast <- try(terra::rast(raster_path), silent = TRUE)
    if (inherits(rast, "try-error")) {
      message(glue("⚠ Failed to read raster for {TERRITORY}: {conditionMessage(attr(rast, 'condition'))}"))
      next
    }

    message(glue("📊 Loading deforisk raster: {basename(raster_path)}"))

    res <- build_class_table(rast, TERRITORY)
    if (is.null(res)) {
      message(glue("⚠ No valid pixels inside project for {TERRITORY}"))
      next
    }

    class_tbl <- res$class_tbl
    class_defs <- res$class_defs

    # --- Save metrics table only once -------------------------------------
    if (identical(LANG, LANGS[[1]])) {
      metrics_tbl <- class_tbl %>%
        transmute(
          Classe = as.character(Classe),
          Score = Score,
          `Surface (ha)` = format_number_fr(surface_ha),
          `Part du total (%)` = format_percent_fr(part)
        )

      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
      metrics_path <- file.path(
        metrics_dir,
        glue(FILENAME_METRICS, territory = TERRITORY)
      )
      readr::write_csv(metrics_tbl, metrics_path, na = "")
      message(glue("[metrics] Saved deforisk summary for {TERRITORY}: {basename(metrics_path)}"))
    }

    # --- Build plot --------------------------------------------------------
    plot_tbl <- class_tbl %>%
      mutate(pct = part * 100)

    p <- ggplot(plot_tbl, aes(x = Classe, y = pct, fill = Classe)) +
      geom_col(width = 0.9, colour = NA) +
      scale_fill_manual(values = setNames(class_defs$couleur, class_defs$classe)) +
      scale_x_discrete(expand = c(0, 0)) +
      axis_y_percent(show_labels = TRUE) +
      labs(
        y = label("y_area_pct"),
        x = NULL
      ) +
      theme_histogram_base()

    print(p)
    message(glue("✓ Histogram generated for {TERRITORY}"))

    # --- Export ------------------------------------------------------------
    if (WRITE_PLOT) {
      dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
      file_stub <- glue("{TERRITORY}_hist_deforisk_grouped_{LANG}")

      png_path <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))
      ggsave(
        filename = png_path, plot = p,
        width = PLOT_WIDTH, height = PLOT_HEIGHT, units = UNITS,
        dpi = DPI
      )

      if (isTRUE(WRITE_SVG)) {
        svg_path <- file.path(OUTPUT_DIR, glue("{file_stub}.svg"))
        ggsave(
          filename = svg_path, plot = p,
          width = PLOT_WIDTH, height = PLOT_HEIGHT, units = UNITS,
          device = "svg", bg = "transparent"
        )
      }

      message("✅ Saved:")
      message(glue("   PNG: {basename(png_path)}"))
      if (isTRUE(WRITE_SVG)) {
        message(glue("   SVG: {basename(svg_path)}"))
      } else {
        message("   SVG: (skipped)")
      }
    } else {
      message("ℹ Preview mode — set WRITE_PLOT <- TRUE to export.")
    }

    cat("\n", paste(rep("-", 64), collapse = ""), "\n", sep = "")
  }
}
