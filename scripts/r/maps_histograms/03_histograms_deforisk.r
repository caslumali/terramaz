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

WRITE_PLOT <- TRUE
WRITE_SVG  <- TRUE

TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")
TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

UNITS       <- "mm"
DPI         <- 300
PLOT_WIDTH  <- 250
PLOT_HEIGHT <- 35
OUTPUT_DIR <- file.path("results", "maps", "histograms", "deforisk_grouped")

CLASS_TABLE <- tibble::tribble(
  ~classe,        ~score_min, ~score_max,
  "Très faible",   0.00,        0.19,
  "Faible",        0.20,        0.39,
  "Modéré",        0.40,        0.59,
  "Élevé",         0.60,        0.79,
  "Très élevé",    0.80,        1.00
)
CLASS_NAMES <- factor(CLASS_TABLE$classe, levels = CLASS_TABLE$classe)

FILENAME_METRICS <- "10_{territory}_deforisk_risk_classes.csv"

## 1.2 Labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")

LABELS <- list(
  y_area_pct = c(fr = "Pourcentage de la surface"),
  x_score    = c(fr = "Risque normalise (0-1)")
)

label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Helpers ----
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

score_label <- function(min_val, max_val) {
  glue("{format_number_fr(min_val, 2)} - {format_number_fr(max_val, 2)}")
}

theme_histogram_base <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.text.y        = element_text(size = 5),
      axis.title.y       = element_text(size = 6, margin = margin(r = 4), colour = "#6a6a6a"),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.title.x       = element_blank(),
      panel.grid         = element_blank(),
      plot.title         = element_blank(),
      legend.position    = "none",
      plot.margin        = margin(4, 4, 4, 4),
      plot.background    = element_rect(fill = NA, colour = NA),
      panel.background   = element_rect(fill = NA, colour = NA)
    )
}

axis_y_percent <- function(show_labels = TRUE) {
  scale_y_continuous(
    limits = c(0, 100),
    breaks = if (show_labels) seq(0, 100, by = 20) else NULL,
    labels = if (show_labels) function(v) paste0(v, "%") else NULL,
    expand = expansion(mult = c(0, 0.02))
  )
}

normalize_risk <- function(values) {
  norm <- (values - 1) / (65535 - 1)
  pmax(pmin(norm, 1), 0)
}

## 2) Processing loop ----
# ------------------------------------------------------------------------- - - -
for (LANG in LANGS) {
  message(glue("[lang] {LANG}"))

  for (TERRITORY in TERRITORIES) {
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")
    cat(glue("PROCESSING DEFORISK: {toupper(TERRITORY)} ({LANG})"))
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")

    raster_path <- file.path(
      "deforisk", "territorios", TERRITORY,
      "outputs", "far_models", "forecast",
      "prob_icar_t3.tif"
    )

    if (!file.exists(raster_path)) {
      message(glue("[warn] Raster not found: {raster_path}"))
      next
    }

    rast <- try(terra::rast(raster_path), silent = TRUE)
    if (inherits(rast, "try-error")) {
      message(glue("[warn] Failed to read raster for {TERRITORY}: {conditionMessage(attr(rast, 'condition'))}"))
      next
    }

    values <- terra::values(rast, mat = FALSE)
    values <- values[!is.na(values) & values > 0]

    if (length(values) == 0) {
      message(glue("[warn] No valid pixels inside project for {TERRITORY}"))
      next
    }

    pixel_area <- abs(prod(terra::res(rast))) / 10000  # ha
    palette_vec <- if (TERRITORY %in% c("paragominas", "madre_de_dios")) {
      c("#196e19", "#298b21", "#c9b00e", "#ffa107", "#e31a1c")
    } else {
      c("#196e19", "#228b22", "#ffa500", "#e31a1c", "#000000")
    }

    class_defs <- CLASS_TABLE %>%
      mutate(couleur = palette_vec, classe = factor(classe, levels = CLASS_TABLE$classe))
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
        Score  = purrr::map_chr(Classe, function(cls) {
          row <- class_defs[class_defs$classe == cls, ]
          score_label(row$score_min, row$score_max)
        }),
        surface_ha = n_pixels * pixel_area,
        part = surface_ha / sum(surface_ha, na.rm = TRUE)
      ) %>%
      arrange(Classe)

    metrics_tbl <- class_tbl %>%
      transmute(
        Classe = as.character(Classe),
        Score  = Score,
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
    message(glue("[metrics] Saved deforisk summary to {basename(metrics_path)}"))

    plot_tbl <- class_tbl %>%
      mutate(pct = part * 100)

    axis_text_size <- if (TERRITORY == "cotriguacu") 7 else 5
    axis_title_size <- if (TERRITORY == "cotriguacu") 9 else 6

    p <- ggplot(plot_tbl, aes(x = Classe, y = pct, fill = Classe)) +
      geom_col(width = 0.9, colour = NA) +
      scale_fill_manual(values = setNames(class_defs$couleur, class_defs$classe)) +
      scale_x_discrete(expand = c(0, 0)) +
      axis_y_percent(show_labels = TRUE) +
      labs(
        y = label("y_area_pct"),
        x = NULL
      ) +
      theme_histogram_base() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.y = element_text(size = axis_title_size)
      )

    print(p)
    message(glue("[plot] Histogram generated for {TERRITORY}"))

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
          device = svglite, bg = "transparent"
        )
        message(glue("[plot] Saved SVG: {basename(svg_path)}"))
      } else {
        message("[plot] SVG export skipped")
      }

      message(glue("[plot] Saved PNG: {basename(png_path)}"))
    } else {
      message("Preview mode - set WRITE_PLOT <- TRUE to export")
    }

    cat("\n", paste(rep("-", 64), collapse = ""), "\n", sep = "")
  }
}
