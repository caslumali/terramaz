##%###########################################################################%##
#                                                                               #
#                     Forest Integrity Histograms (2024)                     ----
#                                                                               #
##%###########################################################################%##

# 1) Configuration ----
# ------------------------------------------------------------------------- - - -
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(glue)
  library(scales)
  library(svglite)
  library(rlang)
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE     # EN: Export PNGs
WRITE_SVG  <- TRUE     # EN: Also export SVGs (vector)

# Quick test: uncomment one ROI
# TERRITORIES <- c("cotriguacu")
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Output file parameters (smaller than TS plots)
FILENAME_STUB <- "hist_forest_integrity"
UNITS         <- "mm"
DPI           <- 300

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")   # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Axes
  x_score = c(fr = "Score", pt = "Score", es = "Score", en = "Score"),
  y_area_pct = c(
    fr = "Pourcentage de la surface par classe",
    es = "Porcentaje del área por clase",
    pt = "Porcentagem da área por classe",
    en = "Percentage of area per class"
  ),
  # Titles (per macroclass)
  title_macro = c(
    "10" = c(fr = "Forêt non perturbée", es = "Bosque no perturbado", pt = "Floresta não perturbada", en = "Undisturbed forest"),
    "20" = c(fr = "Forêt dégradée",      es = "Bosque degradado",     pt = "Floresta degradada",      en = "Degraded forest"),
    "30" = c(fr = "Forêt en régénération", es = "Bosque en regeneración", pt = "Floresta em regeneração", en = "Regrowth forest")
  )
)

# Dynamic label function
label <- function(key, ..., macro = NULL) {
  if (!is.null(macro)) {
    template <- LABELS[["title_macro"]][[macro]][[LANG]]
  } else {
    template <- LABELS[[key]][[LANG]]
  }
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Palette for macroclasses & scores ----
# ------------------------------------------------------------------------- - - -

score_colors <- c(
  # Macroclass 10 (Undisturbed)
  "10_100" = "#194300",
  "10_97"  = "#145700",
  "10_94"  = "#0e6b00",
  "10_91"  = "#098000",

  # Macroclass 20 (Degraded)
  "20_100" = "#0b9a00",
  "20_97"  = "#129d02",
  "20_94"  = "#199f05",
  "20_91"  = "#20a207",
  "20_90"  = "#27a50a",
  "20_87"  = "#2ea70c",
  "20_84"  = "#35aa0e",
  "20_81"  = "#3cad11",
  "20_80"  = "#44af13",
  "20_77"  = "#4bb216",
  "20_74"  = "#52b518",
  "20_71"  = "#59b71a",
  "20_70"  = "#60ba1d",
  "20_67"  = "#67bc1f",
  "20_64"  = "#6ebf22",
  "20_61"  = "#75c224",
  "20_60"  = "#7cc426",
  "20_57"  = "#83c729",
  "20_54"  = "#8aca2b",
  "20_51"  = "#91cc2e",
  "20_50"  = "#99cf30",
  "20_47"  = "#a0d132",
  "20_44"  = "#a7d435",
  "20_41"  = "#aed737",

  # Macroclass 30 (Regrowth)
  "30_100" = "#c9e74f",
  "30_97"  = "#cce959",
  "30_94"  = "#d0eb63",
  "30_91"  = "#d4ee6e",
  "30_90"  = "#d8f078",
  "30_87"  = "#dcf283",
  "30_84"  = "#dff48d",
  "30_81"  = "#e3f697",
  "30_80"  = "#e7f8a2",
  "30_77"  = "#ebfbac",
  "30_74"  = "#effdb7",
  "30_71"  = "#f3ffc1"
)


## 1.4 Theme & scales ----
# ------------------------------------------------------------------------- - - -

# Base theme for all histogram panels (ultra-clean, map-friendly)
theme_histogram_base <- function() {
theme_minimal(base_size = 12) +
  theme(
    axis.text.y        = element_text(size = 5),
    axis.title.y       = element_text(size = 6, margin = margin(r = 4)), 
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

    # No background or borders
    plot.background   = element_rect(fill = NA, colour = NA),
    panel.background  = element_rect(fill = NA, colour = NA),
    strip.background  = element_blank(),

    panel.spacing.x   = unit(0.1, "pt"),
    panel.border      = element_blank(),
    strip.text.x      = element_blank()
  )

}

# Y axis always from 0 to 100 (%)
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

##%###########################################################################%##
#                                                                               #
#                         2) Utility Functions                               ----
#                                                                               #
##%###########################################################################%##

## 2.1 Load and normalize histogram CSV ----
# ------------------------------------------------------------------------- - - -
# Reads a forest integrity histogram CSV for one territory, normalizes by macroclass,
# and returns a tidy df with percentages.
load_histogram_csv <- function(csv_path) {
  df <- suppressMessages(readr::read_csv(csv_path, show_col_types = FALSE))
  
  # Defensive checks
  if (!all(c("macroclasse", "score_final", "area_ha") %in% tolower(names(df)))) {
    stop(glue::glue("CSV does not contain expected columns: {basename(csv_path)}"))
  }
  
  # Harmonize column names
  df <- df %>%
    rename_with(tolower) %>%
    rename(
      macroclass = macroclasse,
      score      = score_final,
      area_ha    = area_ha
    ) %>%
    select(macroclass, score, area_ha)
  
  # Normalize within each macroclass
  df_norm <- df %>%
    group_by(macroclass) %>%
    mutate(
      area_ha   = as.numeric(area_ha),
      pct       = 100 * area_ha / sum(area_ha, na.rm = TRUE)
    ) %>%
    ungroup()
  
  df_norm
}


##%###########################################################################%##
#                                                                               #
#                         3) Main Processing Loop                            ----
#                                                                               #
##%###########################################################################%##

for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))
  
  for (TERRITORY in TERRITORIES) {
    
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    cat(glue("PROCESSING: {toupper(TERRITORY)} ({LANG})"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    
    # Input/output dirs
    INPUT_DIR  <- file.path("results/metrics", TERRITORY)
    OUTPUT_DIR <- file.path("results/maps/hist/forest_integrity")
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
    
    # Locate CSV
    main_csv <- list.files(
      INPUT_DIR,
      pattern = glue("^{TERRITORY}_hist_forest_integrity_2024.*\\.csv$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(main_csv) == 0) {
      message(glue("⚠ CSV not found for {TERRITORY} in {INPUT_DIR} — skipping."))
      next
    }
    
    message(glue("📊 Loading histogram: {basename(main_csv[1])}"))
    df <- load_histogram_csv(main_csv[1])

    # Order macroclasses 
    df <- df %>% mutate(macroclass = factor(macroclass, levels = c(10, 20, 30)))
    
    # Defensive check
    if (nrow(df) == 0) {
      message(glue("⚠ Empty df after normalization for {TERRITORY} — skipping."))
      next
    }
    
    # Build single dataframe for all macroclasses (with fill_key)
    score_levels <- factor(sort(unique(df$score), decreasing = TRUE),
                          levels = sort(unique(df$score), decreasing = TRUE))

    df_all <- df %>%
      arrange(macroclass, desc(score)) %>%
      mutate(
        score    = factor(score, levels = sort(unique(score), decreasing = TRUE)),
        fill_key = paste0(macroclass, "_", score)
      )
    
    # # Big facet histogram
    # p <- ggplot(df_all, aes(x = score, y = pct, fill = fill_key)) +
    #   geom_col(width = 0.9) +
    #   scale_fill_manual(values = score_colors, guide = "none") + 
    #   axis_y_percent(show_labels = TRUE) +
    #   scale_x_discrete(drop = TRUE) +
    #   facet_grid(. ~ macroclass, scales = "free_x", space = "free_x") +
    #   labs(y = label("y_area_pct"), x = NULL) +
    #   theme_histogram_base()

    # Preserve a numeric copy of score to sort truly in descending order within each macroclass
    df_all <- df_all %>%
      mutate(
        score_num = as.numeric(as.character(score))  # keep numeric for ordering
      )

    # Build the x levels: macroclass order fixed (10,20,30) and score strictly descending inside each
    levels_x <- df_all %>%
      mutate(macroclass = factor(macroclass, levels = c(10, 20, 30))) %>%
      arrange(macroclass, desc(score_num)) %>%          # use numeric for true descending
      transmute(x = paste0(macroclass, "_", score)) %>%
      pull(x) %>%
      unique()

    # Final factor for compact single-panel x; no dropped levels
    df_all <- df_all %>%
      mutate(x_comb = factor(paste0(macroclass, "_", score), levels = levels_x))

    # EN: Single-panel plot; bars flush (width=1), no facet, no titles, keep your palette via fill_key
    p <- ggplot(df_all, aes(x = x_comb, y = pct, fill = fill_key)) +
      geom_col(width = 0.9) +                             
      scale_fill_manual(values = score_colors, guide = "none") +
      axis_y_percent(show_labels = TRUE) +
      scale_x_discrete(expand = c(0, 0), drop = FALSE) +  # zero padding, keep all levels
      labs(y = label("y_area_pct"), x = NULL) +
      theme_histogram_base() +
      theme(axis.text.x = element_blank())

    print(p)
    message(glue("✓ Histogram generated for {TERRITORY}"))
    
    # Export
    if (WRITE_PLOT) {
      file_stub <- glue("01_{TERRITORY}_hist_forest_integrity_{LANG}")
      
      # n_bars <- length(levels(df_all$x_comb)) 
      # bar_width_mm <- 10     # largura de cada barra em mm (ajustável)
      # fig_width_mm <- n_bars * bar_width_mm
      # fig_height_mm <- 40   # altura fixa

     # PNG
      png_path <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))
      ggsave(
        filename = png_path, plot = p,
        width = 250, height = 35, units = "mm",
        dpi = DPI
      )        
      # SVG
      if (isTRUE(WRITE_SVG)) {
        svg_path <- file.path(OUTPUT_DIR, glue("{file_stub}.svg"))
        ggsave(
          filename = svg_path, plot = p,
          width  = 250, height = 35, units = "mm",
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
    
    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}
