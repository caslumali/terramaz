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
  library(purrr)
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

UNITS         <- "mm"
DPI           <- 300

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
# LANGS <- c("fr", "es", "pt", "en")   # "pt" | "es" | "fr" | "en"
LANGS <- c("fr")

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
  title_macro = list(
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

CLASS_LABELS_FR <- tibble::tibble(
  macroclass = c(10, 10, 10, 20, 20, 20, 20, 20, 20, 30, 30, 30),
  score = c(94, 97, 100, 100, 90, 80, 70, 60, 50, 100, 90, 80),
  class_fr = c(
    "Forêt non perturbée bord/îlot",
    "Forêt non perturbée perforation",
    "Forêt non perturbée cœur",
    "Forêt dégradée courte durée (commencée avant 2015)",
    "Forêt dégradée courte durée (commencée en 2015–2024)",
    "Forêt dégradée longue durée (commencée avant 2015)",
    "Forêt dégradée longue durée (commencée en 2015–2024)",
    "Forêt dégradée 2/3 périodes courtes (dernière avant 2015)",
    "Forêt dégradée 2/3 périodes courtes (dernière en 2015–2024)",
    "Forêt en régénération ancienne (perturbée avant 2005)",
    "Forêt en régénération jeune (perturbée en 2005–2014)",
    "Forêt en régénération très jeune (perturbée en 2015–2022)"
  )
)

## 1.3 Palette for macroclasses & scores ----
# ------------------------------------------------------------------------- - - -

score_colors <- c(
  # Macroclass 10 (Undisturbed)
  "10_100" = "#007f66",
  "10_97"  = "#54aa66",
  "10_94"  = "#aad466",
  "10_91"  = "#ffff66",

  # Macroclass 20 (Degraded)
  "20_100" = "#b30000",  # cor original do 100
  "20_90"  = "#d43323",  # cor original do 90
  "20_80"  = "#ed6442",  # cor original do 80
  "20_70"  = "#fc925d",  # cor original do 70
  "20_60"  = "#fdbe7f",  # cor original do 60
  "20_50"  = "#fdddb0",  # cor original do 50

  # Macroclass 30 (Regrowth)
  "30_100" = "#810f7c",  # cor original do 100
  "30_90"  = "#8a73b5",  # cor original do 90
  "30_80"  = "#afc8e0"  # cor original do 80
)


## 1.4 Theme & scales ----
# ------------------------------------------------------------------------- - - -

# Base theme for all histogram panels (ultra-clean, map-friendly)
theme_histogram_base <- function() {
theme_minimal(base_size = 12) +
  theme(
    axis.text.y        = element_text(size = 5),
    axis.title.y       = element_text(size = 6, margin = margin(r = 4), color = "#6a6a6aff"),
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
    mutate(
      # recodificação: arredonda sempre para o "dezenal superior"
      score_grouped = case_when(
        macroclass == 10 & score %in% c(91, 94) ~ 94,
        macroclass %in% c(20, 30) ~ ceiling(score / 10) * 10,
        TRUE ~ score
      )
    ) %>%
    group_by(macroclass, score_grouped) %>%
    summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(score = score_grouped)

  
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
    OUTPUT_DIR <- file.path("results/maps/histograms/forest_integrity_grouped")
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

    if (identical(LANG, LANGS[[1]])) {
      total_area <- sum(df$area_ha, na.rm = TRUE)

      metrics_tbl <- df %>%
        mutate(
          macroclass_num = as.integer(macroclass),
          score_value = as.numeric(score)
        ) %>%
        left_join(
          CLASS_LABELS_FR,
          by = c("macroclass_num" = "macroclass", "score_value" = "score")
        ) %>%
        mutate(
          class_fr = dplyr::if_else(
            is.na(class_fr),
            glue::glue("Classe {macroclass_num} ({score_value})"),
            class_fr
          ),
          macro_fr = dplyr::recode(
            as.character(macroclass_num),
            "10" = LABELS$title_macro[["10"]][["fr"]],
            "20" = LABELS$title_macro[["20"]][["fr"]],
            "30" = LABELS$title_macro[["30"]][["fr"]],
            .default = NA_character_
          ),
          part_macroclasse_pct = pct,
          part_totale_pct = if (total_area > 0) 100 * area_ha / total_area else NA_real_
        ) %>%
        arrange(macroclass_num, dplyr::desc(score_value)) %>%
        transmute(
          `Macroclasse (fr)` = macro_fr,
          `Classe (fr)` = class_fr,
          `Score agrégé` = score_value,
          `Part dans la macroclasse (%)` = part_macroclasse_pct,
          `Part du total (%)` = part_totale_pct
        )

      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
      metrics_path <- file.path(
        metrics_dir,
        glue("08_{TERRITORY}_forest_integrity_grouped_metrics.csv")
      )
      readr::write_csv(metrics_tbl, metrics_path, na = "")
      message(
        glue("[metrics] Saved forest integrity grouped summary for {TERRITORY}: {basename(metrics_path)}")
      )
    }

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
      theme(
        axis.text.x  = element_blank(),
        axis.text.y  = element_text(size = if (TERRITORY == "cotriguacu") 7 else 5),
        axis.title.y = element_text(size = if (TERRITORY == "cotriguacu") 9 else 6)
      )

    print(p)
    message(glue("✓ Histogram generated for {TERRITORY}"))
    
    # Export
    if (WRITE_PLOT) {
      file_stub <- glue("{TERRITORY}_hist_forest_integrity_grouped_{LANG}")
      
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



