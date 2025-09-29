##%###########################################################################%##
#                                                                               #
#                            Burned Forest Time Series                       ----
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
  library(cowplot)   # NEW: to stack two independent plots (FO on top, NF below)
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE
WRITE_SVG  <- FALSE

# Choose the territories you want to render
# TERRITORIES <- c("cotriguacu")  # quick test
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

# Pretty labels for plot titles
TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Fixed full-page export size (A4-width figures in your layout)
FILENAME_STUB <- "burned_ts"
FIG_WIDTH_MM  <- 431.8   # 17 in — full page width
FIG_HEIGHT_MM <- 190
UNITS         <- "mm"
DPI           <- 300

# Time window for burned series
BURN_YEAR_MIN <- 2001
BURN_YEAR_MAX <- 2024

# Territories considered "Brazil" (use MapBiomas+GLAD fire in caption)
BRAZIL_TERRITORIES <- c("cotriguacu", "paragominas")

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Titles
  title_burned_in = c(
    fr = "Évolution annuelle de la surface brûlée à {territory}",
    es = "Evolución anual del área quemada en {territory}",
    pt = "Evolução anual da área queimada em {territory}",
    en = "Annual evolution of burned area in {territory}"
  ),
  # Axes
  x_year = c(fr = "Année", pt = "Ano", es = "Año", en = "Year"),
  y_area_ha = c(
    fr = "Surface (ha)",
    es = "Área (ha)",
    pt = "Área (ha)",
    en = "Area (ha)"
  ),
  # Legend texts for each panel (FO and NF)
  legend_fo = c(
    fr = "Surface de forêt brûlée",
    es = "Área de bosque quemada",
    pt = "Área de floresta queimada",
    en = "Burned forest area (ha)"
  ),
  legend_nf = c(
    fr = "Surface de non-forêt brûlée",
    es = "Área de no bosque quemada",
    pt = "Área de não-floresta queimada",
    en = "Burned non-forest area"
  ),
  panel_fo = c(fr = "Forêt brûlée",      pt = "Floresta queimada",     es = "Bosque quemado",     en = "Burned forest"),
  panel_nf = c(fr = "Non-forêt brûlée",  pt = "Não-floresta queimada", es = "No bosque quemado", en = "Burned non-forest")
)

# Helper to resolve translated strings with glue()
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Palette (single source) ----
# ------------------------------------------------------------------------- - - -
source_line_colors <- c(
  fo = "#d62728",  # fire red — forest
  nf = "#ff7f0e"   # orange — non-forest
)
source_line_types  <- c(
  fo = "solid",
  nf = "dashed"
)

## 1.4 Theme & scales (consistent with other series) ----
# ------------------------------------------------------------------------- - - -
theme_time_series <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle       = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 6)),
      axis.text.x         = element_text(size = 11, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 6)),
      axis.text.y         = element_text(size = 11),
      axis.title.x        = element_text(size = 12, margin = margin(t = 12)),
      axis.title.y        = element_text(size = 12, margin = margin(r = 12)),
      axis.title.y.right  = element_text(size = 12, margin = margin(l = 12)),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position     = "top",
      legend.justification= "right",
      legend.direction    = "horizontal",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 12),
      legend.key.size     = unit(2.2, "lines"),
      plot.margin         = margin(10, 12, 6, 12),  # EN: slightly tighter; we’ll add extra gap via cowplot
      plot.caption        = element_text(hjust = 1, size = 10, color = "gray30", margin = margin(t = 12))
    )
}

axis_x_years_all <- function(year_min, year_max) {
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 1),
    expand = expansion(mult = c(0.01, 0.02))
  )
}

# Adaptive Y scale: friendly ticks from small to large values (in 10^3 ha labels)
# axis_y_thousands_auto <- function(y_max_raw) {
#   ymax <- max(0, as.numeric(y_max_raw))
#   if (!is.finite(ymax) || ymax <= 0) ymax <- 1000

#   if (ymax <= 2500) {
#     step_candidates <- c(100, 200, 250, 500, 1000, 2000)
#     label_acc <- 0.1
#   } else if (ymax <= 7000) {
#     step_candidates <- c(500, 1000, 2000, 2500, 5000)
#     label_acc <- 0.5
#   } else {
#     step_candidates <- c(1000, 2000, 5000, 10000, 20000, 50000)
#     label_acc <- 1
#   }

#   target_n <- 7
#   n_breaks <- ceiling(ymax / step_candidates) + 1
#   step <- step_candidates[ which.min(abs(n_breaks - target_n)) ]
#   ymax <- ceiling(ymax / step) * step

#   ggplot2::scale_y_continuous(
#     breaks = seq(0, ymax, by = step),
#     labels = function(v) scales::number(v / 1000, accuracy = label_acc, big.mark = " "),
#     limits = c(0, ymax),
#     expand = expansion(mult = c(0.03, 0.03))
#   )
# }

##%###########################################################################%##
#                                                                               #
#                         2) Utility Functions                               ----
#                                                                               #
##%###########################################################################%##

## 2.1 Map CSV column names to the burned series ----
# ------------------------------------------------------------------------- - - -
# Priority: exact 'combined_fire_ha'. Otherwise, accept any '*fire|burn*_ha' or '*_area_ha'
map_cols_to_fo_nf <- function(colnames) {
  nms <- tolower(colnames)

  # FO candidates
  fo_idx <- which(stringr::str_detect(nms, "combined") &
                  stringr::str_detect(nms, "fire") &
                  stringr::str_detect(nms, "fo") &
                  (stringr::str_detect(nms, "(_area_ha|_ha)$") | TRUE))
  # NF candidates
  nf_idx <- which(stringr::str_detect(nms, "combined") &
                  stringr::str_detect(nms, "fire") &
                  stringr::str_detect(nms, "nf") &
                  (stringr::str_detect(nms, "(_area_ha|_ha)$") | TRUE))

  fo_col <- if (length(fo_idx)) colnames[fo_idx[1]] else NA_character_
  nf_col <- if (length(nf_idx)) colnames[nf_idx[1]] else NA_character_

  list(fo = fo_col, nf = nf_col)
}

## 2.2 Caption (conditional on territory) ----
# ------------------------------------------------------------------------- - - -
# Brazil territories -> "MapBiomas + GLAD fire: 2001-2024"
# Others -> "MODIS + GLAD fire: 2001-2024"
caption_burned <- function(territory, lang = "fr") {
  is_br <- tolower(territory) %in% BRAZIL_TERRITORIES
  
  connector <- switch(lang,
    "fr" = "combiné avec",
    "pt" = "combinado com",
    "es" = "combinado con",
    "en" = "combined with",
    "combined with"
  )
  
  
  if (is_br) {
    core <- switch(lang,
      "fr" = glue("MapBiomas {connector} GLAD fire : 2001-2024"),
      "pt" = glue("MapBiomas {connector} GLAD fire: 2001-2024"),
      "es" = glue("MapBiomas {connector} GLAD fire: 2001-2024"),
      "en" = glue("MapBiomas {connector} GLAD fire: 2001-2024"),
      glue("MapBiomas {connector} GLAD fire: 2001-2024")
    )
  } else {
    core <- switch(lang,
      "fr" = glue("MODIS {connector} GLAD fire : 2001-2024"),
      "pt" = glue("MODIS {connector} GLAD fire: 2001-2024"),
      "es" = glue("MODIS {connector} GLAD fire: 2001-2024"),
      "en" = glue("MODIS {connector} GLAD fire: 2001-2024"),
      glue("MODIS {connector} GLAD fire: 2001-2024")
    )
  }
  
  prefix <- switch(lang,
    "fr" = "Sources — ",
    "pt" = "Fontes — ",
    "es" = "Fuentes — ",
    "en" = "Sources — ",
    "Sources — "
  )
  
  paste0(prefix, core)
}


## 2.3 Trim leading zeros (avoid flat lines glued at zero) ----
# ------------------------------------------------------------------------- - - -
trim_leading_zeros <- function(y) {
  ix <- which(!is.na(y) & y > 0)
  if (length(ix) == 0) return(rep(NA_real_, length(y)))
  cut <- min(ix)
  out <- y
  out[seq_len(cut - 1)] <- NA_real_
  out
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
    cat(glue("PROCESSING: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")

    INPUT_DIR  <- file.path("results/metrics", TERRITORY)
    # Output in a territory/lang subfolder to help partners find assets easily
    OUTPUT_DIR <- file.path("results/plots", TERRITORY, glue("{TERRITORY}_{LANG}"))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 3.1 Load burned CSV ----
    # ----------------------------------------------------------------------- - - -
    # Expect pattern like: "<territory>_burned_forest_2001_2024.csv"
    main_csv <- list.files(
      INPUT_DIR,
      pattern = glue("^{TERRITORY}_burned_fo_nf.*\\.csv$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(main_csv) == 0) {
      message(glue("⚠ CSV not found for {TERRITORY} in {INPUT_DIR} — skipping."))
      next
    }
    message(glue("📊 Loading: {basename(main_csv[1])}"))
    df <- suppressMessages(readr::read_csv(main_csv[1], show_col_types = FALSE))

    # Robust year column detection
    nms_lc <- tolower(names(df))
    year_candidates <- c("year","ano","yr","year_int")
    year_idx <- match(year_candidates, nms_lc, nomatch = 0)
    if (!any(year_idx > 0)) {
      message(glue("⚠ Year column not found. Available: {paste(names(df), collapse=', ')}"))
      next
    }
    year_col <- names(df)[year_idx[year_idx > 0][1]]
    message(glue("✓ Using year column: '{year_col}'"))

    # We detect both FO (forest) and NF (non-forest) burned area columns.
    nms_original <- names(df)
    map <- map_cols_to_fo_nf(nms_original)

    if (is.na(map$fo) || is.na(map$nf)) {
      message(glue::glue(
        "⚠ Could not find both FO/NF columns. FO='{map$fo}', NF='{map$nf}'.\nAvailable: {paste(nms_original, collapse=', ')}"
      ))
      next
    }
    message(glue::glue("✓ FO column: '{map$fo}'  |  NF column: '{map$nf}'"))

    # EN: Build long format with a stable key 'series' (fo|nf), keep only 2001–2024.
    long_main <- df %>%
      dplyr::select(all_of(c(year_col, map$fo, map$nf))) %>%
      tidyr::pivot_longer(
        cols = all_of(c(map$fo, map$nf)),
        names_to = "var", values_to = "area_ha"
      ) %>%
      dplyr::mutate(
        series  = dplyr::if_else(stringr::str_detect(tolower(.data$var), "nf"), "nf", "fo"),
        year    = as.integer(.data[[year_col]]),
        area_ha = suppressWarnings(as.numeric(area_ha))
      ) %>%
      dplyr::filter(!is.na(year), !is.na(area_ha)) %>%
      dplyr::select(year, series, area_ha) %>%
      dplyr::arrange(year, series) %>%
      dplyr::filter(year >= BURN_YEAR_MIN, year <= BURN_YEAR_MAX)


    if (nrow(long_main) == 0) {
      message(glue("⚠ No data rows within {BURN_YEAR_MIN}-{BURN_YEAR_MAX} — skipping."))
      next
    }

    ### 3.2 Filter, cast, and clean ----
    # ----------------------------------------------------------------------- - - -
    df_long <- long_main %>%
      dplyr::mutate(
        series  = factor(series, levels = c("fo","nf")),
        year    = as.integer(year),
        area_ha = pmax(suppressWarnings(as.numeric(area_ha)), 0)
      ) %>%
      dplyr::arrange(series, year)

    df_long <- df_long %>%
      dplyr::group_by(series) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      dplyr::mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      dplyr::ungroup()

    present_series <- levels(droplevels(df_long$series))  # "fo","nf"
    cols <- source_line_colors[present_series]
    ltys <- source_line_types[present_series]

    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    message(glue("✓ Series present: {paste(present_series, collapse=', ')}"))

    message(glue("✓ Year range (clipped): {year_min}-{year_max} (n={nrow(df_long)})"))

    # Final subset for plotting
    df_plot <- df_long %>% filter(!is.na(area_ha)) %>% droplevels()
    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]
    leg_label <- label("legend_burned")  # localized legend string

    ### 3.3 Plot (single panel, two lines with dual Y-axes) ----
    # -------------------------------------------------------------------------
    # EN: We plot FO (forest) on the left Y-axis (red). We rescale NF (non-forest)
    #     by a linear factor 'k' so it fits the left axis, and we expose a
    #     secondary right Y-axis that inverts the transform (~ . / k).
    #     This keeps both axes *linearly consistent* (no cheating).
    #     Legend shows both series with translated labels.

    # Split data per series (ensure both vectors exist even if one is all-NA)
    df_fo <- df_plot %>% dplyr::filter(series == "fo")
    df_nf <- df_plot %>% dplyr::filter(series == "nf")

    # Compute linear scaling factor (k = max(FO)/max(NF)), robust to zeros/NAs
    fo_max <- suppressWarnings(max(df_fo$area_ha, na.rm = TRUE))
    nf_max <- suppressWarnings(max(df_nf$area_ha, na.rm = TRUE))
    fo_max <- ifelse(is.finite(fo_max), fo_max, 1)
    nf_max <- ifelse(is.finite(nf_max) && nf_max > 0, nf_max, 1)
    k <- fo_max / nf_max
    if (!is.finite(k) || k <= 0) k <- 1  # EN: safety fallback

    # Optional: use a quantile-based scale to reduce outlier influence
    # qfo <- suppressWarnings(quantile(df_fo$area_ha, 0.95, na.rm = TRUE))
    # qnf <- suppressWarnings(quantile(df_nf$area_ha, 0.95, na.rm = TRUE))
    # if (is.finite(qfo) && is.finite(qnf) && qnf > 0) k <- qfo / qnf

    # Multilingual labels
    territory_title <- TERRITORY_LABELS[[TERRITORY]]
    legend_text_fo  <- label("legend_fo")  # e.g., "Área queimada na floresta"
    legend_text_nf  <- label("legend_nf")  # e.g., "Área queimada fora da floresta"

    # Colors (pull from your palette)
    col_fo <- source_line_colors[["fo"]]
    col_nf <- source_line_colors[["nf"]]

    # Build combined dataset:
    # - FO stays as-is
    # - NF is rescaled (area_ha_scaled = area_ha * k) so it fits the left axis.
    df_comb <- bind_rows(
      df_fo %>% mutate(area_ha_scaled = area_ha),            # FO on left axis
      df_nf %>% mutate(area_ha_scaled = area_ha * k)         # NF scaled to left axis
    )

    # Pretty Y axis in full hectares (no scientific, nice thousands separators)
    y_max <- max(c(df_fo$area_ha, df_nf$area_ha * k), na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 10000

    steps <- c(5000, 10000, 20000, 50000, 100000, 200000, 500000, 1000000)
    target_n <- 6
    step <- steps[which.min(abs((ceiling(y_max/steps)+1) - target_n))]
    y_max <- ceiling(y_max/step) * step

    # Plot: two lines with different aesthetics, dual axes
    p <- ggplot(df_comb, aes(x = year, y = area_ha_scaled)) +
      # NF line (scaled; still plotted against left axis, but labeled on right axis)
      geom_line(
        data = dplyr::filter(df_comb, series == "nf"),
        aes(color = "nf", linetype = "nf"),
        linewidth = 0.8, lineend = "round", na.rm = TRUE
      ) +
      geom_point(
        data = dplyr::filter(df_comb, series == "nf"),
        aes(color = "nf"),
        size = 2.5, stroke = 0, na.rm = TRUE
      ) +
      # FO line (left axis)
      geom_line(
        data = dplyr::filter(df_comb, series == "fo"),
        aes(color = "fo", linetype = "fo"),
        linewidth = 1, lineend = "round", na.rm = TRUE
      ) +
      geom_point(
        data = dplyr::filter(df_comb, series == "fo"),
        aes(color = "fo"),
        size = 3.5, stroke = 0, na.rm = TRUE
      ) +
      # Manual scales for color/linetype with clear legend labels
      scale_color_manual(
        values = c(fo = col_fo, nf = col_nf),
        breaks = c("fo", "nf"),
        labels = c(fo = legend_text_fo, nf = legend_text_nf)
      ) +
      scale_linetype_manual(
        values = c(fo = source_line_types[["fo"]], nf = source_line_types[["nf"]]),
        breaks = c("fo", "nf"),
        labels = c(fo = legend_text_fo, nf = legend_text_nf),
        guide  = "none"   # EN: hide linetype legend to avoid the extra black legend
      ) +
      # Force full time window and pretty Y scales on both axes
      axis_x_years_all(BURN_YEAR_MIN, BURN_YEAR_MAX) +
      scale_y_continuous(
        breaks = seq(0, y_max, by = step),
        labels = scales::label_number(big.mark = " ", accuracy = 1, trim = TRUE),
        limits = c(0, y_max),
        expand = expansion(mult = c(0.03, 0.03)),
        sec.axis = sec_axis(
          transform = ~ . / k,
          name  = paste0(legend_text_nf, " (ha)"),
          breaks = pretty_breaks(n = 6),
          labels = scales::label_number(big.mark = " ", accuracy = 1, trim = TRUE)
        )
      ) +
      labs(
        title   = label("title_burned_in", territory = territory_title),
        x       = label("x_year"),
        y       = paste0(legend_text_fo, " (ha)"),
        caption = caption_burned(TERRITORY, LANG)
      ) +
      theme_time_series() +
      theme(
        legend.position     = "top",
        # EN: color-code the Y axes to match the lines (clearer reading)
        axis.title.y        = element_text(color = col_fo),
        axis.text.y         = element_text(color = col_fo),
        axis.title.y.right  = element_text(color = col_nf),
        axis.text.y.right   = element_text(color = col_nf)
      )

    print(p)
    message(glue("✓ Plot (dual-axis, two lines) generated for {TERRITORY}"))

    ### 3.4 Export (fixed full-page size) ----
    # ----------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue("04_{TERRITORY}_burned_fo_nf_dualaxis_{LANG}")

      # PNG
      png_path <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))
      ggsave(
        filename = png_path, plot = p,
        width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS,
        dpi = DPI, bg = "white"
      )

      # SVG (optional)
      if (isTRUE(WRITE_SVG)) {
        svg_path <- file.path(OUTPUT_DIR, glue("{file_stub}.svg"))
        ggsave(
          filename = svg_path, plot = p,
          width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS,
          device = svglite, bg = "white"
        )
      }

      message("✅ Saved:")
      message(glue("   PNG: {basename(png_path)}  ({FIG_WIDTH_MM}×{FIG_HEIGHT_MM} mm)"))
      if (isTRUE(WRITE_SVG)) {
        message(glue("   SVG: {basename(svg_path)}  ({FIG_WIDTH_MM}×{FIG_HEIGHT_MM} mm)"))
      } else {
        message("   SVG: (skipped)")
      }
    } else {
      message("ℹ Preview mode — set WRITE_PLOT <- TRUE to export.")
    }

    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}
