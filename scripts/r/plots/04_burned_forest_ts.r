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
FIG_HEIGHT_MM <- 152.4   # 6 in  — consistent with other figures
UNITS         <- "mm"
DPI           <- 300

# Time window for burned series
BURN_YEAR_MIN <- 2001
BURN_YEAR_MAX <- 2024

# Territories considered "Brazil" (use MapBiomas+GLAD fire in caption)
BRAZIL_TERRITORIES <- c("cotriguacu", "paragominas")

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("pt")  # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Titles
  title_burned_in = c(
    fr = "Évolution annuelle de la forêt brûlée à {territory}",
    es = "Evolución anual del bosque quemado en {territory}",
    pt = "Evolução anual da floresta queimada em {territory}",
    en = "Annual evolution of burned forest in {territory}"
  ),
  # Axes
  x_year = c(fr = "Année", pt = "Ano", es = "Año", en = "Year"),
  y_area_ha = c(
    fr = "Surface (ha)",
    es = "Área (ha)",
    pt = "Área (ha)",
    en = "Area (ha)"
  ),
  # Legend (single series label)
  legend_burned = c(
    fr = "Surface brûlée",
    es = "Área quemada",
    pt = "Área queimada",
    en = "Burned area"
  )
)

# Helper to resolve translated strings with glue()
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Palette (single source) ----
# ------------------------------------------------------------------------- - - -
# Use a "fire red" and keep the internal key stable ("burned") — the visible label is localized.
source_line_colors <- c(burned = "#d62728")
source_line_types  <- c(burned = "solid")

## 1.4 Theme & scales (consistent with other series) ----
# ------------------------------------------------------------------------- - - -
theme_time_series <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x         = element_text(size = 11, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 6)),
      axis.text.y         = element_text(size = 11),
      axis.title.x        = element_text(size = 12, margin = margin(t = 12)),
      axis.title.y        = element_text(size = 12, margin = margin(r = 12)),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position     = "top",
      legend.justification= "right",
      legend.direction    = "horizontal",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 12),
      legend.key.size     = unit(2.5, "lines"),
      plot.margin         = margin(12, 12, 12, 12),
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

# Pretty Y axis in full hectares (no scientific, nice thousands separators)
axis_y_ha_auto <- function(y_max_raw) {
  ymax <- max(0, as.numeric(y_max_raw))
  if (!is.finite(ymax) || ymax <= 0) ymax <- 10000

  # Clean steps: 5k, 10k, 20k, 50k, 100k, 200k, 500k, 1M…
  steps <- c(5000, 10000, 20000, 50000, 100000, 200000, 500000, 1000000)
  target_n <- 6
  step <- steps[ which.min(abs((ceiling(ymax/steps)+1) - target_n)) ]
  ymax <- ceiling(ymax/step)*step

  ggplot2::scale_y_continuous(
    breaks = seq(0, ymax, by = step),
    labels = function(v) format(v, big.mark = " ", scientific = FALSE, trim = TRUE),
    limits = c(0, ymax),
    expand = expansion(mult = c(0.03, 0.03))
  )
}

##%###########################################################################%##
#                                                                               #
#                         2) Utility Functions                               ----
#                                                                               #
##%###########################################################################%##

## 2.1 Map CSV column names to the burned series ----
# ------------------------------------------------------------------------- - - -
# Priority: exact 'combined_fire_ha'. Otherwise, accept any '*fire|burn*_ha' or '*_area_ha'
map_col_to_burned <- function(colname) {
  z <- tolower(colname)
  if (z == "combined_fire_ha") return("burned")
  if (str_detect(z, "(fire|burn)") && str_detect(z, "(_area_ha|_ha)$")) return("burned")
  NA_character_
}

## 2.2 Caption (conditional on territory) ----
# ------------------------------------------------------------------------- - - -
# Brazil territories -> "MapBiomas + GLAD fire: 2001–2024"
# Others -> "MODIS + GLAD fire: 2001–2024"
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
      "fr" = glue("MapBiomas {connector} GLAD fire : 2001–2024"),
      "pt" = glue("MapBiomas {connector} GLAD fire: 2001–2024"),
      "es" = glue("MapBiomas {connector} GLAD fire: 2001–2024"),
      "en" = glue("MapBiomas {connector} GLAD fire: 2001–2024"),
      glue("MapBiomas {connector} GLAD fire: 2001–2024")
    )
  } else {
    core <- switch(lang,
      "fr" = glue("MODIS {connector} GLAD fire : 2001–2024"),
      "pt" = glue("MODIS {connector} GLAD fire: 2001–2024"),
      "es" = glue("MODIS {connector} GLAD fire: 2001–2024"),
      "en" = glue("MODIS {connector} GLAD fire: 2001–2024"),
      glue("MODIS {connector} GLAD fire: 2001–2024")
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
      pattern = glue("^{TERRITORY}_burned_.*\\.csv$"),
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

    # Candidate *_ha columns for burned area (prio: combined_fire_ha)
    ha_cols_all <- names(df)[str_detect(tolower(names(df)), "(_area_ha|_ha)$")]
    if (length(ha_cols_all) == 0) {
      message("⚠ No '*_ha' columns found — skipping.")
      next
    }

    # Map columns to "burned" and keep only those
    mapped <- vapply(ha_cols_all, map_col_to_burned, character(1))
    ha_cols <- ha_cols_all[!is.na(mapped)]
    if (length(ha_cols) == 0) {
      message(glue("⚠ No burned area columns matched (looked for 'combined_fire_ha' or '*fire|burn*_ha'). Found: {paste(ha_cols_all, collapse=', ')}"))
      next
    }

    # If multiple matched, prefer the exact 'combined_fire_ha'
    if ("combined_fire_ha" %in% tolower(ha_cols)) {
      ix <- which(tolower(ha_cols) == "combined_fire_ha")[1]
      ha_cols <- ha_cols[ix]
    } else {
      # otherwise take the first mapped column (but log it)
      message(glue("ℹ Using burned column: {ha_cols[1]}"))
      ha_cols <- ha_cols[1]
    }
    message(glue("✓ Burned columns: {paste(ha_cols, collapse=', ')}"))

    # Make long and tidy
    long_main <- df %>%
      select(all_of(c(year_col, ha_cols))) %>%
      pivot_longer(cols = all_of(ha_cols), names_to = "var", values_to = "area_ha") %>%
      mutate(
        source_used = "burned",                       # stable key
        year        = as.integer(.data[[year_col]]),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      filter(!is.na(year), !is.na(area_ha)) %>%
      select(year, source_used, area_ha) %>%
      arrange(year, source_used)

    # Keep only the window 2001–2024
    long_main <- long_main %>%
      filter(year >= BURN_YEAR_MIN, year <= BURN_YEAR_MAX)

    if (nrow(long_main) == 0) {
      message(glue("⚠ No data rows within {BURN_YEAR_MIN}–{BURN_YEAR_MAX} — skipping."))
      next
    }

    ### 3.2 Filter, cast, and clean ----
    # ----------------------------------------------------------------------- - - -
    df_long <- long_main %>%
      mutate(
        source_used = factor(source_used, levels = "burned"),
        year        = as.integer(year),
        area_ha     = pmax(suppressWarnings(as.numeric(area_ha)), 0)
      ) %>%
      arrange(year, source_used)

    # Optional: trim leading zeros to avoid a long flat line glued at 0 at the start
    df_long <- df_long %>%
      group_by(source_used) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after cleaning — skipping.")
      next
    }

    present_sources <- levels(droplevels(df_long$source_used))  # "burned"
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    message(glue("✓ Source present: {paste(present_sources, collapse=', ')}"))
    message(glue("✓ Year range (clipped): {year_min}–{year_max} (n={nrow(df_long)})"))

    # Final subset for plotting
    df_plot <- df_long %>% filter(!is.na(area_ha)) %>% droplevels()
    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]
    leg_label <- label("legend_burned")  # localized legend string

    ### 3.3 Plot ----
    # ----------------------------------------------------------------------- - - -
    p <- ggplot(df_plot, aes(x = year, y = area_ha, color = source_used, linetype = source_used)) +
      geom_line(linewidth = 1.2, lineend = "round", na.rm = TRUE) +
      geom_point(size = 4, stroke = 0, na.rm = TRUE) +
      scale_color_manual(
        values = cols,
        breaks = "burned",
        labels = c(burned = leg_label),
        guide  = guide_legend(title = NULL)
      ) +
      scale_linetype_manual(
        values = ltys,
        breaks = "burned",
        labels = c(burned = leg_label),
        guide  = "none"
      ) +
      # Force full burned window on the axis for consistency across territories
      axis_x_years_all(BURN_YEAR_MIN, BURN_YEAR_MAX) +
      # axis_y_thousands_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      axis_y_ha_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_burned_in", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_burned(TERRITORY, LANG)
      ) +
      theme_time_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 3.4 Export (fixed full-page size) ----
    # ----------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue("04_{TERRITORY}_burned_{LANG}")  # keep numeric prefix distinct

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
