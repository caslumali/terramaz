##%###########################################################################%##
#                                                                               #
#                  Forest Regrowth Indicators (Flow, Stock, Age)             ----
#                                                                               #
##%###########################################################################%##
# Title: 03_regrowth_indicators.r
# Author: Lucas Lima
#
# Purpose:
#   Generate three complementary indicators of secondary forest dynamics:
#     1) Regrowth Flow  – annual new regrowth events (line plot, TMF + MapBiomas)
#     2) Regrowth Stock – accumulated secondary forest area (bar plot, TMF + MB)
#     3) Regrowth Age   – age distribution of secondary forests in last year
#
# Key choices:
#   • Harmonized plotting window (1990–2024)
#   • TMF-JRC vs. MapBiomas coverage rules:
#       - Flow: MapBiomas until 2021 (t+2 rule), TMF until 2022
#       - Stock: MapBiomas until 2023, TMF until 2022
#       - Age: MapBiomas site (BR: 1985–2024), GEE (CO/PE: 1987–2023)
#   • Outputs generated per territory in "results/indicators/<territory>_<lang>/"
#   • Full-page width, consistent height, language-ready (PT/ES/FR/EN)
#
# Outputs:
#   • 03a_<territory>_regrowth_flow_tmf_mb_<lang>.png/svg
#   • 03b_<territory>_regrowth_stock_<dataset>_<lang>.png/svg
#   • 03c_<territory>_regrowth_age_mb_<lang>.png/svg
#
# Notes:
#   - Flow = line plots, Stock & Age = bar plots (documented here, not in filename)
#   - Captions automatically indicate data coverage (TMF vs. MapBiomas)
#
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

# Stock dataset flag:
# "tmf_mb" → shows TMF + MapBiomas |
# "tmf"    → shows just TMF
# "mb"     → shows just MapBiomas
DATASET_MODE <- "mb"  

# Territories to process
# TERRITORIES <- c("cotriguacu")  # quick test
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Filename templates
FILENAME_FLOW  <- "03a_{territory}_regrowth_flow_tmf_mb_{lang}"
FILENAME_STOCK <- "03b_{territory}_regrowth_stock_{dataset}_{lang}"
FILENAME_AGE   <- "03c_{territory}_regrowth_age_mb_{lang}"

# Fixed full-page figure size (A4 width) to match other figures
FIG_WIDTH_MM   <- 431.8   # 17 in — full page width
FIG_HEIGHT_MM  <- 152.4   # 6 in  — consistent height
UNITS          <- "mm"
DPI            <- 300

# Plot window (keep full axis to show NODATA years)
PLOT_YEAR_MIN <- 1990L
PLOT_YEAR_MAX <- 2024L

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Titles
  title_regrowth_flow = c(
    fr = "Évolution annuelle de la régénération à {territory}",
    es = "Evolución anual de la regeneración en {territory}",
    pt = "Evolução anual da regeneração em {territory}",
    en = "Annual evolution of regrowth in {territory}"
  ),
  title_regrowth_stock = c(
    fr = "Surface accumulée des forêts secondaires à {territory}",
    es = "Superficie acumulada de bosques secundarios en {territory}",
    pt = "Superfície acumulada de florestas secundárias em {territory}",
    en = "Accumulated area of secondary forests in {territory}"
  ),
  title_regrowth_age = c(
    fr = "Âge des forêts secondaires à {territory} en {last_year}",
    es = "Edad de los bosques secundarios en {territory} en {last_year}",
    pt = "Idade das florestas secundárias em {territory} em {last_year}",
    en = "Age of secondary forests in {territory} in {last_year}"
  ),
  # Axes
  x_year = c(fr = "Année", pt = "Ano", es = "Año", en = "Year"),
  x_age = c(fr = "Âge de la forêt secondaire (années)", 
            pt = "Idade da floresta secundária (anos)", 
            es = "Edad del bosque secundario (años)", 
            en = "Age of secondary forest (years)"),
  y_area_ha = c(
    fr = "Surface (ha)",
    es = "Área (ha)",
    pt = "Área (ha)",
    en = "Area (ha)"
  )
)

# Dynamic label helper
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Palette for source (line) ----
# ------------------------------------------------------------------------- - - -
source_line_colors <- c(
  `TMF-JRC`  = "#b3dc21ff",
  MapBiomas  = "#1f77b4"
)
source_line_types  <- c(
  `TMF-JRC`  = "solid",
  MapBiomas  = "solid"
)
## 1.4 Theme & scales ----
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

theme_bar_series <- function() {
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
      legend.key.width    = unit(0.9, "cm"),
      legend.key.height   = unit(0.3, "cm"),
      plot.margin         = margin(12, 12, 12, 12),
      plot.caption        = element_text(hjust = 1, size = 10, color = "gray30", margin = margin(t = 12))
    )
}

theme_bar_age <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x         = element_text(size = 11, margin = margin(t = 6)),
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
      legend.key.width    = unit(0.9, "cm"),
      legend.key.height   = unit(0.3, "cm"),
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

# Pretty Y axis in full hectares (no scientific, nice thousands separators)
axis_y_ha_auto <- function(y_max_raw) {
  ymax <- max(0, as.numeric(y_max_raw))
  if (!is.finite(ymax) || ymax <= 0) ymax <- 10000

  # Clean steps: 5k, 10k, 20k, 50k, 100k, 200k, 500k, 1M…
  steps <- c(50, 250, 500, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 20000, 50000, 100000, 200000, 500000, 1000000)
  target_n <- 6
  step <- steps[which.min(abs((ymax/steps) - target_n))]
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

## 2.1 Map CSV column names to TMF (regrowth) ----
# ------------------------------------------------------------------------- - - -
map_col_to_source_regrowth <- function(colname) {
  z <- tolower(colname)
  is_area <- str_detect(z, "area_ha")
  if (!is_area) return(NA_character_)
  if (str_detect(z, "tmf")) return("TMF-JRC")
  if (str_detect(z, "mb")  || str_detect(z, "mapbiomas")) return("MapBiomas")
  NA_character_
}

## 2.2 Read MapBiomas regrowth CSV (if needed) ----
# ------------------------------------------------------------------------- - - -
read_mb_site_regrowth <- function(path) {
  if (!file.exists(path)) return(NULL)
  loc <- readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
  df  <- readr::read_csv(path, show_col_types = FALSE, locale = loc)
  # detecta coluna Ano e a coluna de valores
  year_col <- names(df)[stringr::str_detect(tolower(names(df)), "^(ano|year)$")]
  if (length(year_col) == 0) return(NULL)
  val_col  <- setdiff(names(df), year_col)[1]
  out <- df %>%
    dplyr::rename(year = all_of(year_col), val_raw = all_of(val_col)) %>%
    dplyr::mutate(
      val_raw = dplyr::na_if(stringr::str_trim(as.character(val_raw)), "-"),
      area_ha = readr::parse_number(val_raw, locale = loc)
    ) %>%
    dplyr::mutate(area_ha = tidyr::replace_na(area_ha, 0)) %>%
    dplyr::transmute(year = as.integer(year), area_ha = as.numeric(area_ha)) %>%
    dplyr::filter(!is.na(year))
  if (nrow(out) == 0) return(NULL)
  out
}

## 2.2 Caption (TMF coverage + shown range) ----
# ------------------------------------------------------------------------- - - -
caption_sources <- function(df_in, lang = "fr") {
  if (is.null(df_in) || nrow(df_in) == 0) return("")
  cov <- df_in %>%
    dplyr::filter(!is.na(year), !is.na(area_ha)) %>%
    dplyr::group_by(source_used) %>%
    dplyr::summarise(miny = min(year), maxy = max(year), .groups = "drop") %>%
    dplyr::arrange(factor(source_used, levels = c("TMF-JRC","MapBiomas")))
  cov_str <- paste0(cov$source_used, ": ", cov$miny, "-", cov$maxy, collapse = " | ")
  prefix <- switch(lang, "fr"="Sources — ", "pt"="Fontes — ", "es"="Fuentes — ", "en"="Sources — ", "Sources — ")
  paste0(prefix, cov_str)
}

caption_age <- function(first_year, last_year, lang = "fr") {
  prefix <- switch(lang,
                   "fr"="Source — ",
                   "pt"="Fonte — ",
                   "es"="Fuente — ",
                   "en"="Source — ",
                   "Source — ")
  glue::glue("{prefix}MapBiomas: {first_year}-{last_year}")
}


## 2.3 Trim leading zeros (optional cosmetic) ----
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
#                         3) Processing Flow plots                           ----
#                                                                               #
##%###########################################################################%##

for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))
  for (TERRITORY in TERRITORIES) {

    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    cat(glue("PROCESSING: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")

    MAIN_DIR  <- file.path("results/metrics", TERRITORY)
    COMP_DIR <- file.path("results/metrics", "complementary")
    OUTPUT_DIR <- file.path("results/indicators",   TERRITORY, glue(TERRITORY, '_', LANG))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 3.1 Load regrowth flow CSV ----
    # ----------------------------------------------------------------------- - - -
    main_csv <- list.files(
      MAIN_DIR,
      pattern = glue("^{TERRITORY}_regrowth_flow_tmf_mb_.*\\.csv$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(main_csv) == 0) {
      message(glue("⚠ CSV not found for {TERRITORY} in {MAIN_DIR} — skipping."))
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

    # Identify '*_area_ha' columns belonging to TMF regrowth
    area_cols <- names(df)[str_detect(tolower(names(df)), "area_ha$")]
    if (length(area_cols) == 0) {
      message("⚠ No '*_area_ha' columns found — skipping.")
      next
    }
    message(glue("✓ Area columns: {paste(area_cols, collapse=', ')}"))

    # Long format with TMF-only mapping
    long_main <- df %>%
      dplyr::select(all_of(c(year_col, area_cols))) %>%
      tidyr::pivot_longer(cols = all_of(area_cols), names_to = "var", values_to = "area_ha") %>%
      dplyr::mutate(
        source_used = vapply(var, map_col_to_source_regrowth, character(1)),
        year        = as.integer(.data[[year_col]]),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      dplyr::filter(!is.na(source_used), !is.na(year), !is.na(area_ha)) %>%
      dplyr::select(year, source_used, area_ha) %>%
      dplyr::arrange(year, source_used)

    is_cp <- tolower(TERRITORY) %in% c("cotriguacu","paragominas")
    if (is_cp) {
      mb_site_file <- file.path(COMP_DIR, glue("{TERRITORY}_regrowth_flow_mb_site_1985_2024.csv"))
      mb_site <- read_mb_site_regrowth(mb_site_file)
      if (!is.null(mb_site)) {
        # mantém só TMF do principal e troca o MB pelo do site (cortando <1990)
        long_main <- long_main %>%
          dplyr::filter(source_used != "MapBiomas") %>%
          dplyr::bind_rows(
            mb_site %>%
              dplyr::filter(dplyr::between(year, 1990L, 2024L)) %>%
              dplyr::mutate(source_used = "MapBiomas")
          )
        message(glue("🔗 MapBiomas (site) regrowth override: {basename(mb_site_file)}"))
      } else {
        message("ℹ MB site regrowth: arquivo não encontrado/parsable — mantendo MB do CSV principal (se houver).")
      }
    }

    ### 3.2 Filter, cast, and validate ----
    # ----------------------------------------------------------------------- - - -
    # Valid coverage for flow:
    # - MapBiomas events: 1990–2021 (t+2 rule → no events in 2022+)
    # - TMF-JRC flow:     1990–2022 (mask 2023–2024 as NODATA)
    YEAR_MIN <- c(`TMF-JRC` = 1990L, MapBiomas = 1990L)
    YEAR_MAX <- c(`TMF-JRC` = 2022L, MapBiomas = 2021L)

    df_long <- long_main %>%
      dplyr::mutate(
        source_used = factor(source_used, levels = c("TMF-JRC","MapBiomas")),
        year        = as.integer(year),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      # Force NODATA (NA) after valid coverage for each source
      dplyr::mutate(
        area_ha = dplyr::case_when(
          source_used == "MapBiomas" & year > YEAR_MAX["MapBiomas"] ~ NA_real_,
          source_used == "TMF-JRC"   & year > YEAR_MAX["TMF-JRC"]   ~ NA_real_,
          TRUE ~ area_ha
        )
      ) %>%
      # Keep plotting window (1990–2024) even if values are NA
      dplyr::filter(year >= 1990L, year <= 2024L) %>%
      dplyr::arrange(year, source_used) %>%
      # Fill missing years per source (to keep gaps visible)
      tidyr::complete(source_used, year = 1990:2024) %>%
      # Cosmetic: trim leading zeros before first event
      dplyr::group_by(source_used) %>%
      dplyr::mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      dplyr::ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after filtering/mapping — skipping.")
      next
    }

    present_sources <- levels(droplevels(df_long$source_used))
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    # plotting window
    plot_min <- PLOT_YEAR_MIN
    plot_max <- PLOT_YEAR_MAX

    df_plot <- df_long %>%
      dplyr::filter(!is.na(area_ha), dplyr::between(year, plot_min, plot_max)) %>%
      droplevels()

    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]

    ### 3.3 Plot ----
    # ------------------------------------------------------------------------- - - -
    p <- ggplot(df_plot, aes(x = year, y = area_ha, color = source_used, linetype = source_used)) +
      geom_line(linewidth = 1.2,
         lineend = "round",
          na.rm = TRUE
        ) +
      geom_point(size = 4,
         stroke = 0,
          na.rm = TRUE
        ) +
      scale_color_manual(values = cols,
         breaks = present_sources,
          guide = guide_legend(title = NULL)
        ) +
      scale_linetype_manual(values = ltys,
         breaks = present_sources,
          guide = "none") +
      axis_x_years_all(PLOT_YEAR_MIN, PLOT_YEAR_MAX) +
      axis_y_ha_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_regrowth_flow", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_sources(df_plot, LANG)
      ) +
      theme_time_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 3.4 Export ----
    # ------------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue(FILENAME_FLOW, territory = TERRITORY, lang = LANG)

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
      message(glue("   PNG: {basename(png_path)}  ({FIG_WIDTH_MM}X{FIG_HEIGHT_MM} mm)"))
      if (isTRUE(WRITE_SVG)) {
        message(glue("   SVG: {basename(svg_path)}  ({FIG_WIDTH_MM}X{FIG_HEIGHT_MM} mm)"))
      } else {
        message("   SVG: (skipped)")
      }
    } else {
      message("ℹ Preview mode — set WRITE_PLOT <- TRUE to export.")
    }

    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}

##%###########################################################################%##
#                                                                               #
#                         4) Stock processing loop                           ----
#                                                                               #
##%###########################################################################%##

for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))
  for (TERRITORY in TERRITORIES) {

    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    cat(glue("PROCESSING: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")

    MAIN_DIR  <- file.path("results/metrics", TERRITORY)
    COMP_DIR <- file.path("results/metrics", "complementary")
    OUTPUT_DIR <- file.path("results/indicators",   TERRITORY, glue(TERRITORY, '_', LANG))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 4.1 Load regrowth CSV (TMF-only) ----
    # ----------------------------------------------------------------------- - - -
    main_csv <- list.files(
      MAIN_DIR,
      pattern = glue("^{TERRITORY}_regrowth_stock_tmf_mb_.*\\.csv$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(main_csv) == 0) {
      message(glue("⚠ CSV not found for {TERRITORY} in {MAIN_DIR} — skipping."))
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

    # Identify '*_area_ha' columns belonging to TMF regrowth
    area_cols <- names(df)[str_detect(tolower(names(df)), "area_ha$")]
    if (length(area_cols) == 0) {
      message("⚠ No '*_area_ha' columns found — skipping.")
      next
    }
    message(glue("✓ Area columns: {paste(area_cols, collapse=', ')}"))

    # Long format with TMF-only mapping
    long_main <- df %>%
      dplyr::select(all_of(c(year_col, area_cols))) %>%
      tidyr::pivot_longer(cols = all_of(area_cols), names_to = "var", values_to = "area_ha") %>%
      dplyr::mutate(
        source_used = vapply(var, map_col_to_source_regrowth, character(1)),
        year        = as.integer(.data[[year_col]]),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      dplyr::filter(!is.na(source_used), !is.na(year), !is.na(area_ha)) %>%
      dplyr::select(year, source_used, area_ha) %>%
      dplyr::arrange(year, source_used)

    is_cp <- tolower(TERRITORY) %in% c("cotriguacu","paragominas")
    if (is_cp) {
      mb_site_file <- file.path(COMP_DIR, glue("{TERRITORY}_regrowth_stock_mb_site_1985_2024.csv"))
      mb_site <- read_mb_site_regrowth(mb_site_file)
      if (!is.null(mb_site)) {
        # mantém só TMF do principal e troca o MB pelo do site (cortando <1990)
        long_main <- long_main %>%
          dplyr::filter(source_used != "MapBiomas") %>%
          dplyr::bind_rows(
            mb_site %>%
              dplyr::filter(dplyr::between(year, 1990L, 2024L)) %>%
              dplyr::mutate(source_used = "MapBiomas")
          )
        message(glue("🔗 MapBiomas (site) regrowth override: {basename(mb_site_file)}"))
      } else {
        message("ℹ MB site regrowth: arquivo não encontrado/parsable — mantendo MB do CSV principal (se houver).")
      }
    }

  ### 4.2 Filter, cast, and validate ----
    # ----------------------------------------------------------------------- - - -
    # Valid coverage for flow:
    # - MapBiomas events: 1990–2021 (t+2 rule → no events in 2022+)
    # - TMF-JRC flow:     1990–2022 (mask 2023–2024 as NODATA)
    YEAR_MIN <- c(`TMF-JRC` = 1990L, MapBiomas = 1990L)
    YEAR_MAX <- c(`TMF-JRC` = 2022L, MapBiomas = 2023L)

    df_long <- long_main %>%
      dplyr::mutate(
        source_used = factor(source_used, levels = c("TMF-JRC","MapBiomas")),
        year        = as.integer(year),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      # Force NODATA (NA) after valid coverage for each source
      dplyr::mutate(
        area_ha = dplyr::case_when(
          source_used == "MapBiomas" & year > YEAR_MAX["MapBiomas"] ~ NA_real_,
          source_used == "TMF-JRC"   & year > YEAR_MAX["TMF-JRC"]   ~ NA_real_,
          TRUE ~ area_ha
        )
      ) %>%
      # Keep plotting window (1990–2024) even if values are NA
      dplyr::filter(year >= 1990L, year <= 2024L) %>%
      dplyr::arrange(year, source_used) %>%
      # Fill missing years per source (to keep gaps visible)
      tidyr::complete(source_used, year = 1990:2024) %>%
      # Cosmetic: trim leading zeros before first event
      dplyr::group_by(source_used) %>%
      dplyr::mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      dplyr::ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after filtering/mapping — skipping.")
      next
    }

    present_sources <- levels(droplevels(df_long$source_used))
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    # plotting window
    plot_min <- PLOT_YEAR_MIN
    plot_max <- PLOT_YEAR_MAX

    df_plot <- df_long %>%
      dplyr::filter(!is.na(area_ha), dplyr::between(year, plot_min, plot_max)) %>%
      droplevels()

    # Aply dataset mode filter
    if (DATASET_MODE == "tmf") {
      df_plot <- df_plot %>% dplyr::filter(source_used == "TMF-JRC")
    } else if (DATASET_MODE == "mb") {
      df_plot <- df_plot %>% dplyr::filter(source_used == "MapBiomas")
    }

    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]

    ### 4.3 Plot ----
    # ------------------------------------------------------------------------- - - -
    present_sources <- levels(droplevels(df_plot$source_used))
    cols <- source_line_colors[present_sources]

    p <- ggplot(df_plot, aes(x = year, y = area_ha, fill = source_used)) +
      geom_col(
        position = position_dodge(width = 0.85),
        width = 0.8,
        color = NA,  # sem borda
        na.rm = TRUE
      ) +
      scale_fill_manual(
        values = cols,
        breaks = present_sources,
        guide = guide_legend(
          title = NULL
          # override.aes = list(shape = 22, size = 2)
        )
      ) +
      axis_x_years_all(PLOT_YEAR_MIN, PLOT_YEAR_MAX) +
      axis_y_ha_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_regrowth_stock", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_sources(df_plot, LANG)
      ) +
      theme_bar_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 4.4 Export ----
    # ------------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue(FILENAME_STOCK, territory = TERRITORY, dataset = DATASET_MODE, lang = LANG)

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

##%###########################################################################%##
#                                                                               #
#                         5) Age processing loop                              ----
#                                                                               #
##%###########################################################################%##

for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))
  for (TERRITORY in TERRITORIES) {

    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    cat(glue("PROCESSING AGE: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")

    MAIN_DIR  <- file.path("results/metrics", TERRITORY)
    COMP_DIR  <- file.path("results/metrics", "complementary")
    OUTPUT_DIR <- file.path("results/indicators", TERRITORY, glue("{TERRITORY}_{LANG}"))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 5.1 Load regrowth AGE (MB-only) ----
    # ----------------------------------------------------------------------- - - -
    is_cp <- tolower(TERRITORY) %in% c("cotriguacu","paragominas")

    if (is_cp) {
    # Brazil (site CSVs, special format: columns = "1 Ano", "2 Anos", ...; first row = "351 ha (4%)")
    main_csv <- file.path(COMP_DIR, glue("{TERRITORY}_regrowth_age_mb_site_1985_2024.csv"))
    if (!file.exists(main_csv)) {
      message(glue("⚠ Site AGE CSV not found for {TERRITORY} — skipping."))
      next
    }
    message(glue("📊 Loading (site AGE): {basename(main_csv)}"))

    # read raw (header = ages, first row = "N ha (%)")
    loc <- readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
    raw <- suppressMessages(readr::read_csv(main_csv, show_col_types = FALSE, locale = loc))

    # 1) Column names like "1 Ano", "2 Anos" → extract only numbers
    age_names <- names(raw)
    age_years <- readr::parse_number(age_names)

    # 2) First row has values like "351 ha (4%)" → extract only hectares
    row_vals <- as.character(raw[1, ])

    # Keep only entries that contain at least one digit (avoid " - ha ( - %)")
    valid_idx <- grepl("\\d", row_vals)

    age_years <- readr::parse_number(names(raw)[valid_idx])
    area_ha   <- readr::parse_number(row_vals[valid_idx], locale = loc)

    # 3) Build clean dataframe
    df <- tibble::tibble(
      age_years = age_years,
      area_ha   = area_ha
    ) %>%
      dplyr::filter(!is.na(age_years), !is.na(area_ha))

    # site datasets go until 2024 → max age ≈ 39–40
    first_year <- 1985
    last_year  <- 2024

    } else {
      # Madre de Dios / Guaviare (GEE CSVs, already clean: age_years, area_ha)
      main_csv <- list.files(
        MAIN_DIR,
        pattern = glue("^{TERRITORY}_regrowth_age_mb_.*\\.csv$"),
        full.names = TRUE, ignore.case = TRUE
      )
      if (length(main_csv) == 0) {
        message(glue("⚠ AGE CSV not found for {TERRITORY} — skipping."))
        next
      }
      message(glue("📊 Loading (GEE AGE): {basename(main_csv[1])}"))
      df <- suppressMessages(readr::read_csv(main_csv[1], show_col_types = FALSE))

      # GEE datasets go until 2023 → max age ≈ 36
      first_year <- 1987
      last_year  <- 2023
    }

    ### 5.2 Filter, validate and prepare ----
    # ----------------------------------------------------------------------- - - -
    if (nrow(df) == 0) {
      message("⚠ No data rows in AGE CSV — skipping.")
      next
    }

    df <- df %>%
      dplyr::mutate(
        age_years = as.integer(age_years),
        area_ha   = as.numeric(area_ha)
      ) %>%
      dplyr::filter(age_years > 1, area_ha >= 0)

    if (nrow(df) == 0) {
      message("⚠ Empty AGE dataframe after cleaning — skipping.")
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]

    ### 5.3 Plot ----
    # ----------------------------------------------------------------------- - - -
    p <- ggplot(df, aes(x = age_years, y = area_ha)) +
      geom_col(fill = "#1f77b4", width = 0.8, color = NA, na.rm = TRUE) +
      scale_x_continuous(
        breaks = df$age_years,
        labels = df$age_years,
        expand = expansion(mult = c(0.01, 0.02))
      ) +
      axis_y_ha_auto(max(df$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_regrowth_age", territory = territory_title, last_year = last_year),
        x       = label("x_age"),
        y       = label("y_area_ha"),
        caption = caption_age(first_year, last_year, LANG)
      ) +
      theme_bar_age()

    print(p)
    message(glue("✓ AGE Plot generated for {TERRITORY}"))

    ### 5.4 Export ----
    # ----------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue(FILENAME_AGE, territory = TERRITORY, lang = LANG)

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
      message(glue("   PNG: {basename(png_path)}  ({FIG_WIDTH_MM}X{FIG_HEIGHT_MM} mm)"))
      if (isTRUE(WRITE_SVG)) {
        message(glue("   SVG: {basename(svg_path)}  ({FIG_WIDTH_MM}X{FIG_HEIGHT_MM} mm)"))
      } else {
        message("   SVG: (skipped)")
      }
    } else {
      message("ℹ Preview mode — set WRITE_PLOT <- TRUE to export.")
    }

    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}
