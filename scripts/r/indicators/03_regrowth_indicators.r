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
  library(purrr)
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
# TERRITORIES <- c("paragominas")  # quick test
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
FILENAME_FLOW_OVERVIEW <- "03a_{territory}_regrowth_flow_overview"
FILENAME_FLOW_TRENDS   <- "03a_{territory}_regrowth_flow_trends"
FILENAME_STOCK_OVERVIEW <- "03b_{territory}_regrowth_stock_overview"
FILENAME_STOCK_TRENDS   <- "03b_{territory}_regrowth_stock_trends"
FILENAME_AGE_OVERVIEW   <- "03c_{territory}_regrowth_age_overview"

# Fixed full-page figure size (A4 width) to match other figures
FIG_WIDTH_MM   <- 431.8   # 17 in — full page width
FIG_HEIGHT_MM  <- 152.4   # 6 in  — consistent height
UNITS          <- "mm"
DPI            <- 300

# # Plot window (keep full axis to show NODATA years)
PLOT_YEAR_MIN <- 1990L
PLOT_YEAR_MAX <- 2024L

WINDOWS_11YR <- list(
  `1990-2000` = c(1990L, 2000L),
  `2001-2011` = c(2001L, 2011L),
  `2012-2022` = c(2012L, 2022L)
)

FLOW_WINDOWS <- list(
  `1990-2000 (moyenne / part %)` = c(1990L, 2000L),
  `2001-2011 (moyenne / part %)` = c(2001L, 2011L),
  `2012-2022 (moyenne / part %)` = c(2012L, 2022L)
)

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"
# LANGS <- c("fr", "es", "pt", "en")

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
      axis.text.x         = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 6)),
      axis.text.y         = element_text(size = 12),
      axis.title.x        = element_text(size = 13, margin = margin(t = 12)),
      axis.title.y        = element_text(size = 13, margin = margin(r = 12)),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position     = "top",
      legend.justification= "right",
      legend.direction    = "horizontal",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 13),
      legend.key.size     = unit(2.5, "lines"),
      plot.margin         = margin(12, 12, 12, 12),
      plot.caption        = element_text(hjust = 1, size = 11, color = "gray30", margin = margin(t = 12))
    )
}

theme_bar_series <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x         = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, margin = margin(t = 6)),
      axis.text.y         = element_text(size = 12),
      axis.title.x        = element_text(size = 13, margin = margin(t = 12)),
      axis.title.y        = element_text(size = 13, margin = margin(r = 12)),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position     = "top",
      legend.justification= "right",
      legend.direction    = "horizontal",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 13),
      legend.key.width    = unit(0.9, "cm"),
      legend.key.height   = unit(0.3, "cm"),
      plot.margin         = margin(12, 12, 12, 12),
      plot.caption        = element_text(hjust = 1, size = 11, color = "gray30", margin = margin(t = 12))
    )
}

theme_bar_age <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x         = element_text(size = 12, margin = margin(t = 6)),
      axis.text.y         = element_text(size = 12),
      axis.title.x        = element_text(size = 13, margin = margin(t = 12)),
      axis.title.y        = element_text(size = 13, margin = margin(r = 12)),
      panel.grid.major.x  = element_blank(),
      panel.grid.minor    = element_blank(),
      panel.grid.major.y  = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position     = "top",
      legend.justification= "right",
      legend.direction    = "horizontal",
      legend.title        = element_blank(),
      legend.text         = element_text(size = 13),
      legend.key.width    = unit(0.9, "cm"),
      legend.key.height   = unit(0.3, "cm"),
      plot.margin         = margin(12, 12, 12, 12),
      plot.caption        = element_text(hjust = 1, size = 11, color = "gray30", margin = margin(t = 12))
    )
}

axis_x_years_all <- function(year_min, year_max) {
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 1),
    limits = c(year_min - 0.5, year_max + 0.5), 
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

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

weighted_quantile <- function(x, w, probs) {
  if (length(x) == 0 || length(w) == 0) {
    return(rep(NA_real_, length(probs)))
  }
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  total <- sum(w, na.rm = TRUE)
  if (!is.finite(total) || total == 0) {
    return(rep(NA_real_, length(probs)))
  }
  cum_w <- cumsum(w) / total
  sapply(probs, function(p) {
    idx <- which(cum_w >= p)[1]
    if (is.na(idx)) NA_real_ else x[idx]
  })
}

format_number_fr <- function(x, digits = 0) {
  if (length(x) == 0) return(character(0))
  digits <- rep_len(digits, length.out = length(x))
  vapply(
    seq_along(x),
    function(i) {
      val <- x[i]
      digs <- digits[i]
      if (is.na(val) || !is.finite(val)) {
        "--"
      } else {
        trimws(formatC(
          round(val, digs),
          format = "f",
          big.mark = " ",
          decimal.mark = ",",
          digits = digs
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

share_fraction <- function(value, total) {
  value <- as.numeric(value)
  total <- as.numeric(total)
  out <- rep(NA_real_, length(value))
  valid <- !is.na(value) & !is.na(total)
  pos <- valid & total > 0
  out[pos] <- value[pos] / total[pos]
  zero <- valid & total == 0 & value == 0
  out[zero] <- 0
  out
}

calc_cagr <- function(start_value, end_value, periods) {
  if (is.na(start_value) || is.na(end_value) || is.na(periods)) return(NA_real_)
  if (periods <= 0 || start_value <= 0 || end_value <= 0) return(NA_real_)
  (end_value / start_value)^(1 / periods) - 1
}

format_year_value_share <- function(year, value, share) {
  if (is.na(year) || is.na(value) || is.na(share)) return("--")
  value_fmt <- format_number_fr(value)
  share_fmt <- format_percent_fr(share)
  if (value_fmt == "--" || share_fmt == "--") "--" else glue::glue("{year} ({value_fmt}) / {share_fmt}")
}

format_area_share <- function(area, share) {
  if (is.na(area) || is.na(share)) return("--")
  area_fmt <- format_number_fr(area)
  share_fmt <- format_percent_fr(share)
  if (area_fmt == "--" || share_fmt == "--") "--" else glue::glue("{area_fmt} ha / {share_fmt}")
}

compute_window_stats <- function(data_tbl, windows, total_sum, mean_digits = 0) {
  purrr::map(windows, function(window_range) {
    start_year <- window_range[1]
    end_year <- window_range[2]
    window_df <- data_tbl %>%
      dplyr::filter(dplyr::between(year, start_year, end_year), !is.na(area_ha))
    if (nrow(window_df) == 0) {
      list(mean = "--", share = "--", cagr = "--")
    } else {
      window_df <- window_df %>% dplyr::arrange(year)
      mean_val <- mean(window_df$area_ha, na.rm = TRUE)
      sum_val  <- sum(window_df$area_ha, na.rm = TRUE)
      share_val <- share_fraction(sum_val, total_sum)
      start_row <- window_df %>% dplyr::slice_head(n = 1)
      end_row   <- window_df %>% dplyr::slice_tail(n = 1)
      years_span <- end_row$year - start_row$year
      cagr_val <- calc_cagr(start_row$area_ha, end_row$area_ha, years_span)
      list(
        mean = format_number_fr(mean_val, digits = mean_digits),
        share = format_percent_fr(share_val),
        cagr = format_percent_fr(cagr_val)
      )
    }
  })
}

##%###########################################################################%##
#                                                                               #
#                         3) Flow processing loop                            ----
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

    is_cp <- tolower(TERRITORY) %in% c("cotriguacu", "paragominas", "guaviare")
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
    YEAR_MAX <- c(`TMF-JRC` = 2022L, MapBiomas = 2022L)

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
    if (identical(LANG, LANGS[[1]])) {
      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

      flow_metrics <- df_long %>%
        dplyr::filter(year >= 1990L, year <= 2022L, !is.na(area_ha))

      source_tables <- flow_metrics %>%
        dplyr::group_by(source_used, .drop = FALSE) %>%
        dplyr::summarise(
          data = list(tibble::tibble(year = year, area_ha = area_ha)),
          total_ha = sum(area_ha, na.rm = TRUE),
          .groups = "drop"
        )

      if (nrow(source_tables) == 0) {
        message(glue("[metrics] No regrowth flow data to summarise for {TERRITORY}"))
      } else {
        flow_overview <- purrr::map_dfr(seq_len(nrow(source_tables)), function(idx) {
          src_label <- as.character(source_tables$source_used[idx])
          data_tbl <- source_tables$data[[idx]] %>% dplyr::filter(!is.na(area_ha))
          if (nrow(data_tbl) == 0) {
            return(tibble::tibble(
              Source = src_label,
              `Surface totale régénérée (ha)` = "--",
              `Régénération moyenne (ha/an)` = "--",
              `Année la plus forte / Part du total` = "--",
              `Année la plus faible / Part du total` = "--"
            ))
          }
          total_ha <- sum(data_tbl$area_ha, na.rm = TRUE)
          mean_ha <- mean(data_tbl$area_ha, na.rm = TRUE)
          peak_row <- data_tbl %>% dplyr::arrange(dplyr::desc(area_ha), year) %>% dplyr::slice_head(n = 1)
          low_row  <- data_tbl %>% dplyr::arrange(area_ha, year) %>% dplyr::slice_head(n = 1)
          peak_val <- peak_row$area_ha
          low_val  <- low_row$area_ha
          tibble::tibble(
            Source = src_label,
            `Surface totale régénérée (ha)` = format_number_fr(total_ha),
            `Régénération moyenne (ha/an)` = format_number_fr(mean_ha),
            `Année la plus forte / Part du total` = format_year_value_share(peak_row$year, peak_val, share_fraction(peak_val, total_ha)),
            `Année la plus faible / Part du total` = format_year_value_share(low_row$year, low_val, share_fraction(low_val, total_ha))
          )
        })

        flow_trends <- purrr::map_dfr(seq_len(nrow(source_tables)), function(idx) {
          src_label <- as.character(source_tables$source_used[idx])
          data_tbl <- source_tables$data[[idx]]
          total_sum <- source_tables$total_ha[idx]
          window_stats <- compute_window_stats(data_tbl, FLOW_WINDOWS, total_sum, mean_digits = 0)
          cells <- purrr::map(window_stats, function(item) {
            if (is.null(item) || item$mean == "--" || item$share == "--") {
              "--"
            } else {
              glue::glue("{item$mean} / {item$share}")
            }
          })
          tibble::tibble(
            Source = src_label,
            !!!rlang::set_names(cells, names(FLOW_WINDOWS))
          )
        })

        overview_stub <- glue(FILENAME_FLOW_OVERVIEW, territory = TERRITORY)
        overview_path <- file.path(metrics_dir, glue("{overview_stub}.csv"))
        trends_stub <- glue(FILENAME_FLOW_TRENDS, territory = TERRITORY)
        trends_path <- file.path(metrics_dir, glue("{trends_stub}.csv"))
        readr::write_csv(flow_overview, overview_path, na = "")
        readr::write_csv(flow_trends, trends_path, na = "")
        message(glue("[metrics] Saved regrowth flow overview + trends tables to {basename(metrics_dir)} for {TERRITORY}"))
      }
    }

    present_sources <- levels(droplevels(df_long$source_used))
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    # plotting window
    plot_min <- PLOT_YEAR_MIN
    plot_max <- (PLOT_YEAR_MAX - 2)

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
      axis_x_years_all(plot_min, plot_max) +
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

    is_cp <- tolower(TERRITORY) %in% c("cotriguacu", "paragominas", "guaviare")
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
    is_site <- tolower(TERRITORY) %in% c("cotriguacu", "paragominas", "guaviare")
    is_gee <- tolower(TERRITORY) == "madre_de_dios"

    YEAR_MIN <- c(`TMF-JRC` = 1990L, MapBiomas = 1990L)
    YEAR_MAX <- c(`TMF-JRC` = 2022L, MapBiomas = 2022L)

    plot_min <- 1990L
    plot_max <- 2022L

    df_long <- long_main %>%
      dplyr::mutate(
        source_used = factor(source_used, levels = c("TMF-JRC","MapBiomas")),
        year        = as.integer(year),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      # EN: force NODATA after valid coverage for each source
      dplyr::mutate(
        area_ha = dplyr::case_when(
          source_used == "MapBiomas" & year > YEAR_MAX["MapBiomas"] ~ NA_real_,
          source_used == "TMF-JRC"   & year > YEAR_MAX["TMF-JRC"]   ~ NA_real_,
          TRUE ~ area_ha
        )
      ) %>%
      # EN: keep territory-specific plotting window
      dplyr::filter(year >= plot_min, year <= plot_max) %>%
      dplyr::arrange(year, source_used) %>%
      # EN: fill missing years per source within [plot_min, plot_max]
      tidyr::complete(source_used, year = plot_min:plot_max) %>%
      # EN: cosmetic: trim leading zeros before first event per source
      dplyr::group_by(source_used) %>%
      dplyr::mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      dplyr::ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after filtering/mapping — skipping.")
      next
    }

    if (identical(LANG, LANGS[[1]])) {
      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

      stock_base <- df_long %>%
        dplyr::filter(source_used == "MapBiomas", year >= 1990L, year <= 2022L) %>%
        dplyr::arrange(year)

      if (nrow(stock_base) == 0) {
        stock_base <- df_long %>%
          dplyr::filter(year >= 1990L, year <= 2022L) %>%
          dplyr::arrange(year)
      }

      stock_base <- stock_base %>% dplyr::filter(!is.na(area_ha))

      if (nrow(stock_base) == 0) {
        message(glue("[metrics] No regrowth stock data to summarise for {TERRITORY}"))
      } else {
        initial_row <- stock_base %>% dplyr::slice_head(n = 1)
        current_row <- stock_base %>% dplyr::slice_tail(n = 1)
        initial_year <- initial_row$year
        current_year <- current_row$year
        initial_area <- initial_row$area_ha
        current_area <- current_row$area_ha
        variation_abs <- current_area - initial_area
        variation_rel <- safe_divide(variation_abs, initial_area)
        cagr_global <- calc_cagr(initial_area, current_area, current_year - initial_year)

        overview_table <- tibble::tibble(
          Indicateur = c(
            "Surface initiale de foret secondaire",
            "Surface actuelle de foret secondaire",
            "Variation absolue (ha)",
            "Variation relative (%)",
            "Taux de croissance annuel composé - TCAC* (1990-2022)"
          ),
          Valeur = c(
            format_number_fr(initial_area),
            format_number_fr(current_area),
            format_number_fr(variation_abs),
            format_percent_fr(variation_rel),
            format_percent_fr(cagr_global)
          )
        )

        total_sum <- sum(stock_base$area_ha, na.rm = TRUE)
        window_stats <- compute_window_stats(stock_base, WINDOWS_11YR, total_sum, mean_digits = 0)

        stock_trends <- dplyr::bind_rows(
          tibble::tibble(
            Indicateur = "Moyenne (ha)",
            !!!rlang::set_names(purrr::map(window_stats, "mean"), names(WINDOWS_11YR))
          ),
          tibble::tibble(
            Indicateur = "Part du total (%)",
            !!!rlang::set_names(purrr::map(window_stats, "share"), names(WINDOWS_11YR))
          ),
          tibble::tibble(
            Indicateur = "Taux de croissance annuel composé - TCAC* (%)",
            !!!rlang::set_names(purrr::map(window_stats, "cagr"), names(WINDOWS_11YR))
          )
        )

        overview_stub <- glue(FILENAME_STOCK_OVERVIEW, territory = TERRITORY)
        trends_stub <- glue(FILENAME_STOCK_TRENDS, territory = TERRITORY)
        overview_path <- file.path(metrics_dir, glue("{overview_stub}.csv"))
        trends_path <- file.path(metrics_dir, glue("{trends_stub}.csv"))

        readr::write_csv(overview_table, overview_path, na = "")
        readr::write_csv(stock_trends, trends_path, na = "")

        message(glue("[metrics] Saved regrowth stock overview + trends tables to {basename(metrics_dir)} for {TERRITORY}"))
      }
    }

    present_sources <- levels(droplevels(df_long$source_used))
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    df_plot <- df_long %>%
      dplyr::filter(dplyr::between(year, plot_min, plot_max)) %>%
      dplyr::mutate(area_ha = tidyr::replace_na(area_ha, 0)) %>%
      droplevels()

    # Aply dataset mode filter
    if (DATASET_MODE == "tmf") {
      df_plot <- df_plot %>% dplyr::filter(source_used == "TMF-JRC")
    } else if (DATASET_MODE == "mb") {
      df_plot <- df_plot %>% dplyr::filter(source_used == "MapBiomas")
    }

    plot_max <- max(df_plot$year[df_plot$area_ha > 0], na.rm = TRUE)
    if (!is.finite(plot_max)) {
      plot_max <- max(df_plot$year[!is.na(df_plot$area_ha)], na.rm = TRUE)
    }
    
    df_plot <- df_plot %>% dplyr::filter(year <= plot_max)

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
        guide = "none"
      ) +
      axis_x_years_all(plot_min, plot_max) +
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
    is_cp <- tolower(TERRITORY) %in% c("cotriguacu", "paragominas", "guaviare")

    if (is_cp) {
    # Brazil (site CSVs, special format: columns = "1 Ano", "2 Anos", ...; first row = "351 ha (4%)")
    main_csv <- file.path(COMP_DIR, glue("{TERRITORY}_regrowth_age_mb_site_1985_2024.csv"))
    if (!file.exists(main_csv)) {
      message(glue("⚠ MapBiomas (site) — AGE CSV not found for {TERRITORY}, skipping."))
      next
    }
    message(glue("🔗 MapBiomas (site) — loading official age dataset {basename(main_csv)}"))

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
        message(glue("⚠ MapBiomas (GEE) — AGE CSV not found for {TERRITORY}, skipping."))
        next
      }
      message(glue("🔗 MapBiomas (GEE) — loading generated age dataset {basename(main_csv[1])}"))
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

    if (identical(LANG, LANGS[[1]])) {
      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

      total_area <- sum(df$area_ha, na.rm = TRUE)

      weighted_mean_age <- safe_divide(sum(df$age_years * df$area_ha, na.rm = TRUE), total_area)
      quantiles <- weighted_quantile(df$age_years, df$area_ha, c(0.25, 0.5, 0.75))
      share_le_5 <- safe_divide(sum(df$area_ha[df$age_years <= 5], na.rm = TRUE), total_area)
      share_le_10 <- safe_divide(sum(df$area_ha[df$age_years <= 10], na.rm = TRUE), total_area)
      share_11_20 <- safe_divide(sum(df$area_ha[df$age_years >= 11 & df$age_years <= 20], na.rm = TRUE), total_area)
      share_21_30 <- safe_divide(sum(df$area_ha[df$age_years >= 21 & df$age_years < 30], na.rm = TRUE), total_area)
      share_ge_30 <- safe_divide(sum(df$area_ha[df$age_years >= 30], na.rm = TRUE), total_area)
      area_le_10 <- sum(df$area_ha[df$age_years <= 10], na.rm = TRUE)
      area_11_20 <- sum(df$area_ha[df$age_years >= 11 & df$age_years <= 20], na.rm = TRUE)
      area_21_30 <- sum(df$area_ha[df$age_years >= 21 & df$age_years < 30], na.rm = TRUE)
      area_ge_30 <- sum(df$area_ha[df$age_years >= 30], na.rm = TRUE)
      dominant_idx <- if (nrow(df) > 0) which.max(df$area_ha) else NA_integer_
      dominant_age <- if (is.na(dominant_idx)) NA_real_ else df$age_years[dominant_idx]
      dominant_area <- if (is.na(dominant_idx)) NA_real_ else df$area_ha[dominant_idx]
      dominant_share <- safe_divide(dominant_area, total_area)

      median_age_val <- if (is.na(quantiles[2])) "--" else glue::glue("{format_number_fr(quantiles[2], digits = 1)} ans")
      dominant_text <- if (is.na(dominant_age)) {
        "--"
      } else {
        glue::glue("{dominant_age} ans ({format_number_fr(dominant_area)} ha, {format_percent_fr(dominant_share)})")
      }

      age_overview <- tibble::tibble(
        Indicateur = c(
          "Âge médian (P50)",
          "Classe dominante",
          "Part ≤ 10 ans",
          "Part > 11 ans < 20 ans",
          "Part > 21 ans < 30 ans",
          "Part ≥ 30 ans"
        ),
        Valeur = c(
          median_age_val,
          dominant_text,
          format_area_share(area_le_10, share_le_10),
          format_area_share(area_11_20, share_11_20),
          format_area_share(area_21_30, share_21_30),
          format_area_share(area_ge_30, share_ge_30)
        )
      )

      age_overview_stub <- glue(FILENAME_AGE_OVERVIEW, territory = TERRITORY)
      overview_path <- file.path(metrics_dir, glue("{age_overview_stub}.csv"))

      readr::write_csv(age_overview, overview_path, na = "")

      message(glue("[metrics] Saved regrowth age summaries to {basename(metrics_dir)} for {TERRITORY}"))
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
