##%###########################################################################%##
#                                                                               #
#                         Temperature Boxplots (TMF)                          ----
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
  library(colorspace)  # for darken_hex()
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE
WRITE_SVG  <- FALSE

# Territories to render 
# TERRITORIES <- c("madre_de_dios")
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# File name stub and figure size
FILENAME_ANNUAL  <- "05a_{TERRITORY}_temp_annual_{LANG}"
FILENAME_MONTHLY <- "05b_{TERRITORY}_temp_monthly_{LANG}"

FILENAME_ANNUAL_OVERVIEW  <- "05a_{TERRITORY}_temp_annual_overview"
FILENAME_ANNUAL_TRENDS    <- "05a_{TERRITORY}_temp_annual_trends"
FILENAME_MONTHLY_OVERVIEW <- "05b_{TERRITORY}_temp_monthly_overview"
FILENAME_MONTHLY_TRENDS   <- "05b_{TERRITORY}_temp_monthly_trends"


FIG_WIDTH_MM     <- 431.8  # 17 in
FIG_HEIGHT_MM    <- 220    # a bit taller for boxplots
UNITS            <- "mm"
DPI              <- 300

# Performance guard: use all rows by default. If files are huge, set a cap per group.
MAX_ROWS_PER_GROUP <- Inf  # e.g., 20000 to speed up if needed

# Expected year range (used for axis control and QA)
YEAR_MIN <- 2003
YEAR_MAX <- 2024

# X-axis steps (in arbitrary units; used for spacing computations if needed)
DRAW_SEP_LINES <- TRUE 
SEP_LINE_COLOR <- "#afafafff"
SEP_LINE_WIDTH <- 0.25
SEP_LINE_ALPHA <- 0.9

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
# LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"
LANGS <- c("fr", "es", "pt", "en")

LABELS <- list(
  # Titles
  title_temp_annual_in = c(
    fr = "Température annuelle de surface à {territory}",
    es = "Temperatura superficial anual en {territory}",
    pt = "Temperatura de superfície anual em {territory}",
    en = "Annual land surface temperature in {territory}"
  ),
  title_temp_monthly_in = c(
    fr = "Climatologie mensuelle (température de surface diurne) à {territory}",
    es = "Climatología mensual (temperatura superficial diurna) en {territory}",
    pt = "Climatologia mensal (temperatura de superfície diurna) em {territory}",
    en = "Monthly climatology (daytime land surface temperature) in {territory}"
  ),
  # Axes
  x_year = c(fr="Année", es="Año", pt="Ano", en="Year"),
  x_month = c(fr="Mois", es="Mes", pt="Mês", en="Month"),
  y_temp = c(fr="Température (°C)", es="Temperatura (°C)", pt="Temperatura (°C)", en="Temperature (°C)"),
  # Caption
  caption_temp = c(
    fr = "Sources — MODIS MYD11A2 (diurne, moyenne/boîtes), masque TMF-JRC médian (1 km); 2003‑2024",
    es = "Fuentes — MODIS MYD11A2 (diurno, medias/cajas), máscara TMF-JRC mediana (1 km); 2003‑2024",
    pt = "Fontes — MODIS MYD11A2 (diurno, médias/boxplots), máscara TMF-JRC mediana (1 km); 2003‑2024",
    en = "Sources — MODIS MYD11A2 (daytime, means/boxplots), TMF-JRC median mask (1 km); 2003‑2024"
  )
)

# TMF class labels per language
TMF_LABELS <- list(
  fr = c(
    "Undisturbed" = "Forêt non perturbée",
    "Degraded"    = "Forêt dégradée",
    "Deforested"  = "Terres déforestées",
    "Regrowth"    = "Forêt secondaire"
  ),
  es = c(
    "Undisturbed" = "Bosque no perturbado",
    "Degraded"    = "Bosque degradado",
    "Deforested"  = "Tierra deforestada",
    "Regrowth"    = "Bosque secundario"
  ),
  pt = c(
    "Undisturbed" = "Floresta não perturbada",
    "Degraded"    = "Floresta degradada",
    "Deforested"  = "Área desmatada",
    "Regrowth"    = "Floresta secundária"
  ),
  en = c(
    "Undisturbed" = "Undisturbed forest",
    "Degraded"    = "Degraded forest",
    "Deforested"  = "Deforested land",
    "Regrowth"    = "Forest regrowth"
  )
)


label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

# Month labels per language (ASCII apostrophes not relevant here; keep accents for FR)
MONTH_LABELS <- list(
  fr = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", "Juil", "Aoû", "Sep", "Oct", "Nov", "Déc"),
  es = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
  pt = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
  en = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)

## 1.3 TMF palette & aesthetics ----
# ------------------------------------------------------------------------- - - -
# Fill palette (as requested)
# tmf_fill <- c(
#   "Undisturbed" = "#295029ff",
#   "Degraded"    = "#4b8804ff",
#   "Deforested"    = "#b74848ff",
#   "Regrowth"    = "#caa640ff"
# )

tmf_fill <- c(
  "Undisturbed" = "#005A00",
  "Degraded"    = "#649B23",
  "Deforested"    = "#FF871F",
  "Regrowth"    = "#b3dc21ff"
)

TMF_CLASS_LEVELS <- c("Undisturbed", "Degraded", "Deforested", "Regrowth")

# Build a stroke palette slightly darker than fill
darken_hex <- function(hex, amount = 0.18) {
  if (requireNamespace("colorspace", quietly = TRUE)) {
    return(colorspace::darken(hex, amount = amount))
  }
  rgbv <- col2rgb(hex) / 255
  rgbv <- pmax(rgbv - amount, 0)
  grDevices::rgb(rgbv[1,], rgbv[2,], rgbv[3,], 1)
}
tmf_stroke <- vapply(tmf_fill, darken_hex, character(1), amount = 0.18)

BOX_ALPHA <- 0.55  # alpha for box fill
WHISKER_LINEWIDTH <- 0.5

## 1.4 Theme & axes ----
# ------------------------------------------------------------------------- - - -
theme_boxplots <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text(size = 12, margin = margin(t = 12)),
      axis.title.y = element_text(size = 12, margin = margin(r = 12)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position = "top",
      legend.justification = "right",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.key.size = unit(2.2, "lines"),
      plot.margin = margin(12, 12, 12, 12),
      plot.caption = element_text(hjust = 1, size = 10, color = "gray30", margin = margin(t = 12))
    )
}

axis_x_years_all <- function(year_min, year_max) {
  scale_x_discrete(
    limits = as.character(seq.int(year_min, year_max, by = 1)),
    expand = expansion(mult = c(0.01, 0.02))
  )
}

axis_x_months <- function(lang = "fr") {
  labs <- MONTH_LABELS[[lang]] %||% MONTH_LABELS[["en"]]
  scale_x_discrete(
    limits = sprintf("%02d", 1:12),
    labels = labs,
    expand = expansion(mult = c(0.01, 0.02))
  )
}

## 1.6 Adaptive Y-axis & QC ----
# ------------------------------------------------------------------------- - - -
ADAPTIVE_Y        <- TRUE
Y_WHISKER_MARGIN  <- 0.05      
Y_LIMS_OVERRIDE   <- list()  

# Function to compute adaptive y-limits based on boxplot whiskers
compute_y_limits <- function(df, value_col = "temperature_c", territory = NULL) {
  # Override if specified for this territory
  if (!is.null(territory) && length(Y_LIMS_OVERRIDE) && territory %in% names(Y_LIMS_OVERRIDE)) {
    return(Y_LIMS_OVERRIDE[[territory]])
  }
  if (!isTRUE(ADAPTIVE_Y)) return(NULL)

  group_cols <- intersect(c("year_f","month_f","tmf_class_label"), names(df))
  if (!length(group_cols)) return(NULL)

  whisk <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      lo = boxplot.stats(.data[[value_col]])$stats[1],
      hi = boxplot.stats(.data[[value_col]])$stats[5],
      .groups = "drop"
    )

  lo <- suppressWarnings(min(whisk$lo, na.rm = TRUE))
  hi <- suppressWarnings(max(whisk$hi, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(NULL)

  span <- hi - lo
  pad  <- Y_WHISKER_MARGIN * span
  c(lo - pad, hi + pad)
}


##%###########################################################################%##
#                                                                               #
#                         2) Utility Functions                               ----
#                                                                               #
##%###########################################################################%##

## 2.1 Loaders ----
# ------------------------------------------------------------------------- - - -
find_csv <- function(dir, pattern) {
  list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
}

detect_col <- function(nms, candidates_regex) {
  ix <- which(str_detect(tolower(nms), candidates_regex))
  if (length(ix)) nms[ix[1]] else NA_character_
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

trend_slope <- function(x, y) {
  if (length(x) != length(y) || length(x) < 2) return(NA_real_)
  mod <- tryCatch(stats::lm(y ~ x), error = function(...) NULL)
  if (is.null(mod)) return(NA_real_)
  coef(mod)[[2]]
}

value_at <- function(x, y, target) {
  vals <- y[x == target]
  if (length(vals) == 0) NA_real_ else vals[1]
}

format_fr_num <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "",
    format(
      round(x, digits),
      big.mark = " ",
      decimal.mark = ",",
      trim = TRUE,
      scientific = FALSE,
      nsmall = digits
    )
  )
}

format_fr_signed <- function(x, digits = 1) {
  ifelse(
    is.na(x),
    "",
    paste0(
      ifelse(x > 0, "+", ifelse(x < 0, "-", "")),
      format_fr_num(abs(x), digits = digits)
    )
  )
}

format_year_value <- function(year, value, digits = 1) {
  ifelse(
    is.na(year) | is.na(value),
    "",
    glue::glue("{year} ({format_fr_num(value, digits)})")
  )
}

format_period_pair <- function(v1, v2, digits = 1) {
  paste0(format_fr_num(v1, digits), " / ", format_fr_num(v2, digits))
}

compute_annual_temperature_metrics <- function(df) {
  df_clean <- df %>%
    filter(!is.na(year), !is.na(temperature_c), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()

  if (nrow(df_clean) == 0) {
    return(list(metrics = tibble(), contrasts = tibble()))
  }

  summaries <- df_clean %>%
    group_by(tmf_class_label, year, .drop = FALSE) %>%
    summarise(
      median_temp = median(temperature_c, na.rm = TRUE),
      q25_temp = quantile(temperature_c, 0.25, na.rm = TRUE),
      q75_temp = quantile(temperature_c, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      diff_vs_prev = median_temp - lag(median_temp),
      iqr = q75_temp - q25_temp
    ) %>%
    ungroup()

  metrics <- summaries %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    summarise(
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      obs_years = n(),
      median_first = first(median_temp),
      median_last = last(median_temp),
      delta_first_last = median_last - median_first,
      slope_per_year = trend_slope(year, median_temp),
      slope_per_decade = slope_per_year * 10,
      mean_iqr = mean(iqr, na.rm = TRUE),
      hottest_year = year[which.max(median_temp)],
      hottest_median = max(median_temp, na.rm = TRUE),
      coolest_year = year[which.min(median_temp)],
      coolest_median = min(median_temp, na.rm = TRUE),
      diff_vs_prev_last = last(diff_vs_prev),
      median_year_min = value_at(year, median_temp, year_min),
      median_year_max = value_at(year, median_temp, year_max),
      median_year_mid = value_at(year, median_temp, floor((year_min + year_max) / 2)),
      .groups = "drop"
    ) %>%
    mutate(tmf_class_label = as.character(tmf_class_label))

  contrasts <- summaries %>%
    select(year, tmf_class_label, median_temp) %>%
    pivot_wider(names_from = tmf_class_label, values_from = median_temp) %>%
    mutate(
      diff_defor_vs_undist = `Deforested` - `Undisturbed`,
      diff_degraded_vs_undist = `Degraded` - `Undisturbed`,
      diff_regrowth_vs_undist = `Regrowth` - `Undisturbed`
    ) %>%
    summarise(
      mean_diff_defor_vs_undist = mean(diff_defor_vs_undist, na.rm = TRUE),
      mean_diff_degraded_vs_undist = mean(diff_degraded_vs_undist, na.rm = TRUE),
      mean_diff_regrowth_vs_undist = mean(diff_regrowth_vs_undist, na.rm = TRUE),
      diff_defor_vs_undist_latest = diff_defor_vs_undist[year == max(year, na.rm = TRUE)],
      diff_degraded_vs_undist_latest = diff_degraded_vs_undist[year == max(year, na.rm = TRUE)],
      diff_regrowth_vs_undist_latest = diff_regrowth_vs_undist[year == max(year, na.rm = TRUE)],
      hottest_year_defor = year[which.max(`Deforested`)],
      hottest_year_undist = year[which.max(`Undisturbed`)],
      .groups = "drop"
    )

  list(metrics = metrics, contrasts = contrasts)
}

compute_monthly_temperature_metrics <- function(df) {
  df_clean <- df %>%
    filter(!is.na(year), !is.na(month), !is.na(temperature_c), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()

  if (nrow(df_clean) == 0) {
    return(list(metrics = tibble(), contrasts = tibble()))
  }

  summaries <- df_clean %>%
    group_by(tmf_class_label, month, .drop = FALSE) %>%
    summarise(
      mean_temp = mean(temperature_c, na.rm = TRUE),
      median_temp = median(temperature_c, na.rm = TRUE),
      q25_temp = quantile(temperature_c, 0.25, na.rm = TRUE),
      q75_temp = quantile(temperature_c, 0.75, na.rm = TRUE),
      max_temp = max(temperature_c, na.rm = TRUE),
      min_temp = min(temperature_c, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(iqr = q75_temp - q25_temp)

  metrics <- summaries %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    summarise(
      hottest_month = month[which.max(median_temp)],
      hottest_temp_median = max(median_temp, na.rm = TRUE),
      coldest_month = month[which.min(median_temp)],
      coldest_temp_median = min(median_temp, na.rm = TRUE),
      seasonal_amplitude = hottest_temp_median - coldest_temp_median,
      mean_iqr = mean(iqr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(tmf_class_label = as.character(tmf_class_label))

  contrasts <- summaries %>%
    select(month, tmf_class_label, median_temp) %>%
    pivot_wider(names_from = tmf_class_label, values_from = median_temp) %>%
    mutate(
      diff_defor_vs_undist = `Deforested` - `Undisturbed`,
      diff_degraded_vs_undist = `Degraded` - `Undisturbed`,
      diff_regrowth_vs_undist = `Regrowth` - `Undisturbed`
    ) %>%
    summarise(
      max_diff_defor_vs_undist = diff_defor_vs_undist[which.max(diff_defor_vs_undist)],
      month_max_diff_defor_vs_undist = month[which.max(diff_defor_vs_undist)],
      max_diff_degraded_vs_undist = diff_degraded_vs_undist[which.max(diff_degraded_vs_undist)],
      month_max_diff_degraded_vs_undist = month[which.max(diff_degraded_vs_undist)],
      max_diff_regrowth_vs_undist = diff_regrowth_vs_undist[which.max(diff_regrowth_vs_undist)],
      month_max_diff_regrowth_vs_undist = month[which.max(diff_regrowth_vs_undist)],
      diff_defor_vs_undist_mean = mean(diff_defor_vs_undist, na.rm = TRUE),
      diff_degraded_vs_undist_mean = mean(diff_degraded_vs_undist, na.rm = TRUE),
      diff_regrowth_vs_undist_mean = mean(diff_regrowth_vs_undist, na.rm = TRUE),
      .groups = "drop"
    )

  list(metrics = metrics, contrasts = contrasts)
}

## 2.2 Plot builders ----
# ------------------------------------------------------------------------- - - -
plot_temp_annual_boxes <- function(df, lang, territory_title) {
  # df: pixel-level annual data with columns year, temperature_c, tmf_class_label
  df <- df %>%
    mutate(
      year  = as.integer(year),
      year_f = factor(as.character(year), levels = as.character(seq.int(YEAR_MIN, YEAR_MAX, 1))),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    filter(!is.na(year_f), !is.na(temperature_c), !is.na(tmf_class_label))

  # Optional downsampling to keep groups balanced and fast
  if (is.finite(MAX_ROWS_PER_GROUP)) {
    df <- df %>%
      group_by(year_f, tmf_class_label) %>%
      slice_sample(n = min(MAX_ROWS_PER_GROUP, n()), replace = FALSE) %>%
      ungroup()
  }

  y_limits <- compute_y_limits(df, "temperature_c", territory = tolower(territory_title))

  ggplot(
    df,
    aes(x = year_f, y = temperature_c, fill = tmf_class_label, color = tmf_class_label)
  ) +
    geom_boxplot(
      width = 0.70,
      outlier.shape = NA,
      alpha = BOX_ALPHA,
      linewidth = WHISKER_LINEWIDTH
    ) +
    { 
      if (isTRUE(DRAW_SEP_LINES)) {
        nlev <- length(levels(df$year_f))
        # linhas entre os anos: 1.5, 2.5, ..., (n-0.5)
        geom_vline(
          xintercept = seq(1.5, nlev - 0.5, by = 1),
          linewidth = SEP_LINE_WIDTH,
          color = SEP_LINE_COLOR,
          alpha = SEP_LINE_ALPHA,
          linetype = "dashed"
        )
      } else NULL
    } +
    scale_fill_manual(
      values = tmf_fill,
      breaks = TMF_CLASS_LEVELS,
      labels = TMF_LABELS[[LANG]],
      name   = NULL,
      drop   = FALSE
    ) +
    scale_color_manual(
      values = tmf_stroke,
      breaks = TMF_CLASS_LEVELS,
      labels = TMF_LABELS[[LANG]],
      name   = NULL,
      drop   = FALSE
    ) +
    axis_x_years_all(YEAR_MIN, YEAR_MAX) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.04))) +
    coord_cartesian(ylim = y_limits, expand = FALSE) +
    labs(
      title   = label("title_temp_annual_in", territory = territory_title),
      x       = label("x_year"),
      y       = label("y_temp"),
      caption = label("caption_temp")
    ) +
    theme_boxplots() +
    guides(fill = guide_legend(nrow = 1))
}

plot_temp_monthly_boxes <- function(df, lang, territory_title) {
  # df: pixel-level monthly data with columns year, month, temperature_c, tmf_class_label
  df <- df %>%
    mutate(
      month   = as.integer(month),
      month_f = factor(sprintf("%02d", month), levels = sprintf("%02d", 1:12)),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    filter(!is.na(month_f), !is.na(temperature_c), !is.na(tmf_class_label))

  # Optional downsampling to keep groups balanced and fast
  if (is.finite(MAX_ROWS_PER_GROUP)) {
    df <- df %>%
      group_by(month_f, tmf_class_label) %>%
      slice_sample(n = min(MAX_ROWS_PER_GROUP, n()), replace = FALSE) %>%
      ungroup()
  }

  y_limits <- compute_y_limits(df, "temperature_c", territory = tolower(territory_title))

  ggplot(
    df,
    aes(x = month_f, y = temperature_c, fill = tmf_class_label, color = tmf_class_label)
  ) +
    geom_boxplot(
      width = 0.70,
      outlier.shape = NA,
      alpha = BOX_ALPHA,
      linewidth = WHISKER_LINEWIDTH
    ) +
    { 
      if (isTRUE(DRAW_SEP_LINES)) {
        nlev <- length(levels(df$month_f))
        # linhas entre os meses: 1.5, 2.5, ..., (n-0.5)
        geom_vline(
          xintercept = seq(1.5, nlev - 0.5, by = 1),
          linewidth = SEP_LINE_WIDTH,
          color = SEP_LINE_COLOR,
          alpha = SEP_LINE_ALPHA,
          linetype = "dashed"
        )
      } else NULL
    } +
    scale_fill_manual(
      values = tmf_fill,
      breaks = TMF_CLASS_LEVELS,
      labels = TMF_LABELS[[LANG]],
      name   = NULL,
      drop   = FALSE
    ) +
    scale_color_manual(
      values = tmf_stroke,
      breaks = TMF_CLASS_LEVELS,
      labels = TMF_LABELS[[LANG]],
      name   = NULL,
      drop   = FALSE
    ) +
    axis_x_months(lang) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.04))) +
    coord_cartesian(ylim = y_limits, expand = FALSE) +
    labs(
      title   = label("title_temp_monthly_in", territory = territory_title),
      x       = label("x_month"),
      y       = label("y_temp"),
      caption = label("caption_temp")
    ) +
    theme_boxplots() +
    guides(fill = guide_legend(nrow = 1))
}


##%###########################################################################%##
#                                                                               #
#                            3  Main Loop                                    ----
#                                                                               #
##%###########################################################################%##

## 3.1 Main loop (LANG × TERRITORY) ----
# ------------------------------------------------------------------------- - - -
for (LANG in LANGS) {
  message(glue("🌐 Language: {LANG}"))
  for (TERRITORY in TERRITORIES) {

    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")
    cat(glue("PROCESSING: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse=""), "\n", sep = "")

    INPUT_DIR  <- file.path("results/metrics", TERRITORY)
    OUTPUT_DIR <- file.path("results/indicators",   TERRITORY, glue(TERRITORY, '_', LANG))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ## 3.2 Load annual CSV ----
    # ----------------------------------------------------------------------- - - -
    annual_csv <- find_csv(INPUT_DIR, "_climate_annual_2003_2024\\.csv$")
    if (length(annual_csv) == 0) {
      message(glue("⚠ No annual CSV found in {INPUT_DIR} — skipping annual plot."))
      annual_df <- NULL
    } else {
      message(glue("📄 Annual CSV: {basename(annual_csv[1])}"))
      annual_df <- suppressMessages(readr::read_csv(annual_csv[1], show_col_types = FALSE))
      # Detect columns
      col_temp  <- detect_col(names(annual_df), "temperature|temp_?c")
      col_year  <- detect_col(names(annual_df), "^year$|\\byear\\b|ano|annee|anno")
      col_tmf   <- detect_col(names(annual_df), "tmf.*label|class.*label")
      if (is.na(col_temp) || is.na(col_year) || is.na(col_tmf)) {
        stop(glue(
          "Annual CSV missing required columns. Found: temp='{col_temp}', year='{col_year}', tmf_label='{col_tmf}'."
        ), call. = FALSE)
      }
      annual_df <- annual_df %>%
        transmute(
          year = .data[[col_year]],
          temperature_c = suppressWarnings(as.numeric(.data[[col_temp]])),
          tmf_class_label = as.character(.data[[col_tmf]])
        )
    }

    ## 3.3 Load monthly CSV ----
    # ----------------------------------------------------------------------- - - -
    monthly_csv <- find_csv(INPUT_DIR, "_climate_monthly_2003_2024\\.csv$")
    if (length(monthly_csv) == 0) {
      message(glue("⚠ No monthly CSV found in {INPUT_DIR} — skipping monthly plot."))
      monthly_df <- NULL
    } else {
      message(glue("📄 Monthly CSV: {basename(monthly_csv[1])}"))
      monthly_df <- suppressMessages(readr::read_csv(monthly_csv[1], show_col_types = FALSE))
      # Detect columns
      col_temp  <- detect_col(names(monthly_df), "temperature|temp_?c")
      col_year  <- detect_col(names(monthly_df), "^year$|\\byear\\b|ano|annee|anno")
      col_month <- detect_col(names(monthly_df), "^month$|\\bmonth\\b|mois|mes")
      col_tmf   <- detect_col(names(monthly_df), "tmf.*label|class.*label")
      if (is.na(col_temp) || is.na(col_year) || is.na(col_month) || is.na(col_tmf)) {
        stop(glue(
          "Monthly CSV missing required columns. Found: temp='{col_temp}', year='{col_year}', month='{col_month}', tmf_label='{col_tmf}'."
        ), call. = FALSE)
      }
      monthly_df <- monthly_df %>%
        transmute(
          year = .data[[col_year]],
          month = .data[[col_month]],
          temperature_c = suppressWarnings(as.numeric(.data[[col_temp]])),
          tmf_class_label = as.character(.data[[col_tmf]])
        )
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]] %||% TERRITORY

    if (!is.null(annual_df)) {
      if (identical(LANG, LANGS[[1]])) {
        metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
        dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

        annual_pivot <- annual_df %>%
          filter(!is.na(year), !is.na(temperature_c), !is.na(tmf_class_label)) %>%
          mutate(
            year = as.integer(year),
            tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
          ) %>%
          filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
          drop_na()

        annual_summaries <- annual_pivot %>%
          group_by(tmf_class_label, year, .drop = FALSE) %>%
          summarise(
            median_temp = median(temperature_c, na.rm = TRUE),
            q25_temp = quantile(temperature_c, 0.25, na.rm = TRUE),
            q75_temp = quantile(temperature_c, 0.75, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          group_by(tmf_class_label, .drop = FALSE) %>%
          arrange(year, .by_group = TRUE) %>%
          mutate(
            diff_vs_prev = median_temp - dplyr::lag(median_temp)
          ) %>%
          ungroup()

        baseline_year <- YEAR_MIN
        mid_year <- floor((YEAR_MIN + YEAR_MAX) / 2)
        final_year <- YEAR_MAX

        annual_medians <- annual_summaries %>%
          select(tmf_class_label, year, median_temp)

        period1_years <- YEAR_MIN:(YEAR_MIN + 10)
        period2_years <- (YEAR_MIN + 11):YEAR_MAX

        annual_diff_all <- annual_medians %>%
          left_join(
            annual_medians %>%
              filter(tmf_class_label == "Undisturbed") %>%
              select(year, undist_median = median_temp),
            by = "year"
          ) %>%
          mutate(
            diff_vs_undist = median_temp - undist_median,
            period = dplyr::case_when(
              year %in% period1_years ~ "2003-2013",
              year %in% period2_years ~ "2014-2024",
              TRUE ~ NA_character_
            )
          )

        annual_overview_table <- annual_medians %>%
          group_by(tmf_class_label, .drop = FALSE) %>%
          summarise(
            median_all = median(median_temp, na.rm = TRUE),
            amplitude_all = max(median_temp, na.rm = TRUE) - min(median_temp, na.rm = TRUE),
            hottest_year = year[which.max(median_temp)],
            hottest_median = max(median_temp, na.rm = TRUE),
            coolest_year = year[which.min(median_temp)],
            coolest_median = min(median_temp, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          left_join(
            annual_diff_all %>%
              group_by(tmf_class_label, .drop = FALSE) %>%
              summarise(
                mean_diff_vs_undist = mean(diff_vs_undist, na.rm = TRUE),
                .groups = "drop"
              ),
            by = "tmf_class_label"
          ) %>%
          mutate(
            couverture_fr = TMF_LABELS[["fr"]][tmf_class_label],
            couverture_fr = if_else(is.na(couverture_fr), tmf_class_label, couverture_fr),
            hottest_label = format_year_value(hottest_year, hottest_median, digits = 2),
            coolest_label = format_year_value(coolest_year, coolest_median, digits = 2),
            diff_label = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ "—",
              TRUE ~ format_fr_signed(mean_diff_vs_undist, digits = 2)
            )
          ) %>%
          transmute(
            Couverture = couverture_fr,
            `Température médiane 2003-2024 (°C)` = format_fr_num(median_all, digits = 2),
            `Amplitude 2003-2024 (°C)` = format_fr_num(amplitude_all, digits = 2),
            `Année la plus chaude (°C)` = hottest_label,
            `Année la plus fraîche (°C)` = coolest_label,
            `Différence moyenne vs forêt non perturbée (°C)` = diff_label
          )

        annual_trends_table <- annual_medians %>%
          mutate(
            period = dplyr::case_when(
              year %in% period1_years ~ "2003-2013",
              year %in% period2_years ~ "2014-2024",
              TRUE ~ NA_character_
            )
          ) %>%
          filter(!is.na(period)) %>%
          group_by(tmf_class_label, period, .drop = FALSE) %>%
          summarise(
            median_period = median(median_temp, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(names_from = period, values_from = median_period) %>%
          left_join(
            annual_medians %>%
              group_by(tmf_class_label, .drop = FALSE) %>%
              summarise(
                trend_decade = trend_slope(year, median_temp) * 10,
                .groups = "drop"
              ),
            by = "tmf_class_label"
          ) %>%
          left_join(
            annual_diff_all %>%
              filter(!is.na(period)) %>%
              group_by(tmf_class_label, period, .drop = FALSE) %>%
              summarise(
                mean_diff = mean(diff_vs_undist, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              tidyr::pivot_wider(names_from = period, values_from = mean_diff, names_glue = "diff_{period}"),
            by = "tmf_class_label"
          ) %>%
          mutate(
            couverture_fr = TMF_LABELS[["fr"]][tmf_class_label],
            couverture_fr = if_else(is.na(couverture_fr), tmf_class_label, couverture_fr),
            median_p1 = `2003-2013`,
            median_p2 = `2014-2024`,
            variation = median_p2 - median_p1,
            diff_p1 = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ 0,
              TRUE ~ `diff_2003-2013`
            ),
            diff_p2 = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ 0,
              TRUE ~ `diff_2014-2024`
            ),
            diff_pair = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ "— / —",
              TRUE ~ format_period_pair(diff_p1, diff_p2, digits = 2)
            )
          ) %>%
          transmute(
            Couverture = couverture_fr,
            `Médiane 2003-2013 (°C)` = format_fr_num(median_p1, digits = 2),
            `Médiane 2014-2024 (°C)` = format_fr_num(median_p2, digits = 2),
            `Variation (°C)` = format_fr_signed(variation, digits = 2),
            `Tendance (°C par décennie)` = format_fr_signed(trend_decade, digits = 2),
            `Différence moyenne vs forêt non perturbée (2003-2013 / 2014-2024) (°C)` = diff_pair
          )

        annual_overview_stub <- glue(FILENAME_ANNUAL_OVERVIEW, TERRITORY = TERRITORY)
        annual_overview_path <- file.path(metrics_dir, glue("{annual_overview_stub}.csv"))
        readr::write_csv(annual_overview_table, annual_overview_path, na = "")

        annual_trend_stub <- glue(FILENAME_ANNUAL_TRENDS, TERRITORY = TERRITORY)
        annual_trends_path <- file.path(metrics_dir, glue("{annual_trend_stub}.csv"))
        readr::write_csv(annual_trends_table, annual_trends_path, na = "")

        message(glue("[metrics] Saved annual temperature metrics to {basename(metrics_dir)} for {TERRITORY}"))
      }

      pA <- plot_temp_annual_boxes(annual_df, LANG, territory_title)
      if (WRITE_PLOT) {
        file_stub <- glue(FILENAME_ANNUAL, TERRITORY = TERRITORY, LANG = LANG)

        png_path  <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))
        ggsave(png_path, pA, width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS, dpi = DPI, bg = "white")
        if (isTRUE(WRITE_SVG)) {
          svg_path <- file.path(OUTPUT_DIR, glue("{file_stub}.svg"))
          ggsave(svg_path, pA, width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS, device = svglite, bg = "white")
        }
        message("✅ Saved annual:")
        message(glue("   PNG: {basename(png_path)}"))
        message(glue("   SVG: {if (isTRUE(WRITE_SVG)) basename(svg_path) else '(skipped)'}"))
      } else {
        print(pA)
        message("ℹ Annual preview mode — set WRITE_PLOT <- TRUE to export.")
      }
    }
    if (!is.null(monthly_df)) {
      if (identical(LANG, LANGS[[1]])) {
        metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
        dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

        monthly_pivot <- monthly_df %>%
          filter(!is.na(year), !is.na(month), !is.na(temperature_c), !is.na(tmf_class_label)) %>%
          mutate(
            year = as.integer(year),
            month = as.integer(month),
            tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
          ) %>%
          filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
          drop_na()

        monthly_summaries <- monthly_pivot %>%
          group_by(tmf_class_label, month, .drop = FALSE) %>%
          summarise(
            mean_temp = mean(temperature_c, na.rm = TRUE),
            median_temp = median(temperature_c, na.rm = TRUE),
            q25_temp = quantile(temperature_c, 0.25, na.rm = TRUE),
            q75_temp = quantile(temperature_c, 0.75, na.rm = TRUE),
            max_temp = max(temperature_c, na.rm = TRUE),
            min_temp = min(temperature_c, na.rm = TRUE),
            .groups = "drop"
          )

        monthly_metrics <- monthly_summaries %>%
          group_by(tmf_class_label, .drop = FALSE) %>%
          summarise(
            hottest_month = month[which.max(median_temp)],
            hottest_temp_median = max(median_temp, na.rm = TRUE),
            coldest_month = month[which.min(median_temp)],
            coldest_temp_median = min(median_temp, na.rm = TRUE),
            annual_amplitude = hottest_temp_median - coldest_temp_median,
            mean_iqr = mean(q75_temp - q25_temp, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(tmf_class_label = as.character(tmf_class_label))

        monthly_contrasts <- monthly_summaries %>%
          select(month, tmf_class_label, median_temp) %>%
          pivot_wider(names_from = tmf_class_label, values_from = median_temp) %>%
          mutate(
            diff_defor_vs_undist = `Deforested` - `Undisturbed`,
            diff_degraded_vs_undist = `Degraded` - `Undisturbed`,
            diff_regrowth_vs_undist = `Regrowth` - `Undisturbed`
          )

        monthly_contrasts_summary <- monthly_contrasts %>%
          summarise(
            max_diff_defor_vs_undist = diff_defor_vs_undist[which.max(diff_defor_vs_undist)],
            month_max_diff_defor_vs_undist = month[which.max(diff_defor_vs_undist)],
            max_diff_degraded_vs_undist = diff_degraded_vs_undist[which.max(diff_degraded_vs_undist)],
            month_max_diff_degraded_vs_undist = month[which.max(diff_degraded_vs_undist)],
            max_diff_regrowth_vs_undist = diff_regrowth_vs_undist[which.max(diff_regrowth_vs_undist)],
            month_max_diff_regrowth_vs_undist = month[which.max(diff_regrowth_vs_undist)],
            diff_defor_vs_undist_mean = mean(diff_defor_vs_undist, na.rm = TRUE),
            diff_degraded_vs_undist_mean = mean(diff_degraded_vs_undist, na.rm = TRUE),
            diff_regrowth_vs_undist_mean = mean(diff_regrowth_vs_undist, na.rm = TRUE),
            .groups = "drop"
          )

        monthly_diff_overview <- monthly_summaries %>%
          select(tmf_class_label, month, median_temp) %>%
          left_join(
            monthly_summaries %>%
              filter(tmf_class_label == "Undisturbed") %>%
              select(month, undist_median = median_temp),
            by = "month"
          ) %>%
          mutate(diff_vs_undist = median_temp - undist_median) %>%
          group_by(tmf_class_label, .drop = FALSE) %>%
          summarise(
            mean_diff_vs_undist = mean(diff_vs_undist, na.rm = TRUE),
            .groups = "drop"
          )

        monthly_overview_table <- monthly_metrics %>%
          left_join(monthly_diff_overview, by = "tmf_class_label") %>%
          mutate(
            couverture_fr = TMF_LABELS[["fr"]][tmf_class_label],
            couverture_fr = if_else(is.na(couverture_fr), tmf_class_label, couverture_fr),
            hottest_label = if_else(
              is.na(hottest_month),
              "",
              glue::glue(
                "{MONTH_LABELS[['fr']][hottest_month]} ({format_fr_num(hottest_temp_median, digits = 2)})"
              )
            ),
            coldest_label = if_else(
              is.na(coldest_month),
              "",
              glue::glue(
                "{MONTH_LABELS[['fr']][coldest_month]} ({format_fr_num(coldest_temp_median, digits = 2)})"
              )
            ),
            diff_label = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ "—",
              TRUE ~ format_fr_signed(mean_diff_vs_undist, digits = 2)
            )
          ) %>%
          transmute(
            Couverture = couverture_fr,
            `Mois le plus chaud / médiane (°C)` = hottest_label,
            `Mois le plus frais / médiane (°C)` = coldest_label,
            `Amplitude saisonnière (°C)` = format_fr_num(annual_amplitude, digits = 2),
            `IQR moyen (°C)` = format_fr_num(mean_iqr, digits = 2),
            `Différence moyenne vs forêt non perturbée (°C)` = diff_label
          )

        monthly_period_stats <- monthly_pivot %>%
          mutate(
            period = dplyr::case_when(
              year %in% period1_years ~ "2003-2013",
              year %in% period2_years ~ "2014-2024",
              TRUE ~ NA_character_
            )
          ) %>%
          filter(!is.na(period)) %>%
          group_by(period, tmf_class_label, month, .drop = FALSE) %>%
          summarise(
            median_temp = median(temperature_c, na.rm = TRUE),
            .groups = "drop"
          )

        monthly_amplitude_period <- monthly_period_stats %>%
          group_by(tmf_class_label, period, .drop = FALSE) %>%
          summarise(
            amplitude = max(median_temp, na.rm = TRUE) - min(median_temp, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(names_from = period, values_from = amplitude)

        monthly_diff_period <- monthly_period_stats %>%
          left_join(
            monthly_period_stats %>%
              filter(tmf_class_label == "Undisturbed") %>%
              select(period, month, undist_median = median_temp),
            by = c("period", "month")
          ) %>%
          mutate(diff_vs_undist = median_temp - undist_median) %>%
          group_by(tmf_class_label, period, .drop = FALSE) %>%
          summarise(
            mean_diff = mean(diff_vs_undist, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_wider(names_from = period, values_from = mean_diff, names_glue = "diff_{period}")

        monthly_trends_table <- monthly_amplitude_period %>%
          left_join(monthly_diff_period, by = "tmf_class_label") %>%
          mutate(
            couverture_fr = TMF_LABELS[["fr"]][tmf_class_label],
            couverture_fr = if_else(is.na(couverture_fr), tmf_class_label, couverture_fr),
            amp_p1 = `2003-2013`,
            amp_p2 = `2014-2024`,
            variation_amp = amp_p2 - amp_p1,
            diff_p1 = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ 0,
              TRUE ~ `diff_2003-2013`
            ),
            diff_p2 = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ 0,
              TRUE ~ `diff_2014-2024`
            ),
            diff_pair = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ "— / —",
              TRUE ~ format_period_pair(diff_p1, diff_p2, digits = 2)
            ),
            diff_variation = dplyr::case_when(
              tmf_class_label == "Undisturbed" ~ "—",
              TRUE ~ format_fr_signed(diff_p2 - diff_p1, digits = 2)
            )
          ) %>%
          transmute(
            Couverture = couverture_fr,
            `Amplitude saisonnière 2003-2013 (°C)` = format_fr_num(amp_p1, digits = 2),
            `Amplitude saisonnière 2014-2024 (°C)` = format_fr_num(amp_p2, digits = 2),
            `Variation amplitude (°C)` = format_fr_signed(variation_amp, digits = 2),
            `Différence moyenne vs forêt non perturbée (2003-2013 / 2014-2024) (°C)` = diff_pair,
            `Variation du différentiel (°C)` = diff_variation
          )
        monthly_overview_stub <- glue(FILENAME_MONTHLY_OVERVIEW, TERRITORY = TERRITORY)
        monthly_overview_path <- file.path(metrics_dir, glue("{monthly_overview_stub}.csv"))
        readr::write_csv(monthly_overview_table, monthly_overview_path, na = "")

        monthly_trends_stub <- glue(FILENAME_MONTHLY_TRENDS, TERRITORY = TERRITORY)
        monthly_trends_path <- file.path(metrics_dir, glue("{monthly_trends_stub}.csv"))
        readr::write_csv(monthly_trends_table, monthly_trends_path, na = "")

        message(glue("[metrics] Saved monthly temperature metrics to {basename(metrics_dir)} for {TERRITORY}"))
      }

      pM <- plot_temp_monthly_boxes(monthly_df, LANG, territory_title)
      if (WRITE_PLOT) {
        file_stub <- glue(FILENAME_MONTHLY, TERRITORY = TERRITORY, LANG = LANG)

        png_path  <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))
        ggsave(png_path, pM, width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS, dpi = DPI, bg = "white")
        if (isTRUE(WRITE_SVG)) {
          svg_path <- file.path(OUTPUT_DIR, glue("{file_stub}.svg"))
          ggsave(svg_path, pM, width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS, device = svglite, bg = "white")
        }
        message("✅ Saved monthly:")
        message(glue("   PNG: {basename(png_path)}"))
        message(glue("   SVG: {if (isTRUE(WRITE_SVG)) basename(svg_path) else '(skipped)'}"))
      } else {
        print(pM)
        message("ℹ Monthly preview mode — set WRITE_PLOT <- TRUE to export.")
      }
    }
    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}

message("✓ Temperature boxplots done.")