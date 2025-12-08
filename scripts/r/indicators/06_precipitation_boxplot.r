##%###########################################################################%##
#                                                                               #
#                         Precipitation Boxplots (TMF)                       ----
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
FILENAME_ANUAL            <- "06a_{TERRITORY}_precipitation_annual_{LANG}"
FILENAME_MONTHLY          <- "06b_{TERRITORY}_precipitation_monthly_{LANG}"
FILENAME_ANNUAL_OVERVIEW  <- "06a_{TERRITORY}_precipitation_annual_overview"
FILENAME_ANNUAL_TRENDS    <- "06a_{TERRITORY}_precipitation_annual_trends"
FILENAME_MONTHLY_OVERVIEW <- "06b_{TERRITORY}_precipitation_monthly_overview"
FILENAME_MONTHLY_TRENDS   <- "06b_{TERRITORY}_precipitation_monthly_trends"

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
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"
# LANGS <- c("fr", "es", "pt", "en")

LABELS <- list(
  # Titles
  title_precip_annual_in = c(
    fr = "Précipitations annuelles à {territory}",
    es = "Precipitación anual en {territory}",
    pt = "Precipitação anual em {territory}",
    en = "Annual precipitation in {territory}"
  ),
  title_precip_monthly_in = c(
    fr = "Climatologie mensuelle des précipitations à {territory}",
    es = "Climatología mensual de precipitación en {territory}",
    pt = "Climatologia mensal de precipitação em {territory}",
    en = "Monthly precipitation climatology in {territory}"
  ),
  # Axes
  x_year = c(fr="Année", es="Año", pt="Ano", en="Year"),
  x_month = c(fr="Mois", es="Mes", pt="Mês", en="Month"),
  y_precip = c(
  fr = "Précipitation (mm)",
  es = "Precipitación (mm)",
  pt = "Precipitação (mm)",
  en = "Precipitation (mm)"
),
  # Caption
  caption_precip = c(
    fr = "Sources — CHIRPS (pentades → sommes mensuelles/annuelles), masque TMF-JRC médian (1 km); 2003–2024",
    es = "Fuentes — CHIRPS (péntadas → sumas mensuales/anuales), máscara TMF-JRC mediana (1 km); 2003–2024",
    pt = "Fontes — CHIRPS (pêntadas → somas mensais/anuais), máscara TMF-JRC mediana (1 km); 2003–2024",
    en = "Sources — CHIRPS (pentads → monthly/annual sums), TMF-JRC median mask (1 km); 2003–2024"
  )
)

# Labels for TMF classes per language
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
# Fill palette
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
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 13, margin = margin(t = 12)),
      axis.title.y = element_text(size = 13, margin = margin(r = 12)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e6e6e6", linewidth = 0.3),
      legend.position = "top",
      legend.justification = "right",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      legend.key.size = unit(2.2, "lines"),
      plot.margin = margin(12, 12, 12, 12),
      plot.caption = element_text(hjust = 1, size = 11, color = "gray30", margin = margin(t = 12))
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
compute_y_limits <- function(df, value_col = "precipitation_mm", territory = NULL) {
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

trend_slope <- function(x, y) {
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) < 2) return(NA_real_)
  mod <- tryCatch(stats::lm(y[valid] ~ x[valid]), error = function(...) NULL)
  if (is.null(mod)) return(NA_real_)
  stats::coef(mod)[[2]]
}

format_fr_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "",
    format(
      round(as.numeric(x), digits),
      big.mark = " ",
      decimal.mark = ",",
      trim = TRUE,
      scientific = FALSE,
      nsmall = digits
    )
  )
}

format_fr_signed <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "",
    paste0(
      ifelse(x > 0, "+", ifelse(x < 0, "-", "")),
      format_fr_num(abs(x), digits = digits)
    )
  )
}

format_year_value <- function(year, value, digits = 0) {
  out <- ifelse(
    is.na(year) | is.na(value),
    "",
    paste0(year, " (", format_fr_num(value, digits = digits), ")")
  )
  as.character(out)
}

format_period_pair <- function(value1, value2, digits = 0) {
  paste0(format_fr_num(value1, digits = digits), " / ", format_fr_num(value2, digits = digits))
}

build_precip_annual_tables <- function(df) {
  if (is.null(df)) {
    return(list(overview = tibble(), trends = tibble()))
  }

  df_clean <- df %>%
    filter(!is.na(year), !is.na(precipitation_mm), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()

  if (nrow(df_clean) == 0) {
    return(list(overview = tibble(), trends = tibble()))
  }

  year_range_label <- glue::glue("{YEAR_MIN}-{YEAR_MAX}")
  period1_end <- min(YEAR_MIN + 10, YEAR_MAX)
  period2_start <- min(period1_end + 1, YEAR_MAX)
  period1_years <- seq(YEAR_MIN, period1_end)
  period2_years <- seq(period2_start, YEAR_MAX)
  period1_label <- glue::glue("{YEAR_MIN}-{period1_end}")
  period2_label <- glue::glue("{period2_start}-{YEAR_MAX}")

  annual_medians <- df_clean %>%
    group_by(tmf_class_label, year, .drop = FALSE) %>%
    summarise(median_precip = median(precipitation_mm, na.rm = TRUE), .groups = "drop")

  annual_diff <- annual_medians %>%
    left_join(
      annual_medians %>%
        filter(tmf_class_label == "Undisturbed") %>%
        select(year, undist_median = median_precip),
      by = "year"
    ) %>%
    mutate(diff_vs_undist = median_precip - undist_median)

  tmf_fr <- TMF_LABELS[["fr"]]

  overview <- annual_medians %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    summarise(
      median_all = median(median_precip, na.rm = TRUE),
      amplitude_all = max(median_precip, na.rm = TRUE) - min(median_precip, na.rm = TRUE),
      wettest_year = year[which.max(median_precip)],
      wettest_median = max(median_precip, na.rm = TRUE),
      driest_year = year[which.min(median_precip)],
      driest_median = min(median_precip, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      annual_diff %>%
        group_by(tmf_class_label, .drop = FALSE) %>%
        summarise(mean_diff_vs_undist = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop"),
      by = "tmf_class_label"
    ) %>%
    mutate(
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      wettest_label = format_year_value(wettest_year, wettest_median, digits = 0),
      driest_label = format_year_value(driest_year, driest_median, digits = 0),
      diff_label = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "—",
        TRUE ~ format_fr_signed(mean_diff_vs_undist, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      !!glue::glue("Précipitation médiane {year_range_label} (mm)") := format_fr_num(median_all, digits = 0),
      !!glue::glue("Amplitude {year_range_label} (mm)") := format_fr_num(amplitude_all, digits = 0),
      `Année la plus humide (mm)` = wettest_label,
      `Année la plus sèche (mm)` = driest_label,
      `Différence moyenne vs forêt non perturbée (mm)` = diff_label
    )

  annual_periods <- annual_medians %>%
    mutate(
      period = dplyr::case_when(
        year %in% period1_years ~ period1_label,
        year %in% period2_years ~ period2_label,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))

  trends <- annual_periods %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(median_period = median(median_precip, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = median_period) %>%
    left_join(
      annual_medians %>%
        group_by(tmf_class_label, .drop = FALSE) %>%
        summarise(trend_decade = trend_slope(year, median_precip) * 10, .groups = "drop"),
      by = "tmf_class_label"
    )

  if (!(period1_label %in% names(trends))) trends[[period1_label]] <- NA_real_
  if (!(period2_label %in% names(trends))) trends[[period2_label]] <- NA_real_

  diff_period <- annual_periods %>%
    left_join(
      annual_periods %>%
        filter(tmf_class_label == "Undisturbed") %>%
        select(period, year, ref_median = median_precip),
      by = c("period", "year")
    ) %>%
    mutate(diff_vs_undist = median_precip - ref_median) %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(mean_diff = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = mean_diff, names_glue = "diff_{period}")

  if (!(glue::glue("diff_{period1_label}") %in% names(diff_period))) diff_period[[glue::glue("diff_{period1_label}")]] <- NA_real_
  if (!(glue::glue("diff_{period2_label}") %in% names(diff_period))) diff_period[[glue::glue("diff_{period2_label}")]] <- NA_real_

  trends <- trends %>%
    left_join(diff_period, by = "tmf_class_label") %>%
    mutate(
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      median_p1 = .[[period1_label]],
      median_p2 = .[[period2_label]],
      variation = median_p2 - median_p1,
      diff_p1 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period1_label}")]]
      ),
      diff_p2 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period2_label}")]]
      ),
      diff_pair = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "— / —",
        TRUE ~ format_period_pair(diff_p1, diff_p2, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      !!glue::glue("Médiane {period1_label} (mm)") := format_fr_num(median_p1, digits = 0),
      !!glue::glue("Médiane {period2_label} (mm)") := format_fr_num(median_p2, digits = 0),
      `Variation (mm)` = format_fr_signed(variation, digits = 0),
      `Tendance (mm par décennie)` = format_fr_signed(trend_decade, digits = 0),
      !!glue::glue("Différence moyenne vs forêt non perturbée ({period1_label} / {period2_label}) (mm)") := diff_pair
    )

  list(overview = overview, trends = trends)
}

build_precip_monthly_tables <- function(df) {
  if (is.null(df)) {
    return(list(overview = tibble(), trends = tibble()))
  }

  df_clean <- df %>%
    filter(!is.na(year), !is.na(month), !is.na(precipitation_mm), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()
  }

trend_slope <- function(x, y) {
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) < 2) return(NA_real_)
  mod <- tryCatch(stats::lm(y[valid] ~ x[valid]), error = function(...) NULL)
  if (is.null(mod)) return(NA_real_)
  stats::coef(mod)[[2]]
}

format_fr_num <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "",
    format(
      round(as.numeric(x), digits),
      big.mark = " ",
      decimal.mark = ",",
      trim = TRUE,
      scientific = FALSE,
      nsmall = digits
    )
  )
}

format_fr_signed <- function(x, digits = 0) {
  ifelse(
    is.na(x),
    "",
    paste0(
      ifelse(x > 0, "+", ifelse(x < 0, "-", "")),
      format_fr_num(abs(x), digits = digits)
    )
  )
}

format_period_pair <- function(value1, value2, digits = 0) {
  paste0(format_fr_num(value1, digits = digits), " / ", format_fr_num(value2, digits = digits))
}

build_precip_annual_tables <- function(df) {
  if (is.null(df)) {
    return(list(overview = tibble(), trends = tibble()))
  }

  df_clean <- df %>%
    filter(!is.na(year), !is.na(precipitation_mm), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()

  if (nrow(df_clean) == 0) {
    return(list(overview = tibble(), trends = tibble()))
  }

  year_range_label <- glue::glue("{YEAR_MIN}-{YEAR_MAX}")
  period1_end <- min(YEAR_MIN + 10, YEAR_MAX)
  period2_start <- min(period1_end + 1, YEAR_MAX)
  period1_years <- seq(YEAR_MIN, period1_end)
  period2_years <- seq(period2_start, YEAR_MAX)
  period1_label <- glue::glue("{YEAR_MIN}-{period1_end}")
  period2_label <- glue::glue("{period2_start}-{YEAR_MAX}")

  annual_medians <- df_clean %>%
    group_by(tmf_class_label, year, .drop = FALSE) %>%
    summarise(median_precip = median(precipitation_mm, na.rm = TRUE), .groups = "drop")

  annual_diff <- annual_medians %>%
    left_join(
      annual_medians %>%
        filter(tmf_class_label == "Undisturbed") %>%
        select(year, undist_median = median_precip),
      by = "year"
    ) %>%
    mutate(diff_vs_undist = median_precip - undist_median)

  tmf_fr <- TMF_LABELS[["fr"]]

  overview <- annual_medians %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    summarise(
      median_all = median(median_precip, na.rm = TRUE),
      amplitude_all = max(median_precip, na.rm = TRUE) - min(median_precip, na.rm = TRUE),
      wettest_year = year[which.max(median_precip)],
      wettest_median = max(median_precip, na.rm = TRUE),
      driest_year = year[which.min(median_precip)],
      driest_median = min(median_precip, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      annual_diff %>%
        group_by(tmf_class_label, .drop = FALSE) %>%
        summarise(mean_diff_vs_undist = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop"),
      by = "tmf_class_label"
    ) %>%
    mutate(
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      wettest_label = format_year_value(wettest_year, wettest_median, digits = 0),
      driest_label = format_year_value(driest_year, driest_median, digits = 0),
      diff_label = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "—",
        TRUE ~ format_fr_signed(mean_diff_vs_undist, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      !!glue::glue("Précipitation médiane {year_range_label} (mm)") := format_fr_num(median_all, digits = 0),
      !!glue::glue("Amplitude {year_range_label} (mm)") := format_fr_num(amplitude_all, digits = 0),
      `Année la plus humide (mm)` = wettest_label,
      `Année la plus sèche (mm)` = driest_label,
      `Différence moyenne vs forêt non perturbée (mm)` = diff_label
    )

  annual_periods <- annual_medians %>%
    mutate(
      period = dplyr::case_when(
        year %in% period1_years ~ period1_label,
        year %in% period2_years ~ period2_label,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period))

  trends <- annual_periods %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(median_period = median(median_precip, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = median_period) %>%
    left_join(
      annual_medians %>%
        group_by(tmf_class_label, .drop = FALSE) %>%
        summarise(trend_decade = trend_slope(year, median_precip) * 10, .groups = "drop"),
      by = "tmf_class_label"
    )

  if (!(period1_label %in% names(trends))) trends[[period1_label]] <- NA_real_
  if (!(period2_label %in% names(trends))) trends[[period2_label]] <- NA_real_

  diff_period <- annual_periods %>%
    left_join(
      annual_periods %>%
        filter(tmf_class_label == "Undisturbed") %>%
        select(period, year, ref_median = median_precip),
      by = c("period", "year")
    ) %>%
    mutate(diff_vs_undist = median_precip - ref_median) %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(mean_diff = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = mean_diff, names_glue = "diff_{period}")

  if (!(glue::glue("diff_{period1_label}") %in% names(diff_period))) diff_period[[glue::glue("diff_{period1_label}")]] <- NA_real_
  if (!(glue::glue("diff_{period2_label}") %in% names(diff_period))) diff_period[[glue::glue("diff_{period2_label}")]] <- NA_real_

  trends <- trends %>%
    left_join(diff_period, by = "tmf_class_label") %>%
    mutate(
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      median_p1 = .[[period1_label]],
      median_p2 = .[[period2_label]],
      variation = median_p2 - median_p1,
      diff_p1 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period1_label}")]]
      ),
      diff_p2 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period2_label}")]]
      ),
      diff_pair = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "— / —",
        TRUE ~ format_period_pair(diff_p1, diff_p2, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      !!glue::glue("Médiane {period1_label} (mm)") := format_fr_num(median_p1, digits = 0),
      !!glue::glue("Médiane {period2_label} (mm)") := format_fr_num(median_p2, digits = 0),
      `Variation (mm)` = format_fr_signed(variation, digits = 0),
      `Tendance (mm par décennie)` = format_fr_signed(trend_decade, digits = 0),
      !!glue::glue("Différence moyenne vs forêt non perturbée ({period1_label} / {period2_label}) (mm)") := diff_pair
    )

  list(overview = overview, trends = trends)
}

build_precip_monthly_tables <- function(df) {
  if (is.null(df)) {
    return(list(overview = tibble(), trends = tibble()))
  }

  df_clean <- df %>%
    filter(!is.na(year), !is.na(month), !is.na(precipitation_mm), !is.na(tmf_class_label)) %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    drop_na()

  if (nrow(df_clean) == 0) {
    return(list(overview = tibble(), trends = tibble()))
  }

  period1_end <- min(YEAR_MIN + 10, YEAR_MAX)
  period2_start <- min(period1_end + 1, YEAR_MAX)
  period1_years <- seq(YEAR_MIN, period1_end)
  period2_years <- seq(period2_start, YEAR_MAX)
  period1_label <- glue::glue("{YEAR_MIN}-{period1_end}")
  period2_label <- glue::glue("{period2_start}-{YEAR_MAX}")

  monthly_summaries <- df_clean %>%
    group_by(tmf_class_label, month, .drop = FALSE) %>%
    summarise(
      median_precip = median(precipitation_mm, na.rm = TRUE),
      q25_precip = quantile(precipitation_mm, 0.25, na.rm = TRUE),
      q75_precip = quantile(precipitation_mm, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(iqr = q75_precip - q25_precip)

  tmf_fr <- TMF_LABELS[["fr"]]
  month_fr <- MONTH_LABELS[["fr"]]

  monthly_overview <- monthly_summaries %>%
    group_by(tmf_class_label, .drop = FALSE) %>%
    summarise(
      wettest_month = month[which.max(median_precip)],
      wettest_median = max(median_precip, na.rm = TRUE),
      driest_month = month[which.min(median_precip)],
      driest_median = min(median_precip, na.rm = TRUE),
      seasonal_amplitude = wettest_median - driest_median,
      mean_iqr = mean(iqr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      monthly_summaries %>%
        left_join(
          monthly_summaries %>%
            filter(tmf_class_label == "Undisturbed") %>%
            select(month, ref_median = median_precip),
          by = "month"
        ) %>%
        mutate(diff_vs_undist = median_precip - ref_median) %>%
        group_by(tmf_class_label, .drop = FALSE) %>%
        summarise(mean_diff_vs_undist = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop"),
      by = "tmf_class_label"
    ) %>%
    mutate(
      tmf_class_label = as.character(tmf_class_label),
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      wettest_label = if_else(
        is.na(wettest_month),
        "",
        glue::glue("{month_fr[wettest_month]} ({format_fr_num(wettest_median, digits = 0)})")
      ),
      driest_label = if_else(
        is.na(driest_month),
        "",
        glue::glue("{month_fr[driest_month]} ({format_fr_num(driest_median, digits = 0)})")
      ),
      diff_label = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "—",
        TRUE ~ format_fr_signed(mean_diff_vs_undist, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      `Mois le plus humide / médiane (mm)` = wettest_label,
      `Mois le plus sec / médiane (mm)` = driest_label,
      `Amplitude saisonnière (mm)` = format_fr_num(seasonal_amplitude, digits = 0),
      `IQR moyen (mm)` = format_fr_num(mean_iqr, digits = 0),
      `Déficit moyen vs forêt non perturbée (mm)` = diff_label
    )

  monthly_year_summary <- df_clean %>%
    mutate(
      period = dplyr::case_when(
        year %in% period1_years ~ period1_label,
        year %in% period2_years ~ period2_label,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(period)) %>%
    group_by(tmf_class_label, period, year, month, .drop = FALSE) %>%
    summarise(median_precip = median(precipitation_mm, na.rm = TRUE), .groups = "drop")

  monthly_amplitude <- monthly_year_summary %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(amplitude = max(median_precip, na.rm = TRUE) - min(median_precip, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = amplitude)

  if (!(period1_label %in% names(monthly_amplitude))) monthly_amplitude[[period1_label]] <- NA_real_
  if (!(period2_label %in% names(monthly_amplitude))) monthly_amplitude[[period2_label]] <- NA_real_

  monthly_diff <- monthly_year_summary %>%
    left_join(
      monthly_year_summary %>%
        filter(tmf_class_label == "Undisturbed") %>%
        select(period, year, month, ref_precip = median_precip),
      by = c("period", "year", "month")
    ) %>%
    mutate(diff_vs_undist = median_precip - ref_precip) %>%
    group_by(tmf_class_label, period, .drop = FALSE) %>%
    summarise(mean_diff = mean(diff_vs_undist, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = mean_diff, names_glue = "diff_{period}")

  if (!(glue::glue("diff_{period1_label}") %in% names(monthly_diff))) monthly_diff[[glue::glue("diff_{period1_label}")]] <- NA_real_
  if (!(glue::glue("diff_{period2_label}") %in% names(monthly_diff))) monthly_diff[[glue::glue("diff_{period2_label}")]] <- NA_real_

  monthly_trends <- monthly_amplitude %>%
    left_join(monthly_diff, by = "tmf_class_label") %>%
    mutate(
      tmf_class_label = as.character(tmf_class_label),
      couverture_fr = tmf_fr[tmf_class_label] %||% tmf_class_label,
      amp_p1 = .[[period1_label]],
      amp_p2 = .[[period2_label]],
      variation_amp = amp_p2 - amp_p1,
      diff_p1 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period1_label}")]]
      ),
      diff_p2 = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ 0,
        TRUE ~ .[[glue::glue("diff_{period2_label}")]]
      ),
      diff_pair = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "— / —",
        TRUE ~ format_period_pair(diff_p1, diff_p2, digits = 0)
      ),
      diff_variation = dplyr::case_when(
        tmf_class_label == "Undisturbed" ~ "—",
        TRUE ~ format_fr_signed(diff_p2 - diff_p1, digits = 0)
      )
    ) %>%
    transmute(
      Couverture = couverture_fr,
      !!glue::glue("Amplitude saisonnière {period1_label} (mm)") := format_fr_num(amp_p1, digits = 0),
      !!glue::glue("Amplitude saisonnière {period2_label} (mm)") := format_fr_num(amp_p2, digits = 0),
      `Variation amplitude (mm)` = format_fr_signed(variation_amp, digits = 0),
      !!glue::glue("Déficit moyen vs forêt non perturbée ({period1_label} / {period2_label}) (mm)") := diff_pair,
      `Variation du déficit (mm)` = diff_variation
    )

  list(overview = monthly_overview, trends = monthly_trends)
}

## 2.2 Plot builders ----
# ------------------------------------------------------------------------- - - -
plot_precip_annual_boxes <- function(df, lang, territory_title) {
  # df: pixel-level annual data with columns year, precipitation_mm, tmf_class_label
  df <- df %>%
    mutate(
      year  = as.integer(year),
      year_f = factor(as.character(year), levels = as.character(seq.int(YEAR_MIN, YEAR_MAX, 1))),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    filter(!is.na(year_f), !is.na(precipitation_mm), !is.na(tmf_class_label))

  # Optional downsampling to keep groups balanced and fast
  if (is.finite(MAX_ROWS_PER_GROUP)) {
    df <- df %>%
      group_by(year_f, tmf_class_label) %>%
      slice_sample(n = min(MAX_ROWS_PER_GROUP, n()), replace = FALSE) %>%
      ungroup()
  }

  y_limits <- compute_y_limits(df, "precipitation_mm", territory = tolower(territory_title))

  ggplot(
    df,
    aes(x = year_f, y = precipitation_mm, fill = tmf_class_label, color = tmf_class_label)
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
      title   = label("title_precip_annual_in", territory = territory_title),
      x       = label("x_year"),
      y       = label("y_precip"),
      caption = label("caption_precip")
    ) +
    theme_boxplots() +
    guides(fill = guide_legend(nrow = 1))
}

plot_precip_monthly_boxes <- function(df, lang, territory_title) {
  # df: pixel-level monthly data with columns year, month, precipitation_mm, tmf_class_label
  df <- df %>%
    mutate(
      month   = as.integer(month),
      month_f = factor(sprintf("%02d", month), levels = sprintf("%02d", 1:12)),
      tmf_class_label = factor(tmf_class_label, levels = TMF_CLASS_LEVELS)
    ) %>%
    filter(!is.na(month_f), !is.na(precipitation_mm), !is.na(tmf_class_label))

  # Optional downsampling to keep groups balanced and fast
  if (is.finite(MAX_ROWS_PER_GROUP)) {
    df <- df %>%
      group_by(month_f, tmf_class_label) %>%
      slice_sample(n = min(MAX_ROWS_PER_GROUP, n()), replace = FALSE) %>%
      ungroup()
  }

  y_limits <- compute_y_limits(df, "precipitation_mm", territory = tolower(territory_title))

  ggplot(
    df,
    aes(x = month_f, y = precipitation_mm, fill = tmf_class_label, color = tmf_class_label)
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
      title   = label("title_precip_monthly_in", territory = territory_title),
      x       = label("x_month"),
      y       = label("y_precip"),
      caption = label("caption_precip")
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
      col_precip <- detect_col(names(annual_df), "precip|precipitation_?mm")
      col_year   <- detect_col(names(annual_df), "^year$|\\byear\\b|ano|annee|anno")
      col_tmf    <- detect_col(names(annual_df), "tmf.*label|class.*label")
      if (is.na(col_precip) || is.na(col_year) || is.na(col_tmf)) {
        stop(glue(
          "Annual CSV missing required columns. Found: precip='{col_precip}', year='{col_year}', tmf_label='{col_tmf}'."
        ), call. = FALSE)
      }
      annual_df <- annual_df %>%
        transmute(
          year = .data[[col_year]],
          precipitation_mm = suppressWarnings(as.numeric(.data[[col_precip]])),
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
      col_precip  <- detect_col(names(monthly_df), "precip|precipitation_?mm")
      col_year   <- detect_col(names(monthly_df), "^year$|\\byear\\b|ano|annee|anno")
      col_month  <- detect_col(names(monthly_df), "^month$|\\bmonth\\b|mois|mes")
      col_tmf    <- detect_col(names(monthly_df), "tmf.*label|class.*label")
      if (is.na(col_precip) || is.na(col_year) || is.na(col_month) || is.na(col_tmf)) {
        stop(glue(
          "Monthly CSV missing required columns. Found: precip='{col_precip}', year='{col_year}', month='{col_month}', tmf_label='{col_tmf}'."
        ), call. = FALSE)
      }
      monthly_df <- monthly_df %>%
        transmute(
          year = .data[[col_year]],
          month = .data[[col_month]],
          precipitation_mm = suppressWarnings(as.numeric(.data[[col_precip]])),
          tmf_class_label = as.character(.data[[col_tmf]])
        )
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]] %||% TERRITORY

    if (identical(LANG, LANGS[[1]])) {
      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

      annual_tables <- build_precip_annual_tables(annual_df)
      if (nrow(annual_tables$overview) > 0) {
        annual_overview_stub <- glue(FILENAME_ANNUAL_OVERVIEW, TERRITORY = TERRITORY)
        annual_overview_path <- file.path(metrics_dir, glue("{annual_overview_stub}.csv"))
        readr::write_csv(annual_tables$overview, annual_overview_path, na = "")
      }
      if (nrow(annual_tables$trends) > 0) {
        annual_trends_stub <- glue(FILENAME_ANNUAL_TRENDS, TERRITORY = TERRITORY)
        annual_trends_path <- file.path(metrics_dir, glue("{annual_trends_stub}.csv"))
        readr::write_csv(annual_tables$trends, annual_trends_path, na = "")
      }

      monthly_tables <- build_precip_monthly_tables(monthly_df)
      if (nrow(monthly_tables$overview) > 0) {
        monthly_overview_stub <- glue(FILENAME_MONTHLY_OVERVIEW, TERRITORY = TERRITORY)
        monthly_overview_path <- file.path(metrics_dir, glue("{monthly_overview_stub}.csv"))
        readr::write_csv(monthly_tables$overview, monthly_overview_path, na = "")
      }
      if (nrow(monthly_tables$trends) > 0) {
        monthly_trends_stub <- glue(FILENAME_MONTHLY_TRENDS, TERRITORY = TERRITORY)
        monthly_trends_path <- file.path(metrics_dir, glue("{monthly_trends_stub}.csv"))
        readr::write_csv(monthly_tables$trends, monthly_trends_path, na = "")
      }

      message(glue("[metrics] Saved precipitation overview/trend tables to {basename(metrics_dir)} for {TERRITORY}"))
    }

    ## 3.4 Annual plot ----
    # ----------------------------------------------------------------------- - - -
    if (!is.null(annual_df)) {
      pA <- plot_precip_annual_boxes(annual_df, LANG, territory_title)
      if (WRITE_PLOT) {
        file_stub <- glue(FILENAME_ANUAL, TERRITORY = TERRITORY, LANG = LANG)
        
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

    ## 3.5 Monthly climatology plot ----
    # ----------------------------------------------------------------------- - - -
    if (!is.null(monthly_df)) {
      pM <- plot_precip_monthly_boxes(monthly_df, LANG, territory_title)
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

message("✓ Precipitation boxplots done.")
