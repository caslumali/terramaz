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

# I/O roots
ROOT_IN  <- "results/metrics"
ROOT_OUT <- "results/plots"

# File name stub and figure size
FILENAME_STUB <- "temp_box"
FIG_WIDTH_MM  <- 431.8  # 17 in
FIG_HEIGHT_MM <- 220    # a bit taller for boxplots
UNITS         <- "mm"
DPI           <- 300

# Performance guard: use all rows by default. If files are huge, set a cap per group.
MAX_ROWS_PER_GROUP <- Inf  # e.g., 20000 to speed up if needed

# Expected year range (used for axis control and QA)
YEAR_MIN <- 2005
YEAR_MAX <- 2024

# X-axis steps (in arbitrary units; used for spacing computations if needed)
DRAW_SEP_LINES <- TRUE 
SEP_LINE_COLOR <- "#afafafff"
SEP_LINE_WIDTH <- 0.25
SEP_LINE_ALPHA <- 0.9

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"
LABELS <- list(
  # Titles
  title_temp_annual_in = c(
    fr = "Température annuelle à {territory}",
    es = "Temperatura anual en {territory}",
    pt = "Temperatura anual em {territory}",
    en = "Annual temperature in {territory}"
  ),
  title_temp_monthly_in = c(
    fr = "Climatologie mensuelle (jour) à {territory}",
    es = "Climatología mensual (diurna) en {territory}",
    pt = "Climatologia mensal (diurna) em {territory}",
    en = "Monthly climatology (daytime) in {territory}"
  ),
  # Axes
  x_year = c(fr="Année", es="Año", pt="Ano", en="Year"),
  x_month = c(fr="Mois", es="Mes", pt="Mês", en="Month"),
  y_temp = c(fr="Température (°C)", es="Temperatura (°C)", pt="Temperatura (°C)", en="Temperature (°C)"),
  # Caption
  caption_temp = c(
    fr = "Sources — MODIS MYD11A2 (diurne, moyenne/boîtes), masque TMF-JRC médian (1 km); 2003–2024",
    es = "Fuentes — MODIS MYD11A2 (diurno, medias/cajas), máscara TMF-JRC mediana (1 km); 2003–2024",
    pt = "Fontes — MODIS MYD11A2 (diurno, médias/boxplots), máscara TMF-JRC mediana (1 km); 2003–2024",
    en = "Sources — MODIS MYD11A2 (daytime, means/boxplots), TMF-JRC median mask (1 km); 2003–2024"
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

    INPUT_DIR  <- file.path(ROOT_IN, TERRITORY)
    OUTPUT_DIR <- file.path(ROOT_OUT, TERRITORY, glue("{TERRITORY}_{LANG}"))
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

    ## 3.4 Annual plot ----
    # ----------------------------------------------------------------------- - - -
    if (!is.null(annual_df)) {
      pA <- plot_temp_annual_boxes(annual_df, LANG, territory_title)
      if (WRITE_PLOT) {
        file_stub <- glue("06a_{TERRITORY}_temp_annual_box_{LANG}")
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
      pM <- plot_temp_monthly_boxes(monthly_df, LANG, territory_title)
      if (WRITE_PLOT) {
        file_stub <- glue("06b_{TERRITORY}_temp_monthly_box_{LANG}")
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

