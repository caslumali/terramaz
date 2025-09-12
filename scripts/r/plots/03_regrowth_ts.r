##%###########################################################################%##
#                                                                               #
#                         Regrowth Time Series (TMF-only)                    ----
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

# Territories to process
# TERRITORIES <- c("cotriguacu")  # quick test
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Fixed full-page figure size (A4 width) to match other figures
FILENAME_STUB <- "regrowth_ts"
FIG_WIDTH_MM  <- 431.8   # 17 in — full page width
FIG_HEIGHT_MM <- 152.4   # 6 in  — consistent height
UNITS         <- "mm"
DPI           <- 300

# Year trimming for plotting (e.g., drop 1990 and the last 2 uncertain years)
DROP_FIRST_YEARS <- 1   # drops 1990
DROP_LAST_YEARS  <- 2   # drops 2023–2024

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Titles
  title_regrowth_in = c(
    fr = "Évolution annuelle de la régénération à {territory}",
    es = "Evolución anual de la regeneración en {territory}",
    pt = "Evolução anual da regeneração em {territory}",
    en = "Annual evolution of regrowth in {territory}"
  ),
  # Axes
  x_year = c(fr = "Année", pt = "Ano", es = "Año", en = "Year"),
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
# Single source (TMF). Color distinct from degradation.
source_line_colors <- c(`JRC-TMF` = "#0c8b65ff")
source_line_types  <- c(`JRC-TMF` = "solid")

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

axis_x_years_all <- function(year_min, year_max) {
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 1),
    expand = expansion(mult = c(0.01, 0.02))
  )
}

# axis_y_thousands_auto <- function(y_max_raw) {
#   ymax <- max(0, as.numeric(y_max_raw))
#   if (!is.finite(ymax) || ymax <= 0) ymax <- 1000

#   # Escolhe a família de steps conforme o tamanho da série
#   if (ymax <= 2500) {
#     step_candidates <- c(100, 200, 250, 500, 1000, 2000)  # < 2.5k ha
#     label_acc <- 0.1  # mostra 0.1, 0.2, ... mil ha
#   } else if (ymax <= 7000) {
#     step_candidates <- c(500, 1000, 2000, 2500, 5000)     # 2.5k–7k ha
#     label_acc <- 0.5
#   } else {
#     step_candidates <- c(1000, 2000, 5000, 10000, 20000, 50000)  # > 7k ha
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
#     expand = expansion(mult = c(0.03, 0.03))  # margem para não “cortar” perto do zero
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

## 2.1 Map CSV column names to TMF (regrowth) ----
# Robust mapping: accepts *area_ha* with "tmf" and any regrowth-like token.
map_col_to_tmf_regrowth <- function(colname) {
  z <- tolower(colname)
  is_area <- str_detect(z, "area_ha")
  has_tmf <- str_detect(z, "tmf")
  has_reg <- str_detect(z, "regrow|regen|regener|recovery|reveg|reforest")
  if (is_area && (has_tmf || has_reg)) return("JRC-TMF")
  if (is_area) return("JRC-TMF")  # fallback for TMF-only CSVs
  NA_character_
}

## 2.2 Caption (TMF coverage + shown range) ----
caption_tmf <- function(lang = "fr", source_span = "1990–2024",
                        shown_min = NULL, shown_max = NULL) {
  prefix <- switch(lang, "fr"="Sources — ", "pt"="Fontes — ",
                         "es"="Fuentes — ", "en"="Sources — ", "Sources — ")
  shown  <- if (!is.null(shown_min) && !is.null(shown_max)) {
    switch(lang,
      "fr" = glue::glue(" • Plage affichée : {shown_min}–{shown_max}"),
      "pt" = glue::glue(" • Faixa exibida: {shown_min}–{shown_max}"),
      "es" = glue::glue(" • Intervalo mostrado: {shown_min}–{shown_max}"),
      "en" = glue::glue(" • Range shown: {shown_min}–{shown_max}")
    )
  } else ""
  paste0(prefix, "JRC-TMF: ", source_span, shown)
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
    OUTPUT_DIR <- file.path("results/plots", TERRITORY, glue("{TERRITORY}_{LANG}"))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 3.1 Load regrowth CSV (TMF-only) ----
    # ----------------------------------------------------------------------- - - -
    main_csv <- list.files(
      INPUT_DIR,
      pattern = glue("^{TERRITORY}_regrowth_.*\\.csv$"),
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

    # Identify '*_area_ha' columns belonging to TMF regrowth
    area_cols <- names(df)[str_detect(tolower(names(df)), "area_ha$")]
    if (length(area_cols) == 0) {
      message("⚠ No '*_area_ha' columns found — skipping.")
      next
    }
    message(glue("✓ Area columns: {paste(area_cols, collapse=', ')}"))

    # Long format with TMF-only mapping
    long_main <- df %>%
      select(all_of(c(year_col, area_cols))) %>%
      pivot_longer(cols = all_of(area_cols), names_to = "var", values_to = "area_ha") %>%
      mutate(
        source_used = vapply(var, map_col_to_tmf_regrowth, character(1)),
        year        = as.integer(.data[[year_col]]),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      filter(!is.na(source_used), !is.na(year), !is.na(area_ha)) %>%
      select(year, source_used, area_ha) %>%
      arrange(year, source_used)

    ### 3.2 Filter, cast, and validate ----
    # ----------------------------------------------------------------------- - - -
    df_long <- long_main %>%
      mutate(
        source_used = factor(source_used, levels = "JRC-TMF"),
        year        = as.integer(year),
        area_ha     = pmax(suppressWarnings(as.numeric(area_ha)), 0)
      ) %>%
      arrange(year, source_used) %>%
      group_by(source_used) %>%
      mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after filtering/mapping — skipping.")
      next
    }

    present_sources <- levels(droplevels(df_long$source_used))  # "JRC-TMF"
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)

    # Determine plotting window after configured drops
    plot_min <- year_min + DROP_FIRST_YEARS
    plot_max <- year_max - DROP_LAST_YEARS

    # Apply plotting window filter
    df_plot <- df_long %>%
      filter(!is.na(area_ha), dplyr::between(year, plot_min, plot_max)) %>%
      droplevels()

    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]

    ### 3.3 Plot ----
    # ------------------------------------------------------------------------- - - -
    p <- ggplot(df_plot, aes(x = year, y = area_ha, color = source_used, linetype = source_used)) +
      geom_line(linewidth = 1.2, lineend = "round", na.rm = TRUE) +
      geom_point(size = 4, stroke = 0, na.rm = TRUE) +
      scale_color_manual(values = cols, breaks = present_sources, guide = guide_legend(title = NULL)) +
      scale_linetype_manual(values = ltys, breaks = present_sources, guide = "none") +
      axis_x_years_all(plot_min, plot_max) +
      # axis_y_thousands_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      axis_y_ha_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_regrowth_in", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_tmf(LANG, shown_min = plot_min, shown_max = plot_max)
      ) +
      theme_time_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 3.4 Export (fixed full-page size) ----
    # ------------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue("03_{TERRITORY}_regrowth_{LANG}")

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
