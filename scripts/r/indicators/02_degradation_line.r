##%###########################################################################%##
#                                                                               #
#                      Degradation Time Series (TMF-only)                    ----
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
WRITE_PLOT    <- TRUE
WRITE_SVG     <- FALSE
# TERRITORIES <- c("guaviare")  # quick test
TERRITORIES   <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Output file parameters
FILENAME_DEF      <- "02_{territory}_degradation_{lang}"
FILENAME_OVERVIEW <- "02_{territory}_degradation_overview"
FILENAME_TRENDS   <- "02_{territory}_degradation_trends"
FIG_WIDTH_MM      <- 431.8   # 17 in – full page width
FIG_HEIGHT_MM     <- 152.4   # 6 in  – matches fire plots
UNITS             <- "mm"
DPI               <- 300

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
# LANGS <- c("fr")   # "pt" | "es" | "fr" | "en"
LANGS        <- c("fr", "es", "pt", "en")

LABELS <- list(
  # Titles
  title_degradation_in = c(
    fr = "Évolution annuelle de la dégradation à {territory}",
    es = "Evolución anual de la degradación en {territory}",
    pt = "Evolução anual da degradação em {territory}",
    en = "Annual evolution of degradation in {territory}"
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

# Dynamic label function
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

## 1.3 Formatting helpers ----
# ------------------------------------------------------------------------- - - -
format_number_fr <- function(x, digits = 0) {
  if (length(x) == 0) return(character(0))
  digits <- rep_len(digits, length.out = length(x))
  vapply(
    seq_along(x),
    function(i) {
      val <- x[i]
      dig <- digits[i]
      if (is.na(val) || !is.finite(val)) {
        "--"
      } else {
        trimws(formatC(
          round(val, dig),
          format = "f",
          big.mark = " ",
          decimal.mark = ",",
          digits = dig
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

format_year_share_entry <- function(year, value, share) {
  n <- max(length(year), length(value), length(share))
  if (n == 0) return(character(0))
  year <- rep_len(year, n)
  value <- rep_len(value, n)
  share <- rep_len(share, n)
  vapply(
    seq_len(n),
    function(i) {
      if (is.na(year[i]) || is.na(value[i]) || is.na(share[i])) {
        "--"
      } else {
        sprintf(
          "%s (%s) / %s",
          year[i],
          format_number_fr(value[i])[1],
          format_percent_fr(share[i])[1]
        )
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

safe_min_int <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  as.integer(min(x))
}

safe_max_int <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_integer_)
  as.integer(max(x))
}

safe_mean_num <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  out <- mean(x, na.rm = TRUE)
  if (is.nan(out)) NA_real_ else out
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

extreme_info <- function(df, type = c("max", "min")) {
  type <- match.arg(type)
  clean <- dplyr::filter(df, !is.na(area_ha))
  if (nrow(clean) == 0) return(list(year = NA_integer_, value = NA_real_))
  ordered <- if (type == "max") {
    dplyr::arrange(clean, dplyr::desc(area_ha), year)
  } else {
    dplyr::arrange(clean, area_ha, year)
  }
  row <- dplyr::slice_head(ordered, n = 1)
  list(year = as.integer(row$year), value = as.numeric(row$area_ha))
}

has_full_window <- function(year_min, year_max, window_start, window_end) {
  !is.na(year_min) && !is.na(year_max) &&
    year_min <= window_start && year_max >= window_end
}

compute_window_entry <- function(df, window_start, window_end, total_ha, year_min, year_max) {
  if (!has_full_window(year_min, year_max, window_start, window_end)) {
    return("--")
  }
  window_df <- dplyr::filter(df, year >= window_start, year <= window_end, !is.na(area_ha))
  if (nrow(window_df) == 0) return("--")
  mean_value <- safe_mean_num(window_df$area_ha)
  sum_value <- sum(window_df$area_ha, na.rm = TRUE)
  share <- share_fraction(sum_value, total_ha)
  if (is.na(mean_value) || is.na(share)) return("--")
  mean_fmt <- format_number_fr(mean_value)[1]
  share_fmt <- format_percent_fr(share)[1]
  if (mean_fmt == "--" || share_fmt == "--") {
    "--"
  } else {
    sprintf("%s / %s", mean_fmt, share_fmt)
  }
}

## 1.4 Palette for source (lines) ----
# ------------------------------------------------------------------------- - - -
# Only JRC-TMF is plotted here.
source_line_colors <- c(`TMF-JRC` = "#649B23")
source_line_types  <- c(`TMF-JRC` = "solid")

## 1.5 Theme & scales (kept identical to deforestation) ----
# ------------------------------------------------------------------------- - - -
theme_time_series <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 10)),
      axis.text.x         = element_text(size = 11, angle = 45, hjust = 1, vjust = 1),
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

## 2.1 Map CSV column names to TMF (degradation) ----
# ------------------------------------------------------------------------- - - -
# Accept columns that contain *area_ha* and (tmf OR degrad*) to be robust to variants.
map_col_to_tmf <- function(colname) {
  z <- tolower(colname)
  if (str_detect(z, "area_ha") && (str_detect(z, "tmf") || str_detect(z, "degrad"))) return("TMF-JRC")
  if (str_detect(z, "area_ha")) return("TMF-JRC")  # fallback for TMF-only CSVs
  NA_character_
}

## 2.2 Caption (TMF coverage) ----
# ------------------------------------------------------------------------- - - -
caption_tmf <- function(lang = "fr") {
  prefix <- switch(lang, "fr"="Sources — ", "pt"="Fontes — ", "es"="Fuentes — ", "en"="Sources — ", "Sources — ")
  paste0(prefix, "TMF-JRC: 1990–2024")
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
    OUTPUT_DIR <- file.path("results/indicators",   TERRITORY, glue(TERRITORY, '_', LANG))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 3.1 Load degradation CSV (TMF-only) ----
    # ----------------------------------------------------------------------- - - -
    main_csv <- list.files(
      INPUT_DIR,
      pattern = glue("^{TERRITORY}_degradation_.*\\.csv$"),
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

    # Identify '*_area_ha' columns belonging to TMF degradation
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
        source_used = vapply(var, map_col_to_tmf, character(1)),
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
        source_used = factor(source_used, levels = "TMF-JRC"),
        year        = as.integer(year),
        area_ha     = pmax(suppressWarnings(as.numeric(area_ha)), 0)
      ) %>%
      arrange(year, source_used)

    # Trim leading zeros to avoid a flat line at zero
    df_long <- df_long %>%
      group_by(source_used) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      ungroup()

    if (nrow(df_long) == 0) {
      message("⚠ No rows after filtering/mapping — skipping.")
      next
    }

    if (identical(LANG, LANGS[[1]])) {
      metrics_dir <- file.path("results", "metrics", TERRITORY, "derived")
      dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

      source_stats <- df_long %>%
        arrange(source_used, year) %>%
        group_by(source_used, .drop = FALSE) %>%
        summarise(
          data = list(tibble::tibble(year = year, area_ha = area_ha)),
          year_min = safe_min_int(year),
          year_max = safe_max_int(year),
          total_ha = sum(area_ha, na.rm = TRUE),
          mean_ha = safe_mean_num(area_ha),
          .groups = "drop"
        ) %>%
        mutate(
          source_name = as.character(source_used),
          source_label = dplyr::case_when(
            !is.na(year_min) & !is.na(year_max) ~ glue("{source_name} ({year_min}-{year_max})"),
            TRUE ~ source_name
          ),
          peak_info = purrr::map(data, extreme_info, type = "max"),
          low_info  = purrr::map(data, extreme_info, type = "min"),
          peak_year = purrr::map_int(peak_info, "year"),
          peak_value = purrr::map_dbl(peak_info, "value"),
          low_year = purrr::map_int(low_info, "year"),
          low_value = purrr::map_dbl(low_info, "value"),
          peak_share = share_fraction(peak_value, total_ha),
          low_share  = share_fraction(low_value, total_ha)
        )

      overview_table <- source_stats %>%
        transmute(
          Source = source_label,
          `Surface totale degradee (ha)` = format_number_fr(total_ha),
          `Degradation moyenne (ha/an)` = format_number_fr(mean_ha),
          `Annee la plus forte / Part du total` = format_year_share_entry(peak_year, peak_value, peak_share),
          `Annee la plus faible / Part du total` = format_year_share_entry(low_year, low_value, low_share)
        )

      trend_windows <- list(
        `1995-2004 (moyenne / part %)` = c(1995L, 2004L),
        `2005-2014 (moyenne / part %)` = c(2005L, 2014L),
        `2015-2024 (moyenne / part %)` = c(2015L, 2024L)
      )

      trend_table <- purrr::pmap_dfr(
        list(
          source_label = source_stats$source_label,
          data = source_stats$data,
          total_ha = source_stats$total_ha,
          year_min = source_stats$year_min,
          year_max = source_stats$year_max
        ),
        function(source_label, data, total_ha, year_min, year_max) {
          cells <- purrr::map(trend_windows, function(window_range) {
            compute_window_entry(
              df = data,
              window_start = window_range[1],
              window_end = window_range[2],
              total_ha = total_ha,
              year_min = year_min,
              year_max = year_max
            )
          })
          tibble::tibble(
            Source = source_label,
            !!!rlang::set_names(cells, names(trend_windows))
          )
        }
      )

      overview_stub <- glue(FILENAME_OVERVIEW, territory = TERRITORY)
      trends_stub   <- glue(FILENAME_TRENDS, territory = TERRITORY)
      overview_path <- file.path(metrics_dir, glue("{overview_stub}.csv"))
      trends_path   <- file.path(metrics_dir, glue("{trends_stub}.csv"))

      readr::write_csv(overview_table, overview_path, na = "")
      readr::write_csv(trend_table, trends_path, na = "")

      message(glue("[metrics] Saved degradation overview + trends tables to {basename(metrics_dir)} for {TERRITORY}"))
    }
    present_sources <- levels(droplevels(df_long$source_used))  # should be "JRC-TMF"
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)
    message(glue("✓ Source present: {paste(present_sources, collapse=', ')}"))
    message(glue("✓ Year range: {year_min}–{year_max} (n={nrow(df_long)})"))

    # Defensive subset for plotting
    df_plot <- df_long %>% filter(!is.na(area_ha)) %>% droplevels()
    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]

    ### 3.3 Plot ----
    # ----------------------------------------------------------------------- - - -
    p <- ggplot(df_plot, aes(x = year, y = area_ha, color = source_used, linetype = source_used)) +
      geom_line(linewidth = 1.2, lineend = "round", na.rm = TRUE) +
      geom_point(size = 4, stroke = 0, na.rm = TRUE) +
      scale_color_manual(values = cols, breaks = present_sources, guide = guide_legend(title = NULL)) +
      scale_linetype_manual(values = ltys, breaks = present_sources, guide = "none") +
      axis_x_years_all(year_min, year_max) +
      # axis_y_thousands_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      axis_y_ha_auto(max(df_plot$area_ha, na.rm = TRUE)) +
      labs(
        title   = label("title_degradation_in", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_tmf(LANG)
      ) +
      theme_time_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 3.4 Export (fixed full-page size) ----
    # ----------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue(FILENAME_DEF, territory = TERRITORY, lang = LANG)
      png_path  <- file.path(OUTPUT_DIR, glue("{file_stub}.png"))

      # PNG 
      ggsave(
        filename = png_path, plot = p,
        width = FIG_WIDTH_MM, height = FIG_HEIGHT_MM, units = UNITS,
        dpi = DPI, bg = "white"
      )

      # SVG
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
