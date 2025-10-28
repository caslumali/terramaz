##%###########################################################################%##
#                                                                               #
#                         Deforestation Time Series                          ----
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
FILENAME_STUB    <- "01_{territory}_deforestation_{lang}"
FIG_WIDTH_MM     <- 431.8   # 17 in — full page width
FIG_HEIGHT_MM    <- 152.4   # 6 in  — consistent with fire plots
UNITS            <- "mm"
DPI              <- 300

## 1.2 Language & labels ----
# ------------------------------------------------------------------------- - - -
# LANGS <- c("fr")   # "pt" | "es" | "fr" | "en"
LANGS        <- c("fr", "es", "pt", "en")

LABELS <- list(
  # Titles
  title_deforestation_in = c(
    fr = "Évolution annuelle de la déforestation à {territory}",
    es = "Evolución anual de la deforestación en {territory}",
    pt = "Evolução anual do desmatamento em {territory}",
    en = "Annual evolution of deforestation in {territory}"
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

## 1.3 Palettes for sources (lines) ----
# ------------------------------------------------------------------------- - - -
source_line_colors <- c(
  `JRC-TMF` = "#FF871F",
  MapBiomas = "#1f77b4",
  INPE      = "#d62728",
  IDEAM     = "#9467bd"
)

source_line_types <- c(
  `JRC-TMF` = "solid",
  MapBiomas = "solid",
  INPE      = "solid",
  IDEAM     = "solid"
)

## 1.4 Theme & scales (self-contained) ----
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

## 2.1 Map CSV column names to canonical sources ----
# ------------------------------------------------------------------------- - - -
map_col_to_source <- function(colname) {
  z <- tolower(colname)
  if (str_detect(z, "tmf")       && str_detect(z, "area_ha")) return("JRC-TMF")
  if (str_detect(z, "mapbiomas") && str_detect(z, "area_ha")) return("MapBiomas")
  if ((str_detect(z, "prodes") || str_detect(z, "inpe")) && str_detect(z, "area_ha")) return("INPE")
  if (str_detect(z, "ideam")     && str_detect(z, "area_ha")) return("IDEAM")
  NA_character_
}

## 2.2 Expected sources per territory ----
# ------------------------------------------------------------------------- - - -
expected_sources <- function(territory) {
  switch(tolower(territory),
         "cotriguacu"    = c("JRC-TMF","MapBiomas","INPE"),
         "paragominas"   = c("JRC-TMF","MapBiomas","INPE"),
         "guaviare"      = c("JRC-TMF","MapBiomas","IDEAM"),
         "madre_de_dios" = c("JRC-TMF","MapBiomas"),
         c("JRC-TMF","MapBiomas")
  )
}

## 2.3 Trim leading zeros per source (avoid flat lines glued at zero) ----
# ------------------------------------------------------------------------- - - -
trim_leading_zeros <- function(y) {
  ix <- which(!is.na(y) & y > 0)
  if (length(ix) == 0) return(rep(NA_real_, length(y)))
  cut <- min(ix)
  out <- y
  out[seq_len(cut - 1)] <- NA_real_
  out
}

## 2.4 Readers for complemenary CSVs ----
# ------------------------------------------------------------------------- - - -
read_prodes_official <- function(path) {
  if (!file.exists(path)) return(NULL)
  # robust read with , as decimal and . as thousands separator
  loc <- readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
  df <- tryCatch(readr::read_csv2(path, show_col_types = FALSE, locale = loc),
                 error = function(e) readr::read_csv(path, show_col_types = FALSE, locale = loc))
  if (ncol(df) < 2) return(NULL)
  names(df)[1:2] <- c("year_raw", "km2_raw")
  out <- df %>%
    mutate(
      year = readr::parse_integer(year_raw, na = c("Year", "Ano")),
      km2  = readr::parse_number(km2_raw, locale = loc)
    ) %>%
    filter(!is.na(year), !is.na(km2)) %>%
    transmute(year = as.integer(year), area_ha = as.numeric(km2) * 100)
  if (nrow(out) == 0) return(NULL)
  out
}

read_mb_site <- function(path) {
  if (!file.exists(path) || length(path) == 0) return(NULL)

  # Lê o arquivo (tenta detectar decimal = "." ou ",")
  df <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE,
                    locale = readr::locale(decimal_mark = ".", grouping_mark = ",")),
    error = function(e)
      readr::read_csv(path, show_col_types = FALSE,
                      locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  )

  nms <- names(df)

  # === Caso 1: formato "wide" (como Madre de Dios) ===
  if ("Classe" %in% nms && any(stringr::str_detect(nms, "^\\d{4}$"))) {
    message(glue::glue("🌎 Detected wide format in {basename(path)} (keeping only primary forest loss)"))
    
    out <- df %>%
      # mantém apenas a linha de floresta primária
      dplyr::filter(stringr::str_detect(Classe, "(?i)vegetaci[oó]n primaria")) %>%
      tidyr::pivot_longer(
        cols = tidyselect::matches("^\\d{4}$"),
        names_to = "year",
        values_to = "area_ha"
      ) %>%
      dplyr::mutate(
        year = as.integer(year),
        area_ha = as.numeric(area_ha)
      ) %>%
      dplyr::select(year, area_ha) %>%
      dplyr::filter(!is.na(year), !is.na(area_ha))
    
    return(out)
  }

  # === Caso 2: formato "long" (Brasil, Colômbia etc.) ===
  loc <- readr::locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE, locale = loc))
  nms <- names(df)

  year_col <- nms[stringr::str_detect(tolower(nms), "^(ano|year)$")]
  if (length(year_col) == 0) return(NULL)

  value_cols <- setdiff(nms, year_col)
  primary_col <- value_cols[stringr::str_detect(tolower(value_cols), "prim(aria)?|primary")][1]
  if (is.na(primary_col)) {
    warning(glue::glue("⚠ Nenhuma coluna de floresta primária encontrada em {basename(path)}"))
    return(NULL)
  }

  out <- df %>%
    dplyr::rename(year = all_of(year_col)) %>%
    dplyr::mutate(across(all_of(value_cols),
                         ~ dplyr::na_if(stringr::str_trim(as.character(.x)), "-"))) %>%
    dplyr::mutate(across(all_of(value_cols),
                         ~ readr::parse_number(.x, locale = loc))) %>%
    dplyr::transmute(year = as.integer(year),
                     area_ha = as.numeric(.data[[primary_col]])) %>%
    dplyr::filter(!is.na(year), !is.na(area_ha))
  
  return(out)
}


## 2.5 Caption dynamic for territories ----
# ------------------------------------------------------------------------- - - -
caption_for <- function(df_in, lang = "en") {
  if (is.null(df_in) || nrow(df_in) == 0) return("")
  cov <- df_in %>%
    filter(!is.na(year), !is.na(area_ha)) %>%
    group_by(source_used) %>%
    summarise(miny = min(year), maxy = max(year), .groups = "drop") %>%
    arrange(factor(source_used, levels = c("JRC-TMF","MapBiomas","INPE","IDEAM")))
  cov_str <- paste0(cov$source_used, ": ", cov$miny, "-", cov$maxy, collapse = " | ")
  prefix <- switch(lang, "fr"="Sources — ", "pt"="Fontes — ", "es"="Fuentes — ", "en"="Sources — ", "Sources — ")
  paste0(prefix, cov_str)
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

    MAIN_DIR  <- file.path("results/metrics", TERRITORY)
    COMP_DIR <- file.path("results/metrics", "complementary")
    OUTPUT_DIR <- file.path("results/indicators",   TERRITORY, glue(TERRITORY, '_', LANG))
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

    ### 3.1 Load main deforestation CSV ----
    # ----------------------------------------------------------------------- - - -
    main_csv <- list.files(
      MAIN_DIR,
      pattern = glue("^{TERRITORY}_deforestation_.*\\.csv$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(main_csv) == 0) {
      message(glue("⚠ CSV not found for {TERRITORY} in {MAIN_DIR} — skipping."))
      next
    }
    message(glue("📊 Loading: {basename(main_csv[1])}"))
    df <- suppressMessages(readr::read_csv(main_csv[1], show_col_types = FALSE))

    # robust year column detection
    nms_lc <- tolower(names(df))
    year_candidates <- c("year","ano","yr","year_int")
    year_idx <- match(year_candidates, nms_lc, nomatch = 0)
    if (!any(year_idx > 0)) {
      message(glue("⚠ Year column not found. Available: {paste(names(df), collapse=', ')}"))
      next
    }
    year_col <- names(df)[year_idx[year_idx > 0][1]]
    message(glue("✓ Using year column: '{year_col}'"))

    # *_area_ha columns
    area_cols <- names(df)[str_detect(tolower(names(df)), "area_ha$")]
    if (length(area_cols) == 0) {
      message("⚠ No '*_area_ha' columns found — skipping.")
      next
    }
    message(glue("✓ Area columns: {paste(area_cols, collapse=', ')}"))

    long_main <- df %>%
      select(all_of(c(year_col, area_cols))) %>%
      pivot_longer(cols = all_of(area_cols), names_to = "var", values_to = "area_ha") %>%
      mutate(
        source_used = vapply(var, map_col_to_source, character(1)),
        year        = as.integer(.data[[year_col]]),
        area_ha     = suppressWarnings(as.numeric(area_ha))
      ) %>%
      filter(!is.na(source_used), !is.na(year), !is.na(area_ha)) %>%
      select(year, source_used, area_ha) %>%
      arrange(year, source_used)

    ### 3.2 INPE/PRODES override (Cotriguacu & Paragominas) ----
    # ----------------------------------------------------------------------- - - -
    is_cp <- tolower(TERRITORY) %in% c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")
    
    if (is_cp) {
      prodes_file <- file.path(COMP_DIR, glue("{TERRITORY}_deforestation_prodes_2001_2022.csv"))
      prodes_off  <- read_prodes_official(prodes_file)
      # Detecta faixa real de anos no CSV oficial
      if (!is.null(prodes_off)) {
        range_off <- range(prodes_off$year, na.rm = TRUE)

        prodes_off <- prodes_off %>%
          mutate(source_used = "INPE")

        # Remove só o intervalo realmente coberto pelo CSV oficial
        long_main <- long_main %>%
          filter(!(source_used == "INPE" & dplyr::between(year, range_off[1], range_off[2]))) %>%
          bind_rows(prodes_off)

        message(glue("🔗 INPE/PRODES: override {range_off[1]}–{range_off[2]} com {basename(prodes_file)} (km²→ha)."))
      } else {
        message("ℹ INPE/PRODES: arquivo não encontrado/parsable — mantendo série do GEE.")
      }
    }
    
  ### 3.3 MapBiomas (site oficial) override (Cotriguacu & Paragominas) ----
  # ----------------------------------------------------------------------- - - -
  if (is_cp) {
    mb_site_file <- list.files(
      path = COMP_DIR,
      pattern = glue("^{TERRITORY}_deforestation_mb_site_.*\\.csv$"),
      full.names = TRUE
    )
    mb_site <- read_mb_site(mb_site_file)
    
    if (!is.null(mb_site)) {
      # Define intervalo de substituição
      if (tolower(TERRITORY) == "madre_de_dios") {
        year_replace_min <- 2001L
        year_replace_max <- 2024L
      } else {
        year_replace_min <- 1985L
        year_replace_max <- 2024L
      }
      
      mb_site <- mb_site %>%
        filter(dplyr::between(year, year_replace_min, year_replace_max)) %>%
        mutate(source_used = "MapBiomas")

      # Remove apenas o intervalo coberto pelo CSV oficial
      long_main <- long_main %>%
        filter(!(source_used == "MapBiomas" &
                dplyr::between(year, year_replace_min, year_replace_max))) %>%
        bind_rows(mb_site)

      message(glue("🔗 MapBiomas (site): override {year_replace_min}-{year_replace_max} com {basename(mb_site_file)}."))
    } else {
      message("ℹ MapBiomas (site): arquivo não encontrado/parsable — mantendo série do GEE.")
    }
  }

  if (is_cp && !is.null(mb_site)) {
    message("l: ",
      mb_site %>% filter(year >= 2020) %>%
        arrange(year) %>%
        mutate(area_ha = round(area_ha, 2)) %>%
        transmute(pair = paste0(year, ":", area_ha)) %>%
        pull(pair) %>% paste(collapse = ", ")
    )
  }
    
    ### 3.4 IDEAM integration (Guaviare only) ----
    # ----------------------------------------------------------------------- - - -
    if (tolower(TERRITORY) == "guaviare") {
      ideam_csv <- list.files(
        COMP_DIR,
        pattern = glue("^{TERRITORY}.*deforestation_ideam.*\\.csv$"),
        full.names = TRUE, ignore.case = TRUE
      )
      if (length(ideam_csv) > 0) {
        message(glue("🔗 Integrating IDEAM: {basename(ideam_csv[1])}"))
        ideam <- suppressMessages(readr::read_csv(ideam_csv[1], show_col_types = FALSE))
        if (all(c("year", "def_year_ha") %in% names(ideam))) {
          ideam_long <- ideam %>%
            transmute(
              year        = as.integer(.data[["year"]]),
              source_used = "IDEAM",
              area_ha     = as.numeric(.data[["def_year_ha"]])
            ) %>%
            filter(!is.na(year), !is.na(area_ha), year >= 2012)
          long_main <- bind_rows(long_main, ideam_long)
          message("✓ IDEAM merged.")
        } else {
          message("⚠ IDEAM: required columns not found — skipping merge.")
        }
      } else {
        message("ℹ IDEAM CSV not found — proceeding without it.")
      }
    }

    ### 3.5 Filter, type-cast, and validate ----
    # ----------------------------------------------------------------------- - - -
    # Coverage years per source 
    if (tolower(TERRITORY) %in% c("cotriguacu","paragominas")) {
      # INPE from 2001
      YEAR_MIN <- c(`JRC-TMF` = 1990L, MapBiomas = 1990L, INPE = 2001L)
      YEAR_MAX <- c(`JRC-TMF` = 2024L, MapBiomas = 2024L, INPE = 2024L)
    } else if (tolower(TERRITORY) == "guaviare") {
      # IDEAM from 2012
      YEAR_MIN <- c(`JRC-TMF` = 1990L, MapBiomas = 1990L, IDEAM = 2012L)
      YEAR_MAX <- c(`JRC-TMF` = 2024L, MapBiomas = 2024L, IDEAM = 2023L)
    } else if (tolower(TERRITORY) == "madre_de_dios") {
      # No INPE or IDEAM
      YEAR_MIN <- c(`JRC-TMF` = 1990L, MapBiomas = 1990L)
      YEAR_MAX <- c(`JRC-TMF` = 2024L, MapBiomas = 2024L)
    } else {
      message(glue("⚠ Unknown territory '{TERRITORY}' for year coverage — skipping."))
      next
    }

    message(glue("✓ Year coverage per source: {paste(names(YEAR_MIN), YEAR_MIN, sep='=', collapse=', ')}"))
   
    expected <- expected_sources(TERRITORY)

    df_long <- long_main %>%
      filter(source_used %in% expected) %>%
      mutate(
        source_used = factor(source_used, levels = expected),
        year        = as.integer(year),
        area_ha     = pmax(suppressWarnings(as.numeric(area_ha)), 0)
      ) %>%
      # aplica limites por fonte (evita MapBiomas-2024 indevido em ROIs sem site)
      filter(
        year >= YEAR_MIN[as.character(source_used)],
        year <= YEAR_MAX[as.character(source_used)]
      ) %>%
      arrange(year, source_used) %>%
      group_by(source_used) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(area_ha = trim_leading_zeros(area_ha)) %>%
      ungroup()

    if (nrow(df_long) == 0) {
      message(glue("⚠ No rows after filtering expected sources: {paste(expected, collapse=', ')}"))
      next
    }

    # Build palette after final filtering
    present_sources <- levels(droplevels(df_long$source_used))
    cols <- source_line_colors[present_sources]
    ltys <- source_line_types[present_sources]
    year_min <- min(df_long$year, na.rm = TRUE)
    year_max <- max(df_long$year, na.rm = TRUE)
    message(glue("✓ Sources present: {paste(present_sources, collapse=', ')}"))
    message(glue("✓ Year range: {year_min}-{year_max} (n={nrow(df_long)})"))

    # Defensive subset for plotting
    df_plot <- df_long %>% filter(!is.na(area_ha)) %>% droplevels()
    if (nrow(df_plot) == 0) {
      message(glue("⚠ No valid data after processing for {TERRITORY}"))
      next
    }

    territory_title <- TERRITORY_LABELS[[TERRITORY]]
    present_sources <- levels(df_plot$source_used)

    ### 3.6 Plot ----
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
        title   = label("title_deforestation_in", territory = territory_title),
        x       = label("x_year"),
        y       = label("y_area_ha"),
        caption = caption_for(df_plot, LANG)
      ) +
      theme_time_series()

    print(p)
    message(glue("✓ Plot generated for {TERRITORY}"))

    ### 3.7 Export (fixed full-page size) ----
    # ----------------------------------------------------------------------- - - -
    if (WRITE_PLOT) {
      file_stub <- glue(FILENAME_STUB, territory = TERRITORY, lang = LANG)
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
