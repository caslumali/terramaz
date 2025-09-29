##%###########################################################################%##
#                                                                               #
#                           Disturbance drivers histogram                    ----
#                                                                               #
##%###########################################################################%##

## I Required libraries ----
# ------------------------------------------------------------------------- - - -
library(tidyverse)
library(janitor)
library(ggridges)
library(glue)
library(ggplot2)
library(magick)
library(svglite)
library(fs)


## I.II Filtering mode: ----
# ------------------------------------------------------------------------- - - -
FILTER_TAG <- "mask2020" 

# Split date in filter tag to extract year
MASK_YEAR <- str_extract(FILTER_TAG, "\\d{4}")  # e.g., "2020"

# Define suffix pattern for parsing filenames (used in `str_remove`)
FILTER_SUFFIX <- glue("_{FILTER_TAG}")

## I.III Paths and environment ----
# ------------------------------------------------------------------------- - - -
INPUT_DIR    <- glue("results/metrics/disturbance_histograms_{FILTER_TAG}")
OUTPUT_DIR <- glue("results/plots/supplementary")
dir_create(OUTPUT_DIR)

WRITE_PLOT  <- TRUE
VEG_TYPE    <- 'veg3'

## I.IV  Language setting for plot labels (EN = English, FR = French)        ----
# ------------------------------------------------------------------------- - - - 
# Language selector: "en" or "fr"
LANG <- "en"

# Bilingual label dictionary
LABELS <- list(
  
  # Axes
  y_millions        = c(en = "Pixel count (millions)", fr = "Nombre de pixels (en millions)"),
  x_year            = c(en = "Year", fr = "Année"),
  x_events          = c(en = "Number of disturbance events", fr = "Nombre d’événements de perturbation"),
  x_sequence        = c(en = "Disturbance sequence", fr = "Séquence de perturbation"),
  x_sequence_short  = c(en = "Disturbance Sequence (F = Fire, L = Logging)", fr = "Séquence de pertubation (F = Feu, L = Exploitation)"),
  y_pixel_delta     = c(en = "Years between disturbance events", fr = "Années entre les perturbations"),
  
  # Driver names
  fire_only         = c(en = "Fire only",    fr = "Feu uniquement"),
  logging_only      = c(en = "Logging only", fr = "Exploitation uniquement"),
  mixed             = c(en = "Mixed",        fr = "Mixte"),
  undisturbed       = c(en = "Undisturbed",  fr = "Non perturbée"),
  
  # Mixed 2-pattern disturbance sequences
  fire_logging_seq   = c(en = "Fire - Logging", fr = "Feu - Exploitation"),
  logging_fire_seq   = c(en = "Logging - Fire", fr = "Exploitation - Feu"),
  
  # Mixed 3-pattern disturbance sequences
  pattern3_F_L_F = c(en = "F-L-F", fr = "F-E-F"),
  pattern3_F_F_L = c(en = "F-F-L", fr = "F-F-E"),
  pattern3_F_L_L = c(en = "F-L-L", fr = "F-E-E"),
  pattern3_L_F_L = c(en = "L-F-L", fr = "E-F-E"),
  pattern3_L_L_F = c(en = "L-L-F", fr = "E-E-F"),
  pattern3_L_F_F = c(en = "L-F-F", fr = "E-F-F"),
  
  # Temporal labels
  first = c(en = "First", fr = "Première"),
  last  = c(en = "Last",  fr = "Dernière"),
  
  # Labels for X-axis categories (boxplot labels)
  delta_fire2x = c(en = "Fire (2×)",     fr = "Feu (2×)"),
  delta_log2x  = c(en = "Logging (2×)",  fr = "Exploitation (2×)"),
  delta_fire3x = c(en = "Fire (3×)",     fr = "Feu (3×)"),
  delta_log3x  = c(en = "Logging (3×)",  fr = "Exploitation (3×)"),
  delta_mix2x  = c(en = "Mixed (2×)",    fr = "Mixte (2×)"),
  delta_mix3x  = c(en = "Mixed (3×)",    fr = "Mixte (3×)")
)

# Helper: retrieve label by key
label <- function(key, ...) {
  template <- LABELS[[key]][[LANG]]
  glue::glue(template, .envir = rlang::env(...))
}

# Helper: translate driver names dynamically
translate_driver <- function(driver_name) {
  # Normalize input (remove extra spaces, normalize dashes)
  driver_clean <- driver_name %>%
    str_trim() %>%
    str_replace_all("[–—−‑‒]", "-") %>%   # normalize all types of dashes
    str_replace_all("\\s*-\\s*", "-")    # trim spaces around dashes
  key <- tolower(gsub(" ", "_", driver_clean)) # e.g., "Fire-Logging" -> "fire_logging"
  
  # Case 1: Direct key match (e.g., "Fire only", "Mixed")
  if (key %in% names(LABELS) && !is.null(LABELS[[key]][[LANG]])) {
    return(LABELS[[key]][[LANG]])
  }
  
  # Case 2: Three-event mixed patterns (e.g., "F-L-F")
  if (grepl("^[FL](-[FL]){2}$", driver_clean)) {
    key3 <- paste0("pattern3_", gsub("-", "_", driver_clean))
    if (key3 %in% names(LABELS) && !is.null(LABELS[[key3]][[LANG]])) {
      return(LABELS[[key3]][[LANG]])
    }
  }
  
  # Case 3: Two-event mixed patterns (e.g., "Fire-Logging")
  if (driver_clean %in% c("Fire-Logging", "Logging-Fire")) {
    key2 <- if (driver_clean == "Fire-Logging") "fire_logging_seq" else "logging_fire_seq"
    if (key2 %in% names(LABELS) && !is.null(LABELS[[key2]][[LANG]])) {
      return(LABELS[[key2]][[LANG]])
    }
  }
  
  # Fallback: return original string (in case of missing label)
  driver_name
}


## I.V Load all files ----
# ------------------------------------------------------------------------- - - - 
all_files <- dir(INPUT_DIR, pattern = "\\.csv$", full.names = TRUE)

# Select only histograms and count CSVs for current vegetation type
histo_files <- all_files[grepl(glue("^{VEG_TYPE}_hist_"), basename(all_files))]
count_files <- all_files[grepl(glue("^{VEG_TYPE}_count_"), basename(all_files))]
delta_files <- all_files[grepl(glue("^{VEG_TYPE}_px_delta_"), basename(all_files))]

## I.VI Colors for disturbance type ----
# ------------------------------------------------------------------------- - - - 
drivers_fill_colors <- c("Fire only" = "#e07b7b",  # body color
                         "Logging only" = "#e0c46e",
                         "Mixed" = "#8c85d1")

drivers_border_colors <- c("Fire only" = "#b24242",  # border color
                           "Logging only" = "#b69c42",
                           "Mixed" = "#5f5aa7")


## I.VII Colors for 2-pattern mixed disturbances
# ------------------------------------------------------------------------- - - - 
pattern2_fill_colors <- setNames(
  c("#e07b7b", "#e0c46e"),
  c(label("fire_logging_seq"), label("logging_fire_seq"))
)

pattern2_border_colors <- setNames(
  c("#b24242", "#b69c42"),
  c(label("fire_logging_seq"), label("logging_fire_seq"))
)


## I.VIII Colors for 3-pattern mixed disturbances
# ------------------------------------------------------------------------- - - - 
pattern3_fill_colors <- c(
  "F-L-F" = "#e07b7b",  # more fire
  "F-F-L" = "#ea9d9d",  # more fire
  "F-L-L" = "#efcfcf",  # more logging
  "L-F-L" = "#e6cd8f",  # more logging
  "L-L-F" = "#e0c46e",  # more logging
  "L-F-F" = "#f4e7b0"   # more fire
)

pattern3_border_colors <- c(
  "F-L-F" = "#b24242",
  "F-F-L" = "#b76060",
  "F-L-L" = "#d6abab",
  "L-F-L" = "#b69c42",
  "L-L-F" = "#b69c42",
  "L-F-F" = "#c9b773"
)


## I.VIII Color for edge ----
# ------------------------------------------------------------------------- - - - 
edge_fill_colors <- c("Undisturbed" = "#7bcc8f",
                      "Fire only" = "#e07b7b",  # body color
                      "Logging only" = "#e0c46e",
                      "Mixed" = "#8c85d1")

edge_border_colors <- c("Undisturbed" = "#46985e",
                        "Fire only" = "#b24242",  # border color
                        "Logging only" = "#b69c42",
                        "Mixed" = "#5f5aa7")


## I.IX Colors for pixel delta ----
# ------------------------------------------------------------------------- - - - 
# Define colors
fill_colors <- c("fire2x" = "#e07b7b",
                 "log2x"  = "#e0c46e",
                 "fire3x" = "#e07b7b",
                 "log3x"  = "#e0c46e",
                 "mix2x_ALL"  = "#8c85d1",
                 "mix3x_ALL"  = "#8c85d1")


border_colors <- c("fire2x" = "#b24242",
                   "log2x"  = "#b69c42",
                   "fire3x" = "#b24242",
                   "log3x"  = "#b69c42",
                   "mix2x_ALL"  = "#5f5aa7",
                   "mix3x_ALL"  = "#5f5aa7")

## I.X Colors for fire statistics ----
# ------------------------------------------------------------------------- - - -
fire_stats_fill_colors <- c("MapBiomas only" = "#2E86AB",    # Blue (neutral)
                            "GLAD only" = "#A23B72",        # Dark pink (neutral)  
                            "Both datasets" = "#F18F01")     # Orange (overlap)

# fire_stats_border_colors <- c("MapBiomas only" = "#b24242",
#                              "GLAD only" = "#4a6bb8", 
#                              "Both datasets" = "#5f5aa7")

# Translated labels for fire stats
fire_stats_labels <- list(
  mapbiomas_only = c(en = "MapBiomas only", fr = "MapBiomas uniquement"),
  glad_only = c(en = "GLAD only", fr = "GLAD uniquement"), 
  both_datasets = c(en = "Both datasets", fr = "Les deux datasets")
)

## I.XI Normalize pattern labels (for histogram and delta metrics) ----
# ------------------------------------------------------------------------- - - -
normalize_mixed_label <- function(label_vec) {
  
  label_vec %>%
    # Step 1: clean spacing and hyphen variants
    str_trim() %>%
    str_replace_all("[–—−‑‒]", "-") %>%  # Replace various hyphens with "-"
    str_replace_all("_", "") %>%        # Remove underscores (from metric names)
    str_replace_all("-", " - ") %>%     # Add spaces around dashes
    str_squish() %>%
    
    # Step 2: if the input is a delta metric (e.g. "mix2x_FLF"), extract the pattern
    str_replace("^mix[23]x_", "") %>%
    str_split_fixed("", n = Inf) %>%  # split characters
    apply(1, function(chars) {
      # Keep only valid pattern characters
      chars <- chars[chars %in% c("F", "L")]
      # Build normalized label (e.g. "F - L - F" or "F - L")
      paste(chars, collapse = " - ")
    }) %>%
    
    # Step 3: harmonize final formatting to match color map keys
    str_squish() %>%
    recode(
      "Fire - Logging"   = "Fire - Logging",
      "Logging - Fire"   = "Logging - Fire",
      "F - L" = "Fire - Logging",
      "L - F" = "Logging - Fire",
      "F - L - F" = "F-L-F", "F - F - L" = "F-F-L", "F - L - L" = "F-L-L",
      "L - F - L" = "L-F-L", "L - L - F" = "L-L-F", "L - F - F" = "L-F-F"
    )
}

##%###########################################################################%##
#                                                                               #
#                          II Utility Functions                              ----
#                                                                               #
##%###########################################################################%##
## II.I Functions to format labels ----
# ------------------------------------------------------------------------- - - -
# Format large numbers as "1.23 M"
format_millions <- function(x, digits = 2) {
  glue("{format(round(x, digits), nsmall = digits)} M")
}

# Format percentages as "12.34%" (or "< 0.01%" if very small)
format_percent <- function(x, digits = 2, minimum = 0.01) {
  percent_val <- x * 100
  label <- ifelse(percent_val < minimum,
                  glue("{minimum}%"),
                  glue("{round(percent_val, digits)}%"))
  return(label)
}

# Format number with language-specific thousand separators
format_number <- function(x) {
  big_mark <- ifelse(LANG == "fr", " ", ",")
  format(x, big.mark = big_mark, scientific = FALSE)
}

## II.II Function to standardize .png outputs ----
# ------------------------------------------------------------------------- - - -
plot_size <- function(type) {
  switch(type,
         "compact"  = list(width = 8.5, height = 5.3),
         "temporal" = list(width = 12, height = 6),
         stop("Unknown type")
  )
}

## II.III Function to standardize y-axis formatting ----
# ------------------------------------------------------------------------- - - -
# Standard y-axis formatting
scale_y_millions <- function() {
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = NULL),
    expand = expansion(mult = c(0, 0.1))
  )
}

## II.IV Function to standardize theme for compact barplots (categorical X) ----
# ------------------------------------------------------------------------- - - -
theme_histogram <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(size = 18, margin = margin(r = 10)),
      axis.line = element_line(color = "grey30", linewidth = 0.5),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

## II.V Function to standardize theme for temporal barplots (years in X) ----
# ------------------------------------------------------------------------- - - -
theme_temporal <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0, face = "bold", size = 14, margin = margin(b = 10)),  # Left aligned
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  # All years visible
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(size = 18, margin = margin(r = 10)),
      axis.line = element_line(color = "grey30", linewidth = 0.5),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "right",
      legend.title = element_blank(),
      legend.text  = element_text(size = 18),
      legend.key.size = unit(1, "lines"),
      legend.margin = margin(t = -5, b = 5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

##%###########################################################################%##
#                                                                               #
#                  III. Load .csv counts and show total pixels              ----
#                                                                               #
##%###########################################################################%##
# Load pixel counts separately (e.g., forest mask)
# ------------------------------------------------------------------------- - - -
pixel_counts <- map_dfr(count_files, function(path) {
  name <- path_file(path) %>%
    str_remove(glue("^{VEG_TYPE}_count_")) %>%
    str_remove(glue("{FILTER_SUFFIX}")) %>%
    str_remove("\\.csv$")
  
  df <- read_csv(path, show_col_types = FALSE) %>% clean_names()
  
  # Select first numeric column (excluding metadata)
  value_col <- df %>%
    select(where(is.numeric), -contains("index"), -contains("geo")) %>%
    names() %>%
    first()
  
  tibble(name = name, count = df[[value_col]][1])
})

# Show total pixel for each forest mask
forest_mask       <- pixel_counts %>% filter(name == glue("forest_mask_{MASK_YEAR}")) %>% pull(count)
edge_baseline_1990 <- pixel_counts %>% filter(name == "edge_baseline_1990") %>% pull(count)

glue("✓ Forest mask total pixels:       {format(forest_mask, big.mark = ',')}")
glue("✓ Edge baseline (1990) pixels:    {format(edge_baseline_1990, big.mark = ',')}")

##%###########################################################################%##
#                                                                               #
#                        IV. Load and Clean Histogram Tables                 ----
#                                                                               #
##%###########################################################################%##

# Read and structure one histogram CSV 
read_histogram_csv <- function(path) {
  df <- read_csv(path, show_col_types = FALSE) %>%
    clean_names()
  
  # Get metric name from filename
  metric_name <- path_file(path) %>%
    str_remove(glue("^{VEG_TYPE}_hist_")) %>%
    str_remove(glue("_{FILTER_TAG}\\.csv$")) %>%
    str_remove("\\.csv$") %>%
    str_replace("mixed2", "mixed_2") %>%
    str_replace("mixed3", "mixed_3")
  
  
  value_col <- names(df)[str_detect(names(df), "value")][1]
  count_col <- names(df)[str_detect(names(df), "count")][1]
  label_col <- if ("label" %in% names(df)) "label" else NA
  
  df_out <- df %>%
    transmute(
      metric = metric_name,
      value  = .data[[value_col]],
      count  = .data[[count_col]],
      label  = if (!is.na(label_col)) .data[[label_col]] else NA_character_
    )
  
  return(df_out)
}

# Read and stack all histogram tables
histograms_tbl <- map_dfr(histo_files, read_histogram_csv)

# Preview summary info 
cat(glue("✓ Total histogram rows: {nrow(histograms_tbl)}\n"))
cat(glue("✓ Metrics loaded: {str_wrap(paste(unique(histograms_tbl$metric), collapse = ', '), width = 80)}\n"))


##%###########################################################################%##
#                                                                               #
#                        V. Load and clean pixel delta values                ----
#                                                                               #
##%###########################################################################%##
# Filter and group delta files by base metric (e.g., "log2x" from "log2x_part1", "log2x_NW", etc.)
delta_files_grouped <- delta_files %>%
  tibble(path = .) %>%
  mutate(
    base_metric = path %>%
      path_file() %>%
      str_remove(glue("^{VEG_TYPE}_px_delta_")) %>%
      str_remove(glue("(_part[1-4])?_{FILTER_TAG}(_(NW|NE|SW|SE))?\\.csv$"))) %>%
  group_by(base_metric) %>%
  summarise(paths = list(path), .groups = "drop")


# Function to read a group of split delta files for a given metric
read_delta_group <- function(metric_name, path_list) {
  map_dfr(path_list, function(path) {
    df <- read_csv(path, show_col_types = FALSE) %>% clean_names()
    
    # Find numeric column (excluding common metadata)
    value_col <- names(df)[!(names(df) %in% c("system_index", "geo"))][1]
    
    if (is.null(value_col) || is.na(value_col)) {
      warning(glue("Could not find value column in file: {path}"))
      return(NULL)
    }
    
    tibble(
      metric = metric_name,
      value  = df[[value_col]]
    )
  })
}


# Load and combine all delta tables
delta_tbl <- map2_dfr(delta_files_grouped$base_metric,
                      delta_files_grouped$paths,
                      read_delta_group)

# Preview summary info
cat(glue("✓ Total pixel delta rows: {format(nrow(delta_tbl), big.mark = ',')} rows\n"))
cat(glue("✓ Metrics loaded: {str_wrap(paste(unique(delta_tbl$metric), collapse = ', '), width = 80)}\n"))

##%###########################################################################%##
#                                                                               #
#                    1. Fire Statistics Temporal Analysis                   ----
#                                                                               #
##%###########################################################################%##

## 1.1 Calculate fire detection overlap metrics ----
# ------------------------------------------------------------------------- - - -
calculate_fire_overlap_metrics <- function(data) {
  
  # Filter fire stats data and convert to hectares (Landsat pixels = 900 m²)
  df <- data %>%
    filter(str_detect(metric, "^fireStats_\\d{4}$")) %>%
    mutate(
      year = as.numeric(str_extract(metric, "\\d{4}")),
      count_ha = count * 0.09,  # Convert pixels to hectares (900 m² = 0.09 ha)
      fire_type = case_when(
        value == 1 ~ "MapBiomas only",
        value == 2 ~ "GLAD only", 
        value == 3 ~ "Both datasets",
        TRUE ~ "No fire"
      )
    ) %>%
    filter(fire_type != "No fire")
  
  # Calculate yearly totals and percentages
  yearly_stats <- df %>%
    group_by(year) %>%
    summarise(
      mapbiomas_only = sum(count_ha[fire_type == "MapBiomas only"], na.rm = TRUE),
      glad_only = sum(count_ha[fire_type == "GLAD only"], na.rm = TRUE),
      both_datasets = sum(count_ha[fire_type == "Both datasets"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_fire = mapbiomas_only + glad_only + both_datasets,
      pct_mapbiomas = mapbiomas_only / total_fire * 100,
      pct_glad = glad_only / total_fire * 100, 
      pct_both = both_datasets / total_fire * 100
    )
  
  # Calculate overall metrics (2001-2019, when both datasets available)
  overall_stats <- yearly_stats %>%
    filter(year >= 2001) %>%
    summarise(
      total_mapbiomas = sum(mapbiomas_only, na.rm = TRUE),
      total_glad = sum(glad_only, na.rm = TRUE),
      total_both = sum(both_datasets, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      total_fire = total_mapbiomas + total_glad + total_both,
      pct_additional_glad = total_glad / (total_mapbiomas + total_both) * 100,
      pct_additional_mapbiomas = total_mapbiomas / (total_glad + total_both) * 100,
      pct_overlap = total_both / total_fire * 100
    )
  
  return(list(yearly = yearly_stats, overall = overall_stats))
}

## 1.2 Plot fire detection temporal comparison ----
# ------------------------------------------------------------------------- - - -
plot_fire_stats_temporal <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  # Prepare data for plotting
  df <- data %>%
    filter(str_detect(metric, "^fireStats_\\d{4}$")) %>%
    mutate(
      year = as.numeric(str_extract(metric, "\\d{4}")),
      count_ha = count * 0.09,  # Convert to hectares
      fire_type = case_when(
        value == 1 ~ fire_stats_labels$mapbiomas_only[[LANG]],
        value == 2 ~ fire_stats_labels$glad_only[[LANG]], 
        value == 3 ~ fire_stats_labels$both_datasets[[LANG]],
        TRUE ~ "No fire"
      )
    ) %>%
    filter(fire_type != "No fire") %>%
    mutate(
      fire_type = factor(fire_type, levels = c(
        fire_stats_labels$mapbiomas_only[[LANG]],
        fire_stats_labels$glad_only[[LANG]],
        fire_stats_labels$both_datasets[[LANG]]
      )),
      count_thousand_ha = count_ha / 1000  # Convert to thousands of hectares
    )
  
  # Create translated color palettes
  translated_fill_colors <- setNames(
    fire_stats_fill_colors[c("MapBiomas only", "GLAD only", "Both datasets")],
    c(fire_stats_labels$mapbiomas_only[[LANG]], 
      fire_stats_labels$glad_only[[LANG]], 
      fire_stats_labels$both_datasets[[LANG]])
  )
  
  # Calculate dynamic width and y-axis max for annotation placement
  n_years <- length(unique(df$year))
  n_categories <- 3
  bar_width <- 0.75
  padding <- 3
  width_dynamic <- round(n_years * n_categories * bar_width / 6 + padding, 1)
  y_max <- max(df$count_thousand_ha, na.rm = TRUE)
  
  # Create plot with grouped bars
  p <- ggplot(df, aes(x = year, y = count_thousand_ha, fill = fire_type)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8, alpha = 0.9, color = NA) +
    # Add vertical dashed line at 2001 (only to half height)
    annotate("segment", x = 2001, xend = 2001, y = 0, yend = y_max * 0.5,
             linetype = "dashed", color = "grey50", linewidth = 0.5) +
    # Add annotation for GLAD availability
    annotate("text", x = 2001.5, y = y_max * 0.5, 
             label = "GLAD fire-loss data\navailable from 2001", size = 5, hjust = 0,
             color = "grey40", fontface = "italic") +
    scale_fill_manual(values = translated_fill_colors, name = NULL) +
    scale_x_continuous(
      breaks = seq(1990, 2019, by = 1),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = NULL),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = label("x_year"),
      y = "Fire-related degradation (10³ ha)"
    ) +
    guides(
      fill = guide_legend(keywidth = 2, keyheight = 1)
    ) +
    theme_temporal()
  
  if (WRITE_PLOT) {
    # PNG
    ggsave(file.path(output_dir, glue("01_fire_stats_temporal.png")),
           p, width = width_dynamic, height = 6, dpi = 300)
    message(glue("✓ Saved plot: {veg_type}_01_fire_stats_temporal.png (width = {width_dynamic})"))
    
    ggsave(file.path(output_dir, glue("01_fire_stats_temporal.svg")),
           p, device = svglite, width = width_dynamic, height = 6)
    message(glue("✓ Saved plot: {veg_type}_01_fire_stats_temporal.svg (width = {width_dynamic})"))
  }
  
  return(p)
}



## 1.3 Export fire statistics for manuscript text ----
# ------------------------------------------------------------------------- - - -
export_fire_metrics_for_text <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  metrics <- calculate_fire_overlap_metrics(data)
  
  # Create simple CSV with key metrics for easy text integration
  text_metrics <- tibble(
    metric_name = c(
      "additional_detection_glad_pct",
      "additional_detection_mapbiomas_pct", 
      "overlap_both_datasets_pct",
      "total_burned_area_thousand_ha"
    ),
    value = c(
      round(metrics$overall$pct_additional_glad, 1),
      round(metrics$overall$pct_additional_mapbiomas, 1),
      round(metrics$overall$pct_overlap, 1),
      round(metrics$overall$total_fire / 1000, 0)
    ),
    description = c(
      "Additional fire detection by GLAD compared to MapBiomas alone (%)",
      "Additional fire detection by MapBiomas compared to GLAD alone (%)",
      "Overlap between both datasets (%)", 
      "Total burned area 2001-2019 (thousand hectares)"
    )
  )
  
  if (save) {
    write_csv(text_metrics, file.path(output_dir, glue("{veg_type}_fire_metrics_for_text.csv")))
    message(glue("✓ Saved metrics: {veg_type}_fire_metrics_for_text.csv"))
  }
  
  return(text_metrics)
}

## 1.4 Run fire statistics analysis ----
# ------------------------------------------------------------------------- - - -
# Generate plot
plot_fire_stats_temporal(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

# Export metrics for text
text_metrics <- export_fire_metrics_for_text(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

# Print key metrics for immediate use
cat("\n", rep("=", 60), "\n", sep = "")
cat("FIRE DETECTION METRICS:\n")
cat(rep("=", 60), "\n", sep = "")
text_metrics %>%
  mutate(display = glue("{metric_name}: {value}")) %>%
  pull(display) %>%
  walk(cat, "\n")
cat(rep("=", 60), "\n", sep = "")

##%###########################################################################%##
#                                                                               #
#                        2. Total Edge Type Barplot                          ----
#                                                                               #
##%###########################################################################%##
plot_edge_type_total <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  edge_levels <- c("Undisturbed", "Fire only", "Logging only", "Mixed")
  translated_levels <- setNames(
    c(label("undisturbed"), label("fire_only"), label("logging_only"), label("mixed")),
    tolower(gsub(" ", "_", edge_levels))
  )
  
  df <- data %>%
    filter(
      metric %in% c("temporal_edge_last_burned",
                    "temporal_edge_last_logged",
                    "temporal_edge_last_mixed",
                    "temporal_edge_undisturbed"),
      value != 0
    ) %>%
    mutate(
      edge_type = case_when(
        metric == "temporal_edge_last_burned"  ~ "Fire only",
        metric == "temporal_edge_last_logged"  ~ "Logging only",
        metric == "temporal_edge_last_mixed"   ~ "Mixed",
        metric == "temporal_edge_undisturbed"  ~ "Undisturbed"
      ),
      translated = factor(
        translated_levels[tolower(gsub(" ", "_", edge_type))],
        levels = unname(translated_levels)
      )
    ) %>%
    group_by(translated, edge_type) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      count_million = count / 1e6
    )
  
  p <- ggplot(df, aes(x = translated, y = count_million, fill = edge_type)) +
    geom_col(width = 0.6, alpha = 0.9, color = NA) +
    geom_text(
      aes(label = ifelse(count_million < 1,
                         format(round(count_million, 2), nsmall = 2),
                         format(round(count_million, 1), nsmall = 1))),
      vjust = -0.5, size = 5, color = "grey20", fontface = "bold"
    ) +
    scale_fill_manual(values = edge_fill_colors, guide = "none") +
    scale_y_millions() +
    labs(x = NULL, y = label("y_millions")) +
    theme_histogram()
  
  if (save) {
    size <- plot_size("compact")
    ggsave(file.path(output_dir, glue("03_edge_type_barplot.png")),
           p, width = size$width, height = size$height, dpi = 300)
    message(glue("✓ Saved plot: {veg_type}_03_edge_type_barplot.png"))
  }
  return(p)
}

# Run edge type total
plot_edge_type_total(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

##%###########################################################################%##
#                                                                               #
#                       3. Temporal Distribution of Edge Types               ----
#                                                                               #
##%###########################################################################%##
plot_edge_by_year <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  df <- data %>%
    filter(
      metric %in% c("temporal_edge_last_burned",
                    "temporal_edge_last_logged",
                    "temporal_edge_last_mixed",
                    "temporal_edge_undisturbed"),
      value != 0
    ) %>%
    mutate(
      edge_type_raw = case_when(
        metric == "temporal_edge_last_burned"   ~ "Fire only",
        metric == "temporal_edge_last_logged"   ~ "Logging only",
        metric == "temporal_edge_last_mixed"    ~ "Mixed",
        metric == "temporal_edge_undisturbed"   ~ "Undisturbed"
      ),
      edge_type = factor(
        sapply(edge_type_raw, translate_driver),
        levels = sapply(c("Undisturbed", "Fire only", "Logging only", "Mixed"), translate_driver)
      ),
      count_million = count / 1e6
    )
  
  translated_fill_colors <- setNames(
    edge_fill_colors[names(edge_fill_colors)],
    sapply(names(edge_fill_colors), translate_driver)
  )
  
  n_years <- length(unique(df$value))
  n_bars <- length(levels(df$edge_type))
  bar_width <- 0.75
  padding <- 3
  width_dynamic <- round(n_years * n_bars * bar_width / 6 + padding, 1)
  
  p <- ggplot(df, aes(x = value, y = count_million, fill = edge_type)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.6, alpha = 0.9, color = NA) +
    scale_fill_manual(values = translated_fill_colors[levels(df$edge_type)], name = NULL) +
    scale_x_continuous(breaks = seq(1990, 2020, by = 1),
                       expand = expansion(mult = c(0.005, 0.005))) +
    scale_y_millions() +
    labs(x = label("x_year"), y = label("y_millions")) +
    guides(
      fill = guide_legend(keywidth = 2, keyheight = 1),  # aumenta largura/altura
      color = guide_legend(keywidth = 2, keyheight = 1)
    ) +
    theme_temporal()
  
  if (save) {
    ggsave(file.path(output_dir, glue("04_edge_by_year.png")),
           p, width = width_dynamic, height = 6, dpi = 300)
    message(glue("✓ Saved plot: {veg_type}_04_edge_by_year.png (width = {width_dynamic})"))
  }
  return(p)
}

# Run edge plot
plot_edge_by_year(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)


##%###########################################################################%##
#                                                                               #
#                      4. Temporal delta in multiple events                 ----
#                                                                               #
##%###########################################################################%##
## Boxplot plot for disturbances delta 
# ------------------------------------------------------------------------- - - -
plot_disturbances_delta_boxplot <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  # Define metric order and grouping
  metric_levels <- c("fire2x", "log2x", "mix2x_ALL", "fire3x", "log3x", "mix3x_ALL")
  metric_group  <- c("2 events", "2 events", "2 events", "3 events", "3 events", "3 events")
  names(metric_group) <- metric_levels
  
  # Labels for each category (translated)
  translated_labels <- c(
    fire2x     = label("delta_fire2x"),
    log2x      = label("delta_log2x"),
    mix2x_ALL  = label("delta_mix2x"),
    fire3x     = label("delta_fire3x"),
    log3x      = label("delta_log3x"),
    mix3x_ALL  = label("delta_mix3x")
  )
  
  # Prepare data
  df <- data %>%
    filter(metric %in% metric_levels) %>%
    mutate(
      metric = factor(metric, levels = metric_levels),
      group  = metric_group[as.character(metric)],
      label  = translated_labels[as.character(metric)]
    )
  
  # Build boxplot
  plot_box <- ggplot(df, aes(x = label, y = value, fill = metric)) +
    geom_boxplot(aes(color = metric), outlier.alpha = 0.05, width = 0.4, alpha = 0.7) +
    scale_fill_manual(values = fill_colors, guide = "none") +
    scale_color_manual(values = border_colors, guide = "none") +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_grid(. ~ group, scales = "free_x", space = "free_x") +
    labs(
      x        = NULL,
      y        = label("y_pixel_delta")
    ) +
    theme_minimal(base_size = 14) +
    theme_histogram() +
    theme(strip.text = element_text(size = 12, face = "bold"))
  
  # Save or display
  if (save) {
    filename <- file.path(output_dir, glue("05_pixel_delta_boxplot.png"))
    ggsave(filename, plot_box, width = 9, height = 6, dpi = 300)
    message(glue("✓ Saved plot: {filename}"))
  } else {
    print(plot_box)
  }
}

# Run the pixel delta boxplot function
plot_disturbances_delta_boxplot(delta_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

##%###########################################################################%##
#                                                                               #
#                    5. Two-event patterns of mixed disturbance              ----
#                                                                               #
##%###########################################################################%##

# Total number of pixels with mixed disturbance (used for %)
plot_mixed_pattern_2 <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  df <- data %>%
    filter(metric == "pattern_mixed_2", !is.na(label)) %>%
    mutate(
      label = str_squish(label),
      translated_label = case_when(
        label == "Fire - Logging" ~ label("fire_logging_seq"),
        label == "Logging - Fire" ~ label("logging_fire_seq"),
        TRUE ~ label
      ),
      translated_label = factor(translated_label, levels = c(
        label("fire_logging_seq"), label("logging_fire_seq")
      )),
      count_million = count / 1e6
    )
  
  p <- ggplot(df, aes(x = translated_label, y = count_million, fill = translated_label)) +
    geom_col(aes(color = translated_label), width = 0.6, linewidth = NA, alpha = 0.7) +
    scale_fill_manual(values = pattern2_fill_colors, guide = "none") +
    scale_color_manual(values = pattern2_fill_colors, guide = "none") +
    scale_y_millions() +
    labs(x = label("x_sequence"), y = label("y_millions")) +
    geom_text(
      aes(label = ifelse(count_million < 1,
                         format(round(count_million, 2), nsmall = 2),
                         format(round(count_million, 1), nsmall = 1))),
      vjust = -0.5, size = 3.5, color = "grey20", fontface = "bold"
    ) +
    theme_histogram()
  
  if (save) {
    size <- plot_size("compact")
    ggsave(file.path(output_dir, glue("6a_pattern_mixed_2.png")),
           p, width = size$width, height = size$height, dpi = 300)
  }
  p
}

# # Run the function
# plot_mixed_pattern_2(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

##%###########################################################################%##
#                                                                               #
#                 6. Temporal delta in mixed with 2 patterns                ----
#                                                                               #
##%###########################################################################%##
##  Boxplot for mixed 2x delta 
# ------------------------------------------------------------------------- - - -
plot_mixed2x_delta_boxplot <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  pattern_levels <- c("mix2x_F-L", "mix2x_L-F")
  
  df <- data %>%
    filter(metric %in% pattern_levels) %>%
    mutate(
      pattern = factor(metric, levels = pattern_levels),
      label   = normalize_mixed_label(metric)
    )
  
  p <- ggplot(df, aes(x = label, y = value, fill = label, color = label)) +
    geom_boxplot(
      width = 0.45,
      outlier.alpha = 0.35,
      outlier.size = 1.3,
      linewidth = 0.5,
      alpha = 0.65
    ) +
    scale_fill_manual(values = pattern2_fill_colors, guide = "none") +
    scale_color_manual(values = pattern2_border_colors, guide = "none") +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = label("x_sequence_short"), y = label("y_pixel_delta")) +
    theme_histogram() +
    theme(axis.text.x = element_text(size = 12))
  
  if (save) {
    ggsave(file.path(output_dir, glue("6b_mixed2x_delta_boxplot.png")),
           p, width = 8, height = 6, dpi = 300)
  }
  p
}

 
# # Run the mixed 2x delta boxplot function
# plot_mixed2x_delta_boxplot(delta_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)


## 6.1 Combine mixed 2 pattern and delta boxplot into one panel ----
# ------------------------------------------------------------------------- - - -
mixed2_panel <- patchwork::wrap_plots(
  plot_mixed_pattern_2(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = FALSE),
  plot_mixed2x_delta_boxplot(delta_tbl, OUTPUT_DIR, VEG_TYPE, save = FALSE),
  nrow = 1, widths = c(1, 1)
)
if (WRITE_PLOT) {
  # PNG
  ggsave(file.path(OUTPUT_DIR, glue("06_mixed2_panel.png")),
         mixed2_panel, width = 12, height = 5.3, dpi = 300)
  # SVG
  ggsave(file.path(OUTPUT_DIR, glue("06_mixed2_panel.svg")),
         mixed2_panel, device = svglite, width = 12, height = 5.3)
}

##%###########################################################################%##
#                                                                               #
#              7. Three-event patterns of mixed disturbance                  ----
#                                                                               #
##%###########################################################################%##
plot_mixed_pattern_3 <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  df <- data %>%
    filter(metric == "pattern_mixed_3", !is.na(label)) %>%
    mutate(
      label  = normalize_mixed_label(label),
      label  = factor(label, levels = names(pattern3_fill_colors)),
      count_million = count / 1e6
    )
  
  p <- ggplot(df, aes(x = label, y = count_million, fill = label, color = label)) +
    geom_col(width = 0.6, linewidth = NA, alpha = 0.9, show.legend = FALSE) +
    scale_fill_manual(values = pattern3_fill_colors) +
    scale_color_manual(values = pattern3_fill_colors) +
    scale_y_millions() +
    labs(x = label("x_sequence_short"), y = label("y_millions")) +
    geom_text(
      aes(label = ifelse(count_million < 1,
                         format(round(count_million, 3), nsmall = 3),
                         format(round(count_million, 2), nsmall = 3))),
      vjust = -0.5, size = 3.5, color = "grey20", fontface = "bold"
    ) +
    theme_histogram()
  
  if (save) {
    size <- plot_size("compact")
    ggsave(file.path(output_dir, glue("7a_pattern_mixed_3.png")),
           p, width = size$width, height = size$height, dpi = 300)
  }
  p
}


# # Run the function
# plot_mixed_pattern_3(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = WRITE_PLOT)

##%###########################################################################%##
#                                                                               #
#                 8. Temporal delta in mixed with 3patterns                 ----
#                                                                               #
##%###########################################################################%##
# Boxplot for mixed 3x patterns delta
# ------------------------------------------------------------------------- - - -
plot_mixed3x_delta_boxplot <- function(data, output_dir, veg_type = "veg3", save = TRUE) {
  
  pattern_levels <- c(
    "mix3x_F-F-L", "mix3x_F-L-F", "mix3x_F-L-L",
    "mix3x_L-F-F", "mix3x_L-F-L", "mix3x_L-L-F"
  )
  
  df <- data %>%
    filter(metric %in% pattern_levels) %>%
    mutate(
      pattern = str_remove(metric, "^mix3x_"),
      label   = normalize_mixed_label(pattern),
      label   = factor(label, levels = names(pattern3_fill_colors))
    )
  
  p <- ggplot(df, aes(x = label, y = value, fill = label, color = label)) +
    geom_boxplot(
      width = 0.45,
      outlier.alpha = 0.35,
      outlier.size  = 1.1,
      linewidth = 0.5,
      alpha = 0.6
    ) +
    scale_fill_manual(values = pattern3_fill_colors, guide = "none") +
    scale_color_manual(values = pattern3_border_colors, guide = "none") +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = label("x_sequence_short"), y = label("y_pixel_delta")) +
    theme_histogram() +
    theme(axis.text.x = element_text(size = 11))
  
  if (save) {
    ggsave(file.path(output_dir, glue("7b_mixed3x_delta_boxplot.png")),
           p, width = 9.5, height = 6, dpi = 300)
  }
  p
}

## 8.1 Combine mixed 3 pattern and delta boxplot into one panel ----
# ------------------------------------------------------------------------- - - -
mixed3_panel <- patchwork::wrap_plots(
  plot_mixed_pattern_3(histograms_tbl, OUTPUT_DIR, VEG_TYPE, save = FALSE),
  plot_mixed3x_delta_boxplot(delta_tbl, OUTPUT_DIR, VEG_TYPE, save = FALSE),
  nrow = 1, widths = c(1, 1)
)
if (WRITE_PLOT) {
  # PNG
  ggsave(file.path(OUTPUT_DIR, glue("07_mixed3_panel.png")),
         mixed3_panel, width = 12, height = 5.3, dpi = 300)
  # SVG
  ggsave(file.path(OUTPUT_DIR, glue("07_mixed3_panel.svg")),
         mixed3_panel, device = svglite, width = 12, height = 5.3)
}