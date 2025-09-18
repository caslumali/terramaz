##%###########################################################################%##
#                                                                               #
#                           Transition TMF (Sankey)                          ----
#                         Single long CSV (all years)                           #
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
  library(ggalluvial)
  library(glue)
  library(svglite)
  library(rlang)
  library(colorspace)
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE
WRITE_SVG  <- FALSE

# Visualization flags for Sankey
STAYERS_DEFAULT  <- FALSE   # keep or remove src==dst (permanence)
MIN_FLOW_HA      <- 1      # drop tiny residual flows

# Number of Sankey stages (3 or 4)
N_STAGES <- 3L   # set to 4L to enable T0→T1→T2→T3

# Aesthetics (tunable, used in plot helper)
FLOW_ALPHA       <- 0.40
STRATUM_ALPHA    <- 0.75
STRATUM_WIDTH    <- 0.30
STRATUM_LABELS   <- FALSE  # add class labels on strata when TRUE
STRATUM_MIN_HA   <- 2000   # only label strata >= this area (ha)
MIN_PROP_PER_MID <- 0.02  # 0.3% of the mid's outgoing area (tune 0.002–0.01)

# Territories to render (edit as needed)
# TERRITORIES <- c("madre_de_dios") # quick test
TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

# Pretty labels for titles
TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Output figure specs (A4-width style, like your time series)
FILENAME_STUB <- "transition_tmf"
FIG_WIDTH_MM  <- 431.8  # 17 in
FIG_HEIGHT_MM <- 228.6  # 9 in (a bit taller than TS)
UNITS         <- "mm"
DPI           <- 300

# TMF valid year window
TMF_YEAR_MIN <- 1991
TMF_YEAR_MAX <- 2024

## 1.2 Breakpoints ----
# ------------------------------------------------------------------------- - - -
# Default breakpoints (can be overridden per territory below)
# Example: Paragominas policy turning point ~2008
if (N_STAGES == 3L) {
  BREAKS <- list(
    cotriguacu    = c(T0 = 1991L, T1 = 2004L, T2 = 2024L),
    paragominas   = c(T0 = 1991L, T1 = 2008L, T2 = 2024L),
    guaviare      = c(T0 = 1991L, T1 = 2016L, T2 = 2024L),
    madre_de_dios = c(T0 = 1991L, T1 = 2010L, T2 = 2024L)
  )
} else {
  BREAKS <- list(
    cotriguacu    = c(T0 = 1991L, T1 = 2004L, T2 = 2012L, T3 = 2024L),
    paragominas   = c(T0 = 1991L, T1 = 2004L, T2 = 2008L, T3 = 2024L),
    guaviare      = c(T0 = 1991L, T1 = 2006L, T2 = 2016L, T3 = 2024L),
    madre_de_dios = c(T0 = 1991L, T1 = 2005L, T2 = 2010L, T3 = 2024L)
  )
}

## 1.3 Language & labels ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"

# Localization (titles, axes, caption)
LABELS <- list(
  # Title without dates
  title_tmf = c(
    fr = "Transitions forestières à {territory}",
    pt = "Transições florestais em {territory}",
    es = "Transiciones forestales en {territory}",
    en = "Forest transitions in {territory}"
  ),
  # Subtitle only with the period (T0 → T1 → T2)
  subtitle_tmf_period = c(
    fr = "{T0} \u2192 {T1} \u2192 {T2}",
    pt = "{T0} \u2192 {T1} \u2192 {T2}",
    es = "{T0} \u2192 {T1} \u2192 {T2}",
    en = "{T0} \u2192 {T1} \u2192 {T2}"
  ),
  y_area_ha = c(
    fr = "Surface (ha)",
    es = "Área (ha)",
    pt = "Área (ha)",
    en = "Area (ha)"
  ),
  caption_tmf = c(
    fr = "Sources — JRC-TMF : 1990–2024",
    es = "Fuentes — JRC-TMF: 1990–2024",
    pt = "Fontes — JRC-TMF: 1990–2024",
    en = "Sources — JRC-TMF: 1990–2024"
  )
)

label <- function(key, ...) {
  glue::glue(LABELS[[key]][[LANG]], .envir = rlang::env(...))
}

## 1.3.1 TMF legend labels (i18n) ----
TMF_CLASS_I18N <- list(
  fr = c(
    "Undisturbed forest" = "Forêt non perturbée",
    "Degraded forest"    = "Forêt dégradée",
    "Deforested land"    = "Terres déforestées",
    "Forest regrowth"    = "Forêt secondaire",
    "Water"              = "Eau",
    "Other land cover"   = "Autres couvertures"
  ),
  pt = c(
    "Undisturbed forest" = "Floresta não perturbada",
    "Degraded forest"    = "Floresta degradada",
    "Deforested land"    = "Área desmatada",
    "Forest regrowth"    = "Floresta secundária",
    "Water"              = "Água",
    "Other land cover"   = "Outras coberturas"
  ),
  es = c(
    "Undisturbed forest" = "Bosque no perturbado",
    "Degraded forest"    = "Bosque degradado",
    "Deforested land"    = "Tierra deforestada",
    "Forest regrowth"    = "Bosque secundario",
    "Water"              = "Agua",
    "Other land cover"   = "Otras coberturas"
  ),
  en = c(
    "Undisturbed forest" = "Undisturbed forest",
    "Degraded forest"    = "Degraded forest",
    "Deforested land"    = "Deforested land",
    "Forest regrowth"    = "Forest regrowth",
    "Water"              = "Water",
    "Other land cover"   = "Other land cover"
  )
)
tmf_labels_for <- function(lang) TMF_CLASS_I18N[[lang]]

## 1.4 TMF palette & class order (from your table) ----
# ------------------------------------------------------------------------- - - -
tmf_palette <- c(
  "Undisturbed forest" = "#005A00",   # 0,90,0
  "Degraded forest"    = "#649B23",   # 100,155,35
  "Deforested land"    = "#FF871F",   # 255,135,15
  "Forest regrowth"    = "#D2FA3C",   # 210,250,60
  "Water"              = "#008CBE",   # 0,140,190
  "Other land cover"   = "#575757ff"    # 255,255,255
)

# tmf_palette <- c(
#   "Undisturbed forest" = "#295029ff",   # 0,90,0
#   "Degraded forest"    = "#4b8804ff",   # 100,155,35
#   "Deforested land"    = "#b74848ff",   # 255,135,15
#   "Forest regrowth"    = "#caa640ff",   # 210,250,60
#   "Water"              = "#1b789aff",   # 0,140,190
#   "Other land cover"   = "#575757ff"    # 255,255,255
# )

tmf_order <- c(
  "Undisturbed forest",
  "Forest regrowth",
  "Degraded forest",
  "Deforested land",
  "Water",
  "Other land cover"
)

## 1.5 Theme for Sankey ----
# ------------------------------------------------------------------------- - - -
theme_sankey <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)),
      axis.title.x      = element_blank(),
      axis.text.x       = element_text(size = 12),
      axis.title.y      = element_text(size = 12, margin = margin(r = 12)),
      axis.ticks        = element_blank(),
      panel.grid        = element_blank(),     # no grids
      panel.border      = element_blank(),
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.title      = element_blank(),
      legend.box        = "horizontal",
      legend.background = element_blank(),     # no legend box
      legend.key        = element_rect(fill = NA, colour = NA),
      legend.key.height = unit(0.6, "lines"),  # rectangular keys
      legend.key.width  = unit(3.0, "lines"),  # rectangular keys
      legend.margin     = margin(b = 6),
      plot.margin       = margin(12, 12, 12, 12),
      plot.caption      = element_text(hjust = 1, size = 10, color = "gray30", margin = margin(t = 12)),
      plot.subtitle     = element_text(hjust = 0.5, size = 12, face = "italic", margin = margin(t = 0, b = 12))
    )
}

## 1.6 Y scale helper (pretty, no scientific) ----
# ------------------------------------------------------------------------- - - -
# # Pretty Y axis in full hectares (no scientific, nice thousands separators)
# axis_y_ha_auto <- function(y_max_raw) {
#   ymax <- max(0, as.numeric(y_max_raw))
#   if (!is.finite(ymax) || ymax <= 0) ymax <- 10000

#   # Clean steps: 5k, 10k, 20k, 50k, 100k, 200k, 500k, 1M…
#   steps <- c(5000, 10000, 20000, 50000, 100000, 200000, 500000, 1000000)
#   target_n <- 6
#   step <- steps[ which.min(abs((ceiling(ymax/steps)+1) - target_n)) ]
#   ymax <- ceiling(ymax/step)*step

#   ggplot2::scale_y_continuous(
#     breaks = seq(0, ymax, by = step),
#     labels = function(v) format(v, big.mark = " ", scientific = FALSE, trim = TRUE),
#     limits = c(0, ymax),
#     expand = expansion(mult = c(0.03, 0.03))
#   )
# }

# Y "tight": limits to the maximum observed (+2% slack) and creates nice breaks
axis_y_ha_tight <- function(y_max_raw, pad = 0.02, n_breaks = 6) {
  ymax <- max(0, as.numeric(y_max_raw))
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1
  lim <- c(0, ymax * (1 + pad))
  ggplot2::scale_y_continuous(
    limits = lim,
    breaks = scales::breaks_extended(n = n_breaks)(lim),
    labels = scales::label_number(accuracy = 1000, big.mark = " "),
    expand = c(0, 0)
  )
}

##%###########################################################################%##
#                                                                               #
#                           2) Utility Functions                              ----
#                                                                               #
##%###########################################################################%##

## 2.1 Robust column detection for the long CSV ----
# ------------------------------------------------------------------------- - - -
# We accept multiple conventions. The function returns a list of canonical names:
#   year, area_ha, src_label, dst_label  (strings that exist in df)
detect_columns_tmf <- function(df) {
  nms <- names(df); nms_lc <- tolower(nms)

  # year column
  year_candidates <- c("year","ano","yr","year_int","cut_year","transition_year")
  year_idx <- match(year_candidates, nms_lc, nomatch = 0)
  if (!any(year_idx > 0)) abort(glue("Year column not found. Available: {paste(nms, collapse=', ')}"))
  year_col <- nms[year_idx[year_idx > 0][1]]

  # area (ha)
  ha_idx <- which(str_detect(nms_lc, "(^|_)area_ha$|(^|_)ha$"))
  if (length(ha_idx) == 0) abort(glue("No '*_area_ha' or '*_ha' column found. Available: {paste(nms, collapse=', ')}"))
  area_col <- nms[ha_idx[1]]

  # src/dst labels (prefer *_label; fallback to src/dst if they already are labels)
  src_label_idx <- which(str_detect(nms_lc, "^(src|source).*(label)$|^(src_label)$"))
  dst_label_idx <- which(str_detect(nms_lc, "^(dst|dest|target).*(label)$|^(dst_label)$"))

  if (length(src_label_idx) == 0) {
    # try 'src' column as label
    src_label_idx <- which(nms_lc %in% c("src","source","from"))
  }
  if (length(dst_label_idx) == 0) {
    dst_label_idx <- which(nms_lc %in% c("dst","dest","to","target"))
  }

  if (length(src_label_idx) == 0 || length(dst_label_idx) == 0) {
    abort(glue("Source/Target label columns not found. Need src_label & dst_label (or src/dst). Available: {paste(nms, collapse=', ')}"))
  }

  list(
    year      = nms[src_label_idx*0 + which(nms == year_col)],
    area_ha   = nms[ha_idx[1]],
    src_label = nms[src_label_idx[1]],
    dst_label = nms[dst_label_idx[1]]
  )
}

## 2.2 Build 3-stage long table for ggalluvial ----
# ------------------------------------------------------------------------- - - -
# Fixed to handle annual transition data properly by aggregating periods
build_alluvial_long_3  <- function(d01, d12, tmf_order) {
  
  # Aggregate transitions by src->dst pairs for each period
  # This handles the case where we have annual data that needs to be summed
  d01_agg <- d01 %>% 
    dplyr::group_by(src, dst) %>%
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(area_ha > 0) %>%
    dplyr::rename(mid = dst, area01 = area_ha)
  
  d12_agg <- d12 %>% 
    dplyr::group_by(src, dst) %>%
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(area_ha > 0) %>%
    dplyr::rename(mid = src, area12 = area_ha)

  # Keep only mids present in both periods
  mids <- intersect(unique(d01_agg$mid), unique(d12_agg$mid))
  d01_clean <- d01_agg %>% dplyr::filter(mid %in% mids)
  d12_clean <- d12_agg %>% dplyr::filter(mid %in% mids)
  
  if (nrow(d01_clean) == 0L || nrow(d12_clean) == 0L) {
    rlang::abort("No overlapping mid classes between T0->T1 and T1->T2 periods.")
  }

  # Calculate outgoing proportions in T1->T2 per mid class
  prop_d12 <- d12_clean %>%
    dplyr::group_by(mid) %>%
    dplyr::summarise(
      transitions = list(tibble(dst = dst, area12 = area12)),
      total_mid_out = sum(area12, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::unnest(transitions) %>%
    dplyr::mutate(prop = dplyr::if_else(total_mid_out > 0, area12 / total_mid_out, 0)) %>%
    dplyr::select(mid, dst, prop)

  # Remove negligible proportions to reduce hairlines
  prop_d12 <- prop_d12 %>% dplyr::filter(prop >= MIN_PROP_PER_MID)

  # Distribute each src->mid flow to destinations using calculated proportions
  flows <- d01_clean %>%
    dplyr::inner_join(prop_d12, by = "mid", relationship = "many-to-many") %>%
    dplyr::mutate(area = area01 * prop) %>%
    dplyr::filter(area > 0)

  # Create a stable alluvium id BEFORE pivot, then go long
  flows_id <- flows %>%
    dplyr::transmute(
      src_T0  = src,
      mid     = mid,
      dst_T2  = dst,
      area_ha = area,
      flow_id = interaction(src, mid, dst, drop = TRUE, lex.order = TRUE)
    )

  long <- flows_id %>%
    tidyr::pivot_longer(
      c(src_T0, mid, dst_T2),
      names_to  = "stage",
      values_to = "class"
    ) %>%
    dplyr::mutate(
      class = factor(class, levels = tmf_order),
      stage = factor(stage, levels = c("src_T0","mid","dst_T2"))
    )

  return(long)
}

## 2.3 Build 4-stage long table for ggalluvial ----
# ------------------------------------------------------------------------- - - -
# Build 4-stage long table (T0->T1->T2->T3) by chaining proportions
build_alluvial_long_4 <- function(d01, d12, d23, tmf_order, 
                                  min_prop_mid1 = 0, min_prop_mid2 = 0) {

  d01a <- d01 %>% group_by(src, dst) %>%
    summarise(area01 = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(mid1 = dst)

  d12a <- d12 %>% group_by(src, dst) %>%
    summarise(area12 = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(mid1 = src, mid2 = dst)

  d23a <- d23 %>% group_by(src, dst) %>%
    summarise(area23 = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(mid2 = src)

  mids1 <- intersect(unique(d01a$mid1), unique(d12a$mid1))
  d01a  <- d01a %>% filter(mid1 %in% mids1)
  d12a  <- d12a %>% filter(mid1 %in% mids1)

  mids2 <- intersect(unique(d12a$mid2), unique(d23a$mid2))
  d12a  <- d12a %>% filter(mid2 %in% mids2)
  d23a  <- d23a %>% filter(mid2 %in% mids2)

  if (nrow(d01a)==0L || nrow(d12a)==0L || nrow(d23a)==0L)
    rlang::abort("No overlap across mid1/mid2 between the three periods.")

  p12 <- d12a %>% group_by(mid1) %>%
    mutate(total = sum(area12, na.rm = TRUE),
           prop12 = if_else(total > 0, area12/total, 0)) %>%
    ungroup() %>% select(mid1, mid2, prop12) %>%
    filter(prop12 >= min_prop_mid1)

  p23 <- d23a %>% group_by(mid2) %>%
    mutate(total = sum(area23, na.rm = TRUE),
           prop23 = if_else(total > 0, area23/total, 0)) %>%
    ungroup() %>% select(mid2, dst, prop23) %>%
    filter(prop23 >= min_prop_mid2)

  flows <- d01a %>%
    inner_join(p12, by="mid1", relationship="many-to-many") %>%
    mutate(area_tmp = area01 * prop12) %>%
    inner_join(p23, by="mid2", relationship="many-to-many") %>%
    mutate(area_ha = area_tmp * prop23) %>%
    filter(area_ha > 0)

  long <- flows %>%
    transmute(
      src_T0 = src, mid1 = mid1, mid2 = mid2, dst_T3 = dst,
      area_ha = area_ha,
      flow_id = interaction(src, mid1, mid2, dst, drop = TRUE, lex.order = TRUE)
    ) %>%
    tidyr::pivot_longer(c(src_T0, mid1, mid2, dst_T3),
                        names_to = "stage", values_to = "class") %>%
    mutate(
      class = factor(class, levels = tmf_order),
      stage = factor(stage, levels = c("src_T0","mid1","mid2","dst_T3"))
    )

  long
}


## 2.4 Plotting helper ----
# ------------------------------------------------------------------------- - - -
plot_sankey_tmf <- function(long_df, territory, lang = "fr") {
  # --- Titles & caption -----------------------------------------------------
  title_txt    <- label("title_tmf", territory = TERRITORY_LABELS[[territory]])
  stage_labels <- levels(long_df$stage)                           # labels já setados fora
  subtitle_txt <- paste(stage_labels, collapse = " \u2192 ")
  cap_txt      <- LABELS$caption_tmf[[lang]]

  # --- Y max for tight axis -------------------------------------------------
  totals <- long_df %>%
    dplyr::group_by(stage) %>%
    dplyr::summarise(total = sum(area_ha, na.rm = TRUE), .groups = "drop")
  ymax <- max(totals$total, na.rm = TRUE)

  # --- Legend: only classes that actually appear ---------------------------
  # English: show only present classes, preserving your preferred order
  present_classes <- intersect(tmf_order, levels(droplevels(long_df$class)))
  if (length(present_classes) == 0) present_classes <- tmf_order  # safe fallback
  n_cols_legend <- 6L
  n_rows_legend <- ceiling(length(present_classes) / n_cols_legend)

  # --- Plot -----------------------------------------------------------------
  ggplot(
    long_df,
    aes(x = stage, stratum = class, alluvium = flow_id,
        y = area_ha, fill = class, label = class)
  ) +
    # English: match MB look — subtle white outline, cubic curve, earlier knot,
    # fewer crossings with frontback lode guidance
    geom_flow(
      stat = "alluvium",
      lwd = 0.2,
      color = scales::alpha("white", 0.20),
      alpha = FLOW_ALPHA,
      curve_type = "cubic",
      knot.pos = 0.30,
      lode.guidance = "frontback"
    ) +
    geom_stratum(
      width = STRATUM_WIDTH,
      color = "white", size = 0.3,
      alpha = STRATUM_ALPHA
    ) +
    scale_fill_manual(
      values = tmf_palette[present_classes],
      breaks = present_classes,
      labels = tmf_labels_for(lang)[present_classes],
      drop   = TRUE,
      guide  = guide_legend(
        nrow = n_rows_legend, byrow = TRUE,
        keywidth  = unit(1.5, "lines"),
        keyheight = unit(0.5, "lines"),
        override.aes = list(alpha = STRATUM_ALPHA, colour = NA)
      )
    ) +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
    axis_y_ha_tight(ymax) +
    labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      y        = LABELS$y_area_ha[[lang]],
      caption  = cap_txt
    ) +
    theme_sankey()
}


## 2.5 Main worker (single territory) ----
# ------------------------------------------------------------------------- - - -
# Fixed to properly handle period-based aggregation
make_sankey_tmf  <- function(
  territory,
  lang          = c("fr","pt","es","en"),
  T0, T1, T2, T3 = NULL,          # NEW
  n_stages = N_STAGES,            # NEW
  stayers       = STAYERS_DEFAULT,
  min_flow_ha   = MIN_FLOW_HA,
  width_mm      = FIG_WIDTH_MM,
  height_mm     = FIG_HEIGHT_MM,
  out_dir       = "results/plots",
  csv_long_path = NULL
) {
  lang <- match.arg(lang)
  n_stages <- as.integer(n_stages)

  if (n_stages == 4L) {
    if (is.null(T3)) abort("n_stages=4 requires T3.")
    if (!(TMF_YEAR_MIN <= T0 && T0 < T1 && T1 < T2 && T2 < T3 && T3 <= TMF_YEAR_MAX)) {
      abort(glue("Invalid breakpoints: require {TMF_YEAR_MIN} <= T0 < T1 < T2 < T3 <= {TMF_YEAR_MAX}."))
    }
  } else {
    if (!(TMF_YEAR_MIN <= T0 && T0 < T1 && T1 < T2 && T2 <= TMF_YEAR_MAX)) {
      abort(glue("Invalid breakpoints: require {TMF_YEAR_MIN} <= T0 < T1 < T2 <= {TMF_YEAR_MAX}."))
    }
  }

  # Locate CSV if not provided
  if (is.null(csv_long_path)) {
    input_dir <- file.path("results/metrics", territory)
    cand <- list.files(
      input_dir, full.names = TRUE, ignore.case = TRUE,
      pattern = glue("^{territory}_tmf_transitions.*\\.csv$")
    )
    if (length(cand) == 0) abort(glue("No TMF transition CSV found in {input_dir}."))
    csv_long_path <- cand[1]
  }

  message(glue("📊 Loading: {basename(csv_long_path)}"))
  df <- suppressMessages(readr::read_csv(csv_long_path, show_col_types = FALSE))
  cols <- detect_columns_tmf(df)

  df_clean <- df %>%
    transmute(
      year     = as.integer(.data[[cols$year]]),
      area_ha  = suppressWarnings(as.numeric(.data[[cols$area_ha]])),
      src      = as.character(.data[[cols$src_label]]),
      dst      = as.character(.data[[cols$dst_label]])
    ) %>%
    filter(!is.na(year), !is.na(area_ha), area_ha > 0)

  # Enforce TMF window
  df_clean <- df_clean %>% filter(year >= TMF_YEAR_MIN, year <= TMF_YEAR_MAX)

  # Period 1: T0->T1
  d01 <- df_clean %>% filter(year > T0, year <= T1) %>% select(src, dst, area_ha)
  # Period 2: T1->T2
  d12 <- df_clean %>% filter(year > T1, year <= T2) %>% select(src, dst, area_ha)
  # Period 3: T2->T3 (only if 4-stage)
  if (n_stages == 4L) {
    d23 <- df_clean %>% filter(year > T2, year <= T3) %>% select(src, dst, area_ha)
    if (nrow(d23) == 0L) abort(glue("No transition data found for period {T2}-{T3}."))
  }

  if (nrow(d01) == 0L) {
    abort(glue("No transition data found for period {T0}-{T1}. Check your breakpoints and data."))
  }
  if (nrow(d12) == 0L) {
    abort(glue("No transition data found for period {T1}-{T2}. Check your breakpoints and data."))
  }

  message(glue("📈 Period {T0}-{T1}: {nrow(d01)} transition records"))
  message(glue("📈 Period {T1}-{T2}: {nrow(d12)} transition records"))

  # Apply filters BEFORE aggregation for better control
  if (!stayers) {
    d01 <- d01 %>% filter(src != dst)
    d12 <- d12 %>% filter(src != dst)
    if (n_stages == 4L) d23 <- d23 %>% filter(src != dst)
  }
  d01 <- d01 %>% filter(area_ha >= min_flow_ha)
  d12 <- d12 %>% filter(area_ha >= min_flow_ha)
  if (n_stages == 4L) d23 <- d23 %>% filter(area_ha >= min_flow_ha)

  # Build long alluvial table using fixed function
  if (n_stages == 4L) {
    long <- build_alluvial_long_4(
      d01, d12, d23, tmf_order,
      min_prop_mid1 = MIN_PROP_PER_MID, min_prop_mid2 = MIN_PROP_PER_MID
    ) %>%
      mutate(
        stage = factor(stage, levels = c("src_T0","mid1","mid2","dst_T3"),
                      labels = c(as.character(T0), as.character(T1),
                                  as.character(T2), as.character(T3))),
        class = forcats::fct_na_value_to_level(class, level = "Other land cover")
      )
  } else {
    long <- build_alluvial_long_3(d01, d12, tmf_order) %>%
      mutate(
        stage = factor(stage, levels = c("src_T0","mid","dst_T2"),
                      labels = c(as.character(T0), as.character(T1), as.character(T2))),
        class = forcats::fct_na_value_to_level(class, level = "Other land cover")
      )
}

  if (nrow(long) == 0) {
    abort("No flows left after filtering. Check column detection, breakpoints, or filtering parameters.")
  }

  message(glue("✅ Generated {nrow(long)} alluvial records"))

  # Generate plot
  p <- plot_sankey_tmf(long, territory = territory, lang = lang)
  print(p)
  message(glue("✓ Sankey generated for {territory} ({T0}-{T1}-{T2}) stayers={stayers}"))

  # Export files
  out_base <- file.path(out_dir, territory, glue("{territory}_{lang}"))
  if (!dir.exists(out_base)) dir.create(out_base, recursive = TRUE)
  # tag <- if (stayers) "withStayers" else "noStayers"
  stage_tag <- if (n_stages == 4L) "4stages" else "3stages"
  file_stub <- glue("05a_{territory}_sankey_tmf_{stage_tag}_{lang}")

  if (isTRUE(WRITE_PLOT)) {
    png_path <- file.path(out_base, glue("{file_stub}.png"))
    ggsave(png_path, plot = p, width = width_mm, height = height_mm, units = UNITS,
           dpi = DPI, bg = "white")
    if (isTRUE(WRITE_SVG)) {
      svg_path <- file.path(out_base, glue("{file_stub}.svg"))
      ggsave(svg_path, plot = p, width = width_mm, height = height_mm, units = UNITS,
             device = svglite, bg = "white")
    }
    message("✅ Saved:")
    message(glue("   PNG: {basename(png_path)}  ({width_mm}×{height_mm} mm)"))
    message(glue("   SVG: {if (WRITE_SVG) basename(svg_path) else '(skipped)'}"))
  } else {
    message("ℹ Preview mode — set WRITE_PLOT <- TRUE to export.")
  }

  invisible(p)
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

    brks <- BREAKS[[TERRITORY]]
    if (is.null(brks)) {
      message(glue("⚠ No breakpoints for {TERRITORY} — skipping."))
      next
    }

    # Auto-discover CSV under results/metrics/{territory}
    csv_path <- NULL

    # 3.0 Auto-discover CSV for this territory ----
    csv_path <- list.files(
      file.path("results/metrics", TERRITORY),
      pattern    = glue("^{TERRITORY}.*tmf.*transition.*\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(csv_path) == 0) {
      message(glue("⚠ No TMF transition CSV found under results/metrics/{TERRITORY} — skipping."))
      next
    }
    csv_path <- csv_path[1]
    message(glue("📄 Using CSV: {basename(csv_path)}"))

    # 3.1 Single render ----
    try({
      message(glue("🧩 Variant: {if (STAYERS_DEFAULT) 'withStayers' else 'noStayers'}"))
      if (N_STAGES == 4L) {
        if (!"T3" %in% names(brks)) {
          message("⚠ N_STAGES=4 but T3 missing in BREAKS for {TERRITORY} — skipping.")
        } else {
          make_sankey_tmf(
            territory     = TERRITORY,
            lang          = LANG,
            T0 = brks["T0"], T1 = brks["T1"], T2 = brks["T2"], T3 = brks["T3"],
            n_stages      = 4L,
            stayers       = STAYERS_DEFAULT,
            min_flow_ha   = MIN_FLOW_HA,
            csv_long_path = csv_path
          )
        }
      } else {
        make_sankey_tmf(
          territory     = TERRITORY,
          lang          = LANG,
          T0 = brks["T0"], T1 = brks["T1"], T2 = brks["T2"],
          n_stages      = 3L,
          stayers       = STAYERS_DEFAULT,
          min_flow_ha   = MIN_FLOW_HA,
          csv_long_path = csv_path
        )
      }
    }, silent = FALSE)

    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}
  