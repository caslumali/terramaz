##%###########################################################################%##
#                                                                               #
#                        MapBiomas Land-Use Transitions (Sankey)             ----
#                           Single long CSV (all years)                         #
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
  library(forcats)
})

## 1.1 Global parameters ----
# ------------------------------------------------------------------------- - - -
WRITE_PLOT <- TRUE
WRITE_SVG  <- FALSE

### 1.1.1 Aggregation & filtering knobs  ------
# ------------------------------------------------------------------------- - - -
# English: collapse detailed classes but KEEP Urban and Mining separated.
N_STAGES <- 3L               # set to 3L if you want T0→T1→T2

# Visualization flags for Sankey
STAYERS_DEFAULT   <- TRUE
SHOW_ALL <- FALSE

# Aesthetics 
FLOW_ALPHA <- 0.50 # flow ribbons transparency 
STRATUM_ALPHA <- 0.75 # blocks transparency 
STRATUM_WIDTH <- 0.30
STRATUM_LABELS <- FALSE # add class labels on strata when TRUE 

# If legend is too wide, wrap into columns. Tweak this default if needed.
LEGEND_NCOL  <- 4L

# Global filtering thresholds (can be overridden per territory)
MIN_FLOW_HA      <- 2000
MIN_FLOW_SHARE   <- 0.002
MIN_PROP_PER_MID <- 0.02
TOPK_PER_MID     <- Inf

# Per-territory overrides (choose thresholds by size/complexity)
# en: tune these to taste after a first pass
TERRITORY_OVERRIDES <- list(
  cotriguacu    = list(min_ha = 200,  min_share = 0.001, min_prop_mid = 0.02, topk = Inf),
  paragominas   = list(min_ha = 1000, min_share = 0.002, min_prop_mid = 0.05, topk = 6),
  guaviare      = list(min_ha = 2000, min_share = 0.003, min_prop_mid = 0.05, topk = 6),
  madre_de_dios = list(min_ha = 2000, min_share = 0.004, min_prop_mid = 0.05, topk = 6)
)

# Territories to render
TERRITORIES <- c("paragominas")
# TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")

# Drop these classes from the analysis (e.g., Water)
DROP_CLASSES <- NULL        # set to NULL to keep all
# DROP_CLASSES <- c("Water")    # e.g., c("Water", "Others")

# Force-keep these classes (never drop due to floors or top-k)
KEEP_CLASSES <- NULL          # set to NULL to keep all  
# KEEP_CLASSES <- c("Mining")   # e.g., c("Mining","Urban")

# Pretty labels for titles
TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

# Output figure specs
FILENAME_SANKEY <- "05_{territory}_sankey_{stage_tag}_{lang}"
FIG_WIDTH_MM    <- 431.8  # 17 in
FIG_HEIGHT_MM   <- 228.6  # 9 in
UNITS           <- "mm"
DPI             <- 300

# MapBiomas year window (Collection 6 for Amazônia)
MB_YEAR_MIN <- 1986L
MB_YEAR_MAX <- 2023L

## 1.2 Breakpoints (3- or 4-stage) ----
# ------------------------------------------------------------------------- - - -
# You can tune these per territory. Defaults below mirror your TMF approach.
if (N_STAGES == 3L) {
  BREAKS <- list(
    cotriguacu    = c(T0 = 1986L, T1 = 2004L, T2 = 2023L),
    paragominas   = c(T0 = 1986L, T1 = 2008L, T2 = 2023L),
    guaviare      = c(T0 = 1986L, T1 = 2016L, T2 = 2023L),
    madre_de_dios = c(T0 = 1986L, T1 = 2010L, T2 = 2023L)
  )
} else {
  BREAKS <- list(
    cotriguacu    = c(T0 = 1986L, T1 = 2004L, T2 = 2012L, T3 = 2023L),
    paragominas   = c(T0 = 1986L, T1 = 2004L, T2 = 2008L, T3 = 2023L),
    guaviare      = c(T0 = 1986L, T1 = 2006L, T2 = 2016L, T3 = 2023L),
    madre_de_dios = c(T0 = 1986L, T1 = 2005L, T2 = 2010L, T3 = 2023L)
  )
}

## 1.3 Language & labels (titles, axes, caption) ----
# ------------------------------------------------------------------------- - - -
LANGS <- c("fr")  # "pt" | "es" | "fr" | "en"

LABELS <- list(
  # Title (no dates)
  title_mb = c(
    fr = "Transitions de la couverture du sol à {territory}",
    es = "Transiciones de la cobertura del suelo en {territory}",
    pt = "Transições da cobertura do solo em {territory}",
    en = "Land cover transitions in {territory}"
  ),
  # Subtitle shows only the breakpoints (T0 → T1 → …)
  subtitle_period = c(
    fr = "{period}", pt = "{period}", es = "{period}", en = "{period}"
  ),
  # Y axis
  y_area_ha = c(fr = "Surface (ha)", es = "Área (ha)", pt = "Área (ha)", en = "Area (ha)"),
  # Caption (source line)

  caption_mb = c(
    fr = "Sources — TMF-JRC : 1991-2023 combiné avec MapBiomas : 1985-2023",
    es = "Fuentes — TMF-JRC: 1991-2023 combinado con MapBiomas: 1985-2023",
    pt = "Fontes —  TMF-JRC: 1991-2023 combinado com MapBiomas: 1985-2023",
    en = "Sources — TMF-JRC: 1991-2023 combined with MapBiomas: 1985-2023"
  )
)

label <- function(key, ...) {
  glue::glue(LABELS[[key]][[LANG]], .envir = rlang::env(...))
}

## 1.3.1 Class labels (i18n) for MapBiomas ----
# Keys MUST match the strings in your CSV (src_label/dst_label)
## 1.3.1 Class labels (i18n) for TMF+MB ----
TMFMB_CLASS_I18N <- list(
  fr = c(
    "Undisturbed TF" = "Forêt de terre ferme intacte",
    "Degraded TF"    = "Forêt de terre ferme dégradée",
    "Regrowth TF"    = "Forêt de terre ferme en régénération",
    "Undisturbed FF" = "Forêt inondée intacte",
    "Degraded FF"    = "Forêt inondée dégradée",
    "Regrowth FF"    = "Forêt inondée en régénération",
    "Natural NF"     = "Non-forêt naturelle",
    "Pasture"        = "Pâturage",
    "Agriculture"    = "Agriculture",
    "Mining"         = "Exploitation minière",
    "Urban"          = "Urbain",
    "Others"         = "Autres",
    "Water"          = "Eau"
  ),
  es = c(
    "Undisturbed TF" = "Bosque de tierra firme intacto",
    "Degraded TF"    = "Bosque de tierra firme degradado",
    "Regrowth TF"    = "Bosque de tierra firme en regeneración",
    "Undisturbed FF" = "Bosque inundable intacto",
    "Degraded FF"    = "Bosque inundable degradado",
    "Regrowth FF"    = "Bosque inundable en regeneración",
    "Natural NF"     = "No bosque natural",
    "Pasture"        = "Pastoreo",
    "Agriculture"    = "Agricultura",
    "Mining"         = "Minería",
    "Urban"          = "Urbano",
    "Others"         = "Otros",
    "Water"          = "Agua"
  ),
  pt = c(
    "Undisturbed TF" = "Floresta de terra firme não perturbada",
    "Degraded TF"    = "Floresta de terra firme degradada",
    "Regrowth TF"    = "Floresta de terra firme em regeneração",
    "Undisturbed FF" = "Floresta inundável não perturbada",
    "Degraded FF"    = "Floresta inundável degradada",
    "Regrowth FF"    = "Floresta inundável em regeneração",
    "Natural NF"     = "Não-floresta natural",
    "Pasture"        = "Pastagem",
    "Agriculture"    = "Agricultura",
    "Mining"         = "Mineração",
    "Urban"          = "Urbano",
    "Others"         = "Outros",
    "Water"          = "Água"
  ),
  en = c(
    "Undisturbed TF" = "Undisturbed upland forest",
    "Degraded TF"    = "Degraded upland forest",
    "Regrowth TF"    = "Regenerating upland forest",
    "Undisturbed FF" = "Undisturbed flooded forest",
    "Degraded FF"    = "Degraded flooded forest",
    "Regrowth FF"    = "Regenerating flooded forest",
    "Natural NF"     = "Natural non-forest",
    "Pasture"        = "Pasture",
    "Agriculture"    = "Agriculture",
    "Mining"         = "Mining",
    "Urban"          = "Urban",
    "Others"         = "Others",
    "Water"          = "Water"
  )
)
tmfmb_labels_for <- function(lang) TMFMB_CLASS_I18N[[lang]]


## 1.4 MapBiomas palette & class order ----
# ------------------------------------------------------------------------- - - -
# Colors adapted from MapBiomas (Collection 6) for the aggregated groups you showed.
tmfmb_palette <- c(
  "Undisturbed TF"    = "#006400", # verde escuro
  "Degraded TF"       = "#6B8E23", # verde oliva
  "Regrowth TF"       = "#b3dc21ff", # verde vivo
  "Undisturbed FF"    = "#708090", # cinza-azulado (slate gray)
  "Degraded FF"       = "#556B2F", # verde oliva escuro / marrom
  "Regrowth FF"       = "#32CD32", # verde lima
  "Natural NF"        = "#10DFC7", # verde floresta
  "Pasture"           = "#FFE4B5", # mocassim / amarelo claro
  "Agriculture"       = "#FFD700", # dourado (substitui Mosaic of uses)
  "Mining"            = "#B22222", # vermelho tijolo
  "Urban"             = "#FF0000", # vermelho puro
  "Others"            = "#b3881c82", # dourado escuro
  "Water"             = "#5FAFFF"  # azul
)

tmfmb_order <- c(
  "Undisturbed TF", "Degraded TF", "Regrowth TF",
  "Undisturbed FF", "Degraded FF", "Regrowth FF",
  "Natural NF", "Pasture", "Agriculture", "Mining", "Urban", "Others", "Water"
)

## 1.5 Theme ----
# ------------------------------------------------------------------------- - - -
theme_sankey <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle     = element_text(hjust = 0.5, size = 12, face = "italic", margin = margin(t = 0, b = 12)),
      axis.title.x      = element_blank(),
      axis.text.x       = element_text(size = 12),
      axis.title.y      = element_text(size = 12, margin = margin(r = 12)),
      axis.ticks        = element_blank(),
      panel.grid        = element_blank(),
      panel.border      = element_blank(),
      legend.position   = "top",
      legend.direction  = "horizontal",
      legend.title      = element_blank(),
      legend.box        = "horizontal",
      legend.background = element_blank(),
      legend.key        = element_rect(fill = NA, colour = NA),  # no stroke
      legend.key.height = unit(0.6, "lines"),
      legend.key.width  = unit(3, "lines"),
      legend.margin     = margin(b = 6),
      plot.margin       = margin(12, 12, 12, 12),
      plot.caption      = element_text(hjust = 1, size = 10, color = "gray30", margin = margin(t = 12))
    )
}

## 1.6 Y axis helper (tight, non-scientific) ----
# ------------------------------------------------------------------------- - - -
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

## 2.1 Robust column detection (year, area_ha, src_label, dst_label) ----
# --------------------------------------------------------- - - -
detect_columns_generic <- function(df) {
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

  # src/dst labels
  src_label_idx <- which(str_detect(nms_lc, "^(src|source).*(label)$|^(src_label)$"))
  dst_label_idx <- which(str_detect(nms_lc, "^(dst|dest|target).*(label)$|^(dst_label)$"))

  if (length(src_label_idx) == 0) src_label_idx <- which(nms_lc %in% c("src","source","from"))
  if (length(dst_label_idx) == 0) dst_label_idx <- which(nms_lc %in% c("dst","dest","to","target"))

  if (length(src_label_idx) == 0 || length(dst_label_idx) == 0) {
    abort(glue("Source/Target label columns not found. Need src_label & dst_label (or src/dst). Available: {paste(nms, collapse=', ')}"))
  }

  list(
    year      = year_col,
    area_ha   = area_col,
    src_label = nms[src_label_idx[1]],
    dst_label = nms[dst_label_idx[1]]
  )
}

## 2.2 Build 3-stage long table for ggalluvial ----
# --------------------------------------------------------- - - -
build_alluvial_long_3 <- function(d01, d12, class_order, min_prop_mid = 0, topk_per_mid = Inf, keep_classes = NULL) {

  # Aggregate transitions by src->dst for each period
  d01_agg <- d01 %>%
    group_by(src, dst) %>%
    summarise(area01 = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(mid = dst)

  d12_agg <- d12 %>%
    group_by(src, dst) %>%
    summarise(area12 = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    rename(mid = src)

  # Keep only mid classes present in both periods
  mids <- intersect(unique(d01_agg$mid), unique(d12_agg$mid))
  d01_clean <- d01_agg %>% filter(mid %in% mids)
  d12_clean <- d12_agg %>% filter(mid %in% mids)
  if (nrow(d01_clean) == 0L || nrow(d12_clean) == 0L) {
    abort("No overlapping middle classes between periods (T0→T1 and T1→T2).")
  }

# Base proportions (no filtering)
prop_d12_base <- d12_clean %>%
  group_by(mid, dst) %>%
  summarise(area12 = sum(area12, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(mid) %>%
  mutate(total_mid_out = sum(area12, na.rm = TRUE),
         prop = if_else(total_mid_out > 0, area12 / total_mid_out, 0),
         rank = dplyr::dense_rank(desc(area12))) %>%
  ungroup()

# Apply filters but always keep flagged classes
prop_d12 <- prop_d12_base %>%
  filter(prop >= min_prop_mid | rank <= topk_per_mid |
           dst %in% keep_classes | mid %in% keep_classes) %>%
  select(mid, dst, prop)


  # Distribute each src→mid flow to its destinations using the proportions
  flows <- d01_clean %>%
    inner_join(prop_d12, by = "mid", relationship = "many-to-many") %>%
    mutate(area_ha = area01 * prop) %>%
    filter(area_ha > 0)

  # Long format for ggalluvial
  long <- flows %>%
    transmute(
      src_T0  = src,
      mid     = mid,
      dst_T2  = dst,
      area_ha = area_ha,
      flow_id = interaction(src, mid, dst, drop = TRUE, lex.order = TRUE)
    ) %>%
    pivot_longer(c(src_T0, mid, dst_T2), names_to = "stage", values_to = "class") %>%
    mutate(
      class = factor(class, levels = class_order),
      stage = factor(stage, levels = c("src_T0","mid","dst_T2"))
    )

  long
}

## 2.3 Build 4-stage long table for ggalluvial ----
# --------------------------------------------------------- - - -
build_alluvial_long_4 <- function(d01, d12, d23, class_order,
                                  min_prop_mid1 = 0, min_prop_mid2 = 0,
                                  topk_mid1 = Inf, topk_mid2 = Inf,
                                  keep_classes = NULL) {

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
    abort("No overlap across mid1/mid2 between the three periods (T0→T1→T2→T3).")

  # en: top-k per mid1
  p12_base <- d12a %>%
    group_by(mid1, mid2) %>%
    summarise(area12 = sum(area12, na.rm = TRUE), .groups = "drop_last") %>%
    group_by(mid1) %>%
    mutate(total = sum(area12, na.rm = TRUE),
          prop12 = if_else(total > 0, area12/total, 0),
          rank = dplyr::dense_rank(desc(area12))) %>%
    ungroup()

  p12 <- p12_base %>%
    filter(prop12 >= min_prop_mid1 | rank <= topk_mid1 |
            mid1 %in% keep_classes | mid2 %in% keep_classes) %>%
    select(mid1, mid2, prop12)

  # en: top-k per mid2
  p23_base <- d23a %>%
    group_by(mid2, dst) %>%
    summarise(area23 = sum(area23, na.rm = TRUE), .groups = "drop_last") %>%
    group_by(mid2) %>%
    mutate(total = sum(area23, na.rm = TRUE),
          prop23 = if_else(total > 0, area23/total, 0),
          rank = dplyr::dense_rank(desc(area23))) %>%
    ungroup()

  p23 <- p23_base %>%
    filter(prop23 >= min_prop_mid2 | rank <= topk_mid2 |
            mid2 %in% keep_classes | dst %in% keep_classes) %>%
    select(mid2, dst, prop23)

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
    pivot_longer(c(src_T0, mid1, mid2, dst_T3), names_to = "stage", values_to = "class") %>%
    mutate(
      class = factor(class, levels = class_order),
      stage = factor(stage, levels = c("src_T0","mid1","mid2","dst_T3"))
    )

  long
}

## 2.4 Plot helper ----
# --------------------------------------------------------- - - -
plot_sankey_mb <- function(long_df, territory, lang = "pt") {

  title_txt <- label("title_mb", territory = TERRITORY_LABELS[[territory]])

  # Build subtitle from the stage labels set in make_sankey_mb()
  stage_labels <- levels(long_df$stage)
  subtitle_txt <- label("subtitle_period", period = paste(stage_labels, collapse = " \u2192 "))

  cap_txt <- LABELS$caption_mb[[lang]]

  # y-max per stage to create a tight y-axis
  totals <- long_df %>% group_by(stage) %>% summarise(total = sum(area_ha, na.rm = TRUE), .groups = "drop")
  ymax <- max(totals$total, na.rm = TRUE)

  # sanity-check: every present class must have a color in the palette
  present_classes <- intersect(tmfmb_order, levels(droplevels(long_df$class)))
  missing_cols <- setdiff(present_classes, names(tmfmb_palette))

  if (length(missing_cols)) {
    abort(glue::glue(
      "Palette missing colors for: {paste(missing_cols, collapse=', ')}. ",
      "Add them to tmfmb_palette or remove from KEEP_CLASSES/DROP_CLASSES."
    ))
  }

  # Legend rows (wrap if too many classes)
  n_cols_legend <- LEGEND_NCOL                            
  n_rows_legend <- ceiling(length(present_classes) / n_cols_legend)

  ggplot(long_df,
         aes(x = stage, stratum = class, alluvium = flow_id,
             y = area_ha, fill = class, label = class)) +
    # English: smoother curves, fewer crossings, and a tiny outline for definition
    geom_flow(
      stat = "alluvium",
      lwd = 0.25,                    # thin outline improves definition
      color = scales::alpha("white", 0.25),
      alpha = FLOW_ALPHA,
      curve_type = "cubic",
      knot.pos = 0.35,              # slightly earlier bend (less wobble)
      lode.guidance = "forward"   # ggalluvial tip to reduce crossings
    ) +
    geom_stratum(
      width = STRATUM_WIDTH,
      color = "white", size = 0.3,   # subtle separator between blocks
      alpha = STRATUM_ALPHA
    ) + 
    { if (isTRUE(STRATUM_LABELS)) 
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              size = 3, color = "black", alpha = 0.8, vjust = 0.5) 
      else NULL } +
    scale_fill_manual(
      values = tmfmb_palette[present_classes],
      breaks = present_classes,
      labels = tmfmb_labels_for(lang)[present_classes],
      guide  = guide_legend(
        nrow = n_rows_legend, byrow = TRUE,
        keywidth  = unit(1.2, "lines"),
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

# 2.5 Keep flow functions ----
# --------------------------------------------------------- - - -
# Ensure any flow touching a kept class is present even after floors
restore_kept_flows <- function(df_raw, df_filtered, keep, drop = NULL) {
  if (is.null(keep) || !length(keep)) return(df_filtered)
  add <- df_raw %>% dplyr::filter(src %in% keep | dst %in% keep)
  if (!is.null(drop) && length(drop)) {
    add <- add %>% dplyr::filter(!src %in% drop, !dst %in% drop)
  }
  dplyr::bind_rows(df_filtered, add) %>%
    dplyr::group_by(src, dst) %>%
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")
}

##%###########################################################################%##
#                                                                               #
#                            3) Main Function                                ----
#                                                                               #
##%###########################################################################%##

make_sankey_mb <- function(
  territory,
  lang          = c("fr","pt","es","en"),
  T0, T1, T2, T3 = NULL,
  n_stages      = N_STAGES,
  stayers       = STAYERS_DEFAULT,
  width_mm      = FIG_WIDTH_MM,
  height_mm     = FIG_HEIGHT_MM,
  out_dir       = "results/indicators",
  csv_long_path = NULL,
  drop_classes  = DROP_CLASSES,
  keep_classes  = KEEP_CLASSES,
  show_all      = SHOW_ALL 
) {
  lang <- match.arg(lang)
  n_stages <- as.integer(n_stages)

  # en: overrides for territory
  ov   <- TERRITORY_OVERRIDES[[territory]]
  if (is.null(ov)) ov <- list()
  min_ha_loc     <- if (!is.null(ov$min_ha)) ov$min_ha else MIN_FLOW_HA
  min_share_loc  <- if (!is.null(ov$min_share)) ov$min_share else MIN_FLOW_SHARE
  min_prop_mid_loc <- if (!is.null(ov$min_prop_mid)) ov$min_prop_mid else MIN_PROP_PER_MID
  topk_loc       <- if (!is.null(ov$topk)) ov$topk else TOPK_PER_MID

  # Show all flows, no filtering 
    if (isTRUE(show_all)) {
    min_ha_loc       <- 0
    min_share_loc    <- 0
    min_prop_mid_loc <- 0
    topk_loc         <- Inf
    drop_classes     <- NULL
  }

  # Validate breakpoints against MapBiomas window
  if (n_stages == 4L) {
    if (is.null(T3)) abort("n_stages=4 requires T3.")
    if (!(MB_YEAR_MIN <= T0 && T0 < T1 && T1 < T2 && T2 < T3 && T3 <= MB_YEAR_MAX)) {
      abort(glue("Invalid breakpoints: require {MB_YEAR_MIN} <= T0 < T1 < T2 < T3 <= {MB_YEAR_MAX}."))
    }
  } else {
    if (!(MB_YEAR_MIN <= T0 && T0 < T1 && T1 < T2 && T2 <= MB_YEAR_MAX)) {
      abort(glue("Invalid breakpoints: require {MB_YEAR_MIN} <= T0 < T1 < T2 <= {MB_YEAR_MAX}."))
    }
  }

  # Locate CSV if not provided (expects '*_mb_*transition*.csv')
  if (is.null(csv_long_path)) {
    input_dir <- file.path("results/metrics", territory)
    cand <- list.files(
      input_dir, full.names = TRUE, ignore.case = TRUE,
      pattern = glue("^{territory}.*mb.*transition.*\\.csv$")
    )
    if (length(cand) == 0) abort(glue("No MapBiomas transition CSV found in {input_dir}."))
    csv_long_path <- cand[1]
  }

  message(glue("📊 Loading: {basename(csv_long_path)}"))
  df <- suppressMessages(readr::read_csv(csv_long_path, show_col_types = FALSE))
  cols <- detect_columns_generic(df)

  df_clean <- df %>%
    transmute(
      year     = as.integer(.data[[cols$year]]),
      area_ha  = suppressWarnings(as.numeric(.data[[cols$area_ha]])),
      src      = as.character(.data[[cols$src_label]]),
      dst      = as.character(.data[[cols$dst_label]])
    ) %>%
    filter(!is.na(year), !is.na(area_ha), area_ha > 0)

  # Enforce MB window
  df_clean <- df_clean %>% filter(year >= MB_YEAR_MIN, year <= MB_YEAR_MAX)

  # APPLY CLASS GROUPING (to the aggregated TMF+MB classes)
  df_clean <- df_clean %>%
    mutate(
      src = str_trim(src),
      dst = str_trim(dst),
      src = if_else(src %in% names(tmfmb_palette), src, "Others"),
      dst = if_else(dst %in% names(tmfmb_palette), dst, "Others")
    ) 
  message("Unique src after cleaning: ", paste(unique(df_clean$src), collapse=", "))

    ## 3.1) Drop unwanted classes (e.g., Water) ----
  if (!is.null(drop_classes)) {
    df_clean <- df_clean %>%
      dplyr::filter(!src %in% drop_classes, !dst %in% drop_classes)
    message("⚠ Dropped classes: ", paste(drop_classes, collapse = ", "))
  }
  
  # Build raw period tables (before any reclass/floor) for later restoration
  d01_raw <- df_clean %>% filter(year > T0, year <= T1) %>% select(src, dst, area_ha)
  d12_raw <- df_clean %>% filter(year > T1, year <= T2) %>% select(src, dst, area_ha)
  if (n_stages == 4L) {
    d23_raw <- df_clean %>% filter(year > T2, year <= T3) %>% select(src, dst, area_ha)
    if (nrow(d23_raw) == 0L) abort(glue("No transition data found for period {T2}-{T3}."))
  }

  # Work copies
  d01 <- d01_raw; d12 <- d12_raw; if (n_stages == 4L) d23 <- d23_raw
  
  ## 3.2) Reclassify Pasture→Regrowth TF when dst is forest -----
  reclass_pasture_to_regrowth <- function(df) {
    df %>%
      dplyr::mutate(
        dst = dplyr::if_else(
          src == "Pasture" & dst %in% c("Undisturbed TF","Degraded TF"),
          "Regrowth TF", dst
        )
      )
  }
  d01 <- reclass_pasture_to_regrowth(d01)
  d12 <- reclass_pasture_to_regrowth(d12)
  if (n_stages == 4L) d23 <- reclass_pasture_to_regrowth(d23)


  if (nrow(d01) == 0L) abort(glue("No transition data found for period {T0}-{T1}."))
  if (nrow(d12) == 0L) abort(glue("No transition data found for period {T1}-{T2}."))
  message(glue("📈 Period {T0}-{T1}: {nrow(d01)} transition records"))
  message(glue("📈 Period {T1}-{T2}: {nrow(d12)} transition records"))
  if (n_stages == 4L) message(glue("📈 Period {T2}-{T3}: {nrow(d23)} transition records"))

  ## 3.3) Remove stayers if requested -----
  if (!stayers) {
    d01 <- d01 %>% dplyr::filter(src != dst)
    d12 <- d12 %>% dplyr::filter(src != dst)
    if (n_stages == 4L) d23 <- d23 %>% dplyr::filter(src != dst)
  }

  ## 3.4) Minimum flow area per period (absolute or relative) ----
  stage_floor <- function(df_stage, share, abs_floor) {
    if (isTRUE(show_all)) return(df_stage)
    tot <- sum(df_stage$area_ha, na.rm = TRUE)
    piso <- max(abs_floor, share * tot)
    dplyr::filter(df_stage, area_ha >= piso)
  }

  # Apply floors
  d01 <- stage_floor(d01, min_share_loc, min_ha_loc)
  d12 <- stage_floor(d12, min_share_loc, min_ha_loc)
  if (n_stages == 4L) d23 <- stage_floor(d23, min_share_loc, min_ha_loc)

  ## 3.5) Restore kept classes (never drop due to floors) ----
  d01 <- restore_kept_flows(d01_raw, d01, keep_classes, drop_classes)
  d12 <- restore_kept_flows(d12_raw, d12, keep_classes, drop_classes)
  if (n_stages == 4L) d23 <- restore_kept_flows(d23_raw, d23, keep_classes, drop_classes)

 
  ## 3.6) Build tables (4 and 3 stages) ----
  if (n_stages == 4L) {
    long <- build_alluvial_long_4(
      d01, d12, d23, tmfmb_order,
      min_prop_mid1 = min_prop_mid_loc, min_prop_mid2 = min_prop_mid_loc,
      topk_mid1 = topk_loc, topk_mid2 = topk_loc,
      keep_classes = keep_classes
    ) %>% 
      mutate(
        stage = factor(stage, levels = c("src_T0","mid1","mid2","dst_T3"),
                       labels = c(as.character(T0), as.character(T1),
                                  as.character(T2), as.character(T3))),
        class = fct_na_value_to_level(class, level = "Other")
      )
  } else {
    long <- build_alluvial_long_3(
      d01, d12, tmfmb_order,
      min_prop_mid = min_prop_mid_loc, topk_per_mid = topk_loc,
      keep_classes = keep_classes
    ) %>%
      mutate(
        stage = factor(stage, levels = c("src_T0","mid","dst_T2"),
                       labels = c(as.character(T0), as.character(T1), as.character(T2))),
        class = fct_na_value_to_level(class, level = "Other")
      )
  }

  if (nrow(long) == 0L) abort("No flows remain after filtering.")

  message(glue("✅ Generated {nrow(long)} alluvial records"))

  # 3.7)  Plotting and exporting -----
  p <- plot_sankey_mb(long, territory = territory, lang = lang)
  print(p)
  message(glue("✓ Sankey generated for {territory}"))

  # Export
  out_base <- file.path(out_dir, territory, glue("{territory}_{lang}"))
  if (!dir.exists(out_base)) dir.create(out_base, recursive = TRUE)
  # tag <- if (stayers) "withStayers" else "noStayers"
  stage_tag <- if (n_stages == 4L) "4stages" else "3stages"
  
  file_stub <- glue(FILENAME_SANKEY, territory = territory, stage_tag = stage_tag, lang = lang)
  
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
#                           4) Main Processing Loop                           ----
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
    csv_path <- list.files(
      file.path("results/metrics", TERRITORY),
      pattern    = glue("^{TERRITORY}.*tmf_mb.*transition.*\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(csv_path) == 0) {
      message(glue("⚠ No MapBiomas transition CSV found under results/metrics/{TERRITORY} — skipping."))
      next
    }
    csv_path <- csv_path[1]
    message(glue("📄 Using CSV: {basename(csv_path)}"))

    # Single render
    try({
      message(glue("🧩 Variant: {if (STAYERS_DEFAULT) 'withStayers' else 'noStayers'}"))
      if (N_STAGES == 4L) {
        if (!"T3" %in% names(brks)) {
          message(glue("⚠ N_STAGES=4 but T3 missing in BREAKS for {TERRITORY} — skipping."))
        } else {
          make_sankey_mb(
            territory     = TERRITORY,
            lang          = LANG,
            T0 = brks["T0"], T1 = brks["T1"], T2 = brks["T2"], T3 = brks["T3"],
            n_stages      = 4L,
            stayers       = STAYERS_DEFAULT,
            csv_long_path = csv_path
          )
        }
      } else {
        make_sankey_mb(
          territory     = TERRITORY,
          lang          = LANG,
          T0 = brks["T0"], T1 = brks["T1"], T2 = brks["T2"],
          n_stages      = 3L,
          stayers       = STAYERS_DEFAULT,
          csv_long_path = csv_path
        )
      }
    }, silent = FALSE)

    cat("\n", paste(rep("-", 64), collapse=""), "\n", sep = "")
  }
}
