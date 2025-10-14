##%###########################################################################%##
#                                                                               #
#            TMF + MapBiomas Transitions ? SankeyMATIC Text Generator        ----#
#                                                                               #
##%###########################################################################%##

# 1) Configuration ----
# ------------------------------------------------------------------------- - - -
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(glue)
  library(purrr)
})

LANGS        <- c("fr")
# LANGS        <- c("fr", "es", "pt", "en")
OUT_DIR      <- "results/indicators"
METRICS_DIR  <- "results/metrics"
FILENAME_TXT <- "{territory}_sankeymatic_{lang}.txt"

## 1.1 Territories & stage years ----
STAGE_YEARS <- list(
  cotriguacu    = c(1991L, 2008L, 2024L),
  paragominas   = c(1991L, 2008L, 2024L),
  guaviare      = c(1991L, 2016L, 2024L),
  madre_de_dios = c(1991L, 2010L, 2024L)
)

TERRITORIES <- names(STAGE_YEARS)

TERRITORY_LABELS <- c(
  cotriguacu    = "Cotriguacu",
  paragominas   = "Paragominas",
  guaviare      = "Guaviare",
  madre_de_dios = "Madre de Dios"
)

## 1.2 Class labels (multilingual) ----
CLASS_LABELS <- list(
  fr = c(
    "Undisturbed"              = "Forêt intacte",
    "Degraded"                 = "Forêt dégradée",
    "Regrowth"                 = "Forêt en régénération",
    "Undisturbed TF"           = "Forêt de terre ferme intacte",
    "Degraded TF"              = "Forêt de terre ferme dégradée",
    "Regrowth TF"              = "Forêt de terre ferme en régénération",
    "Undisturbed FF"           = "Forêt inondée intacte",
    "Degraded FF"              = "Forêt inondée dégradée",
    "Regrowth FF"              = "Forêt inondée en régénération",
    "Other natural vegetation" = "Autres formations naturelles",
    "Agriculture"              = "Agriculture",
    "Pasture"                  = "Pâturage",
    "Mining"                   = "Extraction minière",
    "Other LULC"               = "Autres occupations du sol",
    "Mosaic of uses"           = "Mosaïque d'usages"
  ),
  es = c(
    "Undisturbed"              = "Bosque intacto",
    "Degraded"                 = "Bosque degradado",
    "Regrowth"                 = "Bosque en regeneración",
    "Undisturbed TF"           = "Bosque de tierra firme intacto",
    "Degraded TF"              = "Bosque de tierra firme degradado",
    "Regrowth TF"              = "Bosque de tierra firme en regeneración",
    "Undisturbed FF"           = "Bosque inundable intacto",
    "Degraded FF"              = "Bosque inundable degradado",
    "Regrowth FF"              = "Bosque inundable en regeneración",
    "Other natural vegetation" = "Otra vegetación natural",
    "Agriculture"              = "Agricultura",
    "Pasture"                  = "Pastoreo",
    "Mining"                   = "Minería",
    "Other LULC"               = "Otros usos del suelo",
    "Mosaic of uses"           = "Mosaico de usos"
  ),
  pt = c(
    "Undisturbed"              = "Floresta intacta",
    "Degraded"                 = "Floresta degradada",
    "Regrowth"                 = "Floresta em regeneração",
    "Undisturbed TF"           = "Floresta de terra firme intacta",
    "Degraded TF"              = "Floresta de terra firme degradada",
    "Regrowth TF"              = "Floresta de terra firme em regeneração",
    "Undisturbed FF"           = "Floresta inundável intacta",
    "Degraded FF"              = "Floresta inundável degradada",
    "Regrowth FF"              = "Floresta inundável em regeneração",
    "Other natural vegetation" = "Outra vegetação natural",
    "Agriculture"              = "Agricultura",
    "Pasture"                  = "Pastagem",
    "Mining"                   = "Mineração",
    "Other LULC"               = "Outros usos do solo",
    "Mosaic of uses"           = "Mosaico de usos"
  ),
  en = c(
    "Undisturbed"              = "Undisturbed forest",
    "Degraded"                 = "Degraded forest",
    "Regrowth"                 = "Forest regrowth",
    "Undisturbed TF"           = "Undisturbed terra firme forest",
    "Degraded TF"              = "Degraded terra firme forest",
    "Regrowth TF"              = "Terra firme forest regrowth",
    "Undisturbed FF"           = "Undisturbed flooded forest",
    "Degraded FF"              = "Degraded flooded forest",
    "Regrowth FF"              = "Flooded forest regrowth",
    "Other natural vegetation" = "Other natural vegetation",
    "Agriculture"              = "Agriculture",
    "Pasture"                  = "Pasture",
    "Mining"                   = "Mining",
    "Other LULC"               = "Other land-use types",
    "Mosaic of uses"           = "Mosaic of uses"
  )
)

## 1.3 Forest and ordering rules ----
FOREST_CLASSES <- list(
  cotriguacu    = c("Undisturbed", "Degraded", "Regrowth"),
  paragominas   = c("Undisturbed", "Degraded", "Regrowth"),
  guaviare      = c("Undisturbed TF", "Degraded TF", "Regrowth TF",
                    "Undisturbed FF", "Degraded FF", "Regrowth FF"),
  madre_de_dios = c("Undisturbed TF", "Degraded TF", "Regrowth TF",
                    "Undisturbed FF", "Degraded FF", "Regrowth FF")
)

SOURCE_ORDER <- list(
  cotriguacu    = c("Undisturbed", "Degraded", "Regrowth"),
  paragominas   = c("Undisturbed", "Degraded", "Regrowth"),
  guaviare      = c("Undisturbed TF", "Undisturbed FF",
                    "Degraded TF", "Degraded FF",
                    "Regrowth TF", "Regrowth FF"),
  madre_de_dios = c("Undisturbed TF", "Undisturbed FF",
                    "Degraded TF", "Degraded FF",
                    "Regrowth TF", "Regrowth FF")
)

CLASS_COLORS <- c(
  "Undisturbed"              = "#09410d",
  "Degraded"                 = "#6C8E22",
  "Regrowth"                 = "#64de2c",
  "Undisturbed TF"           = "#09410d",
  "Degraded TF"              = "#6C8E22",
  "Regrowth TF"              = "#64de2c",
  "Undisturbed FF"           = "#005E5D",
  "Degraded FF"              = "#1AA49C",
  "Regrowth FF"              = "#6FE7D2",
  "Other natural vegetation" = "#C49A6C",
  "Agriculture"              = "#d340e3",
  "Pasture"                  = "#F2C14E",
  "Mosaic of uses"           = "#F77976",
  "Mining"                   = "#A04942",
  "Other LULC"               = "#7E5AA5"
)


DEST_ORDER <- list(
  cotriguacu    = c("Undisturbed", "Degraded",
                    "Pasture", "Agriculture", "Mosaic of uses",
                    "Other natural vegetation", "Other LULC",
                    "Regrowth"),
  paragominas   = c("Undisturbed", "Degraded",
                    "Pasture", "Agriculture", "Mosaic of uses",
                    "Other natural vegetation", "Other LULC",
                    "Regrowth"),
  guaviare      = c("Undisturbed TF", "Undisturbed FF",
                    "Degraded TF", "Degraded FF",
                    "Pasture", "Agriculture", "Mosaic of uses",
                    "Other natural vegetation", "Other LULC",
                    "Regrowth TF", "Regrowth FF"),
  madre_de_dios = c("Undisturbed TF", "Undisturbed FF",
                    "Degraded TF", "Degraded FF",
                    "Pasture", "Agriculture", "Mosaic of uses", "Mining",
                    "Other natural vegetation", "Other LULC",
                    "Regrowth TF", "Regrowth FF")
)

# 2) Helper functions ----
# ------------------------------------------------------------------------- - - -
# 1  -> Undisturbed TF
# 2  -> Degraded TF
# 3  -> Regrowth TF
# 4  -> Undisturbed FF
# 5  -> Degraded FF
# 6  -> Regrowth FF
# 7  -> Other natural vegetation
# 8  -> Agriculture
# 9  -> Pasture
# 10 -> Mining
# 11 -> Other LULC
# 12 -> Mosaic of uses

## 2.1 Function to map class codes to class keys in each territoriy ----
# ------------------------------------------------------------------------- - - -
map_class_key <- function(code, territory) {
  code <- as.integer(code)
  if (territory %in% c("cotriguacu", "paragominas")) {
    dplyr::case_when(
      code %in% c(1L, 4L) ~ "Undisturbed",
      code %in% c(2L, 5L) ~ "Degraded",
      code %in% c(3L, 6L) ~ "Regrowth",
      code == 7L          ~ "Other natural vegetation",
      code == 8L          ~ "Agriculture",
      code == 9L          ~ "Pasture",
      code == 10L         ~ "Other LULC",
      code == 11L         ~ "Other LULC",
      code == 12L         ~ "Mosaic of uses",
      TRUE                ~ "Other LULC"
    )
  } else if (territory == "guaviare") {
    dplyr::case_when(
      code == 1L ~ "Undisturbed TF",
      code == 2L ~ "Degraded TF",
      code == 3L ~ "Regrowth TF",
      code == 4L ~ "Undisturbed FF",
      code == 5L ~ "Degraded FF",
      code == 6L ~ "Regrowth FF",
      code == 7L ~ "Other natural vegetation",
      code == 8L ~ "Agriculture",
      code == 9L ~ "Pasture",
      code == 10L ~ "Other LULC",
      code == 11L ~ "Other LULC",
      code == 12L ~ "Mosaic of uses",
      TRUE        ~ "Other LULC"
    )
  } else if (territory == "madre_de_dios") {
    dplyr::case_when(
      code == 1L ~ "Undisturbed TF",
      code == 2L ~ "Degraded TF",
      code == 3L ~ "Regrowth TF",
      code == 4L ~ "Undisturbed FF",
      code == 5L ~ "Degraded FF",
      code == 6L ~ "Regrowth FF",
      code == 7L ~ "Other natural vegetation",
      code == 8L ~ "Agriculture",
      code == 9L ~ "Pasture",
      code == 10L ~ "Mining",
      code == 11L ~ "Other LULC",
      code == 12L ~ "Mosaic of uses",
      TRUE        ~ "Other LULC"
    )
  } else {
    stop(glue("Unsupported territory: {territory}"))
  }
}

# 2.2 Functions to format value and summarise ----
# ------------------------------------------------------------------------- - - -
format_value <- function(x) {
  formatC(round(x, 0), format = "f", digits = 0, big.mark = "", decimal.mark = ".")
}

summarise_period <- function(df, territory, year_start, year_end, forest_only = TRUE) {
  if (year_end <= year_start) stop("year_end must be greater than year_start.")
  forest_keys <- FOREST_CLASSES[[territory]]
  if (is.null(forest_keys)) stop(glue("Missing forest class definition for '{territory}'"))

  df %>%
    filter(.data$year_from == year_start, .data$year_to == year_end) %>%
    mutate(
      src_class = map_class_key(src, territory),
      dst_class = map_class_key(dst, territory)
    ) %>%
    { if (isTRUE(forest_only)) dplyr::filter(., src_class %in% forest_keys) else . } %>%
    group_by(src_class, dst_class) %>%
    summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    filter(area_ha > 0)
}

# 2.3 Functions to build and write sankey lines ----
# ------------------------------------------------------------------------- - - -
write_sankey_file <- function(lines, territory, lang) {
  out_dir <- file.path(OUT_DIR, territory, glue("{territory}_{lang}"))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  out_path <- file.path(out_dir, glue(FILENAME_TXT, territory = territory, lang = lang))
  writeLines(lines, con = out_path, useBytes = TRUE)
  message(glue("✓ SankeyMATIC text saved to {out_path}"))
}


build_sankey_lines <- function(df, territory, lang) {
  stages <- STAGE_YEARS[[territory]]
  class_labels <- CLASS_LABELS[[lang]]
  if (is.null(class_labels)) stop(glue("Missing class labels for language '{lang}'"))

  header_line <- glue("// {TERRITORY_LABELS[[territory]]} • {toupper(lang)}")
  color_lines <- list(header_line, "// COLORS")
  defined_nodes <- character(0)
  data_lines <- list()

  for (i in seq_len(length(stages) - 1)) {
    year_from <- stages[i]
    year_to   <- stages[i + 1]

    flows <- summarise_period(
      df, territory, year_from, year_to,
      forest_only = (i == 1)
    )

    if (nrow(flows) == 0) {
      data_lines <- append(data_lines, list(glue("// {year_from} -> {year_to}: no data available"), ""))
      next
    }

    source_levels <- SOURCE_ORDER[[territory]]
    dest_levels   <- DEST_ORDER[[territory]]
    if (is.null(source_levels)) source_levels <- unique(flows$src_class)
    if (is.null(dest_levels))   dest_levels   <- unique(flows$dst_class)

    flows <- flows %>%
      mutate(
        src_order = factor(src_class, levels = c(source_levels, setdiff(src_class, source_levels)), ordered = TRUE),
        dst_order = factor(dst_class, levels = c(dest_levels, setdiff(dst_class, dest_levels)), ordered = TRUE)
      ) %>%
      arrange(src_order, dst_order, desc(area_ha)) %>%
      select(-src_order, -dst_order) %>%
      mutate(
        src_label = class_labels[src_class],
        dst_label = class_labels[dst_class],
        source = glue("{year_from} {src_label}"),
        target = glue("{year_to} {dst_label}"),
        value  = format_value(area_ha),
        line   = glue("{source} [{value}] {target}")
      )

    node_colors <- tibble::tibble(
      label = c(flows$source, flows$target),
      class = c(flows$src_class, flows$dst_class)
    ) %>%
      dplyr::distinct(label, .keep_all = TRUE)

    new_nodes <- setdiff(node_colors$label, defined_nodes)
    if (length(new_nodes)) {
      to_add <- node_colors[node_colors$label %in% new_nodes, ]
      cols <- CLASS_COLORS[to_add$class]
      if (any(is.na(cols))) {
        missing <- unique(to_add$class[is.na(cols)])
        stop(glue("Missing color for classes: {paste(missing, collapse=', ')}"))
      }
      color_lines <- append(color_lines, glue(":{to_add$label} {cols}"))
      defined_nodes <- c(defined_nodes, new_nodes)
    }

    data_lines <- append(data_lines, list(glue("// {year_from} -> {year_to}"), flows$line, ""))
  }

  lines <- unlist(c(color_lines, "", data_lines), use.names = FALSE)
  return(as.character(lines))
}

# 3) Main processing loop ----
# ------------------------------------------------------------------------- - - -
for (LANG in LANGS) {
  message(glue("🌐 Language: {toupper(LANG)}"))

  for (TERRITORY in TERRITORIES) {
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")
    cat(glue("PROCESSING: {toupper(TERRITORY)}"))
    cat("\n", paste(rep("=", 64), collapse = ""), "\n", sep = "")

    csv_path <- list.files(
      path       = file.path(METRICS_DIR, TERRITORY),
      pattern    = glue("^{TERRITORY}_tmf_mb_transitions_custom\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (!length(csv_path)) {
      message(glue("⚠ CSV not found for territory '{TERRITORY}' — skipping."))
      next
    }

    transitions <- read_csv(
      csv_path[1],
      show_col_types = FALSE,
      progress = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    ) %>%
      mutate(
        year_from = as.integer(year_from),
        year_to   = as.integer(year_to),
        src  = as.integer(src),
        dst  = as.integer(dst)
      )

    lines <- build_sankey_lines(transitions, TERRITORY, LANG)
    write_sankey_file(lines, TERRITORY, LANG)

    cat("\n", paste(rep("-", 64), collapse = ""), "\n", sep = "")
  }
}