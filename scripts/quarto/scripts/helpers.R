suppressPackageStartupMessages({
  library(glue)
  library(yaml)
  library(knitr)
  library(readr)
})

DEFAULT_TERRITORIES <- c("cotriguacu", "paragominas", "guaviare", "madre_de_dios")
DEFAULT_LANG_FALLBACK <- "fr"

load_translation_bundle <- function(lang,
                                    path = file.path("config", "translations.yml"),
                                    fallback = DEFAULT_LANG_FALLBACK) {
  dict <- yaml::read_yaml(path)
  list(dict = dict, lang = lang, fallback = fallback)
}

tr <- function(bundle, ...) {
  dict <- bundle$dict
  lang <- bundle$lang
  fallback <- bundle$fallback
  keys <- vapply(list(...), as.character, character(1))

  node <- dict
  for (key in keys) {
    if (is.null(node[[key]])) {
      return("")
    }
    node <- node[[key]]
  }

  resolve_locale_value(node, lang, fallback)
}

ensure_vector <- function(x) {
  if (is.null(x)) character(0)
  else if (is.list(x)) unlist(x, use.names = FALSE)
  else as.character(x)
}

territory_label <- function(bundle, territory) {
  label <- tr(bundle, "territories", territory, "label")
  if (!nzchar(label)) territory else label
}

figure_path <- function(territory, lang, filename) {
  file.path("..", "..", "results", "indicators", territory, glue("{territory}_{lang}"), filename)
}

write_heading <- function(level, text) {
  hashes <- paste(rep("#", level), collapse = "")
  cat(glue("{hashes} {text}"), "\n\n", sep = "")
}

page_break <- function() {
  cat("\\pagebreak", "\n\n", sep = "")
}

render_figure <- function(path, bundle = NULL, scale = 1.25) {
  if (!file.exists(path)) {
    msg <- if (!is.null(bundle)) tr(bundle, "phrases", "missing_data") else "Data missing."
    cat(msg, "\n\n", sep = "")
    return()
  }
  width_percent <- sprintf("%.0f%%", scale * 100)
  norm <- gsub("\\\\", "/", path)
  
  cat("::: {custom-style=\"Image\"}\n", sep = "")
  cat(glue("![]({norm}){{width={width_percent}}}"), "\n", sep = "")
  cat(":::\n\n", sep = "")
}

resolve_locale_value <- function(node, lang, fallback) {
  if (is.character(node) && length(node) == 1) {
    return(node)
  }

  if (is.list(node)) {
    candidate <- node[[lang]]
    if (is.character(candidate) && nzchar(candidate)) {
      return(candidate)
    }

    fallback_candidate <- node[[fallback]]
    if (is.character(fallback_candidate) && nzchar(fallback_candidate)) {
      return(fallback_candidate)
    }

    flattened <- unlist(node, use.names = FALSE)
    non_empty <- flattened[nzchar(flattened)]
    if (length(non_empty) > 0) {
      return(non_empty[[1]])
    }
  }

  ""
}

metric_path <- function(territory, suffix) {
  file.path("..", "..", "results", "metrics", territory, "derived", glue("{territory}_{suffix}.csv"))
}

read_metric_csv <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

load_deforestation_metrics <- function(territory) {
  list(
    overall = read_metric_csv(metric_path(territory, "deforestation_overall_metrics")),
    yearly = read_metric_csv(metric_path(territory, "deforestation_yearly_metrics"))
  )
}
