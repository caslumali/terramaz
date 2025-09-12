###############################################################################
#                                                                             #
#     Inspect all CSVs: classifier fields & unique values (all territories) ----
#                                                                             #
###############################################################################

# 1. Packages ----
# -----------------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(readr)
library(fs)

# 2. Paths ----
# -----------------------------------------------------------------------------
metrics_root <- "results/metrics"  # parent folder containing territories
diag_dir     <- "results/diagnostics"

if (!dir_exists(diag_dir)) dir_create(diag_dir, recurse = TRUE)

# 3. Heuristics: which columns are "classifier" fields? ----
# -----------------------------------------------------------------------------
# We want only columns that drive palettes (categorical labels):
#   - character/factor columns AND name contains "label" or "class" or "source"
#   - explicit allowlist for common fields
#   - explicit denylist for known non-palette fields

allow_cols  <- c("src_label", "dst_label", "tmf_class_label", "source_used")
deny_cols   <- c("system:index", ".geo", "area_id", "pixel_lon", "pixel_lat",
                 "month", "year", "px_total")  # extend if needed

is_classifier_col <- function(name, vec) {
  if (name %in% deny_cols) return(FALSE)
  # only strings/factors
  if (!(is.character(vec) || is.factor(vec))) return(FALSE)
  # name-based heuristics or explicit allowlist
  has_keyword <- str_detect(name, "(label|class|source)")
  in_allow    <- name %in% allow_cols
  has_keyword || in_allow
}

# 4. Helpers ----
# -----------------------------------------------------------------------------
safe_read <- function(path) {
  suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
}

shorten <- function(x, n = 20) {
  x <- unique(x)
  x <- x[!is.na(x)]
  x <- sort(x)
  if (length(x) == 0) return(character())
  if (length(x) <= n) return(x)
  c(x[seq_len(n)], "...")  # preview only
}

territory_of <- function(path) {
  # results/metrics/<territory>/<file>.csv
  basename(dirname(path))
}

# 5. List all CSVs across territories ----
# -----------------------------------------------------------------------------
csv_files <- dir_ls(metrics_root, recurse = TRUE, glob = "*.csv")

if (length(csv_files) == 0) {
  stop("No CSV files found under: ", metrics_root)
}

# 6. Inspect each file ----
# -----------------------------------------------------------------------------
message("Scanning ", length(csv_files), " CSV files...")

per_file <- map_dfr(csv_files, function(f) {
  trr <- territory_of(f)
  df  <- safe_read(f)

  # find classifier columns present in this file
  cls_cols <- names(df)[map_lgl(names(df), ~ is_classifier_col(.x, df[[.x]]))]

  # unique values per classifier col (full + preview)
  uniques_list <- map(cls_cols, ~ sort(unique(df[[.x]])))
  preview_list <- map(uniques_list, shorten, n = 20)
  names(uniques_list) <- cls_cols
  names(preview_list) <- cls_cols

  tibble(
    territory   = trr,
    file        = path_file(f),
    classifier_fields       = list(cls_cols),
    unique_values           = list(uniques_list),   # full sets (per column)
    unique_values_preview   = list(preview_list)    # truncated for quick view
  )
})

# 7. Pretty print (console) ----
# -----------------------------------------------------------------------------
cat("\n================ SUMMARY BY FILE ================\n")
walk(seq_len(nrow(per_file)), function(i) {
  cat("\n-- ", per_file$territory[i], " / ", per_file$file[i], " --\n", sep = "")
  cols <- per_file$classifier_fields[[i]]
  if (length(cols) == 0) {
    cat("No classifier fields found.\n")
  } else {
    cat("Classifier fields: ", paste(cols, collapse = ", "), "\n", sep = "")
    prv <- per_file$unique_values_preview[[i]]
    for (nm in names(prv)) {
      cat("  * ", nm, " = [", paste(prv[[nm]], collapse = ", "), "]\n", sep = "")
    }
  }
})

# 8. Build tidy summary table (per file/field) ----
# -----------------------------------------------------------------------------
tidy_summary <- map_dfr(seq_len(nrow(per_file)), function(i) {
  trr   <- per_file$territory[i]
  file  <- per_file$file[i]
  cols  <- per_file$classifier_fields[[i]]
  uniqs <- per_file$unique_values[[i]]
  prv   <- per_file$unique_values_preview[[i]]

  if (length(cols) == 0) {
    return(tibble(
      territory      = trr,
      file           = file,
      field          = NA_character_,
      n_levels       = 0L,
      values_preview = NA_character_,
      values_full    = NA_character_
    ))
  }

  map_dfr(cols, function(cl) {
    vals_full <- uniqs[[cl]]
    vals_prev <- prv[[cl]]
    tibble(
      territory      = trr,
      file           = file,
      field          = cl,
      n_levels       = length(vals_full),
      values_preview = paste(vals_prev, collapse = " | "),
      values_full    = paste(vals_full, collapse = " | ")
    )
  })
})

# 9. Global unique sets (per territory and overall) ----
# -----------------------------------------------------------------------------
# per territory + field
global_by_territory <- tidy_summary %>%
  filter(!is.na(field), n_levels > 0) %>%
  separate_wider_delim(values_full, delim = " \\| ", names = paste0("v", 1:999), too_few = "align_start") %>%
  pivot_longer(starts_with("v"), names_to = NULL, values_to = "value") %>%
  filter(!is.na(value), value != "") %>%
  distinct(territory, field, value) %>%
  arrange(territory, field, value) %>%
  group_by(territory, field) %>%
  summarise(values = paste(value, collapse = " | "), n_levels = dplyr::n(), .groups = "drop")

# overall (all territories together)
global_overall <- global_by_territory %>%
  separate_wider_delim(values, delim = " \\| ", names = paste0("v", 1:999), too_few = "align_start") %>%
  pivot_longer(starts_with("v"), names_to = NULL, values_to = "value") %>%
  filter(!is.na(value), value != "") %>%
  distinct(field, value) %>%
  arrange(field, value) %>%
  group_by(field) %>%
  summarise(values = paste(value, collapse = " | "), n_levels = dplyr::n(), .groups = "drop")

# 10. Write diagnostics ----
# -----------------------------------------------------------------------------
readr::write_csv(tidy_summary,      file.path(diag_dir, "palette_fields_summary.csv"))
readr::write_csv(global_by_territory, file.path(diag_dir, "palette_fields_global.csv"))
readr::write_csv(global_overall,    file.path(diag_dir, "palette_fields_global_overall.csv"))

cat("\nSaved:\n  - ", file.path(diag_dir, "palette_fields_summary.csv"),
    "\n  - ", file.path(diag_dir, "palette_fields_global.csv"),
    "\n  - ", file.path(diag_dir, "palette_fields_global_overall.csv"), "\n", sep = "")

