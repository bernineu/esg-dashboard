# ============================================================
# load_analysis_data.R
# Standalone loaders for the results-analysis app. Deliberately
# independent of R/load_data.R (the main dashboard's loader) so this app
# has no code dependency on the main dashboard - only a data one, via the
# shared data/ CSVs (single source of truth: thesis Appendix C).
#
# Path convention: like the main app, all paths here are relative to this
# app's own working directory. Shiny sets the working directory to the
# app's own folder (results-analysis/) for the duration of the app, both
# via RStudio's "Run App" and shiny::runApp("results-analysis"), so
# "../data" is correct in app.R. export_figures.R is a plain script, not
# a Shiny app, so it must be run with results-analysis/ as the working
# directory (see the comment at the top of that file).
# ============================================================

library(dplyr)

DATA_DIR <- file.path("..", "data")

# Human-readable labels for the 12 hazard types (thesis Section 2.4 /
# Table X). Kept in sync with HAZARD_LABELS in ../R/load_data.R by hand -
# it's a small, stable label list, not app logic.
HAZARD_LABELS <- c(
  heat_stress   = "Heat stress",
  permafrost    = "Permafrost thawing",
  heat_wave     = "Heat wave",
  wildfire      = "Wildfire",
  storm         = "Storm",
  water_stress  = "Water stress",
  flood         = "Flood",
  heavy_precip  = "Heavy precipitation",
  drought       = "Drought",
  glacial_lake  = "Glacial lake outburst",
  soil_erosion  = "Soil erosion",
  landslide     = "Landslide"
)

READINESS_REASON_LABELS <- c(
  technical   = "Technical access limit",
  granularity = "Granularity limit",
  ready       = "Ready"
)

load_sources_for_analysis <- function(path = file.path(DATA_DIR, "sources.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

  df$relevance_level <- factor(
    df$relevance_level,
    levels = c("Primary", "Supplementary", "Context only")
  )
  df$source_type <- factor(df$source_type, levels = c("Public", "Commercial"))
  df$risk_type   <- factor(df$risk_type,   levels = c("Physical", "Transition", "Both"))
  df$technical_effort <- factor(df$technical_effort, levels = c("Low", "Medium", "High"), ordered = TRUE)
  df$portfolio_ready  <- factor(df$portfolio_ready,  levels = c("No", "Partly", "Yes"), ordered = TRUE)
  df$portfolio_ready_reason <- factor(df$portfolio_ready_reason, levels = c("technical", "granularity", "ready"))

  df
}

load_hazard_coverage_for_analysis <- function(path = file.path(DATA_DIR, "hazard_coverage.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  df$coverage <- factor(df$coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  df
}

load_d01_mapping_for_analysis <- function(path = file.path(DATA_DIR, "d01_mapping.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}
