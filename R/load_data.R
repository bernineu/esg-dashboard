# ============================================================
# load_data.R
# Loads the four normalized CSV files (Artefact 2 data layer)
# and prepares them for use in the Shiny app.
#
# Data model (see thesis Section 2.5 / Appendix C):
#   sources.csv          - one row per data source (master table)
#   hazard_coverage.csv  - long format: source_id x hazard_id x coverage
#   d01_mapping.csv      - long format: source_id x D 01.01 data point
#   source_details.csv   - long format: source_id x field x free text
# ============================================================

library(dplyr)
library(tidyr)

DATA_DIR <- "data"

load_sources <- function(path = file.path(DATA_DIR, "sources.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

  # technical_effort as an ordered factor (Section 2.5 / recommendation point 7)
  df$technical_effort <- factor(
    df$technical_effort,
    levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )

  # convert Yes/No/Partly text flags to explicit ordered factors where useful
  df$portfolio_ready <- factor(
    df$portfolio_ready,
    levels = c("No", "Partly", "Yes"),
    ordered = TRUE
  )

  df
}

load_hazard_coverage <- function(path = file.path(DATA_DIR, "hazard_coverage.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  df$coverage <- factor(df$coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  df
}

load_d01_mapping <- function(path = file.path(DATA_DIR, "d01_mapping.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

load_source_details <- function(path = file.path(DATA_DIR, "source_details.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# Human-readable labels for the 12 hazard types (matches thesis Table X / Section 2.4)
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

# Returns the source_ids that satisfy a minimum coverage level for ANY of the
# selected hazard_ids (a source counts if it covers at least one selected hazard
# at >= min_coverage). Used by Tier 2 filtering (physical-risk branch).
sources_covering_hazards <- function(hazard_cov, hazard_ids, min_coverage = "partial") {
  if (length(hazard_ids) == 0) return(unique(hazard_cov$source_id))
  min_level <- factor(min_coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  hazard_cov %>%
    filter(hazard_id %in% hazard_ids, coverage >= min_level) %>%
    pull(source_id) %>%
    unique()
}
