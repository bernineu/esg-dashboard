# ============================================================
# config.R
# Small lookup tables shared across the UI. Kept together so the wording
# and colours used for relevance / portfolio-readiness live in one place.
# ============================================================

# Relevance-level definitions (Data Source Matrix legend). The names(),
# in this order, drive the order of the "Relevance level" facet; the
# definitions are echoed in that facet's ⓘ tooltip (see FACET_TIPS in
# R/ui_pages.R).
RELEVANCE_DEFS <- c(
  "Primary"        = "Direct input for populating D 01.01 data points.",
  "Supplementary"  = "Useful supporting data; not sufficient alone.",
  "Context only"   = "Background / benchmarking; not suitable for individual exposure classification."
)

# Background colour per relevance level for the card / detail badge.
RELEVANCE_COLORS <- c(
  "Primary"        = "#d4edda",
  "Supplementary"  = "#fff3cd",
  "Context only"   = "#f8f9fa"
)

# output_type / integration_step / access_mode (workbook Legend sheet,
# replacing the old portfolio_ready + portfolio_ready_reason pair). Fixed
# level order drives both facet choice order and detail-chip order; DEFS
# gives each value's ⓘ / chip-title definition.
OUTPUT_TYPE_LEVELS <- c("ready-made", "indicator", "raw variable")
OUTPUT_TYPE_DEFS <- c(
  "ready-made"   = "A value computed for the individual object or a delineated hazard zone.",
  "indicator"    = "A value computed for a generic spatial unit (e.g. grid cell, region) and inherited by every object within it.",
  "raw variable" = "An underlying variable from which a hazard statement must still be derived."
)

INTEGRATION_STEP_LEVELS <- c("point query", "download and join", "multi-product", "none")
INTEGRATION_STEP_DEFS <- c(
  "point query"        = "A single query per exposure (coordinate or address) returns the value directly.",
  "download and join"  = "One dataset is downloaded once and joined to every exposure by location.",
  "multi-product"      = "Several datasets or processing steps must be combined before a value results.",
  "none"               = "No machine-queryable value reaches an individual exposure."
)

ACCESS_MODE_LEVELS <- c("bulk", "single lookup", "none")
ACCESS_MODE_DEFS <- c(
  "bulk"          = "Can be queried or downloaded for a whole portfolio at once.",
  "single lookup" = "Only one object can be queried at a time - no bulk or portfolio-wide access.",
  "none"          = "Returns no object-level value at all."
)

# source_details.csv's `field` column holds snake_case codes; this is the
# only place their display labels are spelled out. abstract / limitations /
# suitability_snci are pulled into their own sections in R/ui_detail.R
# (looked up by code, not by this label); everything else falls into the
# collapsed "Details" list, shown under its FIELD_LABELS entry (or the raw
# code, unprettified, if a field ever shows up that isn't listed here).
FIELD_LABELS <- c(
  abstract                   = "Abstract",
  suitability_snci           = "Suitability for an SNCI",
  limitations                = "Limitations",
  pricing                    = "Pricing details",
  data_format                = "Data format",
  interface_type             = "Interface type",
  data_update_frequency      = "Data update frequency",
  licensing                  = "Licensing notes",
  data_quality               = "Data quality notes",
  portfolio_pipeline         = "Portfolio-ready pipeline"
)
