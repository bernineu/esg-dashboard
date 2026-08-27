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
  "Context only"   = "Background / benchmarking; not suitable for individual exposure classification.",
  "Methodological" = "Provides a replicable methodology rather than ready-made data."
)

# Background colour per relevance level for the card / detail badge.
RELEVANCE_COLORS <- c(
  "Primary"        = "#d4edda",
  "Supplementary"  = "#fff3cd",
  "Context only"   = "#f8f9fa",
  "Methodological" = "#d1ecf1"
)

# portfolio_ready_reason (workbook Legend sheet). "ready" explains a Yes;
# "technical" / "granularity" explain why a source is only Partly / No.
# DEFS is the full definition (chip tooltip); SHORT is the chip suffix.
PORTFOLIO_REASON_DEFS <- c(
  "ready"       = "A one-time pipeline differentiates individual exposures at a meaningful granularity, with no manual work per exposure.",
  "technical"   = "Held back by a technical access limit (no bulk/API access, or data only in an unstructured format such as PDF) - not by resolution.",
  "granularity" = "Held back by the source's own resolution: even a fully automated pipeline returns the same value for many exposures (per region, watershed or country)."
)
PORTFOLIO_REASON_SHORT <- c(
  "ready"       = "ready",
  "technical"   = "technical access",
  "granularity" = "granularity"
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
  download_format            = "Download format",
  web_interface_type         = "Web interface type",
  data_update_frequency      = "Data update frequency",
  licensing                  = "Licensing notes",
  data_quality               = "Data quality notes",
  portfolio_pipeline         = "Portfolio-ready pipeline",
  technical_effort_rationale = "Technical effort rationale"
)
