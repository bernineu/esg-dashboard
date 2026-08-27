# ============================================================
# config.R
# Small lookup tables shared across the UI. Kept together so the wording
# and colours used for relevance / portfolio-readiness live in one place.
# ============================================================

# Relevance-level definitions, shown in the collapsible legend above the
# results grid. The order here is also the order the "Relevance level"
# facet lists them in.
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
