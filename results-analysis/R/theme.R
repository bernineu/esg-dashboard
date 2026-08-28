# ============================================================
# theme.R
# Shared ggplot2 theme + colour constants for every chart in this app,
# so the Shiny view and the static thesis-figure export (export_figures.R)
# render identically. Font is Arial throughout, to match the thesis body
# font (Windows resolves "Arial" as a system font name for both the
# on-screen and PNG graphics devices, no extra font package needed).
#
# Colour roles:
# - Categorical (risk_type, source_type, relevance_level): fixed hue
#   order, never cycled - blue/orange/aqua/yellow.
# - Status (coverage, portfolio_ready): green/amber/red, reused
#   consistently with the main dashboard's badge colours (green = full /
#   ready, amber = partial, red = not covered / not ready) - a state, not
#   a generic series, so the status palette is the right encoding.
# - Sequential (single-series magnitude, e.g. counts per data point):
#   one blue hue, light -> dark.
# ============================================================

library(ggplot2)

ESG_FONT <- "Arial"

ESG_INK_PRIMARY   <- "#0b0b0b"
ESG_INK_SECONDARY <- "#52514e"
ESG_INK_MUTED     <- "#898781"
ESG_GRID          <- "#e1e0d9"
ESG_BASELINE      <- "#c3c2b7"
ESG_SURFACE       <- "#fcfcfb"

ESG_CAT <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100")

ESG_STATUS <- c(good = "#0ca30c", warning = "#fab219", critical = "#d03b3b")

ESG_SEQ_BLUE <- "#2a78d6"

RISK_TYPE_COLORS <- c(Physical = unname(ESG_CAT["blue"]), Transition = unname(ESG_CAT["orange"]), Both = unname(ESG_CAT["aqua"]))

theme_esg <- function(base_size = 12, base_family = ESG_FONT) {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text            = element_text(colour = ESG_INK_PRIMARY, family = base_family),
      plot.title      = element_text(face = "bold", colour = ESG_INK_PRIMARY, size = rel(1.05),
                                      hjust = 0, margin = margin(b = 8)),
      plot.subtitle   = element_text(colour = ESG_INK_SECONDARY, size = rel(0.85),
                                      hjust = 0, margin = margin(b = 10)),
      axis.text       = element_text(colour = ESG_INK_MUTED, size = rel(0.85)),
      axis.title      = element_text(colour = ESG_INK_SECONDARY, size = rel(0.9)),
      panel.grid.major = element_line(colour = ESG_GRID, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line.x     = element_line(colour = ESG_BASELINE, linewidth = 0.4),
      axis.line.y     = element_blank(),
      axis.ticks      = element_blank(),
      legend.title    = element_text(colour = ESG_INK_SECONDARY, size = rel(0.85)),
      legend.text     = element_text(colour = ESG_INK_SECONDARY, size = rel(0.85)),
      legend.position = "top",
      plot.background  = element_rect(fill = ESG_SURFACE, colour = NA),
      panel.background = element_rect(fill = ESG_SURFACE, colour = NA),
      strip.text      = element_text(colour = ESG_INK_PRIMARY, face = "bold", size = rel(0.9)),
      plot.margin     = margin(10, 16, 10, 10),
      plot.title.position = "plot"
    )
}
