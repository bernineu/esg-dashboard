# ============================================================
# export_figures.R
# Renders every chart from R/plots.R as a standalone PNG for direct
# embedding in the thesis Results chapter (Arial, 300 dpi, sized for a
# single text-column figure).
#
# Run with results-analysis/ as the working directory - e.g. open
# esg-dashboard.Rproj, then in the console:
#
#   setwd("results-analysis")
#   source("export_figures.R")
#
# or, from a terminal already inside results-analysis/:
#
#   Rscript export_figures.R
#
# Output goes to results-analysis/figures/.
# ============================================================

if (!dir.exists(file.path("..", "data"))) {
  stop(
    "Can't find ../data - run this script with results-analysis/ as the ",
    "working directory (see the comment at the top of this file)."
  )
}

library(ggplot2)
library(svglite)

for (.f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(.f, local = TRUE)

out_dir <- "figures"
if (!dir.exists(out_dir)) dir.create(out_dir)

sources_df  <- load_sources_for_analysis()
hazard_cov  <- load_hazard_coverage_for_analysis()
d01_mapping <- load_d01_mapping_for_analysis()

# svg = TRUE additionally exports an SVG alongside the PNG - reserved for
# the figures actually embedded in the thesis (Figure 2, Figure 3), where
# a vector version is wanted for print. The rest stay PNG-only, unchanged.
save_fig <- function(name, plot, width = 16, height = 10, svg = FALSE) {
  # No in-chart title on the exported PNGs - the thesis already gives each
  # figure an "Abbildung X: ..." caption below it, so a repeated title
  # inside the image would be redundant. Subtitles/captions stay (they
  # carry a data qualifier or derivation note, not a description of the
  # chart). The Shiny app (app.R) calls the same plot_*() functions
  # directly and keeps titles, since it has no caption of its own.
  plot <- plot + labs(title = NULL)
  ggsave(
    filename = file.path(out_dir, paste0(name, ".png")),
    plot = plot, width = width, height = height, units = "cm",
    dpi = 300, bg = "white"
  )
  message("Saved ", file.path(out_dir, paste0(name, ".png")))
  if (svg) {
    ggsave(
      filename = file.path(out_dir, paste0(name, ".svg")),
      plot = plot, width = width, height = height, units = "cm",
      device = svglite::svglite, bg = "white"
    )
    message("Saved ", file.path(out_dir, paste0(name, ".svg")))
  }
}

save_fig("01_distribution_relevance",        plot_distribution(sources_df, "relevance_level", "Relevance level", "Number of sources"), height = 9)
save_fig("02_distribution_source_type",      plot_distribution(sources_df, "source_type", "Source type", "Number of sources"), height = 7)
save_fig("03_distribution_technical_effort", plot_distribution(sources_df, "technical_effort", "Technical effort", "Number of sources"), height = 8)

# Figure 2 (thesis): hazard coverage across physical-risk data sources.
save_fig("05_hazard_coverage_heatmap", plot_hazard_heatmap(hazard_cov, sources_df), width = 18, height = 14, svg = TRUE)
save_fig("06_hazard_coverage_gap",     plot_hazard_gap(hazard_cov), width = 16, height = 12)

save_fig("07_d01_mapping_coverage", plot_mapping_coverage(d01_mapping, sources_df), width = 16, height = 9)

# Figure 3 (thesis): replaces the old "Portfolio readiness by technical
# effort" chart (portfolio_ready no longer exists) - derivation of
# technical_effort from output_type x integration_step.
save_fig("08_technical_effort_matrix", plot_technical_effort_matrix(sources_df, part = "main"), width = 26, height = 17, svg = TRUE)
save_fig("08b_technical_effort_exceptions", plot_technical_effort_matrix(sources_df, part = "exceptions"), width = 14, height = 9, svg = TRUE)

# Methods-chapter version of Figure 3: same rating grid, no source names.
save_fig("08_technical_effort_matrix_methods",
         plot_technical_effort_matrix(sources_df, part = "methods"),
         width = 26, height = 11, svg = TRUE)

message("Done. ", length(list.files(out_dir, pattern = "\\.png$")), " figures in ", normalizePath(out_dir))
