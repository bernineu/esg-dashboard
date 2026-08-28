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

for (.f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(.f, local = TRUE)

out_dir <- "figures"
if (!dir.exists(out_dir)) dir.create(out_dir)

sources_df  <- load_sources_for_analysis()
hazard_cov  <- load_hazard_coverage_for_analysis()
d01_mapping <- load_d01_mapping_for_analysis()

save_fig <- function(name, plot, width = 16, height = 10) {
  # No in-chart title on the exported PNGs - the thesis already gives each
  # figure an "Abbildung X: ..." caption below it, so a repeated title
  # inside the image would be redundant. Subtitles stay (they carry a
  # data qualifier, e.g. "at least partial coverage", not a description
  # of the chart). The Shiny app (app.R) calls the same plot_*() functions
  # directly and keeps titles, since it has no caption of its own.
  plot <- plot + labs(title = NULL)
  ggsave(
    filename = file.path(out_dir, paste0(name, ".png")),
    plot = plot, width = width, height = height, units = "cm",
    dpi = 300, bg = "white"
  )
  message("Saved ", file.path(out_dir, paste0(name, ".png")))
}

save_fig("01_distribution_relevance",        plot_distribution(sources_df, "relevance_level", "Relevance level", "Number of sources"), height = 9)
save_fig("02_distribution_source_type",      plot_distribution(sources_df, "source_type", "Source type", "Number of sources"), height = 7)
save_fig("03_distribution_technical_effort", plot_distribution(sources_df, "technical_effort", "Technical effort", "Number of sources"), height = 8)
save_fig("04_distribution_portfolio_ready",  plot_distribution(sources_df, "portfolio_ready", "Portfolio-ready", "Number of sources"), height = 8)

save_fig("05_hazard_coverage_heatmap", plot_hazard_heatmap(hazard_cov, sources_df), width = 18, height = 14)
save_fig("06_hazard_coverage_gap",     plot_hazard_gap(hazard_cov), width = 16, height = 12)

save_fig("07_d01_mapping_coverage", plot_mapping_coverage(d01_mapping, sources_df), width = 16, height = 9)

save_fig("08_portfolio_by_effort",   plot_portfolio_by_effort(sources_df), width = 16, height = 10)
save_fig("09_readiness_reason",      plot_readiness_reason(sources_df), width = 16, height = 7)

message("Done. ", length(list.files(out_dir, pattern = "\\.png$")), " figures in ", normalizePath(out_dir))
