# ============================================================
# results-analysis / app.R
#
# Standalone results/analysis dashboard for the thesis Results chapter.
# Deliberately separate from the main ESG Data Source Matrix dashboard
# (../app.R): where the main app is a decision-support tool for browsing
# and filtering individual sources, this one is a fixed, read-only
# analysis of the source register as a whole (all 19 sources, no
# filters) - the quantitative counterpart to the qualitative matrix.
#
# Shares only the data/ CSVs with the main app, not its code - see
# R/load_analysis_data.R. Chart-building functions (R/plots.R) are also
# used by export_figures.R, which renders the same charts as
# thesis-ready PNGs (Arial, 300 dpi) outside of Shiny.
#
# Run: open this folder's app.R in RStudio and click "Run App", or
#   shiny::runApp("results-analysis")
# from the project root (first run results-analysis/requirements.R once).
# ============================================================

library(shiny)
library(bslib)
library(dplyr)

for (.f in list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)) source(.f, local = TRUE)

sources_df  <- load_sources_for_analysis()
hazard_cov  <- load_hazard_coverage_for_analysis()
d01_mapping <- load_d01_mapping_for_analysis()

N_SOURCES <- nrow(sources_df)
N_PHYSICAL_SOURCES <- n_distinct(hazard_cov$source_id)

ui <- page_navbar(
  title = "ESG Data Source Matrix — Results Analysis",
  fillable = FALSE,
  nav_panel(
    "Distributions",
    p(class = "text-muted",
      sprintf("All %d sources in the register, broken down by risk type.", N_SOURCES)),
    layout_column_wrap(
      width = 1/2, heights_equal = "row",
      card(plotOutput("plot_relevance", height = "300px")),
      card(plotOutput("plot_source_type", height = "220px")),
      card(plotOutput("plot_technical_effort", height = "260px"))
    )
  ),
  nav_panel(
    "Hazard coverage",
    p(class = "text-muted",
      sprintf("The %d sources that address at least one physical hazard, across all 12 hazard types.", N_PHYSICAL_SOURCES)),
    card(plotOutput("plot_hazard_heatmap", height = "620px")),
    card(plotOutput("plot_hazard_gap", height = "460px"))
  ),
  nav_panel(
    "D 01.01 mapping",
    p(class = "text-muted",
      "How many sources are mapped to each D 01.01 data point (Appendix C)."),
    card(plotOutput("plot_mapping_coverage", height = "380px"))
  ),
  nav_panel(
    "Technical effort",
    p(class = "text-muted",
      "Derivation of the technical effort rating from output type and integration step, and how the 19 sources distribute across it."),
    card(plotOutput("plot_technical_effort_matrix", height = "460px"))
  )
)

server <- function(input, output, session) {
  output$plot_relevance <- renderPlot(
    plot_distribution(sources_df, "relevance_level", "Relevance level", "Number of sources")
  )
  output$plot_source_type <- renderPlot(
    plot_distribution(sources_df, "source_type", "Source type", "Number of sources")
  )
  output$plot_technical_effort <- renderPlot(
    plot_distribution(sources_df, "technical_effort", "Technical effort", "Number of sources")
  )
  output$plot_hazard_heatmap <- renderPlot(
    plot_hazard_heatmap(hazard_cov, sources_df)
  )
  output$plot_hazard_gap <- renderPlot(
    plot_hazard_gap(hazard_cov)
  )
  output$plot_mapping_coverage <- renderPlot(
    plot_mapping_coverage(d01_mapping, sources_df)
  )
  output$plot_technical_effort_matrix <- renderPlot(
    plot_technical_effort_matrix(sources_df)
  )
}

shinyApp(ui, server)
