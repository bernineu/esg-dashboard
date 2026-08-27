# ============================================================
# ESG Data Source Matrix - R Shiny Prototype (Artefact 3)
#
# Entry point. The app is split into focused files under R/ (sourced
# below); this file only loads the data and wires the UI to the server.
#
#   R/load_data.R          data loaders, references.bib parser, hazard
#                          labels + sources_covering_hazards()
#   R/config.R             relevance / portfolio-readiness lookup tables
#   R/filter_sources.R     the results query + single-source lookup
#   R/ui_cards.R           results grid: badges, card grid, count header
#   R/ui_detail.R          the Source-detail pane
#   R/ui_pages.R           landing / dashboard (navbar + sidebar) /
#                          references page builders + app_ui()
#   R/server_risk_panel.R  wire_risk_panel(): reactive plumbing for one
#                          risk-type nav panel (used for both)
#   www/styles.css         card-grid styling
#
# Navigation (thesis Section 2.5): landing page -> Tier 1 risk type in the
# navbar -> Tier 2 hazard type (physical only) -> shared facet filters.
# Data is kept 1:1 with the thesis Excel artefact (Appendix C); the
# workbook Legend sheet tracks which fields were removed from sources.csv
# or moved into source_details.csv over successive revisions.
# ============================================================

library(shiny)
library(bslib)
library(dplyr)

# Source the app's modules into this file's environment (local = TRUE), so
# the helpers and the data objects created below always see each other -
# under shiny::runApp(), shiny::testServer() and plain source("app.R")
# alike. R/_disable_autoload.R turns off Shiny's own R/ auto-sourcing so
# this loop is the single load path.
for (.module in sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))) {
  if (basename(.module) != "_disable_autoload.R") source(.module, local = TRUE)
}

# ------------------------------------------------------------
# Data (see R/load_data.R). Loaded once at startup, read by the
# filter/render helpers from the global environment.
# ------------------------------------------------------------
sources_df   <- load_sources()
hazard_cov   <- load_hazard_coverage()
d01_mapping  <- load_d01_mapping()
details_df   <- load_source_details()
citations_df <- load_citations()
refs_df      <- load_references()

# The headline "Limitations" note moved from sources.csv into
# source_details.csv. Denormalise it back onto sources_df so the search
# haystack and the detail-view callout can reach it without a join.
sources_df <- sources_df %>% left_join(
  details_df %>% filter(field == "Limitations") %>% select(source_id, limitation = text),
  by = "source_id"
)

TOTAL_SOURCES <- nrow(sources_df)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- app_ui()

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Landing page / dashboard / references switch ----
  current_page <- reactiveVal("landing")   # "landing" | "dashboard" | "references"
  chosen_risk  <- reactiveVal("Physical")  # which navbar panel the dashboard opens on

  observeEvent(input$goto_physical,   { chosen_risk("Physical");   current_page("dashboard") })
  observeEvent(input$goto_transition, { chosen_risk("Transition"); current_page("dashboard") })
  observeEvent(input$goto_references, current_page("references"))
  observeEvent(input$goto_dashboard,  current_page("dashboard"))

  output$main_ui <- renderUI({
    switch(current_page(),
      "landing"    = landing_page_ui(),
      "references" = references_page_ui(),
      dashboard_ui(selected_tab = chosen_risk())
    )
  })

  # ---- Hazard "Select all" / "Clear" ----
  observeEvent(input$hazard_select_all,
    updateCheckboxGroupInput(session, "hazard_types", selected = names(HAZARD_LABELS)))
  observeEvent(input$hazard_clear,
    updateCheckboxGroupInput(session, "hazard_types", selected = character(0)))

  # ---- Reset all filters ----
  observeEvent(input$reset, {
    updateTabsetPanel(session, "risk_tabs",          selected = "Physical")
    updateTabsetPanel(session, "physical_subtabs",   selected = "Results")
    updateTabsetPanel(session, "transition_subtabs", selected = "Results")
    updateCheckboxGroupInput(session, "hazard_types",    selected = character(0))
    updateTextInput(session,   "search_query",    value    = "")
    updateSelectInput(session, "source_type",     selected = "All")
    updateSelectInput(session, "relevance_level", selected = "All")
    updateSelectInput(session, "technical_effort", selected = "All")
  })

  # ---- The two risk-type panels (identical plumbing, see R/server_risk_panel.R) ----
  wire_risk_panel(input, output, session, "Physical",   "physical", "hazard_types")
  wire_risk_panel(input, output, session, "Transition", "transition")
}

shinyApp(ui, server)
