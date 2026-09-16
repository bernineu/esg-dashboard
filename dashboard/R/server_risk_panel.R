# ============================================================
# server_risk_panel.R
# The reactive plumbing for one risk-type nav panel. Physical and
# Transition are identical apart from the risk value and whether the
# hazard-type filter applies, so both are wired by this one helper.
# ============================================================

#' Wire the reactive plumbing for one risk-type nav panel.
#'
#' Registers the filtered-results reactive, the result-count and
#' card-grid outputs, the card-click/back/prev/next observers and the
#' detail-view output for one risk panel (called once for "Physical" and
#' once for "Transition" from app.R's server function - see thesis Section
#' 3.3.4 / Figure 4).
#'
#' @param input,output,session The Shiny server function's standard
#'   arguments.
#' @param risk_value "Physical" or "Transition" - passed through to
#'   filter_sources().
#' @param prefix "physical" or "transition". Must match the output/input
#'   ids built by risk_nav_panel() in ui_pages.R (<prefix>_card_click /
#'   _back / _prev / _next / result_count_<prefix> / results_cards_<prefix>
#'   / detail_view_<prefix>).
#' @param hazard_input_id The checkbox-group input id for the Tier 2
#'   hazard filter, or NULL where it does not apply (Transition).
#' @return NULL (invisibly); called for its side effect of registering
#'   reactives, observers and outputs on `output`/`session`.
wire_risk_panel <- function(input, output, session, risk_value, prefix,
                            hazard_input_id = NULL) {
  selected_id <- reactiveVal(NULL)
  subtabs     <- paste0(prefix, "_subtabs")

  filtered <- reactive({
    filter_sources(
      risk_value           = risk_value,
      hazard_types         = if (is.null(hazard_input_id)) character(0)
                             else input[[hazard_input_id]],
      hazard_full_only     = isTRUE(input$hazard_full_only),
      sel_search           = input$search_query,
      sel_source_type      = input$source_type,
      sel_relevance        = input$relevance_level,
      sel_technical_effort = input$technical_effort,
      sel_output_type      = input$output_type,
      sel_integration_step = input$integration_step
    )
  })

  output[[paste0("result_count_", prefix)]] <- renderText(
    sprintf("%d of %d sources match your filters", nrow(filtered()), TOTAL_SOURCES)
  )

  output[[paste0("results_cards_", prefix)]] <- renderUI(
    render_card_grid(filtered(), paste0(prefix, "_card_click"), selected_id())
  )

  observeEvent(input[[paste0(prefix, "_card_click")]], {
    selected_id(input[[paste0(prefix, "_card_click")]])
    updateTabsetPanel(session, subtabs, selected = "Source detail")
  })

  observeEvent(input[[paste0(prefix, "_back")]], {
    updateTabsetPanel(session, subtabs, selected = "Results")
  })

  # Prev / Next step through the current filtered result order, so the
  # detail view can be browsed one source at a time without going back.
  step_source <- function(delta) {
    ids <- filtered()$source_id
    pos <- match(selected_id(), ids)
    if (!is.na(pos) && (pos + delta) >= 1 && (pos + delta) <= length(ids)) {
      selected_id(ids[pos + delta])
    }
  }
  observeEvent(input[[paste0(prefix, "_prev")]], step_source(-1))
  observeEvent(input[[paste0(prefix, "_next")]], step_source(1))

  output[[paste0("detail_view_", prefix)]] <- renderUI(
    render_detail_ui(lookup_source(selected_id()), prefix, filtered()$source_id)
  )
}
