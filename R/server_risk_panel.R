# ============================================================
# server_risk_panel.R
# The reactive plumbing for one risk-type nav panel. Physical and
# Transition are identical apart from the risk value and whether the
# hazard-type filter applies, so both are wired by this one helper.
# ============================================================

# `prefix` ("physical" / "transition") must match the output/input ids
# built by risk_nav_panel() in ui_pages.R (<prefix>_card_click / _back /
# _prev / _next / result_count_<prefix> / results_cards_<prefix> /
# detail_view_<prefix>). `hazard_input_id` is the checkbox-group id for the
# Tier 2 hazard filter, or NULL where it does not apply (Transition).
wire_risk_panel <- function(input, output, session, risk_value, prefix,
                            hazard_input_id = NULL) {
  selected_id <- reactiveVal(NULL)
  subtabs     <- paste0(prefix, "_subtabs")

  filtered <- reactive({
    filter_sources(
      risk_value           = risk_value,
      hazard_types         = if (is.null(hazard_input_id)) character(0)
                             else input[[hazard_input_id]],
      sel_search           = input$search_query,
      sel_source_type      = input$source_type,
      sel_relevance        = input$relevance_level,
      sel_technical_effort = input$technical_effort
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
