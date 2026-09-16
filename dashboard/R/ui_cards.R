# ============================================================
# ui_cards.R
# The results grid: at-a-glance badge helpers, the clickable card grid,
# and the result-count line shown above it.
# ============================================================

# ---- Badge helpers (shared by the card grid) ----

#' Colour-coded relevance-level badge.
#'
#' @param level A relevance_level value ("Primary", "Supplementary" or
#'   "Context only").
#' @return An htmltools `tags$span` badge, background colour from
#'   RELEVANCE_COLORS (R/config.R).
badge_relevance <- function(level) {
  bg <- RELEVANCE_COLORS[[as.character(level)]]
  if (is.null(bg)) bg <- "#f8f9fa"
  tags$span(class = "badge", style = sprintf(
    "background-color:%s; color:#333; border:1px solid rgba(0,0,0,.08); font-weight:500;", bg),
    as.character(level))
}

#' Colour-coded source-type badge.
#'
#' @param t A source_type value ("Public" or "Commercial").
#' @return An htmltools `tags$span` badge (green for Public, grey
#'   otherwise).
badge_source_type <- function(t) {
  cls <- if (identical(as.character(t), "Public")) "badge bg-success" else "badge bg-secondary"
  tags$span(class = cls, as.character(t))
}

#' Colour-coded technical-effort badge.
#'
#' @param e A technical_effort value ("Low", "Medium" or "High").
#' @return An htmltools `tags$span` badge reading "Effort: <e>", green/
#'   amber/red by level.
badge_effort <- function(e) {
  cls <- switch(as.character(e),
    "Low"    = "badge bg-success",
    "Medium" = "badge bg-warning text-dark",
    "High"   = "badge bg-danger",
    "badge bg-secondary"
  )
  tags$span(class = cls, paste("Effort:", e))
}

#' Colour-coded access-mode badge.
#'
#' @param a An access_mode value ("bulk", "single lookup" or "none").
#' @return An htmltools `tags$span` badge reading "Access: <a>", green/
#'   amber/grey by level.
badge_access_mode <- function(a) {
  cls <- switch(as.character(a),
    "bulk"          = "badge bg-success",
    "single lookup" = "badge bg-warning text-dark",
    "none"          = "badge bg-secondary",
    "badge bg-secondary"
  )
  tags$span(class = cls, paste("Access:", a))
}

#' Render the filtered results as a responsive, clickable card grid.
#'
#' Clicking a card sends its source_id to `click_input_id` as a custom
#' Shiny input event (`Shiny.setInputValue(..., {priority: 'event'})`),
#' which server_risk_panel.R's wire_risk_panel() listens on to open the
#' matching Source detail sub-tab.
#'
#' @param df The filtered results (the data frame returned by
#'   filter_sources()).
#' @param click_input_id The Shiny input id a card click reports its
#'   source_id to (e.g. "physical_card_click").
#' @param selected_id The currently open detail source_id, if any -
#'   highlighted with a border if its card is in `df`.
#' @return An htmltools card-grid `div`, or a "No sources match" placeholder
#'   if `df` has zero rows.
render_card_grid <- function(df, click_input_id, selected_id = NULL) {
  if (nrow(df) == 0) {
    return(div(class = "text-muted fst-italic text-center p-5",
      "No sources match the current filters."))
  }

  div(class = "row row-cols-1 row-cols-md-2 row-cols-xl-3 g-3",
    lapply(seq_len(nrow(df)), function(i) {
      row    <- df[i, ]
      is_sel <- !is.null(selected_id) && identical(row$source_id, selected_id)

      div(class = "col",
        div(
          class   = paste("card h-100 source-card", if (is_sel) "border-primary shadow-sm" else ""),
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                             click_input_id, row$source_id),
          div(class = "card-body d-flex flex-column",
            h6(class = "card-title mb-1", row$source_name),
            p(class = "card-text text-muted small mb-2 source-card-desc", row$short_description),
            p(class = "card-text small mb-0 text-muted",
              tags$strong(class = "text-body", "Operator: "), row$operator),
            p(class = "card-text small mb-2 text-muted",
              tags$strong(class = "text-body", "Granularity: "), row$granularity_level),
            div(class = "mt-auto d-flex flex-wrap gap-1",
              badge_relevance(row$relevance_level),
              badge_source_type(row$source_type),
              badge_effort(row$technical_effort),
              badge_access_mode(row$access_mode)
            )
          )
        )
      )
    })
  )
}

#' Result-count line, shown above each grid.
#'
#' The relevance levels themselves are explained via the "ⓘ" on the
#' Relevance-level facet (see FACET_TIPS in R/ui_pages.R), not here.
#'
#' @param count_output_id The `textOutput` id to bind (e.g.
#'   "result_count_physical"), rendered server-side as e.g. "3 of 19
#'   sources match your filters".
#' @return An htmltools-wrapped `textOutput`, bold and block-level.
result_header_ui <- function(count_output_id) {
  tagAppendAttributes(
    textOutput(count_output_id, inline = TRUE),
    class = "fw-bold d-block mb-2"
  )
}
