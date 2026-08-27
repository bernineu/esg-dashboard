# ============================================================
# ui_cards.R
# The results grid: at-a-glance badge helpers, the clickable card grid,
# and the count + relevance-legend header shown above it.
# ============================================================

# ---- Badge helpers (shared by the card grid) ----

badge_relevance <- function(level) {
  bg <- RELEVANCE_COLORS[[as.character(level)]]
  if (is.null(bg)) bg <- "#f8f9fa"
  tags$span(class = "badge", style = sprintf(
    "background-color:%s; color:#333; border:1px solid rgba(0,0,0,.08); font-weight:500;", bg),
    as.character(level))
}

badge_source_type <- function(t) {
  cls <- if (identical(as.character(t), "Public")) "badge bg-success" else "badge bg-secondary"
  tags$span(class = cls, as.character(t))
}

badge_effort <- function(e) {
  cls <- switch(as.character(e),
    "Low"    = "badge bg-success",
    "Medium" = "badge bg-warning text-dark",
    "High"   = "badge bg-danger",
    "badge bg-secondary"
  )
  tags$span(class = cls, paste("Effort:", e))
}

badge_portfolio <- function(p) {
  cls <- switch(as.character(p),
    "Yes"    = "badge bg-success",
    "Partly" = "badge bg-warning text-dark",
    "No"     = "badge bg-secondary",
    "badge bg-secondary"
  )
  tags$span(class = cls, paste("Portfolio ready:", p))
}

# Renders the results as a responsive, clickable card grid. Clicking a card
# sends its source_id to `click_input_id` (as a Shiny event input), which the
# server uses to open the matching Source detail sub-tab.
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
            p(class = "card-text small mb-2 text-muted",
              tags$strong(class = "text-body", "Operator: "), row$operator),
            div(class = "mt-auto d-flex flex-wrap gap-1",
              badge_relevance(row$relevance_level),
              badge_source_type(row$source_type),
              badge_effort(row$technical_effort),
              badge_portfolio(row$portfolio_ready)
            )
          )
        )
      )
    })
  )
}

# Result-count + collapsible relevance-legend row, shown above each grid.
result_header_ui <- function(count_output_id) {
  div(class = "d-flex justify-content-between align-items-start mb-2",
    tagAppendAttributes(
      textOutput(count_output_id, inline = TRUE),
      class = "fw-bold"
    ),
    tags$details(
      tags$summary(
        class = "text-muted small",
        style = "cursor:pointer; user-select:none",
        "Relevance levels explained"
      ),
      tags$dl(
        class = "small mt-1 mb-0",
        lapply(names(RELEVANCE_DEFS), function(k) {
          tagList(tags$dt(k), tags$dd(class = "ms-3", RELEVANCE_DEFS[[k]]))
        })
      )
    )
  )
}
