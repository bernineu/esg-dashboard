# ============================================================
# ui_detail.R
# The Source-detail pane. Laid out to state each fact once:
#   metadata chips -> cited abstract (the narrative assessment) ->
#   D 01.01 mapping -> hazard coverage -> collapsed rationale notes.
# ============================================================

# Small section heading used throughout the pane.
.detail_h <- function(txt) {
  h6(class = "text-uppercase text-muted mb-2",
     style = "font-size:.75rem; letter-spacing:.03em;", txt)
}

# Renders the detail pane for one source record (a single-row data frame
# from lookup_source(), or NULL/0-row if none selected). `back_input_id`
# is the id of a "Back to results" actionButton the server wires to switch
# the sub-tab back to Results.
render_detail_ui <- function(src, back_input_id = NULL) {
  back_btn <- if (!is.null(back_input_id)) {
    actionButton(back_input_id, "← Back to results",
      class = "btn btn-outline-secondary btn-sm mb-3")
  }

  if (is.null(src) || nrow(src) == 0) {
    return(tagList(
      back_btn,
      div(class = "text-muted fst-italic text-center p-5",
        "No source selected. Click a card in the Results tab.")
    ))
  }

  sid <- src$source_id
  sd  <- details_df  %>% filter(source_id == sid)
  dm  <- d01_mapping %>% filter(source_id == sid)
  haz <- hazard_cov   %>% filter(source_id == sid)

  # The cited abstract is the narrative synthesis of the structured fields
  # and the rationale notes, so it is shown as prose up top and the raw
  # notes are tucked into a collapsible section to avoid repeating it all.
  abstract_txt <- sd %>% filter(field == "Abstract") %>% pull(text)
  sd_rest      <- sd %>% filter(field != "Abstract")
  has_abstract <- length(abstract_txt) > 0 && nzchar(abstract_txt[1])

  # portfolio_ready + its reason: shown once, as a chip whose value carries
  # the short reason and whose tooltip carries the full definition. The
  # source-specific reasoning already lives in the abstract's closing lines.
  pr_reason <- trimws(as.character(src$portfolio_ready_reason))
  pr_value  <- as.character(src$portfolio_ready)
  if (nzchar(pr_reason) && !is.na(PORTFOLIO_REASON_SHORT[pr_reason]) &&
      pr_reason != "ready") {
    pr_value <- paste0(pr_value, " (", PORTFOLIO_REASON_SHORT[pr_reason], ")")
  }
  pr_title <- if (nzchar(pr_reason) && !is.na(PORTFOLIO_REASON_DEFS[pr_reason]))
    PORTFOLIO_REASON_DEFS[[pr_reason]] else NULL

  metadata_items <- list(
    list(label = "Operator type",    value = src$operator_type),
    list(label = "Relevance",        value = as.character(src$relevance_level)),
    list(label = "Source type",      value = src$source_type),
    list(label = "Max. effort",      value = as.character(src$technical_effort)),
    list(label = "API access",       value = src$api),
    list(label = "Portfolio-ready",  value = pr_value, title = pr_title)
  )

  tagList(

    back_btn,

    # ---- Hero header ----
    div(class = "mb-4",
      h3(class = "mb-1", src$source_name),
      p(class = "text-muted mb-2", src$short_description),
      div(class = "d-flex align-items-center flex-wrap gap-3",
        tags$a(class = "btn btn-sm btn-outline-primary",
          href = paste0("https://", src$url), target = "_blank", rel = "noopener noreferrer",
          "Visit source ↗"
        ),
        tags$span(class = "text-muted small", paste("Last checked:", src$last_checked))
      )
    ),

    # ---- Key metadata chips ----
    div(class = "d-flex flex-wrap gap-2 mb-4",
      lapply(metadata_items, function(m) {
        div(class = "border rounded-3 px-3 py-2 text-center bg-light",
          style = "min-width: 130px;", title = m$title,
          div(class = "text-muted text-uppercase", style = "font-size:.7rem; letter-spacing:.03em;", m$label),
          div(class = "fw-semibold", m$value)
        )
      })
    ),

    # ---- Cited source abstract (the narrative assessment) ----
    if (has_abstract) tagList(
      .detail_h("Abstract"),
      div(class = "border-start border-3 border-primary ps-3 mb-4",
        p(class = "mb-0", abstract_txt[1])
      )
    ),

    # ---- D 01.01 mapping ----
    if (nrow(dm) > 0) tagList(
      .detail_h("D 01.01 Mapping"),
      div(class = "d-flex flex-wrap gap-2 mb-4",
        lapply(seq_len(nrow(dm)), function(i) {
          tags$span(class = "badge bg-light text-dark border",
            sprintf("%s · %s", dm$data_point[i], dm$relevance[i]))
        })
      )
    ),

    # ---- Hazard coverage badges (physical sources only; skip all-none coverage) ----
    if (nrow(haz) > 0) {
      covered <- haz %>% filter(coverage != "none")
      if (nrow(covered) > 0) {
        det <- unique(covered$granularity_detail[!is.na(covered$granularity_detail) &
                                                 nzchar(covered$granularity_detail)])
        tagList(
          .detail_h("Hazard coverage"),
          div(class = "d-flex flex-wrap gap-2 mb-2",
            lapply(seq_len(nrow(covered)), function(i) {
              badge_class <- switch(as.character(covered$coverage[i]),
                "full"    = "badge bg-success",
                "partial" = "badge bg-warning text-dark",
                "badge bg-secondary"
              )
              tags$span(class = badge_class,
                paste0(HAZARD_LABELS[covered$hazard_id[i]], " (", covered$coverage[i], ")"))
            })
          ),
          # Coverage granularity, shown once (it is a per-source attribute in
          # practice - the same phrase repeats across a source's hazards).
          if (length(det) > 0) p(class = "text-muted small fst-italic mb-4",
            paste(det, collapse = " / "))
          else div(class = "mb-4")
        )
      }
    },

    # ---- Rationale & source notes (collapsed: the abstract above already
    #      synthesises these; kept for the full text / traceability) ----
    if (nrow(sd_rest) > 0) {
      tags$details(class = "mb-2", open = if (has_abstract) NULL else NA,
        tags$summary(
          class = "text-uppercase text-muted mb-0",
          style = "font-size:.75rem; letter-spacing:.03em; cursor:pointer;",
          sprintf("Rationale & source notes (%d)", nrow(sd_rest))
        ),
        div(class = "d-flex flex-column gap-2 mt-3",
          lapply(seq_len(nrow(sd_rest)), function(i) {
            div(class = "border rounded-3 p-3 bg-light",
              div(class = "fw-semibold small text-secondary mb-1", sd_rest$field[i]),
              div(class = "small", sd_rest$text[i])
            )
          })
        )
      )
    } else if (!has_abstract) {
      p(em("No detail text recorded for this source."))
    }

  )
}
