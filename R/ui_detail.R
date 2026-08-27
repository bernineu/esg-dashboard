# ============================================================
# ui_detail.R
# The Source-detail pane. Top nav row (Back / Prev / Next through the
# filtered results) -> hero -> metadata chips -> Limitations -> Abstract
# -> Suitability -> D 01.01 -> hazard coverage -> Pricing -> collapsed
# technical notes. Laid out to state each fact once.
# ============================================================

# Small section heading used throughout the pane.
.detail_h <- function(txt) {
  h6(class = "text-uppercase text-muted mb-2",
     style = "font-size:.75rem; letter-spacing:.03em;", txt)
}

# Top navigation row: "Back to results" plus Prev / position / Next
# controls that step through `siblings` (the current filtered result order).
# `prefix` ("physical" / "transition") builds the input ids the server in
# server_risk_panel.R listens on: <prefix>_back / _prev / _next.
.detail_nav <- function(prefix, current_id = NULL, siblings = character(0)) {
  if (is.null(prefix)) return(NULL)
  back <- actionButton(paste0(prefix, "_back"), "← Back to results",
    class = "btn btn-outline-secondary btn-sm")

  pos <- if (length(siblings) > 0 && !is.null(current_id)) match(current_id, siblings) else NA
  stepper <- if (!is.na(pos)) {
    n    <- length(siblings)
    prev <- actionButton(paste0(prefix, "_prev"), "← Prev",
      class = "btn btn-outline-secondary btn-sm")
    nxt  <- actionButton(paste0(prefix, "_next"), "Next →",
      class = "btn btn-outline-secondary btn-sm")
    if (pos <= 1) prev <- tagAppendAttributes(prev, disabled = NA)
    if (pos >= n) nxt  <- tagAppendAttributes(nxt,  disabled = NA)
    div(class = "btn-group btn-group-sm", role = "group", `aria-label` = "Source navigation",
      prev,
      tags$span(class = "btn btn-outline-secondary btn-sm disabled",
        sprintf("%d / %d", pos, n)),
      nxt
    )
  }

  div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3",
    back, stepper)
}

# Renders the detail pane for one source record (a single-row data frame
# from lookup_source(), or NULL/0-row if none selected). `prefix` wires the
# navigation row; `siblings` is the source_id order of the current filtered
# results, used for the Prev/Next stepper.
render_detail_ui <- function(src, prefix = NULL, siblings = character(0)) {
  nav_row <- .detail_nav(prefix,
    current_id = if (!is.null(src) && nrow(src) > 0) src$source_id else NULL,
    siblings   = siblings)

  if (is.null(src) || nrow(src) == 0) {
    return(tagList(
      nav_row,
      div(class = "text-muted fst-italic text-center p-5",
        "No source selected. Click a card in the Results tab.")
    ))
  }

  sid <- src$source_id
  sd  <- details_df  %>% filter(source_id == sid)
  dm  <- d01_mapping %>% filter(source_id == sid)
  haz <- hazard_cov   %>% filter(source_id == sid)

  # Pull the reader-facing fields out into their own sections: the factual
  # Abstract, the Suitability verdict, the headline Limitations note and
  # Pricing (commercial sources). Everything else (formats, update
  # frequency, licensing, pipeline steps, methodology notes) stays in the
  # collapsed "Rationale & source notes".
  pull_field <- function(f) {
    v <- sd %>% filter(field == f) %>% pull(text)
    if (length(v) > 0) trimws(v[1]) else ""
  }
  abstract_txt    <- pull_field("Abstract")
  suitability_txt <- pull_field("Suitability assessment")
  key_lim         <- pull_field("Limitations")
  pricing_txt     <- pull_field("Pricing details")
  sd_rest <- sd %>% filter(!field %in% c(
    "Abstract", "Suitability assessment", "Limitations", "Pricing details"))
  has_abstract <- nzchar(abstract_txt)

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
    list(label = "Relevance",        value = as.character(src$relevance_level)),
    list(label = "Source type",      value = src$source_type),
    list(label = "Geographic scope", value = src$geographic_scope),
    list(label = "Granularity",      value = src$granularity_level),
    list(label = "Max. effort",      value = as.character(src$technical_effort)),
    list(label = "API access",       value = src$api),
    list(label = "Portfolio-ready",  value = pr_value, title = pr_title)
  )

  tagList(

    nav_row,

    # ---- Hero header ----
    div(class = "mb-4",
      h3(class = "mb-1", src$source_name),
      p(class = "text-muted mb-1", src$short_description),
      p(class = "small mb-2 text-muted",
        tags$strong(class = "text-body", "Operator: "), src$operator),
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

    # ---- Limitations (the headline caveat for this source) ----
    if (nzchar(key_lim)) div(class = "border-start border-3 border-warning ps-3 mb-4",
      .detail_h("Limitations"),
      p(class = "mb-0", key_lim)
    ),

    # ---- Cited source abstract (factual description of the source) ----
    if (has_abstract) tagList(
      .detail_h("Abstract"),
      div(class = "border-start border-3 border-primary ps-3 mb-4",
        p(class = "mb-0", abstract_txt)
      )
    ),

    # ---- Suitability assessment (the "should an SNCI use this" verdict) ----
    if (nzchar(suitability_txt)) tagList(
      .detail_h("Suitability for an SNCI"),
      p(class = "mb-4", suitability_txt)
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

    # ---- Hazard coverage (physical sources only). All 12 hazards as
    #      colour-coded badges (green = full, amber = partial, red = not
    #      covered); the per-hazard coverage detail for the covered ones
    #      sits in a collapsible table. ----
    if (nrow(haz) > 0) {
      haz_o <- haz %>% arrange(desc(coverage), match(hazard_id, names(HAZARD_LABELS)))
      cov   <- haz_o %>% filter(coverage != "none")
      hz_class <- function(cv) switch(as.character(cv),
        "full"    = "badge bg-success",
        "partial" = "badge bg-warning text-dark",
        "none"    = "badge bg-danger",
        "badge bg-secondary"
      )

      tagList(
        .detail_h("Hazard coverage"),
        div(class = "d-flex flex-wrap gap-2 mb-2",
          lapply(seq_len(nrow(haz_o)), function(i)
            tags$span(class = hz_class(haz_o$coverage[i]),
              paste0(HAZARD_LABELS[haz_o$hazard_id[i]], " (", haz_o$coverage[i], ")")))
        ),
        if (nrow(cov) > 0) tags$details(class = "mb-4",
          tags$summary(
            class = "text-uppercase text-muted mb-0",
            style = "font-size:.75rem; letter-spacing:.03em; cursor:pointer;",
            sprintf("Coverage detail (%d)", nrow(cov))
          ),
          div(class = "table-responsive mt-3",
            tags$table(class = "table table-sm align-middle small mb-0",
              tags$thead(tags$tr(
                tags$th(scope = "col", "Hazard"),
                tags$th(scope = "col", "Coverage"),
                tags$th(scope = "col", "Detail")
              )),
              tags$tbody(lapply(seq_len(nrow(cov)), function(i) {
                d <- cov$granularity_detail[i]
                tags$tr(
                  tags$td(class = "text-nowrap fw-semibold", HAZARD_LABELS[cov$hazard_id[i]]),
                  tags$td(tags$span(class = hz_class(cov$coverage[i]), as.character(cov$coverage[i]))),
                  tags$td(class = "text-muted", if (!is.na(d) && nzchar(d)) d else "—")
                )
              }))
            )
          )
        ) else div(class = "mb-4")
      )
    },

    # ---- Pricing (commercial sources only; public sources have no such note) ----
    if (nzchar(pricing_txt)) tagList(
      .detail_h("Pricing"),
      div(class = "border rounded-3 p-3 bg-light mb-4",
        div(class = "small", pricing_txt)
      )
    ),

    # ---- Technical & source notes (collapsed: reference detail behind the
    #      sections above - formats, update cadence, licensing, pipeline,
    #      methodology notes) ----
    if (nrow(sd_rest) > 0) {
      tags$details(class = "mb-2", open = if (has_abstract) NULL else NA,
        tags$summary(
          class = "text-uppercase text-muted mb-0",
          style = "font-size:.75rem; letter-spacing:.03em; cursor:pointer;",
          sprintf("Technical & source notes (%d)", nrow(sd_rest))
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
