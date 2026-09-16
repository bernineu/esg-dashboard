# ============================================================
# ui_detail.R
# The Source-detail pane. Top nav row (Back / Prev / Next through the
# filtered results) -> hero -> metadata chips -> Abstract -> Suitability
# -> Limitations -> D 01.01 -> hazard coverage -> collapsed Details.
# Each block carries a coloured left rule so the sections read apart.
# Laid out to state each fact once.
# ============================================================

#' Small uppercase section heading used throughout the detail pane.
#'
#' @param txt The heading text.
#' @return An htmltools `h6` tag.
.detail_h <- function(txt) {
  h6(class = "text-uppercase text-muted mb-2",
     style = "font-size:.75rem; letter-spacing:.03em;", txt)
}

#' A titled detail-pane section with a coloured left rule.
#'
#' @param title Section heading (rendered via .detail_h()).
#' @param ... The section's body content (htmltools tags).
#' @param accent A Bootstrap theme colour name (primary / info / warning /
#'   secondary / ...) for the 3px left rule.
#' @return An htmltools `div` wrapping the heading and body.
.detail_section <- function(title, ..., accent = "secondary") {
  div(class = sprintf("border-start border-3 border-%s ps-3 mb-4", accent),
    .detail_h(title),
    ...
  )
}

#' Top navigation row of the detail pane: Back plus Prev/position/Next.
#'
#' The Prev/Next controls step through `siblings` (the current filtered
#' result order); each button is disabled at the corresponding end of the
#' list rather than removed, so its position stays fixed.
#'
#' @param prefix "physical" or "transition" - builds the input ids the
#'   server in server_risk_panel.R listens on: <prefix>_back / _prev /
#'   _next. NULL suppresses the whole row (e.g. no source ever opened).
#' @param current_id The source_id currently shown in the detail pane, or
#'   NULL.
#' @param siblings The source_id order of the current filtered results,
#'   used to find `current_id`'s position and neighbours.
#' @return An htmltools `div` with the Back button and, once a valid
#'   position is found, the Prev / "n / total" / Next stepper; NULL if
#'   `prefix` is NULL.
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

#' Render the full Source-detail pane for one source.
#'
#' Lays out (in order) the nav row, hero header, metadata chips, the cited
#' Abstract, the Suitability-for-an-SNCI verdict, the headline Limitations
#' note, the D 01.01 mapping badges, colour-coded hazard-coverage badges
#' with a collapsible per-hazard table, and a collapsed "Details" section
#' for every remaining source_details.csv field (thesis Section 3.3.5 /
#' Figure 6).
#'
#' @param src A one-row data frame from lookup_source() (the full
#'   sources_df record), or NULL/0-row if nothing is selected.
#' @param prefix "physical" or "transition", passed through to
#'   .detail_nav(); NULL suppresses the navigation row.
#' @param siblings The source_id order of the current filtered results,
#'   passed through to .detail_nav() for the Prev/Next stepper.
#' @return An htmltools `tagList` for the whole pane - a "No source
#'   selected" placeholder (plus the nav row) if `src` is NULL/0-row.
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
  # Abstract, the Suitability verdict and the headline Limitations note.
  # Everything else - pricing, formats, update frequency, licensing,
  # pipeline steps, methodology notes - stays in the collapsed "Details".
  # `field` holds snake_case codes (see FIELD_LABELS in R/config.R for the
  # display label of everything that isn't pulled out here by code).
  pull_field <- function(f) {
    v <- sd %>% filter(field == f) %>% pull(text)
    if (length(v) > 0) trimws(v[1]) else ""
  }
  abstract_txt    <- pull_field("abstract")
  suitability_txt <- pull_field("suitability_snci")
  key_lim         <- pull_field("limitations")
  sd_rest <- sd %>% filter(!field %in% c("abstract", "suitability_snci", "limitations"))
  has_abstract <- nzchar(abstract_txt)

  # output_type / integration_step / access_mode: each shown once, as a
  # chip whose tooltip carries the full legend definition (R/config.R).
  ot_val <- as.character(src$output_type)
  is_val <- as.character(src$integration_step)
  am_val <- as.character(src$access_mode)

  metadata_items <- list(
    list(label = "Relevance",        value = as.character(src$relevance_level)),
    list(label = "Source type",      value = src$source_type),
    list(label = "Geographic scope", value = src$geographic_scope),
    list(label = "Granularity",      value = src$granularity_level),
    list(label = "Output type",      value = ot_val, title = OUTPUT_TYPE_DEFS[[ot_val]]),
    list(label = "Integration step", value = is_val, title = INTEGRATION_STEP_DEFS[[is_val]]),
    list(label = "Technical effort", value = as.character(src$technical_effort)),
    list(label = "Access mode",      value = am_val, title = ACCESS_MODE_DEFS[[am_val]]),
    list(label = "API access",       value = src$api)
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

    # ---- 1. Cited source abstract (factual description of the source) ----
    if (has_abstract) .detail_section("Abstract", accent = "primary",
      p(class = "mb-0", abstract_txt)
    ),

    # ---- 2. Suitability assessment (the "should an SNCI use this" verdict) ----
    if (nzchar(suitability_txt)) .detail_section("Suitability for an SNCI", accent = "info",
      p(class = "mb-0", suitability_txt)
    ),

    # ---- 3. Limitations (the headline caveat for this source) ----
    if (nzchar(key_lim)) .detail_section("Limitations", accent = "warning",
      p(class = "mb-0", key_lim)
    ),

    # ---- D 01.01 mapping (d01_mapping.csv dropped its per-mapping
    #      `relevance` column - the source's overall Relevance chip above
    #      already carries that) ----
    if (nrow(dm) > 0) .detail_section("D 01.01 Mapping",
      div(class = "d-flex flex-wrap gap-2",
        lapply(seq_len(nrow(dm)), function(i) {
          tags$span(class = "badge bg-light text-dark border", dm$data_point[i])
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

      .detail_section("Hazard coverage",
        div(class = "d-flex flex-wrap gap-2 mb-2",
          lapply(seq_len(nrow(haz_o)), function(i)
            tags$span(class = hz_class(haz_o$coverage[i]),
              paste0(HAZARD_LABELS[haz_o$hazard_id[i]], " (", haz_o$coverage[i], ")")))
        ),
        if (nrow(cov) > 0) tags$details(class = "mb-0",
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
                tags$th(scope = "col", "Granularity"),
                tags$th(scope = "col", "Indicator"),
                tags$th(scope = "col", "Rationale")
              )),
              tags$tbody(lapply(seq_len(nrow(cov)), function(i) {
                gran <- cov$hazard_granularity[i]
                ind  <- cov$hazard_indicator[i]
                note <- cov$coverage_rationale[i]
                tags$tr(
                  tags$td(class = "text-nowrap fw-semibold", HAZARD_LABELS[cov$hazard_id[i]]),
                  tags$td(tags$span(class = hz_class(cov$coverage[i]), as.character(cov$coverage[i]))),
                  tags$td(class = "text-muted", if (!is.na(gran) && nzchar(gran)) gran else "—"),
                  tags$td(class = "text-muted", if (!is.na(ind) && nzchar(ind)) ind else "—"),
                  tags$td(class = "text-muted", if (!is.na(note) && nzchar(note)) note else "—")
                )
              }))
            )
          )
        )
      )
    },

    # ---- Details (collapsed: pricing, formats, update cadence, licensing,
    #      pipeline steps, methodology notes) ----
    if (nrow(sd_rest) > 0) {
      div(class = "border-start border-3 border-secondary ps-3 mb-2",
        tags$details(open = if (has_abstract) NULL else NA,
          tags$summary(
            class = "text-uppercase text-muted mb-0",
            style = "font-size:.75rem; letter-spacing:.03em; cursor:pointer;",
            sprintf("Details (%d)", nrow(sd_rest))
          ),
          div(class = "d-flex flex-column gap-2 mt-3",
            lapply(seq_len(nrow(sd_rest)), function(i) {
              lbl <- FIELD_LABELS[[sd_rest$field[i]]]
              div(class = "border rounded-3 p-3 bg-light",
                div(class = "fw-semibold small text-secondary mb-1",
                  if (is.null(lbl)) sd_rest$field[i] else lbl),
                div(class = "small", sd_rest$text[i])
              )
            })
          )
        )
      )
    } else if (!has_abstract) {
      p(em("No detail text recorded for this source."))
    }

  )
}
