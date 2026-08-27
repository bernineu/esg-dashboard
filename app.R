# ============================================================
# ESG Data Source Matrix - R Shiny Prototype (Artefact 3)
#
# Implements the navigation logic described in thesis Section 2.5:
#   Landing page: introduction + Physical/Transition risk choice
#   Tier 1: risk type (Physical / Transition) - top navbar (bslib::navset_bar)
#   Tier 2: hazard type (physical-risk branch only, per Section 2.4 Table X)
#   Facets: search, source type, relevance level, max. effort
#           (shared sidebar, applied across whichever tier the user is on)
#
# Results are shown as a clickable card grid (source, description, operator,
# plus at-a-glance badges); clicking a card opens a detailed view in the
# Source detail sub-tab, which also carries the cited source abstract.
#
# The References page renders data/references.bib as a bibliography: the
# supporting literature the source abstracts cite.
#
# research_status, access_status and cost_category were removed from
# sources.csv (workbook Legend sheet): process metadata, redundant with the
# download/api/web_interface columns, and redundant with source_type. The
# app no longer references them. portfolio_ready_reason (technical /
# granularity / ready) was added and is surfaced in the detail view.
#
# Data layer: R/load_data.R reads the normalized files described in
# Appendix C (sources.csv, hazard_coverage.csv, d01_mapping.csv,
# source_details.csv, citations.csv, references.bib), keeping the
# dashboard's data 1:1 with the thesis artefact rather than a separate,
# undocumented dataset.
# ============================================================

library(shiny)
library(bslib)
library(dplyr)

source("R/load_data.R")

sources_df   <- load_sources()
hazard_cov   <- load_hazard_coverage()
d01_mapping  <- load_d01_mapping()
details_df   <- load_source_details()
citations_df <- load_citations()
refs_df      <- load_references()

TOTAL_SOURCES <- nrow(sources_df)

# Remap HAZARD_LABELS to shiny's c(label = value) convention so that
# checkboxGroupInput shows "Heat stress" (not "heat_stress") as the label,
# while input$hazard_types stores the hazard ID ("heat_stress") directly.
HAZARD_CHOICES <- setNames(names(HAZARD_LABELS), HAZARD_LABELS)

RELEVANCE_DEFS <- c(
  "Primary"        = "Direct input for populating D 01.01 data points.",
  "Supplementary"  = "Useful supporting data; not sufficient alone.",
  "Context only"   = "Background / benchmarking; not suitable for individual exposure classification.",
  "Methodological" = "Provides a replicable methodology rather than ready-made data."
)

RELEVANCE_COLORS <- c(
  "Primary"        = "#d4edda",
  "Supplementary"  = "#fff3cd",
  "Context only"   = "#f8f9fa",
  "Methodological" = "#d1ecf1"
)

# portfolio_ready_reason (workbook Legend sheet). "ready" explains a Yes;
# "technical" / "granularity" explain why a source is only Partly / No.
PORTFOLIO_REASON_DEFS <- c(
  "ready"       = "A one-time pipeline differentiates individual exposures at a meaningful granularity, with no manual work per exposure.",
  "technical"   = "Held back by a technical access limit (no bulk/API access, or data only in an unstructured format such as PDF) - not by resolution.",
  "granularity" = "Held back by the source's own resolution: even a fully automated pipeline returns the same value for many exposures (per region, watershed or country)."
)
PORTFOLIO_REASON_SHORT <- c(
  "ready"       = "ready",
  "technical"   = "technical access",
  "granularity" = "granularity"
)

# ------------------------------------------------------------
# Shared helpers (pure functions, reused for both the Physical
# and Transition tabs so the filtering/rendering logic lives once)
# ------------------------------------------------------------

# Filters sources_df for one risk-type tab. Parameters are prefixed with
# "sel_" so they never collide with the sources_df column names of the
# same name inside dplyr::filter()'s data-masking evaluation.
filter_sources <- function(risk_value,
                            hazard_types         = character(0),
                            sel_search           = "",
                            sel_source_type      = "All",
                            sel_relevance        = "All",
                            sel_technical_effort = "All") {
  df <- sources_df %>% filter(risk_type == risk_value | risk_type == "Both")

  # Tier 2: hazard filter (physical-risk branch only). AND semantics -
  # with several hazards ticked, only sources covering every one of them
  # (at >= partial coverage) are kept.
  if (risk_value == "Physical" && length(hazard_types) > 0) {
    covering_ids <- sources_covering_hazards(
      hazard_cov, hazard_types, min_coverage = "partial")
    df <- df %>% filter(source_id %in% covering_ids)
  }

  # Free-text search: every whitespace-separated token must appear (case-
  # insensitively) somewhere in the source's name, operator, operator type
  # or short description. Token-based, so word order and surrounding
  # punctuation don't matter ("munich re", "re munich", "central bank ecb"
  # all match). Done with base subsetting on a pre-built haystack to avoid
  # colliding with the same-named columns inside dplyr data masking.
  tokens <- strsplit(tolower(trimws(sel_search)), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) > 0) {
    haystack <- tolower(paste(df$source_name, df$operator,
                              df$operator_type, df$short_description))
    keep <- Reduce(`&`, lapply(tokens, function(tk) grepl(tk, haystack, fixed = TRUE)))
    df <- df[keep, , drop = FALSE]
  }

  # Facets
  if (sel_source_type != "All") df <- df %>% filter(source_type     == sel_source_type)
  if (sel_relevance   != "All") df <- df %>% filter(relevance_level == sel_relevance)
  if (sel_technical_effort != "All") {
    max_level <- factor(sel_technical_effort, levels = c("Low", "Medium", "High"), ordered = TRUE)
    df <- df %>% filter(technical_effort <= max_level)
  }

  df %>%
    select(source_id, source_name, short_description, operator, source_type,
           relevance_level, technical_effort, portfolio_ready)
}

# ---- Small badge helpers (shared by the card grid) ----

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

# Looks up one source's full record for the detail view, independent of the
# current filter/tab state - so a previously opened detail stays viewable
# even if later filter changes would hide it from the results grid.
lookup_source <- function(sid) {
  if (is.null(sid)) return(NULL)
  sources_df %>% filter(source_id == sid)
}

# Renders the visually detailed source-detail pane for one source record
# (a single-row data frame from lookup_source(), or NULL/0-row if none selected).
# `back_input_id` is the id of a "Back to results" actionButton the server
# wires to switch the sub-tab back to Results.
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
      h6(class = "text-uppercase text-muted mb-2", style = "font-size:.75rem; letter-spacing:.03em;",
         "Abstract"),
      div(class = "border-start border-3 border-primary ps-3 mb-4",
        p(class = "mb-0", abstract_txt[1])
      )
    ),

    # ---- D 01.01 mapping ----
    if (nrow(dm) > 0) tagList(
      h6(class = "text-uppercase text-muted mb-2", style = "font-size:.75rem; letter-spacing:.03em;",
         "D 01.01 Mapping"),
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
          h6(class = "text-uppercase text-muted mb-2", style = "font-size:.75rem; letter-spacing:.03em;",
             "Hazard coverage"),
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

# Result-count + collapsible relevance-legend row, shared by both risk tabs.
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

# Landing page: introduction + the Physical/Transition risk choice. Shown
# once when the app opens; the dashboard itself keeps both tabs visible so
# users can switch risk type freely afterwards without returning here.
landing_page_ui <- function() {
  tagList(
    div(class = "text-center mt-5 mb-4",
      h1("ESG Data Source Matrix"),
      h5(class = "text-muted fw-normal",
        "Decision-support dashboard for identifying external ESG data sources ",
        "for Template D 01.01 (Austrian SNCIs)"),
      p(class = "text-muted mt-2",
        "Based on Artefact 2 (ESG Data Source Matrix) - thesis Section 2.5.")
    ),
    div(class = "row justify-content-center mb-4",
      div(class = "col-md-7",
        tags$ol(
          tags$li("Choose ", tags$strong("Physical risk"), " or ",
                  tags$strong("Transition risk"), " below to open the dashboard."),
          tags$li("For physical risk, narrow further by ", tags$strong("hazard type"),
                  " and the shared facet filters (search, source type, relevance level, ",
                  "max. effort) in the sidebar."),
          tags$li("Click any card in the ", tags$strong("Results"), " tab to open its ",
                  tags$strong("Source detail"), " tab: cited abstract, D 01.01 mapping, ",
                  "hazard coverage, and rationale text."),
          tags$li("Both risk types stay one click away in the ", tags$strong("navbar"),
                  " at the top of the dashboard - switch anytime. The navbar's ",
                  tags$strong("References"), " link opens the bibliography for the ",
                  "source abstracts.")
        )
      )
    ),
    div(class = "row justify-content-center g-4 mb-5",
      div(class = "col-auto",
        actionButton("goto_physical",
          tagList(
            div(class = "fs-5 fw-bold", "Physical risk"),
            div(class = "small fw-normal text-muted",
                "Climate hazards: heat, flood, drought, storm, ...")
          ),
          class = "btn btn-outline-primary p-4", style = "min-width: 280px;")
      ),
      div(class = "col-auto",
        actionButton("goto_transition",
          tagList(
            div(class = "fs-5 fw-bold", "Transition risk"),
            div(class = "small fw-normal text-muted",
                "NACE sector classification sources")
          ),
          class = "btn btn-outline-primary p-4", style = "min-width: 280px;")
      )
    )
  )
}

# Filter sidebar, shared across the risk-type nav panels (bslib::sidebar,
# so the inputs are defined once). The whole filter block is hidden while a
# "Source detail" sub-tab is open, or on any non-filterable nav panel.
filter_sidebar <- function() {
  # JS predicate: true while a source-detail sub-tab is open on either risk tab.
  in_detail_view <- paste(
    "(input.risk_tabs == 'Physical' && input.physical_subtabs == 'Source detail')",
    "|| (input.risk_tabs == 'Transition' && input.transition_subtabs == 'Source detail')"
  )

  sidebar(
    width = 320,
    title = "Filters",

    # ---- Filters (hidden while a source detail view is open) ----
    conditionalPanel(
      condition = paste0("!(", in_detail_view, ")"),

      # Free-text search (name, operator, description)
      textInput(
        "search_query", "Search sources",
        placeholder = "Name, operator, operator type, or description..."
      ),

      hr(),

      # Tier 2: hazard type (physical-risk tab only)
      conditionalPanel(
        condition = "input.risk_tabs == 'Physical'",
        h5("Hazard type"),
        div(class = "mb-1",
          actionLink("hazard_select_all", "Select all", class = "small"),
          " | ",
          actionLink("hazard_clear", "Clear", class = "small")
        ),
        checkboxGroupInput(
          "hazard_types", NULL,
          choices  = HAZARD_CHOICES,
          selected = character(0)
        ),
        helpText("Leave empty to show all physical-risk sources. ",
                 "Ticking several hazards shows only sources that cover ",
                 tags$strong("all"), " of them.")
      ),

      # Transition-risk note (replaces the missing Tier 2)
      conditionalPanel(
        condition = "input.risk_tabs == 'Transition'",
        div(class = "alert alert-info p-2 mb-2",
          tags$small(
            "Transition risk sources cover NACE sector classification uniformly ",
            "- no hazard-type filter applies."
          )
        )
      ),

      hr(),
      h5("Facets"),

      selectInput(
        "source_type", "Source type",
        choices  = c("All", sort(unique(sources_df$source_type))),
        selected = "All"
      ),
      selectInput(
        "relevance_level", "Relevance level",
        choices  = c("All", intersect(names(RELEVANCE_DEFS),
                                      unique(sources_df$relevance_level))),
        selected = "All"
      ),
      selectInput(
        "technical_effort", "Max. effort",
        choices  = c("All", "Low", "Medium", "High"),
        selected = "All"
      ),

      hr(),
      actionButton("reset", "Reset all filters",
        class = "btn-outline-secondary btn-sm w-100")
    ),

    # ---- Shown instead, while a source detail view is open ----
    conditionalPanel(
      condition = in_detail_view,
      div(class = "alert alert-light border small mb-0",
        "Viewing one source in detail. Use ",
        tags$strong("← Back to results"),
        " (top of the panel) to return to the filtered list."
      )
    ),

    hr(),
    helpText("Prototype for Artefact 3 (Section 2.5). ",
             "Data: data/*.csv, generated from the same source as thesis Appendix C.")
  )
}

# One risk-type nav panel: result-count header + Results / Source detail sub-tabs.
risk_nav_panel <- function(title, value, subtabs_id, cards_output, detail_output,
                           count_output) {
  nav_panel(title, value = value,
    br(),
    result_header_ui(count_output),
    tabsetPanel(
      id = subtabs_id,
      tabPanel("Results",       br(), uiOutput(cards_output)),
      tabPanel("Source detail", br(), uiOutput(detail_output))
    )
  )
}

# Main dashboard UI: a top navbar (Physical risk / Transition risk, plus a
# right-aligned References link) over a shared filter sidebar. `selected_tab`
# sets which risk panel opens first, based on the landing-page choice.
dashboard_ui <- function(selected_tab) {
  navset_bar(
    id       = "risk_tabs",
    title    = "ESG Data Source Matrix",
    selected = selected_tab,
    fillable = FALSE,
    sidebar  = filter_sidebar(),

    risk_nav_panel("Physical risk", "Physical", "physical_subtabs",
      "results_cards_physical", "detail_view_physical", "result_count_physical"),
    risk_nav_panel("Transition risk", "Transition", "transition_subtabs",
      "results_cards_transition", "detail_view_transition", "result_count_transition"),

    nav_spacer(),
    nav_item(actionLink("goto_references", "References"))
  )
}

# ------------------------------------------------------------
# References page
# ------------------------------------------------------------

# One <li> for a bibliography entry from references.bib.
format_bib_entry <- function(r) {
  head <- paste0(r$author, " (", r$year, "). ", r$title,
                 if (grepl("[.!?]$", r$title)) "" else ".")
  tail <- if (nzchar(r$urldate)) paste0(" Retrieved ", r$urldate, ",") else ""
  extra_note <- sub("^\\([^)]*\\)\\.?\\s*", "", r$note)   # note text after the (Author, year)
  tags$li(class = "mb-2",
    head, tail, " from ",
    tags$a(href = r$url, target = "_blank", rel = "noopener noreferrer", r$url),
    if (nzchar(extra_note)) tags$span(class = "text-muted", paste0(" — ", extra_note))
  )
}

references_page_ui <- function() {
  tagList(
    div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mt-2 mb-3",
      h2(class = "mb-0", "References"),
      actionLink("goto_dashboard", "← Back to dashboard", class = "btn btn-outline-secondary btn-sm")
    ),
    div(class = "row justify-content-center",
      div(class = "col-lg-9",
        p(class = "text-muted",
          "Bibliography for the source abstracts: the operating institutions, ",
          "INSPIRE metadata records and sector-classification documents the abstracts cite. ",
          "Maintained in ", tags$code("data/references.bib"),
          "; the mapping from each in-text citation to the claim it supports is in ",
          tags$code("data/source_abstracts_cited.md"), "."),

        if (nrow(refs_df) > 0)
          tags$ul(class = "small",
            lapply(seq_len(nrow(refs_df)), function(i) format_bib_entry(refs_df[i, ])))
        else
          p(em("references.bib not found."))
      )
    )
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
# The whole body is swapped between the landing page, the dashboard and the
# references page via a single uiOutput, driven by a reactiveVal (see main_ui).
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    .source-card { cursor: pointer; transition: box-shadow .15s ease, transform .15s ease; }
    .source-card:hover { box-shadow: 0 .25rem .75rem rgba(0,0,0,.08); transform: translateY(-1px); }
    .source-card-desc {
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
      overflow: hidden;
      min-height: 3.6em;
    }
  "))),
  uiOutput("main_ui")
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Landing page <-> dashboard switch ----
  current_page <- reactiveVal("landing")   # "landing" | "dashboard"
  chosen_risk  <- reactiveVal("Physical")  # which tab the dashboard opens on

  observeEvent(input$goto_physical, {
    chosen_risk("Physical")
    current_page("dashboard")
  })

  observeEvent(input$goto_transition, {
    chosen_risk("Transition")
    current_page("dashboard")
  })

  observeEvent(input$goto_references, current_page("references"))
  observeEvent(input$goto_dashboard,  current_page("dashboard"))

  output$main_ui <- renderUI({
    switch(current_page(),
      "landing"    = landing_page_ui(),
      "references" = references_page_ui(),
      dashboard_ui(selected_tab = chosen_risk())
    )
  })

  # ---- Hazard Select all / Clear ----
  observeEvent(input$hazard_select_all, {
    updateCheckboxGroupInput(session, "hazard_types",
      selected = names(HAZARD_LABELS))   # hazard IDs = values of HAZARD_CHOICES
  })

  observeEvent(input$hazard_clear, {
    updateCheckboxGroupInput(session, "hazard_types", selected = character(0))
  })

  # ---- Reset all filters ----
  observeEvent(input$reset, {
    updateTabsetPanel(session,        "risk_tabs",          selected = "Physical")
    updateTabsetPanel(session,        "physical_subtabs",   selected = "Results")
    updateTabsetPanel(session,        "transition_subtabs", selected = "Results")
    updateCheckboxGroupInput(session, "hazard_types",       selected = character(0))
    updateTextInput(session,          "search_query",       value    = "")
    updateSelectInput(session,        "source_type",        selected = "All")
    updateSelectInput(session,        "relevance_level",    selected = "All")
    updateSelectInput(session,        "technical_effort",   selected = "All")
  })

  # ---- Physical risk tab ----
  selected_physical_id <- reactiveVal(NULL)

  filtered_physical <- reactive({
    filter_sources(
      risk_value           = "Physical",
      hazard_types         = input$hazard_types,
      sel_search           = input$search_query,
      sel_source_type      = input$source_type,
      sel_relevance        = input$relevance_level,
      sel_technical_effort = input$technical_effort
    )
  })

  output$result_count_physical <- renderText({
    sprintf("%d of %d sources match your filters", nrow(filtered_physical()), TOTAL_SOURCES)
  })

  output$results_cards_physical <- renderUI({
    render_card_grid(filtered_physical(), "physical_card_click", selected_physical_id())
  })

  observeEvent(input$physical_card_click, {
    selected_physical_id(input$physical_card_click)
    updateTabsetPanel(session, "physical_subtabs", selected = "Source detail")
  })

  observeEvent(input$physical_back, {
    updateTabsetPanel(session, "physical_subtabs", selected = "Results")
  })

  output$detail_view_physical <- renderUI({
    render_detail_ui(lookup_source(selected_physical_id()), "physical_back")
  })

  # ---- Transition risk tab ----
  selected_transition_id <- reactiveVal(NULL)

  filtered_transition <- reactive({
    filter_sources(
      risk_value           = "Transition",
      sel_search           = input$search_query,
      sel_source_type      = input$source_type,
      sel_relevance        = input$relevance_level,
      sel_technical_effort = input$technical_effort
    )
  })

  output$result_count_transition <- renderText({
    sprintf("%d of %d sources match your filters", nrow(filtered_transition()), TOTAL_SOURCES)
  })

  output$results_cards_transition <- renderUI({
    render_card_grid(filtered_transition(), "transition_card_click", selected_transition_id())
  })

  observeEvent(input$transition_card_click, {
    selected_transition_id(input$transition_card_click)
    updateTabsetPanel(session, "transition_subtabs", selected = "Source detail")
  })

  observeEvent(input$transition_back, {
    updateTabsetPanel(session, "transition_subtabs", selected = "Results")
  })

  output$detail_view_transition <- renderUI({
    render_detail_ui(lookup_source(selected_transition_id()), "transition_back")
  })
}

shinyApp(ui, server)
