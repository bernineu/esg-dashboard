# ============================================================
# ui_pages.R
# The three top-level views the server swaps between via output$main_ui:
#   landing_page_ui()  - intro + Physical/Transition choice (shown on open)
#   dashboard_ui()     - navbar (risk types + search + References) over a
#                        shared filter sidebar; built from filter_sidebar(),
#                        navbar_search() and risk_nav_panel()
#   references_page_ui() - references.bib as an alphabetical bibliography
# plus app_ui(), the outer page shell.
# ============================================================

# ---- Landing page ------------------------------------------------------

# Introduction + the Physical/Transition risk choice. Shown once when the
# app opens; the dashboard navbar keeps both risk types one click away
# afterwards, so returning here is not required to switch.
landing_page_ui <- function() {
  tagList(
    div(class = "text-center mt-5 mb-4",
      h1("ESG Data Procurement for SNCIs"),
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
          tags$li("Narrow the results with the ", tags$strong("search"),
                  " box in the navbar and the facet filters in the sidebar ",
                  "(hazard type for physical risk; source type, relevance level, ",
                  "max. effort). Each filter's ", tags$strong("ⓘ"), " explains it."),
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
            div(class = "small fw-normal risk-choice-sub",
                "Climate hazards: heat, flood, drought, storm, ...")
          ),
          class = "btn btn-outline-primary p-4 risk-choice", style = "min-width: 280px;")
      ),
      div(class = "col-auto",
        actionButton("goto_transition",
          tagList(
            div(class = "fs-5 fw-bold", "Transition risk"),
            div(class = "small fw-normal risk-choice-sub",
                "NACE sector classification sources")
          ),
          class = "btn btn-outline-primary p-4 risk-choice", style = "min-width: 280px;")
      )
    )
  )
}

# ---- Dashboard: filter sidebar ---------------------------------------

# Short explanations for the facet filters, taken from the Data Source
# Matrix / workbook Legend sheet. Surfaced as a small "ⓘ" next to each
# label (native browser tooltip, upgraded to a Bootstrap tooltip by the
# init script in app_ui() where Bootstrap's JS is available).
FACET_TIPS <- list(
  search    = paste(
    "Case-insensitive. Matches the source name, operator or short",
    "description; the words can appear in any order."),
  hazard    = paste(
    "Tier 2 filter (Section 2.4): the physical hazard types with at least",
    "one assessed source. A source is kept only if it covers every ticked",
    "hazard at partial or full coverage (or at full coverage only, if",
    "\"Only full coverage\" is ticked below). Hazards with no covering",
    "source (e.g. glacial lake outburst flood) aren't listed here - see",
    "the hazard-coverage matrix (Appendix C) for the full 12-hazard",
    "classification, gaps included."),
  hazard_full_only = paste(
    "Off (default): a ticked hazard counts as covered at partial or full",
    "coverage (Data Source Matrix legend). On: only sources with full",
    "coverage of every ticked hazard are kept - a stricter bar, so with",
    "several hazards ticked this can return very few or no sources."),
  source_type = paste(
    "Public = free / open data (institutional or open-government).",
    "Commercial = paid, requires a licence agreement."),
  relevance = paste(
    "How the source relates to the D 01.01 data point (Data Source Matrix",
    "legend). Primary: directly usable as an operational data source.",
    "Supplementary: complements a primary source, not sufficient alone.",
    "Context only: screening or benchmarking use, not a usable data",
    "source."),
  effort    = paste(
    "Effort to turn the source's own raw data into an exposure classification",
    "- format conversion, geocoding, spatial overlay (Data Source Matrix",
    "legend). Low: one well-structured layer or API. Medium: several layers",
    "or a complex structure needing interpretation. High: substantial extra",
    "processing, or no bulk access. The filter keeps sources at the chosen",
    "level or below.")
)

.facet_label <- function(text, tip) {
  tagList(
    text, " ",
    tags$span(
      class = "facet-info text-muted", style = "cursor: help;",
      tabindex = "0", title = tip,
      `data-bs-toggle` = "tooltip", `data-bs-placement` = "right",
      # the icon sits inside a <label for=...>; don't let a click on it
      # open/toggle the associated input.
      onclick = "event.preventDefault(); event.stopPropagation();",
      "ⓘ"
    )
  )
}

# Shared across the risk-type nav panels (bslib::sidebar, so the inputs
# are defined once). The whole filter block is hidden while a "Source
# detail" sub-tab is open, replaced by a short hint.
filter_sidebar <- function() {
  # JS predicate: true while a source-detail sub-tab is open on either risk tab.
  in_detail_view <- paste(
    "(input.risk_tabs == 'Physical' && input.physical_subtabs == 'Source detail')",
    "|| (input.risk_tabs == 'Transition' && input.transition_subtabs == 'Source detail')"
  )

  sidebar(
    width = 320,
    title = "Filters",

    # ---- Filters (hidden while a source detail view is open; the free-text
    #      search lives in the navbar, see dashboard_ui()) ----
    conditionalPanel(
      condition = paste0("!(", in_detail_view, ")"),

      # Tier 2: hazard type (physical-risk tab only)
      conditionalPanel(
        condition = "input.risk_tabs == 'Physical'",
        h5(.facet_label("Hazard type", FACET_TIPS$hazard)),
        div(class = "mb-1",
          actionLink("hazard_select_all", "Select all", class = "small"),
          " | ",
          actionLink("hazard_clear", "Clear", class = "small")
        ),
        checkboxGroupInput(
          "hazard_types", NULL,
          choices  = HAZARD_CHOICES_ACTIVE,
          selected = character(0)
        ),
        checkboxInput(
          "hazard_full_only",
          .facet_label("Only full coverage", FACET_TIPS$hazard_full_only),
          value = FALSE
        ),
        helpText("Leave empty to show all physical-risk sources. ",
                 "Ticking several hazards shows only sources that cover ",
                 tags$strong("all"), " of them.")
      ),

      # Transition risk has no Tier 2 - a quiet note in the footer-helpText
      # style, not a coloured callout (the facets below are the point).
      conditionalPanel(
        condition = "input.risk_tabs == 'Transition'",
        helpText("No hazard-type filter here — transition-risk sources cover ",
                 "NACE sector classification uniformly.")
      ),

      hr(),
      h5("Facets"),

      selectInput(
        "source_type", .facet_label("Source type", FACET_TIPS$source_type),
        choices  = c("All", sort(unique(sources_df$source_type))),
        selected = "All"
      ),
      selectInput(
        "relevance_level", .facet_label("Relevance level", FACET_TIPS$relevance),
        choices  = c("All", intersect(names(RELEVANCE_DEFS),
                                      unique(sources_df$relevance_level))),
        selected = "All"
      ),
      selectInput(
        "technical_effort", .facet_label("Max. effort", FACET_TIPS$effort),
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

# ---- Dashboard: one risk-type nav panel -----------------------------

# Result-count header + Results / Source detail sub-tabs. The id strings
# passed here are the contract with wire_risk_panel() in the server.
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

# ---- Dashboard shell ------------------------------------------------

# Free-text search field for the navbar. Applies across both risk panels
# (server reads input$search_query) and stays available even in the
# detail view, unlike the sidebar facets.
navbar_search <- function() {
  fld <- textInput("search_query", label = NULL, width = "15rem",
                   placeholder = "Search sources…")
  fld <- tagAppendAttributes(fld, class = "mb-0")
  fld <- htmltools::tagQuery(fld)$find("input")$
    addAttrs(title = FACET_TIPS$search, `aria-label` = "Search sources")$
    allTags()
  nav_item(div(class = "navbar-search", fld))
}

# A top navbar (Physical risk / Transition risk, a search field and a
# right-aligned References link) over the shared filter sidebar.
# `selected_tab` sets which risk panel opens first.
dashboard_ui <- function(selected_tab) {
  navset_bar(
    id       = "risk_tabs",
    title    = "ESG Data Procurement for SNCIs",
    selected = selected_tab,
    fillable = FALSE,
    sidebar  = filter_sidebar(),

    risk_nav_panel("Physical risk", "Physical", "physical_subtabs",
      "results_cards_physical", "detail_view_physical", "result_count_physical"),
    risk_nav_panel("Transition risk", "Transition", "transition_subtabs",
      "results_cards_transition", "detail_view_transition", "result_count_transition"),

    nav_spacer(),
    navbar_search(),
    nav_item(actionLink("goto_references", "References"))
  )
}

# ---- References page ----------------------------------------------

# One <li> for a bibliography entry from references.bib.
format_bib_entry <- function(r) {
  head <- paste0(r$author, " (", r$year, "). ", r$title,
                 if (grepl("[.!?]$", r$title)) "" else ".")
  extra_note <- sub("^\\([^)]*\\)\\.?\\s*", "", r$note)   # note text after the (Author, year)
  link <- if (nzchar(r$url)) tagList(
    if (nzchar(r$urldate)) paste0(" Retrieved ", r$urldate, ", from ") else " ",
    tags$a(href = r$url, target = "_blank", rel = "noopener noreferrer", r$url)
  )
  tags$li(class = "mb-2",
    head, link,
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

# ---- Outer page shell --------------------------------------------

# The whole body is swapped between the landing page, the dashboard and
# the references page via the single uiOutput("main_ui"), driven by a
# reactiveVal in the server.
app_ui <- function() {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    tags$head(
      includeCSS("www/styles.css"),
      # Upgrade the facet "ⓘ" title tooltips to Bootstrap tooltips where
      # Bootstrap's JS is present; re-run after each renderUI of #main_ui.
      # The native `title` attribute is the fallback if this no-ops.
      tags$script(HTML("
        $(function () {
          function initTooltips() {
            if (typeof bootstrap === 'undefined' || !bootstrap.Tooltip) return;
            document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function (el) {
              if (!bootstrap.Tooltip.getInstance(el)) new bootstrap.Tooltip(el);
            });
          }
          $(document).on('shiny:value', function () { setTimeout(initTooltips, 0); });
          setTimeout(initTooltips, 400);
        });
      "))
    ),
    uiOutput("main_ui")
  )
}
