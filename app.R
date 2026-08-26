# ============================================================
# ESG Data Source Matrix - R Shiny Prototype (Artefact 3)
#
# Implements the navigation logic described in thesis Section 2.5:
#   Landing page: introduction + Physical/Transition risk choice
#   Tier 1: risk type (Physical / Transition) - top-level tabs in the dashboard
#   Tier 2: hazard type (physical-risk branch only, per Section 2.4 Table X)
#   Facets: source type, operator type, technical effort, legal usability, API
#           (applied across whichever tier(s) the user has navigated through)
#
# Results are shown as a clickable card grid (source, description, operator,
# plus at-a-glance badges); clicking a card opens a detailed view in the
# Source detail sub-tab. cost_category is intentionally not used anywhere in
# the app - in this data set it is a 1:1 restatement of source_type
# (Public = Free, Commercial = Paid), so source_type alone covers it without
# a redundant facet/badge. The column stays in data/sources.csv untouched,
# to keep the CSV 1:1 with the thesis Excel artefact (Appendix C).
#
# Data layer: R/load_data.R reads the four normalized CSVs described in
# Appendix C (sources.csv, hazard_coverage.csv, d01_mapping.csv,
# source_details.csv), keeping the dashboard's data 1:1 with the thesis
# artefact rather than a separate, undocumented dataset.
# ============================================================

library(shiny)
library(bslib)
library(dplyr)

source("R/load_data.R")

sources_df  <- load_sources()
hazard_cov  <- load_hazard_coverage()
d01_mapping <- load_d01_mapping()
details_df  <- load_source_details()

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

# ------------------------------------------------------------
# Shared helpers (pure functions, reused for both the Physical
# and Transition tabs so the filtering/rendering logic lives once)
# ------------------------------------------------------------

# Filters sources_df for one risk-type tab. Parameters are prefixed with
# "sel_" so they never collide with the sources_df column names of the
# same name inside dplyr::filter()'s data-masking evaluation.
filter_sources <- function(risk_value,
                            hazard_types         = character(0),
                            sel_source_type      = "All",
                            sel_operator_type    = "All",
                            sel_technical_effort = "All",
                            sel_access_status    = "All",
                            sel_has_api          = FALSE) {
  df <- sources_df %>% filter(risk_type == risk_value | risk_type == "Both")

  # Tier 2: hazard filter (physical-risk branch only)
  if (risk_value == "Physical" && length(hazard_types) > 0) {
    covering_ids <- sources_covering_hazards(
      hazard_cov, hazard_types, min_coverage = "partial")
    df <- df %>% filter(source_id %in% covering_ids)
  }

  # Facets
  if (sel_source_type      != "All") df <- df %>% filter(source_type      == sel_source_type)
  if (sel_operator_type    != "All") df <- df %>% filter(operator_type    == sel_operator_type)
  if (sel_technical_effort != "All") {
    max_level <- factor(sel_technical_effort, levels = c("Low", "Medium", "High"), ordered = TRUE)
    df <- df %>% filter(technical_effort <= max_level)
  }
  if (sel_access_status != "All") df <- df %>% filter(access_status == sel_access_status)
  if (sel_has_api)                df <- df %>% filter(api == "Yes")

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
  tags$span(class = cls, paste("Portfolio:", p))
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
render_detail_ui <- function(src) {
  if (is.null(src) || nrow(src) == 0) {
    return(div(class = "text-muted fst-italic text-center p-5",
      "No source selected. Click a card in the Results tab."))
  }

  sid <- src$source_id
  sd  <- details_df  %>% filter(source_id == sid)
  dm  <- d01_mapping %>% filter(source_id == sid)
  haz <- hazard_cov   %>% filter(source_id == sid)

  metadata_items <- list(
    list(label = "Operator type",    value = src$operator_type),
    list(label = "Relevance",        value = as.character(src$relevance_level)),
    list(label = "Source type",      value = src$source_type),
    list(label = "Tech. effort",     value = as.character(src$technical_effort)),
    list(label = "Legal access",     value = src$access_status),
    list(label = "Portfolio-ready",  value = as.character(src$portfolio_ready))
  )

  tagList(

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
        div(class = "border rounded-3 px-3 py-2 text-center bg-light", style = "min-width: 130px;",
          div(class = "text-muted text-uppercase", style = "font-size:.7rem; letter-spacing:.03em;", m$label),
          div(class = "fw-semibold", m$value)
        )
      })
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
      if (nrow(covered) > 0) tagList(
        h6(class = "text-uppercase text-muted mb-2", style = "font-size:.75rem; letter-spacing:.03em;",
           "Hazard coverage"),
        div(class = "d-flex flex-wrap gap-2 mb-4",
          lapply(seq_len(nrow(covered)), function(i) {
            label       <- HAZARD_LABELS[covered$hazard_id[i]]
            badge_class <- switch(as.character(covered$coverage[i]),
              "full"    = "badge bg-success",
              "partial" = "badge bg-warning text-dark",
              "badge bg-secondary"
            )
            tags$span(class = badge_class,
              paste0(label, " (", covered$coverage[i], ")"))
          })
        )
      )
    },

    # ---- Detail text fields ----
    if (nrow(sd) > 0) tagList(
      h6(class = "text-uppercase text-muted mb-2", style = "font-size:.75rem; letter-spacing:.03em;",
         "Detail"),
      div(class = "d-flex flex-column gap-2",
        lapply(seq_len(nrow(sd)), function(i) {
          div(class = "border rounded-3 p-3 bg-light",
            div(class = "fw-semibold small text-secondary mb-1", sd$field[i]),
            div(class = "small", sd$text[i])
          )
        })
      )
    ) else {
      p(em("No additional detail text recorded for this source."))
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
                  " and the shared facet filters (source type, operator type, technical ",
                  "effort, legal usability, API access) in the sidebar."),
          tags$li("Click any card in the ", tags$strong("Results"), " tab to open its ",
                  tags$strong("Source detail"), " tab: D 01.01 mapping, hazard coverage, ",
                  "and rationale text."),
          tags$li("Both risk types stay one click away via the tabs at the top of the ",
                  "dashboard - switch anytime.")
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

# Main dashboard UI (sidebar filters + risk-type tabs). `selected_tab` sets
# which of the two top-level tabs opens first, based on the landing-page choice.
dashboard_ui <- function(selected_tab) {
  tagList(
    titlePanel("ESG Data Source Matrix - Decision-Support Dashboard"),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        # ---- Tier 2: hazard type (physical-risk tab only) ----
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
          helpText("Leave empty to show all physical-risk sources.")
        ),

        # ---- Transition-risk note (replaces the missing Tier 2) ----
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
          "operator_type", "Operator type",
          choices  = c("All", sort(unique(sources_df$operator_type))),
          selected = "All"
        ),
        selectInput(
          "technical_effort", "Max. technical effort",
          choices  = c("All", "Low", "Medium", "High"),
          selected = "All"
        ),
        selectInput(
          "access_status", "Legal usability",
          choices  = c("All", sort(unique(sources_df$access_status))),
          selected = "All"
        ),
        checkboxInput("has_api", "Has API access only", value = FALSE),

        hr(),
        actionButton("reset", "Reset all filters",
          class = "btn-outline-secondary btn-sm w-100"),

        hr(),
        helpText("Prototype for Artefact 3 (Section 2.5). ",
                 "Data: data/*.csv, generated from the same source as thesis Appendix C.")
      ),

      mainPanel(
        width = 9,

        tabsetPanel(
          id       = "risk_tabs",
          selected = selected_tab,

          tabPanel("Physical risk", value = "Physical",
            br(),
            result_header_ui("result_count_physical"),
            tabsetPanel(
              id = "physical_subtabs",
              tabPanel("Results",
                br(),
                uiOutput("results_cards_physical")
              ),
              tabPanel("Source detail",
                br(),
                uiOutput("detail_view_physical")
              )
            )
          ),

          tabPanel("Transition risk", value = "Transition",
            br(),
            result_header_ui("result_count_transition"),
            tabsetPanel(
              id = "transition_subtabs",
              tabPanel("Results",
                br(),
                uiOutput("results_cards_transition")
              ),
              tabPanel("Source detail",
                br(),
                uiOutput("detail_view_transition")
              )
            )
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
# The whole body is swapped between the landing page and the dashboard via
# a single uiOutput, driven by a reactiveVal in the server (see main_ui below).
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

  output$main_ui <- renderUI({
    if (current_page() == "landing") {
      landing_page_ui()
    } else {
      dashboard_ui(selected_tab = chosen_risk())
    }
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
    updateSelectInput(session,        "source_type",        selected = "All")
    updateSelectInput(session,        "operator_type",      selected = "All")
    updateSelectInput(session,        "technical_effort",   selected = "All")
    updateSelectInput(session,        "access_status",      selected = "All")
    updateCheckboxInput(session,      "has_api",            value    = FALSE)
  })

  # ---- Physical risk tab ----
  selected_physical_id <- reactiveVal(NULL)

  filtered_physical <- reactive({
    filter_sources(
      risk_value           = "Physical",
      hazard_types         = input$hazard_types,
      sel_source_type      = input$source_type,
      sel_operator_type    = input$operator_type,
      sel_technical_effort = input$technical_effort,
      sel_access_status    = input$access_status,
      sel_has_api          = input$has_api
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

  output$detail_view_physical <- renderUI({
    render_detail_ui(lookup_source(selected_physical_id()))
  })

  # ---- Transition risk tab ----
  selected_transition_id <- reactiveVal(NULL)

  filtered_transition <- reactive({
    filter_sources(
      risk_value           = "Transition",
      sel_source_type      = input$source_type,
      sel_operator_type    = input$operator_type,
      sel_technical_effort = input$technical_effort,
      sel_access_status    = input$access_status,
      sel_has_api          = input$has_api
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

  output$detail_view_transition <- renderUI({
    render_detail_ui(lookup_source(selected_transition_id()))
  })
}

shinyApp(ui, server)
