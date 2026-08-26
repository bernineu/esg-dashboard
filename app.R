# ============================================================
# ESG Data Source Matrix - R Shiny Prototype (Artefact 3)
#
# Implements the navigation logic described in thesis Section 2.5:
#   Landing page: introduction + Physical/Transition risk choice
#   Tier 1: risk type (Physical / Transition) - top-level tabs in the dashboard
#   Tier 2: hazard type (physical-risk branch only, per Section 2.4 Table X)
#   Facets: source type, cost, technical effort, legal usability
#           (applied across whichever tier(s) the user has navigated through)
#
# Data layer: R/load_data.R reads the four normalized CSVs described in
# Appendix C (sources.csv, hazard_coverage.csv, d01_mapping.csv,
# source_details.csv), keeping the dashboard's data 1:1 with the thesis
# artefact rather than a separate, undocumented dataset.
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(DT)

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
                            sel_cost_category    = "All",
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
  if (sel_cost_category    != "All") df <- df %>% filter(cost_category    == sel_cost_category)
  if (sel_technical_effort != "All") {
    max_level <- factor(sel_technical_effort, levels = c("Low", "Medium", "High"), ordered = TRUE)
    df <- df %>% filter(technical_effort <= max_level)
  }
  if (sel_access_status != "All") df <- df %>% filter(access_status == sel_access_status)
  if (sel_has_api)                df <- df %>% filter(api == "Yes")

  df %>%
    select(source_id, source_name, operator, source_type, operator_type,
           relevance_level, geographic_scope, granularity_level,
           cost_category, technical_effort, access_status, portfolio_ready)
}

# Renders the results DT for a given (already filtered) data frame.
render_results_datatable <- function(df) {
  # Render portfolio_ready as Bootstrap badges
  df$portfolio_ready <- dplyr::case_when(
    as.character(df$portfolio_ready) == "Yes"    ~
      '<span class="badge bg-success">Yes</span>',
    as.character(df$portfolio_ready) == "Partly" ~
      '<span class="badge bg-warning text-dark">Partly</span>',
    as.character(df$portfolio_ready) == "No"     ~
      '<span class="badge bg-secondary">No</span>',
    TRUE ~ as.character(df$portfolio_ready)
  )

  datatable(
    df,
    escape     = FALSE,
    selection  = "single",
    rownames   = FALSE,
    colnames   = c("ID", "Source", "Operator", "Type", "Operator type",
                   "Relevance", "Geo. scope", "Granularity", "Cost",
                   "Tech. effort", "Legal access", "Portfolio-ready"),
    extensions = "Buttons",
    options    = list(
      pageLength  = 10,
      scrollX     = TRUE,
      dom         = "Bfrtip",
      buttons     = list("colvis"),
      # Hide ID (0), Operator (2), Granularity (7) by default; user can re-enable via column visibility button
      # Column order: ID(0), Source(1), Operator(2), Type(3), Operator type(4), Relevance(5),
      #               Geo.scope(6), Granularity(7), Cost(8), Tech.effort(9), Legal access(10), Portfolio-ready(11)
      columnDefs  = list(
        list(visible = FALSE, targets = c(0, 2, 7))
      )
    )
  ) %>%
    formatStyle(
      "relevance_level",
      backgroundColor = styleEqual(
        c("Primary",  "Supplementary", "Context only", "Methodological"),
        c("#d4edda",  "#fff3cd",       "#f8f9fa",      "#d1ecf1")
      )
    )
}

# Renders the source-detail pane for a single selected row (data frame with
# 0 or 1 rows, as produced by DT's single-row selection).
render_detail_ui <- function(sel_row_df) {
  if (is.null(sel_row_df) || nrow(sel_row_df) == 0) {
    return(p(em("No source selected. Click a row in the Results tab.")))
  }

  row      <- sel_row_df[1, ]
  sid      <- row$source_id
  sd       <- details_df  %>% filter(source_id == sid)
  dm       <- d01_mapping %>% filter(source_id == sid)
  haz      <- hazard_cov  %>% filter(source_id == sid)
  src_full <- sources_df  %>% filter(source_id == sid)

  tagList(

    h4(row$source_name),
    p(class = "text-muted mb-1", src_full$short_description),
    tags$a(
      href   = paste0("https://", src_full$url),
      src_full$url,
      target = "_blank",
      rel    = "noopener noreferrer"
    ),
    tags$small(class = "text-muted ms-2",
      paste("Last checked:", src_full$last_checked)),

    hr(),

    # Key metadata row
    div(class = "row g-3 mb-3",
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Operator type"),
        tags$strong(src_full$operator_type)),
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Relevance"),
        tags$strong(row$relevance_level)),
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Cost"),
        tags$strong(as.character(row$cost_category))),
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Tech. effort"),
        tags$strong(as.character(row$technical_effort))),
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Legal access"),
        tags$strong(row$access_status)),
      div(class = "col-auto",
        tags$small(class = "text-muted d-block", "Portfolio-ready"),
        tags$strong(as.character(row$portfolio_ready)))
    ),

    # D 01.01 mapping
    if (nrow(dm) > 0) tagList(
      h6("D 01.01 Mapping"),
      tags$ul(class = "mb-3",
        lapply(seq_len(nrow(dm)), function(i) {
          tags$li(sprintf("%s (%s)", dm$data_point[i], dm$relevance[i]))
        })
      )
    ),

    # Hazard coverage badges (physical sources only; skip sources with all-none coverage)
    if (nrow(haz) > 0) {
      covered <- haz %>% filter(coverage != "none")
      if (nrow(covered) > 0) tagList(
        h6("Hazard coverage"),
        div(class = "d-flex flex-wrap gap-2 mb-3",
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

    # Detail text fields
    if (nrow(sd) > 0) tagList(
      h6("Detail"),
      tags$dl(class = "mb-0",
        lapply(seq_len(nrow(sd)), function(i) {
          tagList(
            tags$dt(sd$field[i]),
            tags$dd(class = "ms-3 mb-2", sd$text[i])
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
                  " and the shared facet filters (source type, cost, technical effort, ",
                  "legal usability, API access) in the sidebar."),
          tags$li("Click any row in the ", tags$strong("Results"), " table to open its ",
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
          "cost_category", "Cost",
          choices  = c("All", sort(unique(sources_df$cost_category))),
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
                DTOutput("results_table_physical")
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
                DTOutput("results_table_transition")
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
  uiOutput("main_ui")
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Landing page <-> dashboard switch ----
  current_page   <- reactiveVal("landing")   # "landing" | "dashboard"
  chosen_risk    <- reactiveVal("Physical")  # which tab the dashboard opens on

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
    updateSelectInput(session,        "cost_category",      selected = "All")
    updateSelectInput(session,        "technical_effort",   selected = "All")
    updateSelectInput(session,        "access_status",      selected = "All")
    updateCheckboxInput(session,      "has_api",            value    = FALSE)
  })

  # ---- Physical risk tab ----
  filtered_physical <- reactive({
    filter_sources(
      risk_value           = "Physical",
      hazard_types         = input$hazard_types,
      sel_source_type      = input$source_type,
      sel_operator_type    = input$operator_type,
      sel_cost_category    = input$cost_category,
      sel_technical_effort = input$technical_effort,
      sel_access_status    = input$access_status,
      sel_has_api          = input$has_api
    )
  })

  output$result_count_physical <- renderText({
    sprintf("%d of %d sources match your filters", nrow(filtered_physical()), TOTAL_SOURCES)
  })

  output$results_table_physical <- renderDT({
    render_results_datatable(filtered_physical())
  })

  observeEvent(input$results_table_physical_rows_selected, {
    if (length(input$results_table_physical_rows_selected) > 0) {
      updateTabsetPanel(session, "physical_subtabs", selected = "Source detail")
    }
  })

  output$detail_view_physical <- renderUI({
    render_detail_ui(filtered_physical()[input$results_table_physical_rows_selected, ])
  })

  # ---- Transition risk tab ----
  filtered_transition <- reactive({
    filter_sources(
      risk_value           = "Transition",
      sel_source_type      = input$source_type,
      sel_operator_type    = input$operator_type,
      sel_cost_category    = input$cost_category,
      sel_technical_effort = input$technical_effort,
      sel_access_status    = input$access_status,
      sel_has_api          = input$has_api
    )
  })

  output$result_count_transition <- renderText({
    sprintf("%d of %d sources match your filters", nrow(filtered_transition()), TOTAL_SOURCES)
  })

  output$results_table_transition <- renderDT({
    render_results_datatable(filtered_transition())
  })

  observeEvent(input$results_table_transition_rows_selected, {
    if (length(input$results_table_transition_rows_selected) > 0) {
      updateTabsetPanel(session, "transition_subtabs", selected = "Source detail")
    }
  })

  output$detail_view_transition <- renderUI({
    render_detail_ui(filtered_transition()[input$results_table_transition_rows_selected, ])
  })
}

shinyApp(ui, server)
