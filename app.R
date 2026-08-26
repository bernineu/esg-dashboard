# ============================================================
# ESG Data Source Matrix - R Shiny Prototype (Artefact 3)
#
# Implements the navigation logic described in thesis Section 2.5:
#   Tier 1: risk type (Physical / Transition)
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
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  titlePanel("ESG Data Source Matrix \u2014 Decision-Support Dashboard"),
  p(class = "text-muted",
    "Decision-support tool for identifying external ESG data sources for Template D\u00a001.01 (Austrian SNCIs). ",
    "Based on Artefact 2 (ESG Data Source Matrix) \u2014 thesis Section\u00a02.5."),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ---- Tier 1: risk type ----
      h5("1. Risk type"),
      radioButtons(
        "risk_type", NULL,
        choices  = c("Physical risk" = "Physical", "Transition risk" = "Transition"),
        selected = "Physical"
      ),

      # ---- Tier 2: hazard type (physical-risk branch only) ----
      conditionalPanel(
        condition = "input.risk_type == 'Physical'",
        h5("2. Hazard type"),
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
        condition = "input.risk_type == 'Transition'",
        div(class = "alert alert-info p-2 mb-2",
          tags$small(
            "Transition risk sources cover NACE sector classification uniformly ",
            "\u2014 no hazard-type filter applies."
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
      helpText("Prototype for Artefact 3 (Section\u00a02.5). ",
               "Data: data/*.csv, generated from the same source as thesis Appendix\u00a0C.")
    ),

    mainPanel(
      width = 9,

      # Result count + collapsible relevance legend
      div(class = "d-flex justify-content-between align-items-start mb-2",
        tagAppendAttributes(
          textOutput("result_count", inline = TRUE),
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
      ),

      tabsetPanel(
        id = "main_tabs",
        tabPanel("Results",
          br(),
          DTOutput("results_table")
        ),
        tabPanel("Source detail",
          br(),
          uiOutput("detail_view")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------
server <- function(input, output, session) {

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
    updateRadioButtons(session,       "risk_type",        selected = "Physical")
    updateCheckboxGroupInput(session, "hazard_types",     selected = character(0))
    updateSelectInput(session,        "source_type",      selected = "All")
    updateSelectInput(session,        "operator_type",    selected = "All")
    updateSelectInput(session,        "cost_category",    selected = "All")
    updateSelectInput(session,        "technical_effort", selected = "All")
    updateSelectInput(session,        "access_status",    selected = "All")
    updateCheckboxInput(session,      "has_api",          value    = FALSE)
  })

  # ---- Filtered data ----
  filtered_sources <- reactive({
    df <- sources_df %>% filter(risk_type == input$risk_type | risk_type == "Both")

    # Tier 2: hazard filter (physical-risk branch only)
    # input$hazard_types now contains hazard IDs directly (e.g. "heat_stress")
    if (input$risk_type == "Physical" && length(input$hazard_types) > 0) {
      covering_ids <- sources_covering_hazards(
        hazard_cov, input$hazard_types, min_coverage = "partial")
      df <- df %>% filter(source_id %in% covering_ids)
    }

    # Facets
    if (input$source_type      != "All") df <- df %>% filter(source_type      == input$source_type)
    if (input$operator_type    != "All") df <- df %>% filter(operator_type    == input$operator_type)
    if (input$cost_category    != "All") df <- df %>% filter(cost_category    == input$cost_category)
    if (input$technical_effort != "All") {
      max_level <- factor(input$technical_effort, levels = c("Low", "Medium", "High"), ordered = TRUE)
      df <- df %>% filter(technical_effort <= max_level)
    }
    if (input$access_status    != "All") df <- df %>% filter(access_status    == input$access_status)
    if (input$has_api)                   df <- df %>% filter(api              == "Yes")

    df %>%
      select(source_id, source_name, operator, source_type, operator_type,
             relevance_level, geographic_scope, granularity_level,
             cost_category, technical_effort, access_status, portfolio_ready)
  })

  # ---- Result count ----
  output$result_count <- renderText({
    sprintf("%d of %d sources match your filters", nrow(filtered_sources()), TOTAL_SOURCES)
  })

  # ---- Results table ----
  output$results_table <- renderDT({
    df <- filtered_sources()

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
  })

  # Auto-switch to detail tab when a row is selected
  observeEvent(input$results_table_rows_selected, {
    if (length(input$results_table_rows_selected) > 0) {
      updateTabsetPanel(session, "main_tabs", selected = "Source detail")
    }
  })

  # ---- Source detail ----
  output$detail_view <- renderUI({
    sel <- input$results_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(p(em("No source selected. Click a row in the Results tab.")))
    }

    row      <- filtered_sources()[sel, ]
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
        h6("D\u00a001.01 Mapping"),
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
  })
}

shinyApp(ui, server)
