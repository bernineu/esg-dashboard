---
editor_options: 
  markdown: 
    wrap: 72
---

# ESG Data Source Matrix — Decision-Support Dashboard

R Shiny prototype (Artefact 3) for identifying external ESG data sources
relevant to Template D 01.01 (Austrian SNCIs), built on top of the ESG
Data Source Matrix (Artefact 2). See thesis Section 2.5 for the design
rationale.

## How to open this project

1.  Open `esg-dashboard.Rproj` in RStudio (double-click, or *File \>
    Open Project*). RStudio sets the working directory automatically —
    all paths are relative to the project root, so no manual `setwd()`
    is needed.

2.  Install required packages (first time only):

    ``` r
    source("requirements.R")
    ```

3.  Open `app.R` and click **Run App** (top-right of the source pane),
    or run:

    ``` r
    shiny::runApp()
    ```

## Required packages

Run `requirements.R` once before launching the app — it installs only
what is missing:

``` r
source("requirements.R")
```

The full package list with descriptions is maintained in
`requirements.R`. Any reasonably recent CRAN version of each package
should work.

## Project structure

```         
esg-dashboard/
├── esg-dashboard.Rproj    # RStudio project file — open this first
├── app.R                  # Shiny app (UI + server)
├── requirements.R         # installs all required packages
├── README.md              # this file
├── R/
│   └── load_data.R        # data loading, cleaning, hazard-label lookup,
│                          # sources_covering_hazards() helper
└── data/
    ├── sources.csv         # source master table (19 rows)
    ├── hazard_coverage.csv # long format: source_id x hazard_id x coverage
    ├── d01_mapping.csv     # long format: source_id x D 01.01 data point
    └── source_details.csv  # long format: source_id x field x free text
```

## Data model

All four CSVs are generated from the same underlying data as the thesis
Excel artefact (`ESG_Data_Source_Matrix_Structured.xlsx`, Appendix C),
so the dashboard and the printed matrix stay consistent — see the "Note
on data model" entry in that workbook's Legend sheet.

-   **`sources.csv`** — one row per source. Key columns used by the app:
    `source_id`, `source_name`, `risk_type` (Physical/Transition/Both),
    `relevance_level`, `source_type`, `cost_category`,
    `technical_effort` (ordered factor: Low \< Medium \< High),
    `access_status` (legal usability), `api` (Yes/No),
    `portfolio_ready`, `url`, `last_checked`.
-   **`hazard_coverage.csv`** — one row per source × hazard combination,
    `coverage` ∈ {none, partial, full}. Drives the Tier 2 (hazard type)
    filter and the hazard coverage badges in the detail view.
-   **`d01_mapping.csv`** — links each source to the specific D 01.01 /
    DPM data point(s) it is relevant for (primary/secondary), shown in
    the detail view.
-   **`source_details.csv`** — long-form rationale text (processing
    effort, limitations, suitability, licensing notes), shown only when
    a row is selected.

## Navigation logic (Section 2.5)

-   **Landing page** — on open, the app shows an introduction plus a choice
    between Physical risk and Transition risk. Picking one opens the
    dashboard on that tab; the dashboard itself keeps both tabs visible
    afterwards, so returning to the landing page is not required to
    switch risk type.
-   **Tier 1** — Risk type: Physical or Transition, selected via the two
    top-level tabs in the main panel. Each tab keeps its own Results /
    Source detail sub-tabs and row selection, so switching between
    Physical and Transition doesn't lose your place in either.
-   **Tier 2** (Physical branch only) — Hazard type: multi-select
    checkboxes over the 12 hazard types from Table X (Section 2.4),
    shown in the sidebar only while the Physical risk tab is active. No
    Tier 2 exists for the Transition branch, since NACE classification
    sources apply uniformly across subsectors (see Section 2.5).
-   **Facets** (applied regardless of tier) — source type, cost, maximum
    technical effort, legal usability, and API availability. These
    dimensions determine practical usability independently of topical
    relevance (e.g. the HORA case).

## Dashboard features

-   **Quick-start instructions** — the landing page carries a short
    walkthrough alongside the Physical/Transition choice, shown once
    before entering the dashboard.
-   **Result count** — shows how many sources match the current filters
    out of the total.
-   **Relevance legend** — collapsible definitions for all four
    relevance levels (Primary, Supplementary, Context only,
    Methodological).
-   **Hazard select all / clear** — quickly select or deselect all 12
    hazard checkboxes.
-   **Reset all filters** — single button to restore all filters to
    their defaults.
-   **Column visibility** — toggle individual table columns on/off via
    the column visibility button above the results table.
-   **Source detail tab** — clicking a row switches to a dedicated
    detail tab showing: source URL and last-checked date, key metadata,
    D 01.01 mapping, hazard coverage badges (color-coded: green = full,
    amber = partial), and long-form rationale text.
-   **Portfolio-ready badges** — color-coded (green / amber / grey) in
    the results table for quick scanning.

## Known limitations

-   No portfolio-input feature (deliberately out of scope — see Section
    2.5).
-   The `technical_effort` filter is a maximum-effort ceiling; requiring
    a minimum effort level is not supported (unlikely to be needed in
    practice).
-   This is a functional prototype for thesis evaluation (Section 2.6),
    not a production application — no authentication, no persistent
    storage, no automated data refresh from the source registers.
