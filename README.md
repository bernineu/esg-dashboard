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
├── app.R                  # entry point: loads data, wires ui + server
├── requirements.R         # installs all required packages
├── README.md              # this file
├── R/                     # app modules, sourced by app.R in order
│   ├── _disable_autoload.R # turns off Shiny's own R/ auto-sourcing
│   ├── load_data.R        # CSV loaders, references.bib parser, hazard
│   │                      # labels, sources_covering_hazards()
│   ├── config.R           # relevance / portfolio-readiness lookup tables
│   ├── filter_sources.R   # the results query + single-source lookup
│   ├── ui_cards.R         # results grid: badges, card grid, count header
│   ├── ui_detail.R        # the Source-detail pane
│   ├── ui_pages.R         # landing / dashboard / references builders, app_ui()
│   └── server_risk_panel.R # wire_risk_panel(): plumbing for one risk panel
├── www/
│   └── styles.css         # card-grid styling
└── data/
    ├── sources.csv         # source master table (21 rows)
    ├── hazard_coverage.csv # long: source_id x hazard_id x coverage (+ granularity_detail)
    ├── d01_mapping.csv     # long: source_id x D 01.01 data point
    ├── source_details.csv  # long: source_id x field x free text (incl. cited "Abstract")
    ├── citations.csv       # long: source_id x citation_role x zotero_key
    ├── references.bib      # supporting-literature bibliography (BibTeX)
    └── source_abstracts_cited.md  # abstracts with inline citations (reference doc)
```

`app.R` sources every `R/*.R` file (except `_disable_autoload.R`) at
startup, so opening the app with the RStudio **Run App** button or
`shiny::runApp()` picks all of them up. Each module has a header comment
describing its slice.

## Data model

All data files are generated from the same underlying data as the thesis
Excel artefact (`ESG_Data_Source_Matrix_Structured.xlsx`, Appendix C),
so the dashboard and the printed matrix stay consistent — see the "Note
on data model" entry in that workbook's Legend sheet.

-   **`sources.csv`** — one row per source (21). Key columns used by the
    app: `source_id`, `source_name`, `operator`, `short_description`,
    `risk_type` (Physical/Transition/Both), `relevance_level`,
    `source_type`, `geographic_scope`, `granularity_level`,
    `technical_effort` (ordered factor: Low \< Medium \< High),
    `api` (Yes/No), `portfolio_ready` (No \< Partly \< Yes),
    `portfolio_ready_reason` (technical / granularity / ready — why a
    source is or is not portfolio-ready), `url`, `last_checked`.
    Fields removed over successive revisions (workbook Legend, "Removed
    fields"): `research_status`, `access_status`, `cost_category`,
    `operator_type`, the boolean `web_interface` (process metadata, or
    redundant with `source_type`); and `web_interface_type`,
    `download_format`, `key_limitation` were **moved into
    `source_details.csv`** as long-form fields.
-   **`hazard_coverage.csv`** — one row per source × hazard combination,
    `coverage` ∈ {none, partial, full}, plus a per-hazard
    `granularity_detail` verification note where covered. Drives the Tier
    2 (hazard type) filter and, in the detail view, the colour-coded
    hazard badges (green = full, amber = partial, red = not covered) plus
    a collapsible per-hazard *Coverage detail* table for the covered ones.
-   **`d01_mapping.csv`** — links each source to the specific D 01.01 /
    DPM data point(s) it is relevant for (primary/secondary), shown in
    the detail view.
-   **`source_details.csv`** — one `field` / `text` row per note. Shown
    as their own sections: the factual cited **Abstract**, the
    **Suitability for an SNCI** verdict, the headline **Limitations** note
    (as a callout) and **Pricing** (commercial sources only). The rest
    (download format, web interface type, data update frequency, licensing
    notes, portfolio-ready pipeline, methodology notes, …) sits in a
    collapsed *Technical & source notes* section. Shown only when a row is
    selected.
-   **`citations.csv`** — one row per source × reference, `citation_role`
    (primary / methodology / legal\_basis / technical\_doc). `zotero_key`
    is otherwise resolved in the thesis Zotero library. Loaded by
    `load_data.R` for completeness; not currently surfaced in the app.
-   **`references.bib`** — the supporting literature the abstracts cite
    (institutional pages, INSPIRE metadata records, sector-classification
    docs). Parsed by `R/load_data.R` (`load_references()`) with a small
    line-oriented reader, no extra package. Rendered as the References
    page.
-   **`source_abstracts_cited.md`** — human-readable companion to
    `references.bib`: the abstracts with each citation marked in bold at
    the claim it supports. Not read by the app.

## Navigation logic (Section 2.5)

-   **Landing page** — on open, the app shows an introduction plus a choice
    between Physical risk and Transition risk. Picking one opens the
    dashboard on that panel; the dashboard navbar keeps both within one
    click afterwards, so returning to the landing page is not required to
    switch risk type.
-   **Tier 1** — Risk type: Physical or Transition, selected from the
    **top navbar** (`bslib::navset_bar`). Each panel keeps its own
    Results / Source detail sub-tabs and row selection, so switching
    between Physical and Transition doesn't lose your place in either.
    The navbar also carries the free-text **search** field and a
    right-aligned **References** link.
-   **Tier 2** (Physical branch only) — Hazard type: multi-select
    checkboxes over the 12 hazard types from Table X (Section 2.4),
    shown in the shared sidebar only while the Physical risk panel is active.
    **AND semantics**: ticking several hazards narrows to sources that
    cover *every* selected hazard (at ≥ partial coverage), not any of
    them. No Tier 2 exists for the Transition branch (NACE classification
    sources apply uniformly across subsectors) — a one-line sidebar note
    says so.
-   **Facets** (applied regardless of tier) — source type, relevance
    level (exact match), and maximum technical effort (a ceiling: "Low"
    also returns nothing above Low). Combine freely with the other
    filters. API availability is shown in the detail view for traceability
    but is not a filter facet.
-   **Search** — in the **navbar** (not the sidebar), so it stays put
    across both risk panels and even in the detail view. Case-insensitive,
    token-based over source name, operator, short description and the
    limitations note; each whitespace-separated token must appear
    somewhere, in any order ("munich re", "ecb central bank" both match).
    Combines with all filters.
-   **Filters hidden in the detail view** — opening a source's *Source
    detail* sub-tab hides the sidebar facets (they don't apply to a
    single source) and shows a hint; a **← Back to results** button at
    the top of the detail pane (and the *Results* sub-tab) return to the
    filtered list. The navbar search stays available.
-   **Prev / Next in the detail view** — a stepper next to *Back to
    results* walks through the current filtered results one source at a
    time (with an *n / total* position indicator), so a shortlist can be
    reviewed without returning to the grid between each.
-   **References** — reached from the navbar link; a full-width page
    rendering `references.bib` as an alphabetical bibliography of the
    literature the source abstracts cite. A **← Back to dashboard** link
    returns to the last risk panel.

## Dashboard features

-   **Quick-start instructions** — the landing page carries a short
    walkthrough alongside the Physical/Transition choice, shown once
    before entering the dashboard.
-   **Result count** — shows how many sources match the current filters
    out of the total.
-   **Facet help** — each filter label carries a small **ⓘ** with a short
    explanation taken from the Data Source Matrix legend (relevance
    levels, technical-effort scale, public vs. commercial, what the
    search covers). Native browser tooltip, upgraded to a Bootstrap
    tooltip where available.
-   **Card-based results** — each matching source is a clickable card
    showing its name, short description, operator, and four at-a-glance
    badges: relevance (color-coded), source type (Public/Commercial),
    technical effort, and portfolio-readiness.
-   **Hazard select all / clear** — quickly select or deselect all 12
    hazard checkboxes.
-   **Reset all filters** — single button to restore all filters to
    their defaults.
-   **Source detail tab** — clicking a card switches to a dedicated
    detail tab, laid out to state each fact once: hero header (description,
    operator, external link, last-checked date); a row of key-metadata
    chips (relevance, source type, geographic scope, granularity, max.
    effort, API access, portfolio-ready + reason, the last with its
    definition on hover); the headline **Limitations** note as a
    highlighted callout; the factual cited **abstract**; the
    **Suitability for an SNCI** verdict (where recorded); D 01.01 mapping
    badges; **hazard coverage** as colour-coded badges (green = full,
    amber = partial, red = not covered) with a collapsible per-hazard
    detail table; a **Pricing** section for commercial sources; and a
    collapsed **Technical & source notes** section for the reference
    detail. A
    selected source stays viewable here even if a later filter change
    would hide it from the Results grid.

## Known limitations

-   No portfolio-input feature (deliberately out of scope — see Section
    2.5).
-   The `technical_effort` filter is a maximum-effort ceiling; requiring
    a minimum effort level is not supported (unlikely to be needed in
    practice).
-   This is a functional prototype for thesis evaluation (Section 2.6),
    not a production application — no authentication, no persistent
    storage, no automated data refresh from the source registers.
