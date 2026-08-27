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
    ├── sources.csv         # source master table (16 rows)
    ├── hazard_coverage.csv # long: source_id x hazard_id x coverage (+ granularity_detail)
    ├── d01_mapping.csv     # long: source_id x D 01.01 data point
    ├── source_details.csv  # long: source_id x field x free text (incl. cited "Abstract")
    ├── citations.csv       # long: source_id x citation_role x zotero_key
    ├── references.bib      # supporting-literature bibliography (BibTeX)
    └── source_abstracts_cited.md  # abstracts with inline citations (reference doc)
```

## Data model

All data files are generated from the same underlying data as the thesis
Excel artefact (`ESG_Data_Source_Matrix_Structured.xlsx`, Appendix C),
so the dashboard and the printed matrix stay consistent — see the "Note
on data model" entry in that workbook's Legend sheet.

-   **`sources.csv`** — one row per source (16). Key columns used by the
    app: `source_id`, `source_name`, `risk_type` (Physical/Transition/Both),
    `relevance_level`, `source_type`, `operator_type`,
    `technical_effort` (ordered factor: Low \< Medium \< High),
    `api` (Yes/No), `portfolio_ready` (No \< Partly \< Yes),
    `portfolio_ready_reason` (technical / granularity / ready — why a
    source is or is not portfolio-ready), `url`, `last_checked`.
    `research_status`, `access_status` and `cost_category` were removed
    from the matrix (workbook Legend, "Removed fields"): process
    metadata, redundant with the download/api/web_interface columns, and
    redundant with `source_type` respectively.
-   **`hazard_coverage.csv`** — one row per source × hazard combination,
    `coverage` ∈ {none, partial, full}, plus `granularity_detail` free
    text where covered. Drives the Tier 2 (hazard type) filter, the
    hazard coverage badges and their tooltips in the detail view.
-   **`d01_mapping.csv`** — links each source to the specific D 01.01 /
    DPM data point(s) it is relevant for (primary/secondary), shown in
    the detail view.
-   **`source_details.csv`** — long-form rationale text (processing
    effort, limitations, suitability, licensing notes, portfolio-ready
    pipeline) plus the cited **Abstract** per source. The abstract is
    shown as prose in the detail view; the rest sits in a collapsed
    *Rationale & source notes* section, since the abstract already
    synthesises it. Shown only when a row is selected.
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
    The navbar also carries a right-aligned **References** link.
-   **Tier 2** (Physical branch only) — Hazard type: multi-select
    checkboxes over the 12 hazard types from Table X (Section 2.4),
    shown in the shared sidebar only while the Physical risk panel is active.
    **AND semantics**: ticking several hazards narrows to sources that
    cover *every* selected hazard (at ≥ partial coverage), not any of
    them. No Tier 2 exists for the Transition branch, since NACE
    classification sources apply uniformly across subsectors.
-   **Facets** (applied regardless of tier) — source type, relevance
    level (exact match), and maximum technical effort (a ceiling: "Low"
    also returns nothing above Low). Combine freely with the other
    filters. Operator type and API availability are shown in the detail
    view for traceability but are not filter facets.
-   **Search** (applied regardless of tier) — case-insensitive, token-based
    free-text search over source name, operator, operator type and short
    description. Each whitespace-separated token must appear somewhere, in
    any order ("munich re", "ecb central bank" both match); combines with
    all other filters above.
-   **Filters hidden in the detail view** — opening a source's *Source
    detail* sub-tab hides the sidebar filters (they don't apply to a
    single source) and shows a hint; a **← Back to results** button at
    the top of the detail pane (and the *Results* sub-tab) return to the
    filtered list.
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
-   **Relevance legend** — collapsible definitions for all four
    relevance levels (Primary, Supplementary, Context only,
    Methodological).
-   **Card-based results** — each matching source is a clickable card
    showing its name, short description, operator, and four at-a-glance
    badges: relevance (color-coded), source type (Public/Commercial),
    technical effort, and portfolio-readiness.
-   **Hazard select all / clear** — quickly select or deselect all 12
    hazard checkboxes.
-   **Reset all filters** — single button to restore all filters to
    their defaults.
-   **Source detail tab** — clicking a card switches to a dedicated
    detail tab, laid out to state each fact once: hero header
    (description, external link, last-checked date); a row of key-metadata
    chips (operator type, relevance, source type, max. effort, API access,
    portfolio-ready + reason, the last with its definition on hover); the
    cited **abstract** as prose — the narrative assessment that ties the
    structured fields together; D 01.01 mapping badges; hazard coverage
    badges (green = full, amber = partial) with a one-line coverage
    granularity; and a collapsed **Rationale & source notes** section
    holding the raw rationale text the abstract is built from (open by
    default only if a source has no abstract). A selected source stays
    viewable here even if a later filter change would hide it from the
    Results grid.

## Known limitations

-   No portfolio-input feature (deliberately out of scope — see Section
    2.5).
-   The `technical_effort` filter is a maximum-effort ceiling; requiring
    a minimum effort level is not supported (unlikely to be needed in
    practice).
-   This is a functional prototype for thesis evaluation (Section 2.6),
    not a production application — no authentication, no persistent
    storage, no automated data refresh from the source registers.
