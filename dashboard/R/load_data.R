# ============================================================
# load_data.R
# Loads the normalized data files (Artefact 2 data layer)
# and prepares them for use in the Shiny app.
#
# Data model (see thesis Section 2.5 / Appendix C, workbook Legend sheet):
#   sources.csv          - one row per data source (master table)
#   hazard_coverage.csv  - long format: source_id x hazard_id x coverage
#                          (+ hazard_granularity / hazard_indicator /
#                          coverage_rationale free text where covered)
#   d01_mapping.csv      - long format: source_id x D 01.01 data point
#   source_details.csv   - long format: source_id x field x free text
#                          (includes the cited "Abstract" per source)
#   references.bib       - supporting-literature bibliography backing the
#                          abstract citations (BibTeX)
#
# sources.csv has been trimmed over successive revisions (workbook Legend,
# "Removed fields"): research_status / access_status / cost_category /
# operator_type / web_interface dropped; web_interface_type /
# download_format / key_limitation moved into source_details.csv as
# long-form fields (later renamed there to interface_type / data_format).
# portfolio_ready and portfolio_ready_reason were replaced by output_type,
# integration_step and access_mode, which together also derive
# technical_effort (see R/config.R). citations.csv (source_id x
# citation_role x zotero_key) was dropped - it was loaded but never
# surfaced anywhere, and every row's zotero_key was empty.
# ============================================================

library(dplyr)
library(tidyr)

# dashboard/ is normally a sibling of the top-level data/ folder (data/ is
# shared with results-analysis/, which reads it the same way - see
# results-analysis/R/load_analysis_data.R). Shiny sets the working
# directory to the app's own folder (dashboard/) for the app's lifetime,
# both via RStudio's "Run App" and shiny::runApp("dashboard"), so "../data"
# is correct there. The Shinylive export (see the demo repo's build
# script) stages data/ as a *child* of the exported app root instead - so
# "../data" doesn't exist there and "data" is used instead.
DATA_DIR <- if (dir.exists(file.path("..", "data"))) file.path("..", "data") else "data"

# NULL/empty-coalescing helper (used by the references.bib parser)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

#' Load the source master table.
#'
#' Reads sources.csv and coerces `technical_effort`, `output_type`,
#' `integration_step` and `access_mode` to factors in their fixed legend
#' order, so facet dropdowns and detail-chip order follow the Data Source
#' Matrix legend rather than alphabetical order.
#'
#' @param path Path to sources.csv (defaults to `data/sources.csv`).
#' @return A data frame, one row per source (19 rows in the current data).
load_sources <- function(path = file.path(DATA_DIR, "sources.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

  # technical_effort as an ordered factor (Section 2.5 / recommendation point 7)
  df$technical_effort <- factor(
    df$technical_effort,
    levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )

  # output_type / integration_step / access_mode as factors in their fixed
  # legend order (see R/config.R), so facet choices and detail chips list
  # them consistently rather than alphabetically.
  df$output_type      <- factor(df$output_type,      levels = OUTPUT_TYPE_LEVELS)
  df$integration_step <- factor(df$integration_step, levels = INTEGRATION_STEP_LEVELS)
  df$access_mode      <- factor(df$access_mode,      levels = ACCESS_MODE_LEVELS)

  df
}

#' Load the source x hazard coverage table.
#'
#' @param path Path to hazard_coverage.csv.
#' @return A data frame, one row per source x hazard pair, with `coverage`
#'   as an ordered factor (none < partial < full).
load_hazard_coverage <- function(path = file.path(DATA_DIR, "hazard_coverage.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  df$coverage <- factor(df$coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  df
}

#' Load the source -> Template D 01.01 data-point mapping table.
#'
#' @param path Path to d01_mapping.csv.
#' @return A data frame, one row per source x D 01.01 data-point link.
load_d01_mapping <- function(path = file.path(DATA_DIR, "d01_mapping.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

#' Load the long-format source detail texts (abstract, suitability,
#' limitations, pricing, licensing, ...).
#'
#' @param path Path to source_details.csv.
#' @return A data frame, one row per source x field x free text.
load_source_details <- function(path = file.path(DATA_DIR, "source_details.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# ------------------------------------------------------------
# references.bib -> data frame
# ------------------------------------------------------------
# data/references.bib is machine-generated with exactly one
# `field = {value},` per line and each entry closed by a line that is just
# "}", so a line-oriented parse is enough and we avoid a new package
# dependency (RefManageR / bibtex). Returns one row per @entry with the
# fields the References page needs, plus:
#   url    - extracted from howpublished's \url{...}
#   intext - the leading "(Author, year)" taken from the note field, used
#            to match a reference to the source abstract(s) that cite it.

#' Clean one raw BibTeX field value for display.
#'
#' Un-escapes the small set of LaTeX sequences the generated
#' references.bib uses for German umlauts/ß and a few punctuation
#' characters, strips \url{...} down to its bare URL, and drops any
#' remaining braces.
#'
#' @param x A single raw field value (character scalar), or NA/empty.
#' @return The cleaned string, or "" if `x` is NA, NULL or empty.
.bib_clean <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(x)) return("")
  reps <- c(
    '{\\"a}' = "ä", '{\\"o}' = "ö", '{\\"u}' = "ü",
    '{\\"A}' = "Ä", '{\\"O}' = "Ö", '{\\"U}' = "Ü",
    '{\\ss}' = "ß", "\\&" = "&", "\\#" = "#", "--" = "–", "~" = " "
  )
  for (k in names(reps)) x <- gsub(k, reps[[k]], x, fixed = TRUE)
  x <- gsub("\\\\url\\{([^}]*)\\}", "\\1", x)
  x <- gsub("[{}]", "", x)
  trimws(x)
}

#' Parse references.bib into a data frame for the References page.
#'
#' Line-oriented parser tailored to the machine-generated layout of
#' data/references.bib (exactly one `field = {value},` per line, each entry
#' closed by a line that is just "}") - avoids a bibtex/RefManageR
#' dependency for a file this regular.
#'
#' @param path Path to references.bib.
#' @return A data frame, one row per @entry, with columns key, type,
#'   author, title, howpublished, url, urldate, year, note and intext
#'   (the leading "(Author, year)" parsed out of `note`, used to match a
#'   reference to the source abstract(s) that cite it), sorted by author
#'   then year. Returns a 0-row data frame with the same columns if the
#'   file is missing or empty.
load_references <- function(path = file.path(DATA_DIR, "references.bib")) {
  cols <- c("key", "type", "author", "title", "howpublished",
            "url", "urldate", "year", "note", "intext")
  empty <- setNames(data.frame(matrix("", 0, length(cols)),
                               stringsAsFactors = FALSE), cols)
  if (!file.exists(path)) return(empty)

  lines   <- readLines(path, encoding = "UTF-8", warn = FALSE)
  entries <- list()
  cur     <- NULL
  for (ln in lines) {
    hdr <- regmatches(ln, regexec("^@(\\w+)\\{([^,]+),\\s*$", ln))[[1]]
    if (length(hdr) == 3) {
      if (!is.null(cur)) entries[[length(entries) + 1L]] <- cur
      cur <- list(key = trimws(hdr[3]), type = tolower(hdr[2]))
      next
    }
    if (is.null(cur)) next
    if (grepl("^\\}\\s*$", ln)) {
      entries[[length(entries) + 1L]] <- cur
      cur <- NULL
      next
    }
    fld <- regmatches(ln, regexec("^\\s*(\\w+)\\s*=\\s*\\{(.*)\\},?\\s*$", ln))[[1]]
    if (length(fld) == 3) cur[[tolower(fld[2])]] <- fld[3]
  }
  if (!is.null(cur)) entries[[length(entries) + 1L]] <- cur
  if (length(entries) == 0) return(empty)

  df <- do.call(rbind, lapply(entries, function(e) {
    hp  <- e$howpublished %||% ""
    url <- if (grepl("\\\\url\\{", hp)) sub(".*\\\\url\\{([^}]*)\\}.*", "\\1", hp) else ""
    row <- lapply(c("key", "type", "author", "title", "howpublished",
                    "urldate", "year", "note"),
                  function(f) .bib_clean(e[[f]] %||% ""))
    names(row) <- c("key", "type", "author", "title", "howpublished",
                    "urldate", "year", "note")
    row$url    <- .bib_clean(url)
    as.data.frame(row, stringsAsFactors = FALSE)
  }))

  df$intext <- ifelse(grepl("^\\(", df$note),
                      sub("^\\(([^)]*)\\).*", "\\1", df$note), "")
  df$year   <- ifelse(nzchar(df$year), df$year, "n.d.")
  df[order(tolower(df$author), df$year), c(cols)]
}

# Human-readable labels for the 12 hazard types (matches thesis Table X / Section 2.4)
HAZARD_LABELS <- c(
  heat_stress   = "Heat stress",
  permafrost    = "Permafrost thawing",
  heat_wave     = "Heat wave",
  wildfire      = "Wildfire",
  storm         = "Storm",
  water_stress  = "Water stress",
  flood         = "Flood",
  heavy_precip  = "Heavy precipitation",
  drought       = "Drought",
  glacial_lake  = "Glacial lake outburst",
  soil_erosion  = "Soil erosion",
  landslide     = "Landslide"
)

# HAZARD_LABELS remapped to shiny's c(label = value) convention, so
# checkboxGroupInput shows "Heat stress" while input$hazard_types stores
# the hazard id ("heat_stress").
HAZARD_CHOICES <- setNames(names(HAZARD_LABELS), HAZARD_LABELS)

# HAZARD_CHOICES narrowed to hazards with at least one source at partial
# or full coverage. A hazard with zero covering sources (e.g. glacial lake
# outburst flood) is a dead filter option - ticking it can only return an
# empty result - so it's left off the Tier 2 checkbox list. This is
# data-driven, not hardcoded: the hazard reappears automatically once
# hazard_coverage.csv records a source covering it. The full 12-hazard
# classification (HAZARD_LABELS) is untouched - only the filter UI is
# narrowed, so the hazard-coverage matrix/heatmap still shows the gap.
#' Narrow the 12-hazard classification to hazards with at least one
#' covering source.
#'
#' A hazard with zero covering sources (e.g. glacial lake outburst flood)
#' is a dead filter option, so it is left off the Tier 2 checkbox list;
#' the full 12-hazard classification (HAZARD_LABELS) is untouched
#' elsewhere, so the hazard-coverage matrix still shows the gap.
#'
#' @param hazard_cov The hazard-coverage data frame (see
#'   load_hazard_coverage()).
#' @return A named character vector in `c(label = hazard_id)` form (the
#'   subset of HAZARD_CHOICES with coverage != "none" for at least one
#'   source), suitable for checkboxGroupInput's `choices`.
active_hazard_choices <- function(hazard_cov) {
  covered <- unique(hazard_cov$hazard_id[hazard_cov$coverage != "none"])
  HAZARD_CHOICES[HAZARD_CHOICES %in% covered]
}

#' Find sources covering every one of a set of hazards (AND semantics).
#'
#' Used by Tier 2 filtering on the physical-risk branch: a source qualifies
#' only if it covers *every* hazard in `hazard_ids` at or above
#' `min_coverage`, not just any one of them.
#'
#' @param hazard_cov The hazard-coverage data frame (see
#'   load_hazard_coverage()).
#' @param hazard_ids Character vector of hazard ids to require coverage
#'   for; an empty vector means "no hazard filter" (see @return).
#' @param min_coverage Minimum coverage level to count as "covered":
#'   "none", "partial" (default) or "full".
#' @return Character vector of qualifying source_ids; if `hazard_ids` is
#'   empty, every source_id present in `hazard_cov`.
sources_covering_hazards <- function(hazard_cov, hazard_ids, min_coverage = "partial") {
  if (length(hazard_ids) == 0) return(unique(hazard_cov$source_id))
  min_level <- factor(min_coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  hazard_cov %>%
    filter(hazard_id %in% hazard_ids, coverage >= min_level) %>%
    distinct(source_id, hazard_id) %>%
    count(source_id, name = "n_covered") %>%
    filter(n_covered == length(hazard_ids)) %>%
    pull(source_id)
}
