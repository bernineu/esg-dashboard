# ============================================================
# load_data.R
# Loads the normalized data files (Artefact 2 data layer)
# and prepares them for use in the Shiny app.
#
# Data model (see thesis Section 2.5 / Appendix C, workbook Legend sheet):
#   sources.csv          - one row per data source (master table)
#   hazard_coverage.csv  - long format: source_id x hazard_id x coverage
#                          (+ granularity_detail free text where covered)
#   d01_mapping.csv      - long format: source_id x D 01.01 data point
#   source_details.csv   - long format: source_id x field x free text
#                          (includes the cited "Abstract" per source)
#   citations.csv        - long format: source_id x citation_role x zotero_key
#   references.bib       - supporting-literature bibliography backing the
#                          abstract citations (BibTeX; zotero_key in
#                          citations.csv is otherwise resolved in Zotero)
#
# research_status, access_status and cost_category were removed from
# sources.csv (workbook Legend: "Removed fields"): process metadata,
# redundant with the download/api/web_interface columns, and redundant
# with source_type respectively. portfolio_ready_reason was added
# (technical / granularity / ready - see PORTFOLIO_REASON_DEFS in app.R).
# ============================================================

library(dplyr)
library(tidyr)

DATA_DIR <- "data"

# NULL/empty-coalescing helper (used by the references.bib parser)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

load_sources <- function(path = file.path(DATA_DIR, "sources.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

  # technical_effort as an ordered factor (Section 2.5 / recommendation point 7)
  df$technical_effort <- factor(
    df$technical_effort,
    levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )

  # convert Yes/No/Partly text flags to explicit ordered factors where useful
  df$portfolio_ready <- factor(
    df$portfolio_ready,
    levels = c("No", "Partly", "Yes"),
    ordered = TRUE
  )

  df
}

load_hazard_coverage <- function(path = file.path(DATA_DIR, "hazard_coverage.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  df$coverage <- factor(df$coverage, levels = c("none", "partial", "full"), ordered = TRUE)
  df
}

load_d01_mapping <- function(path = file.path(DATA_DIR, "d01_mapping.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

load_source_details <- function(path = file.path(DATA_DIR, "source_details.csv")) {
  read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

load_citations <- function(path = file.path(DATA_DIR, "citations.csv")) {
  df <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  df$zotero_key <- trimws(ifelse(is.na(df$zotero_key), "", df$zotero_key))
  df
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

# Returns the source_ids that cover ALL of the selected hazard_ids at
# >= min_coverage (AND semantics: a source qualifies only if every selected
# hazard is covered). Used by Tier 2 filtering (physical-risk branch).
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
