# ============================================================
# filter_sources.R
# The result-set query behind both risk panels, plus the single-source
# lookup used by the detail view. Both read the data frames created in
# app.R (sources_df, hazard_cov) from the global environment.
# ============================================================

# Filters sources_df for one risk-type panel. Selection parameters are
# prefixed "sel_" so they never collide with the sources_df column names
# of the same name inside dplyr::filter()'s data-masking evaluation.
filter_sources <- function(risk_value,
                            hazard_types         = character(0),
                            hazard_full_only     = FALSE,
                            sel_search           = "",
                            sel_source_type      = "All",
                            sel_relevance        = "All",
                            sel_technical_effort = "All") {
  df <- sources_df %>% filter(risk_type == risk_value | risk_type == "Both")

  # Tier 2: hazard filter (physical-risk branch only). AND semantics -
  # with several hazards ticked, only sources covering every one of them
  # are kept. Coverage threshold defaults to >= partial; hazard_full_only
  # tightens this to full coverage on every ticked hazard.
  if (risk_value == "Physical" && length(hazard_types) > 0) {
    covering_ids <- sources_covering_hazards(
      hazard_cov, hazard_types,
      min_coverage = if (hazard_full_only) "full" else "partial")
    df <- df %>% filter(source_id %in% covering_ids)
  }

  # Free-text search: every whitespace-separated token must appear (case-
  # insensitively) somewhere in the source's name, operator or short
  # description. Token-based, so word order and surrounding punctuation
  # don't matter ("munich re", "re munich", "central bank ecb" all match).
  # Done with base subsetting on a pre-built haystack to avoid colliding
  # with the same-named columns inside dplyr data masking.
  tokens <- strsplit(tolower(trimws(sel_search)), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) > 0) {
    haystack <- tolower(paste(df$source_name, df$operator, df$short_description))
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
           relevance_level, technical_effort, portfolio_ready, granularity_level)
}

# Looks up one source's full record for the detail view, independent of the
# current filter/panel state - so a previously opened detail stays viewable
# even if later filter changes would hide it from the results grid.
lookup_source <- function(sid) {
  if (is.null(sid)) return(NULL)
  sources_df %>% filter(source_id == sid)
}
