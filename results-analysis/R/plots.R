# ============================================================
# plots.R
# One function per chart, each returning a ggplot object. Used by both
# app.R (renderPlot) and export_figures.R (ggsave), so the interactive
# view and the thesis figures are always the same chart.
#
#   plot_distribution()       - 1 of the 4 metadata-distribution charts
#   plot_hazard_heatmap()     - source x hazard coverage grid
#   plot_hazard_gap()         - % of sources covering each hazard
#   plot_mapping_coverage()   - sources mapped per D 01.01 data point
#   plot_portfolio_by_effort()- portfolio readiness x technical effort
#   plot_readiness_reason()   - why non-ready sources fall short
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)

# Wraps long labels onto multiple lines (base strwrap) so a long source
# name doesn't force the plot panel too narrow for the title/legend to
# fit on the canvas - used for the hazard heatmap's y-axis.
wrap_label <- function(x, width = 30) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

# ---- 1. Metadata distributions, split by risk type ----------------
# var: column name (string) in sources_df to break down (relevance_level,
# source_type, technical_effort or portfolio_ready). Counts are stacked
# horizontal bars by risk_type (the only categorical breakdown used
# across all four, so one shared legend / colour meaning throughout).
plot_distribution <- function(sources_df, var, title, x_lab) {
  d <- sources_df %>%
    count(risk_type, .data[[var]], name = "n")

  totals <- d %>%
    group_by(.data[[var]]) %>%
    summarise(total = sum(n), .groups = "drop")

  cat_levels <- levels(sources_df[[var]])
  d[[var]]      <- factor(d[[var]],      levels = rev(cat_levels))
  totals[[var]] <- factor(totals[[var]], levels = rev(cat_levels))

  ggplot(d, aes(x = n, y = .data[[var]], fill = risk_type)) +
    geom_col(width = 0.62) +
    geom_text(
      data = totals, aes(x = total, y = .data[[var]], label = total),
      inherit.aes = FALSE, hjust = -0.4, size = 3.3, family = ESG_FONT,
      colour = ESG_INK_SECONDARY
    ) +
    scale_fill_manual(values = RISK_TYPE_COLORS, name = "Risk type", drop = FALSE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.16)), breaks = scales::breaks_pretty(n = 4)) +
    labs(title = title, x = x_lab, y = NULL) +
    theme_esg() +
    theme(panel.grid.major.y = element_blank())
}

# ---- 2. Hazard coverage heatmap ------------------------------------
plot_hazard_heatmap <- function(hazard_cov, sources_df, hazard_labels = HAZARD_LABELS) {
  d <- hazard_cov %>%
    left_join(sources_df %>% select(source_id, source_name), by = "source_id") %>%
    mutate(
      hazard_label = factor(unname(hazard_labels[hazard_id]), levels = unname(hazard_labels)),
      source_label = wrap_label(source_name, width = 30)
    )

  order_df <- d %>%
    distinct(source_name, source_label) %>%
    left_join(
      d %>% group_by(source_name) %>% summarise(score = sum(as.integer(coverage) - 1), .groups = "drop"),
      by = "source_name"
    ) %>%
    arrange(score)
  d$source_label <- factor(d$source_label, levels = order_df$source_label)

  ggplot(d, aes(x = hazard_label, y = source_label, fill = coverage)) +
    geom_tile(colour = ESG_SURFACE, linewidth = 1.1) +
    scale_fill_manual(
      values = c(none = unname(ESG_STATUS["critical"]), partial = unname(ESG_STATUS["warning"]), full = unname(ESG_STATUS["good"])),
      labels = c(none = "Not covered", partial = "Partial", full = "Full"),
      name = "Coverage", drop = FALSE
    ) +
    labs(title = "Hazard coverage across data sources", x = NULL, y = NULL) +
    theme_esg() +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1),
      axis.text.y = element_text(size = rel(0.72), lineheight = 0.85),
      panel.grid  = element_blank(),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}

# ---- 3. Hazard coverage gap summary --------------------------------
plot_hazard_gap <- function(hazard_cov, hazard_labels = HAZARD_LABELS) {
  d <- hazard_cov %>%
    group_by(hazard_id) %>%
    summarise(n_covered = sum(coverage != "none"), .groups = "drop") %>%
    mutate(hazard_label = unname(hazard_labels[hazard_id])) %>%
    arrange(n_covered)
  d$hazard_label <- factor(d$hazard_label, levels = d$hazard_label)

  ggplot(d, aes(x = n_covered, y = hazard_label)) +
    geom_col(fill = ESG_SEQ_BLUE, width = 0.62) +
    geom_text(aes(label = n_covered), hjust = -0.4, size = 3.3,
              family = ESG_FONT, colour = ESG_INK_SECONDARY) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12)), breaks = scales::breaks_pretty(n = 5)) +
    labs(title = "Number of sources covering each hazard",
         x = "Number of physical-risk sources", y = NULL) +
    theme_esg() +
    theme(panel.grid.major.y = element_blank())
}

# ---- 4. D 01.01 mapping coverage -----------------------------------
# Not every source has a D 01.01 mapping row (context-only sources are
# left unmapped), so n_sources here is out of nrow(d01_mapping) source
# rows, not the full register - see the caption in app.R / the subtitle
# below.
plot_mapping_coverage <- function(d01_mapping, sources_df = NULL) {
  d <- d01_mapping %>%
    count(data_point, name = "n_sources") %>%
    arrange(n_sources) %>%
    mutate(data_point_label = wrap_label(data_point, width = 34))
  d$data_point_label <- factor(d$data_point_label, levels = d$data_point_label)

  subtitle <- if (!is.null(sources_df)) {
    n_mapped <- n_distinct(d01_mapping$source_id)
    n_total  <- nrow(sources_df)
    sprintf("%d of %d sources have a D 01.01 mapping (context-only sources are unmapped)", n_mapped, n_total)
  } else NULL

  ggplot(d, aes(x = n_sources, y = data_point_label)) +
    geom_col(fill = ESG_SEQ_BLUE, width = 0.6) +
    geom_text(aes(label = n_sources), hjust = -0.4, size = 3.3, family = ESG_FONT,
              colour = ESG_INK_SECONDARY) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)), breaks = scales::breaks_pretty(n = 5)) +
    labs(title = "Sources mapped per D 01.01 data point", subtitle = subtitle,
         x = "Number of sources", y = NULL) +
    theme_esg() +
    theme(panel.grid.major.y = element_blank())
}

# ---- 5. Portfolio readiness x technical effort ---------------------
plot_portfolio_by_effort <- function(sources_df) {
  d <- sources_df %>% count(technical_effort, portfolio_ready, name = "n")

  ggplot(d, aes(x = technical_effort, y = n, fill = portfolio_ready)) +
    geom_col(width = 0.55) +
    scale_fill_manual(
      values = c(No = unname(ESG_STATUS["critical"]), Partly = unname(ESG_STATUS["warning"]), Yes = unname(ESG_STATUS["good"])),
      name = "Portfolio-ready", drop = FALSE
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
    labs(title = "Portfolio readiness by technical effort", x = "Technical effort", y = "Number of sources") +
    theme_esg() +
    theme(panel.grid.major.x = element_blank())
}

# ---- 6. Why non-ready sources fall short ---------------------------
plot_readiness_reason <- function(sources_df, reason_labels = READINESS_REASON_LABELS) {
  d <- sources_df %>%
    filter(portfolio_ready != "Yes") %>%
    count(portfolio_ready_reason, name = "n") %>%
    mutate(reason_label = unname(reason_labels[as.character(portfolio_ready_reason)])) %>%
    arrange(n)
  d$reason_label <- factor(d$reason_label, levels = d$reason_label)

  ggplot(d, aes(x = n, y = reason_label)) +
    geom_col(fill = unname(ESG_CAT["orange"]), width = 0.5) +
    geom_text(aes(label = n), hjust = -0.4, size = 3.3, family = ESG_FONT, colour = ESG_INK_SECONDARY) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18)), breaks = scales::breaks_pretty(n = 4)) +
    labs(title = "Why non-ready sources fall short",
         subtitle = "Sources rated portfolio_ready = No or Partly",
         x = "Number of sources", y = NULL) +
    theme_esg() +
    theme(panel.grid.major.y = element_blank())
}
