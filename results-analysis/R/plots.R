# ============================================================
# plots.R
# One function per chart, each returning a ggplot object. Used by both
# app.R (renderPlot) and export_figures.R (ggsave), so the interactive
# view and the thesis figures are always the same chart.
#
#   plot_distribution()          - 1 of the 3 metadata-distribution charts
#   plot_hazard_heatmap()        - source x hazard coverage grid
#   plot_hazard_gap()            - % of sources covering each hazard
#   plot_mapping_coverage()      - sources mapped per D 01.01 data point
#   plot_technical_effort_matrix()- derivation of technical_effort from
#                                  output_type x integration_step
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
# source_type or technical_effort). Counts are stacked
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

# ---- 5. Technical effort matrix -------------------------------------
# Figure 3 (thesis): derivation of technical_effort from output_type
# (rows) x integration_step (columns), the 19 assessed sources listed by
# display name in the combination they fall into, plus the two sources
# with no machine access at all (integration_step = "none"), set apart
# in their own column since their effort recurs per exposure rather than
# being incurred once.
#
# Cell ratings are read from the data wherever a source occupies that
# combination. The two combinations no source falls into (ready-made x
# multi-product, raw variable x point query) have no row to read a rating
# from; their Medium rating is the one documented derivation-rule value
# for that combination (Data Source Matrix legend) and is the only
# hardcoded content in this function.
plot_technical_effort_matrix <- function(sources_df, wrap_width = 24) {
  output_levels <- c("ready-made", "indicator", "raw variable")
  step_levels   <- c("point query", "download and join", "multi-product")

  # Manual numeric layout: the exception column sits at extra x-distance
  # from "multi-product" so the gap reads as a visual break, not a fourth
  # ordinary grid column (bslib::sidebar-style "set apart" per the brief).
  col_x <- c("point query" = 1, "download and join" = 2, "multi-product" = 3,
             "No machine access" = 4.35)
  row_y <- c("ready-made" = 3, "indicator" = 2, "raw variable" = 1)  # top to bottom

  main <- sources_df %>%
    filter(as.character(integration_step) %in% step_levels) %>%
    mutate(output_type = as.character(output_type), integration_step = as.character(integration_step)) %>%
    group_by(output_type, integration_step) %>%
    summarise(
      effort  = dplyr::first(as.character(technical_effort)),
      sources = paste(sort(source_name), collapse = ", "),
      n       = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::complete(output_type = output_levels, integration_step = step_levels,
                     fill = list(n = 0L, sources = "")) %>%
    mutate(col_label = integration_step)

  # The two combinations with no assessed source: fall back to the
  # documented derivation-rule rating (see function comment above) - the
  # only hardcoded content in this function.
  empty_rule <- c("ready-made.multi-product" = "Medium", "raw variable.point query" = "Medium")
  main <- main %>%
    mutate(
      key     = paste(output_type, integration_step, sep = "."),
      effort  = ifelse(n == 0, unname(empty_rule[key]), effort),
      sources = ifelse(n == 0, "no source in this combination", sources)
    )

  exceptions <- sources_df %>%
    filter(as.character(integration_step) == "none") %>%
    mutate(
      output_type = as.character(output_type),
      note = case_when(
        source_id == "hora" ~ "Note: the effort recurs for every exposure instead of being incurred once.",
        source_id == "eba_esg_dashboard" ~ "Note: the rating refers to consulting the source, not to integrating it.",
        TRUE ~ NA_character_
      ),
      sources   = paste0(source_name, "\n", vapply(note, function(nt)
        paste(strwrap(nt, width = wrap_width + 12), collapse = "\n"), character(1))),
      effort    = as.character(technical_effort),
      col_label = "No machine access"
    ) %>%
    select(output_type, col_label, effort, sources)

  cells <- bind_rows(main %>% select(output_type, col_label, effort, sources), exceptions) %>%
    mutate(
      x           = unname(col_x[col_label]),
      y           = unname(row_y[output_type]),
      tile_width  = ifelse(col_label == "No machine access", 1.25, 0.92),
      sources_wrapped = mapply(function(s, is_note) {
        if (!nzchar(s)) return(s)
        if (is_note) return(s)  # already hand-wrapped with an explicit \n
        wrap_label(s, width = wrap_width)
      }, sources, col_label == "No machine access"),
      effort = factor(effort, levels = c("Low", "Medium", "High"))
    )

  col_breaks <- col_x
  row_breaks <- row_y

  ggplot(cells, aes(x = x, y = y)) +
    geom_tile(aes(fill = effort, width = tile_width), height = 0.86,
              colour = ESG_SURFACE, linewidth = 1.6, na.rm = TRUE) +
    geom_text(aes(y = y + 0.30, label = effort, colour = effort), fontface = "bold",
              vjust = 1, size = 4.1, family = ESG_FONT, na.rm = TRUE, show.legend = FALSE) +
    geom_text(aes(y = y + 0.10, label = sources_wrapped, colour = effort), vjust = 1, size = 2.6,
              lineheight = 0.95, family = ESG_FONT, alpha = 0.92, na.rm = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = EFFORT_FILL, na.value = "transparent", guide = "none") +
    scale_colour_manual(values = EFFORT_TEXT, na.value = "transparent", guide = "none") +
    scale_x_continuous(breaks = col_breaks, labels = names(col_breaks), position = "top",
                        limits = c(0.45, 5.05), expand = c(0, 0)) +
    scale_y_continuous(breaks = row_breaks, labels = names(row_breaks),
                        limits = c(0.45, 3.65), expand = c(0, 0)) +
    labs(
      x = "Integration step — how the value reaches an individual exposure",
      y = "Output type — what the source delivers",
      caption = paste(strwrap(paste(
        "Effort rises with both dimensions but not additively: raw variables retrieved by download and",
        "join remain Medium, while an indicator assembled from several products is already High."
      ), width = 110), collapse = "\n")
    ) +
    coord_cartesian(clip = "off") +
    theme_esg() +
    theme(
      panel.grid.major    = element_blank(),
      panel.grid.minor    = element_blank(),
      axis.line.x         = element_blank(),
      axis.ticks.length   = unit(4, "pt"),
      axis.text.x         = element_text(size = rel(0.88), colour = ESG_INK_PRIMARY, face = "bold"),
      axis.text.y         = element_text(size = rel(0.88), colour = ESG_INK_PRIMARY, face = "bold"),
      axis.title          = element_text(size = rel(0.85)),
      plot.caption        = element_text(hjust = 0, size = rel(0.72), colour = ESG_INK_SECONDARY,
                                          margin = margin(t = 12)),
      plot.margin         = margin(10, 18, 10, 10),
      plot.title.position = "plot"
    )
}
