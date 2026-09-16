# Install the packages required by the results-analysis app.
# Run this once before launching the app or the export script:
#
#   source("results-analysis/requirements.R")

required_packages <- c(
  "shiny",   # web application framework
  "bslib",   # Bootstrap 5 theme + navset/card layout
  "dplyr",   # data manipulation
  "tidyr",   # data reshaping
  "ggplot2", # charts (app + static thesis-figure export)
  "scales",  # axis label formatting
  "tibble",  # small literal data frames (technical effort matrix, Figure 3)
  "svglite"  # SVG export of the thesis figures (export_figures.R)
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
