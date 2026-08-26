# Install all packages required by the ESG dashboard.
# Run this once before launching the app:
#
#   source("requirements.R")
#
# or paste it into the RStudio console.

required_packages <- c(
  "shiny",   # web application framework
  "bslib",   # Bootstrap 5 theme (bs_theme)
  "dplyr",   # data manipulation
  "tidyr",   # data reshaping (used in load_data.R)
  "DT"       # interactive DataTables (renderDT / DTOutput)
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
