# ============================================================
# build_shinylive.R
# Exports dashboard/ as a static, serverless Shinylive site - no Shiny
# server or hosting subscription needed, just static files (see thesis
# Section 2.5.2 for why Shinylive was chosen, Section 3.3.4 for how).
#
# Why this script exists, not a plain shinylive::export("dashboard", ...):
# Shinylive bundles only the exported app directory into the browser's
# virtual filesystem. dashboard/R/load_data.R normally reads the data
# from "../data" (a sibling folder, shared with results-analysis/ - see
# that app's own loader for the same pattern) - but nothing outside the
# exported directory exists in Shinylive's sandbox, sibling or not, so
# "../data" resolves to nothing there and the app fails to start. This
# script instead stages a throwaway copy of dashboard/ with data/ copied
# in as a child folder, and exports that. The real dashboard/ and data/
# folders are never modified - data/ stays the single source of truth
# for the normal (non-Shinylive) app and for results-analysis/.
#
# Run from the project root:
#   Rscript build_shinylive.R
# Output goes to shinylive_site/ (gitignored - see .gitignore).
# ============================================================

if (!dir.exists("dashboard") || !dir.exists("data")) {
  stop("Run this script from the project root (dashboard/ and data/ must both exist here).")
}

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Package 'shinylive' is required: install.packages(\"shinylive\")")
}

out_dir <- "shinylive_site"

stage_dir <- file.path(tempdir(), paste0("dashboard_stage_", as.integer(Sys.time())))
dir.create(stage_dir)
on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

message("Staging dashboard/ + data/ into ", stage_dir)
file.copy(list.files("dashboard", full.names = TRUE), stage_dir, recursive = TRUE)
dir.create(file.path(stage_dir, "data"))
file.copy(list.files("data", full.names = TRUE), file.path(stage_dir, "data"), recursive = TRUE)

message("Exporting Shinylive site to ", out_dir, " ...")
shinylive::export(stage_dir, out_dir)

message("Done. Serve locally to test, e.g.:\n  R -e \"httpuv::runStaticServer('", out_dir, "')\"")
