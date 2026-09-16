# ============================================================
# document_functions.R
#
# Generates the function-level code documentation for the thesis appendix
# (Artefact 3 / R Shiny prototype) straight from the source files.
#
# How it works: every function in R/*.R and app.R that is meant to be
# documented carries a short comment block directly above its definition,
# written in a roxygen2-like style:
#
#   #' One-line title / description.
#   #'
#   #' Optional longer explanation, still starting each line with #'.
#   #'
#   #' @param x What x is.
#   #' @return What the function returns.
#   my_function <- function(x) { ... }
#
# This script parses those comment blocks (a small regex-based reader, not
# a dependency on the roxygen2 package - the app itself is not an R
# package, so roxygen2's usual Rd-generation workflow doesn't apply here)
# and renders them as one Markdown file, grouped by source file in the
# same order app.R's own header comment lists them in.
#
# The documentation therefore lives with the code it describes (single
# source of truth, kept in sync by re-running this script - the same
# separation-of-concerns argument the thesis makes in Section 2.5.2 for
# keeping the data layer in plain CSV files) rather than as a separately
# maintained, driftable copy.
#
# Usage (from the dashboard/ folder, or via RStudio's Source button):
#   Rscript document_functions.R
# Output:
#   ../Appendix/Appendix_E_Code_Documentation.md
# ============================================================

OUT_FILE <- file.path("..", "Appendix", "Appendix_E_Code_Documentation.md")

# Files to document, in the order app.R's header comment lists the
# modules (plus app.R itself, listed first as the entry point).
FILES <- c(
  "app.R",
  "R/load_data.R",
  "R/config.R",
  "R/filter_sources.R",
  "R/server_risk_panel.R",
  "R/ui_pages.R",
  "R/ui_cards.R",
  "R/ui_detail.R"
)

# One-line description of each file's role, shown under its heading.
# Kept here (not parsed from the file) because it describes the module as
# a whole, not any one function.
FILE_BLURB <- c(
  "app.R"                 = "Entry point: loads the CSV data once at startup, builds the UI, and defines the Shiny server function.",
  "R/load_data.R"         = "CSV loaders, the references.bib parser, hazard labels and hazard-coverage helpers.",
  "R/config.R"            = "Lookup tables only (relevance / output-type / integration-step / access-mode definitions and colours, field labels) - no functions to document.",
  "R/filter_sources.R"    = "The results query behind both risk panels, plus the single-source lookup used by the detail view.",
  "R/server_risk_panel.R" = "The reactive plumbing for one risk-type nav panel (instantiated once each for Physical and Transition).",
  "R/ui_pages.R"          = "The landing page, dashboard shell (navbar + sidebar), references page, and outer page shell.",
  "R/ui_cards.R"          = "The results grid: badge helpers, the clickable card grid, and the result-count line.",
  "R/ui_detail.R"         = "The Source-detail pane: section helpers, the nav row, and the full detail-view renderer."
)

# ------------------------------------------------------------
# Parsing
# ------------------------------------------------------------

# Matches a top-level `name <- function(args) {` (or `= function`)
# assignment, capturing the function name.
fun_def_re <- "^([A-Za-z_.][A-Za-z0-9_.]*)\\s*(?:<-|=)\\s*function\\s*\\("

# Reads one file and returns a list of function records:
#   name, params_declared (character vector, in signature order),
#   defaults (named character vector), title, details, params (named list,
#   @param descriptions keyed by parameter name), returns (character).
parse_functions <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  out <- list()

  for (i in seq_len(n)) {
    m <- regmatches(lines[i], regexec(fun_def_re, lines[i]))[[1]]
    if (length(m) == 0) next
    fname <- m[2]

    # Walk upward over a contiguous run of blank-then-comment lines to find
    # the doc block immediately preceding this definition (skipping at
    # most one blank line, e.g. after a section divider comment).
    j <- i - 1
    doc_lines <- character(0)
    while (j >= 1 && grepl("^\\s*#'", lines[j])) {
      doc_lines <- c(lines[j], doc_lines)
      j <- j - 1
    }

    # Function signature (may span multiple lines) - captured for the
    # parameter table's "default" column, read straight from formals()
    # after sourcing would be more robust, but a source()-free static
    # reader keeps this script runnable without loading shiny/bslib/dplyr.
    sig_lines <- lines[i]
    depth <- lengths(regmatches(lines[i], gregexpr("\\(", lines[i]))) -
             lengths(regmatches(lines[i], gregexpr("\\)", lines[i])))
    k <- i
    while (depth > 0 && k < n) {
      k <- k + 1
      sig_lines <- paste(sig_lines, lines[k])
      depth <- depth + lengths(regmatches(lines[k], gregexpr("\\(", lines[k]))) -
                        lengths(regmatches(lines[k], gregexpr("\\)", lines[k])))
    }
    args_str <- sub("^[^(]*\\(", "", sig_lines)
    args_str <- sub("\\)\\s*\\{?\\s*$", "", args_str)
    params_declared <- if (nzchar(trimws(args_str))) {
      trimws(strsplit(args_str, ",(?![^(]*\\))", perl = TRUE)[[1]])
    } else character(0)
    param_names <- sub("\\s*=.*$", "", params_declared)
    param_names <- sub("^\\.\\.\\.$", "...", param_names)

    if (length(doc_lines) == 0) {
      out[[length(out) + 1L]] <- list(
        name = fname, params_declared = param_names,
        title = NA_character_, details = character(0),
        params = list(), returns = NA_character_
      )
      next
    }

    body <- sub("^\\s*#'\\s?", "", doc_lines)
    param_re <- "^@param\\s+(\\S+)\\s+(.*)$"
    return_re <- "^@return\\s+(.*)$"

    title <- NA_character_
    details <- character(0)
    params <- list()
    returns <- NA_character_
    mode <- "title"  # title -> details -> tags
    title_buf <- NULL
    ret_buf <- NULL
    cur_param <- NULL

    flush_return <- function() if (!is.null(ret_buf)) returns <<- paste(ret_buf, collapse = " ")
    flush_param <- function() if (!is.null(cur_param)) params[[cur_param$name]] <<- paste(cur_param$text, collapse = " ")
    flush_title <- function() if (!is.null(title_buf)) title <<- paste(title_buf, collapse = " ")

    for (bl in body) {
      pm <- regmatches(bl, regexec(param_re, bl))[[1]]
      rm_ <- regmatches(bl, regexec(return_re, bl))[[1]]
      if (length(pm) == 3) {
        flush_title(); flush_param(); flush_return()
        cur_param <- list(name = pm[2], text = pm[3]); ret_buf <- NULL; mode <- "param"
      } else if (length(rm_) == 2) {
        flush_title(); flush_param()
        cur_param <- NULL; ret_buf <- rm_[2]; mode <- "return"
      } else if (mode == "param" && nzchar(trimws(bl))) {
        cur_param$text <- c(cur_param$text, trimws(bl))
      } else if (mode == "return" && nzchar(trimws(bl))) {
        ret_buf <- c(ret_buf, trimws(bl))
      } else if (mode == "title" && nzchar(trimws(bl))) {
        # Title is the whole first paragraph (until a blank line), so a
        # sentence that wraps across two `#'` comment lines stays one line.
        title_buf <- c(title_buf, bl)
      } else if (mode == "title" && !nzchar(trimws(bl))) {
        flush_title()
        mode <- "details"
      } else if (mode == "details") {
        details <- c(details, bl)
      }
    }
    flush_title(); flush_param(); flush_return()
    # trim leading/trailing blank lines in details
    while (length(details) > 0 && !nzchar(trimws(details[1])))       details <- details[-1]
    while (length(details) > 0 && !nzchar(trimws(details[length(details)]))) details <- details[-length(details)]

    out[[length(out) + 1L]] <- list(
      name = fname, params_declared = param_names,
      title = title, details = details, params = params, returns = returns
    )
  }
  out
}

# ------------------------------------------------------------
# Rendering
# ------------------------------------------------------------

md <- c(
  "# Appendix E: Function-Level Code Documentation",
  "",
  "Auto-generated from the doc comments (`#'` blocks) directly above each",
  "function in `dashboard/app.R` and `dashboard/R/*.R` - see",
  "`dashboard/document_functions.R`. Regenerate after changing the code",
  "by running that script; do not edit this file by hand, edits will be",
  "overwritten on the next run.",
  "",
  "Functions are grouped by file in the same order as the module list in",
  "`app.R`'s header comment (thesis Section 3.3.4). Helper functions whose",
  "name starts with `.` are internal to their file (not called from",
  "elsewhere) and are listed alongside the rest for completeness.",
  ""
)

total_fn <- 0
total_undoc <- 0

for (f in FILES) {
  md <- c(md, sprintf("## `%s`", f), "")
  if (!is.na(FILE_BLURB[f])) md <- c(md, FILE_BLURB[f], "")

  if (!file.exists(f)) {
    md <- c(md, "_File not found._", "")
    next
  }
  fns <- parse_functions(f)
  if (length(fns) == 0) next

  for (fn in fns) {
    total_fn <- total_fn + 1
    md <- c(md, sprintf("### `%s()`", fn$name), "")

    sig <- sprintf("%s(%s)", fn$name, paste(fn$params_declared, collapse = ", "))
    md <- c(md, sprintf("```r\n%s\n```", sig), "")

    if (is.na(fn$title)) {
      total_undoc <- total_undoc + 1
      md <- c(md, "_No doc comment found for this function._", "")
      next
    }

    md <- c(md, fn$title, "")
    if (length(fn$details) > 0) md <- c(md, fn$details, "")

    if (length(fn$params) > 0) {
      md <- c(md, "**Parameters**", "",
              "| Parameter | Description |", "|---|---|")
      for (pn in names(fn$params)) {
        md <- c(md, sprintf("| `%s` | %s |", pn, fn$params[[pn]]))
      }
      md <- c(md, "")
    }

    if (!is.na(fn$returns)) {
      md <- c(md, "**Returns**", "", fn$returns, "")
    }
  }
}

md <- c(md, "---", "",
        sprintf("_%d functions documented across %d files (%d without a doc comment)._",
                total_fn, length(FILES), total_undoc))

dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
writeLines(md, OUT_FILE, useBytes = TRUE)
cat(sprintf("Wrote %s (%d functions, %d files)\n", OUT_FILE, total_fn, length(FILES)))
