#!/usr/bin/env Rscript

## Figure 1 release builder.
##
## Purpose:
## - rebuild Figure 1 from the exact validated aggregate CSV outputs produced by
##   R/validate-rdhs.R;
## - avoid manual re-entry of rounded numbers;
## - use base R only, so no additional package is added to renv.lock;
## - produce black-only vector/raster outputs that remain interpretable without colour.
##
## Run from repository root after the RDHS validation gate has passed:
##   Rscript R/make-figure1.R
##
## This script does not read or export DHS microdata.

fail <- function(...) stop(sprintf(...), call. = FALSE)
info <- function(...) cat(sprintf("[INFO] %s\n", sprintf(...)))
pass <- function(...) cat(sprintf("[PASS] %s\n", sprintf(...)))

panel_a_file <- "data/derived/figure1-panel-a-generated.csv"
panel_b_file <- "data/derived/figure1-panel-b-generated.csv"
summary_file <- "data/derived/section6-rdhs-validation-summary.csv"
out_dir <- "figures"

for (f in c(panel_a_file, panel_b_file, summary_file)) {
  if (!file.exists(f)) fail("Required validated aggregate file not found: %s", f)
}

if (!capabilities("cairo")) {
  fail("R was built without Cairo support; Cairo is required for the release PDF/SVG/PNG devices.")
}

panel_a <- read.csv(panel_a_file, stringsAsFactors = FALSE, check.names = FALSE)
panel_b <- read.csv(panel_b_file, stringsAsFactors = FALSE, check.names = FALSE)
summary <- read.csv(summary_file, stringsAsFactors = FALSE, check.names = FALSE)

required_a <- c("panel", "method", "estimate", "lower", "upper", "se")
required_b <- c("panel", "method", "term", "OR", "lower", "upper")
required_s <- c("check", "observed", "official_or_target")

if (!identical(names(panel_a), required_a)) {
  fail("Panel A schema mismatch. Expected: %s", paste(required_a, collapse = ", "))
}
if (!identical(names(panel_b), required_b)) {
  fail("Panel B schema mismatch. Expected: %s", paste(required_b, collapse = ", "))
}
if (!identical(names(summary), required_s)) {
  fail("Validation-summary schema mismatch. Expected: %s", paste(required_s, collapse = ", "))
}
if (nrow(panel_a) != 2L) fail("Panel A must contain exactly 2 validated rows; found %d", nrow(panel_a))
if (nrow(panel_b) != 6L) fail("Panel B must contain exactly 6 validated rows; found %d", nrow(panel_b))
if (any(!is.finite(panel_a$estimate)) || any(!is.finite(panel_a$lower)) || any(!is.finite(panel_a$upper))) {
  fail("Panel A contains non-finite estimate/CI values")
}
if (any(!is.finite(panel_b$OR)) || any(!is.finite(panel_b$lower)) || any(!is.finite(panel_b$upper))) {
  fail("Panel B contains non-finite OR/CI values")
}
if (any(panel_a$lower > panel_a$estimate | panel_a$estimate > panel_a$upper)) {
  fail("Panel A contains an estimate outside its confidence interval")
}
if (any(panel_b$lower > panel_b$OR | panel_b$OR > panel_b$upper)) {
  fail("Panel B contains an odds ratio outside its confidence interval")
}

expected_a_methods <- c("Naive SRS", "Design-based")
if (!setequal(panel_a$method, expected_a_methods)) {
  fail("Panel A methods changed unexpectedly: %s", paste(panel_a$method, collapse = ", "))
}
expected_b_methods <- c("Naive (unweighted)", "Design-based")
if (!setequal(panel_b$method, expected_b_methods)) {
  fail("Panel B methods changed unexpectedly: %s", paste(unique(panel_b$method), collapse = ", "))
}

term_map <- c(
  educ_panelprimary = "Primary",
  educ_panelsecondary = "Secondary",
  educ_panelhigher = "Higher"
)
if (!all(panel_b$term %in% names(term_map))) {
  fail("Panel B contains unexpected education terms: %s", paste(unique(panel_b$term), collapse = ", "))
}
panel_b$education <- unname(term_map[panel_b$term])

## Pre-order Panel B once so plotting and factual-aid sections use the same rows.
edu_order <- c("Primary", "Secondary", "Higher")
naive_b <- panel_b[panel_b$method == "Naive (unweighted)", , drop = FALSE]
design_b <- panel_b[panel_b$method == "Design-based", , drop = FALSE]
naive_b <- naive_b[match(edu_order, naive_b$education), , drop = FALSE]
design_b <- design_b[match(edu_order, design_b$education), , drop = FALSE]
if (nrow(naive_b) != 3L || nrow(design_b) != 3L || anyNA(naive_b$education) || anyNA(design_b$education)) {
  fail("Panel B could not be ordered into Primary, Secondary, Higher for both methods")
}

## Cross-check plotted design-based insurance estimate against validation summary.
summary_insurance <- summary$observed[summary$check == "Insurance %"]
if (length(summary_insurance) != 1L || !is.finite(summary_insurance)) {
  fail("Validation summary must contain exactly one finite 'Insurance %%' row")
}
plot_insurance <- panel_a$estimate[panel_a$method == "Design-based"]
if (length(plot_insurance) != 1L || abs(plot_insurance - summary_insurance) > 1e-10) {
  fail("Panel A design-based insurance estimate does not match the validation summary")
}

## Figure text is intentionally factual/minimal. Final article caption and alt text are
## human-authored submission material and are not generated here.
font_family <- "Times New Roman"
base_cex <- 1.0

if (.Platform$OS.type == "windows") {
  try(grDevices::windowsFonts(`Times New Roman` = grDevices::windowsFont("Times New Roman")), silent = TRUE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_ci <- function(x, lo, hi, y, pch, lwd = 1.25, cex = 1.0, cap = 0.045) {
  segments(lo, y, hi, y, lwd = lwd)
  segments(lo, y - cap, lo, y + cap, lwd = lwd)
  segments(hi, y - cap, hi, y + cap, lwd = lwd)
  points(x, y, pch = pch, cex = cex, lwd = 1.25)
}

panel_heading <- function(letter, title, subtitle) {
  mtext(sprintf("%s   %s", letter, title), side = 3, line = 1.55, adj = 0,
        cex = 1.03, font = 2)
  mtext(subtitle, side = 3, line = 0.35, adj = 0, cex = 0.80)
}

render_figure <- function() {
  old <- par(no.readonly = TRUE)
  on.exit({
    layout(1)
    par(old)
  }, add = TRUE)

  ## Unequal panel heights: Panel A is deliberately compact; Panel B carries
  ## three paired estimates plus a legend and therefore receives more space.
  layout(matrix(c(1, 2), nrow = 2), heights = c(0.43, 0.57))

  ## -----------------------------------------------------------------------
  ## Panel A: national health-insurance coverage.
  ## A wider right margin is reserved as a results column so labels cannot be
  ## clipped by the plotting device.
  ## -----------------------------------------------------------------------
  par(
    family = font_family,
    mar = c(3.45, 6.25, 3.45, 7.75),
    cex = base_cex,
    las = 1,
    xaxs = "i",
    mgp = c(2.15, 0.65, 0),
    tcl = -0.25
  )

  a <- panel_a[match(expected_a_methods, panel_a$method), , drop = FALSE]
  y_a <- c(1.72, 0.92)
  xlim_a <- c(81.0, 84.0)
  plot(
    NA,
    xlim = xlim_a,
    ylim = c(0.55, 2.05),
    xaxt = "n",
    yaxt = "n",
    ylab = "",
    xlab = "Coverage (%)",
    bty = "n"
  )
  axis(1, at = seq(81, 84, by = 0.5), labels = sprintf("%.1f", seq(81, 84, by = 0.5)))
  axis(2, at = y_a, labels = a$method, tick = FALSE, line = -0.25)

  for (i in seq_len(nrow(a))) {
    plot_ci(a$estimate[i], a$lower[i], a$upper[i], y_a[i], pch = c(1, 16)[i])
    ## Fixed results column in the right margin: no dependence on CI endpoint.
    text(
      x = 84.10,
      y = y_a[i],
      labels = sprintf("%.2f%%  [%.2f, %.2f]", a$estimate[i], a$lower[i], a$upper[i]),
      pos = 4,
      cex = 0.78,
      xpd = NA
    )
  }
  mtext("Estimate [95% CI]", side = 4, line = 0.9, adj = 1, cex = 0.72, font = 2)
  panel_heading(
    "A",
    "National health-insurance coverage",
    "Point estimate with 95% confidence interval"
  )

  ## -----------------------------------------------------------------------
  ## Panel B: ANC4+ education gradient.
  ## Legend occupies its own band above the estimates; the OR=1 reference line
  ## starts below that band so it no longer collides with legend text.
  ## -----------------------------------------------------------------------
  par(
    family = font_family,
    mar = c(3.85, 6.25, 4.55, 1.25),
    cex = base_cex,
    las = 1,
    xaxs = "i",
    mgp = c(2.25, 0.70, 0),
    tcl = -0.25
  )

  y_base <- c(3.0, 2.0, 1.0)
  xlim_b <- c(0.80, 6.00)
  plot(
    NA,
    xlim = xlim_b,
    ylim = c(0.55, 3.82),
    log = "x",
    xaxt = "n",
    yaxt = "n",
    ylab = "",
    xlab = "Odds ratio (log scale)",
    bty = "n"
  )
  ticks <- c(0.8, 1, 1.25, 1.5, 2, 3, 4, 5, 6)
  axis(1, at = ticks, labels = c("0.80", "1.00", "1.25", "1.50", "2.00", "3.00", "4.00", "5.00", "6.00"))
  axis(2, at = y_base, labels = edu_order, tick = FALSE, line = -0.25)

  ## Draw the no-association reference only across the estimate field.
  segments(1, 0.63, 1, 3.38, lty = 2, lwd = 1)

  offset <- 0.105
  for (i in seq_along(edu_order)) {
    plot_ci(naive_b$OR[i], naive_b$lower[i], naive_b$upper[i], y_base[i] + offset, pch = 1)
    plot_ci(design_b$OR[i], design_b$lower[i], design_b$upper[i], y_base[i] - offset, pch = 16)
  }

  ## Dedicated legend band: symbols and text are kept away from Primary CI bars.
  legend_y <- 3.62
  points(0.90, legend_y, pch = 1, cex = 0.95, lwd = 1.2)
  text(0.94, legend_y, "Naive (unweighted)", pos = 4, cex = 0.78)
  points(1.62, legend_y, pch = 16, cex = 0.95)
  text(1.69, legend_y, "Design-based", pos = 4, cex = 0.78)

  panel_heading(
    "B",
    "Education and four-or-more antenatal-care visits",
    "No education is the reference category; point estimate with 95% confidence interval"
  )
}

pdf_file <- file.path(out_dir, "figure1-validated.pdf")
svg_file <- file.path(out_dir, "figure1-validated.svg")
png_file <- file.path(out_dir, "figure1-validated.png")
facts_file <- file.path(out_dir, "figure1-alt-text-facts.txt")

## Publication proportions: slightly wider than the first draft to prevent
## cramped result labels while retaining a compact two-panel figure.
fig_width <- 8.2
fig_height <- 7.2

## Vector PDF.
grDevices::cairo_pdf(pdf_file, width = fig_width, height = fig_height,
                     family = font_family, pointsize = 12)
render_figure()
dev.off()

## Editable vector SVG.
grDevices::svg(svg_file, width = fig_width, height = fig_height,
               family = font_family, pointsize = 12)
render_figure()
dev.off()

## High-resolution raster PNG at 300 dpi.
grDevices::png(
  png_file,
  width = round(fig_width * 300),
  height = round(fig_height * 300),
  res = 300,
  type = "cairo",
  family = font_family,
  pointsize = 12
)
render_figure()
dev.off()

for (f in c(pdf_file, svg_file, png_file)) {
  if (!file.exists(f) || file.info(f)$size <= 0) fail("Figure output was not created correctly: %s", f)
}

## Structured facts only. The human author should convert these into venue-compliant alt text.
facts <- c(
  "FIGURE 1 ALT-TEXT FACTS - NOT SUBMISSION PROSE",
  "Final alt text must be written/reviewed by the human author.",
  "",
  sprintf(
    "Panel A: Naive/SRS insurance coverage %.2f%% (95%% CI %.2f-%.2f); design-based %.2f%% (%.2f-%.2f).",
    panel_a$estimate[panel_a$method == "Naive SRS"],
    panel_a$lower[panel_a$method == "Naive SRS"],
    panel_a$upper[panel_a$method == "Naive SRS"],
    panel_a$estimate[panel_a$method == "Design-based"],
    panel_a$lower[panel_a$method == "Design-based"],
    panel_a$upper[panel_a$method == "Design-based"]
  ),
  "Panel B: odds ratios compare Primary, Secondary and Higher education with No education.",
  paste0(
    "Naive ORs: ",
    paste(sprintf("%s %.2f (%.2f-%.2f)", naive_b$education, naive_b$OR, naive_b$lower, naive_b$upper), collapse = "; "),
    "."
  ),
  paste0(
    "Design-based ORs: ",
    paste(sprintf("%s %.2f (%.2f-%.2f)", design_b$education, design_b$OR, design_b$lower, design_b$upper), collapse = "; "),
    "."
  ),
  "Primary education: the naive 95% interval excludes 1; the design-based 95% interval crosses 1.",
  "Figure uses black-only symbols and confidence-interval lines; open circle = naive/unweighted, filled circle = design-based."
)
writeLines(facts, facts_file, useBytes = TRUE)

info("Source Panel A: %s", normalizePath(panel_a_file, winslash = "/"))
info("Source Panel B: %s", normalizePath(panel_b_file, winslash = "/"))
info("Source summary: %s", normalizePath(summary_file, winslash = "/"))
pass("Figure 1 rebuilt from validated aggregate CSVs")
pass("Created vector PDF: %s", pdf_file)
pass("Created editable SVG: %s", svg_file)
pass("Created 300-dpi PNG: %s", png_file)
pass("Created human-alt-text factual aid: %s", facts_file)
cat("\nFIGURE 1 R RELEASE BUILD: PASS\n")
cat("Inspect the three rendered outputs visually before any publication use.\n")
