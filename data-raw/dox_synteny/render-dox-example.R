# Run the tutorial's actual object-first code in a fresh environment, validate
# each stage, then export the same plots. Run from the repository root.
suppressPackageStartupMessages(library(ggplot2))
if (dir.exists("R")) devtools::load_all(quiet = TRUE, export_all = FALSE) else library(ggexon)
if (.Platform$OS.type == "windows") stop("Render the BigWig tutorial on macOS or Linux.")
script <- tempfile("dox-tutorial-", fileext = ".R")
knitr::purl("vignettes/dox-synteny-demo.Rmd", output = script,
  documentation = 0, quiet = TRUE)
tutorial <- new.env(parent = globalenv())
sys.source(script, envir = tutorial)
unlink(script)
plots <- list(
  `dox-annotation-only` = tutorial$p_annotation,
  `dox-with-links` = tutorial$p_links,
  `dox-three-species` = tutorial$p_rna,
  `dox-three-species-compressed` = tutorial$p_compressed
)
expected <- list(
  rep("annotation", 3),
  c("annotation", "link", "annotation", "link", "annotation"),
  c("coverage", "annotation", "link", "coverage", "annotation", "link", "coverage", "annotation")
)
for (i in seq_along(plots)) {
  plot <- plots[[i]]
  stopifnot(methods::is(plot@data, "SynSpecies"), length(individual_names(plot@data)) == 3L,
    is.null(plot$labels$caption))
  built <- ggexon_build(plot)
  for (data in built@data) {
    if (all(c("xmin", "xmax") %in% names(data)))
      stopifnot(!anyNA(data$xmin), !anyNA(data$xmax))
  }
  layout <- as.data.frame(built@layout$layout)
  stopifnot(identical(as.character(layout$panel_type), expected[[min(i, 3L)]]))
  bounded <- layout$panel_type %in% c("coverage", "annotation") &
    layout$track %in% c("Dmau", "Dsech")
  stopifnot(all(layout$xlim_max[bounded] == 9440000))
  for (row in which(bounded)) {
    panel <- as.integer(layout$PANEL[row])
    limits <- c(layout$xlim_min[row], layout$xlim_max[row])
    transform <- built@layout$genomic_x_transforms[[as.character(panel)]]
    if (!is.null(transform)) limits <- ggexon:::ggexon_genomic_to_plot_x(limits, transform)
    stopifnot(isTRUE(all.equal(built@layout$panel_params[[panel]]$x.range, limits)))
  }
  annotation_ids <- as.integer(layout$PANEL[layout$panel_type == "annotation"])
  centers <- ggexon:::annotation_panel_body_centers(built@data, annotation_ids)
  for (panel in annotation_ids) {
    stopifnot(isTRUE(all.equal(mean(built@layout$panel_params[[panel]]$y.range),
      unname(centers[[as.character(panel)]]))))
  }
  table <- withCallingHandlers(ggplot2::ggplot_gtable(built),
    warning = function(w) stop("Tutorial draw warning: ", conditionMessage(w)))
  stopifnot(sum(grepl("^strip-l-individual-", table$layout$name)) == 3L)
  if (i >= 3L) {
    stopifnot(length(unique(layout$SCALE_Y[layout$panel_type == "coverage"])) == 1L)
    index <- match(paste0("panel-", layout$COL, "-", layout$ROW), table$layout$name)
    heights <- as.numeric(table$heights[table$layout$t[index]])
    stopifnot(isTRUE(all.equal(heights, c(.5, 1, .5, .5, 1, .5, .5, 1))))
    for (id in tutorial$species_ids) {
      panel <- as.integer(layout$PANEL[layout$panel_type == "coverage" & layout$track == id])
      stopifnot(isTRUE(all.equal(built@layout$panel_params[[panel]]$y.range,
        tutorial$rna_limits)))
    }
  }
  cat(names(plots)[i], ":", nrow(layout), "panels; object-backed layers verified\n")
}

out <- "data-raw/dox_synteny/figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
heights <- c(3.4, 4.6, 6.5, 6.5)
for (i in seq_along(plots)) for (extension in c("png", "pdf")) {
  ggsave(file.path(out, paste0(names(plots)[i], ".", extension)), plots[[i]],
    width = 9.5, height = heights[i], units = "in", dpi = 220,
    device = if (extension == "pdf") grDevices::cairo_pdf else ragg::agg_png,
    bg = "white", limitsize = FALSE)
}
file.copy(file.path(out, "dox-three-species.png"),
  "man/figures/dox-three-species-demo.png", overwrite = TRUE)
