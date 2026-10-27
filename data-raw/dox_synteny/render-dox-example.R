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
  table <- withCallingHandlers(ggplot2::ggplot_gtable(built),
    warning = function(w) stop("Tutorial draw warning: ", conditionMessage(w)))
  if (i >= 3L) {
    index <- match(paste0("panel-", layout$COL, "-", layout$ROW), table$layout$name)
    heights <- as.numeric(table$heights[table$layout$t[index]])
    stopifnot(isTRUE(all.equal(heights, c(.5, 1, .5, .5, 1, .5, .5, 1))))
    for (id in tutorial$species_ids) {
      panel <- as.integer(layout$PANEL[layout$panel_type == "coverage" & layout$track == id])
      stopifnot(isTRUE(all.equal(built@layout$panel_params[[panel]]$y.range,
        c(0, 1.1 * unname(tutorial$peaks[id])))))
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
