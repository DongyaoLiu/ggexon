#!/usr/bin/env Rscript
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE, export_all = FALSE))
code <- tempfile(fileext = ".R")
knitr::purl("vignettes/mhc-continuous-demo.Rmd", output = code, quiet = TRUE)
tutorial <- new.env(parent = globalenv())
sys.source(code, envir = tutorial)
unlink(code)

plots <- list(mhc_gene_spans = tutorial$p_genes,
  mhc_gene_heads = tutorial$p_heads,
  mhc_three_haplotypes = tutorial$p_mhc)
heights <- c(2.4, 4.2, 4.8)
out <- "data-raw/mhc_haplotypes/figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (i in seq_along(plots)) {
  plot <- plots[[i]]
  stopifnot(methods::is(plot@data, "SynSpecies"),
    identical(individual_names(plot@data), c("SSYh1", "GGOh1", "HSAh1")))
  head_records <- list()
  export_format <- "build"
  if (i > 1L) {
    stopifnot(inherits(plot@layers[[3]]$geom, "GeomGeneLollipop"))
    plot@layers[[3]]$geom_params$layout_callback <- function(data) {
      data$format <- export_format
      head_records[[length(head_records) + 1L]] <<- data
    }
  }
  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  stopifnot(sum(layout$panel_type == "annotation") == 3L,
    sum(layout$panel_type == "link") == if (i == 3L) 2L else 0L,
    !any(layout$panel_type == "coverage"),
    nrow(built@data[[1]]) == 443L, nrow(built@data[[2]]) == 58L)
  annotation <- layout[layout$panel_type == "annotation", ]
  stopifnot(all(annotation$xlim_max - annotation$xlim_min == 3628205))
  for (panel in as.integer(annotation$PANEL)) {
    stopifnot(isTRUE(all.equal(abs(diff(built@layout$panel_params[[panel]]$x.range)),
      3628205)))
  }
  if (i > 1L) {
    source_at <- match(paste(built@data[[3]]$individual, built@data[[3]]$gene_id),
      paste(tutorial$mhc_genes$individual, tutorial$mhc_genes$gene_id))
    stopifnot(nrow(built@data[[3]]) == 58L,
      identical(sort(as.character(built@data[[3]]$label)), sort(tutorial$mhc_genes$label)),
      !anyNA(source_at),
      all(built@data[[3]]$genomic_x == tutorial$mhc_genes$x[source_at]))
  }
  if (i == 3L) {
    stopifnot(length(unique(built@data[[4]]$group)) == 108L,
      length(unique(built@data[[5]]$group)) == 44L)
    for (j in 4:5) {
      owner <- layout[match(built@data[[j]]$PANEL, layout$PANEL), ]
      stopifnot(all(owner$panel_type == "link"),
        all(table(built@data[[j]]$group) == 4L))
    }
  }
  table <- withCallingHandlers(ggexon_gtable(built),
    warning = function(w) stop("MHC draw warning: ", conditionMessage(w)))
  withCallingHandlers({
    export_format <- "pdf"
    ggplot2::ggsave(file.path(out, paste0(names(plots)[i], ".pdf")), table,
      width = 9, height = heights[i], units = "in",
      device = grDevices::cairo_pdf, bg = "white")
    export_format <- "png"
    ggplot2::ggsave(file.path(out, paste0(names(plots)[i], ".png")), table,
      width = 9, height = heights[i], units = "in", dpi = 180, bg = "white")
  }, warning = function(w) stop("MHC export warning: ", conditionMessage(w)))
  if (i > 1L) {
    stopifnot(length(head_records) > 0L)
    heads <- do.call(rbind, head_records)
    for (format in c("pdf", "png")) {
      drawn <- heads[heads$format == format, ]
      stopifnot(nrow(drawn) == 58L)
      source <- tutorial$mhc_genes[match(paste(drawn$track, drawn$label),
        paste(tutorial$mhc_genes$track, tutorial$mhc_genes$label)), ]
      direction <- ifelse(source$strand == "+", 1, -1) *
        ifelse(source$track == "SSYh1", -1, 1)
      stopifnot(!anyNA(source$gene_id), all(drawn$genomic_x == source$x),
        all(drawn$direction == direction), all(drawn$row == source$head_row),
        all(drawn$column == source$head_col),
        all(drawn$left_mm >= 0), all(drawn$right_mm <= drawn$panel_width_mm),
        all(drawn$top_mm <= drawn$headroom_limit_mm + 1e-8),
        all(drawn$label_width_mm + 1 + 1.5 <= drawn$head_width_mm + 1e-8),
        all(abs(drawn$head_x_mm - drawn$anchor_x_mm) <= 85 + 1e-8))
      for (key in unique(drawn$slot_key)) {
        same <- drawn[drawn$slot_key == key, ]
        stopifnot(diff(range(same$head_x_mm)) < 1e-7,
          diff(range(same$head_y_mm)) < 1e-7)
      }
    }
    write.table(heads, file.path(out, paste0(names(plots)[i], "-layout.tsv")),
      sep = "\t", quote = FALSE, row.names = FALSE)
  }
  cat(names(plots)[i], ":", nrow(layout), "panels; stored native genes and direct links verified\n")
}
file.copy(file.path(out, "mhc_three_haplotypes.png"),
  "vignettes/figures/mhc-three-species.png", overwrite = TRUE)
file.copy(file.path(out, "mhc_three_haplotypes.png"),
  "docs/articles/figures/mhc-three-species.png", overwrite = TRUE)
