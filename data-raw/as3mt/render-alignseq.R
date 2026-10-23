# Run from the repository root with the development package loaded.
parameters <- list(width_mm = 180, height_mm = 160, rows_height_mm = 75,
  text_size_pt = 8, family = "Times", intron_factor = 30, dpi = 220,
  continuous_palette = "viridis", continuous_breaks = "equal-count quantiles")
source("inst/extdata/as3mt/load-as3mt-example.R")
result <- plot_as3mt_alignseq(demo_dir = "inst/extdata/as3mt",
  intron_factor = parameters$intron_factor,
  base_size = parameters$text_size_pt, base_family = parameters$family)
# Context is in the tutorial caption; exported scientific panels have no heading.
result$plot <- result$plot + ggplot2::theme(plot.title = ggplot2::element_blank(),
  plot.subtitle = ggplot2::element_blank())
result$grob <- as3mt_sequence_legend(result$plot, parameters$intron_factor,
  parameters$text_size_pt, parameters$family,
  coverage_height = result$coverage_height)$grob
for (kind in c("plot", "rows")) {
  filename <- if (kind == "plot") "AS3MT-aligned-DNA" else "AS3MT-aligned-DNA-rows"
  for (extension in c("png", "pdf")) {
    path <- file.path("data-raw/as3mt/figures", paste0(filename, ".", extension))
    ggplot2::ggsave(path, if (kind == "plot") result$grob else result[[kind]], width = parameters$width_mm,
      height = if (kind == "plot") parameters$height_mm else parameters$rows_height_mm,
      units = "mm", dpi = parameters$dpi,
      device = if (extension == "pdf") grDevices::cairo_pdf else "png", bg = "white")
  }
}
