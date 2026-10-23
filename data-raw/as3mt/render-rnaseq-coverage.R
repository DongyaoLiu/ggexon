# Run from the repository root with the development package loaded or installed.
library(ggexon)
source("inst/extdata/as3mt/load-as3mt-example.R")
example <- plot_as3mt_coverage("inst/extdata/as3mt", intron_factor = 30)
dir.create("data-raw/as3mt/figures", recursive = TRUE, showWarnings = FALSE)
for (extension in c("png", "pdf")) {
  ggplot2::ggsave(paste0("data-raw/as3mt/figures/AS3MT-compressed-RNAseq.", extension),
    plot = example$plot, width = 9, height = 6.5, dpi = 220, bg = "white",
    device = if (extension == "pdf") grDevices::cairo_pdf else "png")
}
