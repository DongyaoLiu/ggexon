# Run from the repository root; uses the current source package and offline data.
pkgload::load_all(quiet = TRUE)
demo_dir <- file.path("inst", "extdata", "hla_a_lollipop")
source(file.path(demo_dir, "load-hla-a-example.R"))
source(file.path(demo_dir, "plot-hla-a-example.R"))
source(file.path(demo_dir, "project-hla-a-example.R"))
hla <- load_hla_a_example(demo_dir)
plots <- plot_hla_a_example(hla)
gene <- load_hla_a_gene_model(hla, demo_dir)
plots$genomic <- plot_hla_a_gene_example(hla, gene, intron_factor = 3)$plot
out_dir <- file.path("data-raw", "hla_a_lollipop", "preview")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
for (name in c("overview", "focus", "genomic")) {
  for (extension in c("pdf", "png")) {
    ggplot2::ggsave(
      file.path(out_dir, paste0("hla-a-lollipop-", name, ".", extension)),
      plot = plots[[name]], width = hla_a_plot_parameters$width_mm,
      height = hla_a_plot_parameters$height_mm, units = "mm", dpi = 220,
      bg = "white"
    )
  }
}
utils::write.table(hla$mutations, file.path(out_dir, "hla_a_substitutions.tsv"),
                   sep = "\t", row.names = FALSE, quote = FALSE)
utils::write.table(hla$states, file.path(out_dir, "hla_a_residue_states.tsv"),
                   sep = "\t", row.names = FALSE, quote = FALSE)
utils::write.table(gene$projection, file.path(out_dir, "hla_a_projected_codon_segments.tsv"),
                   sep = "\t", row.names = FALSE, quote = FALSE)
cat("Rendered HLA-A plots and comparison tables to", normalizePath(out_dir), "\n")
