prepared_gene_lollipop_individual <- function(id, keys, role = rep("MHC", length(keys)),
    copies = as.character(ave(seq_along(keys), keys, FUN = seq_along)),
    chr = "chr1", starts = seq_along(keys) * 100L, copy_column = "copy") {
  file <- tempfile(fileext = ".gff3")
  on.exit(unlink(file), add = TRUE)
  attrs <- paste0("ID=", id, "_", seq_along(keys), ";Name=source_", keys,
    ";plot_label=", keys, ";shared=", keys, ";gene_role=", role,
    ";plot_fill=%230077BB;", copy_column, "=", copies)
  records <- data.frame(chr, "fixture", "gene", starts, starts + 40L,
    ".", rep(c("+", "-"), length.out = length(keys)), ".", attrs)
  writeLines("##gff-version 3", file)
  utils::write.table(records, file, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = FALSE, append = TRUE)
  load_annotation(SynIndividual(id = id, annotation_file = file, annotation_format = "gff"))
}

prepared_gene_lollipop_fixture <- function() {
  x <- add_individual(SynSpecies("fixture"),
    prepared_gene_lollipop_individual("ref", c("A", "B", "C", "background"),
      role = c("MHC", "MHC", "MHC", "context")))
  add_individual(x, prepared_gene_lollipop_individual("other", c("A", "X", "B", "C", "C")))
}
