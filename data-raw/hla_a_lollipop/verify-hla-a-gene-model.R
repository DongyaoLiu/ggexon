# Verify reference sequence, splice-aware projection, and actual scale geometry.
pkgload::load_all(quiet = TRUE)
demo_dir <- file.path("inst", "extdata", "hla_a_lollipop")
source(file.path(demo_dir, "load-hla-a-example.R"))
source(file.path(demo_dir, "plot-hla-a-example.R"))
source(file.path(demo_dir, "project-hla-a-example.R"))
hla <- load_hla_a_example(demo_dir)
gene <- load_hla_a_gene_model(hla, demo_dir)
stopifnot(nrow(gene$exons) == 8L, nrow(gene$introns) == 7L,
          nrow(gene$anchors) == 1212L)
genomic <- as.character(Biostrings::readDNAStringSet(file.path(
  demo_dir, "hla_a_reference_genomic.fasta"
)))[[1L]]
cds <- paste0(substring(genomic, gene$exons$start, gene$exons$end), collapse = "")
protein <- as.character(Biostrings::translate(Biostrings::DNAString(cds)))
ref <- as.character(hla$proteins[hla$alleles$reference])
stopifnot(identical(protein, paste0(ref, "*")), nchar(cds) == 1098L)
# Independently enumerate every genomic CDS base; check each projected codon.
coding_bases <- unlist(Map(seq.int, gene$exons$start, gene$exons$end))
for (i in seq_len(nrow(hla$mutations))) {
  mutation <- hla$mutations[i, ]
  precursor <- mutation$position + 24L
  expected <- coding_bases[(precursor * 3L - 2L):(precursor * 3L)]
  rows <- gene$projection[gene$projection$mutation == mutation$mutation, ]
  actual <- unlist(Map(seq.int, as.integer(rows$xmin), as.integer(rows$xmax)))
  stopifnot(identical(sort(actual), sort(expected)))
  anchor <- gene$anchors$anchor[gene$anchors$mutation == mutation$mutation]
  stopifnot(length(anchor) == 1L,
            any(anchor >= gene$exons$start & anchor <= gene$exons$end))
}
# Mature residue 1 (precursor 25) has a splice-split codon, not an intronic base.
first <- gene$projection[gene$projection$mature_position == 1L, ]
stopifnot(nrow(first) > length(unique(first$mutation)),
          all(gene$anchors$anchor[gene$anchors$mature_position == 1L] == 504.5))
compressed <- plot_hla_a_gene_example(hla, gene, intron_factor = 3)
native <- plot_hla_a_gene_example(hla, gene, intron_factor = 1)
b3 <- ggplot2::ggplot_build(compressed$plot)
b1 <- ggplot2::ggplot_build(native$plot)
e3 <- b3$data[[1L]]
e1 <- b1$data[[1L]]
stopifnot(isTRUE(all.equal(e1$xmax - e1$xmin, as.numeric(gene$exons$length))),
          isTRUE(all.equal(e1$xmin[-1L] - e1$xmax[-8L], as.numeric(gene$introns$length))),
          isTRUE(all.equal(e3$xmax - e3$xmin, e1$xmax - e1$xmin)),
          isTRUE(all.equal(e3$xmin[-1L] - e3$xmax[-8L],
                           (e1$xmin[-1L] - e1$xmax[-8L]) / 3)))
points <- which(vapply(b3$plot$layers, function(layer) {
  inherits(layer$geom, "GeomPoint")
}, logical(1)))
stopifnot(length(points) == 1L, nrow(b3$data[[points]]) == 1212L,
          isTRUE(all.equal(b3$data[[points]]$y, log10(gene$anchors$allele_count + 1))))
cat("Verified 1,212 genomic projections, split codons, eight exons, seven introns, and exact 3-fold gap compression.\n")
