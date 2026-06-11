xz_variant_individual <- function(mutation_data) {
  # XZ1516 example genome/GFF live in inst/extdata, which is gitignored, so they
  # are only present in a local working copy. Skip these integration checks when
  # the data is absent (CI / clean checkout); the synthetic .aa_interval_to_genome
  # tests below cover the projection maths without it.
  fa <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  gff <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")
  testthat::skip_if_not(
    nzchar(fa) && nzchar(gff) && file.exists(fa) && file.exists(gff),
    "XZ1516 example data not installed (inst/extdata is gitignored)"
  )
  ind <- test_syn_individual(genome_file = fa, annotation_file = gff, id = "XZ1516")
  ann <- SynProteinMutationAnnotation(
    name = "muts",
    mutation_file = "<inmemory>",
    keytype = "gene_id",
    mutation_data = mutation_data,
    lazy = FALSE
  )
  add_annotation(ind, ann, set_active = FALSE)
}

make_mut <- function(gene_id, position, ref = "X", alt = "A", sample_count = 1L) {
  mut <- data.frame(
    gene_id = gene_id,
    position = as.integer(position),
    ref = ref,
    alt = alt,
    sample_count = as.integer(sample_count),
    stringsAsFactors = FALSE
  )
  mut$mutation <- paste0(mut$ref, mut$position, mut$alt)
  mut$source_row_id <- seq_len(nrow(mut))
  mut
}

test_that(".aa_interval_to_genome maps codons with strand, intron split, and phase", {
  gr_plus <- GenomicRanges::GRanges(
    "chr1", IRanges::IRanges(c(100, 200), c(104, 204)), strand = "+"
  )

  # residue 1, phase 0 -> coding nt 1-3 -> first exon bases 100-102
  r1 <- ggexon:::.aa_interval_to_genome(gr_plus, 1, 1)
  expect_equal(nrow(r1), 1L)
  expect_equal(c(r1$xmin, r1$xmax), c(100, 102))

  # 5'-truncated model: a phase-1 start shifts the reading frame by one base
  r1p <- ggexon:::.aa_interval_to_genome(gr_plus, 1, 1, cds_phase = 1)
  expect_equal(c(r1p$xmin, r1p$xmax), c(101, 103))

  # residue 2 straddles the splice junction -> one row per exon fragment
  r2 <- ggexon:::.aa_interval_to_genome(gr_plus, 2, 2)
  expect_equal(nrow(r2), 2L)
  r2 <- r2[order(r2$xmin), ]
  expect_equal(r2$xmin, c(103, 200))
  expect_equal(r2$xmax, c(104, 200))

  # minus strand reads from the 3' genomic end of the 5'-most exon
  gr_minus <- GenomicRanges::GRanges(
    "chr1", IRanges::IRanges(c(100, 200), c(104, 204)), strand = "-"
  )
  m1 <- ggexon:::.aa_interval_to_genome(gr_minus, 1, 1)
  expect_equal(c(m1$xmin, m1$xmax), c(202, 204))
})

test_that(".aa_interval_to_genome reads phase from a metadata column", {
  gr <- GenomicRanges::GRanges(
    "chr1", IRanges::IRanges(c(100, 200), c(104, 204)), strand = "+"
  )
  S4Vectors::mcols(gr)$phase <- c(2L, 0L)
  r1 <- ggexon:::.aa_interval_to_genome(gr, 1, 1)
  # phase 2 on the 5'-most exon -> first complete codon starts at coding nt 3
  expect_equal(c(r1$xmin, r1$xmax), c(102, 104))
})

test_that("project_mutations_to_genome projects + strand codons onto the CDS", {
  ind <- xz_variant_individual(make_mut(
    "geneXZ1516_zina-1", c(1L, 274L), ref = c("M", "N")
  ))
  proj <- project_mutations_to_genome(ind)

  m1 <- proj[proj$position == 1L, ]
  expect_equal(nrow(m1), 1L)
  expect_equal(c(m1$xmin, m1$xmax), c(21575003, 21575005))
  expect_equal(m1$strand, "+")
  expect_equal(m1$transcripts, "mRNAXZ1516_zina-1")

  n274 <- proj[proj$position == 274L, ]
  expect_equal(c(n274$xmin, n274$xmax), c(21578072, 21578074))
  # variant metadata is carried through
  expect_equal(n274$ref, "N")
  expect_equal(n274$mutation, "N274A")
})

test_that("project_mutations_to_genome splits intron-spanning codons", {
  ind <- xz_variant_individual(make_mut("geneXZ1516_zina-1", 39L, ref = "M"))
  proj <- project_mutations_to_genome(ind)
  proj <- proj[order(proj$xmin), ]
  expect_equal(nrow(proj), 2L)
  expect_equal(proj$xmin, c(21575162, 21576210))
  expect_equal(proj$xmax, c(21575163, 21576210))
})

test_that("project_mutations_to_genome handles minus-strand transcripts", {
  ind <- xz_variant_individual(make_mut(
    "FUN_018510-T1", c(2L, 84L), ref = c("P", "E")
  ))
  proj <- project_mutations_to_genome(ind)
  expect_true(all(proj$strand == "-"))
  expect_equal(c(proj$xmin[proj$position == 2L], proj$xmax[proj$position == 2L]),
               c(21596738, 21596740))
  expect_equal(c(proj$xmin[proj$position == 84L], proj$xmax[proj$position == 84L]),
               c(21596402, 21596404))
})

test_that("project_mutations_to_genome returns empty for unmatched genes and rejects non-individuals", {
  ind <- xz_variant_individual(make_mut("no_such_gene", 10L))
  expect_equal(nrow(project_mutations_to_genome(ind)), 0L)
  expect_error(project_mutations_to_genome(list()), "SynIndividual")
})

test_that("geom_aa_variant builds markers on the exon track", {
  ind <- xz_variant_individual(make_mut(
    "geneXZ1516_zina-1", c(1L, 39L, 274L), ref = c("M", "M", "N"),
    sample_count = c(5L, 3L, 1L)
  ))
  window <- c(21575003, 21582693)

  built <- ggplot2::ggplot_build(
    ggexon(ind) +
      geom_exon(chr = "V_RagTag", subset = window) +
      geom_aa_variant(
        ggplot2::aes(fill = sample_count),
        chr = "V_RagTag", subset = window, genes = "geneXZ1516_zina-1"
      )
  )

  exon_ld <- built$data[[1L]]
  var_ld <- built$data[[2L]]

  expect_equal(nrow(var_ld), 3L)
  expect_true(all(var_ld$x >= window[1] & var_ld$x <= window[2]))
  # heads sit above the exon row, stems start at the exon top
  expect_true(all(var_ld$y > max(exon_ld$ymax)))
  expect_true(all(var_ld$y_base <= var_ld$y))
  expect_equal(unique(var_ld$y_base), max(exon_ld$ymax))
})

test_that("project_domain_to_transcript still returns the documented columns", {
  gr <- GenomicRanges::GRanges(
    "chr1", IRanges::IRanges(c(100, 200), c(120, 220)), strand = "+"
  )
  out <- ggexon:::.project_domain_to_transcript(
    gr, aa_start = 1, aa_end = 5, transcript_id = "tx1",
    text = "dom", motif = "dom", domain_id = "d1", model = "Pfam"
  )
  expect_equal(
    names(out),
    c("seqnames", "xmin", "xmax", "strand", "transcripts",
      "model", "motif", "domain_id", "text")
  )
  expect_equal(out$xmin[1], 100)
})
