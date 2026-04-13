test_that("SynSpecies stores individuals and explicit alignment relationships", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  expect_true(nzchar(paf_path))

  x1 <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path,
    id = "XZ1516"
  )
  x2 <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path,
    id = "N2"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(sp, x1)
  sp <- add_individual(sp, x2)

  pair <- SynPairAlignment(
    name = "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path
  )
  multi <- SynMultiAlignment(
    name = "worm-maf",
    individuals = c("XZ1516", "N2", "CB4856"),
    file = "worms.maf"
  )

  sp <- add_pairwise_alignment(sp, pair)
  sp <- add_multiple_alignment(sp, multi)

  expect_identical(species_name(sp), "Caenorhabditis")
  expect_setequal(names(individuals(sp)), c("XZ1516", "N2"))
  expect_identical(names(pairwise_alignments(sp)), "XZ1516_vs_N2")
  expect_identical(names(multiple_alignments(sp)), "worm-maf")
  expect_identical(query_individual(pair), "XZ1516")
  expect_identical(target_individual(pair), "N2")
  expect_identical(alignment_file(pair), paf_path)
  expect_identical(alignment_individuals(pair), c("XZ1516", "N2"))
  expect_identical(
    alignment_individuals(multi),
    c("XZ1516", "N2", "CB4856")
  )
})

test_that("syn-aware geoms default to the only individual in a SynSpecies", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )

  exon_plot <- ggexon(sp) +
    geom_exon(
      chr = "RagTag_V",
      subset = c(21550000, 21680000)
    )
  exon_build <- ggplot2::ggplot_build(exon_plot)

  expect_true(nrow(exon_build$data[[1L]]) > 0L)
  expect_identical(unique(exon_build$data[[1L]]$track), "XZ1516")

  gene_plot <- ggexon(sp) +
    geom_gene(
      chr = "RagTag_V",
      subset = c(21550000, 21680000)
    )
  gene_build <- ggplot2::ggplot_build(gene_plot)

  expect_true(nrow(gene_build$data[[1L]]) > 0L)
  expect_identical(unique(gene_build$data[[1L]]$track), "XZ1516")
})

test_that("syn-aware geoms require species when a SynSpecies has multiple individuals", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "N2"
    )
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(sp) +
        geom_exon(
          chr = "RagTag_V",
          subset = c(21550000, 21680000)
        )
    ),
    "Use `species` to select one individual"
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(sp) +
        geom_gene(
          chr = "RagTag_V",
          subset = c(21550000, 21680000)
        )
    ),
    "Use `species` to select one individual"
  )
})
