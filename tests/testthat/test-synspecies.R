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
