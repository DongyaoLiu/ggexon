test_that("SynIndividual validates and stores the test genome inputs", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  expect_true(nzchar(genome_path))
  expect_true(nzchar(annotation_path))

  expect_true(check_syn_files(genome_path, annotation_path))

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  )

  expect_s4_class(x, "SynIndividual")
  expect_identical(genome_file(x), genome_path)
  expect_identical(annotation_file(x), annotation_path)
  expect_identical(annotation_format(x), "auto")
  expect_identical(syn_id(x), "XZ1516")
  expect_null(annotation_data(x))
  expect_null(seqinfo(x))
  expect_null(nucleotide_seq(x))
  expect_null(protein_seq(x))
})
