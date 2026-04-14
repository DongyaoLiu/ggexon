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

test_that("SynIndividual can waive the genome file at construction time", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  expect_true(nzchar(annotation_path))

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )

  expect_s4_class(x, "SynIndividual")
  expect_true(is.na(genome_file(x)))
  expect_identical(annotation_file(x), annotation_path)
  expect_identical(syn_id(x), "caenorhabditis_XZ1516")

  x <- load_annotation(x)
  expect_s4_class(annotation_data(x), "GRanges")
  expect_s4_class(seqinfo(x), "Seqinfo")

  cds_gr <- query_features(x, all = TRUE, feature_type = "CDS")
  expect_true(length(cds_gr) > 0L)
  target_tx <- as.character(S4Vectors::mcols(cds_gr)$transcript_id[[1L]])
  expect_true(nzchar(target_tx))

  expect_error(
    extract_cds_seq(x, transcripts = target_tx),
    "`extract_cds_seq\\(\\)` requires a genome FASTA"
  )
  expect_error(
    translate_protein(x, transcripts = target_tx),
    "`extract_cds_seq\\(\\)` requires a genome FASTA"
  )
})
