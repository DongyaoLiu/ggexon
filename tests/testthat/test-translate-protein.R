test_that("translate_protein translates the first three genes from extdata", {
  skip_if_not_installed("dplyr")
  library(dplyr)

  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  expect_true(nzchar(genome_path))
  expect_true(nzchar(annotation_path))

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) %>%
    load_annotation()

  ann <- annotation_data(x)
  ann_meta <- S4Vectors::mcols(ann)
  ann_type <- as.character(ann_meta$type)

  gene_rows <- ann[ann_type == "gene"]
  selected_genes <- head(as.character(S4Vectors::mcols(gene_rows)$ID), 3L)
  selected_genes <- selected_genes[!is.na(selected_genes) & nzchar(selected_genes)]

  expect_length(selected_genes, 3L)

  transcript_rows <- ann[ann_type == "mRNA" &
    as.character(S4Vectors::mcols(ann)$Parent) %in% selected_genes]
  expected_transcripts <- as.character(S4Vectors::mcols(transcript_rows)$ID)
  expected_transcripts <- expected_transcripts[
    !is.na(expected_transcripts) & nzchar(expected_transcripts)
  ]

  expect_gte(length(expected_transcripts), 3L)

  x2 <- x %>%
    translate_protein(genes = selected_genes)

  translated_tx <- names(protein_seq(x2))
  translated_tx <- translated_tx[!is.na(translated_tx) & nzchar(translated_tx)]
  protein_strings <- as.character(protein_seq(x2))

  expect_setequal(translated_tx, expected_transcripts)
  expect_setequal(names(nucleotide_seq(x2)), expected_transcripts)
  expect_true(all(Biostrings::width(nucleotide_seq(x2)) > 0))
  expect_true(all(Biostrings::width(protein_seq(x2)) > 0))
  expect_true(all(substr(protein_strings, 1L, 1L) == "M"))
  expect_true(all(!grepl("\\*", substr(protein_strings, 1L, pmax(nchar(protein_strings) - 1L, 1L)))))
})
