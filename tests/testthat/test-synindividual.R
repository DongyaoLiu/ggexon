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

test_that("query_features returns the same window on unloaded and loaded annotations", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  unloaded <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )
  loaded <- load_annotation(unloaded)

  full_gr <- annotation_data(loaded)
  target_chr <- as.character(GenomeInfoDb::seqnames(full_gr))[[1L]]
  target_start <- IRanges::start(full_gr)[[1L]]
  target_end <- IRanges::end(full_gr)[[1L]]

  region_unloaded <- query_features(
    unloaded,
    chr = target_chr,
    start = target_start,
    end = target_end,
    feature_type = NULL
  )
  region_loaded <- query_features(
    loaded,
    chr = target_chr,
    start = target_start,
    end = target_end,
    feature_type = NULL
  )

  expect_identical(
    as.data.frame(region_unloaded),
    as.data.frame(region_loaded)
  )
  expect_identical(resolve_syn_seqname(unloaded, target_chr), target_chr)
})

test_that("subset_individual trims all feature annotation layers by default", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  )

  alt_annotation <- SynFeatureAnnotation(
    name = "altpred",
    annotation_file = annotation_path
  )
  alt_annotation <- load_annotation(alt_annotation)
  x <- add_annotation(x, alt_annotation)
  x <- set_active_feature_annotation(x, "altpred")
  x <- load_annotation(x)
  x <- add_annotation(x, load_annotation(get_annotation(x, "default")))
  x <- add_interproscan_annotation(x)

  plot_cache(x) <- list(example = data.frame(x = 1L))
  projected_domains(x) <- list(example = data.frame(feature_id = "gene1"))

  gr <- annotation_data(x)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]

  subset_x <- subset_individual(
    x,
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_s4_class(subset_x, "SynIndividual")
  expect_setequal(annotation_names(subset_x), annotation_names(x))
  expect_identical(active_feature_annotation(subset_x), "altpred")
  expect_true("interpro" %in% annotation_names(subset_x))
  expect_identical(plot_cache(subset_x), list())
  expect_identical(projected_domains(subset_x), list())
  expect_null(nucleotide_seq(subset_x))
  expect_null(protein_seq(subset_x))
  expect_null(feature_index(subset_x))

  for (layer_name in c("default", "altpred")) {
    layer_gr <- annotation_data(get_annotation(subset_x, layer_name))
    expect_true(length(layer_gr) >= 1L)
    expect_true(all(as.character(GenomeInfoDb::seqnames(layer_gr)) == target_chr))
    expect_true(all(
      IRanges::start(layer_gr) <= target_end &
        IRanges::end(layer_gr) >= target_start
    ))
  }
})

test_that("subset_individual can limit trimming to the active feature layer", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  )

  alt_annotation <- SynFeatureAnnotation(
    name = "altpred",
    annotation_file = annotation_path
  )
  alt_annotation <- load_annotation(alt_annotation)
  x <- add_annotation(x, alt_annotation)
  x <- set_active_feature_annotation(x, "altpred")
  x <- load_annotation(x)
  x <- add_annotation(x, load_annotation(get_annotation(x, "default")))

  gr <- annotation_data(x)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]

  default_before <- annotation_data(get_annotation(x, "default"))

  subset_x <- subset_individual(
    x,
    chr = target_chr,
    start = target_start,
    end = target_end,
    annotations = "active"
  )

  expect_true(length(annotation_data(get_annotation(subset_x, "altpred"))) <= length(gr))
  expect_identical(
    length(annotation_data(get_annotation(subset_x, "default"))),
    length(default_before)
  )
})
