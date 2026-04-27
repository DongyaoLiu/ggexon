write_indexed_annotation_fixture <- function(lines, ext = ".gff3") {
  src <- tempfile(fileext = ext)
  gz <- paste0(src, ".gz")

  writeLines(lines, src)
  Rsamtools::bgzip(src, dest = gz, overwrite = TRUE)
  Rsamtools::indexTabix(gz, format = "gff")

  gz
}

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

test_that("SynIndividual can be initialized with only an id", {
  x <- SynIndividual(id = "bare")

  expect_s4_class(x, "SynIndividual")
  expect_identical(syn_id(x), "bare")
  expect_true(is.na(genome_file(x)))
  expect_true(is.na(annotation_file(x)))
  expect_identical(annotation_format(x), "auto")
  expect_identical(annotation_names(x), character())
  expect_identical(active_feature_annotation(x), "default")
})

test_that("SynIndividual requires id when no file inputs are supplied", {
  expect_error(
    SynIndividual(),
    "`id` must be supplied when neither `genome_file` nor `annotation_file` is provided."
  )
})

test_that("SynIndividual accepts multiple annotation files and merges them on load", {
  annotation_one <- tempfile(fileext = ".gff3")
  annotation_two <- tempfile(fileext = ".gff3")

  writeLines(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t10\t100\t.\t+\t.\tID=geneA;Name=geneA",
      "chrI\ttest\tmRNA\t10\t100\t.\t+\t.\tID=txA;Parent=geneA;Name=txA",
      "chrI\ttest\texon\t10\t40\t.\t+\t.\tID=exonA1;Parent=txA"
    ),
    annotation_one
  )
  writeLines(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t200\t260\t.\t-\t.\tID=geneB;Name=geneB",
      "chrI\ttest\tmRNA\t200\t260\t.\t-\t.\tID=txB;Parent=geneB;Name=txB",
      "chrI\ttest\texon\t220\t260\t.\t-\t.\tID=exonB1;Parent=txB"
    ),
    annotation_two
  )

  x <- SynIndividual(
    annotation_file = c(annotation_one, annotation_two),
    genome_file = genome_waiver(),
    id = "multi"
  )

  expect_identical(annotation_file(x), c(annotation_one, annotation_two))
  expect_identical(annotation_format(x), c("auto", "auto"))

  loaded <- load_annotation(x)
  gr <- annotation_data(loaded)

  expect_s4_class(gr, "GRanges")
  expect_equal(length(gr), 6L)
  expect_true(all(c("geneA", "geneB") %in% as.character(S4Vectors::mcols(gr)$gene_name)))
})

test_that("load_annotation can target a stored annotation layer through SynIndividual", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver(),
    id = "XZ1516"
  )
  alt_annotation <- SynFeatureAnnotation(
    name = "alt",
    annotation_file = annotation_path
  )
  x <- add_annotation(x, alt_annotation, set_active = FALSE)

  expect_null(annotation_data(get_annotation(x, "alt")))

  loaded <- load_annotation(x, annotation = "alt")

  expect_s4_class(loaded, "SynIndividual")
  expect_s4_class(annotation_data(get_annotation(loaded, "alt")), "GRanges")
  expect_identical(active_feature_annotation(loaded), "default")
})

test_that("query_features combines indexed windows across multiple annotation files", {
  skip_if_not_installed("Rsamtools")

  annotation_one <- write_indexed_annotation_fixture(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t10\t100\t.\t+\t.\tID=geneA;Name=geneA",
      "chrI\ttest\tmRNA\t10\t100\t.\t+\t.\tID=txA;Parent=geneA;Name=txA",
      "chrI\ttest\texon\t10\t40\t.\t+\t.\tID=exonA1;Parent=txA",
      "chrI\ttest\texon\t60\t100\t.\t+\t.\tID=exonA2;Parent=txA"
    ),
    ext = ".gff3"
  )
  annotation_two <- write_indexed_annotation_fixture(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t200\t260\t.\t-\t.\tID=geneB;Name=geneB",
      "chrI\ttest\tmRNA\t200\t260\t.\t-\t.\tID=txB;Parent=geneB;Name=txB",
      "chrI\ttest\texon\t200\t220\t.\t-\t.\tID=exonB1;Parent=txB",
      "chrI\ttest\texon\t240\t260\t.\t-\t.\tID=exonB2;Parent=txB"
    ),
    ext = ".gff3"
  )

  x <- SynIndividual(
    annotation_file = c(annotation_one, annotation_two),
    genome_file = genome_waiver(),
    id = "multi_indexed"
  )

  region_gr <- query_features(
    x,
    chr = "chrI",
    start = 1,
    end = 300,
    feature_type = "exon"
  )

  expect_null(annotation_data(x))
  expect_identical(as.character(unique(GenomeInfoDb::seqnames(region_gr))), "chrI")
  expect_identical(as.character(unique(S4Vectors::mcols(region_gr)$type)), "exon")
  expect_identical(length(region_gr), 4L)
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

test_that("query_features uses indexed gff3.gz windows without full loading", {
  skip_if_not_installed("Rsamtools")

  annotation_path <- write_indexed_annotation_fixture(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t10\t100\t.\t+\t.\tID=geneA;Name=geneA",
      "chrI\ttest\tmRNA\t10\t100\t.\t+\t.\tID=txA;Parent=geneA;Name=txA",
      "chrI\ttest\texon\t10\t40\t.\t+\t.\tID=exonA1;Parent=txA",
      "chrI\ttest\texon\t60\t100\t.\t+\t.\tID=exonA2;Parent=txA",
      "chrII\ttest\tgene\t200\t260\t.\t-\t.\tID=geneB;Name=geneB"
    ),
    ext = ".gff3"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )

  expect_identical(
    syn_id(x),
    tools::file_path_sans_ext(sub("\\.gz$", "", basename(annotation_path)))
  )
  expect_null(annotation_data(x))

  region_gr <- query_features(
    x,
    chr = "chrI",
    start = 15,
    end = 80,
    feature_type = "exon"
  )

  expect_null(annotation_data(x))
  expect_identical(as.character(unique(GenomeInfoDb::seqnames(region_gr))), "chrI")
  expect_identical(as.character(unique(S4Vectors::mcols(region_gr)$type)), "exon")
  expect_identical(length(region_gr), 2L)

  loaded <- load_annotation(x)
  expect_s4_class(annotation_data(loaded), "GRanges")
  expect_true(length(annotation_data(loaded)) >= 5L)
})

test_that("query_features uses indexed gtf.gz windows", {
  skip_if_not_installed("Rsamtools")

  annotation_path <- write_indexed_annotation_fixture(
    c(
      "chrI\ttest\tgene\t10\t100\t.\t+\t.\tgene_id \"geneA\"; gene_name \"geneA\";",
      "chrI\ttest\ttranscript\t10\t100\t.\t+\t.\tgene_id \"geneA\"; transcript_id \"txA\"; gene_name \"geneA\";",
      "chrI\ttest\texon\t10\t40\t.\t+\t.\tgene_id \"geneA\"; transcript_id \"txA\"; exon_number \"1\";",
      "chrI\ttest\texon\t60\t100\t.\t+\t.\tgene_id \"geneA\"; transcript_id \"txA\"; exon_number \"2\";",
      "chrII\ttest\tgene\t200\t260\t.\t-\t.\tgene_id \"geneB\"; gene_name \"geneB\";"
    ),
    ext = ".gtf"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )

  region_gr <- query_features(
    x,
    chr = "chrI",
    start = 15,
    end = 80,
    feature_type = "exon"
  )

  expect_null(annotation_data(x))
  expect_identical(as.character(unique(GenomeInfoDb::seqnames(region_gr))), "chrI")
  expect_identical(as.character(unique(S4Vectors::mcols(region_gr)$type)), "exon")
  expect_identical(length(region_gr), 2L)
})

test_that("build_feature_index stores reusable lookups for loaded individuals", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )
  x <- load_annotation(x)
  expect_null(feature_index(x))

  meta <- S4Vectors::mcols(annotation_data(x))
  gene_keys <- as.character(meta$gene_id)
  gene_key <- gene_keys[!is.na(gene_keys) & nzchar(gene_keys)][1L]
  expect_true(nzchar(gene_key))

  indexed <- build_feature_index(x)
  idx <- feature_index(indexed)

  expect_true(is.list(idx))
  expect_true(all(c("seqname", "type", "gene", "transcript", "parent") %in% names(idx)))
  expect_true(length(idx$gene) > 0L)

  expect_identical(
    as.data.frame(query_features(x, genes = gene_key, feature_type = NULL)),
    as.data.frame(query_features(indexed, genes = gene_key, feature_type = NULL))
  )
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

test_that("subset_individual accepts coords strings", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver()
  )
  x <- load_annotation(x)

  gr <- annotation_data(x)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]
  coords <- paste0(target_chr, ":", target_start, "-", target_end)

  subset_by_coords <- subset_individual(x, coords = coords)
  subset_by_args <- subset_individual(
    x,
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_identical(
    as.data.frame(annotation_data(subset_by_coords)),
    as.data.frame(annotation_data(subset_by_args))
  )
  expect_error(
    subset_individual(x, chr = target_chr, coords = coords),
    "Provide either `coords` or `chr`/`start`/`end`"
  )
})

test_that("subset_individual can resolve through SynSpecies", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver(),
    id = "XZ1516"
  )
  x <- load_annotation(x)
  sp <- SynSpecies(name = "worms")
  sp <- add_individual(sp, x)

  gr <- annotation_data(x)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]

  subset_x <- subset_individual(
    sp,
    individual = "XZ1516",
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_s4_class(subset_x, "SynIndividual")
  expect_identical(syn_id(subset_x), "XZ1516")
  expect_true(length(annotation_data(subset_x)) >= 1L)
})
