test_that("SynAnnotation subclasses can be attached to SynIndividual", {
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
  )

  expect_s4_class(get_annotation(x), "SynFeatureAnnotation")
  expect_s4_class(get_annotation(x), "SynIndAnnotation")
  expect_s4_class(get_annotation(x), "SynAnnotation")
  expect_identical(annotation_names(x), "default")
  expect_identical(active_feature_annotation(x), "default")
  expect_identical(annotation_kind(get_annotation(x)), "SynFeatureAnnotation")
  expect_identical(annotation_scope(get_annotation(x)), "nucleotide")

  alt_annotation <- SynFeatureAnnotation(
    name = "altpred",
    annotation_file = annotation_path,
    metadata = list(source = "alternative-predictor")
  )

  x <- add_annotation(x, alt_annotation)
  expect_setequal(annotation_names(x), c("default", "altpred"))
  expect_identical(active_feature_annotation(x), "default")

  x <- set_active_feature_annotation(x, "altpred")
  expect_identical(active_feature_annotation(x), "altpred")
  expect_identical(annotation_name(get_annotation(x)), "altpred")
  expect_identical(
    annotation_metadata(get_annotation(x))$source,
    "alternative-predictor"
  )

  vcf_layer <- SynVCFAnnotation(
    name = "variants",
    vcf_file = "variants.vcf.gz",
    index_file = "variants.vcf.gz.tbi"
  )
  bw_layer <- SynBigWigAnnotation(
    name = "coverage",
    bigwig_file = "coverage.bw"
  )
  domain_layer <- SynProteinDomainAnnotation(
    name = "pfam",
    domain_file = "pfam.tsv",
    source_db = "Pfam"
  )

  expect_s4_class(vcf_layer, "SynGenomeAnnotation")
  expect_s4_class(vcf_layer, "SynIndAnnotation")
  expect_true(is_lazy(vcf_layer))
  expect_false(is_loaded(vcf_layer))
  expect_identical(annotation_scope(vcf_layer), "nucleotide")
  expect_s4_class(bw_layer, "SynGenomeAnnotation")
  expect_s4_class(bw_layer, "SynIndAnnotation")
  expect_identical(annotation_kind(bw_layer), "SynBigWigAnnotation")
  expect_s4_class(domain_layer, "SynProteinAnnotation")
  expect_s4_class(domain_layer, "SynIndAnnotation")
  expect_identical(annotation_scope(domain_layer), "protein")

  x <- add_annotation(x, vcf_layer)
  x <- add_annotation(x, bw_layer)
  x <- add_annotation(x, domain_layer)

  expect_setequal(
    annotation_names(x),
    c("default", "altpred", "variants", "coverage", "pfam")
  )
  expect_identical(active_feature_annotation(x), "altpred")
})

test_that("type-specific annotation verbs query their data sources", {
  vcf_path <- system.file(
    "extdata",
    "DL238.rename.ChrV.XZ1516.vcf.gz",
    package = "ggexon"
  )
  expect_true(nzchar(vcf_path))

  vcf_con <- gzfile(vcf_path, "rt")
  on.exit(close(vcf_con), add = TRUE)
  vcf_lines <- readLines(vcf_con, warn = FALSE)
  first_variant <- vcf_lines[!grepl("^#", vcf_lines)][1L]
  first_fields <- strsplit(first_variant, "\t", fixed = TRUE)[[1L]]

  query_chr <- first_fields[[1L]]
  query_pos <- as.integer(first_fields[[2L]])
  query_ref <- first_fields[[4L]]
  query_alt <- first_fields[[5L]]

  vcf_layer <- SynVCFAnnotation(name = "variants", vcf_file = vcf_path)
  vcf_hits <- query_variants(
    vcf_layer,
    chr = query_chr,
    start = query_pos,
    end = query_pos + 20L
  )
  expect_true(nrow(vcf_hits) >= 1L)
  expect_identical(as.character(vcf_hits$CHROM[[1L]]), query_chr)
  expect_identical(as.integer(vcf_hits$POS[[1L]]), query_pos)
  expect_identical(as.character(vcf_hits$REF[[1L]]), query_ref)
  expect_identical(as.character(vcf_hits$ALT[[1L]]), query_alt)

  bw_path <- tempfile(fileext = ".bw")
  bw_gr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr1"),
    ranges = IRanges::IRanges(start = c(1, 21), end = c(10, 30)),
    score = c(5, 8)
  )
  GenomeInfoDb::seqinfo(bw_gr) <- GenomeInfoDb::Seqinfo(
    seqnames = "chr1",
    seqlengths = 100
  )
  rtracklayer::export.bw(bw_gr, bw_path)
  bw_layer <- SynBigWigAnnotation(name = "coverage", bigwig_file = bw_path)
  bw_hits <- query_signal(bw_layer, chr = "chr1", start = 5, end = 22)
  expect_s4_class(bw_hits, "GRanges")
  expect_true(length(bw_hits) >= 1L)
  expect_true(all(as.character(GenomeInfoDb::seqnames(bw_hits)) == "chr1"))

  domain_path <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "protein_id\tdomain\tstart\tend",
      "protA\tPF0001\t5\t20",
      "protB\tPF0002\t10\t35",
      "protA\tPF0003\t40\t50"
    ),
    domain_path
  )
  domain_layer <- SynProteinDomainAnnotation(
    name = "pfam",
    domain_file = domain_path,
    keytype = "protein_id",
    source_db = "Pfam"
  )
  domain_hits <- query_domains(domain_layer, ids = "protA", domains = "PF0003")
  expect_identical(as.character(domain_hits$protein_id), "protA")
  expect_identical(as.character(domain_hits$domain), "PF0003")
})

test_that("SynAnnotationPatch now participates in the genome annotation hierarchy", {
  patch_gr <- GenomicRanges::GRanges(
    seqnames = "chr1",
    ranges = IRanges::IRanges(start = 1L, end = 10L)
  )

  patch_obj <- SynAnnotationPatch(
    name = "patch-1",
    patch_data = patch_gr,
    target_ids = "gene1",
    mode = "replace"
  )

  expect_s4_class(patch_obj, "SynAnnotationPatch")
  expect_s4_class(patch_obj, "SynGenomeAnnotation")
  expect_s4_class(patch_obj, "SynIndAnnotation")
  expect_s4_class(patch_obj, "SynAnnotation")
  expect_identical(source_file(patch_obj), "<patch>")
  expect_identical(annotation_scope(patch_obj), "nucleotide")
  expect_false(is_lazy(patch_obj))
  expect_true(is_loaded(patch_obj))
})

test_that("subset_feature_annotation accepts coords strings", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  )
  ann <- load_annotation(ann)

  full_gr <- annotation_data(ann)
  target_chr <- as.character(GenomeInfoDb::seqnames(full_gr))[[1L]]
  target_start <- IRanges::start(full_gr)[[1L]]
  target_end <- IRanges::end(full_gr)[[1L]]
  coords <- paste0(target_chr, ":", target_start, "-", target_end)

  subset_by_coords <- subset_feature_annotation(ann, coords = coords)
  subset_by_args <- subset_feature_annotation(
    ann,
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_identical(
    as.data.frame(annotation_data(subset_by_coords)),
    as.data.frame(annotation_data(subset_by_args))
  )
  expect_error(
    subset_feature_annotation(ann, chr = target_chr, coords = coords),
    "Provide either `coords` or `chr`/`start`/`end`"
  )
})

test_that("subset_feature_annotation returns an updated SynIndividual when given SynIndividual", {
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

  subset_x <- subset_feature_annotation(
    x,
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_s4_class(subset_x, "SynIndividual")
  expect_true(length(annotation_data(subset_x)) >= 1L)
  expect_true(all(as.character(GenomeInfoDb::seqnames(annotation_data(subset_x))) == target_chr))
})

test_that("SynProteinDomainAnnotation reads the shipped InterProScan export", {
  interpro_path <- system.file(
    "extdata",
    "InterProScan.tsv",
    package = "ggexon"
  )
  expect_true(nzchar(interpro_path))

  domain_layer <- SynProteinDomainAnnotation(
    name = "interpro",
    domain_file = interpro_path,
    keytype = "protein_id",
    source_db = "InterPro"
  )

  domain_hits <- query_domains(domain_layer)
  expect_true(nrow(domain_hits) > 0L)
  expect_true(all(c(
    "protein_id",
    "analysis",
    "signature_accession",
    "signature_description",
    "interpro_accession",
    "interpro_description",
    "domain",
    "domain_name"
  ) %in% colnames(domain_hits)))

  expect_identical(as.character(domain_hits$protein_id[[1L]]), "Sequence1")
  expect_identical(
    as.character(domain_hits$signature_accession[[1L]]),
    "G3DSA:3.30.160.60"
  )
  expect_identical(
    as.character(domain_hits$domain[[1L]]),
    "G3DSA:3.30.160.60"
  )
  expect_identical(
    as.character(domain_hits$domain_name[[1L]]),
    "Classic Zinc Finger"
  )

  filtered_hits <- query_domains(
    domain_layer,
    ids = "Sequence1",
    domains = "G3DSA:3.30.160.60"
  )
  expect_true(nrow(filtered_hits) >= 1L)
  expect_true(all(as.character(filtered_hits$protein_id) == "Sequence1"))
  expect_true(all(as.character(filtered_hits$domain) == "G3DSA:3.30.160.60"))
})

test_that("add_interproscan_annotation attaches the shipped InterProScan layer", {
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
  x <- add_interproscan_annotation(x)

  expect_true("interpro" %in% annotation_names(x))

  interpro_ann <- get_annotation(x, "interpro")
  expect_s4_class(interpro_ann, "SynProteinDomainAnnotation")
  expect_identical(annotation_scope(interpro_ann), "protein")
  expect_identical(annotation_metadata(interpro_ann), list())

  domain_hits <- query_domains(interpro_ann)
  expect_true(nrow(domain_hits) > 0L)
  expect_identical(as.character(domain_hits$protein_id[[1L]]), "Sequence1")
})

test_that("subset_feature_annotation returns a clean windowed snapshot", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  expect_true(nzchar(annotation_path))

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  )
  ann <- load_annotation(ann)

  gr <- annotation_data(ann)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]

  feature_index(ann) <- list(example = 1L)
  nucleotide_seq(ann) <- Biostrings::DNAStringSet(c(example = "ATG"))
  protein_seq(ann) <- Biostrings::AAStringSet(c(example = "M"))
  plot_cache(ann) <- list(example = data.frame(x = 1L))
  ann@patches <- list(
    window = SynAnnotationPatch(
      name = "window",
      patch_data = gr[1L],
      target_ids = "gene-1",
      mode = "replace"
    )
  )

  subset_ann <- subset_feature_annotation(
    ann,
    chr = target_chr,
    start = target_start,
    end = target_end
  )

  expect_s4_class(subset_ann, "SynFeatureAnnotation")
  expect_true(length(annotation_data(subset_ann)) >= 1L)
  expect_true(all(
    as.character(GenomeInfoDb::seqnames(annotation_data(subset_ann))) == target_chr
  ))
  expect_true(all(
    IRanges::start(annotation_data(subset_ann)) <= target_end &
      IRanges::end(annotation_data(subset_ann)) >= target_start
  ))
  expect_s4_class(base_annotation(subset_ann), "GRanges")
  expect_length(patches(subset_ann), 0L)
  expect_null(feature_index(subset_ann))
  expect_null(nucleotide_seq(subset_ann))
  expect_null(protein_seq(subset_ann))
  expect_identical(plot_cache(subset_ann), list())
  expect_true(all(IRanges::start(annotation_data(subset_ann)) >= target_start))
  expect_true(all(IRanges::end(annotation_data(subset_ann)) <= target_end))
  expect_identical(
    annotation_metadata(subset_ann)$subset_window,
    list(chr = target_chr, start = target_start, end = target_end)
  )
})

test_that("subset_feature_annotation supports gene and transcript selectors", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  expect_true(nzchar(annotation_path))

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  ) |>
    load_annotation()

  gr <- annotation_data(ann)
  meta <- S4Vectors::mcols(gr)
  gene_id <- unique(as.character(meta$gene_id[!is.na(meta$gene_id) & nzchar(meta$gene_id)]))[[1L]]
  transcript_id <- unique(as.character(meta$transcript_id[!is.na(meta$transcript_id) & nzchar(meta$transcript_id)]))[[1L]]

  gene_subset <- subset_feature_annotation(ann, gene = gene_id)
  gene_meta <- S4Vectors::mcols(annotation_data(gene_subset))
  expect_true(all(
    (is.na(gene_meta$gene_id) | !nzchar(gene_meta$gene_id)) | gene_meta$gene_id == gene_id
  ))

  transcript_subset <- subset_feature_annotation(ann, transcript = transcript_id)
  tx_meta <- S4Vectors::mcols(annotation_data(transcript_subset))
  expect_true(any(tx_meta$transcript_id == transcript_id, na.rm = TRUE))
  expect_true(all(
    (is.na(tx_meta$transcript_id) | !nzchar(tx_meta$transcript_id)) |
      tx_meta$transcript_id == transcript_id
  ))
})

test_that("subset_feature_annotation reports retained rows and features", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  expect_true(nzchar(annotation_path))

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  ) |>
    load_annotation()

  gr <- annotation_data(ann)
  target_chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
  target_start <- IRanges::start(gr)[[1L]]
  target_end <- IRanges::end(gr)[[1L]]

  expect_message(
    subset_feature_annotation(
      ann,
      chr = target_chr,
      start = target_start,
      end = target_end
    ),
    "kept .* rows, .* genes, and .* transcripts"
  )
})

test_that("subset_feature_annotation intersects selectors with coordinates", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  expect_true(nzchar(annotation_path))

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  ) |>
    load_annotation()

  gr <- annotation_data(ann)
  meta <- S4Vectors::mcols(gr)
  transcript_id <- unique(as.character(meta$transcript_id[!is.na(meta$transcript_id) & nzchar(meta$transcript_id)]))[[1L]]
  tx_gr <- query_features(ann, transcripts = transcript_id, feature_type = NULL)
  tx_chr <- unique(as.character(GenomeInfoDb::seqnames(tx_gr)))[[1L]]
  tx_start <- min(IRanges::start(tx_gr))
  tx_end <- max(IRanges::end(tx_gr))
  clip_end <- tx_start + floor((tx_end - tx_start) / 2)

  subset_ann <- subset_feature_annotation(
    ann,
    transcript = transcript_id,
    chr = tx_chr,
    start = tx_start,
    end = clip_end
  )

  subset_gr <- annotation_data(subset_ann)
  subset_meta <- S4Vectors::mcols(subset_gr)
  expect_true(all(as.character(GenomeInfoDb::seqnames(subset_gr)) == tx_chr))
  expect_true(all(IRanges::start(subset_gr) >= tx_start))
  expect_true(all(IRanges::end(subset_gr) <= clip_end))
  expect_true(all(
    (is.na(subset_meta$transcript_id) | !nzchar(subset_meta$transcript_id)) |
      subset_meta$transcript_id == transcript_id
  ))
})

test_that("build_feature_index also supports SynFeatureAnnotation objects", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  expect_true(nzchar(annotation_path))

  ann <- SynFeatureAnnotation(
    name = "default",
    annotation_file = annotation_path
  )
  ann <- build_feature_index(ann)

  expect_true(is.list(feature_index(ann)))
  expect_true(length(feature_index(ann)$seqname) > 0L)
  expect_true(length(feature_index(ann)$type) > 0L)
})

test_that("rename_domain_annotation_ids rewrites the domain key column", {
  domain_path <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "protein_id\tdomain\tstart\tend",
      "Sequence1\tPF0001\t1\t20",
      "Sequence2\tPF0002\t5\t10"
    ),
    domain_path
  )

  ann <- SynProteinDomainAnnotation(
    name = "interpro",
    domain_file = domain_path,
    keytype = "protein_id",
    source_db = "InterPro"
  )
  ann <- rename_domain_annotation_ids(
    ann,
    mapping = c(Sequence1 = "txA"),
    to = "transcript_id"
  )

  domain_hits <- query_domains(ann)
  expect_identical(ann@keytype, "transcript_id")
  expect_identical(as.character(domain_hits$transcript_id[[1L]]), "txA")
  expect_true(is.na(domain_hits$transcript_id[[2L]]))
})

test_that("geom_motif projects renamed domain coordinates onto genomic tracks", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  patch_path <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")
  interpro_path <- system.file("extdata", "InterProScan.tsv", package = "ggexon")

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) |>
    load_annotation()
  x <- patch_annotation_from_gff(
    x,
    patch_file = patch_path,
    mode = "replace",
    name = "ta-correction"
  )
  x <- add_interproscan_annotation(
    x,
    domain_file = interpro_path,
    name = "interpro"
  )
  x <- rename_domain_annotation_ids(
    x,
    annotation = "interpro",
    mapping = c(Sequence1 = "mRNAXZ1516_zina-1"),
    from = "protein_id",
    to = "transcript_id",
    drop_unmapped = TRUE
  )

  motif_df <- syn_to_motif_df(
    x,
    annotation = "interpro",
    chr = "V_RagTag",
    subset = c(21574000, 21583000),
    model = c("SMART", "PANTHER"),
    y_offset = -1
  )

  expect_true(nrow(motif_df) > 0L)
  expect_true(all(as.character(motif_df$transcripts) == "mRNAXZ1516_zina-1"))
  expect_true("text" %in% names(motif_df))
  expect_setequal(unique(as.character(motif_df$model)), c("SMART", "PANTHER"))
  expect_identical(unique(as.character(motif_df$track)), syn_id(x))
  expect_gte(min(motif_df$xmin), 21575003)
  expect_lte(max(motif_df$xmax), 21582693)
  model_y <- tapply(motif_df$ymin, motif_df$model, function(x) unique(x)[1L])
  expect_gt(model_y[["SMART"]], model_y[["PANTHER"]])

  plot_obj <- ggexon(x) +
    geom_motif(
      annotation = "interpro",
      chr = "V_RagTag",
      subset = c(21574000, 21583000),
      model = c("SMART", "PANTHER"),
      y_offset = -1
    )
  build <- ggplot2::ggplot_build(plot_obj)
  motif_layer <- build$data[[1L]]

  expect_true(nrow(motif_layer) > 0L)
  expect_identical(unique(as.character(motif_layer$track)), syn_id(x))
})

test_that("project_domains_to_genome filters InterProScan models and motifs", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  patch_path <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")
  interpro_path <- system.file("extdata", "InterProScan.tsv", package = "ggexon")

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) |>
    load_annotation()
  x <- patch_annotation_from_gff(
    x,
    patch_file = patch_path,
    mode = "replace",
    name = "ta-correction"
  )
  x <- add_interproscan_annotation(
    x,
    domain_file = interpro_path,
    name = "interpro"
  )
  x <- rename_domain_annotation_ids(
    x,
    annotation = "interpro",
    mapping = c(Sequence1 = "mRNAXZ1516_zina-1"),
    from = "protein_id",
    to = "transcript_id",
    drop_unmapped = TRUE
  )

  projected <- project_domains_to_genome(
    x,
    annotation = "interpro",
    model = "Gene3D",
    motif = "Classic Zinc Finger",
    chr = "V_RagTag",
    start = 21574000,
    end = 21583000
  )

  expect_true(nrow(projected) > 0L)
  expect_true(all(as.character(projected$model) == "Gene3D"))
  expect_true(all(as.character(projected$motif) == "Classic Zinc Finger"))
})

test_that("store_projected_domains keeps projected tables on SynIndividual", {
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

  projected_tbl <- data.frame(
    seqnames = "chr1",
    xmin = 1L,
    xmax = 10L,
    strand = "+",
    transcripts = "tx1",
    model = "SMART",
    motif = "mock motif",
    domain_id = "IPR000001",
    text = "mock motif",
    stringsAsFactors = FALSE
  )

  x <- store_projected_domains(x, projected_tbl, name = "mock_projection")

  expect_true("mock_projection" %in% names(projected_domains(x)))
  expect_identical(
    as.character(projected_domains(x)[["mock_projection"]]$motif[[1L]]),
    "mock motif"
  )
})

test_that("set_gene_labels stores plot labels without replacing stable IDs", {
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
  x <- load_annotation(x)

  ann <- get_annotation(x)
  gene_gr <- annotation_data(ann)[S4Vectors::mcols(annotation_data(ann))$type == "gene"]
  target_gene <- as.character(S4Vectors::mcols(gene_gr)$ID[[1L]])

  x <- set_gene_labels(x, c(setNames("unc-1", target_gene)))
  ann2 <- get_annotation(x)
  ann2_meta <- S4Vectors::mcols(annotation_data(ann2))

  gene_rows <- as.character(ann2_meta$ID) == target_gene
  expect_true(any(gene_rows))
  expect_true(all(as.character(ann2_meta$plot_label[gene_rows]) == "unc-1"))
  expect_true(all(as.character(ann2_meta$ID[gene_rows]) == target_gene))
  expect_identical(as.character(label_map(ann2)$feature_id), target_gene)
  expect_identical(as.character(label_map(ann2)$label), "unc-1")
})

test_that("set_gene_labels normalizes and validates mapping inputs", {
  mapping <- .normalize_label_mapping(data.frame(
    ignored = c("x", "y"),
    gene_name = c("unc-1", "rpl-8"),
    gene_id = c("WBGene00000001", "WBGene00000002"),
    stringsAsFactors = FALSE
  ))

  expect_identical(
    as.character(mapping$feature_id),
    c("WBGene00000001", "WBGene00000002")
  )
  expect_identical(as.character(mapping$label), c("unc-1", "rpl-8"))
  expect_error(.normalize_label_mapping(c(g1 = "one", g1 = "two")), "unique")
  expect_error(.normalize_label_mapping(stats::setNames("missing", "")), "feature IDs")
  expect_error(
    .normalize_label_mapping(data.frame(gene_id = "g1", label = "")),
    "labels"
  )
})

test_that("geom_genelabel uses Syn annotation plot labels with syn-aware defaults", {
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
  x <- load_annotation(x)

  ann <- get_annotation(x)
  gene_gr <- annotation_data(ann)[S4Vectors::mcols(annotation_data(ann))$type == "gene"]
  target_gene <- as.character(S4Vectors::mcols(gene_gr)$ID[[1L]])
  target_chr <- as.character(GenomeInfoDb::seqnames(gene_gr))[1L]
  target_start <- IRanges::start(gene_gr)[[1L]]
  target_end <- IRanges::end(gene_gr)[[1L]]

  x <- set_gene_labels(x, c(setNames("unc-1", target_gene)))

  plot_obj <- ggexon(x) +
    geom_genelabel(
      chr = target_chr,
      subset = c(target_start, target_end)
    )
  build <- ggexon_build(plot_obj)

  expect_true(nrow(build@data[[1L]]) >= 1L)
  expect_true("label" %in% names(build@data[[1L]]))
  expect_true("unc-1" %in% build@data[[1L]]$label)
})

test_that("patch_annotation replaces a gene model and clears feature caches", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x_base <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) |>
    load_annotation()

  base_ann <- get_annotation(x_base)
  base_gr <- annotation_data(base_ann)
  base_meta <- S4Vectors::mcols(base_gr)
  target_gene <- as.character(base_meta$ID[as.character(base_meta$type) == "gene"][[1L]])

  x <- x_base |>
    translate_protein(genes = target_gene)

  ann <- get_annotation(x)
  ann_gr <- annotation_data(ann)
  ann_meta <- S4Vectors::mcols(ann_gr)
  expect_true(nzchar(target_gene))

  original_gene_rows <- sum(as.character(ann_meta$ID) == target_gene)

  patch_gr <- ann_gr[as.character(ann_meta$ID) == target_gene][1]
  S4Vectors::mcols(patch_gr)$plot_label <- "patched-gene"
  patch_obj <- SynAnnotationPatch(
    name = "replace-first-gene",
    patch_data = patch_gr,
    target_ids = target_gene,
    mode = "replace"
  )

  x2 <- patch_annotation(x, patch_obj)
  ann2 <- get_annotation(x2)
  ann2_gr <- annotation_data(ann2)
  ann2_meta <- S4Vectors::mcols(ann2_gr)

  expect_length(list_patches(x2), 1L)
  expect_identical(patch_name(list_patches(x2)[[1L]]), "replace-first-gene")
  expect_true(sum(as.character(ann2_meta$ID) == target_gene) <= original_gene_rows)
  expect_true(any(as.character(ann2_meta$plot_label) == "patched-gene"))
  expect_null(nucleotide_seq(ann2))
  expect_null(protein_seq(ann2))

  x3 <- clear_patches(x2)
  expect_length(list_patches(x3), 0L)
  expect_null(nucleotide_seq(get_annotation(x3)))
  expect_null(protein_seq(get_annotation(x3)))
})

test_that("replace-mode patches remove all old features overlapping the patch range", {
  base_gr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr1", "chr1"),
    ranges = IRanges::IRanges(start = c(100L, 120L, 180L), end = c(160L, 140L, 260L)),
    strand = c("+", "+", "-")
  )
  S4Vectors::mcols(base_gr) <- S4Vectors::DataFrame(
    type = c("gene", "exon", "gene"),
    ID = c("old_gene", "old_exon", "old_other"),
    Parent = c(NA_character_, "old_gene", NA_character_),
    gene_id = c("old_gene", "old_gene", "old_other"),
    gene_name = c("old_gene", "old_gene", "old_other")
  )

  ann <- SynFeatureAnnotation(name = "mock", annotation_file = "mock.gff")
  annotation_data(ann) <- base_gr

  patch_gr <- GenomicRanges::GRanges(
    seqnames = "chr1",
    ranges = IRanges::IRanges(start = 150L, end = 220L),
    strand = "+"
  )
  S4Vectors::mcols(patch_gr) <- S4Vectors::DataFrame(
    type = "gene",
    ID = "patched_gene",
    Parent = NA_character_,
    gene_id = "patched_gene",
    gene_name = "patched_gene"
  )

  patched <- patch_annotation(
    ann,
    patch = patch_gr,
    target_ids = "old_gene",
    mode = "replace",
    name = "overlap-replace"
  )
  patched_ids <- as.character(S4Vectors::mcols(annotation_data(patched))$ID)

  expect_false("old_gene" %in% patched_ids)
  expect_false("old_exon" %in% patched_ids)
  expect_false("old_other" %in% patched_ids)
  expect_true("patched_gene" %in% patched_ids)
})

test_that("read_patch_gff and patch_annotation_from_gff use the real patch file", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  patch_path <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")

  expect_true(nzchar(patch_path))

  patch_gr <- read_patch_gff(patch_path)
  patch_meta <- S4Vectors::mcols(patch_gr)
  patch_gene_ids <- unique(as.character(patch_meta$ID[as.character(patch_meta$type) == "gene"]))
  patch_gene_ids <- patch_gene_ids[!is.na(patch_gene_ids) & nzchar(patch_gene_ids)]

  expect_s4_class(patch_gr, "GRanges")
  expect_gte(length(patch_gene_ids), 2L)

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) |>
    load_annotation()

  x2 <- patch_annotation_from_gff(
    x,
    patch_file = patch_path,
    mode = "replace",
    name = "ta-correction"
  )

  ann2 <- get_annotation(x2)
  ann2_meta <- S4Vectors::mcols(annotation_data(ann2))
  patched_genes <- unique(as.character(ann2_meta$ID[as.character(ann2_meta$type) == "gene"]))

  expect_true(all(patch_gene_ids %in% patched_genes))
  expect_identical(names(list_patches(x2)), "ta-correction")
  expect_identical(patch_mode(list_patches(x2)[[1L]]), "replace")
})

test_that("geom_exon falls back to CDS when patched transcripts have no exon rows", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  patch_path <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")

  x <- SynIndividual(
    genome_file = genome_path,
    annotation_file = annotation_path
  ) |>
    load_annotation()

  x <- patch_annotation_from_gff(
    x,
    patch_file = patch_path,
    mode = "replace",
    name = "ta-correction"
  )

  plot_obj <- ggexon(x) +
    geom_exon(
      chr = "V_RagTag",
      subset = c(21574000, 21583000)
    )
  build <- ggplot2::ggplot_build(plot_obj)
  exon_layer <- build$data[[1L]]
  zina_rows <- grepl("zina-1", as.character(exon_layer$transcripts), fixed = TRUE)

  expect_true(any(zina_rows))
  expect_true(all(as.character(exon_layer$type[zina_rows]) == "exon"))
  expect_equal(sum(zina_rows), 15L)
})
