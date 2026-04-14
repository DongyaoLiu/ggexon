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
  expect_true(is_lazy(vcf_layer))
  expect_false(is_loaded(vcf_layer))
  expect_identical(annotation_scope(vcf_layer), "nucleotide")
  expect_s4_class(bw_layer, "SynGenomeAnnotation")
  expect_identical(annotation_kind(bw_layer), "SynBigWigAnnotation")
  expect_s4_class(domain_layer, "SynProteinAnnotation")
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
