test_that("strip_scale() returns correct spec structure", {
  spec <- strip_scale(gene_gap_ratio = 3, align = "left")
  expect_s3_class(spec, "ggexon_strip_scale_spec")
  expect_identical(spec$gene_gap_ratio, 3)
  expect_identical(spec$align, "left")
})

test_that("strip_scale() validates gene_gap_ratio", {
  expect_error(strip_scale(gene_gap_ratio = 0), "positive")
  expect_error(strip_scale(gene_gap_ratio = -1), "positive")
  expect_error(strip_scale(gene_gap_ratio = c(2, 3)), "single")
  expect_error(strip_scale(gene_gap_ratio = NA_real_), "positive")
})

test_that("strip_scale() validates align", {
  expect_error(strip_scale(align = "middle"), "should be one of")
  expect_identical(strip_scale(align = "right")$align, "right")
  expect_identical(strip_scale(align = "center")$align, "center")
})

test_that("strip_scale() defaults work", {
  spec <- strip_scale()
  expect_null(spec$gene_gap_ratio)
  expect_identical(spec$align, "left")
})

test_that("ggplot_add validates ggexon-only usage", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  expect_error(p + strip_scale(), "ggexon plot")
})

test_that("ggplot_add rejects strip_scale when genomic_x_scale is present", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    scale_x_ggexon_genomic(intron_factor = 10) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  expect_error(p + strip_scale(), "mutually exclusive")
})

test_that("ggplot_add stores strip_scale on plot", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  p <- p + strip_scale(gene_gap_ratio = 2, align = "center")
  expect_s3_class(p@strip_scale, "ggexon_strip_scale_spec")
  expect_identical(p@strip_scale$gene_gap_ratio, 2)
  expect_identical(p@strip_scale$align, "center")
})

test_that("strip_scale forces fixed x for single-individual genelabel plot", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  scale_x_values <- unique(as.integer(built@layout$layout$SCALE_X))
  expect_identical(scale_x_values, 1L)
  expect_true(nrow(built@data[[1L]]) > 0L)
})

test_that("strip_scale gene widths are uniform per track for single individual", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  label_data <- built@data[[1L]]

  gene_widths <- vapply(split(label_data, label_data$transcripts), function(df) {
    diff(range(c(df$xmin, df$xmax), na.rm = TRUE))
  }, numeric(1))
  gene_widths <- gene_widths[is.finite(gene_widths) & gene_widths > 0]

  expect_true(length(gene_widths) >= 1L)
  expect_equal(sd(gene_widths), 0, tolerance = 1e-6)
})

test_that("strip_scale errors when no genelabel layer present", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_exon(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale() +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  expect_error(ggexon_build(p), "geom_genelabel")
})

test_that("strip_scale intergenic gaps are uniform", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  label_data <- built@data[[1L]]

  genes <- split(label_data, label_data$transcripts)
  gene_x_ranges <- vapply(genes, function(df) {
    c(min(df$xmin, df$xmax, na.rm = TRUE),
      max(df$xmin, df$xmax, na.rm = TRUE))
  }, numeric(2))
  gene_x_ranges <- gene_x_ranges[, order(gene_x_ranges[1L, ]), drop = FALSE]

  if (ncol(gene_x_ranges) >= 2L) {
    gaps <- gene_x_ranges[1L, -1L] - gene_x_ranges[2L, -ncol(gene_x_ranges)]
    gaps <- gaps[gaps > 0]
    if (length(gaps) >= 2L) {
      expect_equal(sd(gaps), 0, tolerance = 1e-6)
    }
  }
  expect_true(ncol(gene_x_ranges) >= 1L)
})

test_that("strip_scale left alignment for two individuals with different gene counts", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )

  sp <- SynSpecies(name = "test")
  sp <- add_individual(
    sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )

  p <- ggexon(sp) +
    geom_genelabel(
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21558028, 21620381)
    ) +
    geom_genelabel(
      species = "N2",
      chr = "V",
      subset = c(20454111, 20491853)
    ) +
    strip_scale(gene_gap_ratio = 3, align = "left") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)

  scale_x <- unique(as.integer(built@layout$layout$SCALE_X))
  expect_identical(scale_x, 1L)

  pan_tracks <- as.character(built@layout$layout$track)
  expect_setequal(pan_tracks, c("XZ1516", "N2"))

  xz_data <- built@data[[1L]]
  n2_data <- built@data[[2L]]

  xz_min <- min(xz_data$xmin, xz_data$xmax, na.rm = TRUE)
  n2_min <- min(n2_data$xmin, n2_data$xmax, na.rm = TRUE)
  expect_equal(xz_min, n2_min, tolerance = 1e-6)
})

test_that("strip_scale center alignment offsets sparse tracks correctly", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )

  sp <- SynSpecies(name = "test")
  sp <- add_individual(
    sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )

  p_left <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V", subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V", subset = c(20454111, 20491853)) +
    strip_scale(gene_gap_ratio = 3, align = "left") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  p_center <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V", subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V", subset = c(20454111, 20491853)) +
    strip_scale(gene_gap_ratio = 3, align = "center") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built_left <- ggexon_build(p_left)
  built_center <- ggexon_build(p_center)

  left_data <- built_left@data[[1L]]
  center_data <- built_center@data[[1L]]

  left_min <- min(left_data$xmin, left_data$xmax, na.rm = TRUE)
  center_min <- min(center_data$xmin, center_data$xmax, na.rm = TRUE)
  expect_true(center_min > left_min)
})

test_that("strip_scale auto-derives gene_gap_ratio from data", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale() +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  expect_true(nrow(built@data[[1L]]) > 0L)
})

test_that("strip_scale preserves genomic_x columns for later reference", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  label_data <- built@data[[1L]]

  expect_true("genomic_xmin" %in% names(label_data))
  expect_true("genomic_xmax" %in% names(label_data))
  expect_true(all(label_data$genomic_xmin >= 21558028))
  expect_true(all(label_data$genomic_xmax <= 21620381))
})

test_that("strip_scale works with facet_genomictree", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )

  sp <- SynSpecies(name = "test")
  sp <- add_individual(
    sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = n2_annotation, id = "N2", annotation_format = "gtf"
    )
  )

  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V") +
    geom_genelabel(species = "N2", chr = "V") +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomictree(ggplot2::vars(track), scales = "free_x")

  expect_no_error(ggexon_build(p))
})

test_that("strip_scale with multiple genelabel layers uses first one for intervals", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )

  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    geom_genelabel(chr = "RagTag_V", subset = c(21550000, 21630000),
                   label_direction = "bottom") +
    strip_scale(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  expect_true(nrow(built@data[[1L]]) > 0L)
  expect_true(nrow(built@data[[2L]]) > 0L)
})

test_that("strip_scale right alignment places sparse track at right edge", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )

  sp <- SynSpecies(name = "test")
  sp <- add_individual(
    sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = n2_annotation, id = "N2", annotation_format = "gtf"
    )
  )

  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V", subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V", subset = c(20454111, 20491853)) +
    strip_scale(gene_gap_ratio = 3, align = "right") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(p)
  xz_data <- built@data[[1L]]
  n2_data <- built@data[[2L]]

  xz_max <- max(xz_data$xmin, xz_data$xmax, na.rm = TRUE)
  n2_max <- max(n2_data$xmin, n2_data$xmax, na.rm = TRUE)
  expect_equal(xz_max, n2_max, tolerance = 1e-6)
})

# ── homo_align tests ────────────────────────────────────────────────────

test_that("strip_scale validates homo_align and species_ratio", {
  expect_identical(strip_scale(homo_align = "yes")$homo_active, TRUE)
  expect_identical(strip_scale(homo_align = TRUE)$homo_active, TRUE)
  expect_identical(strip_scale(homo_align = "C. elegans")$homo_active, TRUE)
  expect_identical(strip_scale()$homo_active, FALSE)
  expect_error(strip_scale(species_ratio = 0), "in \\(0, 1\\]")
  expect_error(strip_scale(species_ratio = 2), "in \\(0, 1\\]")
  expect_error(strip_scale(species_ratio = c(0.3, 0.5)), "number in")
})

test_that("strip_scale warns when align is set with homo_align", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )
  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(align = "center", homo_align = TRUE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_warning(ggexon_build(p), "align.*ignored")
})

test_that("strip_scale homo_align builds with single species", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )
  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(homo_align = TRUE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_no_error(ggexon_build(p))
})

test_that("strip_scale homo_align builds with two species and no homology", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )
  sp <- SynSpecies(name = "test")
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516"))
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = n2_annotation, id = "N2",
                        annotation_format = "gtf"))
  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V",
                   subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V",
                   subset = c(20454111, 20491853)) +
    strip_scale(homo_align = TRUE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_no_error(ggexon_build(p))
})

test_that("strip_scale homo_align forces fixed x", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )
  sp <- SynSpecies(name = "test")
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516"))
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = n2_annotation, id = "N2",
                        annotation_format = "gtf"))
  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V") +
    geom_genelabel(species = "N2", chr = "V") +
    strip_scale(homo_align = TRUE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  scale_x <- unique(as.integer(built@layout$layout$SCALE_X))
  expect_identical(scale_x, 1L)
})

test_that("strip_scale homo_align with explicit reference species name", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )
  sp <- SynSpecies(name = "test")
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516"))
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = n2_annotation, id = "N2",
                        annotation_format = "gtf"))
  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V",
                   subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V",
                   subset = c(20454111, 20491853)) +
    strip_scale(homo_align = "N2") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  expect_true(nrow(built@data[[1L]]) > 0L)
})

test_that("strip_scale homo_align errors for unknown reference species", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  sp <- SynSpecies(name = "test") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "XZ1516")
    )
  p <- ggexon(sp) +
    geom_genelabel(chr = "RagTag_V", subset = c(21558028, 21620381)) +
    strip_scale(homo_align = "NoSuchSpecies") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_error(ggexon_build(p), "not found in genelabel tracks")
})

test_that("strip_scale homo_align with homology table aligns genes", {
  annotation_path <- system.file(
    "extdata", "caenorhabditis_XZ1516.gff3", package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata", "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf", package = "ggexon"
  )
  sp <- SynSpecies(name = "test")
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = annotation_path, id = "XZ1516"))
  sp <- add_individual(sp,
    test_syn_individual(annotation_file = n2_annotation, id = "N2",
                        annotation_format = "gtf"))

  xz_genes <- query_features(individuals(sp)[["XZ1516"]],
    chr = "RagTag_V", start = 21558028, end = 21620381, feature_type = "gene")
  n2_genes <- query_features(individuals(sp)[["N2"]],
    chr = "V", start = 20454111, end = 20491853, feature_type = "gene")
  xz_ids <- unique(as.character(S4Vectors::mcols(xz_genes)$gene_id))
  xz_ids <- xz_ids[!is.na(xz_ids) & nzchar(xz_ids)]
  n2_ids <- unique(as.character(S4Vectors::mcols(n2_genes)$gene_id))
  n2_ids <- n2_ids[!is.na(n2_ids) & nzchar(n2_ids)]

  n_pairs <- min(length(xz_ids), length(n2_ids), 3L)
  if (n_pairs > 0L) {
    homo_tbl <- data.frame(
      query_gene = xz_ids[seq_len(n_pairs)],
      reference_gene = n2_ids[seq_len(n_pairs)],
      stringsAsFactors = FALSE
    )
    ha <- HomologyAnnotation(name = "test_homology",
      reference_species = "N2", query_species = "XZ1516",
      homology_table = homo_tbl)
    sp <- add_homology_annotation(sp, ha)
  }

  p <- ggexon(sp) +
    geom_genelabel(species = "XZ1516", chr = "RagTag_V",
                   subset = c(21558028, 21620381)) +
    geom_genelabel(species = "N2", chr = "V",
                   subset = c(20454111, 20491853)) +
    strip_scale(gene_gap_ratio = 3, homo_align = TRUE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  expect_true(nrow(built@data[[1L]]) > 0L)
  expect_true(nrow(built@data[[2L]]) > 0L)
})
