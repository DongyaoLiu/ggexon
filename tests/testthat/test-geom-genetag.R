test_that("geom_genetag polygon data uses exon bodies with strand triangles", {
  data <- data.frame(
    xmin = c(0, 10),
    xmax = c(10, 20),
    y = c(1, 2),
    strand = c("+", "-"),
    PANEL = 1L,
    group = 1:2,
    colour = "black",
    fill = "grey35",
    linewidth = 0.25,
    linetype = 1,
    alpha = NA_real_
  )

  poly <- .genetag_polygon_data(data, exon_height = 0.5, arrow_fraction = 0.2)

  expect_identical(nrow(poly), 10L)

  plus <- poly[poly$group == 1L, , drop = FALSE]
  minus <- poly[poly$group == 2L, , drop = FALSE]

  expect_equal(range(plus$y), c(0.75, 1.25))
  expect_true(any(plus$x == 10 & plus$y == 1))
  expect_equal(sum(plus$x == 0), 2L)
  expect_equal(range(minus$y), c(1.75, 2.25))
  expect_true(any(minus$x == 10 & minus$y == 2))
  expect_equal(sum(minus$x == 20), 2L)

  alias_poly <- .genetag_polygon_data(data[1L, , drop = FALSE], height = 0.4)
  expect_equal(range(alias_poly$y), c(0.8, 1.2))
})

test_that("geom_genetag renders with data-default aesthetics", {
  data <- data.frame(
    xmin = c(1, 5),
    xmax = c(4, 8),
    y = c(1, 2),
    strand = c("+", "-"),
    gene = c("g1", "g2")
  )

  p <- ggplot2::ggplot(data) +
    geom_genetag(ggplot2::aes(fill = gene))

  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})

test_that("geom_genetag auto labels fall back outside when labels do not fit", {
  data <- data.frame(
    xmin = c(0, 10),
    xmax = c(8, 18),
    y = 0.4,
    strand = c("+", "+"),
    label = c("g1", "very_long_gene_label"),
    track = "track_a",
    stringsAsFactors = FALSE
  )

  layout <- .genetag_label_layout(
    data,
    label_position = "auto",
    exon_height = 0.8,
    panel_width_mm = 20
  )

  expect_equal(layout$inside$label, "g1")
  expect_equal(layout$outside$label, "very_long_gene_label")
  expect_equal(layout$outside$label_pos, "top")
  expect_gt(layout$outside$y, layout$outside$gene_ymax)

  inside_only <- .genetag_label_layout(
    data,
    label_position = "inside",
    exon_height = 0.8,
    panel_width_mm = 20
  )
  expect_equal(inside_only$inside$label, "g1")
  expect_equal(nrow(inside_only$outside), 0L)
})

test_that("geom_genetag supports prefixed label aesthetics", {
  data <- data.frame(
    xmin = c(0, 10),
    xmax = c(8, 18),
    y = 1,
    strand = c("+", "-"),
    gene = c("g1", "g2"),
    text_colour = c("red", "blue"),
    link_colour = c("orange", "green"),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    geom_genetag(
      data = data,
      ggplot2::aes(
        label_colour = text_colour,
        label_link_colour = link_colour
      ),
      label_position = "outside"
    )
  built <- ggplot2::ggplot_build(p)$data[[1]]

  expect_equal(built$label, data$gene)
  expect_equal(built$label_colour, data$text_colour)
  expect_equal(built$label_link_colour, data$link_colour)
  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})

test_that("geom_genetag tandem label collapse stays within tracks", {
  data <- data.frame(
    xmin = c(0, 2, 10, 12),
    xmax = c(1, 3, 11, 13),
    y = 0.4,
    strand = "+",
    label = "dup",
    track = c("track_a", "track_a", "track_b", "track_b"),
    stringsAsFactors = FALSE
  )

  layout <- .genetag_label_layout(
    data,
    label_position = "outside",
    collapse_tandem = TRUE,
    exon_height = 0.8,
    panel_width_mm = 40
  )

  expect_equal(nrow(layout$outside), 2L)
  expect_equal(layout$outside$track, c("track_a", "track_b"))
  expect_length(layout$tandem_anchors, 2L)

  p <- ggplot2::ggplot() +
    geom_genetag(
      data = data,
      label_position = "outside",
      collapse_tandem = TRUE
    )
  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})

test_that("gene-tag layout modes can union gaps and feature lengths independently", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    xmin = c(0, 15, 100, 140),
    xmax = c(10, 25, 120, 150),
    start = c(0, 15, 100, 140),
    end = c(10, 25, 120, 150),
    stringsAsFactors = FALSE
  )

  scaled <- .genetag_apply_layout_modes(data, inter_genetic = "scaled", exon_length = "scaled")
  expect_identical(scaled, data)

  union_gap <- .genetag_apply_layout_modes(data, inter_genetic = "union", exon_length = "scaled")
  expect_equal(union_gap$xmin, c(0, 30, 0, 40))
  expect_equal(union_gap$xmax, c(10, 40, 20, 50))
  expect_equal(union_gap$genomic_xmin, data$xmin)

  union_length <- .genetag_apply_layout_modes(data, inter_genetic = "scaled", exon_length = "union")
  expect_equal(union_length$xmin, c(0, 25, 0, 40))
  expect_equal(union_length$xmax, c(20, 35, 20, 50))

  union_both <- .genetag_apply_layout_modes(data, inter_genetic = "union", exon_length = "union")
  expect_equal(union_both$xmin, c(0, 40, 0, 40))
  expect_equal(union_both$xmax, c(20, 50, 20, 50))
  expect_equal(union_both$layout_index, c(1L, 2L, 1L, 2L))
})

test_that("rectangular ggtree segments can be added to a ggexon faceted plot", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  tree <- ape::read.tree(text = "(A:0.1,B:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  tree_segments <- compile_ggtree_rectangular_segments(tree_plot = tree_plot)

  expect_true(all(c("track", "x", "xend", "y", "yend") %in% names(tree_segments)))
  expect_true(any(tree_segments$segment == "horizontal"))
  expect_true(any(tree_segments$segment == "vertical"))

  gene_tags <- data.frame(
    track = "Gene tags",
    xmin = c(0, 10),
    xmax = c(8, 18),
    y = c(1, 2),
    strand = c("+", "-"),
    gene = c("gA", "gB")
  )

  p <- ggexon() +
    tree_plot +
    geom_genetag(data = gene_tags, ggplot2::aes(fill = gene)) +
    facet_genomics(ggplot2::vars(track), nrow = 1, scales = "free_x")

  expect_true(inherits(suppressWarnings(ggplot2::ggplotGrob(p)), "gtable"))
})

test_that("compile_ggtree_genetag aligns gene rows to rectangular ggtree tips", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  sp <- SynSpecies(name = "worms")
  for (id in c("XZ1516", "N2")) {
    sp <- add_individual(
      sp,
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = id
      )
    )
  }

  tree <- ape::read.tree(text = "(XZ1516:0.1,N2:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  species_tree_plot(sp) <- tree_plot
  gene_tags <- compile_ggtree_genetag(
    sp,
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )

  expect_true(nrow(gene_tags) > 0L)
  expect_false("y" %in% names(gene_tags))
  expect_true(all(c("id", "tree_y", "xmin", "xmax", "strand") %in% names(gene_tags)))
  expect_true(all(c("genomic_xmin", "genomic_xmax", "gene_key") %in% names(gene_tags)))
  expect_setequal(unique(gene_tags$id), c("XZ1516", "N2"))

  p <- ggtree::facet_plot(
    tree_plot,
    panel = "genes",
    data = gene_tags,
    geom = geom_genetag,
    mapping = ggplot2::aes(xmin = xmin, xmax = xmax, strand = strand, fill = gene)
  )

  expect_true(inherits(suppressWarnings(ggplot2::ggplotGrob(p)), "gtable"))
})

test_that("gene tags expose direct transcript children as homology aliases", {
  annotation_path <- tempfile(fileext = ".gff3")
  writeLines(
    c(
      "##gff-version 3",
      "chrI\ttest\tgene\t10\t100\t.\t+\t.\tID=gene-GCK72_021860;Name=GCK72_021860",
      paste0(
        "chrI\ttest\tmRNA\t10\t100\t.\t+\t.\t",
        "ID=rna-XM_053734541.1;Parent=gene-GCK72_021860;",
        "Name=XM_053734541.1;transcript_id=XM_053734541.1"
      ),
      "chrI\ttest\texon\t10\t100\t.\t+\t.\tID=exon-XM_053734541.1-1;Parent=rna-XM_053734541.1"
    ),
    annotation_path
  )

  individual <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver(),
    id = "query"
  )
  individual <- load_annotation(individual)
  sp <- SynSpecies(name = "worms") |>
    add_individual(individual) |>
    add_homology_annotation(HomologyAnnotation(
      name = "query_to_ref",
      reference_species = "ref",
      query_species = "query",
      homology_table = data.frame(
        query_gene = "rna-XM_053734541.1",
        reference_gene = "prp-6"
      )
    ))

  gene_tags <- syn_to_genetag_df(sp, species = "query", chr = "chrI", feature_type = "gene")

  expect_equal(nrow(gene_tags), 1L)
  expect_match(gene_tags$homology_query_aliases[[1L]], "rna-XM_053734541.1", fixed = TRUE)
  expect_true(gene_tags$homology_hit[[1L]])
  expect_identical(gene_tags$reference_gene[[1L]], "prp-6")
})

test_that("compile_ggtree_genetag rejects old x-layout modifiers", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = "XZ1516"
      )
    )
  tree <- ape::read.tree(text = "(XZ1516:0.1,other:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))

  expect_error(
    compile_ggtree_genetag(
      sp,
      tree_plot = tree_plot,
      chr = "RagTag_V",
      subset = c(21574445, 21584356),
      inter_genetic = "union"
    ),
    "Use `strip_scale_x\\(\\)`"
  )
})

test_that("ggtree genomic alignment keeps tree y and per-individual genomic panels", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  sp <- SynSpecies(name = "worms")
  for (id in c("XZ1516", "N2")) {
    sp <- add_individual(
      sp,
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = id
      )
    )
  }

  tree <- ape::read.tree(text = "(XZ1516:0.1,N2:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  species_tree_plot(sp) <- tree_plot
  alignment <- compile_ggtree_genomic_alignment(
    sp,
    chr = c(XZ1516 = "RagTag_V", N2 = "RagTag_V"),
    subset = list(
      XZ1516 = c(21574445, 21584356),
      N2 = c(21574445, 21584356)
    )
  )

  expect_s3_class(alignment, "ggtree_genomic_alignment")
  expect_equal(nrow(alignment$tip_layout), 2L)
  expect_setequal(alignment$tip_layout$individual, c("XZ1516", "N2"))
  expect_true(nrow(alignment$tree_segments) > 0L)
  expect_true(nrow(alignment$gene_tags) > 0L)
  expect_true(all(c("alignment_id", "alignment_panel", "y") %in% names(alignment$gene_tags)))

  aligned_plot <- plot_ggtree_genomic_alignment(alignment)
  expect_s3_class(aligned_plot, "ggtree_genomic_alignment_gtable")
  expect_true(inherits(aligned_plot, "gtable"))
})

test_that("additive genomic tree grammar renders gene tags and exon layers", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  sp <- SynSpecies(name = "worms")
  for (id in c("XZ1516", "N2")) {
    sp <- add_individual(
      sp,
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = id
      )
    )
  }

  tree <- ape::read.tree(text = "(XZ1516:0.1,N2:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  species_tree_plot(sp) <- tree_plot

  gene_tag_plot <- ggexon(sp) +
    geom_genetag(chr = "RagTag_V", subset = c(21574445, 21584356)) +
    geom_genomic_tree() +
    facet_genomictree(scales = "free_x")
  gene_tag_grob <- ggplot2::ggplotGrob(gene_tag_plot)
  expect_true(inherits(gene_tag_grob, "gtable"))
  expect_true(any(gene_tag_grob$layout$name == "genomic-tree"))

  exon_plot <- ggexon(sp) +
    geom_exon(chr = "RagTag_V", subset = c(21574445, 21584356)) +
    geom_genomic_tree(tree_plot = tree_plot) +
    facet_genomictree(scales = "free_x")
  exon_grob <- ggplot2::ggplotGrob(exon_plot)
  expect_true(inherits(exon_grob, "gtable"))
  expect_true(any(exon_grob$layout$name == "genomic-tree"))

  exon2_plot <- ggexon(sp) +
    geom_exon2(chr = "RagTag_V", subset = c(21574445, 21584356)) +
    geom_genomic_tree(tree_plot = tree_plot) +
    facet_genomictree(scales = "free_x")
  exon2_grob <- ggplot2::ggplotGrob(exon2_plot)
  expect_true(inherits(exon2_grob, "gtable"))
  expect_true(any(exon2_grob$layout$name == "genomic-tree"))
})

test_that("facet_genomictree orders ordinary track data by tree tips", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  tree <- ape::read.tree(text = "((sp_a:0.1,sp_b:0.1):0.1,sp_c:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  expected <- tree_plot$data[tree_plot$data$isTip %in% TRUE, c("label", "y")]
  expected <- expected[order(-expected$y), "label", drop = TRUE]

  track_data <- data.frame(
    track = c("sp_c", "sp_a", "sp_b"),
    xmin = c(1, 1, 1),
    xmax = c(2, 2, 2),
    y = 1,
    strand = "+",
    stringsAsFactors = FALSE
  )

  p <- ggexon(track_data) +
    geom_genetag() +
    geom_genomic_tree(tree_plot = tree_plot) +
    facet_genomictree(scales = "free_x")
  built <- ggplot2::ggplot_build(p)
  panel_layout <- as.data.frame(built@layout$layout)

  expect_equal(as.character(panel_layout$track), expected)
  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})
