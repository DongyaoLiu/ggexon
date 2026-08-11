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

test_that("geom_genetag height alias controls the rendered body", {
  plot <- ggplot2::ggplot() +
    geom_genetag(
      data = data.frame(xmin = 0, xmax = 10, y = 0.4, strand = "+"),
      height = 0.4,
      show_label = FALSE
    )
  built <- ggplot2::ggplot_build(plot)
  panel_grob <- built$plot$layers[[1L]]$draw_geom(
    built$data[[1L]],
    built$layout
  )[[1L]]
  body_grobs <- Filter(
    function(x) inherits(x, "polygon"),
    as.list(panel_grob$children)
  )
  expect_length(body_grobs, 1L)

  rendered_y <- range(as.numeric(body_grobs[[1L]]$y))
  expected_y <- range(built$plot$coordinates$transform(
    data.frame(x = 5, y = c(0.2, 0.6)),
    built$layout$panel_params[[1L]]
  )$y)
  expect_equal(rendered_y, expected_y)
})

test_that("geom_genetag can fix terminal arrow aesthetics independently", {
  data <- data.frame(
    xmin = c(0, 10),
    xmax = c(10, 20),
    y = c(1, 2),
    strand = c("+", "-"),
    gene = c("g1", "g2"),
    PANEL = 1L,
    group = 1:2,
    colour = "black",
    fill = c("red", "blue"),
    linewidth = 0.25,
    linetype = 1,
    alpha = NA_real_,
    stringsAsFactors = FALSE
  )

  arrow <- .genetag_arrow_polygon_data(data, exon_height = 0.5, arrow_fraction = 0.2)
  fixed_arrow <- ggexon:::.apply_transcript_backbone_aes(
    arrow,
    fill = "grey82",
    colour = NA
  )

  expect_identical(nrow(arrow), 6L)
  expect_equal(fixed_arrow$fill, rep("grey82", nrow(fixed_arrow)))
  expect_true(all(is.na(fixed_arrow$colour)))
  expect_true(inherits(ggplot2::ggplotGrob(
    ggplot2::ggplot() +
      geom_genetag(
        data = data,
        ggplot2::aes(fill = gene),
        tag_arrow_fill = "grey82",
        tag_arrow_colour = NA,
        show_label = FALSE
      )
  ), "gtable"))
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

test_that("geom_genetag can lane overlapping and nested gene bodies", {
  data <- data.frame(
    xmin = c(0, 30, 110, 0, 20),
    xmax = c(100, 60, 140, 100, 50),
    y = 0.4,
    strand = "+",
    label = c("parent", "child", "after", "track_b_parent", "track_b_child"),
    gene = c("parent", "child", "after", "track_b_parent", "track_b_child"),
    track = c("track_a", "track_a", "track_a", "track_b", "track_b"),
    stringsAsFactors = FALSE
  )

  single <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_genetag(data = data, exon_height = 0.5, gene_layout = "single", show_label = FALSE)
  )$data[[1L]]
  nested <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_genetag(data = data, exon_height = 0.5, gene_layout = "nested", gene_lane_gap = 0.2, show_label = FALSE)
  )$data[[1L]]
  stack <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_genetag(data = data, exon_height = 0.5, gene_layout = "stack", gene_lane_gap = 0.2, show_label = FALSE)
  )$data[[1L]]

  expect_equal(unique(single$gene_lane), 1L)
  expect_equal(nested$gene_lane[nested$gene == "parent"], 1L)
  expect_equal(nested$gene_lane[nested$gene == "child"], 2L)
  expect_equal(nested$gene_lane[nested$gene == "after"], 1L)
  expect_equal(nested$gene_lane[nested$gene == "track_b_parent"], 1L)
  expect_equal(nested$gene_lane[nested$gene == "track_b_child"], 2L)
  expect_gt(nested$y[nested$gene == "child"], nested$y[nested$gene == "parent"])
  expect_equal(unique(nested$gene_lane_count[nested$track == "track_a"]), 2L)
  expect_true(inherits(ggplot2::ggplotGrob(
    ggplot2::ggplot() + geom_genetag(data = data, gene_layout = "nested")
  ), "gtable"))
  expect_error(
    ggplot2::ggplot_build(ggplot2::ggplot() + geom_genetag(data = data, gene_lane_gap = -0.1)),
    "gene_lane_gap"
  )
  expect_error(
    ggplot2::ggplot_build(ggplot2::ggplot() + geom_genetag(data = data, gene_layout = "bad")),
    "gene_layout"
  )
  expect_equal(stack$gene_lane[stack$gene == "child"], 2L)
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

test_that("geom_genetag outside labels use deterministic lanes", {
  data <- data.frame(
    xmin = c(0, 1, 2),
    xmax = c(0.5, 1.5, 2.5),
    y = 0.4,
    strand = "+",
    label = c("long_gene_a", "long_gene_b", "long_gene_c"),
    track = "track_a",
    stringsAsFactors = FALSE
  )

  layout <- .genetag_label_layout(
    data,
    label_position = "outside",
    exon_height = 0.8,
    label_max_lanes = 3,
    panel_width_mm = 20
  )

  expect_equal(layout$outside$label_lane, c(1L, 2L, 3L))
  expect_equal(length(unique(layout$outside$y)), 3L)
  expect_false(isTRUE(attr(layout, "unresolved_collision", exact = TRUE)))

  single_lane <- .genetag_label_layout(
    data,
    label_position = "outside",
    exon_height = 0.8,
    label_max_lanes = 1,
    panel_width_mm = 20
  )
  expect_true(isTRUE(attr(single_lane, "unresolved_collision", exact = TRUE)))
  expect_error(
    .genetag_label_layout(data, label_position = "outside", label_panel_width = -1),
    "label_panel_width"
  )
  expect_error(
    .genetag_label_layout(data, label_position = "outside", label_max_lanes = 0),
    "label_max_lanes"
  )
})

test_that("ggexon flattens gene-tag body lanes after strip scale", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "qry", "qry"),
    xmin = c(0, 50, 0, 10),
    xmax = c(40, 60, 40, 20),
    y = 1,
    strand = "+",
    gene_key = c("ref_a", "ref_b", "qry_parent", "qry_child"),
    label = c("ref_a", "ref_b", "qry_parent", "qry_child"),
    reference_gene = c("ref_a", "ref_b", "ref_a", "ref_b"),
    homology_hit = TRUE,
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(
      data = gene_tags,
      exon_height = 0.5,
      gene_layout = "nested",
      gene_lane_gap = 0.2,
      label_position = "outside",
      label_direction = "top:bottom",
      label_panel_width = 100,
      label_max_lanes = 2
    ) +
    strip_scale_x(reference_track = "ref", gene_order = "reference", guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  data <- built@data[[1L]]
  query <- data[as.character(data$track) == "qry", , drop = FALSE]
  parent <- query[query$gene_key == "qry_parent", , drop = FALSE]
  child <- query[query$gene_key == "qry_child", , drop = FALSE]

  expect_true(all(c(
    "genetag_label_x", "genetag_label_y", "genetag_label_gene_ymax",
    "genetag_label_precomputed"
  ) %in% names(data)))
  expect_true(isTRUE(parent$genetag_label_precomputed[[1L]]))
  expect_equal(parent$gene_lane, 1L)
  expect_equal(child$gene_lane, 1L)
  expect_equal(parent$y, child$y)
  expect_equal(parent$genetag_label_gene_ymax, child$genetag_label_gene_ymax)
  expect_equal(parent$genetag_label_pos, "top")
  expect_equal(child$genetag_label_pos, "bottom")
  expect_equal(parent$genetag_label_y, parent$genetag_label_gene_ymax + 0.15)
  expect_equal(child$genetag_label_y, child$genetag_label_gene_ymin - 0.15)
  expect_true(!is.null(built@layout$genetag_label_layouts[[1L]]))
  expect_true(inherits(ggplot2::ggplot_gtable(built), "gtable"))
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

test_that("geom_genetag can select partial labels by gene identifiers", {
  data <- data.frame(
    xmin = c(0, 10, 20, 30),
    xmax = c(6, 16, 26, 36),
    y = 0.4,
    strand = "+",
    label = c("rpl-8", "calf-1", "dhcr-7", "zina-1"),
    gene = c("rpl-8", "calf-1", "dhcr-7", "zina-1"),
    gene_id = c("B0250.1", "B0250.2", "B0250.3", "B0250.4"),
    track = c("N2", "N2", "XZ1516", "XZ1516"),
    individual = c("N2", "N2", "XZ1516", "XZ1516"),
    reference_gene = c("rpl-8", "calf-1", NA, "B0250.4"),
    homology_hit = c(TRUE, TRUE, FALSE, TRUE),
    homology_anchor = c(TRUE, FALSE, FALSE, NA),
    visual_class = c(
      "homologous_anchor",
      "homologous_offtrack",
      "species_specific",
      "homologous_offtrack"
    ),
    stringsAsFactors = FALSE
  )

  exact <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_genes = "B0250.4",
    label_match_by = "gene_id",
    panel_width_mm = 80
  )
  expect_equal(exact$outside$label, "zina-1")

  regex <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_genes = "^(rpl|calf)",
    label_match = "regex",
    panel_width_mm = 80
  )
  expect_equal(regex$outside$label, c("rpl-8", "calf-1"))

  per_track <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_genes = list(N2 = "rpl-8", XZ1516 = "B0250.4"),
    panel_width_mm = 80
  )
  expect_equal(per_track$outside$label, c("rpl-8", "zina-1"))

  expect_silent(
    panel_track <- .genetag_label_layout(
      data[data$track == "N2", , drop = FALSE],
      label_position = "outside",
      label_genes = list(N2 = "rpl-8", XZ1516 = "B0250.4"),
      panel_width_mm = 80
    )
  )
  expect_equal(panel_track$outside$label, "rpl-8")

  expect_silent({
    p <- ggplot2::ggplot(data) +
      geom_genetag(label_genes = "B0250.4", label_match_by = "gene_id")
    built <- ggplot2::ggplot_build(p)
  })
  expect_true("homology_anchor" %in% names(built$data[[1]]))
})

test_that("geom_genetag semantic label filters use homology metadata", {
  data <- data.frame(
    xmin = c(0, 10, 20, 30),
    xmax = c(6, 16, 26, 36),
    y = 0.4,
    strand = "+",
    label = c("rpl-8", "calf-1", "dhcr-7", "zina-1"),
    gene_id = c("B0250.1", "B0250.2", "B0250.3", "B0250.4"),
    track = c("N2", "N2", "XZ1516", "XZ1516"),
    homology_hit = c(TRUE, TRUE, FALSE, TRUE),
    homology_anchor = c(TRUE, FALSE, FALSE, NA),
    visual_class = c(
      "homologous_anchor",
      "homologous_offtrack",
      "species_specific",
      "homologous_offtrack"
    ),
    stringsAsFactors = FALSE
  )

  hit <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "homology_hit",
    panel_width_mm = 80
  )
  expect_equal(hit$outside$label, c("rpl-8", "calf-1", "zina-1"))

  species_specific <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "species_specific",
    panel_width_mm = 80
  )
  expect_equal(species_specific$outside$label, "dhcr-7")

  strip_filtered <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = c("homology_anchor", "homology_offtrack"),
    panel_width_mm = 80
  )
  expect_equal(strip_filtered$outside$label, c("rpl-8", "calf-1", "zina-1"))

  offtrack_fallback <- .genetag_label_layout(
    data[setdiff(names(data), "visual_class")],
    label_position = "outside",
    label_filter = "homology_offtrack",
    panel_width_mm = 80
  )
  expect_equal(offtrack_fallback$outside$label, c("calf-1", "zina-1"))
})

test_that("geom_genetag homology label filters include reference hits by default", {
  data <- data.frame(
    xmin = c(0, 10, 20, 30),
    xmax = c(6, 16, 26, 36),
    y = 0.4,
    strand = "+",
    label = c("query-hit", "reference-hit", "reference-other", "specific"),
    gene_id = c("q1", "r1", "r3", "s1"),
    track = c("query", "ref", "ref", "other"),
    homology_hit = c(TRUE, FALSE, FALSE, FALSE),
    homology_query_hit = c(TRUE, FALSE, FALSE, FALSE),
    homology_reference_hit = c(FALSE, TRUE, FALSE, FALSE),
    is_homology_reference_track = c(FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  hit <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "homology_hit",
    panel_width_mm = 80
  )
  expect_equal(hit$outside$label, c("query-hit", "reference-hit"))

  query <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "homology_query_hit",
    panel_width_mm = 80
  )
  expect_equal(query$outside$label, "query-hit")

  reference <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "homology_reference_hit",
    panel_width_mm = 80
  )
  expect_equal(reference$outside$label, "reference-hit")

  specific <- .genetag_label_layout(
    data,
    label_position = "outside",
    label_filter = "species_specific",
    panel_width_mm = 80
  )
  expect_equal(specific$outside$label, "specific")
})

test_that("geom_genetag warns for unmatched label selectors and missing strip metadata", {
  data <- data.frame(
    xmin = c(0, 10),
    xmax = c(6, 16),
    y = 0.4,
    strand = "+",
    label = c("rpl-8", "calf-1"),
    gene_id = c("B0250.1", "B0250.2"),
    track = "N2",
    homology_hit = TRUE,
    stringsAsFactors = FALSE
  )

  expect_warning(
    selected <- .genetag_label_layout(
      data,
      label_position = "outside",
      label_genes = c("B0250.1", "missing_gene"),
      panel_width_mm = 80
    ),
    "missing_gene"
  )
  expect_equal(selected$outside$label, "rpl-8")

  expect_warning(
    no_strip <- .genetag_label_layout(
      data,
      label_position = "outside",
      label_filter = "homology_visible",
      panel_width_mm = 80
    ),
    "strip_scale_x"
  )
  expect_equal(nrow(no_strip$inside), 0L)
  expect_equal(nrow(no_strip$outside), 0L)
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

  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
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

  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
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

  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
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

  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
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

test_that("facet_genomictree keeps free-y semantics with unused panel specs", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  species <- SynSpecies(name = "tree panel scales")
  for (id in c("XZ1516", "N2")) {
    species <- add_individual(
      species,
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = id
      )
    )
  }

  tree <- ape::read.tree(text = "(XZ1516:0.1,N2:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  base_plot <- ggexon(species) +
    geom_exon(chr = "RagTag_V", subset = c(21574445, 21584356)) +
    geom_genomic_tree(tree_plot = tree_plot)

  without_spec <- ggexon_build(
    base_plot + facet_genomictree(scales = "free_y")
  )
  with_unused_spec <- ggexon_build(
    base_plot +
      facet_genomictree(scales = "free_y") +
      scale_panel_coverage("free_y")
  )
  with_center_wrapper <- ggexon_build(
    base_plot +
      facet_genomictree(scales = "free_y") +
      center_panel_annotation()
  )

  expect_identical(
    as.integer(without_spec@layout$layout$SCALE_Y),
    c(1L, 2L)
  )
  expect_identical(
    as.integer(with_unused_spec@layout$layout$SCALE_Y),
    c(1L, 2L)
  )
  expect_true(without_spec@layout$facet_params$free$y)
  expect_true(without_spec@layout$facet_params$draw_axes$y)
  expect_true(without_spec@layout$facet_params$axis_labels$y)
  expect_identical(
    with_unused_spec@layout$facet_params[c(
      "free", "draw_axes", "axis_labels"
    )],
    without_spec@layout$facet_params[c(
      "free", "draw_axes", "axis_labels"
    )]
  )
  expect_length(with_unused_spec@data, length(without_spec@data))
  for (layer_index in seq_along(without_spec@data)) {
    expect_identical(
      with_unused_spec@data[[layer_index]],
      without_spec@data[[layer_index]]
    )
    expect_identical(
      with_center_wrapper@data[[layer_index]],
      without_spec@data[[layer_index]]
    )
  }
  expect_identical(
    with_center_wrapper@layout$layout,
    without_spec@layout$layout
  )
  expect_identical(
    lapply(with_center_wrapper@layout$panel_params, `[[`, "y.range"),
    lapply(without_spec@layout$panel_params, `[[`, "y.range")
  )
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
