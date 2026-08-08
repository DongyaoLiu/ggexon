test_that("strip_scale_x() returns the new spec and strip_scale() wraps it", {
  spec <- strip_scale_x(gene_gap_ratio = 3, align = "left")
  expect_s3_class(spec, "ggexon_strip_scale_x_spec")
  expect_identical(spec$gene_gap_ratio, 3)
  expect_identical(spec$species_specific_ratio, 0.5)
  expect_identical(spec$secondary_homology_ratio, 0.75)
  expect_identical(spec$gene_order, "genomic")
  expect_identical(spec$guide, "range")
  expect_false(spec$homo_active)

  ref_spec <- strip_scale_x(gene_gap_ratio = 3, reference_track = "ref")
  expect_true(ref_spec$homo_active)
  expect_identical(ref_spec$reference_track, "ref")
  expect_identical(ref_spec$homo_align, "ref")

  wrapped <- strip_scale(gene_gap_ratio = 2)
  expect_s3_class(wrapped, "ggexon_strip_scale_x_spec")
  expect_identical(wrapped$gene_gap_ratio, 2)

  no_guide <- strip_scale_x(guide = "none")
  expect_identical(no_guide$guide, "none")

  template <- strip_scale_x(slot_order = c("Hox3", "Hox2", "Hox1"))
  expect_true(template$template_active)
  expect_identical(template$slot_order, c("Hox3", "Hox2", "Hox1"))
})

test_that("strip_scale_x() validates homology and ratios", {
  expect_error(strip_scale_x(gene_gap_ratio = 0), "positive")
  expect_error(strip_scale_x(species_specific_ratio = 0), "in \\(0, 1\\]")
  expect_error(strip_scale_x(species_specific_ratio = 2), "in \\(0, 1\\]")
  expect_error(strip_scale_x(secondary_homology_ratio = 0), "in \\(0, 1\\]")
  expect_error(strip_scale_x(secondary_homology_ratio = 2), "in \\(0, 1\\]")
  expect_error(strip_scale_x(homo_align = TRUE), "explicit reference track")
  expect_error(strip_scale_x(homo_align = c("A", "B")), "single reference")
  expect_error(strip_scale_x(reference_track = c("A", "B")), "single non-empty")
  expect_error(strip_scale_x(reference_track = "ref", homo_align = "ref"), "only one")
  expect_error(strip_scale_x(slot_order = character()), "slot_order")
  expect_error(strip_scale_x(slot_order = c("A", "A")), "slot_order")
  expect_error(strip_scale_x(slot_order = c("A", NA_character_)), "slot_order")
  expect_error(strip_scale_x(slot_order = "A", reference_track = "ref"), "only one")
  expect_error(strip_scale_x(gene_order = "reference"), "requires")
  expect_error(strip_scale_x(guide = "ticks"), "should be one of")
})

test_that("strip_scale_x() maps gene-box anchors to exact synthetic slots", {
  genes <- data.frame(
    track = c("A", "A", "B", "B", "B"),
    species = factor(c("A", "A", "B", "B", "B"), levels = c("A", "B")),
    cluster = factor("cluster", levels = "cluster"),
    x = c(100, 900, 20, 40, 500),
    y = 1,
    strand = "+",
    gene_key = c("a15", "a1", "b13a", "b13b", "b1"),
    slot = c("Hox15", "Hox1", "Hox13", "Hox13", "Hox1"),
    stringsAsFactors = FALSE
  )
  slot_order <- c("Hox15", "Hox14", "Hox13", "Hox1")

  p <- ggexon() +
    geom_genebox(data = genes) +
    strip_scale_x(slot_order = slot_order, guide = "none") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(species),
      cols = ggplot2::vars(cluster),
      drop = FALSE
    )
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  plotted <- built@data[[1L]]

  expect_equal(transform$plot_anchor, match(transform$slot, slot_order))
  expect_equal(plotted$x, match(plotted$slot, slot_order))
  expect_equal(plotted$genomic_x, genes$x)
  expect_equal(unique(plotted$x[plotted$slot == "Hox13"]), 3)
  expect_false(any(transform$slot == "Hox14"))
  expect_equal(built@layout$strip_scale_x_limits, c(0.5, 4.5))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("exact template mode preserves raw genomic provenance across x transforms", {
  genes <- data.frame(
    track = "scaled",
    x = c(100, 1000),
    genomic_x = c(101, NA_real_),
    y = 1,
    strand = "+",
    gene_key = c("scaled_hox2", "scaled_hox1"),
    slot = c("Hox2", "Hox1"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genebox(data = genes) +
    strip_scale_x(slot_order = c("Hox2", "Hox1"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x") +
    ggplot2::scale_x_log10()
  built <- ggexon_build(p)

  expect_equal(built@data[[1L]]$x, c(1, 2))
  expect_equal(built@data[[1L]]$genomic_x, c(101, 1000))
  expect_equal(
    built@layout$strip_scale_x_transform$genomic_anchor,
    log10(genes$x)
  )
  expect_false(any(startsWith(
    names(built@data[[1L]]),
    .strip_scale_raw_marker_prefix
  )))
})

test_that("exact template mode passes slot through geom_genetag", {
  gene_tags <- data.frame(
    track = "tag_track",
    xmin = c(100, 300),
    xmax = c(200, 400),
    y = 1,
    strand = "+",
    gene_key = c("tag_hox2", "tag_hox1"),
    slot = c("Hox2", "Hox1"),
    stringsAsFactors = FALSE
  )
  tag_layer <- geom_genetag(data = gene_tags, show_label = FALSE)

  expect_true("slot" %in% names(tag_layer$mapping))
  expect_true("slot" %in% GeomGeneTag$syn_default_aes)

  p <- ggexon() +
    tag_layer +
    strip_scale_x(slot_order = c("Hox2", "Hox1"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  plotted <- built@data[[1L]]

  expect_equal(plotted$slot, gene_tags$slot)
  expect_equal((plotted$xmin + plotted$xmax) / 2, c(1, 2))
})

test_that("exact template direction warns once for underdetermined tracks", {
  genes <- data.frame(
    track = c("single", rep("zero_correlation", 3)),
    x = c(100, 10, 20, 30),
    y = 1,
    strand = "+",
    gene_key = c("single", "zero_a", "zero_b", "zero_c"),
    slot = c("Hox1", "Hox1", "Hox2", "Hox1"),
    stringsAsFactors = FALSE
  )
  p <- ggexon() +
    geom_genebox(data = genes) +
    strip_scale_x(slot_order = c("Hox1", "Hox2"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  warnings <- character()
  built <- withCallingHandlers(
    ggexon_build(p),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "could not infer template direction")
  expect_match(warnings, "fewer than two distinct anchors or slots")
  expect_match(warnings, "zero or undefined rank correlation")
  expect_equal(unique(built@data[[1L]]$strip_x_direction), 1)
})

test_that("exact template slots are independent of selected genomic anchor", {
  anchors <- data.frame(
    track = c("start", "middle", "end"),
    x = c(101, 250, 399),
    y = 1,
    strand = "+",
    gene_key = c("g_start", "g_middle", "g_end"),
    slot = "Hox1",
    anchor_mode = c("start", "middle", "end"),
    stringsAsFactors = FALSE
  )
  p <- ggexon() +
    geom_genebox(data = anchors) +
    strip_scale_x(slot_order = c("Hox2", "Hox1"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- NULL
  expect_warning(
    built <- ggexon_build(p),
    "could not infer template direction for 3 panel/track group"
  )

  expect_equal(built@data[[1L]]$x, rep(2, 3))
  expect_equal(built@data[[1L]]$genomic_x, anchors$x)

  reverse_track <- data.frame(
    track = "reverse",
    x = c(900, 100),
    y = 1,
    strand = "+",
    gene_key = c("reverse_hox2", "reverse_hox1"),
    slot = c("Hox2", "Hox1"),
    stringsAsFactors = FALSE
  )
  reverse_plot <- ggexon() +
    geom_genebox(data = reverse_track) +
    strip_scale_x(slot_order = c("Hox2", "Hox1"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  reverse_built <- ggexon_build(reverse_plot)
  expect_equal(reverse_built@data[[1L]]$strip_x_direction, c(-1, -1))

  bad <- anchors[1L, , drop = FALSE]
  bad$slot <- "Hox9"
  bad_plot <- ggexon() +
    geom_genebox(data = bad) +
    strip_scale_x(slot_order = c("Hox2", "Hox1"), guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_error(ggexon_build(bad_plot), "absent from `slot_order`")
})

test_that("strip_scale_x() requires geom_genetag()", {
  p <- ggexon() + ggplot2::geom_point(ggplot2::aes(1, 1)) + strip_scale_x()
  expect_error(ggexon_build(p), "geom_genetag")
})

test_that("strip_scale_x() applies level-1 equal gene and gap widths", {
  gene_tags <- data.frame(
    track = c("A", "A", "A", "B", "B"),
    xmin = c(10, 30, 70, 100, 150),
    xmax = c(20, 40, 90, 120, 170),
    y = 1,
    strand = "+",
    gene_key = c("a1", "a2", "a3", "b1", "b2"),
    label = c("a1", "a2", "a3", "b1", "b2"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, align = "left") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform

  genes <- transform[transform$region_type == "gene", , drop = FALSE]
  gaps <- transform[transform$region_type == "gap", , drop = FALSE]
  expect_true(all((genes$plot_end - genes$plot_start) == 3))
  expect_true(all((gaps$plot_end - gaps$plot_start) == 1))
  expect_identical(unique(as.integer(built@layout$layout$SCALE_X)), 1L)
})

test_that("strip_scale_x() keeps manual gene-tag rows separate inside collapsed runs", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "ref", "qry", "qry", "qry", "qry", "qry"),
    xmin = c(1, 10, 20, 1, 4, 6, 10, 20),
    xmax = c(2, 11, 21, 2, 5, 7, 11, 21),
    genomic_xmin = c(1, 10, 20, 1, 4, 6, 10, 20),
    genomic_xmax = c(2, 11, 21, 2, 5, 7, 11, 21),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "C", "A1", "x1", "x2", "B1", "C1"),
    label = c("A", "B", "C", "A1", "x1", "x2", "B1", "C1"),
    reference_gene = c(NA, NA, NA, "A", NA, NA, "B", "C"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(
      gene_gap_ratio = 3,
      homo_align = "ref",
      species_specific_ratio = 0.5,
      collapse_contiguous_slot = TRUE
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  data <- built@data[[1L]]

  run <- transform[transform$track == "qry" & transform$slot_type == "species_specific_run", , drop = FALSE]
  expect_equal(nrow(run), 1L)
  expect_equal(run$members, "x1,x2")
  expect_equal(run$plot_end - run$plot_start, 1.5)

  qry_rows <- data[data$track == "qry" & data$gene_key %in% c("x1", "x2"), , drop = FALSE]
  expect_equal(nrow(qry_rows), 2L)
  expect_true(all(qry_rows$xmin >= run$plot_start & qry_rows$xmax <= run$plot_end))
})

test_that("strip_scale_x() uses row identity for overlapping gene intervals", {
  gene_tags <- data.frame(
    track = c("ref", "ref"),
    xmin = c(1, 3),
    xmax = c(5, 10),
    genomic_xmin = c(1, 3),
    genomic_xmax = c(5, 10),
    y = 1,
    strand = "+",
    gene_key = c("left", "overlap"),
    label = c("left", "overlap"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  data <- built@data[[1L]]
  overlap <- data[data$gene_key == "overlap", , drop = FALSE]

  expect_equal(overlap$xmin, 4)
  expect_equal(overlap$xmax, 7)
  expect_true(all((data$xmax - data$xmin) == 3))
})

test_that("strip_scale_x() builds a per-track genomic range guide", {
  gene_tags <- data.frame(
    track = c("A", "A", "B", "B"),
    xmin = c(1000, 3000, 10000, 15000),
    xmax = c(2000, 5000, 12000, 17000),
    y = 1,
    strand = "+",
    gene_key = c("a1", "a2", "b1", "b2"),
    label = c("a1", "a2", "b1", "b2"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  axis_data <- built@layout$strip_scale_x_axis_data

  expect_equal(axis_data$track, c("A", "B"))
  expect_equal(axis_data$start_label, c("1,000", "10,000"))
  expect_equal(axis_data$end_label, c("5,000", "17,000"))
  expect_equal(axis_data$plot_start, c(0, 0))
  expect_equal(axis_data$plot_end, c(7, 7))

  table <- ggplot2::ggplotGrob(p)
  has_strip_axis <- vapply(
    table$grobs,
    function(x) inherits(x, "ggexonStripScaleXAxisGrob"),
    logical(1)
  )
  expect_true(any(has_strip_axis))
})

test_that("strip_scale_x() labels explicit panel-window endpoints", {
  gene_tags <- data.frame(
    track = c("A", "A", "B", "B"),
    xmin = c(1000, 3000, 10000, 15000),
    xmax = c(2000, 5000, 12000, 17000),
    y = 1,
    strand = "+",
    gene_key = c("a1", "a2", "b1", "b2"),
    label = c("a1", "a2", "b1", "b2"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3) +
    facet_genomics(
      ggplot2::vars(track),
      scales = "free_x",
      xlim = list(A = c(0, 10000), B = c(5000, 20000))
    )
  built <- ggexon_build(p)
  axis_data <- built@layout$strip_scale_x_axis_data

  expect_equal(axis_data$start_label, c("0", "5,000"))
  expect_equal(axis_data$end_label, c("10,000", "20,000"))
  expect_equal(axis_data$plot_start, c(0, 0))
  expect_equal(axis_data$plot_end, c(7, 7))
  expect_false(anyNA(built@data[[1L]]$xmin))
  expect_false(anyNA(built@data[[1L]]$xmax))
  expect_equal(
    lapply(built@layout$panel_params, function(panel) panel$x.range),
    list(c(-0.35, 7.35), c(-0.35, 7.35))
  )
})

test_that("strip_scale_x(guide = 'none') suppresses the custom range guide", {
  gene_tags <- data.frame(
    track = "A",
    xmin = c(1000, 3000),
    xmax = c(2000, 5000),
    y = 1,
    strand = "+",
    gene_key = c("a1", "a2"),
    label = c("a1", "a2"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, guide = "none") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  expect_equal(nrow(built@layout$strip_scale_x_axis_data), 0L)

  table <- ggplot2::ggplotGrob(p)
  has_strip_axis <- vapply(
    table$grobs,
    function(x) inherits(x, "ggexonStripScaleXAxisGrob"),
    logical(1)
  )
  expect_false(any(has_strip_axis))
})

test_that("strip_scale_x() translates by the most conserved block", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "ref", "qry", "qry", "qry", "qry", "qry"),
    xmin = c(1, 10, 20, 1, 4, 6, 10, 20),
    xmax = c(2, 11, 21, 2, 5, 7, 11, 21),
    genomic_xmin = c(1, 10, 20, 1, 4, 6, 10, 20),
    genomic_xmax = c(2, 11, 21, 2, 5, 7, 11, 21),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "C", "A1", "x1", "x2", "B1", "C1"),
    label = c("A", "B", "C", "A1", "x1", "x2", "B1", "C1"),
    reference_gene = c(NA, NA, NA, "A", NA, NA, "B", "C"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, homo_align = "ref") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  data <- built@data[[1L]]
  ref <- data[data$track == "ref", , drop = FALSE]
  qry <- data[data$track == "qry", , drop = FALSE]

  expect_equal(ref$xmin[ref$gene_key == "B"], qry$xmin[qry$gene_key == "B1"])
  expect_equal(ref$xmin[ref$gene_key == "C"], qry$xmin[qry$gene_key == "C1"])
  expect_lt(qry$xmin[qry$gene_key == "A1"], ref$xmin[ref$gene_key == "A"])
})

test_that("strip_scale_x() chooses globally conserved homology blocks without order matching", {
  gene_tags <- data.frame(
    track = c(
      rep("ref", 6),
      rep("reordered", 6),
      rep("support", 2)
    ),
    xmin = c(
      1, 10, 20, 30, 40, 50,
      1, 10, 20, 30, 40, 50,
      1, 10
    ),
    xmax = c(
      2, 11, 21, 31, 41, 51,
      2, 11, 21, 31, 41, 51,
      2, 11
    ),
    genomic_xmin = c(
      1, 10, 20, 30, 40, 50,
      1, 10, 20, 30, 40, 50,
      1, 10
    ),
    genomic_xmax = c(
      2, 11, 21, 31, 41, 51,
      2, 11, 21, 31, 41, 51,
      2, 11
    ),
    y = 1,
    strand = "+",
    gene_key = c(
      "A", "B", "C", "D", "E", "F",
      "A1", "B1", "E1", "F1", "C1", "D1",
      "C2", "D2"
    ),
    label = c(
      "A", "B", "C", "D", "E", "F",
      "A1", "B1", "E1", "F1", "C1", "D1",
      "C2", "D2"
    ),
    reference_gene = c(
      rep(NA_character_, 6),
      "A", "B", "E", "F", "C", "D",
      "C", "D"
    ),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, homo_align = "ref") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  data <- built@data[[1L]]
  ref <- data[data$track == "ref", , drop = FALSE]
  reordered <- data[data$track == "reordered", , drop = FALSE]

  expect_equal(
    built@layout$strip_scale_x_conserved_block,
    c("C", "D")
  )
  expect_equal(ref$xmin[ref$gene_key == "C"], reordered$xmin[reordered$gene_key == "C1"])
  expect_equal(ref$xmin[ref$gene_key == "D"], reordered$xmin[reordered$gene_key == "D1"])
  expect_false(isTRUE(all.equal(ref$xmin[ref$gene_key == "A"], reordered$xmin[reordered$gene_key == "A1"])))
})

test_that("strip_scale_x() treats duplicate reference mappings as secondary homologs", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "qry", "qry", "qry"),
    xmin = c(1, 10, 1, 4, 10),
    xmax = c(2, 11, 2, 5, 11),
    genomic_xmin = c(1, 10, 1, 4, 10),
    genomic_xmax = c(2, 11, 2, 5, 11),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "A1", "A2", "B1"),
    label = c("A", "B", "A1", "A2", "B1"),
    reference_gene = c(NA, NA, "A", "A", "B"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, homo_align = "ref") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  qry_genes <- transform[transform$track == "qry" & transform$region_type == "gene", , drop = FALSE]

  expect_equal(
    qry_genes$visual_class,
    c("homologous_visible_primary", "homologous_visible_duplicate", "homologous_visible_primary")
  )
  expect_equal(qry_genes$homology_anchor, c(TRUE, FALSE, TRUE))
  expect_equal(qry_genes$slot_type, qry_genes$visual_class)
  expect_equal(qry_genes$members[[2L]], "A2")
  expect_equal(qry_genes$plot_end[[2L]] - qry_genes$plot_start[[2L]], 2.25)
})

test_that("strip_scale_x(gene_order = 'reference') orders query genes by reference", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "ref", "qry", "qry", "qry", "qry"),
    xmin = c(1, 10, 20, 1, 4, 10, 20),
    xmax = c(2, 11, 21, 2, 5, 11, 21),
    genomic_xmin = c(1, 10, 20, 1, 4, 10, 20),
    genomic_xmax = c(2, 11, 21, 2, 5, 11, 21),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "C", "C1", "x", "B1", "A1"),
    label = c("A", "B", "C", "C1", "x", "B1", "A1"),
    reference_gene = c(NA, NA, NA, "C", NA, "B", "A"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, reference_track = "ref", gene_order = "reference") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  qry_genes <- transform[transform$track == "qry" & transform$region_type == "gene", , drop = FALSE]

  expect_equal(qry_genes$members, c("A1", "B1", "x", "C1"))
  expect_equal(qry_genes$slot_type, c(
    "homologous_visible_primary",
    "homologous_visible_primary",
    "species_specific_run",
    "homologous_visible_primary"
  ))
})

test_that("strip_scale_x(gene_order = 'reference') groups duplicate homologs", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "qry", "qry", "qry", "qry"),
    xmin = c(1, 10, 1, 4, 10, 20),
    xmax = c(2, 11, 2, 5, 11, 21),
    genomic_xmin = c(1, 10, 1, 4, 10, 20),
    genomic_xmax = c(2, 11, 2, 5, 11, 21),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "A1", "x", "A2", "B1"),
    label = c("A", "B", "A1", "x", "A2", "B1"),
    reference_gene = c(NA, NA, "A", NA, "A", "B"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, reference_track = "ref", gene_order = "reference") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  qry_genes <- transform[transform$track == "qry" & transform$region_type == "gene", , drop = FALSE]

  expect_equal(qry_genes$members, c("A1", "A2", "x", "B1"))
  expect_equal(qry_genes$visual_class, c(
    "homologous_visible_primary",
    "homologous_visible_duplicate",
    "species_specific",
    "homologous_visible_primary"
  ))
})

test_that("strip_scale_x() accepts character homology_hit for offtrack homologs", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "qry", "qry", "qry"),
    xmin = c(1, 10, 1, 4, 10),
    xmax = c(2, 11, 2, 5, 11),
    genomic_xmin = c(1, 10, 1, 4, 10),
    genomic_xmax = c(2, 11, 2, 5, 11),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "s1", "off", "s2"),
    label = c("A", "B", "s1", "off", "s2"),
    reference_gene = c(NA, NA, NA, "Z", NA),
    homology_hit = c("FALSE", "FALSE", "FALSE", "TRUE", "FALSE"),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, homo_align = "ref") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  qry_genes <- transform[transform$track == "qry" & transform$region_type == "gene", , drop = FALSE]
  data <- built@data[[1L]]
  qry_data <- data[data$track == "qry", , drop = FALSE]

  expect_equal(
    qry_genes$slot_type,
    c("species_specific_run", "homologous_offtrack", "species_specific_run")
  )
  expect_equal(qry_genes$visual_class, c("species_specific", "homologous_offtrack", "species_specific"))
  expect_equal(qry_genes$reference_gene[[2L]], "Z")
  expect_equal(qry_genes$plot_end[[2L]] - qry_genes$plot_start[[2L]], 2.25)
  expect_equal(qry_data$visual_class, c("species_specific", "homologous_offtrack", "species_specific"))
  expect_equal(qry_data$slot_type, c("species_specific_run", "homologous_offtrack", "species_specific_run"))
  expect_equal(qry_data$homology_anchor, c(FALSE, FALSE, FALSE))
})

test_that("strip_scale_x() uses visible row identity when homology hit is offtrack", {
  gene_tags <- data.frame(
    track = c("ref", "ref", "qry", "qry"),
    xmin = c(1, 10, 1, 10),
    xmax = c(2, 11, 2, 11),
    genomic_xmin = c(1, 10, 1, 10),
    genomic_xmax = c(2, 11, 2, 11),
    y = 1,
    strand = "+",
    gene_key = c("A", "B", "A_q", "B_q"),
    gene = c("A", "B", "A", "B"),
    label = c("A", "B", "A", "B"),
    reference_gene = c(NA, NA, "offtrack", "B"),
    homology_hit = c(FALSE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  p <- ggexon() +
    geom_genetag(data = gene_tags) +
    strip_scale_x(gene_gap_ratio = 3, homo_align = "ref") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  built <- ggexon_build(p)
  transform <- built@layout$strip_scale_x_transform
  qry_genes <- transform[transform$track == "qry" & transform$region_type == "gene", , drop = FALSE]

  expect_equal(qry_genes$visual_class, c("homologous_visible_primary", "homologous_visible_primary"))
  expect_equal(qry_genes$reference_gene, c("A", "B"))
  expect_equal(qry_genes$homology_anchor, c(TRUE, TRUE))
})

test_that("strip_scale_to_plot_x maps unordered intervals and inter-interval values", {
  transform <- data.frame(
    genomic_start = c(100, 10, 30),
    genomic_end = c(110, 20, 35),
    plot_start = c(20, 0, 60),
    plot_end = c(23, 3, 63),
    slope = c(0.3, 0.3, 0.6),
    region_type = "gene",
    stringsAsFactors = FALSE
  )

  expect_equal(strip_scale_to_plot_x(32, transform), 61.2)
  expect_equal(strip_scale_to_plot_x(12, transform), 0.6)
})
