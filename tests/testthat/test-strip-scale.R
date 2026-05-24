test_that("strip_scale_x() returns the new spec and strip_scale() wraps it", {
  spec <- strip_scale_x(gene_gap_ratio = 3, align = "left")
  expect_s3_class(spec, "ggexon_strip_scale_x_spec")
  expect_identical(spec$gene_gap_ratio, 3)
  expect_identical(spec$species_specific_ratio, 0.5)
  expect_identical(spec$secondary_homology_ratio, 0.75)
  expect_false(spec$homo_active)

  wrapped <- strip_scale(gene_gap_ratio = 2)
  expect_s3_class(wrapped, "ggexon_strip_scale_x_spec")
  expect_identical(wrapped$gene_gap_ratio, 2)
})

test_that("strip_scale_x() validates homology and ratios", {
  expect_error(strip_scale_x(gene_gap_ratio = 0), "positive")
  expect_error(strip_scale_x(species_specific_ratio = 0), "in \\(0, 1\\]")
  expect_error(strip_scale_x(species_specific_ratio = 2), "in \\(0, 1\\]")
  expect_error(strip_scale_x(secondary_homology_ratio = 0), "in \\(0, 1\\]")
  expect_error(strip_scale_x(secondary_homology_ratio = 2), "in \\(0, 1\\]")
  expect_error(strip_scale_x(homo_align = TRUE), "explicit reference track")
  expect_error(strip_scale_x(homo_align = c("A", "B")), "single reference")
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

test_that("strip_scale_x() keeps offtrack homologs out of species-specific collapse", {
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
    homology_hit = c(FALSE, FALSE, FALSE, TRUE, FALSE),
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
