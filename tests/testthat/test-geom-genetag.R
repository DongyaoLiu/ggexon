test_that("geom_genetag polygon data uses constant-height strand arrows", {
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

  poly <- .genetag_polygon_data(data, height = 0.5, arrow_fraction = 0.2)

  expect_identical(nrow(poly), 10L)

  plus <- poly[poly$group == 1L, , drop = FALSE]
  minus <- poly[poly$group == 2L, , drop = FALSE]

  expect_equal(range(plus$y), c(0.75, 1.25))
  expect_true(any(plus$x == 10 & plus$y == 1))
  expect_equal(range(minus$y), c(1.75, 2.25))
  expect_true(any(minus$x == 10 & minus$y == 2))
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
  gene_tags <- compile_ggtree_genetag(
    sp,
    tree_plot = tree_plot,
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )

  expect_true(nrow(gene_tags) > 0L)
  expect_false("y" %in% names(gene_tags))
  expect_true(all(c("id", "tree_y", "xmin", "xmax", "strand") %in% names(gene_tags)))
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
