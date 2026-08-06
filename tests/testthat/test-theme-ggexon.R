expect_blank_ggexon_backgrounds <- function(th) {
  background_elements <- c(
    "plot.background",
    "panel.background",
    "panel.border",
    "strip.background",
    "legend.background",
    "legend.key"
  )
  for (element in background_elements) {
    expect_s3_class(ggplot2::calc_element(element, th), "element_blank")
  }
}

test_that("theme_ggexon_base removes backgrounds but retains labels and x grids", {
  th <- theme_ggexon_base()

  expect_s3_class(th, "theme")
  expect_blank_ggexon_backgrounds(th)
  expect_s3_class(
    ggplot2::calc_element("strip.text.y.left", th),
    "element_text"
  )
  expect_s3_class(
    ggplot2::calc_element("panel.grid.major.x", th),
    "element_line"
  )
})

test_that("theme_ggexon_track keeps genomic x axes visible by default", {
  th <- theme_ggexon_track()

  expect_s3_class(th, "theme")
  expect_s3_class(th$axis.text.x, "element_text")
  expect_s3_class(th$axis.ticks.x, "element_line")
  expect_s3_class(th$axis.line.x, "element_line")
  expect_s3_class(th$axis.text.y, "element_blank")
  expect_blank_ggexon_backgrounds(th)
})

test_that("derived ggexon themes inherit the blank-background contract", {
  expect_blank_ggexon_backgrounds(theme_ggexon_genomictree())
  expect_blank_ggexon_backgrounds(theme_ggexon_side_strips("left"))
  expect_blank_ggexon_backgrounds(theme_ggexon_pairwise())
})

test_that("theme_ggexon_genomictree renders aligned panels with themed labels", {
  testthat::skip_if_not_installed("ape")
  testthat::skip_if_not_installed("ggtree")

  tree <- ape::read.tree(text = "(sp_a:0.1,sp_b:0.2);")
  tree_plot <- suppressWarnings(ggtree::ggtree(tree, layout = "rectangular"))
  tracks <- data.frame(
    track = c("sp_b", "sp_a"),
    xmin = c(1, 4),
    xmax = c(3, 8),
    y = 1,
    strand = c("+", "-"),
    gene = c("gene_b", "gene_a"),
    stringsAsFactors = FALSE
  )

  p <- ggexon(tracks) +
    geom_genetag(ggplot2::aes(fill = gene)) +
    geom_genomic_tree(tree_plot = tree_plot) +
    facet_genomictree(scales = "free_x") +
    theme_ggexon_genomictree()

  grob <- ggplot2::ggplotGrob(p)
  expect_true(inherits(grob, "gtable"))
  expect_true(any(grob$layout$name == "genomic-tree"))
  expect_true(any(grepl("^genomic-tree-label-", grob$layout$name)))

  blank_label_grob <- ggplot2::ggplotGrob(
    p + ggplot2::theme(strip.text.y = ggplot2::element_blank())
  )
  expect_false(any(grepl("^genomic-tree-label-", blank_label_grob$layout$name)))
})

test_that("theme_ggexon_side_strips styles side strips for horizontal reading", {
  th_left <- theme_ggexon_side_strips("left")
  expect_s3_class(th_left, "theme")
  expect_identical(th_left$strip.placement, "outside")
  expect_s3_class(th_left$strip.text.y.left, "element_text")
  expect_equal(th_left$strip.text.y.left$angle, 0)
  expect_equal(th_left$strip.text.y.left$hjust, 1)
  expect_s3_class(th_left$strip.background, "element_blank")

  th_right <- theme_ggexon_side_strips("right")
  expect_s3_class(th_right$strip.text.y.right, "element_text")
  expect_equal(th_right$strip.text.y.right$angle, 0)
  expect_equal(th_right$strip.text.y.right$hjust, 0)

  expect_s3_class(
    ggplot2::calc_element(
      "strip.background",
      theme_ggexon_side_strips("left", background = "grey96")
    ),
    "element_rect"
  )
  expect_error(theme_ggexon_side_strips("bad"), "side")
})

test_that("theme_ggexon_pairwise uses left labels and compact genomic axes", {
  th <- theme_ggexon_pairwise()

  expect_s3_class(th, "theme")
  expect_s3_class(th$axis.text.x, "element_text")
  expect_s3_class(th$axis.text.y, "element_blank")
  expect_s3_class(th$axis.ticks.y, "element_blank")
  expect_s3_class(th$axis.title, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_line")
  expect_identical(th$strip.placement, "outside")
  expect_s3_class(th$strip.text.y.left, "element_text")
  expect_equal(th$strip.text.y.left$angle, 0)
  expect_equal(th$strip.text.y.left$hjust, 1)
  expect_identical(th$legend.position, "none")
})

test_that("theme_ggexon_pairwise exposes axis, grid, and legend switches", {
  th <- theme_ggexon_pairwise(
    show_x_axis = FALSE,
    show_x_grid = FALSE,
    show_legend = TRUE
  )

  expect_s3_class(th$axis.text.x, "element_blank")
  expect_s3_class(th$axis.ticks.x, "element_blank")
  expect_s3_class(th$axis.line.x, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")
  expect_identical(th$legend.position, "right")
})
