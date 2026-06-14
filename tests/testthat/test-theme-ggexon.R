test_that("theme_ggexon_track keeps genomic x axes visible by default", {
  th <- theme_ggexon_track()

  expect_s3_class(th, "theme")
  expect_s3_class(th$axis.text.x, "element_text")
  expect_s3_class(th$axis.ticks.x, "element_line")
  expect_s3_class(th$axis.line.x, "element_line")
  expect_s3_class(th$axis.text.y, "element_blank")
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
  expect_s3_class(th_left$strip.background, "element_rect")

  th_right <- theme_ggexon_side_strips("right")
  expect_s3_class(th_right$strip.text.y.right, "element_text")
  expect_equal(th_right$strip.text.y.right$angle, 0)
  expect_equal(th_right$strip.text.y.right$hjust, 0)

  expect_s3_class(theme_ggexon_side_strips("left", background = NA)$strip.background, "element_blank")
  expect_error(theme_ggexon_side_strips("bad"), "side")
})
