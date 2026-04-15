test_that("link_panels attaches a cross-panel annotation spec to ggexon plots", {
  links <- data.frame(
    id = c("a", "a", "b", "b"),
    panel = c("4", "6", "4", "8"),
    x = c(2, 3, 4, 5),
    y = c(20, 21, 18, 19)
  )

  p <- ggexon(mtcars, aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ cyl) +
    link_panels(
      data = links,
      id = "id",
      panel = "panel",
      x = "x",
      y = "y",
      colour = "red"
    )

  expect_length(p@cross_panel_annotations, 1L)
  expect_s3_class(p@cross_panel_annotations[[1]], "cross_panel_annotation")
  expect_identical(p@cross_panel_annotations[[1]]$mode, "shared_id")
})

test_that("cross-panel specs are carried through gtable rendering", {
  links <- data.frame(
    id = c("a", "a"),
    panel = c("4", "6"),
    x = c(2, 3),
    y = c(20, 21)
  )

  p <- ggexon(mtcars, aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ cyl) +
    link_panels(
      data = links,
      id = "id",
      panel = "panel",
      x = "x",
      y = "y"
    )

  build <- ggexon:::ggexon_build(p)
  table <- ggexon:::ggexon_gtable(build)
  specs <- attr(table, "cross_panel_annotations", exact = TRUE)

  expect_type(specs, "list")
  expect_length(specs, 1L)
  expect_true(all(c("annotation", "anchors", "panel_info") %in% names(specs[[1L]])))
})

test_that("annotate_cross_panel requires panel, x, and y mappings", {
  expect_error(
    annotate_cross_panel(
      data = data.frame(x1 = 1, y1 = 2, x2 = 3, y2 = 4),
      from = ggplot2::aes(x = x1, y = y1),
      to = ggplot2::aes(panel = "b", x = x2, y = y2)
    ),
    "must include mappings"
  )
})
