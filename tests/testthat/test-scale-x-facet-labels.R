test_that("scale_x_facet_labels returns a facet label specification", {
  labels <- list(
    A = c("A2", "A1"),
    B = c("B2", "B1")
  )

  spec <- scale_x_facet_labels(
    facet = cluster,
    labels = labels,
    breaks = c(1, 2)
  )

  expect_s3_class(spec, "ggexon_facet_labels_spec")
})

test_that("scale_x_facet_labels accepts a column-name string", {
  spec <- scale_x_facet_labels(
    facet = "cluster",
    labels = list(A = c("A2", "A1"), B = c("B2", "B1"))
  )

  expect_s3_class(spec, "ggexon_facet_labels_spec")
})

test_that("scale_x_facet_labels validates mappings and scales", {
  expect_error(
    scale_x_facet_labels(cluster, labels = list(c("A1", "A2"))),
    "named"
  )
  expect_error(
    scale_x_facet_labels(cluster, labels = list(A = c("A1"), B = c("B1", "B2"))),
    "same length"
  )
  expect_error(
    scale_x_facet_labels(cluster, labels = list(A = c("A1", "A2")), breaks = c(1, 1)),
    "unique"
  )
  expect_error(
    scale_x_facet_labels(cluster, labels = list(A = c("A1", "A2")), limits = c(2, 1)),
    "increasing"
  )
})

test_that("scale_x_facet_labels renders labels with fixed coordinates and shared ranges", {
  data <- data.frame(
    cluster = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 1, 4),
    y = 0
  )

  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(. ~ cluster) +
    scale_x_facet_labels(
      facet = cluster,
      labels = list(A = c("HOXA2", "HOXA1"), B = c("HOXB2", "HOXB1"))
    ) +
    ggplot2::coord_fixed(ratio = 1)

  built <- ggplot2::ggplot_build(plot)
  get_labels <- function(panel) {
    panel$x$scale$get_labels(panel$x$breaks)
  }

  expect_equal(get_labels(built$layout$panel_params[[1L]]), c("HOXA2", "HOXA1"))
  expect_equal(get_labels(built$layout$panel_params[[2L]]), c("HOXB2", "HOXB1"))
  expect_equal(built$layout$panel_params[[1L]]$x.range,
               built$layout$panel_params[[2L]]$x.range)
  expect_equal(built$layout$panel_params[[1L]]$x.range, c(1, 4))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("scale_x_facet_labels leaves unlisted facets on the default scale", {
  data <- data.frame(
    cluster = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    x = c(1, 1, 1),
    y = 0
  )

  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(. ~ cluster) +
    scale_x_facet_labels(
      facet = cluster,
      labels = list(A = "HOXA1", B = "HOXB1")
    )

  built <- ggplot2::ggplot_build(plot)
  default_built <- ggplot2::ggplot_build(
    ggplot2::ggplot(data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(. ~ cluster) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0))
  )
  expect_equal(
    built$layout$panel_params[[1L]]$x$scale$get_labels(
      built$layout$panel_params[[1L]]$x$breaks
    ),
    "HOXA1"
  )
  expect_equal(
    built$layout$panel_params[[3L]]$x$scale$get_labels(
      built$layout$panel_params[[3L]]$x$breaks
    ),
    default_built$layout$panel_params[[3L]]$x$scale$get_labels(
      default_built$layout$panel_params[[3L]]$x$breaks
    )
  )
})

test_that("scale_x_facet_labels works with a ggexon plot", {
  genes <- data.frame(
    cluster = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
    track = c("A", "A", "B", "B"),
    x = c(1, 2, 1, 2),
    y = 0,
    strand = "+"
  )

  plot <- ggexon() +
    geom_genebox(data = genes) +
    ggplot2::facet_grid(. ~ cluster) +
    scale_x_facet_labels(
      facet = cluster,
      labels = list(A = c("HOXA2", "HOXA1"), B = c("HOXB2", "HOXB1"))
    ) +
    ggplot2::coord_fixed(ratio = 1)

  built <- ggexon_build(plot)
  expect_equal(
    built@layout$panel_params[[1L]]$x$scale$get_labels(
      built@layout$panel_params[[1L]]$x$breaks
    ),
    c("HOXA2", "HOXA1")
  )
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("shared labels work with wrapping, explicit limits, and repeated builds", {
  data <- data.frame(cluster = c("A", "B"), x = c(1, 3), y = 0)
  base <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~cluster) + ggplot2::coord_fixed()
  plot <- base + scale_x_facet_labels("cluster",
    labels = list(A = c("A1", "A3"), B = c("B1", "B3")),
    breaks = c(1, 3), limits = c(0.5, 3.5)
  )
  get_axis_labels <- function(plot) {
    lapply(ggplot2::ggplot_build(plot)$layout$panel_params,
           function(panel) panel$x$get_labels())
  }
  expect_equal(get_axis_labels(plot), list(c("A1", "A3"), c("B1", "B3")))
  expect_equal(get_axis_labels(plot), get_axis_labels(plot))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
  expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x.range,
               c(0.5, 3.5))
  expect_equal(ggplot2::ggplot_build(base)$layout$layout$SCALE_X, c(1L, 1L))
})

test_that("facet label helper rejects incompatible layouts", {
  data <- data.frame(cluster = c("A", "B"), x = 1, y = 0)
  base <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point()
  spec <- scale_x_facet_labels(cluster, labels = list(A = "A1", B = "B1"))
  expect_error(base + spec, "facet_grid.*facet_wrap")
  expect_error(base + ggplot2::facet_grid(. ~ cluster, scales = "free_x") + spec,
               "fixed x")
  expect_error(ggplot2::ggplot_build(base + ggplot2::facet_grid(cluster ~ .) + spec),
               "columns, not rows")
  expect_error(ggplot2::ggplot_build(base + ggplot2::facet_grid(. ~ cluster) +
    scale_x_facet_labels(other, labels = list(A = "A1"))), "faceting variable")
})

test_that("every wrapped panel displays its x axis", {
  data <- data.frame(cluster = LETTERS[1:4], x = 1, y = 0)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~cluster, ncol = 2) +
    scale_x_facet_labels(cluster, labels = list(A = "A1", B = "B1", C = "C1", D = "D1")) +
    ggplot2::coord_fixed()
  table <- ggplot2::ggplotGrob(plot)
  axes <- table$grobs[grepl("^axis-b", table$layout$name)]
  expect_equal(sum(!vapply(axes, inherits, logical(1), "zeroGrob")), 4L)
})

test_that("facet labels preserve genomic genes aligned to a complete slot template", {
  genes <- data.frame(cluster = rep(c("A", "B"), each = 2),
    track = rep(c("A", "B"), each = 2), x = c(100, 200, 900, 1000),
    y = 0, strand = "+", slot = rep(c("Hox2", "Hox1"), 2))
  plot <- ggexon() + geom_genebox(data = genes) +
    strip_scale_x(slot_order = c("Hox3", "Hox2", "Hox1"), guide = "none") +
    ggplot2::facet_grid(. ~ cluster) +
    scale_x_facet_labels(cluster, labels = list(
      A = c("Hox3", "HOXA2", "HOXA1"), B = c("Hox3", "HOXB2", "HOXB1"))) +
    ggplot2::coord_fixed(ratio = 1)
  built <- ggexon_build(plot)
  expect_equal(nrow(built@data[[1]]), 4L)
  expect_equal(built@data[[1]]$x, c(2, 3, 2, 3))
  expect_equal(built@layout$panel_params[[1]]$x.range, c(0.5, 3.5))
  expect_equal(built@layout$panel_params[[2]]$x.range, c(0.5, 3.5))
  expect_equal(built@layout$panel_params[[2]]$x$get_labels(),
               c("Hox3", "HOXB2", "HOXB1"))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})
