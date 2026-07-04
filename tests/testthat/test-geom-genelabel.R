test_that("geom_genelabel renders tandem labels as a grob tree", {
  data <- data.frame(
    ymin = 0,
    xmin = c(0, 2),
    xmax = c(1, 3),
    transcripts = c("g1", "g2"),
    strand = "+",
    track = "track_a",
    label = "dup",
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    geom_genelabel(
      data = data,
      ggplot2::aes(
        ymin = ymin,
        xmin = xmin,
        xmax = xmax,
        transcripts = transcripts,
        strand = strand,
        track = track,
        label = label
      ),
      collapse_tandem = TRUE,
      link_type = "elbow",
      panel_width_mm = 60
    )

  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})

test_that("geom_genelabel validates label options", {
  expect_equal(.parse_label_positions("Top: bottom"), c("top", "bottom"))
  expect_error(geom_genelabel(panel_width_mm = 0), "panel_width_mm")
  expect_error(geom_genelabel(label_offset_fraction = -0.1), "label_offset_fraction")
  expect_error(geom_genelabel(show_link = "yes"), "show_link")
  expect_error(geom_genelabel(check_overlap = NA), "check_overlap")
})

test_that("geom_genelabel skips empty text labels", {
  data <- data.frame(
    ymin = 0,
    xmin = c(0, 2),
    xmax = c(1, 3),
    transcripts = c("g1", "g2"),
    strand = "+",
    track = "track_a",
    label = c("", ""),
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    geom_genelabel(
      data = data,
      ggplot2::aes(
        ymin = ymin,
        xmin = xmin,
        xmax = xmax,
        transcripts = transcripts,
        strand = strand,
        track = track,
        label = label
      )
    )

  expect_true(inherits(ggplot2::ggplotGrob(p), "gtable"))
})
