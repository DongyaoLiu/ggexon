test_that("geom_synteny_link delegates to the panel link geom", {
  layer <- geom_synteny_link(
    alignment = "alignment_a",
    reference = "reference_a",
    chr = "chr1",
    subset = c(1, 100),
    filter_by_len = "> 10"
  )

  expect_s3_class(layer, "LayerSyn")
  expect_s3_class(layer$geom, "GeomPanel")
  expect_identical(layer$geom$required_aes, GeomNucLink$required_aes)
  expect_identical(layer$geom_params$alignment, "alignment_a")
  expect_identical(layer$geom_params$reference, "reference_a")
  expect_identical(layer$geom_params$chr, "chr1")
  expect_equal(layer$geom_params$subset, c(1, 100))
  expect_identical(layer$geom_params$filter_by_len, "> 10")
})

test_that("geom_synteny_link renders manual interval ribbons", {
  track_levels <- c("human", "link_human_macaque", "macaque")

  annotation_df <- data.frame(
    track = factor(c("human", "macaque"), levels = track_levels),
    x = c(10, 1000),
    y = c(1, 1)
  )

  link_df <- data.frame(
    track = factor("link_human_macaque", levels = track_levels),
    tspecies = "human",
    tchr = "chr7",
    tstart = 12,
    tend = 20,
    strand = "+",
    qspecies = "macaque",
    qchr = "chr3",
    qstart = 1010,
    qend = 1030,
    group = 1,
    hox_group = "HOXA1"
  )

  built <- ggexon_build(
    ggexon() +
      ggplot2::geom_blank(
        data = annotation_df,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      geom_synteny_link(
        data = link_df,
        mapping = ggplot2::aes(
          tspecies = tspecies,
          tchr = tchr,
          tstart = tstart,
          tend = tend,
          strand = strand,
          qspecies = qspecies,
          qchr = qchr,
          qstart = qstart,
          qend = qend,
          group = group,
          fill = hox_group
        ),
        inherit.aes = FALSE
      ) +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )

  link_layout_row <- built@layout$layout[
    as.character(built@layout$layout$track) == "link_human_macaque",
    ,
    drop = FALSE
  ]
  link_data <- built@data[[2L]]

  expect_identical(nrow(link_layout_row), 1L)
  expect_identical(nrow(link_data), 4L)
  expect_setequal(unique(link_data$x_variable), c("tstart", "tend", "qstart", "qend"))
  expect_false(anyNA(link_data$fill))
  expect_length(unique(link_data$fill), 1L)
  expect_setequal(
    unique(as.integer(link_data$source_panel)),
    c(link_layout_row$t_panel, link_layout_row$q_panel)
  )
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("tstart", "tend")]) ==
      link_layout_row$t_panel
  ))
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("qstart", "qend")]) ==
      link_layout_row$q_panel
  ))
})

test_that("facet_genomics can compact link panels in the rendered gtable", {
  track_levels <- c("human", "link_human_macaque", "macaque")

  annotation_df <- data.frame(
    track = factor(c("human", "macaque"), levels = track_levels),
    x = c(10, 1000),
    y = c(1, 1)
  )

  link_df <- data.frame(
    track = factor("link_human_macaque", levels = track_levels),
    tspecies = "human",
    tchr = "chr7",
    tstart = 12,
    tend = 20,
    strand = "+",
    qspecies = "macaque",
    qchr = "chr3",
    qstart = 1010,
    qend = 1030,
    group = 1,
    hox_group = "HOXA1"
  )

  built <- ggexon_build(
    ggexon() +
      ggplot2::geom_blank(
        data = annotation_df,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      geom_synteny_link(
        data = link_df,
        mapping = ggplot2::aes(
          tspecies = tspecies,
          tchr = tchr,
          tstart = tstart,
          tend = tend,
          strand = strand,
          qspecies = qspecies,
          qchr = qchr,
          qstart = qstart,
          qend = qend,
          group = group,
          fill = hox_group
        ),
        inherit.aes = FALSE
      ) +
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        ncol = 1,
        link_panel_height = 0.25,
        link_axis = "none",
        link_strip = "blank"
      )
  )
  table <- ggplot2::ggplot_gtable(built)

  link_panel_idx <- which(table$layout$name == "panel-1-2")
  annotation_panel_idx <- which(table$layout$name == "panel-1-1")
  link_axis_idx <- which(table$layout$name == "axis-b-1-2")
  link_strip_idx <- which(table$layout$name == "strip-t-1-2")

  expect_identical(length(link_panel_idx), 1L)
  expect_identical(length(annotation_panel_idx), 1L)
  expect_identical(grid::unitType(table$heights[table$layout$t[[link_panel_idx]]]), "null")
  expect_equal(as.numeric(table$heights[table$layout$t[[link_panel_idx]]]), 0.25)
  expect_equal(as.numeric(table$heights[table$layout$t[[annotation_panel_idx]]]), 1)

  expect_s3_class(table$grobs[[link_axis_idx]], "zeroGrob")
  expect_s3_class(table$grobs[[link_strip_idx]], "zeroGrob")
  expect_equal(
    grid::convertHeight(table$heights[table$layout$t[[link_axis_idx]]], "pt", valueOnly = TRUE),
    0
  )
  expect_equal(
    grid::convertHeight(table$heights[table$layout$t[[link_strip_idx]]], "pt", valueOnly = TRUE),
    0
  )

  expect_error(
    facet_genomics(ggplot2::vars(track), link_panel_height = 0),
    "link_panel_height"
  )
  expect_error(
    facet_genomics(ggplot2::vars(track), link_axis = "bad"),
    "link_axis"
  )
  expect_error(
    facet_genomics(ggplot2::vars(track), link_strip = "bad"),
    "link_strip"
  )
})
