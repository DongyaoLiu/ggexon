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

test_that("geom_nuclink uses a polygon legend key", {
  key <- GeomNucLink$draw_key(
    data.frame(
      fill = "#123456",
      colour = NA_character_,
      alpha = 1,
      linewidth = 0.5,
      linetype = 1
    ),
    list(),
    5
  )

  expect_s3_class(key, "rect")
  expect_identical(key$gp$fill, "#123456FF")
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
  link_panel <- as.integer(link_layout_row$PANEL[[1L]])
  link_data <- built@data[[2L]]

  expect_identical(nrow(link_layout_row), 1L)
  expect_identical(nrow(link_data), 4L)
  expect_equal(built@layout$panel_params[[link_panel]]$y.range, c(0, 1))
  expect_equal(
    built@layout$panel_params[[link_panel]]$y$continuous_range,
    c(0, 1)
  )
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

test_that("facet_genomics(xlim) trains manual annotation panels beyond observed features", {
  track_levels <- c("human", "link_human_macaque", "macaque")

  annotation_df <- data.frame(
    track = factor(c("human", "macaque"), levels = track_levels),
    x = c(10, 1010),
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
    group = 1
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
          group = group
        ),
        inherit.aes = FALSE
      ) +
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        xlim = list(
          human = c(0, 100),
          macaque = c(1000, 1200)
        )
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0))
  )

  layout_df <- as.data.frame(built@layout$layout)
  human_panel <- as.integer(layout_df$PANEL[layout_df$track == "human"][[1L]])
  macaque_panel <- as.integer(layout_df$PANEL[layout_df$track == "macaque"][[1L]])

  expect_equal(built@layout$panel_params[[human_panel]]$x.range, c(0, 100))
  expect_equal(built@layout$panel_params[[macaque_panel]]$x.range, c(1000, 1200))
  expect_equal(layout_df$xlim_min[layout_df$track == "human"], 0)
  expect_equal(layout_df$xlim_max[layout_df$track == "macaque"], 1200)
})

test_that("facet_genomics(reverse_x) reverses selected annotation panels and source-link x transforms", {
  track_levels <- c("human", "link_human_mouse", "mouse")

  annotation_df <- data.frame(
    track = factor(
      c("human", "human", "mouse", "mouse"),
      levels = track_levels
    ),
    x = c(0, 100, 1000, 1100),
    y = 1
  )
  link_df <- data.frame(
    track = factor("link_human_mouse", levels = track_levels),
    tspecies = "human",
    tchr = "chr11",
    tstart = 10,
    tend = 30,
    strand = "+",
    qspecies = "mouse",
    qchr = "chr2",
    qstart = 1010,
    qend = 1030,
    group = 1
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
          group = group
        ),
        inherit.aes = FALSE
      ) +
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        reverse_x = "mouse",
        reverse_x_match_by = "track"
      )
  )

  layout_df <- as.data.frame(built@layout$layout)
  human_panel <- as.integer(layout_df$PANEL[layout_df$track == "human"][[1L]])
  mouse_panel <- as.integer(layout_df$PANEL[layout_df$track == "mouse"][[1L]])

  expect_identical(built@layout$panel_params[[human_panel]]$reverse, "none")
  expect_identical(built@layout$panel_params[[mouse_panel]]$reverse, "x")
  expect_identical(built@layout$ggexon_reverse_x_panels, mouse_panel)

  link_data <- built@data[[2L]]
  transformed <- .transform_link_x_by_source_panel(
    link_data,
    built@layout$panel_params,
    built@plot@coordinates
  )

  transformed_x <- stats::setNames(transformed$x, transformed$x_variable)

  expect_true(transformed_x[["tstart"]] < transformed_x[["tend"]])
  expect_true(transformed_x[["qstart"]] > transformed_x[["qend"]])
})

test_that("facet_genomics(reverse_x) validates matches", {
  expect_error(
    facet_genomics(ggplot2::vars(track), reverse_x = NA),
    "reverse_x"
  )

  track_df <- data.frame(
    track = c("human", "mouse"),
    x = c(1, 2),
    y = c(1, 1)
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(track_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        facet_genomics(
          ggplot2::vars(track),
          scales = "free_x",
          reverse_x = "rat",
          reverse_x_match_by = "track"
        )
    ),
    "reverse_x"
  )
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

test_that("facet_genomics(annotation_axis = 'bottom') keeps the x-axis only on the bottom annotation panel", {
  track_levels <- c("human", "link_human_macaque", "macaque")
  annotation_df <- data.frame(
    track = factor(c("human", "macaque"), levels = track_levels),
    x = c(10, 1000), y = c(1, 1)
  )
  link_df <- data.frame(
    track = factor("link_human_macaque", levels = track_levels),
    tspecies = "human", tchr = "chr7", tstart = 12, tend = 20, strand = "+",
    qspecies = "macaque", qchr = "chr3", qstart = 1010, qend = 1030,
    group = 1, hox_group = "HOXA1"
  )
  make_table <- function(annotation_axis) {
    built <- ggexon_build(
      ggexon() +
        ggplot2::geom_blank(data = annotation_df, mapping = ggplot2::aes(x = x, y = y)) +
        geom_synteny_link(
          data = link_df,
          mapping = ggplot2::aes(
            tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend, strand = strand,
            qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
            group = group, fill = hox_group
          ),
          inherit.aes = FALSE
        ) +
        facet_genomics(
          ggplot2::vars(track), scales = "free_x", ncol = 1,
          link_axis = "none", annotation_axis = annotation_axis
        )
    )
    ggplot2::ggplot_gtable(built)
  }

  # default keeps the per-panel x-axis on both annotation panels
  tab_all <- make_table("all")
  expect_false(inherits(tab_all$grobs[[which(tab_all$layout$name == "axis-b-1-1")]], "zeroGrob"))
  expect_false(inherits(tab_all$grobs[[which(tab_all$layout$name == "axis-b-1-3")]], "zeroGrob"))

  # 'bottom' blanks the interior (top) annotation axis and keeps the bottom one
  tab_bottom <- make_table("bottom")
  top_axis <- which(tab_bottom$layout$name == "axis-b-1-1")
  bot_axis <- which(tab_bottom$layout$name == "axis-b-1-3")
  expect_s3_class(tab_bottom$grobs[[top_axis]], "zeroGrob")
  expect_false(inherits(tab_bottom$grobs[[bot_axis]], "zeroGrob"))
  expect_equal(
    grid::convertHeight(tab_bottom$heights[tab_bottom$layout$t[[top_axis]]], "pt", valueOnly = TRUE),
    0
  )

  expect_error(facet_genomics(ggplot2::vars(track), annotation_axis = "bad"), "annotation_axis")
})
