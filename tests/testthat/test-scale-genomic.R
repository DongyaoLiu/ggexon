test_that("whole-gene consumers do not change exon-trained compression", {
  exon <- data.frame(xmin = c(1, 1000), xmax = c(100, 1100), ymin = 1,
    track = "reference", transcripts = "tx", strand = "+", type = "exon", group = 1)
  gene <- exon[1, ]
  gene$xmax <- 1100
  plot <- ggexon(exon, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
    track = track, transcripts = transcripts, strand = strand, type = type, group = group)) +
    geom_exon2(annotation_type = "all") +
    geom_gene(data = gene, proportion_trim3 = 0) +
    scale_x_ggexon_genomic(intron_factor = 10)
  built <- ggexon_build(plot)
  expect_equal(built@data[[1L]]$xmin, c(1, 190))
  expect_equal(built@data[[1L]]$xmax, c(100, 290))
  expect_equal(built@data[[2L]]$xmax, 290)
})

test_that("scale_x_ggexon_genomic compresses introns but labels genomic coordinates", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = c("exon", "exon"),
    group = 1,
    stringsAsFactors = FALSE
  )

  built <- ggplot2::ggplot_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      scale_x_ggexon_genomic(
        intron_factor = 10,
        breaks = c(1, 100, 1000, 1100),
        labels = function(x) paste0("g", x)
      )
  )

  layer_data <- built$data[[1L]]
  expect_equal(layer_data$genomic_xmin, exon_df$xmin)
  expect_equal(layer_data$genomic_xmax, exon_df$xmax)
  expect_equal(layer_data$xmin, c(1, 190))
  expect_equal(layer_data$xmax, c(100, 290))

  x_view <- built@layout$panel_params[[1L]]$x
  expect_equal(unname(x_view$get_breaks()), c(1, 100, 190, 290))
  expect_equal(x_view$get_labels(), c("g1", "g100", "g1000", "g1100"))
})

test_that("scale_x_ggexon_genomic uses exon union intervals within a panel", {
  exon_df <- data.frame(
    xmin = c(1, 1000, 50),
    xmax = c(100, 1100, 150),
    ymin = c(2, 2, 3),
    transcripts = c("tx1", "tx1", "tx2"),
    strand = "+",
    track = "gene1",
    type = "exon",
    group = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  built <- ggplot2::ggplot_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      scale_x_ggexon_genomic(intron_factor = 10, breaks = c(1, 150, 1000, 1100))
  )

  layer_data <- built$data[[1L]]
  expect_equal(layer_data$xmin, c(1, 235, 50))
  expect_equal(layer_data$xmax, c(100, 335, 150))
})

test_that("scale_x_ggexon_genomic can compress a subset of panels", {
  exon_df <- data.frame(
    xmin = c(1, 1000, 1, 1000),
    xmax = c(100, 1100, 100, 1100),
    ymin = c(2, 2, 2, 2),
    transcripts = c("tx_human", "tx_human", "tx_worm", "tx_worm"),
    strand = "+",
    track = c("human_like", "human_like", "worm_like", "worm_like"),
    type = "exon",
    group = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  built <- ggplot2::ggplot_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      facet_genomics(ggplot2::vars(track), scales = "free_x") +
      scale_x_ggexon_genomic(
        intron_factor = 10,
        species = "human_like",
        match_by = "track",
        breaks = c(1, 100, 1000, 1100)
      )
  )

  layout_df <- as.data.frame(built@layout$layout)
  human_panel <- layout_df$PANEL[layout_df$track == "human_like"][[1L]]
  worm_panel <- layout_df$PANEL[layout_df$track == "worm_like"][[1L]]

  layer_data <- built$data[[1L]]
  panel_ids <- as.integer(as.character(layer_data$PANEL))
  human_data <- layer_data[panel_ids == human_panel, ]
  worm_data <- layer_data[panel_ids == worm_panel, ]
  human_data <- human_data[order(human_data$genomic_xmin), ]
  worm_data <- worm_data[order(worm_data$genomic_xmin), ]

  expect_equal(human_data$xmin, c(1, 190))
  expect_equal(human_data$xmax, c(100, 290))
  expect_equal(worm_data$xmin, c(1, 1000))
  expect_equal(worm_data$xmax, c(100, 1100))
})

test_that("scale_x_ggexon_genomic errors when selected species are absent", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = "tx1",
    strand = "+",
    track = "gene1",
    type = "exon",
    group = 1,
    stringsAsFactors = FALSE
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(
        exon_df,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          transcripts = transcripts,
          strand = strand,
          track = track,
          type = type,
          group = group
        )
      ) +
        geom_exon2(annotation_type = "all") +
        facet_genomics(ggplot2::vars(track), scales = "free_x") +
        scale_x_ggexon_genomic(species = "missing", match_by = "track")
    ),
    "`species` did not match any panel layout values",
    fixed = TRUE
  )
})

test_that("scale_x_ggexon_genomic can build a piecewise representative axis", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = "exon",
    group = 1,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      scale_x_ggexon_genomic(
        intron_factor = 10,
        guide = guide_x_ggexon_piecewise()
      )
  )

  axis_data <- built@layout$genomic_x_axis_data
  expect_s3_class(axis_data, "data.frame")
  expect_equal(axis_data$region_type, c("exon", "intron"))
  expect_equal(axis_data$genomic_width, c(99, 900))
  expect_equal(axis_data$plot_width, c(99, 90))
  expect_equal(axis_data$label, c("99 bp", "900 bp"))

  x_view <- built@layout$panel_params[[1L]]$x
  expect_equal(unname(x_view$get_breaks()), numeric())
})

test_that("piecewise genomic axis replaces bottom axis grobs", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = "exon",
    group = 1,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      scale_x_ggexon_genomic(guide = guide_x_ggexon_piecewise()) +
      theme_ggexon_track()
  )
  table <- ggplot_gtable(built)
  axis_idx <- grep("^axis-b", table$layout$name)

  expect_true(any(vapply(table$grobs[axis_idx], inherits, logical(1), "ggexonGenomicPiecewiseAxisGrob")))

  axis <- Filter(function(grob) inherits(grob, "ggexonGenomicPiecewiseAxisGrob"),
    table$grobs[axis_idx])[[1L]]
  drawn <- grid::grid.grabExpr(grid::drawDetails(axis))
  bars <- Filter(function(grob) inherits(grob, "segments") && length(grob$y0) == 1L,
    as.list(drawn$children))
  labels <- Filter(function(grob) inherits(grob, "text"), as.list(drawn$children))
  expect_length(bars, 2L)
  expect_equal(as.numeric(bars[[1L]]$y0), as.numeric(bars[[2L]]$y0))
  expect_equal(as.numeric(labels[[1L]]$y), as.numeric(labels[[2L]]$y))
})

test_that("piecewise linewidth controls bars and end ticks in millimetres", {
  exon <- data.frame(xmin = c(1, 1000), xmax = c(100, 1100), ymin = 1,
    track = "reference", transcripts = "tx", strand = "+", type = "exon", group = 1)
  plot <- ggexon(exon, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
    track = track, transcripts = transcripts, strand = strand, type = type, group = group)) +
    geom_exon2(compress_introns = FALSE) +
    scale_x_ggexon_genomic(guide = guide_x_ggexon_piecewise(by = "panel", linewidth = 0.7)) +
    theme_ggexon_track() +
    ggplot2::theme(axis.line.x = ggplot2::element_line(linewidth = 0.1),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.2))
  table <- ggplot_gtable(ggexon_build(plot))
  axis <- Filter(function(grob) inherits(grob, "ggexonGenomicPiecewiseAxisGrob"),
    table$grobs)[[1L]]
  drawn <- grid::grid.grabExpr(grid::drawDetails(axis))
  lines <- Filter(function(grob) inherits(grob, "segments"), as.list(drawn$children))
  expect_length(lines, 4L)
  expect_equal(vapply(lines, function(grob) grob$gp$lwd, numeric(1)),
    rep(0.7 * ggplot2::.pt, 4L), ignore_attr = TRUE)
})

test_that("piecewise labels use only base-pair lengths", {
  expect_equal(ggexon_genomic_x_axis_label("exon", 1500, 1500), "1,500 bp")
  expect_equal(ggexon_genomic_x_axis_label("intron", 1500, 150), "1,500 bp")
})

test_that("piecewise length labels do not overlap near the panel edge", {
  data <- data.frame(plot_start = c(0, 1), plot_end = c(1, 1.1),
    region_type = c("exon", "intron"), label = c("124 bp", "211 bp"),
    axis_group_index = 1L, axis_group_count = 1L)
  axis <- ggexon_genomic_piecewise_axis_grob(data, x_range = c(0, 100),
    text_gp = grid::gpar(fontsize = 9))
  drawn <- grid::grid.grabExpr(grid::drawDetails(axis), width = 7, height = 1)
  labels <- Filter(function(grob) inherits(grob, "text"), as.list(drawn$children))
  x <- vapply(labels, function(grob) as.numeric(grob$x), numeric(1))
  grDevices::pdf(NULL, width = 7, height = 1)
  widths <- tryCatch(
    vapply(labels, function(grob) grid::convertWidth(grid::grobWidth(grob),
      "npc", valueOnly = TRUE), numeric(1)),
    finally = grDevices::dev.off()
  )
  expect_gt(x[[2L]] - widths[[2L]] / 2, x[[1L]] + widths[[1L]] / 2)
  expect_gte(x[[1L]] - widths[[1L]] / 2, -1e-9)
  expect_lte(x[[2L]] + widths[[2L]] / 2, 1 + 1e-9)
})

test_that("piecewise linewidth accepts theme defaults and non-negative finite values", {
  expect_null(guide_x_ggexon_piecewise()$linewidth)
  expect_identical(guide_x_ggexon_piecewise(linewidth = 0)$linewidth, 0)
  for (value in list(-1, NA_real_, Inf, c(0.2, 0.3), "thick")) {
    expect_error(guide_x_ggexon_piecewise(linewidth = value),
      "`linewidth` must be one non-negative finite number.", fixed = TRUE)
  }
})

test_that("guide_x_ggexon_piecewise can hide selected representative bars and labels", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = "exon",
    group = 1,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(
      exon_df,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        transcripts = transcripts,
        strand = strand,
        track = track,
        type = type,
        group = group
      )
    ) +
      geom_exon2(annotation_type = "all") +
      scale_x_ggexon_genomic(
        guide = guide_x_ggexon_piecewise(
          show_exon = FALSE,
          label = FALSE
        )
      )
  )

  axis_data <- built@layout$genomic_x_axis_data
  expect_equal(axis_data$region_type, "intron")
  expect_equal(axis_data$label, "")
})
