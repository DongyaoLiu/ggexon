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
  expect_equal(axis_data$label, c("exon 99 bp", "intron 900 bp /10"))

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
