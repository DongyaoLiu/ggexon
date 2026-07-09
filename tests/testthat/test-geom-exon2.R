test_that("geom_exon2 compresses introns and keeps genomic coordinates", {
  exon_df <- data.frame(
    xmin = c(1, 1000, 1200),
    xmax = c(100, 1100, 1250),
    ymin = c(2, 2, 2),
    transcripts = c("tx1", "tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = c("five_prime_UTR", "CDS", "three_prime_UTR"),
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
      geom_exon2(intron_width = 20, annotation_type = "all")
  )

  layer_data <- built$data[[1L]]
  expect_equal(layer_data$genomic_xmin, exon_df$xmin)
  expect_equal(layer_data$genomic_xmax, exon_df$xmax)
  expect_equal(layer_data$xmin, c(1, 120, 240))
  expect_equal(layer_data$xmax, c(100, 220, 290))
})

test_that("geom_exon2 draws UTR boxes thinner than CDS boxes", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = c("five_prime_UTR", "CDS"),
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
      geom_exon2(annotation_type = "all", utr_height = 0.5, cds_height = 1)
  )

  layer_data <- built$data[[1L]]
  heights <- layer_data$ymax - layer_data$ymin
  expect_lt(heights[layer_data$type == "five_prime_UTR"], heights[layer_data$type == "CDS"])
})

test_that("geom_exon2 chevron direction controls intron peak", {
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

  built_up <- ggplot2::ggplot_build(
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
      geom_exon2(intron_width = 20, chevron_direction = "up")
  )
  built_down <- ggplot2::ggplot_build(
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
      geom_exon2(intron_width = 20, chevron_direction = "down")
  )

  up_intron <- ggexon:::.exon2_intron_data(built_up$data[[1L]], direction = "up")
  down_intron <- ggexon:::.exon2_intron_data(built_down$data[[1L]], direction = "down")

  expect_gt(up_intron$y[2L], up_intron$y[1L])
  expect_lt(down_intron$y[2L], down_intron$y[1L])
})

test_that("geom_exon2 arrow points to transcript strand direction", {
  exon_df <- data.frame(
    xmin = c(1, 1000, 1, 1000),
    xmax = c(100, 1100, 100, 1100),
    ymin = c(2, 2, 4, 4),
    transcripts = c("plus_tx", "plus_tx", "minus_tx", "minus_tx"),
    strand = c("+", "+", "-", "-"),
    track = "gene1",
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
      geom_exon2(intron_width = 20, arrow_width = 10)
  )

  arrow_data <- ggexon:::.exon2_arrow_data(built$data[[1L]])
  plus_arrow <- arrow_data[arrow_data$transcripts == "plus_tx", , drop = FALSE]
  minus_arrow <- arrow_data[arrow_data$transcripts == "minus_tx", , drop = FALSE]

  expect_equal(plus_arrow$x[1L], max(plus_arrow$x))
  expect_equal(minus_arrow$x[1L], min(minus_arrow$x))
})

test_that("geom_exon2 can fix structural intron and arrow aesthetics", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(100, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = "exon",
    group = 1,
    exon_role = c("common", "variable"),
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
        group = group,
        fill = exon_role
      )
    ) +
      geom_exon2(
        intron_width = 20,
        transcript_backbone_fill = "grey82",
        transcript_backbone_colour = "grey70"
      )
  )

  intron_data <- ggexon:::.exon2_intron_data(built$data[[1L]])
  arrow_data <- ggexon:::.exon2_arrow_data(built$data[[1L]])
  fixed_intron <- ggexon:::.apply_transcript_backbone_aes(
    intron_data,
    fill = "grey82",
    colour = "grey70",
    use_fill_as_colour = TRUE
  )
  fixed_arrow <- ggexon:::.apply_transcript_backbone_aes(
    arrow_data,
    fill = "grey82",
    colour = "grey70",
    use_fill_as_colour = TRUE
  )

  expect_true(inherits(ggplot2::ggplotGrob(
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
        group = group,
        fill = exon_role
      )
    ) +
      geom_exon2(
        intron_width = 20,
        transcript_backbone_fill = "grey82",
        transcript_backbone_colour = "grey70"
      )
  ), "gtable"))
  expect_equal(fixed_intron$colour, rep("grey70", nrow(fixed_intron)))
  expect_equal(fixed_arrow$fill, rep("grey82", nrow(fixed_arrow)))
  expect_equal(fixed_arrow$colour, rep("grey70", nrow(fixed_arrow)))
})

test_that("geom_exon2 terminal rectangles are trimmed before arrow caps", {
  exon_df <- data.frame(
    xmin = c(1, 1000),
    xmax = c(101, 1100),
    ymin = c(2, 2),
    transcripts = c("tx1", "tx1"),
    strand = "+",
    track = "gene1",
    type = "exon",
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
      geom_exon2(intron_width = 20)
  )

  layer_data <- built$data[[1L]]
  rect_data <- ggexon:::.exon2_trim_terminal_rects(layer_data)
  terminal <- layer_data[nrow(layer_data), , drop = FALSE]
  terminal_rect <- rect_data[nrow(rect_data), , drop = FALSE]

  expect_lt(terminal_rect$xmax, terminal$xmax)
  expect_equal(terminal$xmax - terminal_rect$xmax, (terminal$xmax - terminal$xmin) * 0.35)
})

test_that("geom_exon2 resolves SynSpecies data like geom_exon", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )

  built <- ggplot2::ggplot_build(
    ggexon(sp) +
      geom_exon2(
        chr = "RagTag_V",
        subset = c(21550000, 21680000)
      )
  )

  expect_true(nrow(built$data[[1L]]) > 0L)
  expect_identical(unique(built$data[[1L]]$track), "XZ1516")
})

test_that("geom_exon2 defaults to the same Syn exon rows as geom_exon", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )

  exon_build <- ggplot2::ggplot_build(
    ggexon(sp) +
      geom_exon(
        chr = "RagTag_V",
        subset = c(21550000, 21680000)
      )
  )
  exon2_build <- ggplot2::ggplot_build(
    ggexon(sp) +
      geom_exon2(
        chr = "RagTag_V",
        subset = c(21550000, 21680000)
      )
  )

  expect_equal(nrow(exon2_build$data[[1L]]), nrow(exon_build$data[[1L]]))
  expect_true(all(exon2_build$data[[1L]]$type == "exon"))
})

test_that("geom_exon2 annotation_type all keeps CDS and exon rows", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )

  built <- ggplot2::ggplot_build(
    ggexon(sp) +
      geom_exon2(
        chr = "RagTag_V",
        subset = c(21550000, 21680000),
        annotation_type = "all"
      )
  )

  expect_true(all(c("CDS", "exon") %in% unique(built$data[[1L]]$type)))
})
