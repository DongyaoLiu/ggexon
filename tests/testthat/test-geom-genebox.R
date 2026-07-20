test_that("geom_genebox draws fixed-size boxes and omits unknown-strand arrows", {
  genes <- data.frame(
    x = c(1, 2, 3),
    y = 1,
    strand = c("+", "-", "*"),
    fill = c("black", "white", "grey70"),
    initiation_anchor_source = c(
      "explicit_start_codon",
      "terminal_CDS_positional_proxy",
      "manual"
    ),
    stop_anchor_source = c(
      "explicit_stop_codon",
      "terminal_CDS_positional_proxy",
      "manual"
    ),
    initiation_anchor_fallback = c(FALSE, TRUE, NA),
    stop_anchor_fallback = c(FALSE, TRUE, NA),
    any_anchor_fallback = c(FALSE, TRUE, NA),
    stringsAsFactors = FALSE
  )
  p <- ggplot2::ggplot() +
    geom_genebox(
      data = genes,
      ggplot2::aes(fill = fill),
      box_size = 4
    ) +
    ggplot2::scale_fill_identity()

  built <- ggplot2::ggplot_build(p)
  grob <- GeomGeneBox$draw_panel(
    built$data[[1L]],
    built$layout$panel_params[[1L]],
    built$layout$coord,
    box_size = 4
  )

  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  expect_s3_class(grob$children[[1L]], "rect")
  expect_equal(
    grid::convertWidth(grob$children[[1L]]$width[[1L]], "mm", valueOnly = TRUE),
    4
  )
  expect_equal(
    grid::convertHeight(grob$children[[1L]]$height[[1L]], "mm", valueOnly = TRUE),
    4
  )
  expect_s3_class(grob$children[[2L]], "segments")
  expect_length(grob$children[[2L]]$x0, 2L)
  expect_identical(
    built$data[[1L]]$initiation_anchor_source,
    genes$initiation_anchor_source
  )
  expect_identical(built$data[[1L]]$stop_anchor_source, genes$stop_anchor_source)
  expect_identical(
    built$data[[1L]]$initiation_anchor_fallback,
    genes$initiation_anchor_fallback
  )
  expect_identical(built$data[[1L]]$stop_anchor_fallback, genes$stop_anchor_fallback)
  expect_identical(built$data[[1L]]$any_anchor_fallback, genes$any_anchor_fallback)

  unknown <- genes[3L, , drop = FALSE]
  unknown_plot <- ggplot2::ggplot() + geom_genebox(data = unknown)
  unknown_built <- ggplot2::ggplot_build(unknown_plot)
  unknown_grob <- GeomGeneBox$draw_panel(
    unknown_built$data[[1L]],
    unknown_built$layout$panel_params[[1L]],
    unknown_built$layout$coord
  )
  expect_s3_class(unknown_grob$children[[2L]], "zeroGrob")
})

test_that("geom_genebox reverses arrow orientation once per x reversal", {
  genes <- data.frame(x = 1:2, y = 1, strand = c("+", "-"))
  normal <- ggplot2::ggplot_build(
    ggplot2::ggplot() + geom_genebox(data = genes)
  )
  reversed <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_genebox(data = genes) +
      ggplot2::scale_x_reverse()
  )
  coord_reversed <- ggplot2::ggplot_build(
    ggplot2::ggplot() +
      geom_genebox(data = genes) +
      ggplot2::coord_cartesian(reverse = "x")
  )

  expect_equal(
    .genebox_x_orientation(normal$layout$panel_params[[1L]], normal$layout$coord),
    1
  )
  expect_equal(
    .genebox_x_orientation(reversed$layout$panel_params[[1L]], reversed$layout$coord),
    -1
  )
  expect_equal(
    .genebox_x_orientation(
      coord_reversed$layout$panel_params[[1L]],
      coord_reversed$layout$coord
    ),
    -1
  )
  expect_equal(
    .genebox_strip_x_direction(c(1, -1, 0, NA, Inf)),
    c(1, -1, 1, 1, 1)
  )
  expect_equal(.genebox_contrast_colour(c("black", "white")), c("white", "black"))
})

test_that("geom_genebox uses facet-specific reverse_x for arrow orientation", {
  genes <- data.frame(
    track = factor(
      c("human", "human", "mouse", "mouse"),
      levels = c("human", "mouse")
    ),
    x = c(1, 2, 10, 20),
    y = 1,
    strand = "+",
    stringsAsFactors = FALSE
  )
  built <- ggexon_build(
    ggexon() +
      geom_genebox(data = genes) +
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
  expect_equal(
    .genebox_x_orientation(
      built@layout$panel_params[[human_panel]],
      built@plot@coordinates
    ),
    1
  )
  expect_equal(
    .genebox_x_orientation(
      built@layout$panel_params[[mouse_panel]],
      built@plot@coordinates
    ),
    -1
  )

  grid::pushViewport(grid::viewport(xscale = c(0, 1)))
  on.exit(grid::popViewport(), add = TRUE)
  arrow_delta <- function(panel_id) {
    panel_data <- built@data[[1L]][
      as.integer(built@data[[1L]]$PANEL) == panel_id,
      ,
      drop = FALSE
    ]
    grob <- GeomGeneBox$draw_panel(
      panel_data,
      built@layout$panel_params[[panel_id]],
      built@plot@coordinates
    )
    arrow <- grob$children[[2L]]
    grid::convertX(arrow$x1[1L] - arrow$x0[1L], "mm", valueOnly = TRUE)
  }
  expect_gt(arrow_delta(human_panel), 0)
  expect_lt(arrow_delta(mouse_panel), 0)
})

test_that("geom_genebox stacks only boxes sharing both x and y", {
  offsets <- .genebox_stack_offsets(
    x = c(5, 5, 5, 8),
    y = c(1, 1, 2, 1),
    box_size = 3
  )

  expect_equal(offsets[1:2], c(-1.725, 1.725))
  expect_equal(offsets[3:4], c(0, 0))
  expect_error(
    ggplot2::ggplot_build(
      ggplot2::ggplot() +
        geom_genebox(data = data.frame(x = 1, y = 1, strand = "+"), box_size = 0)
    ),
    "box_size"
  )
})

test_that("Syn gene boxes select the longest coding transcript and codon anchors", {
  annotation_path <- tempfile(fileext = ".gff3")
  on.exit(unlink(annotation_path), add = TRUE)
  writeLines(
    c(
      "##gff-version 3",
      "chr1\ttest\tgene\t50\t700\t.\t+\t.\tID=gene:g1;gene_id=g1;gene_name=HOXA1",
      "chr1\ttest\tmRNA\t100\t400\t.\t+\t.\tID=tx_short;Parent=gene:g1;gene_id=g1;transcript_id=tx_short;slot=Hox1",
      "chr1\ttest\tCDS\t150\t250\t.\t+\t0\tParent=tx_short;gene_id=g1;transcript_id=tx_short",
      "chr1\ttest\tstart_codon\t150\t152\t.\t+\t0\tParent=tx_short;gene_id=g1;transcript_id=tx_short",
      "chr1\ttest\tstop_codon\t248\t250\t.\t+\t0\tParent=tx_short;gene_id=g1;transcript_id=tx_short",
      "chr1\ttest\tmRNA\t100\t600\t.\t+\t.\tID=tx_long;Parent=gene:g1;gene_id=g1;transcript_id=tx_long;slot=Hox1",
      "chr1\ttest\tCDS\t200\t300\t.\t+\t0\tParent=tx_long;gene_id=g1;transcript_id=tx_long",
      "chr1\ttest\tCDS\t500\t550\t.\t+\t0\tParent=tx_long;gene_id=g1;transcript_id=tx_long",
      "chr1\ttest\tstart_codon\t200\t202\t.\t+\t0\tParent=tx_long;gene_id=g1;transcript_id=tx_long",
      "chr1\ttest\tstop_codon\t548\t550\t.\t+\t0\tParent=tx_long;gene_id=g1;transcript_id=tx_long",
      "chr1\ttest\tmRNA\t60\t680\t.\t+\t.\tID=tx_noncoding;Parent=gene:g1;gene_id=g1;transcript_id=tx_noncoding;slot=Hox1",
      "chr1\ttest\tgene\t1000\t1400\t.\t-\t.\tID=gene:g2;gene_id=g2;gene_name=HOXA2",
      "chr1\ttest\tmRNA\t1000\t1400\t.\t-\t.\tID=tx_minus;Parent=gene:g2;gene_id=g2;transcript_id=tx_minus;slot=Hox2",
      "chr1\ttest\tCDS\t1050\t1100\t.\t-\t0\tParent=tx_minus;gene_id=g2;transcript_id=tx_minus",
      "chr1\ttest\tCDS\t1300\t1350\t.\t-\t0\tParent=tx_minus;gene_id=g2;transcript_id=tx_minus",
      "chr1\ttest\tstart_codon\t1348\t1350\t.\t-\t0\tParent=tx_minus;gene_id=g2;transcript_id=tx_minus",
      "chr1\ttest\tstop_codon\t1050\t1052\t.\t-\t0\tParent=tx_minus;gene_id=g2;transcript_id=tx_minus",
      "chr1\ttest\tgene\t1600\t1800\t.\t+\t.\tID=gene:g3;gene_id=g3;gene_name=noncoding",
      "chr1\ttest\tmRNA\t1600\t1800\t.\t+\t.\tID=tx_only_noncoding;Parent=gene:g3;gene_id=g3;transcript_id=tx_only_noncoding",
      "chr1\ttest\tgene\t2000\t2200\t.\t+\t.\tID=gene:g4;gene_id=g4;gene_name=HOXA4",
      "chr1\ttest\tmRNA\t2000\t2200\t.\t+\t.\tID=tx_cds_only;Parent=gene:g4;gene_id=g4;transcript_id=tx_cds_only;slot=Hox4",
      "chr1\ttest\tCDS\t2000\t2002\t.\t+\t0\tParent=tx_cds_only;gene_id=g4;transcript_id=tx_cds_only",
      "chr1\ttest\tCDS\t2100\t2102\t.\t+\t0\tParent=tx_cds_only;gene_id=g4;transcript_id=tx_cds_only",
      "chr1\ttest\tstart_codon\t2000\t2001\t.\t+\t0\tParent=tx_cds_only;gene_id=g4;transcript_id=tx_cds_only",
      "chr1\ttest\tstop_codon\t2099\t2102\t.\t+\t0\tParent=tx_cds_only;gene_id=g4;transcript_id=tx_cds_only"
    ),
    annotation_path
  )
  individual <- SynIndividual(
    annotation_file = annotation_path,
    genome_file = genome_waiver(),
    id = "human"
  )

  expect_warning(
    middle <- syn_to_genebox_df(individual, chr = "chr1", anchor = "middle"),
    "omitted 2 transcript\\(s\\).*1 gene\\(s\\)"
  )
  middle <- middle[order(middle$gene_key), , drop = FALSE]

  expect_equal(middle$gene_key, c("g1", "g2", "g4"))
  expect_equal(middle$transcript_id, c("tx_long", "tx_minus", "tx_cds_only"))
  expect_equal(middle$transcript_span, c(501, 401, 201))
  expect_equal(middle$anchor_start, c(201, 1349, 2001))
  expect_equal(middle$anchor_end, c(549, 1051, 2101))
  expect_equal(middle$anchor_middle, c(375, 1200, 2051))
  expect_equal(middle$x, middle$anchor_middle)
  expect_equal(middle$genomic_x, middle$x)
  expect_equal(middle$slot, c("Hox1", "Hox2", "Hox4"))
  expect_equal(
    middle$initiation_anchor_source,
    c("explicit_start_codon", "explicit_start_codon", "terminal_CDS_positional_proxy")
  )
  expect_equal(
    middle$stop_anchor_source,
    c("explicit_stop_codon", "explicit_stop_codon", "terminal_CDS_positional_proxy")
  )
  expect_equal(middle$initiation_anchor_fallback, c(FALSE, FALSE, TRUE))
  expect_equal(middle$stop_anchor_fallback, c(FALSE, FALSE, TRUE))
  expect_equal(middle$any_anchor_fallback, c(FALSE, FALSE, TRUE))

  start <- syn_to_genebox_df(
    individual,
    chr = "chr1",
    anchor = "start",
    na.rm = TRUE
  )
  end <- syn_to_genebox_df(
    individual,
    chr = "chr1",
    anchor = "end",
    na.rm = TRUE
  )
  start <- start[order(start$gene_key), , drop = FALSE]
  end <- end[order(end$gene_key), , drop = FALSE]
  expect_equal(start$x, middle$anchor_start)
  expect_equal(end$x, middle$anchor_end)
  expect_true(all(start$anchor_mode == "start"))
  expect_true(all(end$anchor_mode == "end"))

  p <- ggexon(individual) +
    geom_genebox(chr = "chr1", na.rm = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
})

test_that("complete codon centres can cross feature range boundaries", {
  split_codon <- GenomicRanges::GRanges(
    seqnames = "chr1",
    ranges = IRanges::IRanges(start = c(10, 20), end = c(10, 21)),
    strand = "+"
  )
  reverse_split <- GenomicRanges::GRanges(
    seqnames = "chr1",
    ranges = IRanges::IRanges(start = c(10, 20), end = c(11, 20)),
    strand = "-"
  )

  expect_true(.genebox_complete_codon_feature(split_codon))
  expect_true(.genebox_complete_codon_feature(reverse_split))
  expect_false(.genebox_complete_codon_feature(split_codon[1L]))
  expect_false(.genebox_complete_codon_feature(c(split_codon, split_codon)))
  expect_equal(.genebox_nth_transcribed_base(split_codon, 2), 20)
  expect_equal(.genebox_nth_transcribed_base(reverse_split, 2), 11)
})
