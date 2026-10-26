test_that("AS3MT combined DNA figure includes real RNA-seq on the human genomic map", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  source_path <- file.path(demo_dir, "human-rnaseq-GTEX-15G19.tsv")
  source <- utils::read.delim(source_path)
  source_checksum <- unname(tools::md5sum(source_path))
  expect_equal(nrow(source), 1072L)
  source_width <- sum(source$end - source$start + 1)
  expect_equal(source_width, 32430)

  for (factor in c(10, 30)) {
    result <- env$plot_as3mt_alignseq(demo_dir, intron_factor = factor)
    layer_index <- function(class) which(vapply(result$plot$layers,
      function(layer) inherits(layer$geom, class), logical(1)))
    coverage_layer <- layer_index("GeomCoverage")
    expect_length(coverage_layer, 1L)
    if (length(coverage_layer) != 1L) next

    built <- ggexon_build(result$plot)
    layout <- as.data.frame(built@layout$layout)
    annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]
    coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
    human_panel <- as.integer(annotation$PANEL[annotation$track == "human"])
    annotation_panels <- as.integer(annotation$PANEL)
    expect_setequal(as.character(annotation$track), c("human", "chimp"))
    expect_equal(nrow(coverage), 1L)
    expect_identical(as.character(coverage$track), "human")
    expect_identical(as.integer(coverage$x_source_panel), human_panel)
    expect_identical(coverage$SCALE_X,
      annotation$SCALE_X[annotation$track == "human"])
    coverage_panel <- as.integer(coverage$PANEL)
    expect_equal(built@layout$panel_params[[coverage_panel]]$x.range,
      built@layout$panel_params[[human_panel]]$x.range)

    signal <- built@data[[coverage_layer]]
    expect_identical(unique(as.integer(signal$PANEL)), coverage_panel)
    parent <- match(paste(signal$interval_start, signal$interval_end),
      paste(source$start, source$end))
    expect_false(anyNA(parent))
    expect_equal(signal$coverage, source$score[parent])
    weights <- .coverage_span_weights(signal)
    expect_equal(sum(weights), source_width)
    expect_equal(sum(signal$coverage * weights),
      sum(source$score * (source$end - source$start + 1)))
    parent_weights <- rowsum(weights, parent, reorder = FALSE)
    expect_equal(as.numeric(parent_weights[match(seq_len(nrow(source)),
      as.integer(rownames(parent_weights))), 1L]), source$end - source$start + 1)
    transform <- built@layout$genomic_x_transforms[[as.character(human_panel)]]
    expect_equal(signal$xmin,
      ggexon_genomic_to_plot_x(signal$genomic_edge_start, transform))
    expect_equal(signal$xmax,
      ggexon_genomic_to_plot_x(signal$genomic_edge_end, transform))
    expect_equal(result$signal[, names(source)], source)
    expect_true(all(result$signal$track == "human"))

    # Transcript labels, skipped exons, DNA calls, repeat boxes and donor marks
    # must stay out of the quantitative RNA-seq panel.
    annotation_layers <- setdiff(seq_along(result$plot$layers),
      c(coverage_layer, layer_index("GeomPanel"), layer_index("GeomJunction")))
    for (i in annotation_layers) {
      expect_gt(nrow(built@data[[i]]), 0L)
      expect_true(all(as.integer(built@data[[i]]$PANEL) %in% annotation_panels))
    }
    dna_layer <- layer_index("GeomAlignseq")
    expect_length(dna_layer, 1L)
    dna <- built@data[[dna_layer]]
    expect_setequal(as.integer(dna$PANEL), annotation_panels)
    expect_equal(sum(dna$boundary_after), 2L)
    expect_true(all(dna$boundary_label[dna$boundary_after] == "E1 donor | intron 1"))
    expect_equal(result$data$position[result$data$boundary_after],
      c(102869593, 104324234))
    junction <- unlist(lapply(built@data, function(layer) {
      if (!"label" %in% names(layer)) return(integer())
      as.integer(layer$PANEL[!is.na(layer$label) & layer$label == "E1 → E4"])
    }))
    expect_identical(junction, human_panel)
    links <- built@data[[layer_index("GeomPanel")]]
    expect_gt(nrow(links), 0L)
    expect_true(all(as.integer(links$source_panel) %in% annotation_panels))
    expect_equal(length(unique(links$parent_group)), 37L)

    expect_s3_class(result$grob, "gtable")
    expect_true(all(c("as3mt-scale-legend", "as3mt-vntr-evidence") %in%
      result$grob$layout$name))
    coverage_grob <- result$grob$grobs[[match(
      paste0("panel-", coverage$COL, "-", coverage$ROW), result$grob$layout$name)]]
    signal_rectangles <- Filter(function(grob) inherits(grob, "rect") &&
      length(grob$x) == nrow(signal), coverage_grob$children)
    expect_length(signal_rectangles, 1L)
    panel_rows <- grepl("^panel-", result$grob$layout$name)
    external_rows <- result$grob$layout$name %in%
      c("as3mt-scale-legend", "as3mt-vntr-evidence")
    expect_true(all(result$grob$layout$t[external_rows] >
      max(result$grob$layout$b[panel_rows])))
    scale_legend <- result$grob$grobs[[match("as3mt-scale-legend",
      result$grob$layout$name)]]
    expect_setequal(names(scale_legend$children), c("exon-scale-bar", "intron-scale-bar"))
    expect_equal(nrow(result$scale_bars), 2L)
    expect_identical(result$scale_bars$region_type, c("exon", "intron"))
    expect_equal(result$scale_bars$genomic_width, c(500, 500 * factor))
    expect_equal(result$scale_bars$plot_width, c(500, 500))
    expect_equal(result$display_spans[1], result$display_spans[2], tolerance = 1e-6)
    expect_equal(result$scale_bars$fraction,
      rep(500 / result$display_spans[1], 2L))
    expect_equal(result$vntr$constructs$repeat_count, c(2L, 3L))
    expect_silent(ggplot2::ggplot_gtable(built))
  }
  expect_identical(unname(tools::md5sum(source_path)), source_checksum)
  expect_equal(utils::read.delim(source_path), source)
})

test_that("AS3MT DNA-only opt-out retains DNA evidence and calibrated scales", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  expect_true("show_coverage" %in% names(formals(env$plot_as3mt_alignseq)))
  if (!"show_coverage" %in% names(formals(env$plot_as3mt_alignseq))) return()
  result <- env$plot_as3mt_alignseq(demo_dir, show_coverage = FALSE)
  geoms <- vapply(result$plot$layers, function(layer) class(layer$geom)[1L], character(1))
  expect_false("GeomCoverage" %in% geoms)
  expect_true(all(c("GeomExon2", "GeomAlignseq", "GeomPanel", "GeomSegment") %in% geoms))
  built <- ggexon_build(result$plot)
  expect_false(any(built@layout$layout$panel_type == "coverage"))
  expect_setequal(as.character(built@layout$layout$track[
    built@layout$layout$panel_type == "annotation"]), c("human", "chimp"))
  expect_null(result$signal)
  expect_equal(result$data$position[result$data$boundary_after], c(102869593, 104324234))
  expect_equal(result$vntr$constructs$repeat_count, c(2L, 3L))
  expect_equal(result$scale_bars$genomic_width, c(500, 15000))
  expect_equal(result$scale_bars$plot_width, c(500, 500))
  expect_equal(result$display_spans[1], result$display_spans[2], tolerance = 1e-6)
  expect_true(all(c("as3mt-scale-legend", "as3mt-vntr-evidence") %in%
    result$grob$layout$name))
  expect_silent(ggplot2::ggplot_gtable(built))
})
