test_that("AS3MT combines matched human coverage and both splice patterns", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  result <- env$plot_as3mt_alignseq(demo_dir)
  junction_layer <- which(vapply(result$plot$layers,
    function(layer) inherits(layer$geom, "GeomJunction"), logical(1)))
  expect_length(junction_layer, 1L)
  if (length(junction_layer) != 1L) return()

  source <- utils::read.delim(file.path(demo_dir, "human-rnaseq-GTEX-15G19.tsv"))
  expect_equal(result$signal[, names(source)], source)
  expected <- data.frame(
    donor = c(102869593, 102869845, 102870211, 102869593),
    acceptor = c(102869805, 102870084, 102872448, 102872448),
    reads = c(9L, 10L, 11L, 12L),
    event = c("inclusion", "inclusion", "inclusion", "skipping"))
  expect_equal(result$junctions[, names(expected)], expected)
  expect_equal(result$junctions$x, expected$donor + .5)
  expect_equal(result$junctions$xend, expected$acceptor - .5)
  expect_true(all(result$junctions$track == "human"))
  # Each end meets the measured coverage on the exonic splice-boundary base.
  coverage_at <- function(position) vapply(position, function(pos) {
    source$score[source$start <= pos & source$end >= pos]
  }, numeric(1))
  expect_equal(result$junctions$y, coverage_at(expected$donor))
  expect_equal(result$junctions$yend, coverage_at(expected$acceptor))

  for (factor in c(10, 30)) {
    plot <- env$plot_as3mt_alignseq(demo_dir, intron_factor = factor)$plot
    built <- ggexon_build(plot)
    panels <- built@layout$layout
    human <- as.integer(panels$PANEL[panels$panel_type == "coverage" &
      panels$track == "human"])
    arcs <- built@data[[junction_layer]]
    expect_equal(nrow(arcs), 4L)
    expect_identical(unique(as.integer(arcs$PANEL)), human)
    expect_equal(arcs$.ggexon_raw_x, expected$donor + .5)
    expect_equal(arcs$.ggexon_raw_xend, expected$acceptor - .5)
    expect_equal(as.character(arcs$label), as.character(expected$reads))
    expect_equal(arcs$y, coverage_at(expected$donor))
    expect_equal(arcs$yend, coverage_at(expected$acceptor))
    expect_gt(built@layout$panel_params[[human]]$y.range[2], max(arcs$ymax))
    expect_equal(arcs$linewidth[order(expected$reads)],
      sort(arcs$linewidth))
    expect_silent(ggplot2::ggplot_gtable(built))
  }

  without <- env$plot_as3mt_alignseq(demo_dir, show_coverage = FALSE)
  expect_null(without$junctions)
  expect_false(any(vapply(without$plot$layers,
    function(layer) inherits(layer$geom, "GeomJunction"), logical(1))))
})

test_that("AS3MT can keep junction evidence on the human annotation instead", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  for (helper in c("plot_as3mt_alignseq", "plot_as3mt_coverage")) {
    result <- env[[helper]](demo_dir, junction_panel = "annotation")
    built <- ggexon_build(result$plot)
    panels <- built@layout$layout
    human <- as.integer(panels$PANEL[panels$panel_type == "annotation" &
      panels$track == "human"])
    index <- which(vapply(result$plot$layers,
      function(layer) inherits(layer$geom, "GeomJunction"), logical(1)))
    expect_length(index, 1L)
    arcs <- built@data[[index]]
    expect_identical(unique(as.integer(arcs$PANEL)), human)
    expect_equal(arcs$y, c(2.22, 2.22, 2.22, 1.12))
    expect_equal(result$junctions$reads, c(9L, 10L, 11L, 12L))
    expect_silent(ggplot2::ggplot_gtable(built))
  }
  expect_error(env$plot_as3mt_alignseq(demo_dir, junction_panel = "both"), "arg")
})
