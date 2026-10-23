.junction_test_exons <- function() {
  data.frame(xmin = c(1, 1000), xmax = c(100, 1100), ymin = 1,
    transcripts = "tx", strand = "+", track = "gene", type = "exon")
}

.junction_test_plot <- function(arcs, ...) {
  ggexon(.junction_test_exons(), ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type)) +
    geom_exon2(annotation_type = "all") +
    geom_junction(data = arcs, mapping = ggplot2::aes(
      x = donor, xend = acceptor, y = baseline, label = reads,
      height = rise, linewidth = reads), inherit.aes = FALSE, ...) +
    scale_x_ggexon_genomic(intron_factor = 10)
}

.junction_test_grobs <- function(grob, predicate) {
  found <- if (predicate(grob)) list(grob) else list()
  children <- c(as.list(grob$children), grob$grobs)
  c(found, unlist(lapply(children, .junction_test_grobs,
    predicate = predicate), recursive = FALSE))
}

test_that("junction endpoints consume one trained map and labels use displayed midpoint", {
  arcs <- data.frame(donor = c(100, 1100), acceptor = c(1100, 100),
    baseline = c(2, 3), reads = c(21, 45), rise = c(.8, .4))
  plot <- .junction_test_plot(arcs, colour = "#D55E00", label_family = "mono")
  built <- ggexon_build(plot)
  expect_equal(built@data[[2L]]$x, c(100, 290))
  expect_equal(built@data[[2L]]$xend, c(290, 100))
  expect_equal(built@data[[2L]]$height, c(.8, .4))
  expect_equal(built@data[[2L]]$ymax, c(3, 3.5))
  expect_lt(built@data[[2L]]$linewidth[1], built@data[[2L]]$linewidth[2])
  expect_identical(built@layout$map_genomic_x(built@data), built@data)
  without_arcs <- ggexon_build(ggexon(.junction_test_exons(), ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type)) +
    geom_exon2(annotation_type = "all") + scale_x_ggexon_genomic(intron_factor = 10))
  expect_equal(built@layout$genomic_x_scale_registry$maps,
    without_arcs@layout$genomic_x_scale_registry$maps)

  gtable <- ggplot2::ggplot_gtable(built)
  junctions <- .junction_test_grobs(gtable,
    function(g) identical(g$name, "geom_junction"))
  expect_length(junctions, 1L)
  paths <- .junction_test_grobs(junctions[[1L]], function(g) inherits(g, "polyline"))
  labels <- .junction_test_grobs(junctions[[1L]], function(g) inherits(g, "text"))
  expect_length(paths, 1L)
  expect_length(labels, 1L)
  expect_equal(as.character(labels[[1L]]$label), c("21", "45"))
  params <- built@layout$panel_params[[1L]]
  expected_midpoint <- (195 - params$x.range[1]) / diff(params$x.range)
  expect_equal(as.numeric(labels[[1L]]$x), rep(expected_midpoint, 2))
  expect_equal(as.numeric(labels[[1L]]$y),
    (c(2.8, 3.4) - params$y.range[1]) / diff(params$y.range))
  expect_equal(grDevices::col2rgb(labels[[1L]]$gp$col),
    grDevices::col2rgb(rep("#D55E00", 2)))
  expect_equal(labels[[1L]]$gp$fontfamily, rep("mono", 2))
  expect_gt(params$y.range[2], 3.5)
  path_y <- as.numeric(paths[[1L]]$y)
  expect_gt(max(path_y[1:61]), path_y[1])
  expect_gt(max(path_y[62:122]), path_y[62])
  expect_equal(path_y[c(1, 61)], rep((2 - params$y.range[1]) / diff(params$y.range), 2))
  expect_equal(path_y[c(62, 122)], rep((3 - params$y.range[1]) / diff(params$y.range), 2))
})

test_that("junction arcs stay on annotation panels when coverage uses the same track", {
  annotation <- withr::local_tempfile(fileext = ".gff3")
  writeLines(c("##gff-version 3",
    "chr1\ttest\tmRNA\t1\t1100\t.\t+\t.\tID=tx;transcript_id=tx;transcript_name=tx",
    "chr1\ttest\texon\t1\t100\t.\t+\t.\tParent=tx;transcript_id=tx;transcript_name=tx",
    "chr1\ttest\texon\t1000\t1100\t.\t+\t.\tParent=tx;transcript_id=tx;transcript_name=tx"), annotation)
  individual <- load_annotation(SynIndividual(id = "gene", annotation_file = annotation))
  arcs <- data.frame(donor = 100, acceptor = 1000, baseline = 2,
    reads = 21, track = "gene")
  signal <- data.frame(start = 1, end = 1100, score = 7, track = "gene")
  plot <- ggexon(individual) + geom_exon2(annotation_type = "exon") +
    geom_junction(data = arcs, ggplot2::aes(x = donor, xend = acceptor,
      y = baseline, label = reads, track = track), inherit.aes = FALSE) +
    geom_coverage(data = signal, ggplot2::aes(xmin = start, xmax = end,
      coverage = score, track = track), inherit.aes = FALSE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1) +
    scale_panel_coverage("free_y") + scale_x_ggexon_genomic(intron_factor = 10)
  built <- ggexon_build(plot)
  panels <- as.data.frame(built@layout$layout)
  annotation_panel <- as.integer(panels$PANEL[panels$panel_type == "annotation"])
  coverage_panel <- as.integer(panels$PANEL[panels$panel_type == "coverage"])
  expect_length(annotation_panel, 1L)
  expect_length(coverage_panel, 1L)
  expect_identical(unique(as.integer(built@data[[2L]]$PANEL)), annotation_panel)
  expect_identical(unique(as.integer(built@data[[3L]]$PANEL)), coverage_panel)
  expect_equal(nrow(built@data[[2L]]), 1L)
  expect_equal(built@data[[2L]]$xend, 190)
  expect_silent(ggplot2::ggplot_gtable(built))
})

test_that("unlabelled junctions draw in ordinary ggplot with configurable height", {
  d <- data.frame(x = c(1, 10), xend = c(10, 1), y = 2)
  built <- ggplot2::ggplot_build(ggplot2::ggplot(d,
    ggplot2::aes(x = x, xend = xend, y = y)) + geom_junction(height = 1.2))
  expect_equal(built$data[[1L]]$ymax, c(3.2, 3.2))
  gtable <- ggplot2::ggplot_gtable(built)
  junctions <- .junction_test_grobs(gtable, function(g) identical(g$name, "geom_junction"))
  expect_length(junctions, 1L)
  expect_length(.junction_test_grobs(junctions[[1L]], function(g) inherits(g, "text")), 0L)
  expect_length(.junction_test_grobs(junctions[[1L]], function(g) inherits(g, "polyline")), 1L)
  expect_equal(built$data[[1L]]$x, c(1, 10))
  expect_equal(built$data[[1L]]$xend, c(10, 1))
})

test_that("junction heights reject invalid constants and mapped values", {
  for (height in c(0, -1, NA_real_, Inf, NaN)) {
    expect_error(geom_junction(height = height), "height.*positive.*finite")
    d <- data.frame(x = 1, xend = 2, y = 1, rise = height)
    expect_error(ggplot2::ggplot_build(ggplot2::ggplot(d,
      ggplot2::aes(x = x, xend = xend, y = y, height = rise)) + geom_junction()),
      "height.*positive.*finite")
  }
  expect_error(geom_junction(height = c(.2, .5)), "height")
  expect_error(geom_junction(label_size = 0), "label_size")
})

test_that("constant height overrides mapped rise and only nonempty labels add headroom", {
  arcs <- data.frame(donor = c(100, 1100, 100), acceptor = c(1100, 100, 1000),
    baseline = 2, reads = c("", NA, "0"), rise = c(-1, NA, Inf))
  built <- ggexon_build(.junction_test_plot(arcs, height = .4) +
    ggplot2::scale_linewidth_manual(values = stats::setNames(c(.2, .4), c("", "0"))))
  expect_equal(built@data[[2L]]$height, rep(.4, 3))
  expect_equal(built@data[[2L]]$ymax, c(2.4, 2.4, 2.5))
  tree <- .junction_test_grobs(ggplot2::ggplot_gtable(built),
    function(g) identical(g$name, "geom_junction"))[[1L]]
  labels <- .junction_test_grobs(tree, function(g) inherits(g, "text"))
  expect_length(labels, 1L)
  expect_equal(as.character(labels[[1L]]$label), "0")
})

test_that("reversing an annotation panel retains symmetric labels and upward arcs", {
  arcs <- data.frame(donor = 100, acceptor = 1100, baseline = 2, reads = 21,
    rise = .6, track = "gene")
  built <- ggexon_build(.junction_test_plot(arcs) +
    facet_genomics(ggplot2::vars(track), reverse_x = "gene", reverse_x_match_by = "track"))
  expect_identical(built@layout$panel_params[[1L]]$reverse, "x")
  tree <- .junction_test_grobs(ggplot2::ggplot_gtable(built),
    function(g) identical(g$name, "geom_junction"))[[1L]]
  line <- .junction_test_grobs(tree, function(g) inherits(g, "polyline"))[[1L]]
  label <- .junction_test_grobs(tree, function(g) inherits(g, "text"))[[1L]]
  expect_equal(as.numeric(label$x), mean(as.numeric(line$x[c(1L, 61L)])))
  expect_gt(as.numeric(line$y[31L]), as.numeric(line$y[1L]))
  expect_gt(as.numeric(line$x[1L]), as.numeric(line$x[61L]))
})

.junction_panel_plot <- function(panel = "annotation", auto = FALSE,
                                 inherited_coverage = FALSE) {
  annotation <- tempfile(fileext = ".gff3")
  on.exit(unlink(annotation), add = TRUE)
  writeLines(c("##gff-version 3",
    "chr1\ttest\tmRNA\t1\t1100\t.\t+\t.\tID=tx;transcript_id=tx;transcript_name=tx",
    "chr1\ttest\texon\t1\t100\t.\t+\t.\tParent=tx;transcript_id=tx;transcript_name=tx",
    "chr1\ttest\texon\t1000\t1100\t.\t+\t.\tParent=tx;transcript_id=tx;transcript_name=tx"), annotation)
  individual <- load_annotation(SynIndividual(id = "gene", annotation_file = annotation))
  arcs <- data.frame(donor = c(100, 1100), acceptor = c(1000, 100),
    baseline = c(9, 10), rise = c(2, 1), reads = c(21, 45), track = "gene")
  signal <- data.frame(start = c(1, 101), end = c(100, 1100),
    score = c(5, 7), track = "gene")
  global_mapping <- if (inherited_coverage) {
    ggplot2::aes(coverage = 10000, xmin = 1, xmax = 100000)
  } else ggplot2::aes()
  ggexon(individual, global_mapping) +
    geom_exon2(ggplot2::aes(xmin = xmin, xmax = xmax), annotation_type = "exon") +
    geom_coverage(data = signal, ggplot2::aes(xmin = start, xmax = end,
      coverage = score, track = track), inherit.aes = FALSE) +
    geom_junction(data = arcs, ggplot2::aes(x = donor, xend = acceptor,
      y = baseline, height = rise, label = reads, track = track),
      inherit.aes = inherited_coverage, panel = panel) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1) +
    scale_panel_coverage("free_y", auto = auto) +
    scale_x_ggexon_genomic(intron_factor = 10)
}

test_that("panel selection routes junctions without changing signal or genomic mapping", {
  annotation_plot <- .junction_panel_plot("annotation")
  coverage_plot <- .junction_panel_plot("coverage")
  annotation <- ggexon_build(annotation_plot)
  coverage <- ggexon_build(coverage_plot)
  for (built in list(annotation, coverage)) {
    panels <- as.data.frame(built@layout$layout)
    annotation_id <- as.integer(panels$PANEL[panels$panel_type == "annotation"])
    coverage_id <- as.integer(panels$PANEL[panels$panel_type == "coverage"])
    expected <- if (identical(built, annotation)) annotation_id else coverage_id
    expect_identical(unique(as.integer(built@data[[3L]]$PANEL)), expected)
    expect_s3_class(built@plot@layers[[3L]]$geom, "GeomJunction")
    expect_equal(built@data[[3L]]$x, c(100, 290))
    expect_equal(built@data[[3L]]$xend, c(190, 100))
    expect_equal(built@data[[3L]]$y, c(9, 10))
    expect_equal(built@data[[3L]]$height, c(2, 1))
    expect_equal(built@data[[3L]]$ymax, c(11.5, 11.25))
    target <- as.integer(built@data[[3L]]$PANEL[[1L]])
    expect_gt(built@layout$panel_params[[target]]$y.range[[2L]], 11.5)
    expect_identical(built@layout$map_genomic_x(built@data), built@data)
    expect_silent(ggplot2::ggplot_gtable(built))
  }
  expect_equal(coverage@data[[2L]], annotation@data[[2L]])
  expect_equal(coverage@layout$genomic_x_scale_registry$maps,
    annotation@layout$genomic_x_scale_registry$maps)
  signal <- coverage@data[[2L]]
  expect_equal(sum(ggexon:::.coverage_span_weights(signal)), 1100)
  expect_equal(unique(signal$coverage), c(5, 7))
  expect_equal(ggexon_build(coverage_plot)@data, coverage@data)
})

test_that("coverage-target junctions do not contaminate automatic signal limits", {
  annotation <- ggexon_build(.junction_panel_plot("annotation", auto = TRUE))
  coverage <- ggexon_build(.junction_panel_plot("coverage", auto = TRUE,
    inherited_coverage = TRUE))
  panels <- as.data.frame(coverage@layout$layout)
  coverage_id <- as.integer(panels$PANEL[panels$panel_type == "coverage"])
  expect_equal(coverage@data[[3L]]$coverage, c(10000, 10000))
  expect_equal(coverage@data[[3L]]$ymax, c(11.5, 11.25))
  expect_false(".ggexon_panel_role" %in% names(coverage@data[[3L]]))
  expect_equal(coverage@layout$panel_params[[coverage_id]]$y.range, c(0, 7 / .85))
  expect_equal(coverage@layout$panel_params[[coverage_id]]$y.range,
    annotation@layout$panel_params[[coverage_id]]$y.range)
  expect_equal(coverage@data[[2L]], annotation@data[[2L]])
})

test_that("coverage-target junctions alone do not activate legacy coverage bands", {
  arcs <- data.frame(donor = 100, acceptor = 1000, baseline = 9,
    reads = 21, rise = 2)
  built <- ggexon_build(.junction_test_plot(arcs, panel = "coverage"))
  expect_equal(built@data[[1L]]$ymin, c(1, 1))
  expect_equal(built@data[[2L]]$y, 9)
  expect_equal(built@data[[2L]]$ymax, 11.5)
  expect_gt(built@layout$panel_params[[1L]]$y.range[[2L]], 11.5)
  expect_length(built@layout$ggexon_composite_coverage_panels, 0L)
  expect_silent(ggplot2::ggplot_gtable(built))
  ordinary <- ggplot2::ggplot_build(ggplot2::ggplot(arcs,
    ggplot2::aes(x = donor, xend = acceptor, y = baseline)) +
      geom_junction(height = 2, panel = "coverage"))
  expect_equal(ordinary$data[[1L]]$ymax, 11)
})

test_that("junction panel rejects unsupported selections", {
  expect_error(geom_junction(panel = "bigwig"), "panel|arg")
  expect_error(geom_junction(panel = character()), "panel|arg")
  expect_error(geom_junction(panel = NA_character_), "panel|arg")
})


test_that("junction endpoint heights follow the trained genomic map and midpoint labels", {
  exons <- .junction_test_exons()
  arcs <- data.frame(donor = c(100, 1100), acceptor = c(1100, 100),
    start = c(2, 4), finish = c(4, 2), track = "gene", reads = c(21, 45))
  plot <- ggexon(exons, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
    transcripts = transcripts, strand = strand, track = track, type = type)) +
    geom_exon2(annotation_type = "all") +
    geom_junction(data = arcs, ggplot2::aes(x = donor, xend = acceptor,
      y = start, yend = finish, track = track, label = reads),
      height = 1.125, inherit.aes = FALSE) +
    scale_x_ggexon_genomic(intron_factor = 10)

  for (reverse in c(FALSE, TRUE)) {
    current <- if (reverse) plot + facet_genomics(ggplot2::vars(track),
      reverse_x = "gene", reverse_x_match_by = "track") else plot
    built <- ggexon_build(current)
    junction_data <- built@data[[2L]]
    expect_equal(junction_data$x, c(100, 290))
    expect_equal(junction_data$xend, c(290, 100))
    expect_equal(junction_data$yend, c(4, 2))
    expect_equal(junction_data$ymin, c(2, 2))
    expect_equal(junction_data$ymax, c(4.8125, 4.8125))
    expect_identical(built@layout$map_genomic_x(built@data), built@data)
    tree <- .junction_test_grobs(ggplot2::ggplot_gtable(built),
      function(g) identical(g$name, "geom_junction"))[[1L]]
    line <- .junction_test_grobs(tree, function(g) inherits(g, "polyline"))[[1L]]
    label <- .junction_test_grobs(tree, function(g) inherits(g, "text"))[[1L]]
    params <- built@layout$panel_params[[1L]]
    y_native <- function(y) (y - params$y.range[1L]) / diff(params$y.range)
    expect_equal(as.numeric(line$y[c(1L, 61L, 62L, 122L)]),
      y_native(c(2, 4, 4, 2)))
    expect_equal(as.numeric(line$y[c(31L, 92L)]), y_native(c(4.125, 4.125)))
    expect_equal(as.numeric(line$y[c(46L, 77L)]), y_native(c(4.53125, 4.53125)))
    expect_equal(as.numeric(label$y), y_native(c(4.125, 4.125)))
    expect_equal(as.numeric(label$x), c(mean(as.numeric(line$x[c(1L, 61L)])),
      mean(as.numeric(line$x[c(62L, 122L)]))))
    expect_gt(params$y.range[2L], 4.8125)
  }
})

test_that("constant endpoint heights override mappings and omitted heights default to y", {
  arcs <- data.frame(x = c(1, 10), xend = c(10, 1), y = c(2, 4),
    finish = c(5, 1))
  base <- ggplot2::ggplot(arcs, ggplot2::aes(x = x, xend = xend, y = y))
  omitted <- ggplot2::ggplot_build(base + geom_junction(height = 1))
  explicit <- ggplot2::ggplot_build(base + geom_junction(height = 1,
    ggplot2::aes(yend = y)))
  expect_equal(omitted$data[[1L]]$yend, c(2, 4))
  expect_equal(omitted$data[[1L]],
    explicit$data[[1L]][names(omitted$data[[1L]])])
  constant <- ggplot2::ggplot_build(base + geom_junction(height = 1,
    ggplot2::aes(yend = finish), yend = 3))
  expect_equal(constant$data[[1L]]$yend, c(3, 3))
  tree <- .junction_test_grobs(ggplot2::ggplot_gtable(constant),
    function(g) identical(g$name, "geom_junction"))[[1L]]
  line <- .junction_test_grobs(tree, function(g) inherits(g, "polyline"))[[1L]]
  params <- constant$layout$panel_params[[1L]]
  expect_equal(as.numeric(line$y[c(61L, 122L)]),
    rep((3 - params$y.range[1L]) / diff(params$y.range), 2L))
})

test_that("junction bounds include endpoint extrema when the rise is small", {
  arcs <- data.frame(x = c(1, 10), xend = c(10, 1), y = c(2, 12),
    finish = c(12, 2))
  built <- ggplot2::ggplot_build(ggplot2::ggplot(arcs,
    ggplot2::aes(x = x, xend = xend, y = y, yend = finish)) +
      geom_junction(height = 1))
  expect_equal(built$data[[1L]]$ymin, c(2, 2))
  expect_gt(built$data[[1L]]$ymax[1L], 12)
  expect_equal(built$data[[1L]]$ymax[1L], built$data[[1L]]$ymax[2L])
  expect_gt(built$layout$panel_params[[1L]]$y.range[2L], 12)
})

test_that("missing junction endpoint heights use ordinary missing-row handling", {
  arcs <- data.frame(x = c(1, 2), xend = c(10, 11), y = 2,
    finish = c(4, NA_real_))
  plot <- ggplot2::ggplot(arcs,
    ggplot2::aes(x = x, xend = xend, y = y, yend = finish))
  built <- ggplot2::ggplot_build(plot + geom_junction())
  expect_warning(gtable <- ggplot2::ggplot_gtable(built), "Removed 1 row")
  tree <- .junction_test_grobs(gtable,
    function(g) identical(g$name, "geom_junction"))[[1L]]
  line <- .junction_test_grobs(tree, function(g) inherits(g, "polyline"))[[1L]]
  expect_length(line$x, 61L)
  expect_silent(ggplot2::ggplot_gtable(ggplot2::ggplot_build(
    plot + geom_junction(na.rm = TRUE))))
})

test_that("junction endpoint heights reject infinite values", {
  for (finish in c(-Inf, Inf)) {
    arcs <- data.frame(x = 1, xend = 10, y = 2, finish = finish)
    expect_error(ggplot2::ggplot_build(ggplot2::ggplot(arcs,
      ggplot2::aes(x = x, xend = xend, y = y, yend = finish)) + geom_junction()),
      "yend.*finite")
  }
})

test_that("junction labels honor mapped and constant horizontal justification", {
  arcs <- data.frame(x = 1, xend = 10, y = 2, reads = 9,
    justification = 1.5)
  plot <- ggplot2::ggplot(arcs,
    ggplot2::aes(x = x, xend = xend, y = y, label = reads))
  label_hjust <- function(layer) {
    tree <- .junction_test_grobs(ggplot2::ggplotGrob(plot + layer),
      function(g) identical(g$name, "geom_junction"))[[1L]]
    .junction_test_grobs(tree, function(g) inherits(g, "text"))[[1L]]$hjust
  }
  expect_equal(c(label_hjust(geom_junction()),
    label_hjust(geom_junction(ggplot2::aes(hjust = justification)))),
    c(.5, 1.5))
  expect_equal(label_hjust(geom_junction(hjust = 1.5)), 1.5)
})
