coverage_compression_sources <- function() {
  data.frame(PANEL = 2L, xmin = c(1, 1000), xmax = c(100, 1100),
    track = "human", transcripts = "tx", type = "exon", strand = "+")
}

coverage_compression_signal <- function(start = 90, end = 1010, score = 7,
                                        panel = 1L) {
  GeomCoverage$setup_data(data.frame(PANEL = panel, xmin = start, xmax = end,
    interval_start = start, interval_end = end, coverage = score,
    track = "sample"), list())
}

coverage_compression_layout <- function() {
  list(layout = data.frame(PANEL = 1:2, panel_type = c("coverage", "annotation"),
    track = c("sample", "human"), x_source_panel = c(2L, NA_integer_)))
}

test_that("coverage borrows annotation compression and splits actual rectangle edges", {
  signal <- coverage_compression_signal()
  original <- signal
  result <- apply_ggexon_genomic_x_scale(
    list(signal, coverage_compression_sources()),
    scale_x_ggexon_genomic(intron_factor = 10), coverage_compression_layout(),
    layers = list(geom_coverage(), geom_exon2()))
  drawn <- result$data[[1L]]

  expect_equal(drawn$xmin, c(89.5, 100, 190))
  expect_equal(drawn$xmax, c(100, 190, 200.5))
  expect_equal(drawn$coverage, rep(7, 3))
  expect_equal(drawn$ymax, rep(7, 3))
  expect_equal(drawn$ymin, rep(0, 3))
  expect_equal(drawn$interval_start, rep(90, 3))
  expect_equal(drawn$interval_end, rep(1010, 3))
  expect_equal(drawn$genomic_xmin, rep(90, 3))
  expect_equal(drawn$genomic_xmax, rep(1010, 3))
  expect_equal(sum(.coverage_span_weights(drawn)), 921)
  expect_equal(result$transforms[["1"]]$slope, result$transforms[["2"]]$slope)
  expect_identical(signal, original)
  expect_equal(transform_ggexon_genomic_x_layer(drawn, result$transforms), drawn)
})

test_that("coverage subdivision preserves genomic weighting and adjacent intervals", {
  signal <- coverage_compression_signal(c(90, 1011, 1012), c(1010, 1011, 1012),
    c(1, 10, 100))
  result <- apply_ggexon_genomic_x_scale(
    list(signal, coverage_compression_sources()),
    scale_x_ggexon_genomic(intron_factor = 10), coverage_compression_layout(),
    layers = list(geom_coverage(), geom_exon2()))
  drawn <- result$data[[1L]]
  expect_equal(tail(drawn$xmin, 2), c(200.5, 201.5))
  expect_equal(tail(drawn$xmax, 2), c(201.5, 202.5))
  expect_equal(.weighted_coverage_quantile(drawn$coverage, .coverage_span_weights(drawn)),
    .weighted_coverage_quantile(signal$coverage, .coverage_span_weights(signal)))
  expect_equal(sum(.coverage_span_weights(drawn)), 923)
})

test_that("coverage does not supply exon boundaries even with inherited annotation fields", {
  signal <- coverage_compression_signal()
  signal$strand <- "+"
  signal$type <- "exon"
  expect_equal(nrow(ggexon_genomic_x_source_rows(signal)), 0L)
})

test_that("uncompressed coverage and coverage-only plots retain their interval geometry", {
  signal <- coverage_compression_signal(c(0, 100, 200, 1200), c(0, 100, 300, 1200),
    c(0, 1, 2, 3))
  identity <- apply_ggexon_genomic_x_scale(
    list(signal, coverage_compression_sources()),
    scale_x_ggexon_genomic(intron_factor = 1), coverage_compression_layout(),
    layers = list(geom_coverage(), geom_exon2()))
  expect_equal(identity$data[[1L]]$xmin, signal$xmin)
  expect_equal(identity$data[[1L]]$xmax, signal$xmax)
  expect_equal(identity$data[[1L]]$coverage, signal$coverage)
  standalone <- apply_ggexon_genomic_x_scale(list(signal),
    scale_x_ggexon_genomic(intron_factor = 10), coverage_compression_layout(),
    layers = list(geom_coverage()))
  expect_equal(standalone$data[[1L]], signal)
})

test_that("ordinary composite coverage follows the panel map without losing base padding", {
  signal <- coverage_compression_signal(panel = 2L)
  plot <- ggexon() +
    geom_coverage(data = signal, ggplot2::aes(xmin = interval_start,
      xmax = interval_end, coverage = coverage, track = track), inherit.aes = FALSE) +
    geom_exon2(data = transform(coverage_compression_sources(), track = "sample", ymin = 1),
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, track = track,
        transcripts = transcripts, type = type, strand = strand),
      annotation_type = "all", inherit.aes = FALSE) +
    scale_x_ggexon_genomic(intron_factor = 10)
  built <- ggexon_build(plot)
  expect_equal(built@data[[1L]]$xmin, c(89.5, 100, 190))
  expect_equal(built@data[[1L]]$xmax, c(100, 190, 200.5))
  expect_equal(built@data[[1L]], ggexon_build(plot)@data[[1L]])
  expect_silent(ggplot2::ggplotGrob(plot))
})

coverage_compression_plot <- function(coverage, annotation, ...) {
  species <- SynSpecies(name = "compression")
  gtf <- tempfile(fileext = ".gtf")
  writeLines(c(
    'chr1\ttest\tgene\t1\t1100\t.\t+\t.\tgene_id "g";',
    'chr1\ttest\ttranscript\t1\t1100\t.\t+\t.\tgene_id "g"; transcript_id "tx";',
    'chr1\ttest\texon\t1\t100\t.\t+\t.\tgene_id "g"; transcript_id "tx";',
    'chr1\ttest\texon\t1000\t1100\t.\t+\t.\tgene_id "g"; transcript_id "tx";'), gtf)
  for (track in unique(c(coverage$track, annotation$track))) {
    individual <- SynIndividual(id = track, annotation_file = gtf, annotation_format = "gtf")
    species <- add_individual(species, individual)
  }
  ggexon(species) +
    geom_coverage(data = coverage, ggplot2::aes(xmin = xmin, xmax = xmax,
      coverage = coverage, track = track), inherit.aes = FALSE) +
    geom_exon2(data = annotation,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 1, track = track,
        transcripts = transcripts, type = type, strand = strand),
      annotation_type = "all", inherit.aes = FALSE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ...)
}

test_that("broadcast coverage shares annotation coordinates, ticks, windows and raw y limits", {
  coverage <- data.frame(track = rep(c("sample_a", "sample_b"), each = 3),
    xmin = rep(c(90, 1011, 1012), 2), xmax = rep(c(1010, 1011, 1012), 2),
    coverage = rep(c(1, 10, 100), 2))
  plot <- coverage_compression_plot(coverage, coverage_compression_sources(),
    xlim = c(0, 1200), xlim_chr = "chr1") + scale_panel_coverage("free_y", auto = TRUE)
  raw <- ggexon_build(plot)
  compressed <- ggexon_build(plot + scale_x_ggexon_genomic(intron_factor = 10,
    breaks = c(100, 1000)))
  layout <- as.data.frame(compressed@layout$layout)
  sources <- as.integer(layout$PANEL[layout$panel_type == "annotation"])
  panels <- as.integer(layout$PANEL[layout$panel_type == "coverage"])
  expect_length(panels, 2L)
  expect_equal(layout$x_source_panel[layout$panel_type == "coverage"], rep(sources, 2))
  for (panel in panels) {
    hit <- as.integer(compressed@data[[1L]]$PANEL) == panel
    expect_equal(compressed@data[[1L]]$xmin[hit], c(89.5, 100, 190, 200.5, 201.5))
    expect_equal(compressed@layout$panel_params[[panel]]$y.range,
      raw@layout$panel_params[[panel]]$y.range)
    expect_equal(compressed@layout$panel_params[[panel]]$x$get_breaks(), c(100, 190))
    scale <- compressed@layout$panel_scales_x[[layout$SCALE_X[layout$PANEL == panel]]]
    expect_equal(scale$range$range, c(0, 390))
  }
  expect_silent(ggplot2::ggplotGrob(plot + scale_x_ggexon_genomic(intron_factor = 10)))
})

test_that("coverage follows species selection through its resolved annotation source", {
  annotation <- rbind(coverage_compression_sources(),
    transform(coverage_compression_sources(), track = "chimp", transcripts = "chimp_tx"))
  coverage <- data.frame(track = c("human", "chimp"), xmin = 90, xmax = 1010,
    coverage = c(7, 0))
  plot <- coverage_compression_plot(coverage, annotation) +
    scale_x_ggexon_genomic(intron_factor = 10, species = "human", match_by = "track")
  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  for (track in c("human", "chimp")) {
    panel <- layout$PANEL[layout$track == track & layout$panel_type == "coverage"]
    signal <- built@data[[1L]][as.integer(built@data[[1L]]$PANEL) == panel, ]
    expect_equal(signal$xmin, if (track == "human") c(89.5, 100, 190) else 89.5)
    expect_equal(signal$xmax, if (track == "human") c(100, 190, 200.5) else 1010.5)
    expect_equal(signal$coverage, rep(if (track == "human") 7 else 0, nrow(signal)))
  }
})

test_that("coverage compression handles intronic, single-base and flanking signals", {
  signal <- coverage_compression_signal(c(0, 100, 200, 1200), c(0, 100, 300, 1200),
    c(0, 1, 2, 3))
  result <- apply_ggexon_genomic_x_scale(
    list(signal, coverage_compression_sources()),
    scale_x_ggexon_genomic(intron_factor = 10), coverage_compression_layout(),
    layers = list(geom_coverage(), geom_exon2()))
  drawn <- result$data[[1L]]
  expect_equal(drawn$xmin, c(-0.5, 99.5, 100, 109.95, 389.5))
  expect_equal(drawn$xmax, c(0.5, 100, 100.05, 120.05, 390.5))
  expect_equal(drawn$coverage, c(0, 1, 1, 2, 3))
  expect_equal(sum(.coverage_span_weights(drawn)), 104)
})

test_that("genomic mapping retains boundaries, extrapolation and non-finite coordinates", {
  tr <- build_ggexon_genomic_x_transform(
    ggexon_genomic_x_source_rows(coverage_compression_sources()), intron_factor = 10)
  raw <- c(-5, 1, 100, 550, 1000, 1100, 1200, NA_real_, Inf, -Inf, NaN)
  expected <- c(-5, 1, 100, 145, 190, 290, 390, NA_real_, Inf, -Inf, NaN)
  expect_equal(ggexon_genomic_to_plot_x(raw, tr), expected)
  expect_equal(ggexon_plot_to_genomic_x(expected, tr), raw)
  expect_identical(ggexon_genomic_to_plot_x(raw, NULL), raw)
})
