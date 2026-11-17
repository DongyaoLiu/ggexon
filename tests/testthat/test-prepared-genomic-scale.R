prepared_scale_individual <- function(id = "sample") {
  x <- prepared_genomic_fixture(id)
  x <- project_mutations_to_genome(x, annotation = "muts", transcripts = "tx1",
    store = TRUE, name = "projected")
  prepare_genomic_layout(x, annotation = "projected", name = "locus",
    transcripts = "tx1", intron_factor = 3, head_spread = 4,
    mutation_y_by = "sample_count", mutation_y_trans = "log10",
    mutation_y_range = log10(c(2, 11)), track_ymin = 2, track_ymax = 2.8)
}

test_that("stored genomic scale names are validated before building", {
  expect_error(scale_x_ggexon_genomic(genomic_layout = ""),
    "genomic_layout.*non-empty")
  expect_error(scale_x_ggexon_genomic(genomic_layout = c("one", "two")),
    "genomic_layout.*one")
  expect_error(scale_x_ggexon_genomic(genomic_layout = NA_character_),
    "genomic_layout.*non-empty")
})

test_that("stored genomic scales require a Syn source even without exon layers", {
  expect_error(train_ggexon_genomic_x_scale(
    list(data.frame(PANEL = 1L, x = 100)), list(ggplot2::geom_point()),
    scale_x_ggexon_genomic(genomic_layout = "locus"),
    list(layout = data.frame(PANEL = 1L))
  ), "genomic_layout.*Syn")
})

test_that("stored maps align ordinary points and labels without exon providers", {
  x <- prepared_scale_individual()
  original <- serialize(x, NULL)
  anchors <- data.frame(x = c(104.5, 199.5, 208.5), y = 3,
    label = c("first end", "second start", "second end"), track = "sample")
  plot <- ggexon(x) +
    ggplot2::geom_point(data = anchors, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(data = anchors, ggplot2::aes(x = x, y = y, label = label)) +
    facet_genomics(ggplot2::vars(track), scales = "free_x") +
    scale_x_ggexon_genomic(genomic_layout = "locus",
      breaks = c(104.5, 199.5))

  built <- ggexon_build(plot)
  expected <- c(104.5, 104.5 + 95 / 3, 113.5 + 95 / 3)
  expect_equal(built@data[[1L]]$x, expected)
  expect_equal(built@data[[2L]]$x, expected)
  expect_equal(built@layout$panel_params[[1L]]$x$get_breaks(), expected[1:2],
    ignore_attr = TRUE)
  piecewise_plot <- plot + scale_x_ggexon_genomic(genomic_layout = "locus",
    guide = guide_x_ggexon_piecewise())
  piecewise <- ggexon_build(piecewise_plot)
  expect_equal(piecewise@data, built@data)
  axis <- piecewise@layout$genomic_x_axis_data
  expect_equal(axis$genomic_width, c(5, 95))
  expect_equal(axis$plot_width, c(5, 95 / 3))
  expect_equal(axis$PANEL, c(1L, 1L))
  expect_s3_class(ggplot2::ggplot_gtable(piecewise), "gtable")

  rebuilt <- ggexon_build(plot)
  expect_equal(rebuilt@data, built@data)
  expect_equal(ggexon_build(piecewise_plot)@layout$genomic_x_axis_data, axis)
  expect_identical(built@layout$map_genomic_x(built@data), built@data)
  expect_identical(serialize(x, NULL), original)
})

test_that("stored maps bind to current source panels and dependent coverage", {
  x <- prepared_scale_individual()
  layout <- list(facet_params = list(plot_data = x), layout = data.frame(
    PANEL = 1:3, panel_type = c("coverage", "annotation", "coverage"),
    track = c("signal", "sample", "signal2"),
    x_source_panel = c(2L, NA_integer_, 2L)))
  input <- list(data.frame(PANEL = c(1L, 3L), x = c(199.5, 152)))
  spec <- scale_x_ggexon_genomic(genomic_layout = "locus",
    guide = guide_x_ggexon_piecewise())
  registry <- train_ggexon_genomic_x_scale(input,
    list(ggplot2::geom_point()), spec, layout)

  expect_named(registry$maps, "2")
  expect_identical(registry$panel_map[c("1", "2", "3")],
    c(`1` = 2L, `2` = 2L, `3` = 2L))
  expect_equal(map_ggexon_genomic_x_scale(input, registry)[[1L]]$x,
    c(104.5 + 95 / 3, 104.5 + 47.5 / 3))
  expect_equal(registry$axis_data$PANEL, c(2L, 2L))

  rebound_layout <- list(facet_params = list(plot_data = x),
    layout = data.frame(PANEL = 7L, track = "sample"))
  rebound <- train_ggexon_genomic_x_scale(
    list(data.frame(PANEL = 7L, x = 199.5)), list(ggplot2::geom_point()),
    spec, rebound_layout)
  expect_named(rebound$maps, "7")
  expect_equal(rebound$axis_data$PANEL, c(7L, 7L))
  expect_false(any(c("PANEL", "SCALE_X") %in% names(.get_genomic_layout(x, "locus")$map)))
  expect_false(any(c("PANEL", "SCALE_X") %in% names(.get_genomic_layout(x, "locus")$source_rows)))
})

test_that("stored scales reject missing layouts and conflicting selectors", {
  x <- prepared_scale_individual()
  input <- list(data.frame(PANEL = 1L, x = 199.5))
  layers <- list(ggplot2::geom_point())
  layout <- list(facet_params = list(plot_data = x),
    layout = data.frame(PANEL = 1L, track = "sample"))
  expect_error(train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "missing"), layout),
    "[Uu]nknown|[Nn]o.*layout|not found")
  expect_error(train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "locus", intron_factor = 4), layout),
    "intron_factor.*conflict|intron_factor.*prepared")
  expect_no_error(train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "locus", intron_factor = 3,
      species = "sample", match_by = "track"), layout))
  expect_error(train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "locus", species = "other"), layout),
    "species")

  layer <- geom_exon2()
  layer$geom_params$genomic_layout <- "other"
  expect_error(train_ggexon_genomic_x_scale(input, list(layer),
    scale_x_ggexon_genomic(genomic_layout = "locus"), layout),
    "genomic_layout.*conflict|genomic_layout.*match")
})

test_that("stored scale binding rejects incompatible identity and chromosome", {
  x <- prepared_scale_individual()
  input <- list(data.frame(PANEL = 1L, x = 199.5))
  layers <- list(ggplot2::geom_point())
  spec <- scale_x_ggexon_genomic(genomic_layout = "locus")
  layout <- list(facet_params = list(plot_data = x),
    layout = data.frame(PANEL = 1L, track = "other"))
  expect_error(train_ggexon_genomic_x_scale(input, layers, spec, layout),
    "identity|source|track")

  layout$layout$track <- "sample"
  layout$layout$xlim_chr <- "chr2"
  expect_error(train_ggexon_genomic_x_scale(input, layers, spec, layout),
    "chromosome|chr")
  layout$layout$xlim_chr <- "chr1"
  input[[1L]]$seqnames <- "chr2"
  expect_error(train_ggexon_genomic_x_scale(input, layers, spec, layout),
    "chromosome|chr")

  layout$layout <- data.frame(PANEL = 1:2, track = c("sample", "signal"),
    panel_type = c("annotation", "coverage"), x_source_panel = c(NA_integer_, 1L),
    xlim_chr = c("chr1", "chr2"))
  expect_error(train_ggexon_genomic_x_scale(
    list(data.frame(PANEL = 2L, x = 100)), layers, spec, layout), "chromosome|chr")
})

test_that("layer-owned Syn sources must identify one unambiguous object", {
  x <- prepared_scale_individual()
  y <- prepared_scale_individual("other")
  input <- list(data.frame(PANEL = 1L, x = 199.5))
  layout <- list(layout = data.frame(PANEL = 1L, track = "sample"))
  spec <- scale_x_ggexon_genomic(genomic_layout = "locus")
  layer_x <- geom_exon2()
  layer_x$data <- x
  layer_y <- geom_exon2()
  layer_y$data <- y
  expect_no_error(train_ggexon_genomic_x_scale(input,
    list(layer_x), spec, layout))
  expect_error(train_ggexon_genomic_x_scale(rep(input, 2L),
    list(layer_x, layer_y), spec, layout),
    "[Aa]mbiguous|one.*Syn")
})

test_that("comparative prepared maps use individual identity behind track aliases", {
  x <- prepared_scale_individual()
  y <- prepared_scale_individual("other")
  species <- add_individual(add_individual(SynSpecies(name = "comparison"), x), y)
  input <- list(data.frame(PANEL = c(4L, 9L), x = c(199.5, 199.5)))
  layers <- list(ggplot2::geom_point())
  layout <- list(facet_params = list(plot_data = species), layout = data.frame(
    PANEL = c(4L, 9L), track = c("left-locus", "right-locus"),
    individual = c("sample", "other"), seqnames = "chr1"))
  registry <- train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "locus"), layout)
  expect_named(registry$maps, c("4", "9"))
  expect_equal(map_ggexon_genomic_x_scale(input, registry)[[1L]]$x,
    rep(104.5 + 95 / 3, 2L))

  selected <- train_ggexon_genomic_x_scale(input, layers,
    scale_x_ggexon_genomic(genomic_layout = "locus", species = "sample"), layout)
  expect_named(selected$maps, "4")
  expect_equal(map_ggexon_genomic_x_scale(input, selected)[[1L]]$x,
    c(104.5 + 95 / 3, 199.5))
})
