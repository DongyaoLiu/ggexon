genomic_training_exons <- function(panel = 1L) {
  data.frame(
    PANEL = panel, xmin = c(1, 1000), xmax = c(100, 1100),
    ymin = 1, transcripts = "tx", strand = "+", track = "gene",
    type = "exon", stringsAsFactors = FALSE
  )
}

genomic_training_plot <- function() {
  exons <- genomic_training_exons()
  whole_gene <- transform(exons[1L, ], xmin = 1, xmax = 1100, ymax = 2)
  ggexon(exons, ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type
  )) +
    geom_exon2(annotation_type = "all") +
    ggplot2::geom_rect(data = whole_gene, ggplot2::aes(ymax = ymax)) +
    ggplot2::geom_text(data = data.frame(x = 1000, y = 3, label = "second exon"),
      ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    ggplot2::geom_point(data = data.frame(x = 550, y = 3),
      ggplot2::aes(x = x, y = y), inherit.aes = FALSE) +
    ggplot2::geom_segment(data = data.frame(x = 100, xend = 1100, y = 3, yend = 3),
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE) +
    scale_x_ggexon_genomic(intron_factor = 10, breaks = c(100, 1000))
}

test_that("training creates a reusable genomic scale before any mapping", {
  exons <- genomic_training_exons()
  registry <- train_ggexon_genomic_x_scale(
    list(exons), list(geom_exon2()),
    scale_x_ggexon_genomic(intron_factor = 10)
  )

  expect_s3_class(registry, "ggexon_trained_genomic_x_scale")
  expect_named(registry$maps, "1")
  expect_identical(registry$panel_map, c(`1` = 1L))
  expect_equal(map_ggexon_genomic_x_scale(list(exons), registry)[[1L]]$xmin,
    c(1, 190))
})

test_that("ordinary full-gene rectangles cannot erase exon compression", {
  built <- ggexon_build(genomic_training_plot())

  expect_equal(built@data[[1L]]$xmin, c(1, 190))
  expect_equal(built@data[[1L]]$xmax, c(100, 290))
  expect_equal(built@data[[2L]]$xmin, 1)
  expect_equal(built@data[[2L]]$xmax, 290)
  expect_equal(built@data[[3L]]$x, 190)
  expect_equal(built@data[[4L]]$x, 145)
  expect_equal(built@data[[5L]]$x, 100)
  expect_equal(built@data[[5L]]$xend, 290)
  expect_equal(unname(built@layout$panel_params[[1L]]$x$get_breaks()), c(100, 190))
})

test_that("gene-box metadata cannot replace its actual plotted coordinates", {
  exons <- genomic_training_exons()
  boxes <- data.frame(x = c(1000, 550), y = 3, strand = "+",
    genomic_x = NA_real_, genomic_xmin = 1, genomic_xmax = 1100)
  plot <- ggexon(exons, ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type
  )) +
    geom_exon2(annotation_type = "all") +
    geom_genebox(data = boxes, box_size = 20) +
    scale_x_ggexon_genomic(intron_factor = 10)
  built <- ggexon_build(plot)
  mapped <- built@data[[2L]]

  expect_equal(mapped$x, c(190, 145))
  expect_equal(mapped$xmin, c(189, 144))
  expect_equal(mapped$xmax, c(200, 146))
  expect_equal(mapped$genomic_xmin, c(1, 1))
  expect_equal(mapped$genomic_xmax, c(1100, 1100))
  expect_true(all(is.na(mapped$genomic_x)))
  expect_equal(built@layout$map_genomic_x(built@data), built@data)
})

test_that("whole-gene arrow bounds consume the exon-trained map", {
  exons <- genomic_training_exons()
  gene <- transform(exons[1L, ], xmin = 1, xmax = 1100)
  plot <- ggexon(exons, ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type
  )) +
    geom_exon2(annotation_type = "all") +
    geom_gene(data = gene, proportion_trim3 = 0) +
    scale_x_ggexon_genomic(intron_factor = 10)
  built <- ggexon_build(plot)

  expect_equal(built@data[[1L]]$xmin, c(1, 190))
  expect_equal(built@data[[2L]]$xmin, 1)
  expect_equal(built@data[[2L]]$xmax, 290)
  expect_equal(built@data[[2L]]$Xmin, 1)
  expect_equal(built@data[[2L]]$Xmax, 290)
})

test_that("all declared exon layers train one union independent of layer order", {
  exons <- genomic_training_exons()
  overlapping_exon <- transform(exons[1L, ], xmin = 50, xmax = 150,
    transcripts = "overlapping")
  whole_gene <- transform(exons[1L, ], xmin = 1, xmax = 1100)
  data <- list(exons, overlapping_exon, whole_gene)
  layers <- list(geom_exon(), geom_exon2(), ggplot2::geom_rect())
  spec <- scale_x_ggexon_genomic(intron_factor = 10)
  forward <- train_ggexon_genomic_x_scale(data, layers, spec)
  order <- c(3L, 2L, 1L)
  reordered <- train_ggexon_genomic_x_scale(data[order], layers[order], spec)

  expect_equal(forward$maps, reordered$maps)
  expect_identical(forward$panel_map, reordered$panel_map)
  mapped <- map_ggexon_genomic_x_scale(data, forward)
  expect_equal(mapped[[1L]]$xmin, c(1, 235))
  expect_equal(mapped[[1L]]$xmax, c(100, 335))
  expect_equal(mapped[[2L]]$xmin, 50)
  expect_equal(mapped[[2L]]$xmax, 150)
  expect_equal(mapped[[3L]]$xmax, 335)
})

test_that("mapping reuses immutable training even when consumers extend the union", {
  exons <- genomic_training_exons()
  registry <- train_ggexon_genomic_x_scale(
    list(exons), list(geom_exon2()),
    scale_x_ggexon_genomic(intron_factor = 10)
  )
  trained <- serialize(registry, NULL)
  consumer <- transform(exons[1L, ], xmin = -100, xmax = 5000,
    x = 550, xend = 1000, xintercept = 1100, xlower = 100, xupper = 1000)
  input <- list(exons, consumer)
  original <- input

  mapped <- map_ggexon_genomic_x_scale(input, registry)

  expect_identical(input, original)
  expect_identical(serialize(registry, NULL), trained)
  expect_equal(mapped[[1L]]$xmin, c(1, 190))
  expect_equal(mapped[[2L]]$xmin, -100)
  expect_equal(mapped[[2L]]$xmax, 4190)
  expect_equal(mapped[[2L]]$x, 145)
  expect_equal(mapped[[2L]]$xend, 190)
  expect_equal(mapped[[2L]]$xintercept, 290)
  expect_equal(mapped[[2L]]$xlower, 100)
  expect_equal(mapped[[2L]]$xupper, 190)
  expect_identical(map_ggexon_genomic_x_scale(mapped, registry), mapped)
  expect_identical(serialize(registry, NULL), trained)
})

test_that("coverage bindings refer to a single canonical annotation map", {
  exons <- genomic_training_exons(panel = 2L)
  signal <- GeomCoverage$setup_data(data.frame(
    PANEL = 1L, xmin = 90, xmax = 1010, interval_start = 90,
    interval_end = 1010, coverage = 7, track = "sample"
  ), list())
  points <- data.frame(PANEL = c(1L, 3L), x = c(1000, 550))
  layout <- list(layout = data.frame(
    PANEL = 1:3, panel_type = c("coverage", "annotation", "coverage"),
    track = c("sample", "gene", "sample2"),
    x_source_panel = c(2L, NA_integer_, 2L)
  ))
  registry <- train_ggexon_genomic_x_scale(
    list(signal, exons, points),
    list(geom_coverage(), geom_exon2(), ggplot2::geom_point()),
    scale_x_ggexon_genomic(intron_factor = 10), layout
  )

  expect_named(registry$maps, "2")
  expect_equal(unique(registry$maps[["2"]]$PANEL), 2L)
  expect_identical(registry$panel_map[c("1", "2", "3")],
    c(`1` = 2L, `2` = 2L, `3` = 2L))
  mapped <- map_ggexon_genomic_x_scale(list(signal, exons, points), registry)
  expect_equal(mapped[[1L]]$xmin, c(89.5, 100, 190))
  expect_equal(mapped[[1L]]$xmax, c(100, 190, 200.5))
  expect_equal(mapped[[3L]]$x, c(190, 145))
  expect_named(registry$maps, "2")
})

test_that("metadata cannot make undeclared geoms train a genomic scale", {
  metadata <- transform(genomic_training_exons()[1L, ], xmin = 1, xmax = 1100)
  data <- rep(list(metadata), 3L)
  layers <- list(ggplot2::geom_rect(), geom_gene(), geom_genetag())
  registry <- train_ggexon_genomic_x_scale(
    data, layers, scale_x_ggexon_genomic(intron_factor = 10)
  )

  expect_s3_class(registry, "ggexon_trained_genomic_x_scale")
  expect_length(registry$maps, 0L)
  expect_length(registry$panel_map, 0L)
  expect_identical(map_ggexon_genomic_x_scale(data, registry), data)
})

test_that("blank annotation placeholders cannot fill the trained intron", {
  exons <- transform(genomic_training_exons(), blank_panel = FALSE)
  placeholders <- transform(exons, xmin = 1, xmax = 1100,
    blank_panel = c(TRUE, FALSE), transcripts = c("placeholder", "__blank__gene"))
  spec <- scale_x_ggexon_genomic(intron_factor = 10)
  registry <- train_ggexon_genomic_x_scale(
    list(rbind(exons, placeholders)), list(geom_exon2()), spec
  )

  expect_equal(map_ggexon_genomic_x_scale(list(exons), registry)[[1L]]$xmin,
    c(1, 190))
  blank_only <- train_ggexon_genomic_x_scale(
    list(placeholders), list(geom_exon2()), spec
  )
  expect_s3_class(blank_only, "ggexon_trained_genomic_x_scale")
  expect_length(blank_only$maps, 0L)
  expect_identical(map_ggexon_genomic_x_scale(list(placeholders), blank_only),
    list(placeholders))
})

test_that("builds retain trained maps through final position training and reuse", {
  plot <- genomic_training_plot()
  original <- plot@data
  built <- ggexon_build(plot)
  registry <- built@layout$genomic_x_scale_registry
  trained <- serialize(registry, NULL)

  expect_s3_class(registry, "ggexon_trained_genomic_x_scale")
  expect_named(registry$maps, "1")
  expect_identical(registry$panel_map, c(`1` = 1L))
  expect_equal(built@data[[1L]]$xmin, c(1, 190))
  expect_equal(built@layout$genomic_x_axis_data, registry$axis_data)

  built@layout$train_position(built@data,
    ggplot2::scale_x_continuous(), ggplot2::scale_y_continuous())
  expect_identical(serialize(built@layout$genomic_x_scale_registry, NULL), trained)
  expect_equal(built@layout$map_genomic_x(built@data), built@data)

  rebuilt <- ggexon_build(plot)
  expect_identical(plot@data, original)
  expect_identical(serialize(rebuilt@layout$genomic_x_scale_registry, NULL), trained)
  expect_equal(rebuilt@data, built@data)
})
