piecewise_link_example <- function(strand = "+", factor = 10, species = NULL) {
  # A has its intron at 100–200; B has its intron at 1100–1200.
  # B's full link starts at 1050, giving distinct alignment fractions.
  exons <- data.frame(
    xmin = c(0, 200, 1000, 1200), xmax = c(100, 300, 1100, 1300),
    ymin = 1, transcripts = c("a", "a", "b", "b"),
    strand = "+", track = c("a", "a", "b", "b"), type = "exon")
  links <- data.frame(tspecies = "a", tchr = "chr1", tstart = 50, tend = 250,
    qspecies = "b", qchr = "chr1", qstart = 1050, qend = 1250,
    strand = strand, track = "link_a_b", group = 1, identity = 98)
  p <- ggexon(exons, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
      transcripts = transcripts, strand = strand, track = track, type = type)) +
    geom_exon2(compress_introns = FALSE, annotation_type = "all") +
    geom_nuclink(data = links, mapping = ggplot2::aes(
      tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend,
      qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
      strand = strand), fill = "steelblue", alpha = .4, inherit.aes = FALSE) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1)
  if (!is.null(factor)) p <- p + scale_x_ggexon_genomic(
    intron_factor = factor, species = species, match_by = "track")
  list(plot = p, links = links)
}

test_that("nucleotide links subdivide and follow each source panel's mosaic scale", {
  example <- piecewise_link_example()
  built <- ggexon_build(example$plot)
  d <- built$data[[2L]]
  # Link fractions 0, .25, .75, 1 delimit exon / intron / exon.
  expect_equal(nrow(d), 12L)
  expect_true(all(c("genomic_x", "parent_group") %in% names(d)))
  if (!all(c("genomic_x", "parent_group") %in% names(d))) return()
  t <- d[d$x_variable %in% c("tstart", "tend"), ]
  q <- d[d$x_variable %in% c("qstart", "qend"), ]
  expect_equal(sort(unique(t$genomic_x)), c(50, 100, 200, 250))
  expect_equal(sort(unique(t$x)), c(50, 100, 110, 160))
  expect_equal(sort(unique(q$genomic_x)), c(1050, 1100, 1200, 1250))
  expect_equal(sort(unique(q$x)), c(1050, 1100, 1110, 1160))
  expect_equal(length(unique(d$group)), 3L)
  expect_equal(unique(d$fill), "steelblue")
  expect_equal(unique(d$alpha), .4)
  expect_equal(example$links$tstart, 50)
  expect_silent(ggplot2::ggplot_gtable(built))
})

test_that("source scale boundaries are combined even when they disagree", {
  example <- piecewise_link_example()
  # Move only B's link endpoints. Boundary fractions become .05 and .55.
  example$plot@layers[[2L]]$data$qstart <- 1090
  example$plot@layers[[2L]]$data$qend <- 1290
  d <- ggexon_build(example$plot)$data[[2L]]
  expect_equal(nrow(d), 20L) # union: 0, .05, .25, .55, .75, 1
  if (!"genomic_x" %in% names(d)) return()
  t <- d[d$x_variable %in% c("tstart", "tend"), ]
  expect_equal(sort(unique(t$genomic_x)), c(50, 60, 100, 160, 200, 250))
  expect_equal(sort(unique(t$x)), c(50, 60, 100, 106, 110, 160))
})

test_that("reverse-strand links preserve base correspondence when subdivided", {
  d <- ggexon_build(piecewise_link_example(strand = "-")$plot)$data[[2L]]
  expect_equal(nrow(d), 12L)
  if (!"genomic_x" %in% names(d)) return()
  for (piece in split(d, d$group)) {
    expect_equal(sort(piece$draw_order), 1:4)
    t <- piece[piece$x_variable %in% c("tstart", "tend"), ]
    q <- piece[piece$x_variable %in% c("qstart", "qend"), ]
    expect_equal(min(t$genomic_x) + max(q$genomic_x), 1300)
    expect_equal(max(t$genomic_x) + min(q$genomic_x), 1300)
  }
})

test_that("partial panel compression leaves the other link end uncompressed", {
  d <- ggexon_build(piecewise_link_example(species = "a")$plot)$data[[2L]]
  expect_equal(nrow(d), 12L)
  if (!"genomic_x" %in% names(d)) return()
  q <- d[d$x_variable %in% c("qstart", "qend"), ]
  expect_equal(q$x, q$genomic_x)
})

test_that("unscaled and unit-factor nucleotide links retain their geometry", {
  plain <- ggexon_build(piecewise_link_example(factor = NULL)$plot)$data[[2L]]
  unit <- ggexon_build(piecewise_link_example(factor = 1)$plot)$data[[2L]]
  expect_equal(nrow(plain), 4L)
  expect_equal(nrow(unit), 4L)
  expect_equal(unit$x, plain$x)
  expect_equal(unit$group, plain$group)
})

test_that("subdivision keeps independent parents and paired unequal-length intervals", {
  example <- piecewise_link_example()
  second <- example$links
  second$qend <- 1290
  second$strand <- "-"
  example$plot@layers[[2L]]$data <- rbind(example$links, second)
  d <- ggexon_build(example$plot)$data[[2L]]
  expect_true("parent_group" %in% names(d))
  if (!"parent_group" %in% names(d)) return()
  expect_equal(length(unique(d$parent_group)), 2L)
  for (piece in split(d, d$group)) {
    expect_length(unique(piece$parent_group), 1L)
    if (piece$strand[1L] == "-") {
      t <- piece$genomic_x[piece$x_variable == "tstart"]
      q <- piece$genomic_x[piece$x_variable == "qend"]
      expect_equal((t - 50) / 200, (1290 - q) / 240)
    }
  }
})

test_that("zero-width links stay finite under a piecewise scale", {
  example <- piecewise_link_example()
  example$plot@layers[[2L]]$data$tstart <- 150
  example$plot@layers[[2L]]$data$tend <- 150
  example$plot@layers[[2L]]$data$qstart <- 1150
  example$plot@layers[[2L]]$data$qend <- 1150
  d <- ggexon_build(example$plot)$data[[2L]]
  expect_equal(nrow(d), 4L)
  expect_equal(sort(unique(d$x)), c(105, 1105))
})

test_that("stored panel windows use compressed coordinates after retraining", {
  tr <- build_ggexon_genomic_x_transform(data.frame(
    PANEL = 1, genomic_start = c(0, 200), genomic_end = c(100, 300)), 10)
  layout <- list(layout = data.frame(PANEL = 1L, SCALE_X = 1L,
    panel_type = "annotation", xlim_min = 0, xlim_max = 300),
    panel_scales_x = list(list(range = list(range = c(0, 300)))),
    genomic_x_transforms = list(`1` = tr))
  trained <- apply_panel_xlim_to_trained_scales(layout)
  expect_equal(trained$panel_scales_x[[1L]]$range$range, c(0, 210))
  expect_equal(trained$layout$xlim_max, 300) # stored genomic window is untouched
})
