af_points_plot <- function(rows, reversed = FALSE, reverse_x = NULL, compressed = FALSE,
                           mapping = NULL) {
  order <- c("ref", "link_pair", "query")
  if (reversed) order <- rev(order)
  rows$track <- factor(rows$track, levels = order)
  exons <- data.frame(
    xmin = if (compressed) c(0, 200, 1000, 1400) else c(0, 1000),
    xmax = if (compressed) c(100, 300, 1100, 1500) else c(100, 1400),
    track = factor(rep(c("ref", "query"), each = if (compressed) 2 else 1), order)
  )
  p <- ggexon() +
    geom_exon2(data = exons, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 1,
      transcripts = track, strand = "+", track = track, type = "exon"),
      annotation_type = "all", compress_introns = FALSE) +
    geom_allele_frequency(mapping, rows) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1,
                   reverse_x = reverse_x, reverse_x_match_by = "track") +
    ggplot2::scale_x_continuous(expand = c(0, 0))
  if (compressed) p <- p + scale_x_ggexon_genomic(intron_factor = 10)
  p
}

af_drawn_points <- function(p) {
  table <- ggplot2::ggplot_gtable(ggexon_build(p))
  panels <- table$grobs[startsWith(table$layout$name, "panel")]
  unlist(lapply(panels, function(panel) {
    Filter(function(g) inherits(g, "points"), as.list(panel$children))
  }), recursive = FALSE, use.names = FALSE)
}

af_geom_rows <- function(reverse_alignment = FALSE, pos = 20, other = 1300) {
  fixture <- af_data_fixture(
    sprintf("%d\tA\tG\t%d\t1\t1\t2000\t2000\t1\t-1\tchrR\tchrQ",pos,other),
    sprintf("chrR\t%d\t.\tA\tG\t.\tPASS\tAF=%s", pos, c(0,.25,.5,1)),
    reverse_alignment = reverse_alignment)
  query_allele_frequency(af_data_prepare(fixture))
}

test_that("actual point grobs interpolate displayed endpoints in reference order", {
  for (swapped in c(FALSE, TRUE)) {
    rows <- af_geom_rows(swapped)
    for (reversed in c(FALSE, TRUE)) {
      points <- af_drawn_points(af_points_plot(rows, reversed))
      expect_length(points, 1)
      expect_equal(as.numeric(points[[1]]$x), c(.2,.3375,.475,.75))
      expect_equal(as.numeric(points[[1]]$y), if (reversed) c(0,.25,.5,1) else c(1,.75,.5,0))
    }
  }
})

test_that("points interpolate after panel reversal and intron compression", {
  points <- af_drawn_points(af_points_plot(af_geom_rows(), reverse_x = "query"))
  expect_equal(as.numeric(points[[1]]$x), c(.2,.2125,.225,.25))
  rows <- af_geom_rows(pos = 250, other = 1250)
  points <- af_drawn_points(af_points_plot(rows[2, ], compressed = TRUE))
  expect_equal(as.numeric(points[[1]]$x), 39/56)
})

test_that("ordinary point scales control grob size shape and colour", {
  rows <- af_geom_rows()
  rows$frequency <- rows$af
  rows$af <- NULL
  p <- af_points_plot(rows, mapping = ggplot2::aes(af = frequency, size = frequency,
      colour = frequency, shape = factor(frequency))) +
    ggplot2::scale_shape_manual(values = c(1, 2, 3, 4))
  pts <- af_drawn_points(p)[[1]]
  expect_equal(pts$pch, 1:4)
  expect_true(all(diff(pts$gp$fontsize) > 0))
  expect_length(unique(pts$gp$col), 4)
})

test_that("invalid frequencies fail at build time", {
  rows <- af_geom_rows()
  rows$af[2] <- 2
  expect_error(ggexon_build(af_points_plot(rows)), "Allele frequency must be numeric")
})

af_implicit_plot <- function(x, preparation = NULL, ...) {
  genes <- data.frame(xmin = c(1, 800), xmax = c(90, 1200), track = c("ref", "query"))
  ggexon(x) +
    geom_exon2(data = genes, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 1,
      transcripts = track, strand = "+", track = track, type = "exon"),
      annotation_type = "all", compress_introns = FALSE) +
    geom_nuclink(alignment = "pair") +
    geom_allele_frequency(preparation = preparation) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1, ...)
}

test_that("stored preparations route to their alignment and respect panel windows", {
  fixture <- af_data_fixture(track = "population ribbon")
  x <- af_data_prepare(fixture)
  p <- af_implicit_plot(x)
  b <- ggexon_build(p)
  panel <- b@layout$layout
  expect_identical(as.character(panel$track[match(b$data[[3]]$PANEL[1], panel$PANEL)]),
                   "population ribbon")
  expect_length(af_drawn_points(p), 1)
  x <- prepare_allele_frequency(x, "population", "snps", "replicate")
  expect_error(ggexon_build(af_implicit_plot(x)), "preparation name")
  expect_length(af_drawn_points(af_implicit_plot(x, "replicate")), 1)
  p <- af_implicit_plot(x, "replicate", xlim = list(ref = c(1, 100), query = c(800, 1200)))
  expect_length(af_drawn_points(p), 0)
})

test_that("valid empty preparations draw no points", {
  fixture <- af_data_fixture(vcf_rows = "chrR\t101\t.\tA\tT\t.\tPASS\tAF=0.25")
  expect_warning(x <- af_data_prepare(fixture), "allele_mismatch")
  expect_no_warning(pts <- af_drawn_points(af_implicit_plot(x)))
  expect_length(pts, 0)
})

test_that("missing panels and unsupported coordinates are reported explicitly", {
  rows <- af_geom_rows()
  p <- af_points_plot(rows)
  expect_error(ggexon_build(ggexon() + geom_allele_frequency(data = rows)), "link panel")
  rows$tspecies <- "absent"
  expect_error(ggexon_build(af_points_plot(rows)), "source annotation panels")
  expect_error(af_drawn_points(p + ggplot2::coord_flip()), "unflipped Cartesian")
  expect_error(af_drawn_points(p + ggplot2::coord_polar()), "unflipped Cartesian|Cartesian")
})

test_that("alternate endpoint column names provide the facet its routing inputs", {
  rows <- af_geom_rows()
  names(rows)[match(c("tspecies","qspecies","tstart","qstart"), names(rows))] <-
    c("target","query","target_pos","query_pos")
  pts <- af_drawn_points(af_points_plot(rows, mapping = ggplot2::aes(
    tspecies = target, qspecies = query, tstart = target_pos, qstart = query_pos)))
  expect_equal(as.numeric(pts[[1]]$x), c(.2,.3375,.475,.75))
})

for (track in c("link_pair", "population ribbon")) {
  test_that(paste("explicit ribbon and point layers share one panel named", track), {
    rows <- af_geom_rows()[2L, ]
    reference <- if (track == "link_pair") "ref" else "ref_link_assembly"
    rows$tspecies <- reference
    rows$track <- factor(track, levels = c(reference, track, "query"))
    annotations <- data.frame(track = factor(rep(c(reference, "query"), each = 2),
      levels = c(reference, track, "query")), x = c(0, 100, 1000, 1400))
    ribbon <- rows
    ribbon$tstart <- 10
    ribbon$tend <- 30
    ribbon$qstart <- 1290
    ribbon$qend <- 1310
    p <- ggexon() +
      ggplot2::geom_blank(data = annotations, ggplot2::aes(x = x, y = 1)) +
      geom_nuclink(data = ribbon, ggplot2::aes(
        tspecies = tspecies, qspecies = qspecies, tstart = tstart, tend = tend,
        qstart = qstart, qend = qend, tchr = tchr, qchr = qchr, strand = strand)) +
      geom_allele_frequency(data = rows) +
      facet_genomics(ggplot2::vars(track), ncol = 1, scales = "free_x") +
      ggplot2::scale_x_continuous(expand = c(0, 0))
    b <- ggexon_build(p)
    panels <- b@layout$layout
    expect_equal(as.character(panels$track), c(reference, track, "query"))
    expect_equal(link_panel_type(panels), c("annotation", "link", "annotation"))
    expect_equal(nrow(b$data[[2]]), 4L)
    expect_equal(nrow(b$data[[3]]), 4L)
    points <- af_drawn_points(p)
    expect_length(points, 1L)
    expect_equal(as.numeric(points[[1]]$x), .3375)
    expect_equal(as.numeric(points[[1]]$y), .75)
  })
}
