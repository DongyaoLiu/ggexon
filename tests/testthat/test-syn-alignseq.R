native_dna <- function(x, start = c(200, 300), chr = "chr1", name = "dna") {
  dna <- c(h = "ACTG", q = "AC-G")
  prepare_alignseq(x, dna, data.frame(sequence = names(dna), strand = "+",
    start = start, track = c("a", "b"), chr = chr), "h", name = name,
    details = range(start[1] + 0:3), flank = 0)
}

dna_pair <- function(genes = FALSE) {
  individual <- if (genes) prepared_genomic_fixture else function(id) SynIndividual(id = id)
  add_individual(SynSpecies("pair"), individual("a"), individual("b"))
}

test_that("prepared DNA alone dispatches by name and native coordinates", {
  model <- native_dna(dna_pair())
  p <- ggexon(model) + geom_alignseq(species = "b", chr = "chr1", subset = c(301, 302)) +
    facet_genomics(ggplot2::vars(track))
  d <- ggexon_build(p)@data[[1]]
  expect_equal(d$x, c(301, 301.5, 302))
  expect_identical(as.character(d$sequence), rep("q", 3))
  expect_silent(ggplot2::ggplot_gtable(ggexon_build(p)))
  empty <- ggexon(model) + geom_alignseq(chr = "chr1", subset = c(800, 900)) +
    facet_genomics(ggplot2::vars(track))
  expect_equal(nrow(ggexon_build(empty)@data[[1]]), 0L)
  model <- native_dna(model, name = "second")
  expect_error(ggexon_build(ggexon(model) + geom_alignseq()), "Multiple.*name")
  expect_equal(nrow(ggexon_build(ggexon(model) + geom_alignseq(alignment = "second"))@data[[1]]), 8L)
})

test_that("prepared DNA follows custom annotation recipients and stored windows", {
  model <- native_dna(dna_pair())
  panels <- data.frame(PANEL = 1:4, ROW = 1:4, COL = 1L,
    track = c("reads", "model_a", "pair_link", "model_b"),
    panel_type = c("coverage", "annotation", "link", "annotation"),
    species = c("a", "a", NA, "b"), xlim_chr = c("chr1", "chr1", NA, "chr2"),
    xlim_min = c(200, 201, NA, 300), xlim_max = c(201, 202, NA, 304),
    tspecies = c(NA, NA, "a", NA), qspecies = c(NA, NA, "b", NA))
  species_layout(model) <- SynLayout(panels, free = list(x = TRUE, y = TRUE))
  built <- ggexon_build(ggexon(model) + geom_alignseq() +
    facet_genomics(ggplot2::vars(track), scales = "free_x"))
  d <- built@data[[1]]
  expect_equal(d$x, c(201, 202))
  expect_identical(unique(as.character(d$track)), "model_a")
  expect_identical(as.character(built@layout$layout$panel_type[
    match(unique(d$PANEL), built@layout$layout$PANEL)]), "annotation")
})

test_that("hiding event markers retains the coloured bases, gaps and connector", {
  d <- query_alignseq(native_dna(dna_pair()), individual = "b")
  b <- ggplot2::ggplot_build(ggplot2::ggplot() + geom_alignseq(data = d,
    show_markers = FALSE, colour_scheme = "difference"))
  g <- b@plot@layers[[1]]$draw_geom(b@data[[1]], b@layout)[[1]]
  expect_false(any(vapply(g$children, inherits, logical(1), "points")))
  labels <- unlist(lapply(g$children, function(child) as.character(child$label)))
  expect_true("-" %in% labels)
  expect_false(any(grepl("[+\u2212][0-9]", labels)))
  expect_true(any(vapply(g$children, inherits, logical(1), "polyline")))
  fills <- unlist(lapply(g$children, function(child) child$gp$fill))
  expect_true(any(grepl("#D73027", fills)))
})

test_that("connectors meet the selected displayed model after y remapping and compression", {
  model <- native_dna(dna_pair(TRUE))
  target <- geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1", b = "tx2"),
    show_markers = FALSE)
  exons <- geom_exon2(ggplot2::aes(transcripts = paste0("alias-", transcript_id),
    ymin = ifelse(transcript_id == "tx1", 8, 3)), exon_height = 1.5, compress_introns = FALSE)
  p <- ggexon(model) + target + exons + facet_genomics(ggplot2::vars(track)) +
    scale_x_ggexon_genomic(intron_factor = 8, match_by = "track")
  b <- ggexon_build(p)
  for (id in c("a", "b")) {
    d <- subset(b@data[[1]], track == id)
    e <- subset(b@data[[2]], track == id &
      transcripts == if (id == "a") "alias-tx1" else "alias-tx2")
    x <- mean(range(d$x))
    expect_equal(unique(d$xend), x)
    top <- e$ymax[e$xmin <= x & e$xmax >= x]
    expect_length(top, 1L)
    expect_equal(unique(d$yend), top)
    g <- b@plot@layers[[1]]$draw_geom(b@data[[1]], b@layout)[[as.integer(d$PANEL[1])]]
    path <- Filter(function(child) inherits(child, "polyline"), g$children)[[1]]
    origin <- b@plot@coordinates$transform(data.frame(x = x, y = top),
      b@layout$panel_params[[as.integer(d$PANEL[1])]])
    expect_equal(as.numeric(path$x)[1], origin$x)
    expect_equal(as.numeric(path$y)[1], origin$y)
  }
  expect_error(ggexon_build(ggexon(model) + target), "displayed.*exon")
  expect_error(ggexon_build(p + exons), "[Aa]mbiguous")
  intron <- native_dna(dna_pair(TRUE), start = c(150, 170))
  b <- ggexon_build(ggexon(intron) + target + exons + facet_genomics(ggplot2::vars(track)))
  e <- subset(b@data[[2]], track == "a" & transcripts == "alias-tx1")
  d <- subset(b@data[[1]], track == "a")
  path <- .exon2_intron_data(e)
  expect_equal(unique(d$yend), stats::approx(path$x, path$y, unique(d$xend))$y)
  expect_gt(unique(d$yend), e$y_middle[1])
})

test_that("transcript anchoring respects exon2 local compression and model bounds", {
  model <- native_dna(dna_pair(TRUE))
  build <- function(model) ggexon_build(ggexon(model) +
    geom_exon2(exon_height = 1, compress_introns = TRUE, intron_width = 5) +
    geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1", b = "tx2")) +
    facet_genomics(ggplot2::vars(track)))
  b <- build(model)
  d <- subset(b@data[[2]], track == "a")
  e <- subset(b@data[[1]], track == "a" & transcripts == "tx1")
  expect_lt(max(d$x), 120)
  expect_equal(unique(d$yend), e$ymax[2])
  outside <- native_dna(dna_pair(TRUE), start = c(500, 300))
  expect_error(build(outside), "outside displayed transcript")
})

test_that("a connector on an exon2 terminal arrow meets its sloping edge", {
  model <- native_dna(dna_pair(TRUE), start = c(206, 305))
  b <- ggexon_build(ggexon(model) + geom_exon2(compress_introns = FALSE) +
    geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1", b = "tx2")) +
    facet_genomics(ggplot2::vars(track)))
  d <- subset(b@data[[2]], track == "a")
  e <- subset(b@data[[1]], track == "a" & transcripts == "tx1")
  arrow <- .exon2_arrow_data(e)
  expect_equal(unique(d$yend), stats::approx(arrow$x[1:2], arrow$y[1:2], unique(d$xend))$y)
})

test_that("explicit-table transcript anchoring can match the displayed transcript aesthetic", {
  d <- query_alignseq(native_dna(dna_pair()), individual = "a")
  d$individual <- NULL
  exon <- data.frame(xmin = 190, xmax = 215, ymin = 2, transcripts = "model",
    track = "a", type = "exon", strand = "+")
  p <- ggexon(exon, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
    transcripts = transcripts, track = track, type = type, strand = strand)) +
    geom_exon2(compress_introns = FALSE) +
    geom_alignseq(d, anchor = "transcript", transcripts = c(a = "model"))
  b <- ggexon_build(p)
  expect_equal(unique(b@data[[2]]$yend), b@data[[1]]$ymax)
})

test_that("an unknown anchor chromosome requires an unambiguous recipient", {
  model <- native_dna(dna_pair(), chr = NA_character_)
  panels <- data.frame(PANEL = 1:2, ROW = 1:2, COL = 1L,
    track = c("a_first", "a_second"), individual = "a", panel_type = "annotation",
    xlim_chr = c("chr1", "chr2"), xlim_min = 200, xlim_max = 203)
  species_layout(model) <- SynLayout(panels, free = list(x = TRUE, y = TRUE))
  facet <- facet_genomics(ggplot2::vars(track), scales = "free_x")
  expect_error(ggexon_build(ggexon(model) + geom_alignseq() + facet), "chromosome anchors")
  d <- ggexon_build(ggexon(model) + geom_alignseq(chr = "chr2") + facet)@data[[1]]
  expect_equal(unique(as.character(d$track)), "a_second")
  expect_equal(d$x, 200:203)
})

test_that("transcript connectors follow the displayed exon's position nudge", {
  model <- native_dna(dna_pair(TRUE))
  for (shared_scale in c(FALSE, TRUE)) {
    p <- ggexon(model) +
      geom_exon2(compress_introns = FALSE, position = ggplot2::position_nudge(x = 3, y = 2)) +
      geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1", b = "tx2")) +
      facet_genomics(ggplot2::vars(track))
    if (shared_scale) p <- p + scale_x_ggexon_genomic(intron_factor = 8, match_by = "track")
    b <- ggexon_build(p)
    d <- subset(b@data[[2]], track == "a")
    exon <- subset(b@data[[1]], track == "a" & transcripts == "tx1" & genomic_xmin > 190)
    expect_equal(unique(d$xend), exon$xmin + 201.5 - exon$genomic_xmin)
    expect_equal(unique(d$yend), exon$ymax)
  }
})

test_that("scale censoring an earlier exon leaves visible transcript attachments intact", {
  # One DNA base and the earlier exon fall outside the scale limits.
  model <- native_dna(dna_pair(TRUE), start = c(198, 300))
  p <- ggexon(model) + geom_exon2(compress_introns = FALSE) +
    geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1", b = "tx2")) +
    ggplot2::scale_x_continuous(limits = c(199, 309)) + facet_genomics(ggplot2::vars(track))
  b <- ggexon_build(p)
  d <- subset(b@data[[2]], track == "a" & is.finite(x))
  expect_equal(unique(d$xend), 200)
  exon <- subset(b@data[[1]], track == "a" & transcripts == "tx1" & is.finite(xmin))
  expect_equal(unique(d$yend), exon$ymax)
})

test_that("transcript connectors respect known chromosomes of DNA and exons", {
  model <- prepared_genomic_fixture("a")
  build <- function(chr) {
    anchors <- data.frame(sequence = c("h", "q"), track = "a", strand = "+",
      start = 200, chr = chr)
    saved <- prepare_alignseq(model, c(h = "ACTG", q = "AC-G"), anchors, "h",
      details = c(200, 203), flank = 0)
    ggexon_build(ggexon(saved) + geom_exon2(compress_introns = FALSE) +
      geom_alignseq(anchor = "transcript", transcripts = c(a = "tx1")))
  }
  expect_error(build("chr2"), "No displayed exon.*tx1.*chr2")
  expect_equal(unique(build("chr1")@data[[2]]$xend), c(201.5, 201))
  expect_equal(unique(build(NA_character_)@data[[2]]$xend), c(201.5, 201))

  dna <- query_alignseq(native_dna(dna_pair()), individual = "a")
  exon <- data.frame(xmin = 190, xmax = 215, ymin = 2, transcripts = "tx1",
    track = "a", type = "exon", strand = "+", seqnames = "chr2")
  p <- ggexon(exon, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
    transcripts = transcripts, track = track, type = type, strand = strand)) +
    geom_exon2(compress_introns = FALSE)
  expect_error(ggexon_build(p + geom_alignseq(dna, anchor = "transcript",
    transcripts = c(a = "tx1"))), "No displayed exon.*tx1.*chr1")
})
