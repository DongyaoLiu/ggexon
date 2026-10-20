alignseq_fixture <- function(...) {
  alignseq_data(c(human = "AC-GTAC", chimp = "ATCG-AC"),
    data.frame(sequence = c("human", "chimp"), start = c(100, 900),
      strand = c("+", "+"), track = c("human", "chimp"), y = 1),
    reference = "human", ...)
}

test_that("aligned gaps do not consume coordinates and indels use reference direction", {
  d <- alignseq_fixture()
  h <- d[d$sequence == "human", ]
  q <- d[d$sequence == "chimp", ]
  expect_equal(h$position, c(100, 101, NA, 102, 103, 104, 105))
  expect_equal(q$position, c(900, 901, 902, 903, NA, 904, 905))
  expect_equal(h$x, c(100, 101, 101.5, 102, 103, 104, 105))
  expect_equal(q$x, c(900, 901, 902, 903, 903.5, 904, 905))
  expect_equal(h$event_type, c("match", "substitution", "insertion", "match",
    "deletion", "match", "match"))
  expect_equal(h$event_id[c(2, 3, 5)], c(1L, 2L, 3L))
  expect_true(all(h$reference == "human"))
})

test_that("reverse anchors follow input orientation without consuming gap bases", {
  d <- alignseq_data(c(h = "A--CG", q = "ATTCG"),
    data.frame(sequence = c("h", "q"), start = c(105, 700), strand = c("-", "+")), "h")
  h <- d[d$sequence == "h", ]
  expect_equal(h$x, c(105, 104.5, 104.5, 104, 103))
  expect_equal(h$position, c(105, NA, NA, 104, 103))
  expect_equal(h$event_id[2:3], c(1L, 1L))
  expect_equal(h$event_length[2:3], c(2L, 2L))
})

test_that("reference windows select bases and explicit nearby details merge", {
  d <- alignseq_fixture(regions = c(101, 104),
    details = data.frame(start = c(101, 103), end = c(101, 103)), flank = 1)
  h <- d[d$sequence == "human", ]
  expect_equal(h$column, 2:6)
  expect_equal(unique(stats::na.omit(h$detail_id)), 1L)
  expect_equal(nrow(alignseq_fixture(regions = c(800, 900))), 0L)
  expect_true(all(is.na(alignseq_fixture()$detail_id)))
})

test_that("uncertain bases are distinguished from verified substitutions", {
  d <- alignseq_data(c(h = "ANRY", q = "ATRY"),
    data.frame(sequence = c("h", "q"), start = 1, strand = "+"), "h")
  expect_equal(d$event_type[d$sequence == "h"],
    c("match", "ambiguous", "ambiguous", "ambiguous"))
})

test_that("malformed alignments fail rather than inventing coordinates", {
  a <- data.frame(sequence = c("h", "q"), start = 1, strand = "+")
  expect_error(alignseq_data(c(h = "AA", q = "A"), a, "h"), "equal")
  expect_error(alignseq_data(c(h = "A-", q = "A-"), a, "h"), "double")
  expect_error(alignseq_data(c(h = "AB", q = "AZ"), a, "h"), "DNA")
  expect_error(alignseq_data(c(h = "AC", q = "AC"), a, "other"), "reference")
  a$start[1] <- 1.5
  expect_error(alignseq_data(c(h = "AC", q = "AC"), a, "h"), "integer")
})

alignseq_plot <- function(factor = 10, mode = "markers") {
  d <- alignseq_fixture(details = c(101, 103), flank = 1)
  ex <- data.frame(xmin = c(99, 104, 899, 904), xmax = c(101, 106, 901, 906),
    ymin = 1, transcripts = c("h", "h", "c", "c"), strand = "+",
    track = c("human", "human", "chimp", "chimp"), type = "exon")
  ggexon(ex, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin,
      transcripts = transcripts, strand = strand, track = track, type = type)) +
    geom_exon2(compress_introns = FALSE, annotation_type = "all") +
    geom_alignseq(data = d, mode = mode) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1) +
    scale_x_ggexon_genomic(intron_factor = factor, match_by = "track")
}

test_that("sequence anchors follow the annotation mosaic scale before callout layout", {
  b <- ggexon_build(alignseq_plot())
  d <- b$data[[2]]
  h <- d[d$sequence == "human", ]
  expect_equal(h$x, c(100, 101, 101.05, 101.1, 101.2, 101.3, 102.3))
  expect_equal(h$genomic_x, c(100, 101, 101.5, 102, 103, 104, 105))
  expect_equal(length(unique(d$PANEL)), 2L)
  expect_silent(ggplot2::ggplot_gtable(b))
  expect_silent(ggplot2::ggplot_gtable(ggexon_build(alignseq_plot(mode = "rows"))))
})

test_that("rendered sequence includes explicit gaps and both reference names", {
  gt <- ggplot2::ggplot_gtable(ggexon_build(alignseq_plot()))
  labels <- function(g) {
    c(if (inherits(g, "text")) as.character(g$label),
      unlist(lapply(g$children, labels)), unlist(lapply(g$grobs, labels)))
  }
  text <- labels(gt)
  expect_true(any(text == "-"))
  expect_true(any(grepl("human.*reference", text)))
  expect_true(any(text == "chimp"))
})

test_that("no differences and empty windows render without fabricated events", {
  d <- alignseq_data(c(h = "ACGT", q = "ACGT"),
    data.frame(sequence = c("h", "q"), start = c(1, 20), strand = "+"), "h")
  expect_true(all(is.na(d$event_id)))
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot() + geom_alignseq(data = d)))
  expect_silent(ggplot2::ggplotGrob(ggplot2::ggplot() +
    geom_alignseq(data = alignseq_fixture(regions = c(800, 900)))))
  expect_error(geom_alignseq(data = data.frame(x = 1, y = 1)), "alignseq_data")
})

test_that("nearby separate callouts move apart before stacking and keep both names visible", {
  d <- alignseq_fixture(details = data.frame(start = c(101, 103), end = c(101, 103)), flank = 0)
  d <- d[d$sequence == "human", ]
  gt <- ggplot2::ggplotGrob(ggplot2::ggplot() + geom_alignseq(data = d) + ggplot2::xlim(0, 1000))
  text_grobs <- function(g) {
    c(if (inherits(g, "text") && any(grepl("human.*reference", g$label))) list(g),
      unlist(lapply(g$children, text_grobs), recursive = FALSE),
      unlist(lapply(g$grobs, text_grobs), recursive = FALSE))
  }
  refs <- text_grobs(gt)
  expect_length(refs, 2L)
  expect_gt(abs(as.numeric(refs[[1]]$x) - as.numeric(refs[[2]]$x)), .15)
})

test_that("long detail requires explicit narrower windows", {
  d <- alignseq_fixture(details = c(100, 105), flank = 0)
  expect_error(geom_alignseq(data = d, max_columns = 3), "narrower")
  expect_error(geom_alignseq(data = d, mode = "rows", max_columns = 3), "narrower")
  expect_error(alignseq_fixture(regions = c(105, 100)), "start")
})

test_that("reverse-strand callout edges connect in supplied alignment order", {
  d <- alignseq_data(c(h = "ACGT", q = "ATGT"),
    data.frame(sequence = c("h", "q"), start = c(105, 700), strand = c("-", "+")),
    "h", details = c(102, 105), flank = 0)
  d <- d[d$sequence == "h", ]
  gt <- ggplot2::ggplotGrob(ggplot2::ggplot() + geom_alignseq(data = d))
  find_geom <- function(g) c(if (identical(g$name, "geom_alignseq")) list(g),
    unlist(lapply(g$children, find_geom), recursive = FALSE),
    unlist(lapply(g$grobs, find_geom), recursive = FALSE))
  lines <- Filter(function(g) inherits(g, "polyline"), find_geom(gt)[[1]]$children)
  expect_length(lines, 1L)
  if (!length(lines)) return()
  connectors <- lines[[1]]
  ids <- connectors$id
  starts <- which(!duplicated(ids))
  ends <- which(!duplicated(ids, fromLast = TRUE))
  x <- as.numeric(connectors$x); y <- as.numeric(connectors$y)
  # Reverse-strand genomic endpoints stay in supplied alignment order.
  expect_gt(x[starts[1]], x[starts[2]])
  expect_lt(x[ends[1]], x[ends[2]])
  expect_true(all(diff(y[ids == ids[1]]) > 0))
  i <- which(ids == ids[1])
  straight <- x[i[1]] + (x[tail(i, 1)] - x[i[1]]) *
    (y[i] - y[i[1]]) / (y[tail(i, 1)] - y[i[1]])
  expect_gt(max(abs(x[i] - straight)), .01)

})

test_that("disjoint selections never concatenate omitted DNA into apparent alignment", {
  d <- alignseq_fixture(regions = data.frame(start = c(100, 104), end = c(101, 105)),
    details = c(100, 105), flank = 0)
  h <- d[d$sequence == "human", ]
  expect_equal(h$column, c(1L, 2L, 6L, 7L))
  expect_equal(h$detail_id, c(1L, 1L, 2L, 2L))
  expect_error(geom_alignseq(data = d, mode = "rows"), "contiguous")
})

test_that("each annotation panel displays only its own sequence", {
  for (mode in c("markers", "rows")) {
    b <- ggexon_build(alignseq_plot(mode = mode))
    labels <- function(g) c(if (inherits(g, "text")) as.character(g$label),
      unlist(lapply(g$children, labels)), unlist(lapply(g$grobs, labels)))
    d <- b$data[[2]]
    for (species in c("human", "chimp")) {
      own <- d[d$sequence == species, ]
      panel <- as.integer(own$PANEL[1])
      g <- GeomAlignseq$draw_panel(own, b$layout$panel_params[[panel]],
        b$plot$coordinates, mode = mode)
      txt <- labels(g)
      expect_true(any(grepl(species, txt)))
      expect_false(any(grepl(if (species == "human") "chimp" else "human", txt)))
    }
  }
})

test_that("sequence detail can mark a verified splice boundary without changing bases", {
  d <- alignseq_fixture(details = c(100, 105), flank = 0)
  d$boundary_after <- d$sequence == "human" & d$column == 4
  d$boundary_label <- ifelse(d$boundary_after, "E1 donor", "")
  gt <- ggplot2::ggplotGrob(ggplot2::ggplot() + geom_alignseq(data = d))
  labels <- function(g) c(if (inherits(g, "text")) as.character(g$label),
    unlist(lapply(g$children, labels)), unlist(lapply(g$grobs, labels)))
  expect_equal(sum(labels(gt) == "E1 donor"), 1L)
  expect_equal(sum(labels(gt) == "-"), 2L)
})

test_that("reversed genomic panels preserve schematic DNA order and donor placement", {
  d <- alignseq_data(c(h = "ACGT", q = "ATGT"),
    data.frame(sequence = c("h", "q"), start = c(100, 700), strand = "+"),
    "h", details = c(100, 103), flank = 0)
  d$boundary_after <- d$column == 2
  d$boundary_label <- ifelse(d$boundary_after, "donor", "")
  for (mode in c("markers", "rows")) {
    b <- ggexon_build(ggexon() + geom_alignseq(data = d, mode = mode) +
      facet_genomics(vars(track), scales = "free_x", reverse_x = TRUE))
    own <- b$data[[1]][b$data[[1]]$sequence == "h", ]
    panel <- as.integer(own$PANEL[1])
    g <- GeomAlignseq$draw_panel(own, b$layout$panel_params[[panel]],
      b$plot$coordinates, mode = mode)
    texts <- Filter(function(x) inherits(x, "text"), g$children)
    bases <- Filter(function(x) identical(as.character(x$label), c("A", "C", "G", "T")), texts)[[1]]
    expect_true(all(diff(as.numeric(bases$x)) > 0))
    name <- Filter(function(x) identical(as.character(x$label), "h (reference)"), texts)[[1]]
    expect_lt(as.numeric(name$x), min(as.numeric(bases$x)))
    donor <- Filter(function(x) identical(as.character(x$label), "donor"), texts)[[1]]
    expect_equal(as.numeric(donor$x), mean(as.numeric(bases$x)[2:3]))
  }
})

test_that("sequence names and decimal y positions cannot merge independent rows", {
  d <- alignseq_data(c("a.1" = "ACGT", a = "ATGT"),
    data.frame(sequence = c("a.1", "a"), start = c(100, 200),
      strand = "+", track = "both", y = c(2, 1.2)),
    "a.1", details = c(100, 103), flank = 0)
  b <- ggplot2::ggplot_build(ggplot2::ggplot() + geom_alignseq(data = d, mode = "rows"))
  g <- GeomAlignseq$draw_panel(b$data[[1]], b$layout$panel_params[[1]],
    b$plot$coordinates, mode = "rows")
  texts <- Filter(function(x) inherits(x, "text"), g$children)
  labels <- unlist(lapply(texts, function(x) as.character(x$label)))
  expect_true(all(c("a.1 (reference)", "a") %in% labels))
  expect_equal(sum(labels %in% c("A", "C", "G", "T")), 8L)
})
