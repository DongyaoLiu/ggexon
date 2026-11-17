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

test_that("reverse-strand callouts have one curved midpoint connector", {
  d <- alignseq_data(c(h = "ACGT", q = "ATGT"),
    data.frame(sequence = c("h", "q"), start = c(105, 700), strand = c("-", "+")),
    "h", details = c(102, 105), flank = 0)
  d <- d[d$sequence == "h", ]
  p <- ggplot2::ggplot() + geom_alignseq(data = d) +
    ggplot2::scale_x_continuous(limits = c(0, 1000), expand = ggplot2::expansion(mult = 0))
  gt <- ggplot2::ggplotGrob(p)
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
  expect_length(unique(ids), 1L)
  expect_equal(x[starts], 0.1035)
  texts <- Filter(function(g) inherits(g, "text"), find_geom(gt)[[1]]$children)
  bases <- Filter(function(g) identical(as.character(g$label), c("A", "C", "G", "T")), texts)[[1]]
  expect_equal(x[ends], mean(range(as.numeric(bases$x))))
  expect_true(all(diff(y[ids == ids[1]]) > 0))
  i <- which(ids == ids[1])
  straight <- x[i[1]] + (x[tail(i, 1)] - x[i[1]]) *
    (y[i] - y[i[1]]) / (y[tail(i, 1)] - y[i[1]])
  expect_gt(max(abs(x[i] - straight)), .01)

})

alignseq_test_grobs <- function(g) {
  c(if (identical(g$name, "geom_alignseq")) list(g),
    unlist(lapply(g$children, alignseq_test_grobs), recursive = FALSE),
    unlist(lapply(g$grobs, alignseq_test_grobs), recursive = FALSE))
}

test_that("sequence positions move blocks while one stem retains its genomic midpoint", {
  d <- alignseq_data(c(h = "ACGT", q = "ATGT"),
    data.frame(sequence = c("h", "q"), start = c(100, 700), strand = "+"),
    "h", details = c(100, 103), flank = 0)
  d <- d[d$sequence == "h", ]
  expected <- c(auto = 0.30, left = 0.30, center = 0.50, right = 0.89)
  for (mode in c("markers", "rows")) for (position in names(expected)) {
    p <- ggplot2::ggplot() + geom_alignseq(data = d, mode = mode,
      sequence_position = position) +
      ggplot2::scale_x_continuous(limits = c(0, 1000), expand = ggplot2::expansion(mult = 0))
    g <- alignseq_test_grobs(ggplot2::ggplotGrob(p))[[1]]
    texts <- Filter(function(g) inherits(g, "text"), g$children)
    bases <- Filter(function(g) identical(as.character(g$label), c("A", "C", "G", "T")), texts)[[1]]
    expect_equal(mean(range(as.numeric(bases$x))), unname(expected[position]))
    paths <- Filter(function(g) inherits(g, "polyline"), g$children)
    expect_length(paths, 1L)
    stem <- paths[[1]]
    expect_length(unique(stem$id), 1L)
    expect_equal(as.numeric(stem$x)[1], 0.1015)
    expect_equal(tail(as.numeric(stem$x), 1), unname(expected[position]))
    expect_true(all(diff(as.numeric(bases$x)) > 0))
  }
  expect_error(geom_alignseq(data = d, sequence_position = "middle"), "auto.*left.*center.*right")
})

test_that("fixed sequence positions stack separate details without horizontal drift", {
  d <- alignseq_fixture(details = data.frame(start = c(101, 103), end = c(101, 103)), flank = 0)
  d <- d[d$sequence == "human", ]
  p <- ggplot2::ggplot() + geom_alignseq(data = d, sequence_position = "center") +
    ggplot2::scale_x_continuous(limits = c(0, 1000), expand = ggplot2::expansion(mult = 0))
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(p))[[1]]
  texts <- Filter(function(g) inherits(g, "text"), g$children)
  names <- Filter(function(g) identical(as.character(g$label), "human (reference)"), texts)
  expect_length(names, 2L)
  expect_equal(as.numeric(names[[1]]$x), as.numeric(names[[2]]$x))
  expect_gt(abs(as.numeric(names[[1]]$y)-as.numeric(names[[2]]$y)), 0.1)
  paths <- Filter(function(g) inherits(g, "polyline"), g$children)
  expect_length(paths, 2L)
  expect_true(all(vapply(paths, function(g) length(unique(g$id)) == 1L, logical(1))))
  # Upper stems can cross a lower row: its nucleotide boxes and text must
  # paint over the stem, rather than letting a late stem obscure the bases.
  path_indices <- which(vapply(g$children, inherits, logical(1), "polyline"))
  box_indices <- which(vapply(g$children, inherits, logical(1), "rect"))
  text_indices <- which(vapply(g$children, inherits, logical(1), "text"))
  expect_lt(max(path_indices), min(box_indices))
  expect_lt(max(path_indices), min(text_indices))
})

test_that("fixed placement respects reversed panels after intron compression", {
  d <- alignseq_fixture(details = c(101,103), flank = 1)
  ex <- data.frame(xmin = c(99,104,899,904), xmax = c(101,106,901,906),
    ymin = 1, transcripts = c("h","h","q","q"), strand = "+",
    track = c("human","human","chimp","chimp"), type = "exon")
  for (position in c("left", "center", "right")) {
    p <- ggexon(ex, aes(xmin=xmin,xmax=xmax,ymin=ymin,transcripts=transcripts,
      strand=strand,track=track,type=type)) +
      geom_exon2(compress_introns=FALSE,annotation_type="all") +
      geom_alignseq(data=d,mode="rows",sequence_position=position) +
      facet_genomics(vars(track),scales="free_x",ncol=1,reverse_x=TRUE) +
      scale_x_ggexon_genomic(intron_factor=10,match_by="track")
    b <- ggexon_build(p)
    for (species in c("human", "chimp")) {
      own <- b@data[[2]][b@data[[2]]$sequence == species, ]
      panel <- as.integer(own$PANEL[1])
      g <- GeomAlignseq$draw_panel(own,b@layout$panel_params[[panel]],
        b@plot@coordinates,mode="rows",sequence_position=position)
      texts <- Filter(function(g) inherits(g,"text"),g$children)
      bases <- Filter(function(g) length(g$label)==7L,texts)[[1]]
      centre <- mean(range(as.numeric(bases$x)))
      if (position=="left") expect_lt(centre,0.4)
      if (position=="center") expect_equal(centre,0.5)
      if (position=="right") expect_gt(centre,0.8)
      expect_true(all(diff(as.numeric(bases$x))>0))
      paths <- Filter(function(g) inherits(g,"polyline"),g$children)
      expect_length(paths,1L)
      expect_length(unique(paths[[1]]$id),1L)
      expected <- b@plot@coordinates$transform(data.frame(
        x=mean(range(own$x)),y=own$y[1]+.23),b@layout$panel_params[[panel]])$x
      expect_equal(as.numeric(paths[[1]]$x)[1],expected)
      expect_equal(tail(as.numeric(paths[[1]]$x),1),centre)
    }
  }
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

alignseq_test_rgb <- function(colours) unname(grDevices::col2rgb(colours))

test_that("standard colour arguments still reach the layer", {
  d <- alignseq_fixture(details=c(100,105),flank=0)
  for (params in list(list(colour="grey30"), list(color="grey30"),
      list(colour="grey30",colour_scheme="difference"))) {
    expect_no_error(ggplot2::ggplotGrob(ggplot2::ggplot()+
      do.call(geom_alignseq,c(list(data=d),params))))
  }
})

test_that("difference colours highlight both partners of mismatch and indel columns", {
  d <- alignseq_fixture(details=c(100,105),flank=0)
  palette <- c(aligned="#E6E6E6",mismatch="#E69F00",indel="#D73027")
  expected <- unname(palette[c("aligned","mismatch","indel","aligned","indel","aligned","aligned")])
  for (mode in c("markers","rows")) for (species in c("human","chimp")) {
    own <- d[d$sequence==species,]
    p <- ggplot2::ggplot()+geom_alignseq(data=own,mode=mode,
      colour_scheme="difference")
    g <- alignseq_test_grobs(ggplot2::ggplotGrob(p))[[1]]
    boxes <- Filter(function(x)inherits(x,"rect"),g$children)
    expect_length(boxes,1L)
    expect_equal(alignseq_test_rgb(boxes[[1]]$gp$fill),alignseq_test_rgb(expected))
    texts <- Filter(function(x)inherits(x,"text"),g$children)
    bases <- Filter(function(x)identical(as.character(x$label),own$base),texts)[[1]]
    expect_equal(alignseq_test_rgb(bases$gp$col),
      alignseq_test_rgb(c("black","black","white","black","white","black","black")))
    if (mode=="markers") {
      points <- Filter(function(x)inherits(x,"points"),g$children)
      expect_equal(alignseq_test_rgb(points[[1]]$gp$col),
        alignseq_test_rgb(c("#E69F00","#D73027","#D73027")))
    }
  }
})

test_that("nucleotide colours stay available and difference palettes are customizable", {
  d <- alignseq_fixture(details=c(100,105),flank=0)
  d <- d[d$sequence=="human",]
  base <- ggplot2::ggplot()+geom_alignseq(data=d)
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(base))[[1]]
  boxes <- Filter(function(x)inherits(x,"rect"),g$children)
  expect_equal(alignseq_test_rgb(boxes[[1]]$gp$fill),
    alignseq_test_rgb(c("#D9EDD7","#D9EAF5","#EEEEEE","#FAEBC5","#F4DADB","#D9EDD7","#D9EAF5")))
  custom <- c(aligned="grey90",mismatch="purple",indel="gold")
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(ggplot2::ggplot()+
    geom_alignseq(data=d,colour_scheme="difference",difference_colours=custom)))[[1]]
  boxes <- Filter(function(x)inherits(x,"rect"),g$children)
  expect_equal(alignseq_test_rgb(boxes[[1]]$gp$fill),
    alignseq_test_rgb(c("grey90","purple","gold","grey90","gold","grey90","grey90")))
  expect_error(geom_alignseq(data=d,colour_scheme="other"),"nucleotide.*difference")
  expect_error(geom_alignseq(data=d,colour_scheme="difference",difference_colours=c(aligned="grey")),
    "difference_colours")
  expect_error(geom_alignseq(data=d,colour_scheme="difference",
    difference_colours=c(aligned="grey",mismatch="not-a-colour",indel="orange")),"difference_colours")
})

test_that("difference colours can highlight letters without nucleotide backgrounds", {
  d <- alignseq_fixture(details=c(100,105),flank=0)
  d <- d[d$sequence=="human",]
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(ggplot2::ggplot()+
    geom_alignseq(data=d,mode="rows",colour_scheme="difference",background=FALSE)))[[1]]
  expect_length(Filter(function(x)inherits(x,"rect"),g$children),0L)
  texts <- Filter(function(x)inherits(x,"text"),g$children)
  bases <- Filter(function(x)identical(as.character(x$label),d$base),texts)[[1]]
  expect_equal(alignseq_test_rgb(bases$gp$col),
    alignseq_test_rgb(c("grey45","#E69F00","#D73027","grey45","#D73027","grey45","grey45")))
})

test_that("difference glyph contrast accounts for translucent backgrounds", {
  d <- alignseq_fixture(details=c(100,105),flank=0)
  d <- d[d$sequence=="human",]
  palette <- c(aligned="#00000000",mismatch="#0072B21A",indel="transparent")
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(ggplot2::ggplot()+
    geom_alignseq(data=d,mode="rows",colour_scheme="difference",
      difference_colours=palette)))[[1]]
  texts <- Filter(function(x)inherits(x,"text"),g$children)
  bases <- Filter(function(x)identical(as.character(x$label),d$base),texts)[[1]]
  expect_equal(alignseq_test_rgb(bases$gp$col),alignseq_test_rgb(rep("black",7L)))
})

test_that("ambiguous bases retain neutral backgrounds without claiming a verified mismatch", {
  d <- alignseq_data(c(h="ANRY",q="ATRY"),
    data.frame(sequence=c("h","q"),start=c(100,200),strand="+"),
    reference="h",details=c(100,103),flank=0)
  d <- d[d$sequence=="h",]
  g <- alignseq_test_grobs(ggplot2::ggplotGrob(ggplot2::ggplot()+
    geom_alignseq(data=d,mode="rows",colour_scheme="difference")))[[1]]
  boxes <- Filter(function(x)inherits(x,"rect"),g$children)
  expect_equal(alignseq_test_rgb(boxes[[1]]$gp$fill),alignseq_test_rgb(rep("#E6E6E6",4L)))
  expect_equal(d$event_type,c("match","ambiguous","ambiguous","ambiguous"))
})
