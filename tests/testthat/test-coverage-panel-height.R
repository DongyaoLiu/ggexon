# These tests catch resizing the wrong role, changing signal values while
# resizing, and losing a user's explicit coverage/annotation row order.
coverage_height_plot <- function(height = NULL, panels = NULL,
                                 coverage_only = FALSE, annotation_only = FALSE) {
  tracks <- c("a", "b")
  annotation <- data.frame(track = tracks, xmin = c(100, 1000),
    xmax = c(140, 1040), y = 1, strand = "+", gene = c("g1", "g2"))
  signal <- data.frame(track = rep(tracks, each = 2),
    xmin = c(100, 120, 1000, 1020), xmax = c(119, 139, 1019, 1039),
    coverage = c(5, 12, 4, 8))
  link <- data.frame(track = "link_a_b", tspecies = "a", tchr = "X",
    tstart = 105, tend = 115, qspecies = "b", qchr = "X",
    qstart = 1005, qend = 1015, strand = "+", group = 1)
  if (is.null(panels)) {
    panels <- data.frame(PANEL = 1:5, ROW = 1:5, COL = 1L,
      track = c("a", "a", "link_a_b", "b", "b"),
      panel_type = c("coverage", "annotation", "link", "coverage", "annotation"),
      species = c("a", "a", NA, "b", "b"))
  }
  if (coverage_only) panels <- panels[panels$panel_type == "coverage", ]
  if (annotation_only) panels <- panels[panels$panel_type == "annotation", ]
  if (coverage_only || annotation_only) {
    panels$PANEL <- panels$ROW <- seq_len(nrow(panels))
  }
  panels$tspecies <- ifelse(panels$panel_type == "link", "a", NA_character_)
  panels$qspecies <- ifelse(panels$panel_type == "link", "b", NA_character_)
  species <- SynSpecies(name = "coverage heights")
  species_layout(species) <- SynLayout(panels = panels,
    free = list(x = TRUE, y = TRUE))
  p <- ggexon(species)
  if (!annotation_only) p <- p + geom_coverage(data = signal,
    ggplot2::aes(xmin = xmin, xmax = xmax, coverage = coverage, track = track),
    inherit.aes = FALSE)
  if (!coverage_only) p <- p + geom_genetag(data = annotation, show_label = FALSE)
  if (!coverage_only && !annotation_only) p <- p + geom_nuclink(data = link,
    ggplot2::aes(tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend,
      qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
      strand = strand, group = group), inherit.aes = FALSE)
  args <- list(facets = ggplot2::vars(track), scales = "free", ncol = 1,
    link_panel_height = 1, link_axis = "none", link_strip = "blank")
  if (!is.null(height)) args$coverage_panel_height <- height
  p + do.call(facet_genomics, args)
}

coverage_height_rows <- function(built) {
  layout <- as.data.frame(built@layout$layout)
  table <- ggplot2::ggplot_gtable(built)
  indices <- match(paste0("panel-", layout$COL, "-", layout$ROW), table$layout$name)
  stopifnot(!anyNA(indices))
  table$heights[table$layout$t[indices]]
}

test_that("coverage heights resize only coverage while retaining raw data and scales", {
  small <- ggexon_build(coverage_height_plot(.5))
  large <- ggexon_build(coverage_height_plot(2))
  layout <- as.data.frame(small@layout$layout)
  h <- coverage_height_rows(small)
  expect_equal(as.numeric(h[layout$panel_type == "coverage"]), c(.5, .5))
  expect_equal(as.numeric(h[layout$panel_type != "coverage"]), c(1, 1, 1))
  expect_true(all(grid::unitType(h) == "null"))
  expect_equal(as.numeric(coverage_height_rows(large)[layout$panel_type == "coverage"]), c(2, 2))
  expect_equal(small@data[[1L]]$coverage, c(5, 12, 4, 8))
  expect_equal(small@data, large@data)
  expect_equal(lapply(small@layout$panel_params, function(p) p$x.range),
    lapply(large@layout$panel_params, function(p) p$x.range))
  expect_equal(lapply(small@layout$panel_params, function(p) p$y.range),
    lapply(large@layout$panel_params, function(p) p$y.range))
})

test_that("complete stored single-column role layouts remain interleaved", {
  built <- ggexon_build(coverage_height_plot())
  layout <- as.data.frame(built@layout$layout)
  expect_identical(as.character(layout$panel_type),
    c("coverage", "annotation", "link", "coverage", "annotation"))
  expect_identical(as.character(layout$track), c("a", "a", "link_a_b", "b", "b"))
  expect_identical(layout$ROW, 1:5)
  coverage <- layout[layout$panel_type == "coverage", ]
  annotation <- layout[layout$panel_type == "annotation", ]
  expect_identical(as.integer(coverage$x_source_panel), as.integer(annotation$PANEL))
  expect_identical(coverage$SCALE_X, annotation$SCALE_X)
  link <- layout[layout$panel_type == "link", ]
  expect_identical(as.integer(link$t_panel), 2L)
  expect_identical(as.integer(link$q_panel), 5L)
})

test_that("stored ROW coordinates control interleaving after table serialization", {
  panels <- data.frame(PANEL = 1:5, ROW = 1:5, COL = 1L,
    track = c("a", "a", "link_a_b", "b", "b"),
    panel_type = c("coverage", "annotation", "link", "coverage", "annotation"),
    species = c("a", "a", NA, "b", "b"))
  panels <- panels[c(1, 4, 2, 3, 5), ]
  built <- ggexon_build(coverage_height_plot(.5, panels = panels))
  layout <- as.data.frame(built@layout$layout)
  expect_identical(as.character(layout$panel_type),
    c("coverage", "annotation", "link", "coverage", "annotation"))
  expect_identical(as.character(layout$track), c("a", "a", "link_a_b", "b", "b"))
  expect_identical(layout$ROW, 1:5)
  link <- layout[layout$panel_type == "link", ]
  expect_identical(as.integer(link$t_panel), 2L)
  expect_identical(as.integer(link$q_panel), 5L)
})

test_that("filtering a stored coverage track does not leave an empty panel row", {
  for (track in c("a", "b")) {
    plot <- coverage_height_plot(.5)
    signal <- plot@layers[[1L]]$data
    plot@layers[[1L]]$data <- signal[signal$track == track, ]
    built <- ggexon_build(plot)
    layout <- as.data.frame(built@layout$layout)
    expect_identical(layout$ROW, 1:4)
    table <- ggplot2::ggplot_gtable(built)
    expect_equal(sum(grepl("^panel-[0-9]+-[0-9]+$", table$layout$name)), 4L)
    annotation <- layout[layout$panel_type == "annotation", ]
    link <- layout[layout$panel_type == "link", ]
    expect_true(all(link$t_panel %in% annotation$PANEL))
    expect_true(all(link$q_panel %in% annotation$PANEL))
  }
})

test_that("coverage heights accept absolute grid units and coverage-only plots", {
  built <- ggexon_build(coverage_height_plot(grid::unit(8, "mm"), coverage_only = TRUE))
  h <- coverage_height_rows(built)
  expect_true(all(grid::unitType(h) == "mm"))
  expect_equal(as.numeric(h), c(8, 8))
})

test_that("coverage height defaults and absent coverage preserve ordinary row heights", {
  p <- coverage_height_plot()
  expect_true("coverage_panel_height" %in% names(formals(facet_genomics)))
  expect_null(p@facet$params$coverage_panel_height)
  expect_equal(as.numeric(coverage_height_rows(ggexon_build(p))), rep(1, 5))
  ann <- ggexon_build(coverage_height_plot(.25, annotation_only = TRUE))
  expect_equal(as.numeric(coverage_height_rows(ann)), c(1, 1))
})

test_that("coverage heights do not resize a row shared with annotation", {
  panels <- data.frame(PANEL = 1:5, ROW = c(1L, 2L, 3L, 2L, 3L),
    COL = c(1L, 1L, 1L, 2L, 2L), track = c("a", "a", "link_a_b", "b", "b"),
    panel_type = c("coverage", "annotation", "link", "coverage", "annotation"),
    species = c("a", "a", NA, "b", "b"))
  built <- ggexon_build(coverage_height_plot(.25, panels = panels))
  h <- coverage_height_rows(built)
  expect_equal(as.numeric(h), c(.25, 1, 1, 1, 1))
})

test_that("invalid coverage heights report the coverage argument", {
  for (value in list(0, -1, Inf, NA_real_, numeric(), c(.5, 1), "bad",
                     grid::unit(c(1, 2), "mm"))) {
    expect_error(facet_genomics(ggplot2::vars(track), coverage_panel_height = value),
      "coverage_panel_height")
  }
})

test_that("interleaved panels draw x-axis labels from their own genomic ranges", {
  plot <- coverage_height_plot(.5) + ggplot2::scale_x_continuous(
    breaks = c(100, 120, 140, 1000, 1020, 1040),
    labels = function(x) paste0("bp", x))
  built <- ggexon_build(plot)
  table <- ggplot2::ggplot_gtable(built)
  text_labels <- function(grob) {
    if (inherits(grob, "text")) return(as.character(grob$label))
    unlist(lapply(c(grob$children, grob$grobs), text_labels), use.names = FALSE)
  }
  for (row in c(4, 5)) {
    axis <- table$grobs[[match(paste0("axis-b-1-", row), table$layout$name)]]
    expect_identical(text_labels(axis), c("bp1000", "bp1020", "bp1040"))
  }
})
