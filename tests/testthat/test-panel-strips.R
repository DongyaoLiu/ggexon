individual_strip_plot <- function(side = "left", grouped = TRUE,
                                  coverage_height = .5, columns = FALSE) {
  panels <- data.frame(PANEL = 1:5, ROW = 1:5, COL = 1L,
    track = c("rna_a", "a", "link_a_b", "rna_b", "b"),
    panel_type = c("coverage", "annotation", "link", "coverage", "annotation"),
    individual = c("a", NA, NA, "b", NA),
    species = c(NA, "a", NA, NA, "b"),
    tspecies = c(NA, NA, "a", NA, NA),
    qspecies = c(NA, NA, "b", NA, NA))
  if (columns) {
    panels <- panels[panels$panel_type != "link", ]
    panels$PANEL <- 1:4
    panels$ROW <- c(1L, 2L, 1L, 2L)
    panels$COL <- c(1L, 1L, 2L, 2L)
  }
  model <- SynSpecies(name = "individual strip test")
  species_layout(model) <- SynLayout(panels = panels,
    free = list(x = TRUE, y = FALSE))
  annotation <- data.frame(track = c("a", "b"), xmin = c(100, 1000),
    xmax = c(140, 1040), y = 1, strand = "+", gene = c("g1", "g2"))
  signal <- data.frame(track = c("rna_a", "rna_b"), xmin = c(100, 1000),
    xmax = c(140, 1040), coverage = c(5, 12))
  facet_args <- list(facets = ggplot2::vars(track), scales = "free_x",
    ncol = if (columns) 2 else 1, strip.position = side,
    coverage_panel_height = coverage_height, link_axis = "none", link_strip = "blank",
    labeller = ggplot2::as_labeller(c(a = "Species A", b = "Species B",
      rna_a = "RNA A", rna_b = "RNA B", link_a_b = "")))
  if (grouped) facet_args$strip_group <- "individual"
  ggexon(model) +
    geom_genetag(data = annotation, show_label = FALSE) +
    geom_coverage(data = signal,
      ggplot2::aes(xmin = xmin, xmax = xmax, coverage = coverage, track = track),
      inherit.aes = FALSE) +
    do.call(facet_genomics, facet_args) +
    ggplot2::scale_y_continuous(position = side) +
    theme_ggexon_track(show_y_axis = TRUE) +
    theme_ggexon_side_strips(side)
}

strip_grob_labels <- function(grob) {
  labels <- if (inherits(grob, "text")) as.character(grob$label) else character()
  children <- c(grob$grobs %||% list(), as.list(grob$children %||% list()))
  c(labels, unlist(lapply(children, strip_grob_labels), use.names = FALSE))
}

test_that("individual strips span RNA aliases and annotation panels outside their axes", {
  for (side in c("left", "right")) {
    for (height in list(.35, grid::unit(9, "mm"))) {
      plot <- individual_strip_plot(side, coverage_height = height)
      built <- ggexon_build(plot)
      table <- ggexon_gtable(built)
      layout <- as.data.frame(built@layout$layout)
      strips <- which(grepl(paste0("^strip-", substr(side, 1, 1), "-individual-"),
        table$layout$name))
      expect_length(strips, 2L)
      expect_setequal(unlist(lapply(table$grobs[strips], strip_grob_labels)),
        c("Species A", "Species B"))
      for (id in c("a", "b")) {
        rows <- which(layout$species == id | layout$individual == id)
        rows <- rows[!is.na(rows)]
        panel_cells <- vapply(rows, function(i) panel_gtable_index(table,
          layout$COL[i], layout$ROW[i], n_panels = nrow(layout)), integer(1))
        label <- strips[vapply(table$grobs[strips], function(g)
          paste("Species", base::toupper(id)) %in% strip_grob_labels(g), logical(1))]
        expect_equal(table$layout$t[label], min(table$layout$t[panel_cells]))
        expect_equal(table$layout$b[label], max(table$layout$b[panel_cells]))
      }
      axes <- which(grepl(paste0("^axis-", substr(side, 1, 1)), table$layout$name))
      if (side == "left") {
        expect_true(all(table$layout$r[strips] < min(table$layout$l[axes])))
      } else {
        expect_true(all(table$layout$l[strips] > max(table$layout$r[axes])))
      }
    }
  }
})

test_that("individual strip grouping stays within layout columns", {
  plot <- individual_strip_plot(columns = TRUE)
  table <- ggexon_gtable(ggexon_build(plot))
  strips <- which(grepl("^strip-l-individual-", table$layout$name))
  expect_length(strips, 2L)
  expect_length(unique(table$layout$l[strips]), 2L)
  expect_true(all(table$layout$b[strips] > table$layout$t[strips]))
})

test_that("default facet strips remain separate for each role", {
  table <- ggexon_gtable(ggexon_build(individual_strip_plot(grouped = FALSE)))
  expect_false(any(grepl("^strip-l-individual-", table$layout$name)))
  labels <- unlist(lapply(table$grobs[grepl("^strip-l-", table$layout$name)], strip_grob_labels))
  expect_setequal(labels, c("Species A", "Species B", "RNA A", "RNA B"))
})

test_that("a link panel separates repeated runs of the same individual", {
  built <- ggexon_build(individual_strip_plot())
  layout <- built@layout$layout
  layout$individual[layout$panel_type == "coverage"] <- "same_individual"
  layout$species[layout$panel_type == "annotation"] <- "same_individual"
  built@layout$layout <- layout
  table <- ggexon_gtable(built)
  strips <- which(grepl("^strip-l-individual-", table$layout$name))
  expect_length(strips, 2L)
  expect_true(table$layout$b[strips[1]] < table$layout$t[strips[2]])
})

test_that("individual strip grouping requires side strips and a valid grouping mode", {
  expect_error(facet_genomics(ggplot2::vars(track), strip_group = "unknown"),
    "strip_group")
  expect_error(facet_genomics(ggplot2::vars(track), strip_group = "individual"),
    "left.*right|side")
})
