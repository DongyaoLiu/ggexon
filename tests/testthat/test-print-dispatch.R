test_that("printing a Syn plot preserves ggexon link-panel heights", {
  panels <- data.frame(PANEL = 1:3, ROW = 1:3, COL = 1L,
    track = c("a", "link_a_b", "b"),
    panel_type = c("annotation", "link", "annotation"),
    species = c("a", NA, "b"), tspecies = c(NA, "a", NA),
    qspecies = c(NA, "b", NA))
  model <- SynSpecies(name = "print dispatch")
  species_layout(model) <- SynLayout(panels, free = list(x = TRUE, y = FALSE))
  genes <- data.frame(track = c("a", "b"), xmin = c(1, 101),
    xmax = c(20, 120), y = 0.5, strand = "+")
  plot <- ggexon(model) + geom_genetag(data = genes, show_label = FALSE) +
    facet_genomics(ggplot2::vars(track), ncol = 1, scales = "free_x",
      link_panel_height = 0.25, link_axis = "none", link_strip = "blank")
  drawn <- grid::grid.grabExpr(print(plot), width = 7, height = 5)
  tables <- Filter(function(grob) inherits(grob, "gtable"), as.list(drawn$children))
  expect_length(tables, 1L)
  if (length(tables) != 1L) return()
  table <- tables[[1L]]
  cells <- match(c("panel-1-1", "panel-1-2", "panel-1-3"), table$layout$name)
  expect_false(anyNA(cells))
  expect_equal(as.numeric(table$heights[table$layout$t[cells]]), c(1, 0.25, 1))
})
