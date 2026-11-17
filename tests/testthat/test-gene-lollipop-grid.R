test_that("shared grid cells keep their centres when other genes are absent", {
  full <- .gene_lollipop_grid(c(10, 60, 110), 12, 120,
    head_row = c(2, 1, 2), head_col = 1:3, nrow = 2, ncol = 3,
    max_shift_mm = 120, lane_spacing_mm = .6, lane_clearance_mm = .5)
  expect_equal(full$head_x_mm, c(7.6, 60.8, 114))
  expect_equal(full$row, c(2L, 1L, 2L))
  expect_equal(full$column, 1:3)
  sparse <- .gene_lollipop_grid(c(10, 110), 12, 120,
    head_row = c(2, 2), head_col = c(1, 3), nrow = 2, ncol = 3,
    max_shift_mm = 120, lane_spacing_mm = .6, lane_clearance_mm = .5)
  expect_equal(sparse$head_x_mm, c(7.6, 114))
  expect_equal(sparse$channel_x_mm, full$channel_x_mm[c(1, 3)])
  expect_equal(sparse$anchor_x_mm, c(10, 110))
})

test_that("shared grid drawing rejects collisions and impossible physical fits", {
  draw <- function(...) .gene_lollipop_grid(c(10, 110), 12, 120,
    head_row = c(2, 1), head_col = c(1, 3), nrow = 2, ncol = 3, ...)
  expect_error(draw(max_shift_mm = 0), "shift|displacement")
  expect_error(.gene_lollipop_grid(c(10, 10), 12, 120,
    head_row = c(2, 2), head_col = c(1, 1), nrow = 2, ncol = 3), "cell|slot")
  expect_error(.gene_lollipop_grid(10, 12, 30,
    head_row = 2, head_col = 1, nrow = 2, ncol = 3), "width|fit")
  expect_error(.gene_lollipop_grid(10, 12, 120,
    head_row = 3, head_col = 1, nrow = 2, ncol = 3), "row|grid")
  expect_error(.gene_lollipop_grid(10, 12, 120,
    head_row = 1, head_col = 1, nrow = 2, ncol = 0), "ncol|dimension")
})

.grid_test_render <- function(p, width = 180, height = 120) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width / 25.4, height = height / 25.4)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(ggexon_gtable(ggexon_build(p)))
}

test_that("shared heads align across sparse reversed panels without moving anchors", {
  d <- data.frame(track = c(rep("ref", 3), rep("query", 2)),
    x = c(150.5, 350.5, 550.5, 150.5, 550.5), y = 0, strand = "+",
    label = c("A", "LONGEST", "C", "A", "C"),
    head_row = c(2, 1, 2, 2, 2), head_col = c(1, 1, 3, 1, 3),
    grid_nrow = 2, grid_ncol = 3, lollipop_grid = "shared",
    slot_key = c("A", "B", "C", "A", "C"))
  captures <- list()
  p <- ggexon() + geom_gene_lollipop(data = d, headroom = 8,
    head_layout = "columns", max_shift_mm = 200,
    layout_callback = function(z) captures[[length(captures) + 1L]] <<- z) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1,
      reverse_x = "query") +
    ggplot2::scale_x_continuous(limits = c(0, 700)) +
    ggplot2::scale_y_continuous(limits = c(-1, 9))
  .grid_test_render(p)
  z <- do.call(rbind, captures)
  expect_equal(nrow(z), 5L)
  z <- z[match(paste(d$track, d$label), paste(z$track, z$label)), ]
  for (key in c("A", "C")) {
    same <- z[z$label == key, ]
    expect_equal(same$head_x_mm[1], same$head_x_mm[2])
    expect_equal(same$head_y_mm[1], same$head_y_mm[2])
  }
  expect_equal(z$genomic_x, d$x)
  expect_equal(z$direction, c(1, 1, 1, -1, -1))
  expect_equal(z$column, c(1L, 1L, 3L, 1L, 3L))
  expect_equal(z$slot_key, d$slot_key)
  expect_length(unique(z$head_width_mm), 1L)
  expect_error(.grid_test_render(p + ggplot2::ylim(-1, 100)), "vertically|height")
})

.grid_test_species <- function() {
  species <- SynSpecies(name = "grid")
  for (id in c("ref", "query")) {
    keys <- if (id == "ref") c("A", "B", "C") else c("A", "C")
    starts <- if (id == "ref") c(101, 301, 501) else c(101, 501)
    file <- tempfile(fileext = ".gff3")
    lines <- vapply(seq_along(keys), function(i) paste0(
      "chr1\ttest\tgene\t", starts[i], "\t", starts[i] + 99,
      "\t.\t+\t.\tID=", id, "-", keys[i], ";Name=", keys[i],
      ";shared=", keys[i], ";role=selected;plot_fill=%23CC3311"), character(1))
    writeLines(c("##gff-version 3", lines,
      "chr1\ttest\tgene\t701\t800\t.\t+\t.\tID=background;Name=BACKGROUND;role=context"), file)
    individual <- load_annotation(SynIndividual(id = id, annotation_file = file,
      annotation_format = "gff"))
    species <- add_individual(species, individual)
    unlink(file)
  }
  species
}

test_that("the native Syn geom discovers prepared genes and rejects ambiguous names", {
  model <- prepare_gene_lollipop(.grid_test_species(), name = "genes",
    reference = "ref", key = "shared", where = list(role = "selected"), nrow = 2)
  p <- ggexon(model) + geom_gene_lollipop(y = .3, headroom = 8, max_shift_mm = 200) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", reverse_x = "query")
  built <- ggexon_build(p)
  expect_equal(nrow(built@data[[1]]), 5L)
  expect_equal(built@data[[1]]$y, rep(.3, 5))
  expect_equal(built@data[[1]]$ymax, rep(8.3, 5))
  expect_setequal(built@data[[1]]$label, c("A", "B", "C"))
  expect_equal(sort(built@data[[1]]$genomic_x), c(150.5, 150.5, 350.5, 550.5, 550.5))
  expect_true(all(to_upper_ascii(as.character(built@data[[1]]$fill)) == "#CC3311"))
  expect_true(all(built@data[[1]]$head_col[built@data[[1]]$label == "C"] == 2L))
  expect_error(ggexon_build(ggexon(model) +
    geom_gene_lollipop(ggplot2::aes(x = x + 10))), "remap|prepar")
  for (fixed in list(list(x = 999), list(genomic_x = 999), list(head_col = 3),
      list(slot_key = "wrong"), list(strand = "-"))) {
    expect_error(ggexon_build(ggexon(model) + do.call(geom_gene_lollipop, fixed)), "prepar|Prepared")
  }

  two <- prepare_gene_lollipop(model, name = "second", reference = "ref",
    key = "shared", where = list(role = "selected"), nrow = 2)
  expect_error(ggexon_build(ggexon(two) + geom_gene_lollipop()), "multiple|Multiple|name")
  explicit <- ggexon_build(ggexon(two) + geom_gene_lollipop(lollipop_layout = "genes"))
  expect_equal(nrow(explicit@data[[1]]), 5L)
  expect_error(geom_gene_lollipop(data = data.frame(x = 1, y = 0, strand = "+"),
    lollipop_layout = "genes"), "Syn")
  expect_error(ggexon_build(ggexon(data.frame(x = 1, y = 0, strand = "+")) +
    geom_gene_lollipop(lollipop_layout = "genes")), "Syn")
})

test_that("cropping a Syn panel preserves prepared slots and grid centres", {
  model <- prepare_gene_lollipop(.grid_test_species(), reference = "ref",
    key = "shared", where = list(role = "selected"), nrow = 2, ncol = 3)
  slots <- query_gene_lollipop(model, component = "genes")
  model <- store_chain_layout(model, free = list(x = TRUE, y = FALSE))
  model <- set_panel_xlim(model, individual = "query", xlim = c(400, 650), xlim_chr = "chr1")
  built <- ggexon_build(ggexon(model) +
    geom_gene_lollipop(headroom = 8, max_shift_mm = 200) +
    facet_genomics(ggplot2::vars(track), scales = "free_x"))
  data <- built@data[[1]]
  query <- data[data$individual == "query", ]
  expect_identical(as.character(query$label), "C")
  expect_equal(query$head_col, slots$head_col[slots$individual == "query" & slots$label == "C"])
  expect_equal(query$grid_ncol, 3)
  expect_equal(query$genomic_x, 550.5)
})
