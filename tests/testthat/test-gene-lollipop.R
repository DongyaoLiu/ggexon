test_that("gene-lollipop packing is compact, ordered, bounded and lossless", {
  anchors <- c(3, rep(35, 14), 75, 98)
  d <- .gene_lollipop_pack(anchors, 12, 100, gap_mm = 1, max_shift_mm = 20)
  expect_equal(nrow(d), length(anchors))
  expect_identical(d$anchor_x_mm, anchors)
  expect_true(all(diff(d$head_x_mm) >= -1e-8))
  expect_true(all(abs(d$head_x_mm - anchors) <= 20 + 1e-8))
  expect_true(all(d$left_mm >= -1e-8 & d$right_mm <= 100 + 1e-8))
  for (lane in unique(d$row)) {
    z <- d[d$row == lane, ]
    expect_true(all(diff(z$head_x_mm) >= 13 - 1e-8))
  }
  expect_error(.gene_lollipop_pack(anchors, 12, 100, max_shift_mm = 20,
                                   gap_mm = 1, max_rows = max(d$row) - 1), "fit")
})

test_that("gene-lollipop layout preserves input rows and rejects impossible capacity", {
  a <- c(90, 10, 50)
  d <- .gene_lollipop_pack(a, 10, 100, max_shift_mm = 10)
  expect_identical(d$anchor_x_mm, a)
  expect_equal(d$row, rep(1L, 3))
  expect_equal(d$head_x_mm, a)
  expect_equal(nrow(.gene_lollipop_pack(numeric(), 10, 100)), 0)
  expect_error(.gene_lollipop_pack(20, 101, 100), "width")
  expect_error(.gene_lollipop_pack(c(1, 1), 10, 100, max_shift_mm = 0), "fit")
  expect_error(.gene_lollipop_pack(c(NA_real_, 1), 10, 100), "finite")
})

.lollipop_test_render <- function(p, width = 160, height = 100) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width / 25.4, height = height / 25.4)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

test_that("gene arrows have physical sizes, internal labels and exact anchors", {
  capture <- list()
  genes <- data.frame(x = c(1e6, 1.001e6, 1.002e6), y = 0,
                      strand = c("+", "-", "*"), label = c("A", "DRB1*", "?"))
  p <- ggplot2::ggplot() + geom_gene_lollipop(
    data = genes, ggplot2::aes(x, y, strand = strand, label = label),
    headroom = 8, size = 8 / ggplot2::.pt,
    layout_callback = function(d) capture[[length(capture) + 1L]] <<- d
  ) + ggplot2::scale_x_continuous(limits = c(0, 6e6))
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[1]]$x, genes$x)
  expect_equal(built$data[[1]]$y, genes$y)
  expect_equal(built$data[[1]]$label, genes$label)
  expect_equal(built$data[[1]]$genomic_x, genes$x)
  expect_no_warning(.lollipop_test_render(p))
  d <- capture[[length(capture)]]
  expect_equal(d$genomic_x, genes$x)
  expect_equal(d$direction, c(1, -1, 0))
  expect_equal(length(unique(d$head_width_mm)), 1)
  expect_true(all(d$label_width_mm + 2 * .6 + 1.8 <= d$head_width_mm + 1e-8))
  expect_true(all(d$bottom_mm > d$anchor_y_mm))
  expect_true(all(d$top_mm <= d$panel_height_mm + 1e-8))
  capture <- list()
  .lollipop_test_render(p + ggplot2::scale_x_continuous(limits = c(0, 12e6)))
  z <- capture[[length(capture)]]
  expect_equal(z$head_width_mm, d$head_width_mm)
  expect_equal(z$head_height_mm, d$head_height_mm)
  expect_equal(z$genomic_x, genes$x)
})

test_that("gene-lollipop directions reverse once and keep original coordinates", {
  out <- list()
  genes <- data.frame(x = c(1, 2), y = 0, strand = c("+", "-"), label = c("A", "B"))
  p <- ggplot2::ggplot() + geom_gene_lollipop(
    data = genes, ggplot2::aes(x, y, strand = strand, label = label),
    layout_callback = function(d) out[[length(out) + 1L]] <<- d)
  .lollipop_test_render(p + ggplot2::scale_x_reverse())
  expect_equal(out[[length(out)]]$direction, c(-1, 1))
  expect_equal(out[[length(out)]]$genomic_x, genes$x)
  .lollipop_test_render(p + ggplot2::coord_cartesian(reverse = "x"))
  expect_equal(out[[length(out)]]$direction, c(-1, 1))
})

test_that("gene-lollipop facets pack independently with one common head size", {
  out <- list()
  d <- data.frame(track = rep(c("one", "two"), each = 5),
                  x = rep(seq(1e6, 1.01e6, length.out = 5), 2), y = 0,
                  strand = "+", label = rep(c("A", "DRB1*"), each = 5))
  p <- ggexon() + geom_gene_lollipop(data = d,
    ggplot2::aes(x, y, strand = strand, label = label, track = track),
    headroom = 8, layout_callback = function(z) out[[length(out) + 1L]] <<- z) +
    facet_genomics(ggplot2::vars(track), ncol = 1, scales = "free")
  .lollipop_test_render(p, height = 170)
  z <- do.call(rbind, out)
  expect_setequal(z$track, c("one", "two"))
  expect_length(unique(z$head_width_mm), 1)
  expect_equal(nrow(z), 10)
})

test_that("gene-lollipop Syn input selects true gene midpoints including unknown strands", {
  f <- tempfile(fileext = ".gff3")
  writeLines(c("##gff-version 3",
    "chr1\ttest\tgene\t101\t200\t.\t+\t.\tID=g1;Name=A",
    "chr1\ttest\tgene\t301\t500\t.\t-\t.\tID=g2;Name=DRB1"), f)
  on.exit(unlink(f), add = TRUE)
  obj <- load_annotation(SynIndividual(id = "hap", annotation_file = f, annotation_format = "gff"))
  p <- ggexon(obj) + geom_gene_lollipop()
  b <- ggexon_build(p)
  expect_equal(sort(b@data[[1]]$x), c(150.5, 400.5))
  expect_setequal(b@data[[1]]$label, c("A", "DRB1"))
  expect_true(all(b@data[[1]]$track == "hap"))
})

test_that("visible unknown-strand genes survive and coordinate zoom removes only anchors outside", {
  out <- list()
  d <- data.frame(x = c(1, 2, 3), y = 0, strand = c(NA, "+", "-"),
                  label = c(NA, "B", "C"), transcript_id = c("a", "b", "c"))
  p <- ggplot2::ggplot() + geom_gene_lollipop(data = d,
    layout_callback = function(z) out[[length(out) + 1L]] <<- z) +
    ggplot2::coord_cartesian(xlim = c(.5, 2.5), expand = FALSE)
  expect_no_warning(.lollipop_test_render(p))
  z <- out[[length(out)]]
  expect_equal(z$genomic_x, c(1, 2))
  expect_equal(z$direction, c(0, 1))
  expect_equal(nrow(z), 2)
})

test_that("insufficient headroom and undersized heads report actionable errors", {
  d <- data.frame(x = c(1, 1), y = 0, strand = "+", label = "DRB1*")
  p <- ggplot2::ggplot() + geom_gene_lollipop(data = d, head_width_mm = 1)
  expect_error(.lollipop_test_render(p), "width.*labels")
  p <- ggplot2::ggplot() + geom_gene_lollipop(data = d) +
    ggplot2::scale_y_continuous(limits = c(0, 100))
  expect_error(.lollipop_test_render(p), "vertically")
})

test_that("inherited renamed mappings retain raw positions under a reverse scale", {
  d <- data.frame(pos = c(1, 2), base = 0, s = c("+", "-"), lab = c("A", "B"))
  out <- list()
  p <- ggplot2::ggplot(d, ggplot2::aes(pos, base, strand = s, label = lab)) +
    geom_gene_lollipop(inherit.aes = TRUE,
      layout_callback = function(z) out[[length(out) + 1L]] <<- z) +
    ggplot2::scale_x_reverse()
  .lollipop_test_render(p)
  expect_equal(out[[length(out)]]$genomic_x, d$pos)
  expect_equal(out[[length(out)]]$direction, c(-1, 1))
})

test_that("head outline clearance is included at panel edges", {
  out <- list()
  d <- data.frame(x = c(0, 100), y = 0, strand = c("+", "-"), label = c("A", "B"))
  p <- ggplot2::ggplot() + geom_gene_lollipop(data = d, linewidth = 2,
    layout_callback = function(z) out[[length(out) + 1L]] <<- z) +
    ggplot2::coord_cartesian(xlim = c(0, 100), expand = FALSE)
  .lollipop_test_render(p)
  z <- out[[length(out)]]
  expect_true(all(z$left_mm >= 1))
  expect_true(all(z$right_mm <= z$panel_width_mm - 1))
})

test_that("ordered columns group consecutive genes top to bottom with bounded shifts", {
  z <- .gene_lollipop_columns(rep(35, 8), 12, 70, gap_mm = 1,
    max_shift_mm = 20, max_rows = 4, lane_spacing_mm = .65, lane_clearance_mm = .5)
  expect_equal(z$column, c(1, 1, 1, 2, 2, 2, 3, 3))
  expect_equal(z$row, c(3, 2, 1, 3, 2, 1, 3, 2))
  expect_equal(z$display_order, 1:8)
  expect_equal(z$anchor_x_mm, rep(35, 8))
  expect_true(all(abs(z$head_x_mm - z$anchor_x_mm) <= 20 + 1e-8))
  expect_true(all(z$channel_x_mm < z$left_mm))
  expect_true(all(diff(z$channel_x_mm) > 0))
  expect_true(all(z$left_mm >= 0 & z$right_mm <= 70))
  for (col in unique(z$column)) {
    d <- z[z$column == col, ]
    expect_equal(length(unique(d$head_x_mm)), 1)
  }
  expect_error(.gene_lollipop_columns(rep(35, 8), 12, 70,
    max_shift_mm = 20, max_rows = 2), "fit")
  a <- c(80, 20, 50)
  u <- .gene_lollipop_columns(a, 10, 100, max_shift_mm = 20)
  expect_equal(u$anchor_x_mm, a)
  expect_equal(u$display_order, c(3, 1, 2))
  expect_equal(nrow(.gene_lollipop_columns(numeric(), 10, 100)), 0)
})

# A proper intersection excludes an unavoidable shared genomic anchor.
.lollipop_proper_crossing <- function(a, b, c, d) {
  cross <- function(u, v, w) (v[1]-u[1])*(w[2]-u[2])-(v[2]-u[2])*(w[1]-u[1])
  cross(a,b,c)*cross(a,b,d) < -1e-10 && cross(c,d,a)*cross(c,d,b) < -1e-10
}
.lollipop_route_vertices <- function(z, i) {
  cbind(c(z$anchor_x_mm[i], z$anchor_x_mm[i], z$channel_x_mm[i],
          z$channel_x_mm[i], z$left_mm[i]),
        c(z$anchor_y_mm[i], z$tick_top_mm[i], z$fan_top_mm[i],
          z$head_y_mm[i], z$head_y_mm[i]))
}

test_that("column leaders avoid other heads and preserve visual genomic order", {
  out <- list()
  genes <- data.frame(x = c(20,20.1,20.2,20.3,20.4,50,50.1,50.2), y = 0,
    strand = rep(c("+","-"),4), label = LETTERS[1:8])
  p <- ggplot2::ggplot() + geom_gene_lollipop(data = genes,
    head_layout = "columns", head_width_mm = 12, fill = "#FFFF00",
    max_shift_mm = 20, layout_callback = function(z) out[[length(out)+1L]] <<- z) +
    ggplot2::scale_x_continuous(limits = c(0,100))
  expect_no_warning(.lollipop_test_render(p))
  z <- out[[length(out)]]
  expect_equal(z$genomic_x, genes$x)
  expect_equal(z$display_order, seq_len(nrow(genes)))
  expect_true(all(z$head_layout == "columns"))
  expect_true(all(z$stem_colour == "#999900"))
  expect_true(all(z$tick_top_mm > z$anchor_y_mm))
  expect_true(all(z$fan_top_mm > z$tick_top_mm))
  expect_true(all(z$bottom_mm > z$fan_top_mm))
  paths <- lapply(seq_len(nrow(z)), function(i) .lollipop_route_vertices(z,i))
  for (i in seq_len(nrow(z))) {
    # Horizontal port, vertical lane, and fan are all outside other heads.
    for (j in setdiff(seq_len(nrow(z)),i)) {
      v <- paths[[i]]
      for (k in seq_len(nrow(v)-1)) {
        if (max(v[k:(k+1),2]) <= z$bottom_mm[j]) next
        xmid <- (v[k,1]+v[k+1,1])/2
        ymid <- (v[k,2]+v[k+1,2])/2
        expect_false(xmid > z$left_mm[j] && xmid < z$right_mm[j] &&
                       ymid > z$bottom_mm[j] && ymid < z$top_mm[j])
      }
    }
    if (i == nrow(z)) next
    for (j in (i+1):nrow(z)) {
      a <- paths[[i]]; b <- paths[[j]]
      for (s in 1:4) for (t in 1:4)
        expect_false(.lollipop_proper_crossing(a[s,],a[s+1,],b[t,],b[t+1,]))
    }
  }
})

test_that("fully transparent gene layers leave no opaque leader halo", {
  p <- ggplot2::ggplot() + geom_gene_lollipop(
    data = data.frame(x=50,y=0,strand="+",label="A"),
    head_layout="columns", alpha=0, stem_colour="red") +
    ggplot2::scale_x_continuous(limits=c(0,100))
  b <- ggplot2::ggplot_build(p)
  file <- tempfile(fileext=".pdf")
  grDevices::pdf(file,width=5,height=4)
  on.exit({grDevices::dev.off();unlink(file)},add=TRUE)
  g <- GeomGeneLollipop$draw_panel(b$data[[1]],b$layout$panel_params[[1]],
    b$plot$coordinates,head_layout="columns")
  content <- grid::makeContent(g)
  expect_true(all(grepl("00$", content$children[[1]]$gp$col)))
})

test_that("fixed head rows retain status/copy slots and ordered gutter routes", {
  anchors <- c(20,20.1,30,50,50.1)
  rows <- c(6,2,2,5,1)
  z <- .gene_lollipop_columns(anchors,10,100,max_rows=6,head_row=rows)
  expect_equal(z$row, rows)
  expect_equal(z$column, c(1,1,2,3,3))
  expect_equal(z$anchor_x_mm, anchors)
  expect_true(all(diff(z$channel_x_mm)>0))
  expect_true(all(abs(z$head_x_mm-anchors)<=25+1e-8))
  u <- .gene_lollipop_columns(rev(anchors),10,100,head_row=rev(rows))
  expect_equal(u$row,rev(rows))
  expect_equal(u$column,rev(z$column))
  expect_error(.gene_lollipop_columns(anchors,10,100,head_row=c(6,NA,2,5,1)),"head_row")
  expect_error(.gene_lollipop_columns(anchors,10,100,max_rows=5,head_row=rows),"fit")
})

test_that("shared row metadata keeps matching genes and pseudogenes aligned across facets", {
  capture <- list()
  genes <- data.frame(track=c(rep("one",4),rep("two",5)),
    x=c(20,25,40,50,20,25,25.1,40,50), y=0, strand="+",
    label=c("F","B","DRA","DQA1","F*","B*","B","DRA","DQA1"),
    head_row=c(6,3,6,1,6,3,3,6,1))
  p <- ggexon() + geom_gene_lollipop(data=genes,head_layout="columns",
    headroom=8,layout_callback=function(z)capture[[length(capture)+1L]]<<-z) +
    facet_genomics(ggplot2::vars(track),ncol=1,scales="free") +
    ggplot2::scale_x_continuous(limits=c(0,100))
  expect_no_warning(.lollipop_test_render(p,height=200))
  z <- do.call(rbind,capture)
  expect_equal(z$row,genes$head_row)
  expect_true(all(z$row_is_fixed))
  expect_equal(z$genomic_x,genes$x)
  for (row in unique(z$row)) {
    offset <- z$head_y_mm[z$row==row]-z$anchor_y_mm[z$row==row]
    expect_equal(max(offset)-min(offset),0,tolerance=1e-8)
  }
  expect_equal(nrow(z),9)
})

test_that("fixed-row packing splits a group whose gutter cannot fit edge anchors", {
  z <- .gene_lollipop_columns(c(0,12),10,100,max_shift_mm=6,head_row=c(3,2))
  expect_equal(z$column,c(1,2))
  expect_equal(z$row,c(3,2))
  expect_true(all(abs(z$head_x_mm-c(0,12))<=6+1e-8))
  expect_true(all(z$channel_x_mm>=0))
  expect_true(z$right_mm[1]<z$channel_x_mm[2])
})

test_that("shared rows keep a common physical pitch when outline widths vary by facet", {
  out<-list()
  d<-data.frame(track=rep(c("one","two"),each=2),x=rep(c(20,50),2),y=0,
    strand="+",label=rep(c("A","B"),2),head_row=rep(c(1,2),2),
    linewidth=rep(c(.1,.3),each=2))
  p<-ggexon()+geom_gene_lollipop(data=d,
    ggplot2::aes(x,y,strand=strand,label=label,head_row=head_row,track=track,
                 linewidth=I(linewidth)),head_layout="columns",
    layout_callback=function(z)out[[length(out)+1L]]<<-z)+
    facet_genomics(ggplot2::vars(track),ncol=1,scales="free")+
    ggplot2::scale_x_continuous(limits=c(0,100))
  .lollipop_test_render(p,height=150)
  z<-do.call(rbind,out);b<-z[z$label=="B",]
  expect_equal(diff(b$head_y_mm-b$anchor_y_mm),0,tolerance=1e-8)
})

test_that("fixed-row packing can split a group after preceding-column separation", {
  z<-.gene_lollipop_columns(c(0,11,25),10,100,max_shift_mm=7,head_row=c(3,3,2))
  expect_equal(z$column,1:3)
  expect_equal(z$row,c(3,3,2))
  expect_true(all(abs(z$head_x_mm-c(0,11,25))<=7+1e-8))
  expect_true(all(diff(z$head_x_mm)>=11.8-1e-8))
})

test_that("fixed-row mode rejects a whole facet with missing row assignments", {
  d<-data.frame(track=c("one","two"),x=20,y=0,strand="+",label="A",head_row=c(2,NA))
  p<-ggexon()+geom_gene_lollipop(data=d,head_layout="columns")+
    facet_genomics(ggplot2::vars(track),ncol=1,scales="free")
  expect_error(.lollipop_test_render(p,height=150),"head_row.*every gene")
})
