cd44_object_fixture <- function(demo_dir = system.file(
  "extdata", "cd44_pairwise_ensembl116", package = "ggexon")) {
  env <- new.env(parent = globalenv())
  source(file.path(demo_dir, "load-cd44-example.R"), local = env)
  env$load_cd44_example(demo_dir)
}

test_that("exon resolution preserves feature metadata without overriding layout columns", {
  gr <- GenomicRanges::GRanges("chr1", IRanges::IRanges(c(10, 30), width = 5),
    strand = "+", type = "exon", transcript_id = c("t1", "t2"),
    exon_role = c("common", "variable"), isoform_y = c(0.2, 0.9),
    xmin = c(-100, -200), ymin = c(-3, -4), PANEL = c(99, 99))
  out <- ggexon:::syn_gr_to_exon_df(gr, track = "human")
  idx <- match(c("t1", "t2"), out$transcript_id)
  expect_equal(out$exon_role[idx], c("common", "variable"))
  expect_equal(out$isoform_y[idx], c(0.2, 0.9))
  expect_equal(out$xmin[idx], c(10L, 30L))
  expect_true(all(out$ymin > 0))
  expect_equal(unique(out$PANEL), 1L)
})

test_that("a saved CD44 comparison can plot without its source files", {
  demo_dir <- system.file("extdata", "cd44_pairwise_ensembl116", package = "ggexon")
  input <- withr::local_tempdir(pattern = "cd44-source-")
  expect_true(all(file.copy(list.files(demo_dir, full.names = TRUE), input, recursive = TRUE)))
  cd44 <- cd44_object_fixture(input)
  rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(cd44, rds)
  unlink(input, recursive = TRUE)
  restored <- readRDS(rds)
  p <- ggexon(restored) +
    geom_exon(ggplot2::aes(fill = exon_role, ymin = isoform_y)) +
    geom_nuclink(alignment = "human_mouse") +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1)
  built <- ggexon_build(p)
  expect_equal(nrow(built@data[[1]]), nrow(restored@metadata$tables$cd44_selected_exons))
  expect_equal(length(unique(built@data[[2]]$group)), 25L)
  expect_s3_class(ggexon_gtable(built), "gtable")
})

test_that("CD44 object stores annotations, normalized links, and supporting data", {
  cd44 <- cd44_object_fixture()
  expect_s4_class(cd44, "SynSpecies")
  expect_equal(individual_names(cd44), c("human", "mouse"))
  expect_s4_class(cd44@layout, "SynLayout")
  panels <- cd44@layout@panels
  expect_equal(as.character(panels$track), c("human", "link_human_mouse", "mouse"))
  annotation_panels <- panels[panels$panel_type == "annotation", ]
  expect_equal(annotation_panels$xlim_min, cd44@metadata$species$window_start)
  expect_equal(annotation_panels$xlim_max, cd44@metadata$species$window_end)
  tables <- cd44@metadata$tables
  demo_dir <- system.file("extdata", "cd44_pairwise_ensembl116", package = "ggexon")
  expect_setequal(names(tables), sub("\\.tsv$", "", list.files(demo_dir, pattern = "\\.tsv$")))
  for (id in individual_names(cd44)) {
    ind <- cd44@individuals[[id]]
    gr <- annotation_data(ind)
    expected <- tables$cd44_selected_exons[tables$cd44_selected_exons$species == id, ]
    exons <- gr[as.character(S4Vectors::mcols(gr)$type) == "exon"]
    expect_equal(length(exons), nrow(expected))
    expect_setequal(as.character(S4Vectors::mcols(exons)$transcript_id), expected$transcript_id)
    expect_true(all(!is.na(S4Vectors::mcols(exons)$exon_role)))
    window <- ind@metadata$sequence_window
    expect_s4_class(window$sequence, "DNAStringSet")
    expect_equal(as.integer(BiocGenerics::width(window$sequence)), as.integer(window$end - window$start + 1L))
    expect_true(is.na(ind@genome_file))
  }
  links <- pairwise_alignment_data(cd44, alignment = "human_mouse")
  expected <- tables$cd44_nuclinks_lastz
  cols <- c("tchr", "tstart", "tend", "qchr", "qstart", "qend", "identity")
  expect_equal(links[, cols], expected[, cols])
  expect_equal(as.character(links$identity_bin), expected$identity_bin)
  expect_equal(levels(links$identity_bin),
               c("50-55%", "55-60%", "60-65%", "65-70%", ">=70%"))
  expect_equal(links$alen, expected$alignment_length)
  filtered <- pairwise_alignment_data(cd44, alignment = "human_mouse", filter = 200)
  expect_equal(nrow(filtered), sum(expected$alignment_length >= 200))
  expect_true(all(links$tstart >= cd44@metadata$species$window_start[1]))
  expect_gte(nrow(cd44@metadata$raw_paf), nrow(tables$cd44_nuclinks_lastz))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(cd44, path)
  restored <- readRDS(path)
  expect_equal(pairwise_alignment_data(restored, alignment = "human_mouse"), links)
  expect_equal(annotation_data(restored@individuals$human), annotation_data(cd44@individuals$human))
})

test_that("CD44 plots resolve their exon and link data from the object", {
  cd44 <- cd44_object_fixture()
  p <- ggexon(cd44) +
    geom_nuclink(alignment = "human_mouse",
                 mapping = ggplot2::aes(fill = identity_bin)) +
    geom_exon(mapping = ggplot2::aes(ymin = isoform_y, fill = exon_role,
                                    transcripts = transcript_name), exon_height = 0.5) +
    facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1,
      reverse_x = "mouse", reverse_x_match_by = "track")
  built <- ggexon_build(p)
  expect_equal(as.character(built@layout$layout$track),
               c("human", "link_human_mouse", "mouse"))
  expect_s4_class(built@plot@data, "SynSpecies")
  expected <- cd44@metadata$tables$cd44_selected_exons
  exons <- built@data[[2]]
  expect_equal(nrow(exons), nrow(expected))
  for (id in c("human", "mouse")) {
    actual <- exons[exons$track == id, ]
    reference <- expected[expected$species == id, ]
    idx <- match(paste(actual$transcripts, actual$xmin, actual$xmax),
                 paste(reference$transcript_name, reference$xmin, reference$xmax))
    expect_false(anyNA(idx))
    expect_equal(actual$ymin, reference$ymin[idx])
    expect_equal(length(unique(actual$transcripts)), 4L)
  }
  expect_equal(length(unique(built@data[[1]]$group)),
               nrow(cd44@metadata$tables$cd44_nuclinks_lastz))
  expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  table <- ggexon_gtable(built)
  legends <- table$grobs[grepl("^guide-box", table$layout$name)]
  expect_true(any(!vapply(legends, inherits, logical(1), "zeroGrob")))
})
