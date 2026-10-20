test_that("AS3MT offline inputs preserve ungapped blocks and top-two selection", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  example <- env$load_as3mt_example(demo_dir)
  expect_s4_class(example$comparison, "SynSpecies")
  expect_true(methods::validObject(example$comparison))
  expect_equal(nrow(example$links), 93L)
  expect_equal(nrow(example$selected_links), 37L)
  expect_setequal(as.character(unique(example$selected_links$identity_band)),
    c("97.1–98.3", "98.3–100.0"))
  expect_equal(example$selected_links$tend - example$selected_links$tstart + 1,
    example$selected_links$qend - example$selected_links$qstart + 1)
  raw <- readLines(file.path(demo_dir, "human_chimp.nogap.paf"))
  expect_length(raw, 124L)
  for (row in strsplit(raw, "\t")) {
    expect_true(paste0("cg:Z:", row[11], "M") %in% row[-seq_len(12)])
  }
  summary <- example$organoid_summary
  expect_equal(summary$n, c(7L, 11L, 7L, 11L))
  for (i in seq_len(nrow(summary))) {
    v <- subset(example$organoid_values,
      marker == summary$marker[i] & group == summary$group[i])$value
    expect_equal(mean(v), summary$mean[i], tolerance = 1e-6)
    expect_equal(stats::sd(v) / sqrt(length(v)), summary$sem[i], tolerance = 1e-6)
  }
})

test_that("AS3MT loads portably and uses native scaling at different compression factors", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  portable <- tempfile("as3mt-offline-")
  dir.create(portable)
  withr::defer(unlink(portable, recursive = TRUE))
  file.copy(list.files(demo_dir, full.names = TRUE), portable, recursive = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(portable, "load-as3mt-example.R"), envir = env)
  example <- env$load_as3mt_example(portable)
  original <- pairwise_alignment_data(example$comparison, alignment = "human_chimp")
  for (factor in c(10, 30)) {
    result <- env$plot_as3mt_example(example, intron_factor = factor)
    i <- which(vapply(result$plot$layers,
      function(layer) inherits(layer$geom, "GeomPanel"), logical(1)))
    vertices <- result$build$data[[i]]
    expect_equal(length(unique(vertices$parent_group)), 37L)
    expect_equal(length(unique(vertices$group)), 57L)
    bars <- result$build@layout$genomic_x_axis_data
    introns <- bars[bars$region_type == "intron", ]
    expect_equal(introns$plot_width * factor, introns$genomic_width, tolerance = 1e-5)
    expect_silent(ggplot2::ggplot_gtable(result$build))
  }
  expect_equal(pairwise_alignment_data(example$comparison, alignment = "human_chimp"), original)
})

test_that("AS3MT gapped DNA example retains verified event counts and explicit details", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  path <- file.path(demo_dir, "aligned-dna.rds")
  expect_true(file.exists(path))
  if (!file.exists(path)) return()
  a <- readRDS(path)
  d <- alignseq_data(a$aligned, a$anchors, reference = "human")
  h <- d[d$sequence == "human", ]
  expect_equal(sum(h$event_type == "match"), 31376L)
  expect_equal(sum(h$event_type == "substitution"), 468L)
  expect_equal(length(unique(h$event_id[h$event_type == "insertion"])), 28L)
  expect_equal(length(unique(h$event_id[h$event_type == "deletion"])), 39L)
  expect_equal(sum(h$event_type == "insertion"), 146L)
  expect_equal(sum(h$event_type == "deletion"), 489L)
  expect_equal(range(stats::na.omit(h$position)), c(102869567, 102901899))
  expect_true(all(a$details$start >= a$regions[1] & a$details$end <= a$regions[2]))
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  result <- env$plot_as3mt_alignseq(demo_dir = demo_dir)
  expect_silent(ggplot2::ggplot_gtable(ggexon_build(result$plot)))
  expect_silent(ggplot2::ggplot_gtable(ggexon_build(result$rows)))
})

test_that("AS3MT sequence view has two calibrated legend bars outside compact panels", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  x <- env$plot_as3mt_alignseq(demo_dir)
  expect_s3_class(x$grob, "gtable")
  expect_equal(nrow(x$scale_bars), 2L)
  if (is.null(x$scale_bars)) return()
  expect_equal(x$scale_bars$region_type, c("exon", "intron"))
  expect_equal(x$scale_bars$genomic_width, c(500, 15000))
  expect_equal(x$scale_bars$plot_width, c(500, 500))
  expect_equal(length(unique(x$scale_bars$fraction)), 1L)
  expect_equal(x$display_spans[1], x$display_spans[2], tolerance = 1e-6)
  expect_true(all(tapply(!is.na(x$data$detail_id), x$data$sequence, any)))
  b <- ggexon_build(x$plot)
  expect_true(is.null(b$layout$genomic_x_axis_data))
  expect_true(any(x$grob$layout$name == "as3mt-scale-legend"))
})

test_that("AS3MT sequence example highlights the experimentally supported VNTR", {
  demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
  path <- file.path(demo_dir, "vntr-dna.rds")
  expect_true(file.exists(path))
  if (!file.exists(path)) return()
  v <- readRDS(path)
  expect_equal(v$motif, "CCTGAGTCGCAGGCCGAGGAGACAGTGAGTGCGCGC")
  expect_equal(v$human_units$start, c(102869498, 102869534, 102869570))
  expect_equal(v$human_units$end, c(102869533, 102869569, 102869605))
  expect_equal(v$chimp_unit, "CCCGAGTCGCAGGCCGAGGAGACAGTGAGTTCGCGC")
  expect_equal(nchar(v$constructs$sequence), c(72L, 108L))
  expect_equal(v$constructs$repeat_count, c(2L, 3L))
  expect_equal(v$donor_after, c(human = 102869593, chimp = 104324234))
  # Full original DNA, with no gaps, is preserved in each aligned string.
  h <- as.character(Biostrings::readDNAStringSet(file.path(demo_dir, "human.fa"))[[1]])
  c <- as.character(Biostrings::readDNAStringSet(file.path(demo_dir, "chimp.fa"))[[1]])
  expect_equal(v$aligned[["human"]], substr(h, 29, 136))
  expect_equal(gsub("-", "", v$aligned[["chimp"]], fixed = TRUE), substr(c, 56, 91))
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-as3mt-example.R"), envir = env)
  result <- env$plot_as3mt_alignseq(demo_dir)
  expect_true(all(result$data$reference_x >= 102869498 & result$data$reference_x <= 102869605))
  expect_equal(result$data$position[result$data$boundary_after], c(102869593, 104324234))
  expect_true(any(result$grob$layout$name == "as3mt-vntr-evidence"))
  expect_equal(result$vntr$constructs$repeat_count, c(2L, 3L))
})
