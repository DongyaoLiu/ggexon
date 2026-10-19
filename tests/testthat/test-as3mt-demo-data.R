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
