test_that("geom_transcripts resolves isoforms for unc-44 from the bundled N2 GTF", {
  n2_annotation_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )

  x <- SynIndividual(
    annotation_file = n2_annotation_path,
    genome_file = genome_waiver(),
    id = "N2",
    annotation_format = "gtf"
  )

  resolved <- syn_to_transcript_df(x, genes = "unc-44")

  expect_true(nrow(resolved) > 0L)
  expect_identical(unique(resolved$track), "N2")
  expect_gt(length(unique(resolved$transcripts)), 1L)
  expect_true(all(resolved$type == "exon"))
  expect_true(any(grepl("^B0350\\.2", unique(resolved$transcripts))))

  plot_obj <- ggexon(x) +
    geom_transcripts(
      genes = "unc-44",
      exon_height = 0.6
    )

  built <- ggplot2::ggplot_build(plot_obj)
  layer_data <- built$data[[1L]]

  expect_true(nrow(layer_data) > 0L)
  expect_identical(unique(layer_data$track), "N2")
  expect_true(all(diff(sort(unique(layer_data$ymin), decreasing = TRUE)) < 0))
})
