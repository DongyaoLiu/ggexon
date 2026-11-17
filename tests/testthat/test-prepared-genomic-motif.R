prepared_motif_individual <- function(base_edges = TRUE) {
  x <- prepared_genomic_fixture()
  x <- project_domains_to_genome(x, annotation = "regions", transcripts = c("tx1", "tx2"), store = TRUE,
    name = "projected_regions")
  x <- project_mutations_to_genome(x, annotation = "muts", transcripts = "tx1",
    store = TRUE, name = "projected_mutations")
  prepare_genomic_layout(x, annotation = "projected_mutations", name = "locus",
    intron_factor = 3, track_ymin = -0.34, track_ymax = -0.08,
    base_edges = base_edges)
}

test_that("prepared motifs share exon bounds and the stored genomic map", {
  x <- prepared_motif_individual()
  original <- serialize(x, NULL)
  projection <- query_genomic_projection(x, "projected_regions")
  expect_setequal(projection$transcripts, c("tx1", "tx2"))
  built <- ggexon_build(ggexon(x) + geom_exon2(genomic_layout = "locus") +
    geom_motif(genomic_layout = "locus", annotation = "projected_regions",
      model = "Pfam", fill = "gold", colour = "navy", alpha = 0.4) +
    scale_x_ggexon_genomic(genomic_layout = "locus"))
  motifs <- built@data[[2L]]
  expect_equal(motifs$transcripts, rep("tx1", 2L))
  expect_equal(motifs$ymin, rep(-0.34, 2L))
  expect_equal(motifs$ymax, rep(-0.08, 2L))
  expect_equal(motifs$xmin, c(99.5, 104.5 + 95 / 3))
  expect_equal(motifs$xmax, c(104.5, 108.5 + 95 / 3))
  expect_equal(motifs$xmin, built@data[[1L]]$xmin)
  expect_equal(motifs$fill, rep("gold", 2L))
  expect_equal(motifs$colour, rep("navy", 2L))
  expect_equal(motifs$alpha, rep(0.4, 2L))
  expect_equal(query_genomic_projection(x, "projected_regions"), projection)
  expect_equal(projection$xmin[projection$transcripts == "tx1"], c(100, 200))
  expect_identical(serialize(x, NULL), original)
  expect_s3_class(ggplot2::ggplot_gtable(built), "gtable")
})

test_that("prepared motif edges follow the cached base-edge setting", {
  x <- prepared_motif_individual(base_edges = FALSE)
  built <- ggexon_build(ggexon(x) +
    geom_motif(genomic_layout = "locus", annotation = "projected_regions",
      exon_height = 0.26) + scale_x_ggexon_genomic(genomic_layout = "locus"))
  expect_equal(built@data[[1L]]$xmin, c(100, 104 + 96 / 3))
  expect_equal(built@data[[1L]]$xmax, c(104, 107 + 96 / 3))
  expect_equal(built@data[[1L]]$ymax, rep(-0.08, 2L))
  expect_error(ggexon_build(ggexon(x) +
    geom_motif(genomic_layout = "locus", annotation = "projected_regions",
      exon_height = 0.8) + scale_x_ggexon_genomic(genomic_layout = "locus")),
    "exon_height.*conflict|exon_height.*prepared")
})

test_that("prepared motifs require an explicit stored domain projection", {
  x <- prepared_motif_individual()
  for (annotation in list(NULL, "regions", "projected_mutations")) {
    expect_error(ggexon_build(ggexon(x) +
      geom_motif(genomic_layout = "locus", annotation = annotation) +
      scale_x_ggexon_genomic(genomic_layout = "locus")),
      "stored.*domain|domain.*projection")
  }
})

test_that("prepared motifs reject independent placement controls", {
  expect_error(geom_motif(genomic_layout = "locus", y_offset = 1),
    "prepare_genomic_layout|prepared")
  expect_error(geom_motif(genomic_layout = "locus", x_translation = 1),
    "prepare_genomic_layout|prepared")
  expect_error(geom_motif(genomic_layout = "locus", stat = "summary"),
    "stat.*identity|prepared")
  expect_error(geom_motif(genomic_layout = "locus", position = "nudge"),
    "position.*identity|prepared")
  for (aesthetic in c("xmin", "xmax", "ymin", "ymax")) {
    mapping <- ggplot2::aes(value = 0)
    names(mapping) <- aesthetic
    expect_error(geom_motif(mapping, genomic_layout = "locus"),
      "mapping|prepared")
  }
  x <- prepared_motif_individual()
  expect_error(ggexon_build(ggexon(x, ggplot2::aes(ymin = 0)) +
    geom_motif(genomic_layout = "locus", annotation = "projected_regions") +
    scale_x_ggexon_genomic(genomic_layout = "locus")),
    "mapping|prepared")
})

test_that("prepared motif filters operate on the stored domain segments", {
  x <- prepared_motif_individual()
  layer <- geom_motif(genomic_layout = "locus", annotation = "projected_regions",
    ids = "tx1", domains = "Binding", model = "Pfam", motif = "Binding")
  expect_identical(layer$geom_params$genomic_layout, "locus")
  built <- ggexon_build(ggexon(x) + layer +
    scale_x_ggexon_genomic(genomic_layout = "locus"))
  expect_equal(nrow(built@data[[1L]]), 2L)
  expect_error(ggexon_build(ggexon(x) +
    geom_motif(genomic_layout = "locus", annotation = "projected_regions",
      model = "missing") + scale_x_ggexon_genomic(genomic_layout = "locus")),
    "Unknown InterProScan model")
})
