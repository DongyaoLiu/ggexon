coverage_mixed_edge_species <- function() {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  individual <- SynIndividual(
    annotation_file = file.path(
      fixture_dir,
      "WS285.ugt31-zeel1-peel1-nekl1.gtf"
    ),
    annotation_format = "gtf",
    id = "XZ1516"
  )
  individual <- add_annotation(
    individual,
    SynBigWigAnnotation(
      "coverage",
      file.path(fixture_dir, "XZ1516.raw.bw")
    )
  )
  add_individual(SynSpecies(name = "mixed coverage"), individual)
}

coverage_mixed_edge_layer <- function(data) {
  geom_coverage(
    data = data,
    mapping = ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      coverage = coverage,
      track = track
    ),
    inherit.aes = FALSE
  )
}

test_that("plain-data aliases retain recipients when resolving coverage x sources", {
  coverage <- data.frame(
    track = "depth_A",
    individual = "sample_A",
    species = "sample_A",
    xmin = 100,
    xmax = 119,
    coverage = 5,
    stringsAsFactors = FALSE
  )
  annotation <- data.frame(
    track = c("genes_A", "genes_B"),
    individual = c("sample_A", "sample_B"),
    species = c("sample_A", "sample_B"),
    xmin = c(90, 190),
    xmax = c(150, 250),
    y = 1,
    strand = "+",
    gene = c("gene_a", "gene_b"),
    label = c("gene_a", "gene_b"),
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(SynSpecies(name = "plain aliases")) +
      coverage_mixed_edge_layer(coverage) +
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  layout <- as.data.frame(built@layout$layout)
  coverage_row <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation_rows <- layout[layout$panel_type == "annotation", , drop = FALSE]
  genes_a_panel <- as.integer(
    annotation_rows$PANEL[annotation_rows$track == "genes_A"]
  )
  genes_b_panel <- as.integer(
    annotation_rows$PANEL[annotation_rows$track == "genes_B"]
  )

  expect_identical(as.character(layout$track), c(
    "depth_A", "genes_A", "genes_B"
  ))
  expect_identical(as.character(layout$individual), c(
    "sample_A", "sample_A", "sample_B"
  ))
  expect_identical(as.character(layout$species), c(
    "sample_A", "sample_A", "sample_B"
  ))
  expect_identical(as.integer(coverage_row$x_source_panel), genes_a_panel)
  expect_false(as.integer(coverage_row$x_source_panel) == genes_b_panel)
  expect_identical(
    unique(as.integer(built@data[[1L]]$PANEL)),
    as.integer(coverage_row$PANEL)
  )
  expect_setequal(
    unique(as.integer(built@data[[2L]]$PANEL)),
    as.integer(annotation_rows$PANEL)
  )
})

test_that("recipient-named links reuse aliased annotation panels", {
  coverage <- data.frame(
    track = "depth_A",
    individual = "sample_A",
    species = "sample_A",
    xmin = 100,
    xmax = 119,
    coverage = 5,
    stringsAsFactors = FALSE
  )
  annotation <- data.frame(
    track = c("genes_A", "genes_B"),
    individual = c("sample_A", "sample_B"),
    species = c("sample_A", "sample_B"),
    xmin = c(90, 190),
    xmax = c(150, 250),
    y = 1,
    strand = "+",
    gene = c("gene_a", "gene_b"),
    label = c("gene_a", "gene_b"),
    stringsAsFactors = FALSE
  )
  link <- data.frame(
    track = "link_A_B",
    tspecies = "sample_A",
    tchr = "chr1",
    tstart = 100,
    tend = 110,
    strand = "+",
    qspecies = "sample_B",
    qchr = "chr1",
    qstart = 200,
    qend = 210,
    group = 1L,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(SynSpecies(name = "linked plain aliases")) +
      coverage_mixed_edge_layer(coverage) +
      geom_genetag(data = annotation, show_label = FALSE) +
      geom_nuclink(
        data = link,
        mapping = ggplot2::aes(
          tspecies = tspecies,
          tchr = tchr,
          tstart = tstart,
          tend = tend,
          strand = strand,
          qspecies = qspecies,
          qchr = qchr,
          qstart = qstart,
          qend = qend,
          group = group
        ),
        inherit.aes = FALSE
      ) +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  layout <- as.data.frame(built@layout$layout)
  coverage_row <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation_rows <- layout[layout$panel_type == "annotation", , drop = FALSE]
  link_row <- layout[layout$panel_type == "link", , drop = FALSE]
  genes_a_panel <- as.integer(
    annotation_rows$PANEL[annotation_rows$track == "genes_A"]
  )
  genes_b_panel <- as.integer(
    annotation_rows$PANEL[annotation_rows$track == "genes_B"]
  )

  expect_identical(
    as.character(layout$track),
    c("depth_A", "genes_A", "link_A_B", "genes_B")
  )
  expect_identical(
    as.character(annotation_rows$species),
    c("sample_A", "sample_B")
  )
  expect_identical(as.integer(coverage_row$x_source_panel), genes_a_panel)
  expect_identical(as.integer(link_row$t_panel), genes_a_panel)
  expect_identical(as.integer(link_row$q_panel), genes_b_panel)
  expect_setequal(
    unique(as.integer(built@data[[3L]]$source_panel)),
    c(genes_a_panel, genes_b_panel)
  )
})

coverage_mixed_edge_build <- function(object_first) {
  external <- data.frame(
    track = "external",
    individual = "external",
    species = "external",
    xmin = 1,
    xmax = 10,
    coverage = 7,
    stringsAsFactors = FALSE
  )
  explicit_layer <- coverage_mixed_edge_layer(external)
  object_layer <- geom_coverage(
    annotation = "coverage",
    species = "XZ1516"
  )
  layers <- if (isTRUE(object_first)) {
    list(object_layer, explicit_layer)
  } else {
    list(explicit_layer, object_layer)
  }

  ggexon_build(
    ggexon(coverage_mixed_edge_species()) +
      layers[[1L]] +
      layers[[2L]] +
      geom_exon(
        species = "XZ1516",
        chr = "I",
        subset = c(1L, 50L),
        annotation_type = "exon"
      ) +
      facet_genomics(ggplot2::vars(track))
  )
}

test_that("mixed coverage panels follow layer order with an empty Syn request", {
  external_first <- coverage_mixed_edge_build(object_first = FALSE)
  object_first <- coverage_mixed_edge_build(object_first = TRUE)
  external_layout <- as.data.frame(external_first@layout$layout)
  object_layout <- as.data.frame(object_first@layout$layout)

  expect_identical(
    as.character(external_layout$track),
    c("external", "XZ1516")
  )
  expect_identical(
    as.character(object_layout$track),
    c("XZ1516", "external")
  )
  expect_true(all(external_layout$panel_type == "coverage"))
  expect_true(all(object_layout$panel_type == "coverage"))
  expect_identical(nrow(external_first@data[[1L]]), 1L)
  expect_identical(nrow(external_first@data[[2L]]), 0L)
  expect_identical(nrow(object_first@data[[1L]]), 0L)
  expect_identical(nrow(object_first@data[[2L]]), 1L)
  expect_identical(
    unique(as.integer(external_first@data[[1L]]$PANEL)),
    as.integer(external_layout$PANEL[external_layout$track == "external"])
  )
  expect_identical(
    unique(as.integer(object_first@data[[2L]]$PANEL)),
    as.integer(object_layout$PANEL[object_layout$track == "external"])
  )
})
