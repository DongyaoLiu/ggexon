test_that("syn-aware geoms default to the only individual in a SynSpecies", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )

  exon_plot <- ggexon(sp) +
    geom_exon(
      chr = "RagTag_V",
      subset = c(21550000, 21680000)
    )
  exon_build <- ggplot2::ggplot_build(exon_plot)

  expect_true(nrow(exon_build$data[[1L]]) > 0L)
  expect_identical(unique(exon_build$data[[1L]]$track), "XZ1516")

  gene_plot <- ggexon(sp) +
    geom_gene(
      chr = "RagTag_V",
      subset = c(21550000, 21680000)
    )
  gene_build <- ggplot2::ggplot_build(gene_plot)

  expect_true(nrow(gene_build$data[[1L]]) > 0L)
  expect_identical(unique(gene_build$data[[1L]]$track), "XZ1516")
})

test_that("syn-aware geoms require species when a SynSpecies has multiple individuals", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome_path,
      annotation_file = n2_annotation_path,
      id = "N2",
      annotation_format = "gtf"
    )
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(sp) +
        geom_exon(
          chr = "RagTag_V",
          subset = c(21550000, 21680000)
        )
    ),
    "Use `species` to select one individual"
  )

  expect_error(
    ggplot2::ggplot_build(
      ggexon(sp) +
        geom_gene(
          chr = "RagTag_V",
          subset = c(21550000, 21680000)
        )
    ),
    "Use `species` to select one individual"
  )
})

test_that("SynSpecies chain layout reserves one link panel per pairwise alignment", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome_path,
      annotation_file = n2_annotation_path,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )

  layout <- synspecies_chain_layout(
    sp,
    vars = ggplot2::vars(track),
    free = list(x = FALSE, y = FALSE)
  )

  expect_identical(nrow(layout), 3L)
  expect_identical(as.character(layout$track), c("XZ1516", "link_XZ1516_vs_N2", "N2"))
  expect_identical(
    as.character(layout$panel_type),
    c("annotation", "link", "annotation")
  )
})

test_that("SynSpecies chain layout scales link panels with pairwise alignment count", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  for (id in c("XZ1516", "N2", "CB4856")) {
    sp <- add_individual(
      sp,
      SynIndividual(
        genome_file = genome_path,
        annotation_file = annotation_path,
        id = id
      )
    )
  }

  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "N2_vs_CB4856",
      query_individual = "N2",
      target_individual = "CB4856",
      file = paf_path
    )
  )

  layout <- synspecies_chain_layout(
    sp,
    vars = ggplot2::vars(track),
    free = list(x = FALSE, y = FALSE)
  )

  expect_identical(nrow(layout), 5L)
  expect_identical(
    as.character(layout$track),
    c(
      "XZ1516",
      "link_XZ1516_vs_N2",
      "N2",
      "link_N2_vs_CB4856",
      "CB4856"
    )
  )
  expect_identical(sum(layout$panel_type == "link"), 2L)
})

test_that("SynSpecies can store a ggexon layout table", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome_path,
      annotation_file = n2_annotation_path,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )

  sp <- store_chain_layout(sp, free = list(x = FALSE, y = TRUE))

  expect_true(is.data.frame(species_layout(sp)))
  expect_identical(
    as.character(species_layout(sp)$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(species_layout(sp)$SCALE_Y), c(1L, 2L, 1L))
})

test_that("plot_build uses stored SynSpecies layout during facet setup", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation_path <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome_path,
      annotation_file = n2_annotation_path,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )

  custom_layout <- synspecies_chain_layout(
    sp,
    vars = ggplot2::vars(track),
    free = list(x = FALSE, y = TRUE)
  )
  custom_layout$track <- factor(
    custom_layout$track,
    levels = c("N2", "link_XZ1516_vs_N2", "XZ1516")
  )
  species_layout(sp) <- custom_layout

  plot_obj <- ggexon(sp) +
    geom_gene(species = "XZ1516", chr = "RagTag_V", subset = c(21574445, 21584356)) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)
  expect_identical(as.character(built@layout$layout$track), c("N2", "link_XZ1516_vs_N2", "XZ1516"))
  expect_identical(as.integer(built@layout$layout$ROW), c(1L, 2L, 3L))
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
})

test_that("geom_exon comparative grammar builds paired annotation and link panels", {
  xz_genome <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )

  plot_obj <- ggexon(sp) +
    geom_exon(
      species = c("N2", "XZ1516"),
      reference = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356),
      alignment = "XZ1516_vs_N2"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
  expect_length(built@data, 2L)
  expect_true(all(c(1L, 3L) %in% unique(as.integer(built@data[[1]]$PANEL))))
  expect_true(all(c("x_variable", "y_variable", "x", "y", "group") %in% names(built@data[[2]])))
  expect_true(all(unique(as.integer(built@data[[2]]$PANEL)) == 2L))
})

test_that("geom_gene comparative grammar builds paired annotation and link panels", {
  xz_genome <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_genome <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )
  paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    SynIndividual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_N2",
      query_individual = "XZ1516",
      target_individual = "N2",
      file = paf_path
    )
  )

  plot_obj <- ggexon(sp) +
    geom_gene(
      species = c("N2", "XZ1516"),
      reference = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356),
      alignment = "XZ1516_vs_N2"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
  expect_length(built@data, 2L)
  expect_true(all(c("XZ1516", "N2") %in% unique(as.character(built@data[[1]]$track))))
  expect_true(all(c("x_variable", "y_variable", "x", "y", "group") %in% names(built@data[[2]])))
  expect_true(all(unique(as.integer(built@data[[2]]$PANEL)) == 2L))
})
