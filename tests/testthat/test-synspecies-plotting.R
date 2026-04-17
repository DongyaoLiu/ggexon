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
    test_syn_individual(
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
    test_syn_individual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
  layout_df <- syn_layout_panels(layout)

  expect_s4_class(layout, "SynLayout")
  expect_identical(nrow(layout_df), 3L)
  expect_identical(as.character(layout_df$track), c("XZ1516", "link_XZ1516_vs_N2", "N2"))
  expect_identical(
    as.character(layout_df$panel_type),
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
      test_syn_individual(
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
  layout_df <- syn_layout_panels(layout)

  expect_s4_class(layout, "SynLayout")
  expect_identical(nrow(layout_df), 5L)
  expect_identical(
    as.character(layout_df$track),
    c(
      "XZ1516",
      "link_XZ1516_vs_N2",
      "N2",
      "link_N2_vs_CB4856",
      "CB4856"
    )
  )
  expect_identical(sum(layout_df$panel_type == "link"), 2L)
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
    test_syn_individual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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

  expect_s4_class(species_layout(sp), "SynLayout")
  layout_df <- syn_layout_panels(species_layout(sp))
  expect_identical(
    as.character(layout_df$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(layout_df$SCALE_Y), c(1L, 2L, 1L))
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
    test_syn_individual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
  custom_layout_df <- syn_layout_panels(custom_layout)
  custom_layout_df$track <- factor(
    custom_layout_df$track,
    levels = c("N2", "link_XZ1516_vs_N2", "XZ1516")
  )
  species_layout(sp) <- custom_layout_df

  plot_obj <- ggexon(sp) +
    geom_gene(
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_gene(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(
      alignment = "XZ1516_vs_N2"
    ) +
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(
      alignment = "XZ1516_vs_N2"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
  expect_length(built@data, 3L)
  expect_true(all(unique(as.integer(built@data[[1]]$PANEL)) == 1L))
  expect_true(all(unique(as.integer(built@data[[2]]$PANEL)) == 3L))
  expect_true(all(c("x_variable", "y_variable", "x", "y", "group") %in% names(built@data[[3]])))
  expect_true(all(unique(as.integer(built@data[[3]]$PANEL)) == 2L))
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_gene(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(
      alignment = "XZ1516_vs_N2"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
  expect_length(built@data, 3L)
  expect_true(all(unique(as.character(built@data[[1]]$track)) == "XZ1516"))
  expect_true(all(unique(as.character(built@data[[2]]$track)) == "N2"))
  expect_true(all(c("x_variable", "y_variable", "x", "y", "group") %in% names(built@data[[3]])))
  expect_true(all(unique(as.integer(built@data[[3]]$PANEL)) == 2L))
})

test_that("geom_genelabel inherits species windows from explicit exon layers", {
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
    test_syn_individual(
      genome_file = genome_path,
      annotation_file = annotation_path,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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

  plot_obj <- ggexon(sp) +
    geom_exon(
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21558028, 21620381)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20454111, 20491853)
    ) +
    geom_genelabel() +
    geom_nuclink() +
    facet_genomics(ggplot2::vars(track), scales = "free")

  build <- ggexon_build(plot_obj)
  label_layer <- build@data[[3L]]

  expect_true(nrow(label_layer) > 0L)
  expect_setequal(unique(as.character(label_layer$track)), c("XZ1516", "N2"))
  expect_true("label" %in% names(label_layer))
})

test_that("syn-aware geoms preserve mapped and fixed aesthetics via standard aes semantics", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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

  exon_build <- ggexon_build(
    ggexon(sp) +
      geom_exon(
        ggplot2::aes(fill = strand, colour = strand, alpha = strand),
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21574445, 21584356)
      )
  )
  exon_data <- exon_build@data[[1L]]
  expect_true(all(c("fill", "colour", "alpha") %in% names(exon_data)))
  expect_true(length(unique(exon_data$fill)) >= 1L)
  expect_true(length(unique(exon_data$colour)) >= 1L)
  expect_true(all(!is.na(exon_data$alpha)))

  exon_gene_build <- ggexon_build(
    ggexon(sp) +
      geom_exon(
        ggplot2::aes(fill = gene_id),
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21574445, 21584356)
      )
  )
  exon_gene_data <- exon_gene_build@data[[1L]]
  expect_true(nrow(exon_gene_data) > 0L)
  expect_true(length(unique(exon_gene_data$fill)) >= 1L)

  exon_transcript_build <- ggexon_build(
    ggexon(sp) +
      geom_exon(
        ggplot2::aes(fill = transcript_id),
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21574445, 21584356)
      )
  )
  exon_transcript_data <- exon_transcript_build@data[[1L]]
  expect_true(nrow(exon_transcript_data) > 0L)
  expect_true(length(unique(exon_transcript_data$fill)) >= 1L)

  gene_build <- ggexon_build(
    ggexon(sp) +
      geom_gene(
        ggplot2::aes(fill = strand, colour = strand, alpha = strand),
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21574445, 21584356)
      )
  )
  gene_data <- gene_build@data[[1L]]
  expect_true(all(c("fill", "colour", "alpha") %in% names(gene_data)))
  expect_true(all(!is.na(gene_data$alpha)))

  label_build <- ggexon_build(
    ggexon(sp) +
      geom_exon(
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21558028, 21620381)
      ) +
      geom_exon(
        species = "N2",
        chr = "V",
        subset = c(20454111, 20491853)
      ) +
      geom_genelabel(
        ggplot2::aes(colour = strand, alpha = strand)
      ) +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )
  label_data <- label_build@data[[3L]]
  expect_true(all(c("colour", "alpha") %in% names(label_data)))
  expect_true(all(!is.na(label_data$alpha)))

  fixed_exon_build <- ggexon_build(
    ggexon(sp) +
      geom_exon(
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21574445, 21584356),
        fill = "steelblue",
        colour = "goldenrod",
        alpha = 0.4
      )
  )
  fixed_exon_data <- fixed_exon_build@data[[1L]]
  expect_true(all(fixed_exon_data$fill == "steelblue"))
  expect_true(all(fixed_exon_data$colour == "goldenrod"))
  expect_true(all(fixed_exon_data$alpha == 0.4))

})

test_that("geom_gene comparative grammar supports reference-led subsetting with free_x", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_gene(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(
      alignment = "XZ1516_vs_N2"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_X), c(1L, 2L, 3L))
  expect_true(all(unique(as.integer(built@data[[1]]$PANEL)) == 1L))
  expect_true(all(unique(as.integer(built@data[[2]]$PANEL)) == 3L))
  expect_true(all(unique(as.integer(built@data[[3]]$PANEL)) == 2L))
  expect_true(all(c("t_panel", "q_panel") %in% names(built@data[[3]])))
  expect_false(identical(
    range(built@data[[1]]$xmin[as.integer(built@data[[1]]$PANEL) == 1L]),
    range(built@data[[2]]$xmin[as.integer(built@data[[2]]$PANEL) == 3L])
  ))
})

test_that("geom_nuclink defaults to annotation windows when both species subsets are explicit", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_gene(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(alignment = "XZ1516_vs_N2") +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)
  link_data <- built@data[[3L]]

  expect_true(nrow(link_data) > 0L)
  expect_true(all(link_data$tstart < 21584356 & link_data$tend > 21574445))
  expect_true(all(link_data$qstart < 20465040 & link_data$qend > 20456948))
})

test_that("geom_nuclink comparative plots assemble gtables without panel-list errors", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21558028, 21620381)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20454111, 20491853)
    ) +
    geom_nuclink() +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  expect_no_error(ggplot2::ggplot_gtable(ggexon_build(plot_obj)))
})

test_that("geom_nuclink uses target and query source panels in the correct direction", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21558028, 21620381)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20454111, 20491853)
    ) +
    geom_nuclink() +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(plot_obj)
  link_layout <- built@layout$layout[built@layout$layout$panel_type == "link", , drop = FALSE]
  link_data <- built@data[[3L]]

  expect_identical(as.character(link_layout$tspecies), "N2")
  expect_identical(as.character(link_layout$qspecies), "XZ1516")
  expect_identical(as.integer(link_layout$t_panel), 3L)
  expect_identical(as.integer(link_layout$q_panel), 1L)
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("tstart", "tend")]) == 3L
  ))
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("qstart", "qend")]) == 1L
  ))
})

test_that("annotation subset can be derived from a linked species window", {
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
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
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
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_gene(species = "N2") +
    geom_nuclink(alignment = "XZ1516_vs_N2") +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(plot_obj)

  expect_true(nrow(built@data[[2L]]) > 0L)
  expect_identical(as.character(unique(built@data[[2L]]$track)), "N2")
})

test_that("annotation subset is required without a derivation path", {
  xz_genome <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )

  plot_obj <- ggexon(sp) + geom_exon()

  expect_error(
    ggexon_build(plot_obj),
    "`subset` is required for Syn annotation layers"
  )
})

test_that("facet_genomics behaves like standard faceting for multi-species annotation layers without links", {
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

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = xz_genome,
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )

  plot_obj <- ggexon(sp) +
    geom_exon(
      species = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  built <- ggexon_build(plot_obj)

  expect_identical(nrow(built@layout$layout), 2L)
  expect_false("panel_type" %in% names(built@layout$layout))
  expect_false(any(grepl("^link_", as.character(built@layout$layout$track))))
  expect_setequal(as.character(built@layout$layout$track), c("XZ1516", "N2"))
  expect_identical(as.integer(sort(unique(built@layout$layout$SCALE_X))), c(1L, 2L))
})

test_that("generic facet_genomics assigns source panels for link rescaling", {
  track_levels <- c("sp1", "link_sp1_sp2", "sp2")

  annotation_df <- data.frame(
    track = factor(c("sp1", "sp2"), levels = track_levels),
    x = c(10, 1000),
    y = c(1, 1)
  )

  link_df <- data.frame(
    track = factor("link_sp1_sp2", levels = track_levels),
    tspecies = "sp1",
    tchr = "chr1",
    tstart = 12,
    tend = 20,
    strand = "+",
    qspecies = "sp2",
    qchr = "chr2",
    qstart = 1010,
    qend = 1030,
    group = 1
  )

  plot_obj <- ggexon() +
    ggplot2::geom_blank(
      data = annotation_df,
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    geom_nuclink(
      data = link_df,
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

  built <- ggexon_build(plot_obj)

  link_layout_row <- built@layout$layout[
    as.character(built@layout$layout$track) == "link_sp1_sp2",
    ,
    drop = FALSE
  ]
  link_data <- built@data[[2]]

  expect_identical(nrow(link_layout_row), 1L)
  expect_false(is.na(link_layout_row$t_panel))
  expect_false(is.na(link_layout_row$q_panel))
  expect_setequal(
    unique(as.integer(link_data$source_panel)),
    c(link_layout_row$t_panel, link_layout_row$q_panel)
  )
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("tstart", "tend")]) ==
      link_layout_row$t_panel
  ))
  expect_true(all(
    as.integer(link_data$source_panel[link_data$x_variable %in% c("qstart", "qend")]) ==
      link_layout_row$q_panel
  ))
})
