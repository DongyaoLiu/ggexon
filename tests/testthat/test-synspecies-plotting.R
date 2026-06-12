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

test_that("syn-aware annotation geoms can default to all individuals and full annotations", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  for (id in c("XZ1516", "CB4856")) {
    sp <- add_individual(
      sp,
      test_syn_individual(
        annotation_file = annotation_path,
        id = id
      )
    )
  }

  exon_plot <- ggexon(sp) + geom_exon()
  exon_build <- ggplot2::ggplot_build(exon_plot)

  expect_true(nrow(exon_build$data[[1L]]) > 0L)
  expect_setequal(unique(exon_build$data[[1L]]$track), c("XZ1516", "CB4856"))
})

test_that("geom_nuclink(reference = ...) reorders ODGI comparison panels greedily", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  odgi_tbl <- data.frame(
    node_id = 1:5,
    sequence = rep("A", 5),
    REF_chromosome = rep("chrR", 5),
    REF_strand = c("+", "+", "+", "NA", "NA"),
    REF_absolute_start = c(100L, 101L, 102L, "NA", "NA"),
    REF_absolute_end = c(100L, 101L, 102L, "NA", "NA"),
    ALPHA_chromosome = rep("chrA", 5),
    ALPHA_strand = c("+", "+", "+", "NA", "+"),
    ALPHA_absolute_start = c(200L, 201L, 202L, "NA", 204L),
    ALPHA_absolute_end = c(200L, 201L, 202L, "NA", 204L),
    BETA_chromosome = rep("chrB", 5),
    BETA_strand = c("NA", "+", "+", "NA", "+"),
    BETA_absolute_start = c("NA", 301L, 302L, "NA", 304L),
    BETA_absolute_end = c("NA", 301L, 302L, "NA", 304L),
    GAMMA_chromosome = rep("chrG", 5),
    GAMMA_strand = c("NA", "NA", "NA", "NA", "+"),
    GAMMA_absolute_start = c("NA", "NA", "NA", "NA", 404L),
    GAMMA_absolute_end = c("NA", "NA", "NA", "NA", 404L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sp <- SynSpecies(name = "ODGIChain")
  for (id in c("gamma", "beta", "alpha", "reference")) {
    sp <- add_individual(
      sp,
      test_syn_individual(
        annotation_file = annotation_path,
        id = id
      )
    )
  }
  sp <- add_multiple_alignment(
    sp,
    odgi_multi_alignment(
      odgi_tbl,
      name = "odgi_chain",
      individuals = c(
        REF = "reference",
        ALPHA = "alpha",
        BETA = "beta",
        GAMMA = "gamma"
      )
    )
  )

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_nuclink(reference = "reference", alignment = "odgi_chain") +
      facet_genomics(ggplot2::vars(track), scales = "free_y")
  )

  expect_identical(
    as.character(built@layout$layout$track),
    c(
      "reference",
      "link_odgi_chain__reference__alpha",
      "alpha",
      "link_odgi_chain__alpha__beta",
      "beta",
      "link_odgi_chain__beta__gamma",
      "gamma"
    )
  )
})

test_that("geom_nuclink(filter_by_len = ...) filters ODGI-derived link nodes by sequence length", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  odgi_tbl <- data.frame(
    node_id = c(1L, 2L),
    sequence = c("AC", "G"),
    XZ1516_chromosome = c("V_RagTag", "V_RagTag"),
    XZ1516_strand = c("+", "-"),
    XZ1516_absolute_start = c(100L, 102L),
    XZ1516_absolute_end = c(101L, 102L),
    N2_chromosome = c("V", "V"),
    N2_strand = c("+", "+"),
    N2_absolute_start = c(200L, 202L),
    N2_absolute_end = c(201L, 202L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sp <- SynSpecies(name = "ODGIFilter")
  for (id in c("XZ1516", "N2")) {
    sp <- add_individual(
      sp,
      test_syn_individual(
        annotation_file = annotation_path,
        id = id
      )
    )
  }
  sp <- add_multiple_alignment(
    sp,
    odgi_multi_alignment(
      odgi_tbl,
      name = "odgi_pair",
      individuals = c(XZ1516 = "XZ1516", N2 = "N2")
    )
  )

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_nuclink(alignment = "odgi_pair", filter_by_len = "> 1") +
      facet_genomics(ggplot2::vars(track), scales = "free_y")
  )

  link_data <- built@data[[2L]]
  expect_identical(length(unique(link_data$group)), 1L)
  expect_identical(nrow(link_data), 4L)
})

test_that("implicit geom_exon species narrow to an explicit pairwise alignment", {
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
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "CB4856",
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

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_nuclink(alignment = "XZ1516_vs_N2") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
  expect_true(all(c("target_anchor_y", "query_anchor_y") %in% names(built@data[[2L]])))
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

test_that("stored SynLayout panel x windows seed annotation and link windows", {
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

  custom_layout <- synspecies_chain_layout(
    sp,
    vars = ggplot2::vars(track),
    free = list(x = TRUE, y = TRUE)
  )
  custom_layout_df <- syn_layout_panels(custom_layout)
  custom_layout_df$xlim_chr <- c("RagTag_V", NA, "V")
  custom_layout_df$xlim_min <- c(21574445, NA, 20456948)
  custom_layout_df$xlim_max <- c(21584356, NA, 20465040)
  species_layout(sp) <- custom_layout_df

  plot_obj <- ggexon(sp) +
    geom_gene(species = "XZ1516") +
    geom_gene(species = "N2") +
    geom_nuclink(alignment = "XZ1516_vs_N2") +
    facet_genomics(ggplot2::vars(track), scales = "free")

  built <- ggexon_build(plot_obj)

  xz_layer <- built@data[[1L]]
  n2_layer <- built@data[[2L]]
  link_layer <- built@data[[3L]]

  expect_true(all(xz_layer$xmin >= 21574445 & xz_layer$xmax <= 21584356))
  expect_true(all(n2_layer$xmin >= 20456948 & n2_layer$xmax <= 20465040))
  expect_true(all(link_layer$tstart < 21584356 & link_layer$tend > 21574445))
  expect_true(all(link_layer$qstart < 20465040 & link_layer$qend > 20456948))
})

test_that("facet_genomics panel x windows seed annotation and link windows", {
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
    geom_gene(species = "XZ1516") +
    geom_gene(species = "N2") +
    geom_nuclink(alignment = "XZ1516_vs_N2") +
    facet_genomics(
      ggplot2::vars(track),
      scales = "free",
      xlim = list(
        XZ1516 = c(21574445, 21584356),
        N2 = c(20456948, 20465040)
      ),
      xlim_chr = c(XZ1516 = "RagTag_V", N2 = "V")
    )

  built <- ggexon_build(plot_obj)

  xz_layer <- built@data[[1L]]
  n2_layer <- built@data[[2L]]
  link_layer <- built@data[[3L]]
  windows <- effective_panel_windows(plot_obj)

  expect_true(all(xz_layer$xmin >= 21574445 & xz_layer$xmax <= 21584356))
  expect_true(all(n2_layer$xmin >= 20456948 & n2_layer$xmax <= 20465040))
  expect_true(all(link_layer$tstart < 21584356 & link_layer$tend > 21574445))
  expect_true(all(link_layer$qstart < 20465040 & link_layer$qend > 20456948))

  xz_window <- windows[windows$individual == "XZ1516", , drop = FALSE]
  n2_window <- windows[windows$individual == "N2", , drop = FALSE]
  expect_identical(xz_window$start[[1L]], 21574445)
  expect_identical(xz_window$end[[1L]], 21584356)
  expect_identical(n2_window$start[[1L]], 20456948)
  expect_identical(n2_window$end[[1L]], 20465040)
})

test_that("facet_genomics panel x window works without link panels", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = "XZ1516"
      ) |>
        load_annotation()
    )

  plot_obj <- ggexon(sp) +
    geom_gene(species = "XZ1516") +
    facet_genomics(
      ggplot2::vars(track),
      scales = "free",
      xlim = c(21574445, 21584356),
      xlim_chr = "RagTag_V"
    )

  built <- ggexon_build(plot_obj)
  gene_layer <- built@data[[1L]]
  windows <- effective_panel_windows(plot_obj)

  expect_true(all(gene_layer$xmin >= 21574445 & gene_layer$xmax <= 21584356))
  expect_identical(windows$start[[1L]], 21574445)
  expect_identical(windows$end[[1L]], 21584356)
  expect_identical(as.integer(built@layout$layout$SCALE_X[[1L]]), 1L)
})

test_that("set_panel_xlim can update ggexon plots from subset windows", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = "XZ1516"
      ) |>
        load_annotation() |>
        subset_feature_annotation(chr = "RagTag_V", start = 21574445, end = 21584356)
    )
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = 1L,
      ROW = 1L,
      COL = 1L,
      track = "XZ1516",
      stringsAsFactors = FALSE
    )
  )

  plot_obj <- ggexon(sp) +
    geom_exon() +
    facet_genomics(ggplot2::vars(track), scales = "free")

  plot_obj <- set_panel_xlim(plot_obj)
  built <- ggplot2::ggplot_build(plot_obj)

  expect_true(all(built$data[[1L]]$xmin >= 21574445))
  expect_true(all(built$data[[1L]]$xmax <= 21584356))
})

test_that("set_panel_xlim supports ggplot add grammar", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(
        annotation_file = annotation_path,
        genome_file = genome_waiver(),
        id = "XZ1516"
      ) |>
        load_annotation() |>
        subset_feature_annotation(chr = "RagTag_V", start = 21574445, end = 21584356)
    )
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = 1L,
      ROW = 1L,
      COL = 1L,
      track = "XZ1516",
      stringsAsFactors = FALSE
    )
  )

  plot_obj <- ggexon(sp) +
    geom_exon() +
    facet_genomics(ggplot2::vars(track), scales = "free") +
    set_panel_xlim()

  built <- ggplot2::ggplot_build(plot_obj)

  expect_true(all(built$data[[1L]]$xmin >= 21574445))
  expect_true(all(built$data[[1L]]$xmax <= 21584356))
})

test_that("set_panel_xlim ggplot add grammar supports explicit overrides", {
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(
        annotation_file = xz_annotation,
        genome_file = genome_waiver(),
        id = "XZ1516"
      ) |>
        load_annotation() |>
        subset_feature_annotation(chr = "RagTag_V", start = 21574445, end = 21584356),
      SynIndividual(
        annotation_file = n2_annotation,
        genome_file = genome_waiver(),
        id = "N2"
      ) |>
        load_annotation() |>
        subset_feature_annotation(chr = "V", start = 20448654, end = 20480485)
    )
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("N2", "XZ1516"),
      stringsAsFactors = FALSE
    ),
    free = list(x = TRUE, y = FALSE)
  )

  plot_obj <- ggexon(sp) +
    geom_exon(species = c("N2", "XZ1516")) +
    facet_genomics(ggplot2::vars(track), scales = "free") +
    set_panel_xlim(individual = "N2", xlim = c(20450000, 20470000), xlim_chr = "V")

  windows <- effective_panel_windows(plot_obj)
  n2_window <- windows[windows$individual == "N2", , drop = FALSE]
  xz_window <- windows[windows$individual == "XZ1516", , drop = FALSE]

  expect_identical(n2_window$chr[[1L]], "V")
  expect_identical(n2_window$start[[1L]], 20450000)
  expect_identical(n2_window$end[[1L]], 20470000)
  expect_identical(xz_window$chr[[1L]], NA_character_)
})

test_that("set_panel_xlim does not break plotting for unloaded annotation panels", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  sp <- SynSpecies(name = "worms") |>
    add_individual(
      SynIndividual(annotation_file = annotation_path, id = "ECA1238")
    )
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = 1L,
      ROW = 1L,
      COL = 1L,
      track = "ECA1238",
      stringsAsFactors = FALSE
    )
  )

  plot_obj <- ggexon(sp) +
    geom_exon(species = "ECA1238") +
    facet_genomics(ggplot2::vars(track), scales = "free") +
    set_panel_xlim(individual = "ECA1238", xlim = c(0, 15000))

  built <- ggplot2::ggplot_build(plot_obj)

  expect_true(nrow(built$data[[1L]]) > 0L)
})

test_that("stored SynLayout is pruned to the species requested by the plot", {
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
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "CB4856",
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

  sp <- store_chain_layout(sp, free = list(x = TRUE, y = TRUE))

  plot_obj <- ggexon(sp) +
    geom_exon(species = c("XZ1516", "N2")) +
    geom_nuclink(alignment = "XZ1516_vs_N2") +
    facet_genomics(ggplot2::vars(track), scales = "free")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_XZ1516_vs_N2", "N2")
  )
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

test_that("geom_nuclink can dispatch an ODGI multiple alignment to the middle panel", {
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  n2_annotation <- system.file(
    "extdata",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  )

  odgi_tbl <- data.frame(
    node_id = c(1L, 2L),
    sequence = c("AC", "G"),
    XZ1516_chromosome = c("V_RagTag", "V_RagTag"),
    XZ1516_strand = c("+", "-"),
    XZ1516_absolute_start = c(21574445L, 21574447L),
    XZ1516_absolute_end = c(21574446L, 21574447L),
    N2_chromosome = c("V", "V"),
    N2_strand = c("+", "+"),
    N2_absolute_start = c(20456948L, 20456950L),
    N2_absolute_end = c(20456949L, 20456950L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = xz_annotation,
      id = "XZ1516"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_multiple_alignment(
    sp,
    odgi_multi_alignment(odgi_tbl, name = "worm-graph")
  )

  plot_obj <- ggexon(sp) +
    geom_exon(
      species = "XZ1516",
      chr = "V_RagTag",
      subset = c(21574445, 21584356)
    ) +
    geom_exon(
      species = "N2",
      chr = "V",
      subset = c(20456948, 20465040)
    ) +
    geom_nuclink(
      alignment = "worm-graph"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c("XZ1516", "link_worm-graph__XZ1516__N2", "N2")
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L))
  expect_true(all(unique(as.integer(built@data[[3]]$PANEL)) == 2L))
  expect_true(all(c("x_variable", "y_variable", "x", "y", "group") %in% names(built@data[[3]])))
})

test_that("geom_nuclink can derive multiple middle panels from an ODGI alignment and reference window", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  odgi_tbl <- data.frame(
    node_id = c(1L, 2L, 3L),
    sequence = c("AC", "G", "TT"),
    XZ1516_chromosome = c("RagTag_V", "RagTag_V", "RagTag_V"),
    XZ1516_strand = c("+", "-", "+"),
    XZ1516_absolute_start = c(21574445L, 21574447L, 21580000L),
    XZ1516_absolute_end = c(21574446L, 21574447L, 21580001L),
    N2_chromosome = c("RagTag_V", "RagTag_V", "RagTag_V"),
    N2_strand = c("+", "+", "-"),
    N2_absolute_start = c(21574460L, 21574462L, 21580020L),
    N2_absolute_end = c(21574461L, 21574462L, 21580021L),
    CB4856_chromosome = c("RagTag_V", "RagTag_V", "RagTag_V"),
    CB4856_strand = c("+", "-", "+"),
    CB4856_absolute_start = c(21574480L, 21574482L, 21580040L),
    CB4856_absolute_end = c(21574481L, 21574482L, 21580041L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  for (id in c("XZ1516", "N2", "CB4856")) {
    sp <- add_individual(
      sp,
      test_syn_individual(
        annotation_file = annotation_path,
        id = id
      )
    )
  }
  sp <- add_multiple_alignment(
    sp,
    odgi_multi_alignment(odgi_tbl, name = "worm-graph-3")
  )

  plot_obj <- ggexon(sp) +
    geom_exon(species = "XZ1516") +
    geom_exon(species = "N2") +
    geom_exon(species = "CB4856") +
    geom_nuclink(
      alignment = "worm-graph-3",
      reference = "XZ1516",
      chr = "RagTag_V",
      subset = c(21574445, 21584356)
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot_obj)

  expect_identical(
    as.character(built@layout$layout$track),
    c(
      "XZ1516",
      "link_worm-graph-3__XZ1516__N2",
      "N2",
      "link_worm-graph-3__N2__CB4856",
      "CB4856"
    )
  )
  expect_identical(as.integer(built@layout$layout$SCALE_Y), c(1L, 2L, 1L, 2L, 1L))
  expect_length(built@data, 4L)
  expect_true(all(unique(as.integer(built@data[[4]]$PANEL)) %in% c(2L, 4L)))
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

test_that("geom_nuclink without facet_genomics() errors clearly", {
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
    geom_exon(species = "XZ1516", chr = "RagTag_V", subset = c(21574445, 21584356)) +
    geom_exon(species = "N2", chr = "V", subset = c(20456948, 20465040)) +
    geom_nuclink(alignment = "XZ1516_vs_N2")

  expect_error(
    ggexon_build(plot_obj),
    "facet_genomics"
  )
})

test_that("geom_exon keeps a blank annotation panel for pairwise species without a SynIndividual", {
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

  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "N2_V_20450000_20490000",
        100, 5, 0, 0, 0, 0, 0, 0,
        "++",
        "V", 20924180, 20467551, 20467656,
        "V", 12207686, 10256132, 10256237,
        1, "105,", "20467551,", "10256132,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  sp <- SynSpecies(name = "PairwiseBlank")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  expect_warning({
    sp <- add_pairwise_alignment(
      sp,
      SynPairAlignment(
        name = "N2_vs_AFRA",
        query_individual = "N2",
        target_individual = "AFRA",
        file = psl_path,
        format = "psl"
      )
    )
  }, "references individuals not attached")

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_nuclink(alignment = "N2_vs_AFRA") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_identical(
    as.character(built@layout$layout$track),
    c("N2", "link_N2_vs_AFRA", "AFRA")
  )
  expect_true(any(as.character(built@data[[2L]]$tspecies) == "AFRA"))
  expect_true(any(as.character(built@data[[2L]]$qspecies) == "N2"))
})

test_that("bare SynIndividuals build as blank panels and keep pairwise links", {
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

  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "AFRA_V_10256132_10256237",
        100, 5, 0, 0, 0, 0, 0, 0,
        "++",
        "V", 20924180, 20467551, 20467656,
        "V", 12207686, 10256132, 10256237,
        1, "105,", "20467551,", "10256132,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  sp <- SynSpecies(name = "PairwiseBare")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_individual(sp, SynIndividual(id = "AFRA"))
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "N2_vs_AFRA",
      query_individual = "N2",
      target_individual = "AFRA",
      file = psl_path,
      format = "psl"
    )
  )

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_genelabel() +
      geom_nuclink(alignment = "N2_vs_AFRA") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_identical(
    as.character(built@layout$layout$track),
    c("N2", "link_N2_vs_AFRA", "AFRA")
  )
  expect_true(any(as.character(built@data[[1L]]$track) == "AFRA"))
  expect_true(any(as.character(built@data[[1L]]$transcripts) == "__blank__AFRA"))
  expect_true(all(built@data[[1L]]$blank_panel[as.character(built@data[[1L]]$track) == "AFRA"]))
  expect_true(any(as.integer(built@data[[3L]]$t_panel) == 3L))
  expect_true(any(as.integer(built@data[[3L]]$q_panel) == 1L))
})

test_that("geom_nuclink accepts multiple pairwise alignment names", {
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
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  psl_n2 <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "AFRA_V_10256132_10256237",
        100, 5, 0, 0, 0, 0, 0, 0,
        "++",
        "V", 20924180, 20467551, 20467656,
        "V", 12207686, 10256132, 10256237,
        1, "105,", "20467551,", "10256132,"
      ),
      collapse = "\t"
    ),
    psl_n2
  )
  psl_xz <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "AFRA_RagTag_V_200_260",
        50, 0, 0, 0, 0, 0, 0, 0,
        "++",
        "RagTag_V", 30000000, 21574445, 21574495,
        "RagTag_V", 5000000, 200, 250,
        1, "50,", "21574445,", "200,"
      ),
      collapse = "\t"
    ),
    psl_xz
  )

  sp <- SynSpecies(name = "MultiLink")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = xz_annotation,
      genome_file = genome_waiver(),
      id = "XZ1516"
    )
  )
  sp <- add_individual(sp, SynIndividual(id = "AFRA"))
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "N2_vs_AFRA",
      query_individual = "N2",
      target_individual = "AFRA",
      file = psl_n2,
      format = "psl"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_AFRA",
      query_individual = "XZ1516",
      target_individual = "AFRA",
      file = psl_xz,
      format = "psl"
    )
  )
  sp <- store_chain_layout(sp, free = list(x = TRUE, y = TRUE))

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon(species = c("N2", "XZ1516", "AFRA")) +
      geom_nuclink(alignment = c("N2_vs_AFRA", "XZ1516_vs_AFRA")) +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_true(any(as.character(built@layout$layout$track) == "link_N2_vs_AFRA"))
  expect_true(any(as.character(built@layout$layout$track) == "link_XZ1516_vs_AFRA"))
  expect_true(length(unique(as.integer(built@data[[2L]]$PANEL))) >= 2L)
})

test_that("stored chain layouts drop unused link panels when one alignment is requested", {
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
  xz_annotation <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  psl_n2 <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "AFRA_V_10256132_10256237",
        100, 5, 0, 0, 0, 0, 0, 0,
        "++",
        "V", 20924180, 20467551, 20467656,
        "V", 12207686, 10256132, 10256237,
        1, "105,", "20467551,", "10256132,"
      ),
      collapse = "\t"
    ),
    psl_n2
  )
  psl_xz <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "AFRA_RagTag_V_200_260",
        50, 0, 0, 0, 0, 0, 0, 0,
        "++",
        "RagTag_V", 30000000, 21574445, 21574495,
        "RagTag_V", 5000000, 200, 250,
        1, "50,", "21574445,", "200,"
      ),
      collapse = "\t"
    ),
    psl_xz
  )

  sp <- SynSpecies(name = "SingleLinkFromStoredLayout")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  sp <- add_individual(
    sp,
    test_syn_individual(
      annotation_file = xz_annotation,
      genome_file = genome_waiver(),
      id = "XZ1516"
    )
  )
  sp <- add_individual(sp, SynIndividual(id = "AFRA"))
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "N2_vs_AFRA",
      query_individual = "N2",
      target_individual = "AFRA",
      file = psl_n2,
      format = "psl"
    )
  )
  sp <- add_pairwise_alignment(
    sp,
    SynPairAlignment(
      name = "XZ1516_vs_AFRA",
      query_individual = "XZ1516",
      target_individual = "AFRA",
      file = psl_xz,
      format = "psl"
    )
  )
  sp <- store_chain_layout(sp, free = list(x = TRUE, y = TRUE))

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon(species = c("N2", "XZ1516", "AFRA")) +
      geom_nuclink(alignment = "N2_vs_AFRA") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_true(any(as.character(built@layout$layout$track) == "link_N2_vs_AFRA"))
  expect_false(any(as.character(built@layout$layout$track) == "link_XZ1516_vs_AFRA"))
})

test_that("geom_nuclink preserves anchor metadata for detailed PSL alignments", {
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

  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        10, 0, 0, 0, 0, 0, 0, 0, "+-",
        "N2_V_100_200", 1000, 100, 120,
        "V", 2000, 1490, 1500,
        2, "10,3,", "100,117,", "500,517,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  detailed_pair <- SynPairAlignment(
    name = "N2_vs_AFRA_detailed",
    query_individual = "N2",
    target_individual = "AFRA",
    file = psl_path,
    format = "psl"
  ) |>
    load_alignment(more = TRUE)

  sp <- SynSpecies(name = "PairwiseDetailed")
  sp <- add_individual(
    sp,
    test_syn_individual(
      genome_file = n2_genome,
      annotation_file = n2_annotation,
      id = "N2",
      annotation_format = "gtf"
    )
  )
  expect_warning({
    sp <- add_pairwise_alignment(sp, detailed_pair)
  }, "references individuals not attached")

  built <- ggexon_build(
    ggexon(sp) +
      geom_exon() +
      geom_nuclink(alignment = "N2_vs_AFRA_detailed") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )

  expect_true(all(c("target_anchor_y", "query_anchor_y", "t_panel", "q_panel") %in% names(built@data[[2L]])))
  expect_true(nrow(built@data[[2L]]) > 0L)
  expect_true(isTRUE(pairwise_alignments(sp)[["N2_vs_AFRA_detailed"]]@metadata$psl_more))
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

test_that("annotation layers can use the full annotation range without a subset", {
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
  built <- ggexon_build(plot_obj)

  expect_true(nrow(built@data[[1L]]) > 0L)
  expect_identical(unique(as.character(built@data[[1L]]$track)), "XZ1516")
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
