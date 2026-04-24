test_that("SynSpecies stores individuals and explicit alignment relationships", {
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

  expect_true(nzchar(paf_path))

  x1 <- test_syn_individual(
    genome_file = genome_path,
    annotation_file = annotation_path,
    id = "XZ1516"
  )
  x2 <- test_syn_individual(
    genome_file = n2_genome_path,
    annotation_file = n2_annotation_path,
    id = "N2",
    annotation_format = "gtf"
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(sp, x1)
  sp <- add_individual(sp, x2)

  pair <- SynPairAlignment(
    name = "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path
  )
  multi <- SynMultiAlignment(
    name = "worm-maf",
    individuals = c("XZ1516", "N2", "CB4856"),
    file = "worms.maf"
  )

  sp <- add_pairwise_alignment(sp, pair)
  sp <- add_multiple_alignment(sp, multi)

  expect_identical(species_name(sp), "Caenorhabditis")
  expect_identical(individual_names(sp), c("XZ1516", "N2"))
  expect_setequal(names(individuals(sp)), c("XZ1516", "N2"))
  expect_identical(names(pairwise_alignments(sp)), "XZ1516_vs_N2")
  expect_identical(names(multiple_alignments(sp)), "worm-maf")
  expect_s4_class(pair, "SynSpeAnnotation")
  expect_s4_class(pair, "SynAnnotation")
  expect_s4_class(multi, "SynSpeAnnotation")
  expect_s4_class(multi, "SynAnnotation")
  expect_identical(query_individual(pair), "XZ1516")
  expect_identical(target_individual(pair), "N2")
  expect_identical(alignment_file(pair), paf_path)
  expect_identical(source_file(pair), paf_path)
  expect_identical(annotation_scope(pair), "species")
  expect_identical(alignment_individuals(pair), c("XZ1516", "N2"))
  expect_identical(
    alignment_individuals(multi),
    c("XZ1516", "N2", "CB4856")
  )
  expect_identical(source_file(multi), "worms.maf")
  expect_identical(annotation_scope(multi), "species")

  unnamed_sp <- sp
  unnamed_sp@individuals <- unname(individuals(unnamed_sp))
  validObject(unnamed_sp)
  expect_identical(individual_names(unnamed_sp), c("XZ1516", "N2"))
})

test_that("add_individual accepts multiple SynIndividuals", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x1 <- test_syn_individual(annotation_file = annotation_path, id = "ECA2968")
  x2 <- test_syn_individual(annotation_file = annotation_path, id = "N2")
  x3 <- test_syn_individual(annotation_file = annotation_path, id = "XZ1516")

  sp <- SynSpecies(name = "elegans") |>
    add_individual(x1, x2, x3)

  expect_identical(individual_names(sp), c("ECA2968", "N2", "XZ1516"))
})

test_that("add_individual requires every added object to be a SynIndividual", {
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )

  x1 <- test_syn_individual(annotation_file = annotation_path, id = "ECA2968")

  expect_error(
    add_individual(SynSpecies(name = "elegans"), x1, "N2"),
    "All inputs after `x` must be SynIndividual objects"
  )
})

test_that("add_pairwise_alignment warns when alignment individuals are missing from SynSpecies", {
  genome_path <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
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

  expect_warning(
    add_pairwise_alignment(
      sp,
      SynPairAlignment(
        name = "XZ1516_vs_N2",
        query_individual = "XZ1516",
        target_individual = "N2",
        file = paf_path
      )
    ),
    "references individuals not attached"
  )
})

test_that("load_annotation loads all stored individuals in a SynSpecies", {
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

  expect_true(all(vapply(individuals(sp), function(ind) is.null(annotation_data(ind)), logical(1))))

  loaded_sp <- load_annotation(sp)

  expect_s4_class(loaded_sp, "SynSpecies")
  expect_identical(species_name(loaded_sp), "Caenorhabditis")
  expect_identical(names(pairwise_alignments(loaded_sp)), "XZ1516_vs_N2")
  expect_true(all(vapply(individuals(loaded_sp), function(ind) methods::is(annotation_data(ind), "GRanges"), logical(1))))
  expect_true(all(vapply(individuals(loaded_sp), function(ind) methods::is(seqinfo(ind), "Seqinfo"), logical(1))))
})

test_that("load_alignment loads all stored alignments in a SynSpecies", {
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

  odgi_tbl <- data.frame(
    node_id = c(1L, 2L),
    sequence = c("AC", "G"),
    XZ1516_chromosome = c("RagTag_V", "RagTag_V"),
    XZ1516_strand = c("+", "-"),
    XZ1516_absolute_start = c(21559983L, 21559985L),
    XZ1516_absolute_end = c(21559984L, 21559985L),
    N2_chromosome = c("V", "V"),
    N2_strand = c("+", "+"),
    N2_absolute_start = c(20454111L, 20454113L),
    N2_absolute_end = c(20454112L, 20454113L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  odgi_tsv <- tempfile(fileext = ".tsv")
  utils::write.table(odgi_tbl, file = odgi_tsv, sep = "\t", quote = FALSE, row.names = FALSE)

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
  sp <- add_multiple_alignment(
    sp,
    SynMultiAlignment(
      name = "XZ1516_N2_odgi",
      individuals = c("XZ1516", "N2"),
      file = odgi_tsv,
      format = "odgi"
    )
  )

  expect_null(pairwise_alignments(sp)[["XZ1516_vs_N2"]]@data)
  expect_null(multiple_alignments(sp)[["XZ1516_N2_odgi"]]@data)

  loaded_sp <- load_alignment(sp)
  loaded_pair <- pairwise_alignments(loaded_sp)[["XZ1516_vs_N2"]]
  loaded_multi <- multiple_alignments(loaded_sp)[["XZ1516_N2_odgi"]]

  expect_true(is.data.frame(loaded_pair@data))
  expect_true(is.data.frame(loaded_multi@data))
  expect_true(isTRUE(loaded_pair@loaded))
  expect_true(isTRUE(loaded_multi@loaded))
  expect_false(isTRUE(loaded_pair@lazy))
  expect_false(isTRUE(loaded_multi@lazy))
  expect_true(all(c("qspecies", "tspecies", "track") %in% names(loaded_pair@data)))
  expect_identical(
    multiple_alignment_data(loaded_sp, alignment = "XZ1516_N2_odgi"),
    odgi_tbl
  )
})

test_that("add_individuals_from_folder imports supported annotation files with filename ids", {
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

  tmp_dir <- tempfile("ggexon-annotations-")
  dir.create(tmp_dir)

  xz_copy <- file.path(tmp_dir, "XZ1516_custom.gff3")
  n2_copy <- file.path(tmp_dir, "N2_custom.gtf")
  txt_copy <- file.path(tmp_dir, "README.txt")

  file.copy(xz_annotation, xz_copy)
  file.copy(n2_annotation, n2_copy)
  writeLines("not an annotation file", txt_copy)

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individuals_from_folder(sp, tmp_dir)

  expect_setequal(names(individuals(sp)), c("XZ1516_custom", "N2_custom"))
  expect_identical(annotation_format(individuals(sp)[["XZ1516_custom"]]), "gff")
  expect_identical(annotation_format(individuals(sp)[["N2_custom"]]), "gtf")
  expect_identical(
    basename(annotation_file(individuals(sp)[["XZ1516_custom"]])),
    "XZ1516_custom.gff3"
  )
  expect_identical(
    basename(annotation_file(individuals(sp)[["N2_custom"]])),
    "N2_custom.gtf"
  )
})

test_that("SynSpecies can initialize individuals directly from an annotation folder", {
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

  tmp_dir <- tempfile("ggexon-constructor-")
  dir.create(tmp_dir)

  xz_copy <- file.path(tmp_dir, "XZ1516.gff3")
  n2_copy <- file.path(tmp_dir, "N2.gtf")
  file.copy(xz_annotation, xz_copy)
  file.copy(n2_annotation, n2_copy)

  sp <- SynSpecies(annotation_folder = tmp_dir)

  expect_identical(species_name(sp), basename(tmp_dir))
  expect_setequal(names(individuals(sp)), c("XZ1516", "N2"))

  named_sp <- SynSpecies(name = "Caenorhabditis", annotation_folder = tmp_dir)
  expect_identical(species_name(named_sp), "Caenorhabditis")
  expect_setequal(names(individuals(named_sp)), c("XZ1516", "N2"))
})

test_that("add_individuals_from_folder errors when no supported annotation files are found", {
  tmp_dir <- tempfile("ggexon-empty-")
  dir.create(tmp_dir)
  writeLines("notes", file.path(tmp_dir, "notes.txt"))

  expect_error(
    add_individuals_from_folder(SynSpecies(name = "Caenorhabditis"), tmp_dir),
    "No annotation files with supported extensions"
  )
})

test_that("subset_species trims selected individuals from species-tagged coords", {
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

  xz <- SynIndividual(
    annotation_file = xz_annotation,
    genome_file = genome_waiver(),
    id = "XZ1516"
  )
  n2 <- SynIndividual(
    annotation_file = n2_annotation,
    genome_file = genome_waiver(),
    id = "N2",
    annotation_format = "gtf"
  )

  xz <- load_annotation(xz)
  n2 <- load_annotation(n2)

  xz_gr <- annotation_data(xz)
  n2_gr <- annotation_data(n2)
  xz_coords <- paste0(
    "XZ1516#",
    as.character(GenomeInfoDb::seqnames(xz_gr))[[1L]],
    ":",
    IRanges::start(xz_gr)[[1L]],
    "-",
    IRanges::end(xz_gr)[[1L]]
  )
  n2_coords <- paste0(
    "N2#",
    as.character(GenomeInfoDb::seqnames(n2_gr))[[1L]],
    ":",
    IRanges::start(n2_gr)[[1L]],
    "-",
    IRanges::end(n2_gr)[[1L]]
  )

  sp <- SynSpecies(name = "Caenorhabditis")
  sp <- add_individual(sp, xz)
  sp <- add_individual(sp, n2)
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = c(1L, 2L),
      ROW = c(1L, 2L),
      COL = c(1L, 1L),
      track = c("XZ1516", "N2"),
      stringsAsFactors = FALSE
    )
  )

  subset_sp <- subset_species(sp, coords = list(xz_coords, n2_coords))

  expect_setequal(names(individuals(subset_sp)), c("XZ1516", "N2"))
  expect_null(species_layout(subset_sp))
  expect_true(length(annotation_data(individuals(subset_sp)[["XZ1516"]])) >= 1L)
  expect_true(length(annotation_data(individuals(subset_sp)[["N2"]])) >= 1L)
  expect_error(
    subset_species(sp, coords = c(xz_coords, xz_coords)),
    "duplicate species tags"
  )
})

test_that("SynLayout shared geom parameters are resolved before layer overrides", {
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
  species_layout(sp) <- SynLayout(
    panels = data.frame(
      PANEL = 1L,
      ROW = 1L,
      COL = 1L,
      track = "XZ1516",
      stringsAsFactors = FALSE
    ),
    exon_height = 2.2,
    y_scale = 77,
    x_translation = 15
  )

  plot_obj <- ggexon(sp) +
    geom_gene(
      chr = "RagTag_V",
      subset = c(21550000, 21680000)
    )
  ctx <- ggexon:::collect_syn_plot_context(plot_obj@layers, plot_obj@data)
  layer <- plot_obj@layers[[1L]]
  layer$syn_plot_context <- ctx
  params <- ggexon:::syn_layer_params(layer)

  expect_identical(params$exon_height, 2.2)
  expect_identical(params$y_scale, 77)
  expect_identical(params$x_translation, 15)

  override_plot <- ggexon(sp) +
    geom_gene(
      chr = "RagTag_V",
      subset = c(21550000, 21680000),
      exon_height = 1.1,
      y_scale = 55,
      x_translation = 3
    )
  override_ctx <- ggexon:::collect_syn_plot_context(override_plot@layers, override_plot@data)
  override_layer <- override_plot@layers[[1L]]
  override_layer$syn_plot_context <- override_ctx
  override_params <- ggexon:::syn_layer_params(override_layer)

  expect_identical(override_params$exon_height, 1.1)
  expect_identical(override_params$y_scale, 55)
  expect_identical(override_params$x_translation, 3)
})

test_that("reference-led comparative subsetting trims both annotations and the paf window", {
  skip_if_not(
    exists("subset_synspecies_window", mode = "function"),
    message = "Comparative window subsetting helper not implemented yet."
  )

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

  out <- subset_synspecies_window(
    sp,
    reference_species = "XZ1516",
    chr = "RagTag_V",
    start = 21574445,
    end = 21584356,
    alignment = "XZ1516_vs_N2"
  )

  expect_true(all(c("windows", "annotations", "links") %in% names(out)))
  expect_true(all(c("XZ1516", "N2") %in% names(out$windows)))
  expect_true(all(c("XZ1516", "N2") %in% names(out$annotations)))

  expect_identical(as.character(out$windows$XZ1516$chr[[1L]]), "V_RagTag")
  expect_identical(as.integer(out$windows$XZ1516$start[[1L]]), 21574445L)
  expect_identical(as.integer(out$windows$XZ1516$end[[1L]]), 21584356L)

  expect_identical(as.character(out$windows$N2$chr[[1L]]), "V")
  expect_true(as.integer(out$windows$N2$start[[1L]]) >= 20456948L)
  expect_true(as.integer(out$windows$N2$end[[1L]]) <= 20465040L)
  expect_true(as.integer(out$windows$N2$start[[1L]]) < as.integer(out$windows$N2$end[[1L]]))

  xz_gr <- out$annotations$XZ1516
  n2_gr <- out$annotations$N2
  expect_s4_class(xz_gr, "GRanges")
  expect_s4_class(n2_gr, "GRanges")
  expect_true(all(as.character(GenomeInfoDb::seqnames(xz_gr)) == "V_RagTag"))
  expect_true(all(IRanges::start(xz_gr) <= 21584356L & IRanges::end(xz_gr) >= 21574445L))
  expect_true(all(as.character(GenomeInfoDb::seqnames(n2_gr)) == "V"))
  expect_true(all(
    IRanges::start(n2_gr) <= as.integer(out$windows$N2$end[[1L]]) &
      IRanges::end(n2_gr) >= as.integer(out$windows$N2$start[[1L]])
  ))

  expect_true(nrow(out$links) > 0L)
  expect_true(all(out$links$qchr == "V_RagTag"))
  expect_true(all(out$links$tchr == "V"))
  expect_true(all(out$links$qstart < 21584356L & out$links$qend > 21574445L))
  expect_true(all(
    out$links$tstart < as.integer(out$windows$N2$end[[1L]]) &
      out$links$tend > as.integer(out$windows$N2$start[[1L]])
  ))
})


test_that("reference-led comparative subsetting errors when pairwise alignments do not define a chain", {
  skip_if_not(
    exists("subset_synspecies_window", mode = "function"),
    message = "Comparative window subsetting helper not implemented yet."
  )

  xz_genome <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
  xz_annotation <- system.file(
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
        genome_file = xz_genome,
        annotation_file = xz_annotation,
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
      name = "XZ1516_vs_CB4856",
      query_individual = "XZ1516",
      target_individual = "CB4856",
      file = paf_path
    )
  )

  expect_error(
    subset_synspecies_window(
      sp,
      reference_species = "XZ1516",
      chr = "RagTag_V",
      start = 21574445,
      end = 21584356
    ),
    "chain"
  )
})

test_that("subset_synspecies_window can derive multiple species windows from an ODGI alignment", {
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

  out <- subset_synspecies_window(
    sp,
    reference_species = "XZ1516",
    chr = "RagTag_V",
    start = 21574445,
    end = 21584356,
    alignment = "worm-graph-3",
    selected_species = c("XZ1516", "N2", "CB4856")
  )

  expect_true(all(c("windows", "annotations", "links") %in% names(out)))
  expect_identical(names(out$windows), c("XZ1516", "N2", "CB4856"))
  expect_identical(names(out$annotations), c("XZ1516", "N2", "CB4856"))
  expect_s4_class(out$annotations$XZ1516, "GRanges")
  expect_s4_class(out$annotations$N2, "GRanges")
  expect_s4_class(out$annotations$CB4856, "GRanges")
  expect_setequal(
    unique(as.character(out$links$track)),
    c("link_worm-graph-3__XZ1516__N2", "link_worm-graph-3__N2__CB4856")
  )
})

test_that("pairwise_alignment_data subsets query and target regions and filters short paf rows", {
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

  paf <- pairwise_alignment_data(
    sp,
    alignment = "XZ1516_vs_N2",
    subset = c(
      XZ1516 = "RagTag_V:21550000-21680000",
      N2 = "V：19100000-20510000"
    ),
    filter = 200
  )

  expect_true(nrow(paf) > 0L)
  expect_true(all(paf$alen >= 200L))
  expect_true(all(as.character(paf$qchr) == "V_RagTag"))
  expect_true(all(as.character(paf$tchr) == "V"))
  expect_true(all(paf$qstart < 21680000L & paf$qend > 21550000L))
  expect_true(all(paf$tstart < 20510000L & paf$tend > 19100000L))
  expect_identical(unique(as.character(paf$qspecies)), "XZ1516")
  expect_identical(unique(as.character(paf$tspecies)), "N2")
  expect_identical(unique(as.character(paf$track)), "link_XZ1516_vs_N2")
})

test_that("subset_synspecies_window works with PSL-backed pairwise alignments", {
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
        "V", 20924180, 20467600, 20467705,
        1, "105,", "20467551,", "20467600,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  sp <- SynSpecies(name = "Caenorhabditis")
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
      id = "AFRA",
      annotation_format = "gtf"
    )
  )
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

  out <- subset_synspecies_window(
    sp,
    reference_species = "N2",
    chr = "V",
    start = 20467560,
    end = 20467620,
    alignment = "N2_vs_AFRA"
  )

  expect_true(all(c("windows", "annotations", "links") %in% names(out)))
  expect_identical(names(out$windows), c("N2", "AFRA"))
  expect_true(nrow(out$links) >= 1L)
  expect_identical(unique(as.character(out$links$qchr)), "V")
  expect_identical(unique(as.character(out$links$tchr)), "V")
})

test_that("subset_pairwise_alignment and filter_pairwise_alignment compose on a SynSpecies", {
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

  subsetted <- subset_pairwise_alignment(
    sp,
    alignment = "XZ1516_vs_N2",
    subset = c(
      XZ1516 = "RagTag_V:21574445-21584356",
      N2 = "V:20456000-20465040"
    )
  )
  filtered <- filter_pairwise_alignment(
    sp,
    alignment = "XZ1516_vs_N2",
    filter = 200
  )

  expect_true(nrow(subsetted) > 0L)
  expect_true(nrow(filtered) > 0L)
  expect_true(all(filtered$alen >= 200L))
  expect_true(all(subsetted$qstart < 21584356L & subsetted$qend > 21574445L))
  expect_true(all(subsetted$tstart < 20465040L & subsetted$tend > 20456000L))
})
