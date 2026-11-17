test_that("the MHC bundle retains three haplotypes, functional MHC and background genes", {
  path <- system.file("extdata", "mhc_haplotypes", package = "ggexon")
  expect_true(nzchar(path), info = "The offline three-haplotype bundle is required")
  if (!nzchar(path)) return()
  windows <- read.delim(file.path(path, "windows.tsv"))
  genes <- read.delim(file.path(path, "genes.tsv"))
  expect_identical(windows$track, c("SSYh1", "GGOh1", "HSAh1"))
  expect_identical(windows$label, c("Siamang H1", "Gorilla H1", "Human CHM13"))
  expect_length(unique(windows$end - windows$start + 1L), 1L)
  expect_true(all(windows$end - windows$start < 4e6))
  expect_false(any(grepl("pseudo", genes$gene_type, ignore.case = TRUE)))
  mhc <- genes[genes$role == "MHC", ]
  expect_true(all(mhc$source_status == "functional"))
  expect_true(all(c("Sysy-J", "Gogo-H") %in% mhc$gene_name))
  expect_false(any(c("HLA-H", "HLA-J", "Gogo-DRB") %in% genes$gene_name))
  expect_equal(as.integer(table(factor(mhc$track, windows$track))), c(18L, 21L, 19L))
  context <- genes[genes$role == "context", ]
  expect_true(all(table(factor(context$track, windows$track)) > 0))
  expect_false("CYP21A1P" %in% context$gene_name)
  # Follow the source biotype rather than inferring status from a name suffix.
  expect_true("TAPBP" %in% context$gene_name)
  manifest <- jsonlite::fromJSON(file.path(path, "manifest.json"))
  for (i in seq_len(nrow(manifest$files))) {
    expect_identical(unname(tools::md5sum(file.path(path, manifest$files$file[i]))),
      manifest$files$md5[i])
  }
  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    individual <- SynIndividual(id = w$track,
      annotation_file = file.path(path, paste0(w$track, ".gff3")),
      annotation_format = "gff")
    gr <- query_features(individual, chr = w$chr, feature_type = "gene", all = TRUE)
    source <- genes[genes$track == w$track, ]
    actual <- paste(IRanges::start(gr), IRanges::end(gr),
      as.character(BiocGenerics::strand(gr)), S4Vectors::mcols(gr)$gene_name)
    expect_setequal(actual, paste(source$start, source$end, source$strand, source$gene_name))
  }
})

test_that("the MHC pairwise alignments use the selected native assembly windows", {
  path <- system.file("extdata", "mhc_haplotypes", package = "ggexon")
  expect_true(nzchar(path))
  if (!nzchar(path)) return()
  windows <- read.delim(file.path(path, "windows.tsv"))
  for (i in 1:2) {
    target <- windows[i, ]
    query <- windows[i + 1L, ]
    name <- paste(target$track, query$track, sep = "_")
    blocks <- pairwise_alignment_data(SynPairAlignment(name = name,
      target_individual = target$track, query_individual = query$track,
      file = file.path(path, paste0(name, ".paf"))))
    expect_gt(nrow(blocks), 0L)
    expect_true(all(blocks$tchr == target$chr & blocks$qchr == query$chr))
    expect_true(all(blocks$tstart >= target$start & blocks$tend <= target$end))
    expect_true(all(blocks$qstart >= query$start & blocks$qend <= query$end))
    expect_true(all(blocks$nmatch / blocks$alen >= 0.7))
    expect_true(all(pmin(blocks$tend - blocks$tstart + 1,
      blocks$qend - blocks$qstart + 1) >= 500))
    if (i == 1L) expect_true(any(blocks$strand == "-"))
  }
})

test_that("MHC prepared gene views are derived from their stored feature annotations", {
  path <- system.file("extdata", "mhc_haplotypes", package = "ggexon")
  expect_true(nzchar(path))
  if (!nzchar(path)) return()
  env <- new.env(parent = globalenv())
  sys.source(file.path(path, "prepare-mhc-example.R"), envir = env)
  comparison <- SynSpecies(name = "MHC test")
  windows <- read.delim(file.path(path, "windows.tsv"))
  for (id in windows$track) {
    comparison <- add_individual(comparison, SynIndividual(id = id,
      annotation_file = file.path(path, paste0(id, ".gff3")),
      annotation_format = "gff"))
  }
  prepared <- env$prepare_mhc_context(comparison)
  prepared <- prepare_gene_lollipop(prepared, name = "mhc", reference = "HSAh1",
    key = "plot_label", where = list(gene_role = "MHC"), nrow = 4, ncol = 6,
    order = c("F", "G", "H", "OKO", "A", "J", "E", "C", "B", "DRA",
      "DRB3", "DRB5", "DRB1", "DQA1", "DQB1", "DQA2", "DQB2", "DOB",
      "DMB", "DMA", "DOA", "DPA1", "DPB1"))
  mhc <- env$mhc_plot_data(prepared, "MHC")
  context <- env$mhc_plot_data(prepared, "context")
  expect_equal(nrow(mhc), 58L)
  expect_length(mhc$head_row, nrow(mhc))
  if ("head_row" %in% names(mhc)) {
    expect_true(all(mhc$head_row %in% 1:4))
    for (key in unique(mhc$label)) {
      expect_length(unique(mhc$head_row[mhc$label == key]), 1L)
      expect_length(unique(mhc$head_col[mhc$label == key]), 1L)
    }
  }
  expect_gt(nrow(context), 0L)
  expect_setequal(mhc$track, windows$track)
  expect_setequal(context$track, windows$track)
  expect_true(all(mhc$x == (mhc$xmin + mhc$xmax) / 2))
  expect_true(all(context$ymin == 0))
  expect_false("mhc_plot_data" %in% names(comparison@metadata))
  expect_error(env$mhc_plot_data(prepared, "missing"), "Unknown")
})
