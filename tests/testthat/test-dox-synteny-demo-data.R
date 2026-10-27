test_that("Dox native annotation and alignment inputs preserve source coordinates", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  native <- file.path(demo_dir, "objects")
  expect_true(file.exists(file.path(native, "manifest.json")))
  if (!file.exists(file.path(native, "manifest.json"))) return()
  manifest <- jsonlite::fromJSON(file.path(native, "manifest.json"))
  for (i in seq_len(nrow(manifest$inputs))) {
    expect_identical(unname(tools::md5sum(file.path(demo_dir, manifest$inputs$file[i]))),
      manifest$inputs$md5[i])
  }
  for (i in seq_len(nrow(manifest$outputs))) {
    expect_identical(unname(tools::md5sum(file.path(native, manifest$outputs$file[i]))),
      manifest$outputs$md5[i])
  }
  env <- new.env(parent = globalenv())
  sys.source(file.path(demo_dir, "load-dox-example.R"), envir = env)
  e <- env$load_dox_example(demo_dir)
  for (id in e$windows$species) {
    individual <- SynIndividual(id = id, annotation_file = file.path(native, paste0(id, ".gtf")),
      annotation_format = "gtf")
    gr <- query_features(individual, chr = "X", feature_type = NULL, all = TRUE)
    pieces <- gr[as.character(S4Vectors::mcols(gr)$type) %in% c("CDS", "UTR")]
    actual <- paste(IRanges::start(pieces), IRanges::end(pieces), S4Vectors::mcols(pieces)$type,
      S4Vectors::mcols(pieces)$transcript_id)
    expected <- e$exons[e$exons$species == id, ]
    expect_setequal(actual, paste(expected$xmin, expected$xmax, expected$type, expected$transcripts))
    expect_equal(sum(S4Vectors::mcols(gr)$type == "gene"), c(Dmau=2, Dsim=3, Dsech=2)[[id]])
  }
  for (pair in list(c("Dmau", "Dsim"), c("Dsim", "Dsech"))) {
    actual <- pairwise_alignment_data(SynPairAlignment(name = paste(pair, collapse = "_"),
      target_individual = pair[1], query_individual = pair[2],
      file = file.path(native, paste0(paste(pair, collapse = "_"), ".paf"))))
    expected <- e$links[e$links$tspecies == pair[1] & e$links$qspecies == pair[2], ]
    for (column in c("tstart", "tend", "qstart", "qend", "strand", "nmatch"))
      expect_equal(actual[[column]], expected[[column]])
  }
})

test_that("Dox native BigWigs retain per-base strand magnitudes on the correct assembly", {
  skip_on_os("windows")
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  native <- file.path(demo_dir, "objects")
  expect_true(file.exists(file.path(native, "Dsim.RNAplus.bw")))
  if (!file.exists(file.path(native, "Dsim.RNAplus.bw"))) return()
  windows <- utils::read.delim(file.path(demo_dir, "windows.tsv"))
  signal <- utils::read.delim(file.path(demo_dir, "coverage.tsv.gz"))
  for (i in seq_len(nrow(windows))) for (strand in c("+", "-")) {
    w <- windows[i, ]
    annotation <- SynBigWigAnnotation(name = "RNA",
      bigwig_file = file.path(native, paste0(w$species, if (strand == "+") ".RNAplus.bw" else ".RNAminus.bw")))
    gr <- query_signal(annotation, "X", w$start, w$end)
    expected <- numeric(w$end - w$start + 1L)
    s <- signal[signal$species == w$species & signal$strand == strand, ]
    for (j in seq_len(nrow(s))) expected[seq.int(s$start[j], s$end[j]) - w$start + 1L] <- abs(s$score[j])
    actual <- numeric(length(expected))
    a <- IRanges::start(gr)
    b <- IRanges::end(gr)
    score <- S4Vectors::mcols(gr)$score
    for (j in seq_along(gr)) actual[seq.int(a[j], b[j]) - w$start + 1L] <- score[j]
    expect_equal(actual, expected, tolerance = 1e-6)
    expect_equal(unname(GenomeInfoDb::seqlengths(gr)), c(Dmau=23222241, Dsim=23123156, Dsech=22909512)[[w$species]])
  }
})

test_that("Dox example has three real compact loci and verified RNA-seq coverage", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  manifest <- file.path(demo_dir, "manifest.json")
  expect_true(file.exists(manifest))
  if (!file.exists(manifest)) return()
  info <- jsonlite::fromJSON(manifest)
  windows <- utils::read.delim(file.path(demo_dir, "windows.tsv"))
  expect_identical(windows$species, c("Dmau", "Dsim", "Dsech"))
  genes <- utils::read.delim(file.path(demo_dir, "genes.tsv"))
  expect_equal(as.integer(table(factor(genes$species, windows$species))), c(2L, 3L, 2L))
  expect_identical(genes$species[genes$gene_name == "Dox"], "Dsim")
  signal <- utils::read.delim(file.path(demo_dir, "coverage.tsv.gz"))
  expect_setequal(unique(signal$species), c("Dmau", "Dsim", "Dsech"))
  expect_setequal(unique(signal$strand), c("+", "-"))
  expect_true(all(is.finite(signal$score)))
  expect_true(all(signal$start <= signal$end))
  for (id in c("Dmau", "Dsim", "Dsech")) {
    w <- windows[windows$species == id, ]
    s <- signal[signal$species == id, ]
    expect_true(all(s$start >= w$start & s$end <= w$end))
    expect_true(any(abs(s$score) > 0))
    expect_length(unique(s$sample), 1L)
  }
  expect_true(all(info$sources$range_requests_verified[info$sources$kind == "RNA-seq"]))
  excluded <- info$sources[info$sources$species == "Dsim" & info$sources$kind == "RNA-seq", ]
  expect_equal(nrow(excluded), 2L)
  expect_true(all(!excluded$included))
  remapped <- info$sources[info$sources$kind == "RNA-seq remapped", ]
  expect_equal(nrow(remapped), 2L)
  expect_true(all(remapped$included))
  expect_equal(info$dox_cds_mapping$identity, 100)
  for (i in seq_len(nrow(info$outputs))) {
    path <- file.path(demo_dir, info$outputs$file[i])
    expect_identical(unname(tools::md5sum(path)), info$outputs$md5[i])
  }
})

test_that("Dsim remapping uses the complete original library and plotted assembly", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  path <- file.path(demo_dir, "Dsim-remapping-qc.json")
  expect_true(file.exists(path))
  if (!file.exists(path)) return()
  qc <- jsonlite::fromJSON(path)
  expect_identical(qc$run, "SRR23418969")
  expect_identical(qc$sample, "GSM7039340")
  expect_identical(qc$assembly, "GCA_004382185.1")
  expect_identical(qc$chrom, "CM015611.1")
  expect_equal(qc$chromosome_length, 23123156)
  expect_equal(qc$read_pairs, 40381684)
  expect_equal(qc$primary_records, 80763368)
  expect_equal(qc$min_mapq, 20)
  expect_true(qc$independent_depth_verified)
  expect_equal(qc$verified_genomic_bases, 43781)
  expect_gt(qc$strand_evidence$agreement, .95)
  expect_equal(qc$inputs$md5[1:2],
    c("093d1bbce10343a70c83fdb212d06c79", "c741f7a08fe4c49a899d10e1d1bf5c9a"))
})

test_that("Dox coding exons reconstruct the published CDS exactly", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  path <- file.path(demo_dir, "Dox-published-CDS.fa")
  expect_true(file.exists(path))
  if (!file.exists(path)) return()
  f <- utils::read.delim(file.path(demo_dir, "features.tsv"))
  f <- f[f$gene_name == "Dox" & f$type == "CDS", ]
  expect_equal(nrow(f), 3L)
  w <- utils::read.delim(file.path(demo_dir, "windows.tsv"))
  start <- w$start[w$species == "Dsim"]
  dna <- Biostrings::readDNAStringSet(file.path(demo_dir, "Dsim.fa"))[[1L]]
  parts <- vapply(seq_len(nrow(f)), function(i) as.character(Biostrings::subseq(dna,
    start = f$feature_start[i] - start + 1L, end = f$feature_end[i] - start + 1L)), character(1))
  reconstructed <- Biostrings::reverseComplement(Biostrings::DNAString(paste(parts, collapse = "")))
  expected <- Biostrings::readDNAStringSet(path)[[1L]]
  expect_identical(as.character(reconstructed), as.character(expected))
})

test_that("Dox nucleotide ribbons match the actual bundled genomic sequences", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  path <- file.path(demo_dir, "links.tsv")
  expect_true(file.exists(path))
  if (!file.exists(path)) return()
  windows <- utils::read.delim(file.path(demo_dir, "windows.tsv"))
  links <- utils::read.delim(path)
  expect_setequal(unique(paste(links$tspecies, links$qspecies)),
    c("Dmau Dsim", "Dsim Dsech"))
  expect_true(all(links$tend - links$tstart == links$qend - links$qstart))
  seqs <- lapply(windows$species, function(id)
    Biostrings::readDNAStringSet(file.path(demo_dir, paste0(id, ".fa")))[[1L]])
  names(seqs) <- windows$species
  starts <- stats::setNames(windows$start, windows$species)
  for (i in seq_len(nrow(links))) {
    r <- links[i, ]
    t <- Biostrings::subseq(seqs[[r$tspecies]],
      start = r$tstart - starts[[r$tspecies]] + 1L,
      end = r$tend - starts[[r$tspecies]] + 1L)
    q <- Biostrings::subseq(seqs[[r$qspecies]],
      start = r$qstart - starts[[r$qspecies]] + 1L,
      end = r$qend - starts[[r$qspecies]] + 1L)
    if (r$strand == "-") q <- Biostrings::reverseComplement(q)
    matches <- sum(strsplit(base::toupper(as.character(t)), "")[[1L]] ==
      strsplit(base::toupper(as.character(q)), "")[[1L]])
    expect_equal(matches, r$nmatch)
  }
})

test_that("Dox figure pairs independent RNA subpanels with each species annotation", {
  demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
  helper <- file.path(demo_dir, "load-dox-example.R")
  expect_true(file.exists(helper))
  if (!file.exists(helper)) return()
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)
  example <- env$load_dox_example(demo_dir)
  for (factor in c(1, 3)) {
    result <- env$plot_dox_example(example, intron_factor = factor)
    built <- ggexon_build(result$plot)
    layout <- as.data.frame(built@layout$layout)
    expect_equal(sum(layout$panel_type == "annotation"), 3L)
    expect_equal(sum(layout$panel_type == "link"), 2L)
    expect_equal(sum(layout$panel_type == "coverage"), 3L)
    expect_identical(as.character(layout$panel_type),
      c("coverage", "annotation", "link", "coverage", "annotation", "link", "coverage", "annotation"))
    coverage <- layout[layout$panel_type == "coverage", ]
    annotation <- layout[layout$panel_type == "annotation", ]
    if (nrow(coverage) != 3L) next
    expect_identical(as.character(coverage$track), as.character(annotation$track))
    expect_identical(as.integer(coverage$x_source_panel), as.integer(annotation$PANEL))
    expect_identical(coverage$SCALE_X, annotation$SCALE_X)
    expect_true(all(as.integer(built@data[[1L]]$PANEL) %in% annotation$PANEL))
    for (id in c("Dmau", "Dsim", "Dsech")) {
      panel <- as.integer(coverage$PANEL[coverage$track == id])
      yrange <- built@layout$panel_params[[panel]]$y.range
      peak <- max(abs(example$coverage$score[example$coverage$species == id]))
      expect_equal(yrange, c(0, peak * 1.1))
      expect_equal(peak / diff(yrange), 1 / 1.1)
    }
    link <- layout[layout$panel_type == "link", ]
    expect_true(all(link$t_panel %in% annotation$PANEL))
    expect_true(all(link$q_panel %in% annotation$PANEL))
    signal_layer <- which(vapply(result$plot$layers,
      function(layer) inherits(layer$geom, "GeomCoverage"), logical(1)))
    signal <- built@data[[signal_layer]]
    expect_setequal(as.character(signal$track), c("Dmau", "Dsim", "Dsech"))
    labels <- unlist(lapply(built@data, function(data)
      if ("label" %in% names(data)) as.character(data$label) else character()))
    expect_false(any(grepl("RNA-seq unavailable", labels, fixed = TRUE)))
    source_key <- paste(example$coverage$track, example$coverage$start,
      example$coverage$end, example$coverage$signal)
    rendered_key <- paste(signal$track, signal$interval_start,
      signal$interval_end, signal$coverage)
    expect_true(all(rendered_key %in% source_key))
    table <- ggplot2::ggplot_gtable(built)
    index <- match(paste0("panel-", layout$COL, "-", layout$ROW), table$layout$name)
    heights <- table$heights[table$layout$t[index]]
    expect_equal(as.numeric(heights), c(.5, 1, .5, .5, 1, .5, .5, 1))
    expect_silent(ggplot2::ggplot_gtable(built))
  }
})
