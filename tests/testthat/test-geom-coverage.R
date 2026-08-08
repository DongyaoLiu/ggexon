coverage_fixture_species <- function(strains = c(
  "XZ1516", "ECA2091", "ECA701", "ECA2191"
)) {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  gtf <- file.path(fixture_dir, "WS285.ugt31-zeel1-peel1-nekl1.gtf")
  species <- SynSpecies(name = "PEEL-1 coverage")

  for (strain in strains) {
    individual <- SynIndividual(
      annotation_file = gtf,
      annotation_format = "gtf",
      id = strain
    )
    individual <- add_annotation(
      individual,
      SynBigWigAnnotation(
        "coverage",
        file.path(fixture_dir, paste0(strain, ".raw.bw")),
        metadata = list(signal_unit = "raw_depth")
      )
    )
    species <- add_individual(species, individual)
  }
  species
}

coverage_fixture_context <- function(strains = c(
  "XZ1516", "ECA2091", "ECA701", "ECA2191"
)) {
  list(
    windows = stats::setNames(rep(list(list(
      chr = "I", start = 2332338L, end = 2373985L
    )), length(strains)), strains),
    annotation_species_order = strains,
    query_cache = new.env(parent = emptyenv())
  )
}

coverage_synthetic_species <- function(ids,
                                       signal_starts,
                                       scores) {
  stopifnot(length(ids) == length(signal_starts), length(ids) == length(scores))
  gtf <- tempfile(fileext = ".gtf")
  writeLines(
    c(
      "chr1\ttest\tgene\t10\t40\t.\t+\t.\tgene_id \"gene1\"; gene_name \"gene1\";",
      "chr1\ttest\ttranscript\t10\t40\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\";",
      "chr1\ttest\texon\t10\t40\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\"; exon_number \"1\";"
    ),
    gtf
  )

  species <- SynSpecies(name = "synthetic coverage")
  for (i in seq_along(ids)) {
    bw_path <- tempfile(fileext = ".bw")
    signal <- GenomicRanges::GRanges(
      "chr1",
      IRanges::IRanges(signal_starts[[i]], signal_starts[[i]]),
      score = scores[[i]]
    )
    GenomeInfoDb::seqinfo(signal) <- GenomeInfoDb::Seqinfo(
      "chr1",
      seqlengths = 100L
    )
    rtracklayer::export.bw(signal, bw_path)

    individual <- SynIndividual(
      annotation_file = gtf,
      annotation_format = "gtf",
      id = ids[[i]]
    )
    individual <- add_annotation(
      individual,
      SynBigWigAnnotation("coverage", bw_path)
    )
    species <- add_individual(species, individual)
  }
  species
}

coverage_synthetic_plot <- function(species) {
  ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      chr = "chr1",
      subset = c(1L, 50L),
      annotation_type = "exon"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")
}

test_that("Syn coverage dispatch keeps one interval row per BigWig record", {
  species <- coverage_fixture_species()
  context <- coverage_fixture_context()

  data <- syn_to_coverage_df(
    species,
    annotation = "coverage",
    context = context
  )

  expect_identical(unique(data$track), names(individuals(species)))
  expect_true(all(data$chr == "I"))
  expect_true(all(data$interval_start >= 2332338L))
  expect_true(all(data$interval_end <= 2373985L))
  expect_identical(data$xmin, data$interval_start - 0.5)
  expect_identical(data$xmax, data$interval_end + 0.5)
  expect_true(all(data$coverage >= 0))
  expect_lt(nrow(data), 4L * (2373985L - 2332338L + 1L))
  expect_true(all(data$.ggexon_panel_role == "coverage"))
})

test_that("SynLocusSet coverage preserves repeated-individual panel aliases", {
  species <- coverage_synthetic_species("sample", 10L, 3)
  aliases <- c("sample__left", "sample__right")
  loci <- SynLocusSet(
    "repeated sample",
    data.frame(
      locus_id = c("left", "right"),
      individual = "sample",
      seqname = "chr1",
      start = 1L,
      end = 50L,
      row_group = "sample",
      col_group = c("left", "right"),
      track = aliases,
      stringsAsFactors = FALSE
    )
  )
  species <- species |>
    add_locus_set(loci) |>
    use_locus_grid(row_order = "sample", col_order = c("left", "right"))

  coverage_layer <- geom_coverage(annotation = "coverage")
  exon_layer <- geom_exon(annotation_type = "exon")
  context <- collect_syn_plot_context(
    list(coverage_layer, exon_layer),
    species
  )
  data <- syn_to_coverage_df(
    species,
    annotation = "coverage",
    context = context
  )

  expect_identical(unique(data$track), aliases)
  expect_true(all(data$individual == "sample"))
  expect_true(all(data$species == "sample"))

  built <- ggexon_build(
    ggexon(species) +
      geom_coverage(annotation = "coverage") +
      geom_exon(annotation_type = "exon") +
      facet_genomics(ggplot2::vars(track), scales = "free")
  )
  annotation_rows <- built@layout$layout$panel_type == "annotation"
  expect_identical(
    as.character(built@layout$layout$track[annotation_rows]),
    aliases
  )
  expect_identical(unique(as.character(built@data[[1L]]$track)), aliases)
})

test_that("BigWig resolution validates names and ambiguous defaults", {
  species <- coverage_fixture_species("XZ1516")
  individual <- individuals(species)[["XZ1516"]]

  expect_error(
    resolve_syn_bigwig_annotation(individual, annotation = "missing"),
    "XZ1516.*missing|missing.*XZ1516"
  )

  individual <- add_annotation(
    individual,
    SynBigWigAnnotation(
      "replicate",
      source_file(get_annotation(individual, "coverage"))
    )
  )
  expect_error(
    resolve_syn_bigwig_annotation(individual),
    "XZ1516.*multiple|multiple.*XZ1516"
  )
  expect_null(resolve_syn_bigwig_annotation(
    individual,
    allow_missing = TRUE
  ))
})

test_that("Syn coverage dispatch identifies an individual missing coverage", {
  species <- coverage_fixture_species(c("XZ1516", "ECA2091"))
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  gtf <- file.path(fixture_dir, "WS285.ugt31-zeel1-peel1-nekl1.gtf")
  missing <- SynIndividual(
    annotation_file = gtf,
    annotation_format = "gtf",
    id = "ECA2091"
  )
  species <- add_individual(species, missing)

  expect_error(
    syn_to_coverage_df(
      species,
      annotation = "coverage",
      context = coverage_fixture_context(c("XZ1516", "ECA2091"))
    ),
    "ECA2091.*coverage|coverage.*ECA2091"
  )
})

test_that("BigWig window queries use only the build-local cache", {
  species <- coverage_fixture_species("XZ1516")
  individual <- individuals(species)[["XZ1516"]]
  annotation <- get_annotation(individual, "coverage")
  before <- serialize(annotation, NULL)
  context <- coverage_fixture_context("XZ1516")
  window <- context$windows[["XZ1516"]]

  first <- query_syn_bigwig_window(annotation, window, context)
  second <- query_syn_bigwig_window(annotation, window, context)

  expect_identical(second, first)
  expect_length(ls(context$query_cache, all.names = TRUE), 1L)
  expect_identical(serialize(annotation, NULL), before)
})

test_that("plot builds clone layers and discard their ephemeral query cache", {
  species <- coverage_synthetic_species("sample", 10L, 3)
  plot <- coverage_synthetic_plot(species)
  original_coverage_layer <- plot@layers[[1L]]
  expect_false("syn_plot_context" %in% ls(original_coverage_layer, all.names = TRUE))

  built <- ggexon_build(plot)
  built_coverage_layer <- built@plot@layers[[1L]]
  built_context <- built_coverage_layer$syn_plot_context

  expect_false(identical(built_coverage_layer, original_coverage_layer))
  expect_false("syn_plot_context" %in% ls(original_coverage_layer, all.names = TRUE))
  expect_null(original_coverage_layer$syn_plot_context$query_cache)
  expect_null(built_context$query_cache)
  expect_null(built_context$syn_data)
  expect_true(is.list(built_context$windows))
  expect_identical(names(built_context$windows), "sample")
})

test_that("Syn coverage dispatch preserves raw scores and empty schema", {
  bw_path <- tempfile(fileext = ".bw")
  signal <- GenomicRanges::GRanges(
    "chr1",
    IRanges::IRanges(c(20L, 40L), c(29L, 49L)),
    score = c(2.5, 17.25)
  )
  GenomeInfoDb::seqinfo(signal) <- GenomeInfoDb::Seqinfo(
    "chr1",
    seqlengths = 100L
  )
  rtracklayer::export.bw(signal, bw_path)

  individual <- SynIndividual(id = "sparse")
  individual <- add_annotation(
    individual,
    SynBigWigAnnotation("coverage", bw_path)
  )
  context <- list(
    windows = list(sparse = list(chr = "chr1", start = 1L, end = 60L)),
    annotation_species_order = "sparse",
    query_cache = new.env(parent = emptyenv())
  )

  data <- syn_to_coverage_df(individual, context = context)
  expect_identical(data$coverage, c(2.5, 17.25))
  expect_identical(data$interval_start, c(20L, 40L))
  expect_identical(data$interval_end, c(29L, 49L))
  expect_identical(data$xmin, c(19.5, 39.5))
  expect_identical(data$xmax, c(29.5, 49.5))

  context$windows$sparse <- list(chr = "chr1", start = 1L, end = 10L)
  empty <- syn_to_coverage_df(individual, context = context)
  expect_identical(
    names(empty),
    c(
      "track", "individual", "species", "chr", "interval_start",
      "interval_end", "genomic_xmin", "genomic_xmax", "xmin", "xmax",
      "coverage", "group", ".ggexon_panel_role"
    )
  )
  expect_equal(nrow(empty), 0L)
})

test_that("geom_coverage accepts Syn and interval data inputs", {
  layer <- geom_coverage(annotation = "coverage", species = "XZ1516")
  expect_s3_class(layer, "LayerSyn")
  expect_identical(GeomCoverage$ggexon_panel_role, "coverage")

  direct <- data.frame(
    track = "sample",
    xmin = 1L,
    xmax = 10L,
    coverage = 4
  )
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(
      direct,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        coverage = coverage,
        track = track
      )
    ) + geom_coverage()
  )$data[[1L]]
  expect_identical(built$ymin, 0)
  expect_identical(built$ymax, 4)
  expect_true(all(built$.ggexon_panel_role == "coverage"))
})

test_that("coverage grobs use full inclusive widths for one-base and adjacent intervals", {
  direct <- data.frame(
    track = "sample",
    xmin = c(10L, 11L),
    xmax = c(10L, 12L),
    coverage = c(1, 2)
  )
  plot <- ggplot2::ggplot(
    direct,
    ggplot2::aes(
      xmin = xmin,
      xmax = xmax,
      coverage = coverage,
      track = track
    )
  ) + geom_coverage()
  built <- ggplot2::ggplot_build(plot)
  data <- built$data[[1L]]

  expect_equal(data$interval_start, c(10, 11))
  expect_equal(data$interval_end, c(10, 12))
  expect_equal(data$genomic_xmin, c(10, 11))
  expect_equal(data$genomic_xmax, c(10, 12))
  expect_identical(data$xmin, c(9.5, 10.5))
  expect_identical(data$xmax, c(10.5, 12.5))
  expect_identical(data$xmax[[1L]], data$xmin[[2L]])

  grob <- GeomCoverage$draw_panel(
    data,
    built$layout$panel_params[[1L]],
    built$plot$coordinates
  )
  widths <- as.numeric(grob$width)
  expect_gt(widths[[1L]], 0)
  expect_equal(widths[[2L]] / widths[[1L]], 2, tolerance = 1e-8)
})

test_that("geom_coverage rejects negative and non-finite scores", {
  invalid <- c(-1, Inf, -Inf, NaN, NA_real_)
  for (score in invalid) {
    direct <- data.frame(
      track = "sample",
      xmin = 1L,
      xmax = 1L,
      coverage = score
    )
    expect_error(
      ggplot2::ggplot_build(
        ggplot2::ggplot(
          direct,
          ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            coverage = coverage,
            track = track
          )
        ) + geom_coverage()
      ),
      "finite.*non-negative|non-negative.*finite",
      info = paste("score:", score)
    )
  }
})

test_that("geom_coverage inherits an explicit geom_genebox window", {
  species <- coverage_fixture_species("XZ1516")

  built <- NULL
  expect_warning(
    built <- ggplot2::ggplot_build(
      ggexon(species) +
        geom_genebox(chr = "I", subset = c(2332338L, 2373985L)) +
        geom_coverage(annotation = "coverage")
    ),
    "omitted 2 transcript"
  )
  coverage <- built$data[[2L]]

  expect_gt(nrow(coverage), 0L)
  expect_true(all(coverage$interval_start >= 2332338L))
  expect_true(all(coverage$interval_end <= 2373985L))
  expect_identical(coverage$xmin, coverage$interval_start - 0.5)
  expect_identical(coverage$xmax, coverage$interval_end + 0.5)
  expect_true(all(coverage$.ggexon_panel_role == "coverage"))
})

test_that("geom_coverage soft-deprecates file-driven arguments", {
  expect_warning(
    geom_coverage(bigwig = "coverage.bw"),
    "deprecated"
  )
  expect_warning(
    geom_coverage(ref_chr = "I"),
    "deprecated"
  )
})

test_that("coverage bands preserve raw scores and reserve 25 percent below zero", {
  coverage <- data.frame(
    PANEL = factor(c(1, 1, 3, 3)),
    coverage = c(0, 20, 0, 10),
    ymin = 0,
    ymax = c(0, 20, 0, 10)
  )
  annotation <- data.frame(
    PANEL = factor(c(1, 1, 3, 3)),
    ymin = c(1, 2, 1, 2),
    ymax = c(1.8, 2.8, 1.8, 2.8),
    y_middle = c(1.4, 2.4, 1.4, 2.4)
  )
  link <- data.frame(PANEL = factor(2), ymin = 0, ymax = 1)

  layers <- list(
    list(geom = structure(list(ggexon_panel_role = "coverage"), class = "mock")),
    list(geom = structure(list(ggexon_panel_role = "annotation"), class = "mock")),
    list(geom = structure(list(ggexon_panel_role = "link"), class = "mock"))
  )
  result <- apply_coverage_panel_bands(
    list(coverage, annotation, link),
    layers,
    fraction = 0.25
  )

  expect_identical(result$data[[1L]]$coverage, coverage$coverage)
  expect_identical(result$data[[1L]]$ymax, coverage$coverage)
  expect_equal(range(result$data[[2L]]$ymin, result$data[[2L]]$ymax), c(-5, 0))
  expect_identical(result$data[[3L]], link)
  expect_equal(result$coverage_max, 20)
  expect_equal(result$annotation_depth, 5)
  expect_setequal(result$composite_panels, c(1L, 3L))
})

test_that("all-zero coverage retains zeros and uses a finite training fallback", {
  coverage <- data.frame(
    PANEL = factor(c(1, 1)),
    coverage = c(0, 0),
    ymin = c(0, 0),
    ymax = c(0, 0)
  )
  annotation <- data.frame(
    PANEL = factor(c(1, 1)),
    y = c(2, 2),
    ymin = c(2, 2),
    ymax = c(2, 2)
  )
  layers <- list(
    list(geom = structure(list(ggexon_panel_role = "coverage"), class = "mock")),
    list(geom = structure(list(ggexon_panel_role = "annotation"), class = "mock"))
  )

  result <- apply_coverage_panel_bands(list(coverage, annotation), layers)

  expect_identical(result$data[[1L]]$coverage, c(0, 0))
  expect_identical(result$data[[1L]]$ymax, c(0, 0))
  expect_true(all(result$data[[1L]]$ymin == 0))
  expect_equal(result$coverage_max, 0)
  expect_equal(result$training_max, 1)
  expect_equal(result$annotation_depth, 0.25)
  expect_true(all(result$data[[2L]]$y == -0.125))
})

test_that("requested coverage panels participate even when no signal row exists", {
  coverage <- data.frame(
    PANEL = factor(2L, levels = 1:2),
    coverage = 4,
    ymin = 0,
    ymax = 4
  )
  annotation <- data.frame(
    PANEL = factor(c(1L, 2L), levels = 1:2),
    y = c(1, 1),
    ymin = c(0.8, 0.8),
    ymax = c(1.2, 1.2)
  )
  layers <- list(
    list(
      geom = structure(list(ggexon_panel_role = "coverage"), class = "mock"),
      syn_plot_context = list(coverage_tracks = c("empty", "signal"))
    ),
    list(geom = structure(list(ggexon_panel_role = "annotation"), class = "mock"))
  )
  layout <- list(layout = data.frame(
    PANEL = factor(1:2),
    track = c("empty", "signal"),
    species = c("empty", "signal"),
    SCALE_Y = 1:2,
    stringsAsFactors = FALSE
  ))

  result <- apply_coverage_panel_bands(
    list(coverage, annotation),
    layers,
    layout = layout
  )

  expect_identical(result$composite_panels, 1:2)
  expect_true(all(result$data[[2L]]$y < 0))
  expect_true(all(result$data[[2L]]$ymin >= -1))
  expect_true(all(result$data[[2L]]$ymax <= 0))
})

test_that("one empty requested coverage panel keeps its identity and shared band", {
  species <- coverage_synthetic_species(
    ids = c("signal", "empty"),
    signal_starts = c(10L, 80L),
    scores = c(5, 5)
  )
  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)
  annotation_rows <- layout$panel_type == "annotation"
  panels <- as.integer(layout$PANEL[annotation_rows])

  expect_identical(as.character(layout$track[annotation_rows]), c("signal", "empty"))
  expect_identical(
    sort(built@layout$ggexon_composite_coverage_panels),
    sort(panels)
  )
  expect_identical(unique(as.character(built@data[[1L]]$track)), "signal")
  expect_true(all(built@data[[2L]]$ymax <= 0))
  expect_true(all(vapply(
    panels,
    function(panel) built@layout$panel_params[[panel]]$y.range[[2L]] >= 5,
    logical(1)
  )))
})

test_that("all-empty requested coverage panels retain order and positive training", {
  species <- coverage_synthetic_species(
    ids = c("empty_a", "empty_b"),
    signal_starts = c(80L, 90L),
    scores = c(5, 8)
  )
  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)
  annotation_rows <- layout$panel_type == "annotation"
  panels <- as.integer(layout$PANEL[annotation_rows])

  expect_identical(
    as.character(layout$track[annotation_rows]),
    c("empty_a", "empty_b")
  )
  expect_equal(nrow(built@data[[1L]]), 0L)
  expect_identical(
    sort(built@layout$ggexon_composite_coverage_panels),
    sort(panels)
  )
  expect_identical(built@layout$ggexon_coverage_max, 0)
  expect_identical(built@layout$ggexon_coverage_training_max, 1)
  expect_true(all(built@data[[2L]]$ymax <= 0))
  expect_true(all(vapply(
    panels,
    function(panel) built@layout$panel_params[[panel]]$y.range[[2L]] >= 1,
    logical(1)
  )))
})

test_that("all-zero requested coverage panels explicitly train to one", {
  species <- coverage_synthetic_species(
    ids = c("zero_a", "zero_b"),
    signal_starts = c(10L, 20L),
    scores = c(0, 0)
  )
  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)
  annotation_rows <- layout$panel_type == "annotation"
  panels <- as.integer(layout$PANEL[annotation_rows])

  expect_true(all(built@data[[1L]]$coverage == 0))
  expect_true(all(built@data[[1L]]$ymax == 0))
  expect_identical(built@layout$ggexon_coverage_max, 0)
  expect_identical(built@layout$ggexon_coverage_training_max, 1)
  expect_true(all(vapply(
    panels,
    function(panel) built@layout$panel_params[[panel]]$y.range[[2L]] >= 1,
    logical(1)
  )))
})

test_that("coverage composition classifies all participating geoms explicitly", {
  roles <- vapply(
    list(
      GeomCoverage,
      GeomExon,
      GeomExon2,
      GeomGene,
      GeomGeneTag,
      GeomGeneLabel,
      GeomMotif,
      GeomGeneBox,
      GeomNucLink,
      ggplot2::GeomBlank
    ),
    function(geom) ggexon_layer_panel_role(list(geom = geom)),
    character(1)
  )

  expect_identical(
    roles,
    c(
      "coverage",
      rep("annotation", 7L),
      "link",
      "other"
    )
  )
})

test_that("coverage guide filtering removes negative breaks and matching labels", {
  view_build <- ggplot2::ggplot_build(
    ggplot2::ggplot(
      data.frame(x = 1:3, y = c(-5, 0, 20)),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20))
  )
  layout <- view_build@layout
  layout$ggexon_composite_coverage_panels <- 1L
  original_range <- layout$panel_params[[1L]]$y.range

  layout <- filter_composite_coverage_y_breaks(layout)

  expect_identical(layout$panel_params[[1L]]$y$get_breaks(), c(0, 5, 10, 15, 20))
  expect_identical(layout$panel_params[[1L]]$y$get_labels(), c("0", "5", "10", "15", "20"))
  expect_identical(layout$panel_params[[1L]]$y.range, original_range)
})

test_that("coverage and gene annotations share four composed panel scales", {
  strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  plot <- ggexon(coverage_fixture_species()) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_y")

  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  annotation_rows <- layout$panel_type == "annotation"
  expect_identical(as.character(layout$track[annotation_rows]), strains)
  expect_true(length(unique(layout$SCALE_Y[annotation_rows])) == 1L)

  windows <- effective_panel_windows(plot)
  expect_identical(windows$track, strains)
  expect_identical(windows$chr, rep("I", length(strains)))
  expect_identical(windows$start, rep(2332338, length(strains)))
  expect_identical(windows$end, rep(2373985, length(strains)))

  coverage_data <- built@data[[1L]]
  expect_identical(coverage_data$ymax, coverage_data$coverage)
  expect_true(all(coverage_data$ymin == 0))

  gene_data <- built@data[[2L]]
  expect_true(all(gene_data$ymax <= 0))
  expect_true(all(gene_data$ymin < 0))

  panel_ranges <- lapply(
    layout$PANEL[annotation_rows],
    function(panel) built@layout$panel_params[[as.integer(panel)]]$y.range
  )
  expect_true(all(vapply(
    panel_ranges,
    function(x) x[[2L]] >= max(coverage_data$coverage),
    logical(1)
  )))

  for (panel in layout$PANEL[annotation_rows]) {
    breaks <- built@layout$panel_params[[as.integer(panel)]]$y$get_breaks()
    expect_true(all(breaks[is.finite(breaks)] >= 0))
  }
})

.coverage_rendered_y <- function(grob) {
  rendered_y <- numeric()
  if (inherits(grob, "ggexonGenetagLabelGrob")) {
    label_data <- grob$data
    for (field in c("genetag_label_y", "genetag_label_anchor_y")) {
      values <- label_data[[field]]
      keep <- is.finite(values)
      if (any(keep)) {
        rendered_y <- c(
          rendered_y,
          grob$coord$transform(
            data.frame(x = label_data$genetag_label_x[keep], y = values[keep]),
            grob$panel_params
          )$y
        )
      }
    }
  } else if (inherits(grob, "rect") && !is.null(grob$y) && !is.null(grob$height)) {
    reference_y <- as.numeric(grob$y)
    height <- as.numeric(grob$height)
    vjust <- grob$vjust
    if (is.null(vjust)) {
      just <- grob$just %||% "centre"
      y_just <- if (length(just) >= 2L) just[[2L]] else just[[1L]]
      vjust <- switch(
        as.character(y_just),
        bottom = 0,
        center = 0.5,
        centre = 0.5,
        top = 1,
        0.5
      )
    }
    rendered_y <- c(
      reference_y - as.numeric(vjust) * height,
      reference_y + (1 - as.numeric(vjust)) * height
    )
  } else {
    for (field in c("y", "y0", "y1")) {
      value <- grob[[field]]
      if (!is.null(value) && inherits(value, "unit")) {
        rendered_y <- c(rendered_y, as.numeric(value))
      }
    }
  }

  children <- list()
  if (!is.null(grob$children)) {
    children <- c(children, as.list(grob$children))
  }
  if (!is.null(grob$grobs)) {
    children <- c(children, as.list(grob$grobs))
  }
  c(
    rendered_y,
    unlist(lapply(children, .coverage_rendered_y), use.names = FALSE)
  )
}

.coverage_find_named_grob <- function(grob, name) {
  if (identical(grob$name, name)) {
    return(grob)
  }
  children <- list()
  if (!is.null(grob$children)) {
    children <- c(children, as.list(grob$children))
  }
  if (!is.null(grob$grobs)) {
    children <- c(children, as.list(grob$grobs))
  }
  for (child in children) {
    found <- .coverage_find_named_grob(child, name)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

.expect_coverage_annotation_grob_in_band <- function(plot, layer = 2L) {
  built <- ggexon_build(plot)
  layer_data <- built@data[[layer]]
  panel <- unique(as.integer(layer_data$PANEL))
  expect_length(panel, 1L)

  panel_grob <- built@plot@layers[[layer]]$draw_geom(
    layer_data,
    built@layout
  )[[panel]]
  rendered_y <- .coverage_rendered_y(panel_grob)
  expect_gt(length(rendered_y), 0L)

  band_y <- built@plot@coordinates$transform(
    data.frame(x = 10, y = c(-0.25, 0)),
    built@layout$panel_params[[panel]]
  )$y
  expect_gte(min(rendered_y), min(band_y) - 1e-9)
  expect_lte(max(rendered_y), max(band_y) + 1e-9)
}

.coverage_render_fixture <- function(annotation_layer) {
  coverage <- data.frame(
    track = "sample",
    xmin = c(0, 10),
    xmax = c(10, 20),
    coverage = c(0, 1)
  )

  ggexon() +
    geom_coverage(
      data = coverage,
      mapping = ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        coverage = coverage,
        track = track
      ),
      inherit.aes = FALSE
    ) +
    annotation_layer +
    facet_genomics(ggplot2::vars(track), scales = "free_y")
}

test_that("composite GeomGeneTag grobs remain inside the negative band", {
  gene <- data.frame(
    track = "sample",
    xmin = 2,
    xmax = 18,
    y = 0.4,
    strand = c("+", "-"),
    gene = c("g_plus", "g_minus"),
    label = c("g_plus", "g_minus")
  )
  plot <- .coverage_render_fixture(
    geom_genetag(
      data = gene,
      label_position = "outside",
      label_direction = "top",
      label_panel_width = 100,
      tag_arrow_fill = "grey50"
    )
  )

  .expect_coverage_annotation_grob_in_band(plot)
})

test_that("nested strip-scaled GeomGeneTag grobs remain inside the negative band", {
  gene <- data.frame(
    track = "sample",
    xmin = c(2, 4),
    xmax = c(18, 10),
    y = 0.4,
    strand = c("+", "-"),
    gene = c("parent", "child"),
    label = c("parent", "child")
  )
  plot <- .coverage_render_fixture(
    geom_genetag(
      data = gene,
      gene_layout = "nested",
      show_label = FALSE,
      tag_arrow_fill = "grey50"
    )
  ) +
    strip_scale_x(gene_gap_ratio = 3, guide = "none")

  .expect_coverage_annotation_grob_in_band(plot)
})

test_that("composite GeomExon2 grobs keep chevrons inside the negative band", {
  exon <- data.frame(
    track = "sample",
    xmin = c(2, 12),
    xmax = c(8, 18),
    ymin = 1,
    transcripts = "tx",
    strand = c("+", "-"),
    type = "exon"
  )
  mapping <- ggplot2::aes(
    ymin = ymin,
    xmin = xmin,
    xmax = xmax,
    transcripts = transcripts,
    strand = strand,
    track = track,
    type = type
  )
  plot <- .coverage_render_fixture(
    geom_exon2(
      data = exon,
      mapping = mapping,
      compress_introns = FALSE,
      inherit.aes = FALSE
    )
  )

  .expect_coverage_annotation_grob_in_band(plot)
})

test_that("composite gene and exon direction grobs stay inside the negative band", {
  exon <- data.frame(
    track = "sample",
    xmin = c(2, 12),
    xmax = c(8, 18),
    ymin = 1,
    transcripts = "tx",
    strand = c("+", "-"),
    type = "exon"
  )
  exon_mapping <- ggplot2::aes(
    ymin = ymin,
    xmin = xmin,
    xmax = xmax,
    transcripts = transcripts,
    strand = strand,
    track = track,
    type = type
  )
  gene <- exon[, setdiff(names(exon), "type"), drop = FALSE]
  gene_mapping <- ggplot2::aes(
    ymin = ymin,
    xmin = xmin,
    xmax = xmax,
    transcripts = transcripts,
    strand = strand,
    track = track
  )

  plots <- list(
    .coverage_render_fixture(
      geom_exon(
        data = exon,
        mapping = exon_mapping,
        inherit.aes = FALSE
      )
    ),
    .coverage_render_fixture(
      geom_gene(
        data = gene,
        mapping = gene_mapping,
        inherit.aes = FALSE
      )
    )
  )

  lapply(plots, .expect_coverage_annotation_grob_in_band)
})

test_that("stacked both-strand GeomGeneBox glyphs stay inside the negative band", {
  genes <- data.frame(
    track = "sample",
    x = rep(10, 4),
    y = rep(1, 4),
    strand = c("+", "-", "+", "-"),
    gene_id = paste0("g", 1:4),
    stringsAsFactors = FALSE
  )
  plot <- .coverage_render_fixture(
    geom_genebox(
      data = genes,
      box_size = 3,
      inherit.aes = FALSE
    )
  )
  built <- ggexon_build(plot)
  layer_data <- built@data[[2L]]
  panel <- unique(as.integer(layer_data$PANEL))
  expect_length(panel, 1L)

  panel_grob <- built@plot@layers[[2L]]$draw_geom(
    layer_data,
    built@layout
  )[[panel]]
  boxes <- .coverage_find_named_grob(panel_grob, "genebox-boxes")
  expect_s3_class(boxes, "rect")

  band_y <- built@plot@coordinates$transform(
    data.frame(x = 10, y = c(-0.25, 0)),
    built@layout$panel_params[[panel]]
  )$y
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    width = grid::unit(50, "mm"),
    height = grid::unit(50, "mm"),
    xscale = c(0, 1),
    yscale = c(0, 1)
  ))
  on.exit(grid::popViewport(), add = TRUE)
  centers <- grid::convertY(boxes$y, "native", valueOnly = TRUE)
  half_height <- grid::convertHeight(boxes$height / 2, "native", valueOnly = TRUE)

  expect_gte(min(centers - half_height), min(band_y) - 1e-9)
  expect_lte(max(centers + half_height), max(band_y) + 1e-9)
})
