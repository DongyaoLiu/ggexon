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

peel1_five_panel_plot <- function() {
  ggexon(coverage_fixture_species()) +
    geom_coverage(annotation = "coverage", fill = "#4C78A8") +
    geom_exon(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    ) +
    facet_genomics(
      ggplot2::vars(track),
      ncol = 1,
      strip.position = "left"
    ) +
    scale_panel_coverage("free_y") +
    center_panel_annotation() +
    theme_ggexon_track() +
    theme_ggexon_side_strips("left")
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

coverage_role_scale_layout <- function(strains = c("XZ1516", "ECA2091")) {
  SynLayout(
    panels = data.frame(
      PANEL = 1:4,
      ROW = c(1L, 1L, 2L, 2L),
      COL = c(1L, 2L, 1L, 2L),
      track = rep(strains, 2L),
      panel_type = rep(c("coverage", "annotation"), each = 2L),
      species = rep(strains, 2L),
      stringsAsFactors = FALSE
    )
  )
}

coverage_role_scale_plot <- function(scales,
                                     axes = "margins",
                                     axis.labels = "all",
                                     panel_limits = FALSE) {
  strains <- c("XZ1516", "ECA2091")
  species <- coverage_fixture_species(strains)
  species_layout(species) <- coverage_role_scale_layout(strains)

  facet_args <- list(
    facets = ggplot2::vars(track),
    ncol = 2L,
    scales = scales,
    axes = axes,
    axis.labels = axis.labels
  )
  if (isTRUE(panel_limits)) {
    facet_args$xlim <- stats::setNames(
      rep(list(c(2332338L, 2373985L)), length(strains)),
      strains
    )
    facet_args$xlim_chr <- stats::setNames(rep("I", length(strains)), strains)
  }

  ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    ) +
    do.call(facet_genomics, facet_args)
}

test_that("coverage wrappers override facet fallback in multi-column Syn layouts", {
  expect_no_warning(
    coverage_free <- ggexon_build(
      coverage_role_scale_plot("fixed", panel_limits = TRUE) +
        scale_panel_coverage("free_y")
    )
  )
  free_layout <- as.data.frame(coverage_free@layout$layout)
  free_params <- coverage_free@layout$facet_params
  free_annotation_ids <- free_layout$SCALE_Y[
    free_layout$panel_type == "annotation"
  ]
  free_coverage_ids <- free_layout$SCALE_Y[
    free_layout$panel_type == "coverage"
  ]

  expect_gt(length(unique(free_layout$COL)), 1L)
  expect_false(any(free_layout$panel_type == "link"))
  expect_length(unique(free_annotation_ids), 1L)
  expect_length(unique(free_coverage_ids), length(free_coverage_ids))
  expect_false(any(free_annotation_ids %in% free_coverage_ids))
  expect_identical(
    unname(unlist(free_params$panel_role_y_policies[c(
      "annotation", "coverage"
    )])),
    c("fixed_y", "free_y")
  )
  expect_false(free_params$free$x)
  expect_true(free_params$free$y)
  expect_true(free_params$draw_axes$y)
  expect_true(free_params$axis_labels$y)

  expect_no_warning(
    coverage_fixed <- ggexon_build(
      coverage_role_scale_plot("free_y") +
        scale_panel_coverage("fixed_y")
    )
  )
  fixed_layout <- as.data.frame(coverage_fixed@layout$layout)
  fixed_params <- coverage_fixed@layout$facet_params
  fixed_annotation_ids <- fixed_layout$SCALE_Y[
    fixed_layout$panel_type == "annotation"
  ]
  fixed_coverage_ids <- fixed_layout$SCALE_Y[
    fixed_layout$panel_type == "coverage"
  ]

  expect_gt(length(unique(fixed_layout$COL)), 1L)
  expect_false(any(fixed_layout$panel_type == "link"))
  expect_length(unique(fixed_annotation_ids), 1L)
  expect_length(unique(fixed_coverage_ids), 1L)
  expect_false(any(fixed_annotation_ids %in% fixed_coverage_ids))
  expect_identical(
    unname(unlist(fixed_params$panel_role_y_policies[c(
      "annotation", "coverage"
    )])),
    c("fixed_y", "fixed_y")
  )
  expect_false(fixed_params$free$x)
  expect_false(fixed_params$free$y)
  expect_false(fixed_params$draw_axes$y)
  expect_true(fixed_params$axis_labels$y)
})

test_that("fixed role overrides preserve explicit y-axis requests and free x", {
  explicit <- ggexon_build(
    coverage_role_scale_plot(
      "free_y",
      axes = "all_y",
      axis.labels = "all_y"
    ) +
      scale_panel_coverage("fixed_y")
  )
  explicit_params <- explicit@layout$facet_params

  expect_false(explicit_params$free$x)
  expect_false(explicit_params$free$y)
  expect_true(explicit_params$draw_axes$y)
  expect_true(explicit_params$axis_labels$y)
  expect_identical(explicit_params$requested_axes, "all_y")
  expect_identical(explicit_params$requested_axis_labels, "all_y")

  free_x <- ggexon_build(
    coverage_role_scale_plot("free") +
      scale_panel_coverage("fixed_y")
  )
  free_x_params <- free_x@layout$facet_params

  expect_true(free_x_params$free$x)
  expect_false(free_x_params$free$y)
  expect_true(free_x_params$draw_axes$x)
  expect_true(free_x_params$axis_labels$x)
  expect_false(free_x_params$draw_axes$y)
  expect_true(free_x_params$axis_labels$y)
})

test_that("omitted coverage species are independent of representative annotation", {
  p <- ggexon(coverage_fixture_species()) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    )

  context <- collect_syn_plot_context(
    p@layers,
    p@data,
    facet = p@facet
  )

  expect_identical(
    context$coverage_tracks,
    c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  )
  expect_identical(context$annotation_species_order, "XZ1516")
  coverage_windows <- context$windows[context$coverage_tracks]
  expect_length(coverage_windows, 4L)
  expect_true(all(vapply(
    coverage_windows,
    function(x) identical(c(as.integer(x$start), as.integer(x$end)),
                          c(2332338L, 2373985L)),
    logical(1)
  )))
  expect_identical(
    unname(vapply(coverage_windows, `[[`, character(1), "track")),
    context$coverage_tracks
  )
  expect_identical(
    unname(vapply(coverage_windows, `[[`, character(1), "individual")),
    context$coverage_tracks
  )
  expect_identical(
    unname(vapply(coverage_windows, `[[`, character(1), "species")),
    context$coverage_tracks
  )
})

test_that("coverage requests remain ordered and dispatch only their own tracks", {
  species <- coverage_fixture_species()
  replicate_tracks <- c("ECA2091", "ECA701")
  for (track in replicate_tracks) {
    individual <- individuals(species)[[track]]
    individual <- add_annotation(
      individual,
      SynBigWigAnnotation(
        "replicate",
        source_file(get_annotation(individual, "coverage"))
      )
    )
    species <- add_individual(species, individual)
  }

  p <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_coverage(annotation = "replicate") +
    geom_exon(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    )
  context <- collect_syn_plot_context(p@layers, p@data, facet = p@facet)

  expect_identical(
    vapply(context$coverage_requests, `[[`, character(1), "request_id"),
    c("coverage:1", "coverage:2")
  )
  expect_identical(
    vapply(context$coverage_requests, `[[`, character(1), "annotation"),
    c("coverage", "replicate")
  )
  expect_identical(
    lapply(context$coverage_requests, `[[`, "tracks"),
    list(
      c("XZ1516", "ECA2091", "ECA701", "ECA2191"),
      replicate_tracks
    )
  )

  coverage <- syn_to_coverage_df(
    species,
    annotation = "coverage",
    context = context
  )
  replicate <- syn_to_coverage_df(
    species,
    annotation = "replicate",
    context = context
  )
  expect_identical(unique(coverage$track), context$coverage_requests[[1L]]$tracks)
  expect_identical(unique(replicate$track), replicate_tracks)
})

test_that("duplicate explicit SynIndividual coverage selectors match normalized requests", {
  species <- coverage_synthetic_species("sample", 10L, 3)
  individual <- individuals(species)[["sample"]]
  p <- ggexon(individual) +
    geom_coverage(annotation = "coverage", species = "ignored") +
    geom_coverage(annotation = "coverage", species = "ignored") +
    geom_exon(
      chr = "chr1",
      subset = c(1L, 50L),
      annotation_type = "exon"
    )
  context <- collect_syn_plot_context(p@layers, p@data, facet = p@facet)

  expect_identical(
    lapply(context$coverage_requests, `[[`, "species"),
    list("sample", "sample")
  )
  data <- syn_to_coverage_df(
    individual,
    species = "ignored",
    annotation = "coverage",
    context = context
  )
  expect_gt(nrow(data), 0L)
  expect_identical(unique(data$track), "sample")

  built <- ggexon_build(p)
  expect_equal(built@data[[1L]], built@data[[2L]])
})

test_that("coverage broadcast compares only normalized genomic coordinates", {
  coverage_windows <- list(
    resolved = list(
      chr = "chr2", start = 5L, end = 9L,
      track = "resolved", individual = "resolved", species = "resolved"
    ),
    unresolved = list(
      track = "unresolved", individual = "recipient", species = "recipient"
    )
  )
  annotation_windows <- list(
    representative = list(
      chr = "chr1", start = 10, end = 40,
      track = "representative", individual = "one", species = "one"
    ),
    same_coordinates = list(
      chr = "chr1", start = 10L, end = 40L,
      track = "different-metadata", individual = "two", species = "two"
    )
  )

  windows <- .complete_common_coverage_windows(
    c("resolved", "unresolved"),
    coverage_windows,
    annotation_windows
  )

  expect_identical(names(windows), c("resolved", "unresolved"))
  expect_identical(
    unname(unlist(windows$resolved[c("chr", "start", "end")])),
    c("chr2", "5", "9")
  )
  expect_identical(
    unname(unlist(windows$unresolved[c("chr", "start", "end")])),
    c("chr1", "10", "40")
  )
  expect_identical(windows$unresolved$track, "unresolved")
  expect_identical(windows$unresolved$individual, "recipient")
  expect_identical(windows$unresolved$species, "recipient")
})

test_that("coverage broadcast rejects every incompatible genomic coordinate", {
  coverage_windows <- list(
    unresolved = list(
      track = "unresolved", individual = "recipient", species = "recipient"
    )
  )
  reference <- list(chr = "chr1", start = 10L, end = 40L)
  incompatible <- list(
    list(chr = "chr2", start = 10L, end = 40L),
    list(chr = "chr1", start = 11L, end = 40L),
    list(chr = "chr1", start = 10L, end = 41L)
  )

  for (candidate in incompatible) {
    expect_error(
      .complete_common_coverage_windows(
        "unresolved",
        coverage_windows,
        list(reference = reference, incompatible = candidate)
      ),
      "multiple.*coverage window|explicit.*coverage"
    )
  }
})

test_that("coverage context rejects ambiguous annotation-window broadcast", {
  species <- coverage_fixture_species(c("XZ1516", "ECA2091", "ECA701"))
  p_ambiguous <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2350000L),
      annotation_type = "exon"
    ) +
    geom_exon(
      species = "ECA2091",
      chr = "I",
      subset = c(2350001L, 2373985L),
      annotation_type = "exon"
    )

  expect_error(
    collect_syn_plot_context(
      p_ambiguous@layers,
      p_ambiguous@data,
      facet = p_ambiguous@facet
    ),
    "multiple.*coverage window|explicit.*coverage"
  )
})

test_that("coverage context does not invent a missing genomic window", {
  species <- coverage_synthetic_species("sample", 10L, 3)
  p <- ggexon(species) + geom_coverage(annotation = "coverage")
  context <- collect_syn_plot_context(p@layers, p@data, facet = p@facet)

  expect_identical(context$coverage_tracks, "sample")
  expect_length(context$windows, 0L)
  expect_length(context$coverage_requests[[1L]]$windows, 0L)
  expect_error(
    ggexon_build(p),
    "explicit annotation window|stored layout window|xlim|xlim_chr"
  )
})

test_that("coverage and annotation with the same track own different panels", {
  built <- ggexon_build(
    ggexon(coverage_fixture_species(c("XZ1516", "ECA2091"))) +
      geom_coverage(annotation = "coverage") +
      geom_exon(
        species = "XZ1516",
        chr = "I",
        subset = c(2332338L, 2373985L)
      ) +
      facet_genomics(ggplot2::vars(track), ncol = 1)
  )

  layout <- built@layout$layout
  expect_identical(layout$panel_type,
                   c("coverage", "coverage", "annotation"))
  expect_identical(layout$track,
                   c("XZ1516", "ECA2091", "XZ1516"))

  coverage_panels <- layout$PANEL[layout$panel_type == "coverage"]
  annotation_panel <- layout$PANEL[layout$panel_type == "annotation"]
  expect_true(all(built@data[[1L]]$PANEL %in% coverage_panels))
  expect_true(all(built@data[[2L]]$PANEL %in% annotation_panel))
})

test_that("all-empty coverage requests create ordered coverage panels from context", {
  species <- coverage_synthetic_species(
    ids = c("empty_a", "empty_b"),
    signal_starts = c(80L, 90L),
    scores = c(5, 8)
  )

  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)

  expect_identical(
    as.character(layout$panel_type),
    c("coverage", "coverage", "annotation", "annotation")
  )
  expect_identical(
    as.character(layout$track),
    c("empty_a", "empty_b", "empty_a", "empty_b")
  )
  expect_equal(nrow(built@data[[1L]]), 0L)
})

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
  expect_identical(context$coverage_tracks, aliases)
  expect_identical(context$coverage_requests[[1L]]$tracks, aliases)
  expect_identical(names(context$coverage_requests[[1L]]$windows), aliases)
  expect_identical(
    unname(vapply(context$windows[aliases], `[[`, character(1), "track")),
    aliases
  )
  expect_true(all(vapply(
    context$windows[aliases],
    function(window) identical(window$individual, "sample") &&
      identical(window$species, "sample"),
    logical(1)
  )))
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
  plot <- coverage_synthetic_plot(species) +
    scale_panel_coverage("free_y") +
    center_panel_annotation()
  original_coverage_layer <- plot@layers[[1L]]
  expect_false("syn_plot_context" %in% ls(original_coverage_layer, all.names = TRUE))

  first <- ggexon_build(plot)
  second <- ggexon_build(plot)
  built_coverage_layer <- first@plot@layers[[1L]]
  built_context <- built_coverage_layer$syn_plot_context

  expect_false(identical(built_coverage_layer, original_coverage_layer))
  expect_null(plot@layers[[1L]]$syn_plot_context)
  expect_false(identical(first@plot@layers[[1L]], second@plot@layers[[1L]]))
  expect_false("syn_plot_context" %in% ls(original_coverage_layer, all.names = TRUE))
  expect_null(original_coverage_layer$syn_plot_context$query_cache)
  expect_false("query_cache" %in% names(built_context))
  expect_false("syn_data" %in% names(built_context))
  expect_true(is.list(built_context$windows))
  expect_identical(names(built_context$windows), "sample")
  expect_equal(first@data[[1L]], second@data[[1L]])

  layout_context <- first@layout$facet_params$syn_plot_context
  expect_identical(layout_context, built_context)
  expect_false("query_cache" %in% names(layout_context))
  expect_false("syn_data" %in% names(layout_context))
  expect_false(any(vapply(layout_context, is.environment, logical(1))))
  expect_identical(
    first@layout$facet_params$panel_scale_specs,
    plot@panel_scale_specs
  )
  expect_identical(
    first@layout$facet_params$center_annotation_panels,
    plot@center_annotation_panels
  )
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

test_that("ordinary data retains the legacy composed coverage panel", {
  coverage <- data.frame(
    track = "sample",
    xmin = c(1, 5),
    xmax = c(4, 8),
    coverage = c(0, 20)
  )
  annotation <- data.frame(
    track = "sample",
    xmin = 1,
    xmax = 8,
    y = 1,
    strand = "+",
    gene = "g",
    label = "g"
  )
  built <- ggexon_build(
    ggexon() +
      geom_coverage(
        data = coverage,
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, coverage = coverage, track = track
        ),
        inherit.aes = FALSE
      ) +
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track), scales = "free_y")
  )

  expect_identical(as.character(built@layout$layout$panel_type), "annotation")
  expect_identical(as.integer(built@data[[1L]]$PANEL), c(1L, 1L))
  expect_identical(as.integer(built@data[[2L]]$PANEL), 1L)
  expect_identical(built@data[[1L]]$coverage, c(0, 20))
  expect_identical(built@data[[1L]]$ymin, c(0, 0))
  expect_identical(built@data[[1L]]$ymax, c(0, 20))
  expect_identical(
    built@data[[2L]][c(
      "y", "ymin", "ymax", ".ggexon_band_ymin", ".ggexon_band_ymax"
    )],
    data.frame(
      y = -2.5,
      ymin = -5,
      ymax = 0,
      .ggexon_band_ymin = -5,
      .ggexon_band_ymax = 0,
      check.names = FALSE
    )
  )
  expect_identical(built@layout$ggexon_composite_coverage_panels, 1L)
  expect_identical(built@layout$panel_scales_y[[1L]]$range$range, c(-5, 20))
  expect_identical(built@layout$panel_params[[1L]]$y.range, c(-6.25, 21.25))
  expect_identical(
    built@layout$panel_params[[1L]]$y$get_breaks(),
    c(0, 5, 10, 15, 20)
  )
})

test_that("ordinary panel_type data cannot opt into the first-class Syn path", {
  coverage <- data.frame(
    track = "sample",
    panel_type = "coverage",
    xmin = c(1, 5),
    xmax = c(4, 8),
    coverage = c(0, 20)
  )
  annotation <- data.frame(
    track = "sample",
    panel_type = "coverage",
    xmin = 1,
    xmax = 8,
    y = 1,
    strand = "+",
    gene = "g",
    label = "g"
  )
  built <- ggexon_build(
    ggexon() +
      geom_coverage(
        data = coverage,
        mapping = ggplot2::aes(
          xmin = xmin, xmax = xmax, coverage = coverage, track = track
        ),
        inherit.aes = FALSE
      ) +
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(
        ggplot2::vars(track, panel_type),
        scales = "free_y"
      )
  )

  expect_identical(built@layout$ggexon_composite_coverage_panels, 1L)
  expect_identical(built@data[[2L]]$y, -2.5)
  expect_identical(built@data[[2L]]$ymin, -5)
  expect_identical(built@data[[2L]]$ymax, 0)
  expect_identical(built@layout$panel_scales_y[[1L]]$range$range, c(-5, 20))
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

test_that("one empty free coverage panel receives only its own fallback", {
  species <- coverage_synthetic_species(
    ids = c("signal", "empty"),
    signal_starts = c(10L, 80L),
    scores = c(5, 5)
  )
  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)
  coverage_rows <- layout$panel_type == "coverage"
  annotation_rows <- layout$panel_type == "annotation"
  panels <- as.integer(layout$PANEL[coverage_rows])

  expect_identical(as.character(layout$track[coverage_rows]), c("signal", "empty"))
  expect_identical(as.character(layout$track[annotation_rows]), c("signal", "empty"))
  expect_null(built@layout$ggexon_composite_coverage_panels)
  expect_identical(unique(as.character(built@data[[1L]]$track)), "signal")
  expect_true(all(built@data[[1L]]$PANEL %in% panels))
  expect_false(any(built@data[[2L]]$PANEL %in% panels))
  expect_true(all(built@data[[2L]]$ymin > 0))
  coverage <- layout[coverage_rows, , drop = FALSE]
  signal_scale <- coverage$SCALE_Y[coverage$track == "signal"]
  empty_scale <- coverage$SCALE_Y[coverage$track == "empty"]
  expect_identical(
    built@layout$panel_scales_y[[signal_scale]]$range$range,
    c(0, 5)
  )
  expect_identical(
    built@layout$panel_scales_y[[empty_scale]]$range$range,
    c(0, 1)
  )
})

test_that("one empty fixed coverage panel inherits its non-empty shared scale", {
  species <- coverage_synthetic_species(
    ids = c("signal", "empty"),
    signal_starts = c(10L, 80L),
    scores = c(5, 5)
  )
  built <- ggexon_build(
    coverage_synthetic_plot(species) + scale_panel_coverage("fixed_y")
  )
  layout <- as.data.frame(built@layout$layout)
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]

  expect_length(unique(coverage$SCALE_Y), 1L)
  expect_identical(
    built@layout$panel_scales_y[[unique(coverage$SCALE_Y)]]$range$range,
    c(0, 5)
  )
  expect_identical(unique(as.character(built@data[[1L]]$track)), "signal")
})

test_that("all-empty coverage panels train to one under fixed and free policies", {
  species <- coverage_synthetic_species(
    ids = c("empty_a", "empty_b"),
    signal_starts = c(80L, 90L),
    scores = c(5, 8)
  )
  for (policy in c("fixed_y", "free_y")) {
    built <- ggexon_build(
      coverage_synthetic_plot(species) + scale_panel_coverage(policy)
    )
    layout <- as.data.frame(built@layout$layout)
    coverage_rows <- layout$panel_type == "coverage"
    annotation_rows <- layout$panel_type == "annotation"
    panels <- as.integer(layout$PANEL[coverage_rows])
    scale_ids <- unique(layout$SCALE_Y[coverage_rows])

    expect_identical(
      as.character(layout$track[coverage_rows]),
      c("empty_a", "empty_b"),
      info = policy
    )
    expect_identical(
      as.character(layout$track[annotation_rows]),
      c("empty_a", "empty_b"),
      info = policy
    )
    expect_equal(nrow(built@data[[1L]]), 0L, info = policy)
    expect_null(built@layout$ggexon_composite_coverage_panels, info = policy)
    expect_false(any(built@data[[2L]]$PANEL %in% panels), info = policy)
    expect_true(all(built@data[[2L]]$ymin > 0), info = policy)
    expect_true(all(vapply(
      scale_ids,
      function(scale_id) identical(
        built@layout$panel_scales_y[[scale_id]]$range$range,
        c(0, 1)
      ),
      logical(1)
    )), info = policy)
    expect_true(all(vapply(
      panels,
      function(panel) {
        view <- built@layout$panel_params[[panel]]$y.range
        length(view) == 2L && all(is.finite(view)) &&
          view[[1L]] <= 0 && view[[2L]] >= 1
      },
      logical(1)
    )), info = policy)
  }
})

test_that("all-zero coverage rows stay zero while their scales train to one", {
  species <- coverage_synthetic_species(
    ids = c("zero_a", "zero_b"),
    signal_starts = c(10L, 20L),
    scores = c(0, 0)
  )
  for (policy in c("fixed_y", "free_y")) {
    built <- ggexon_build(
      coverage_synthetic_plot(species) + scale_panel_coverage(policy)
    )
    layout <- as.data.frame(built@layout$layout)
    coverage_rows <- layout$panel_type == "coverage"
    scale_ids <- unique(layout$SCALE_Y[coverage_rows])
    panels <- as.integer(layout$PANEL[coverage_rows])

    expect_true(all(built@data[[1L]]$coverage == 0), info = policy)
    expect_true(all(built@data[[1L]]$ymin == 0), info = policy)
    expect_true(all(built@data[[1L]]$ymax == 0), info = policy)
    expect_true(all(vapply(
      scale_ids,
      function(scale_id) identical(
        built@layout$panel_scales_y[[scale_id]]$range$range,
        c(0, 1)
      ),
      logical(1)
    )), info = policy)
    expect_true(all(vapply(
      panels,
      function(panel) {
        view <- built@layout$panel_params[[panel]]$y.range
        length(view) == 2L && all(is.finite(view)) &&
          view[[1L]] <= 0 && view[[2L]] >= 1
      },
      logical(1)
    )), info = policy)
  }
})

test_that("one-base first-class coverage keeps raw depth and a finite scale", {
  species <- coverage_synthetic_species("single", 10L, 7)
  built <- ggexon_build(coverage_synthetic_plot(species))
  layout <- as.data.frame(built@layout$layout)
  scale_id <- layout$SCALE_Y[layout$panel_type == "coverage"]

  expect_identical(built@data[[1L]]$interval_start, 10)
  expect_identical(built@data[[1L]]$interval_end, 10)
  expect_identical(built@data[[1L]]$coverage, 7)
  expect_identical(built@data[[1L]]$ymin, 0)
  expect_identical(built@data[[1L]]$ymax, 7)
  expect_identical(
    built@layout$panel_scales_y[[scale_id]]$range$range,
    c(0, 7)
  )
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

test_that("coverage and gene annotations occupy role-qualified panels", {
  strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  species <- coverage_fixture_species()
  annotation_layer <- geom_exon(
    chr = "I",
    subset = c(2332338L, 2373985L),
    annotation_type = "exon"
  )
  facet <- facet_genomics(ggplot2::vars(track), scales = "free_y")
  annotation_only <- ggexon_build(
    ggexon(species) + annotation_layer + facet
  )
  annotation_only_reference <- annotation_only@data[[1L]]

  plot <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    annotation_layer +
    facet

  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  coverage_rows <- layout$panel_type == "coverage"
  annotation_rows <- layout$panel_type == "annotation"
  expect_identical(as.character(layout$track[coverage_rows]), strains)
  expect_identical(as.character(layout$track[annotation_rows]), strains)
  expect_true(length(unique(layout$SCALE_Y[annotation_rows])) == 1L)
  expect_identical(
    length(unique(layout$SCALE_Y[coverage_rows])),
    sum(coverage_rows)
  )
  expect_false(any(
    layout$SCALE_Y[annotation_rows] %in% layout$SCALE_Y[coverage_rows]
  ))

  windows <- effective_panel_windows(plot)
  expect_identical(windows$track, strains)
  expect_identical(windows$chr, rep("I", length(strains)))
  expect_identical(windows$start, rep(2332338, length(strains)))
  expect_identical(windows$end, rep(2373985, length(strains)))

  coverage_data <- built@data[[1L]]
  original_scores <- coverage_data$coverage
  expect_identical(coverage_data$ymax, coverage_data$coverage)
  expect_identical(coverage_data$ymax, original_scores)
  expect_true(all(coverage_data$ymin == 0))
  expect_true(all(coverage_data$PANEL %in% layout$PANEL[coverage_rows]))

  annotation_data <- built@data[[2L]]
  expect_true(all(annotation_data$PANEL %in% layout$PANEL[annotation_rows]))
  expect_false(any(annotation_data$PANEL %in% layout$PANEL[coverage_rows]))
  expect_false(any(c(
    ".ggexon_band_ymin", ".ggexon_band_ymax"
  ) %in% names(annotation_data)))
  expect_equal(
    annotation_data[c("ymin", "ymax")],
    annotation_only_reference[c("ymin", "ymax")]
  )
  expect_null(built@layout$ggexon_composite_coverage_panels)

  for (panel in layout$PANEL[coverage_rows]) {
    panel_coverage <- coverage_data$coverage[
      as.integer(coverage_data$PANEL) == as.integer(panel)
    ]
    scale_id <- layout$SCALE_Y[layout$PANEL == panel]
    expect_equal(
      built@layout$panel_scales_y[[scale_id]]$range$range,
      c(0, max(panel_coverage))
    )
    expect_true(
      built@layout$panel_params[[as.integer(panel)]]$y.range[[2L]] >=
        max(panel_coverage)
    )
    breaks <- built@layout$panel_params[[as.integer(panel)]]$y$get_breaks()
    expect_true(all(breaks[is.finite(breaks)] >= 0))
  }
})

test_that("PEEL-1 renders four free coverage panels above one annotation", {
  plot <- peel1_five_panel_plot()
  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")

  expect_identical(
    as.character(layout$panel_type),
    c(rep("coverage", 4L), "annotation")
  )
  expect_identical(
    as.character(layout$track),
    c(strains, "XZ1516")
  )
  expect_length(unique(layout$SCALE_Y[layout$panel_type == "coverage"]), 4L)
  expect_length(unique(layout$SCALE_Y[layout$panel_type == "annotation"]), 1L)
  expect_identical(plot@facet$params$strip.position, "left")

  coverage_data <- built@data[[1L]]
  coverage_maxima <- tapply(
    coverage_data$coverage,
    as.character(coverage_data$track),
    max
  )
  global_maximum <- unname(coverage_maxima[["ECA2091"]])
  coverage_layout <- layout[layout$panel_type == "coverage", , drop = FALSE]
  for (i in seq_len(nrow(coverage_layout))) {
    track <- as.character(coverage_layout$track[[i]])
    scale_id <- as.integer(coverage_layout$SCALE_Y[[i]])
    trained <- built@layout$panel_scales_y[[scale_id]]$range$range
    expect_equal(trained, c(0, unname(coverage_maxima[[track]])), info = track)
    if (!identical(track, "ECA2091")) {
      expect_false(isTRUE(all.equal(trained[[2L]], global_maximum)), info = track)
    }
  }

  annotation_panel <- as.integer(
    layout$PANEL[layout$panel_type == "annotation"]
  )
  annotation_data <- built@data[[2L]]
  expect_true(all(as.integer(annotation_data$PANEL) == annotation_panel))
  gene_ranges <- data.frame(
    gene = c("ugt-31", "zeel-1", "peel-1", "nekl-1"),
    start = c(2333338L, 2342216L, 2352835L, 2357883L),
    end = c(2338693L, 2350536L, 2356238L, 2372985L)
  )
  expect_true(all(vapply(seq_len(nrow(gene_ranges)), function(i) {
    any(
      annotation_data$xmax >= gene_ranges$start[[i]] &
        annotation_data$xmin <= gene_ranges$end[[i]]
    )
  }, logical(1))), info = paste(gene_ranges$gene, collapse = ", "))
  expect_true(all(annotation_data$xmin >= 2332338L))
  expect_true(all(annotation_data$xmax <= 2373985L))
  centers <- annotation_panel_body_centers(built@data, annotation_panel)
  expect_equal(
    mean(built@layout$panel_params[[annotation_panel]]$y.range),
    unname(centers[[as.character(annotation_panel)]])
  )
})

test_that("annotation centering is view-only and isolated from coverage and links", {
  strains <- c("XZ1516", "ECA2091")
  species <- coverage_fixture_species(strains)
  paf <- tempfile(fileext = ".paf")
  writeLines(
    paste(
      c(
        "I", 15072434, 2332400, 2332500, "+",
        "I", 15072434, 2332400, 2332500,
        100, 100, 60
      ),
      collapse = "\t"
    ),
    paf
  )
  species <- add_pairwise_alignment(
    species,
    SynPairAlignment(
      name = "XZ1516_vs_ECA2091",
      query_individual = "ECA2091",
      target_individual = "XZ1516",
      file = paf,
      format = "paf"
    )
  )

  build_mode <- function(mode = c("default", "vertical", "wrapper", "both")) {
    mode <- match.arg(mode)
    facet_args <- list(
      facets = ggplot2::vars(track),
      scales = "free_y"
    )
    if (mode %in% c("vertical", "both")) {
      facet_args$vertical <- "center"
    }
    plot <- ggexon(species) +
      geom_coverage(annotation = "coverage") +
      geom_genetag(
        chr = "I",
        subset = c(2332338L, 2373985L)
      ) +
      geom_nuclink(alignment = "XZ1516_vs_ECA2091") +
      do.call(facet_genomics, facet_args)
    if (mode %in% c("wrapper", "both")) {
      plot <- plot + center_panel_annotation()
    }
    ggexon_build(plot)
  }

  builds <- lapply(
    c("default", "vertical", "wrapper", "both"),
    build_mode
  )
  names(builds) <- c("default", "vertical", "wrapper", "both")
  reference <- builds$default
  reference_layout <- as.data.frame(reference@layout$layout)
  role_panels <- function(role) {
    as.integer(reference_layout$PANEL[reference_layout$panel_type == role])
  }
  coverage_panels <- role_panels("coverage")
  annotation_panels <- role_panels("annotation")
  link_panels <- role_panels("link")
  annotation_scale_ids <- unique(
    reference_layout$SCALE_Y[reference_layout$panel_type == "annotation"]
  )

  expect_length(annotation_panels, 2L)
  expect_length(annotation_scale_ids, 1L)
  expect_length(link_panels, 1L)

  for (mode in c("vertical", "wrapper", "both")) {
    centered <- builds[[mode]]
    centered_layout <- as.data.frame(centered@layout$layout)
    expect_identical(centered@data, reference@data, info = mode)
    expect_identical(
      centered_layout$SCALE_Y[centered_layout$panel_type == "annotation"],
      reference_layout$SCALE_Y[reference_layout$panel_type == "annotation"],
      info = mode
    )
    expect_identical(
      centered@layout$panel_scales_y[[annotation_scale_ids]]$range$range,
      reference@layout$panel_scales_y[[annotation_scale_ids]]$range$range,
      info = mode
    )

    for (panel in coverage_panels) {
      expect_identical(
        centered@layout$panel_params[[panel]]$y.range,
        reference@layout$panel_params[[panel]]$y.range,
        info = mode
      )
    }
    for (panel in link_panels) {
      expect_identical(
        centered@layout$panel_params[[panel]]$y.range,
        reference@layout$panel_params[[panel]]$y.range,
        info = mode
      )
    }

    centers <- annotation_panel_body_centers(centered@data, annotation_panels)
    for (panel in annotation_panels) {
      expect_equal(
        mean(centered@layout$panel_params[[panel]]$y.range),
        unname(centers[[as.character(panel)]]),
        info = mode
      )
    }
  }

  expect_true(any(vapply(
    annotation_panels,
    function(panel) !identical(
      builds$wrapper@layout$panel_params[[panel]]$y.range,
      reference@layout$panel_params[[panel]]$y.range
    ),
    logical(1)
  )))
  for (panel in annotation_panels) {
    expect_identical(
      builds$vertical@layout$panel_params[[panel]]$y.range,
      builds$wrapper@layout$panel_params[[panel]]$y.range
    )
    expect_identical(
      builds$both@layout$panel_params[[panel]]$y.range,
      builds$wrapper@layout$panel_params[[panel]]$y.range
    )
  }
})

test_that("coverage panels inherit a broadcast annotation x source and reversal", {
  plot <- ggexon(coverage_fixture_species()) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    ) +
    facet_genomics(
      ggplot2::vars(track),
      scales = "free_x",
      reverse_x = "XZ1516",
      reverse_x_match_by = "track"
    )

  built <- ggexon_build(plot)
  layout <- as.data.frame(built@layout$layout)
  coverage_rows <- layout$panel_type == "coverage"
  annotation_rows <- layout$panel_type == "annotation"
  annotation_panel <- as.integer(layout$PANEL[annotation_rows])

  expect_length(annotation_panel, 1L)
  expect_identical(
    as.integer(layout$x_source_panel[coverage_rows]),
    rep(annotation_panel, sum(coverage_rows))
  )
  expect_identical(
    as.integer(layout$SCALE_X[coverage_rows]),
    rep(as.integer(layout$SCALE_X[annotation_rows]), sum(coverage_rows))
  )
  inherited_panels <- c(as.integer(layout$PANEL[coverage_rows]), annotation_panel)
  expect_true(all(vapply(
    inherited_panels,
    function(panel) identical(built@layout$panel_params[[panel]]$reverse, "x"),
    logical(1)
  )))
  expect_setequal(built@layout$ggexon_reverse_x_panels, inherited_panels)
})

test_that("stored annotation windows seed matching coverage x sources", {
  strains <- c("XZ1516", "ECA2091")
  species <- coverage_fixture_species(strains)
  annotation_panels <- data.frame(
    PANEL = 1:2,
    ROW = 1:2,
    COL = 1L,
    track = strains,
    panel_type = "annotation",
    species = strains,
    xlim_chr = "I",
    xlim_min = c(2332338, 2350000),
    xlim_max = c(2350000, 2373985),
    stringsAsFactors = FALSE
  )
  species_layout(species) <- SynLayout(
    panels = annotation_panels,
    free = list(x = TRUE, y = FALSE)
  )

  built <- ggexon_build(
    ggexon(species) +
      geom_coverage(annotation = "coverage") +
      geom_exon(annotation_type = "exon") +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  layout <- as.data.frame(built@layout$layout)
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]
  annotation <- annotation[match(coverage$track, annotation$track), , drop = FALSE]

  expect_identical(as.integer(coverage$x_source_panel), as.integer(annotation$PANEL))
  expect_identical(as.integer(coverage$SCALE_X), as.integer(annotation$SCALE_X))
  expect_identical(coverage$xlim_chr, annotation$xlim_chr)
  expect_identical(coverage$xlim_min, annotation$xlim_min)
  expect_identical(coverage$xlim_max, annotation$xlim_max)
})

test_that("stored coverage rows inherit track-specific windows and reversal", {
  strains <- c("XZ1516", "ECA2091")
  species <- coverage_fixture_species(strains)
  stored_panels <- data.frame(
    PANEL = 1:4,
    ROW = 1:4,
    COL = 1L,
    track = c(strains, strains),
    panel_type = c("coverage", "coverage", "annotation", "annotation"),
    individual = c(strains, strains),
    species = c(strains, strains),
    xlim_chr = c(NA, NA, "I", "I"),
    xlim_min = c(NA, NA, 2332338, 2350000),
    xlim_max = c(NA, NA, 2350000, 2373985),
    stringsAsFactors = FALSE
  )
  species_layout(species) <- SynLayout(
    panels = stored_panels,
    free = list(x = TRUE, y = FALSE)
  )

  built <- ggexon_build(
    ggexon(species) +
      geom_coverage(annotation = "coverage") +
      geom_exon(annotation_type = "exon") +
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        reverse_x = "XZ1516",
        reverse_x_match_by = "track"
      )
  )
  layout <- as.data.frame(built@layout$layout)
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]
  annotation <- annotation[match(coverage$track, annotation$track), , drop = FALSE]

  expect_identical(as.character(layout$panel_type), stored_panels$panel_type)
  expect_identical(as.integer(coverage$x_source_panel), as.integer(annotation$PANEL))
  expect_identical(as.integer(coverage$SCALE_X), as.integer(annotation$SCALE_X))
  expect_identical(coverage$xlim_chr, annotation$xlim_chr)
  expect_identical(coverage$xlim_min, annotation$xlim_min)
  expect_identical(coverage$xlim_max, annotation$xlim_max)

  xz_panels <- c(
    coverage$PANEL[coverage$track == "XZ1516"],
    annotation$PANEL[annotation$track == "XZ1516"]
  )
  eca_panels <- c(
    coverage$PANEL[coverage$track == "ECA2091"],
    annotation$PANEL[annotation$track == "ECA2091"]
  )
  expect_true(all(vapply(
    as.integer(xz_panels),
    function(panel) identical(built@layout$panel_params[[panel]]$reverse, "x"),
    logical(1)
  )))
  expect_true(all(vapply(
    as.integer(eca_panels),
    function(panel) identical(built@layout$panel_params[[panel]]$reverse, "none"),
    logical(1)
  )))
})

test_that("link panels never source coordinates from coverage panels", {
  strains <- c("XZ1516", "ECA2091")
  species <- coverage_fixture_species(strains)
  paf <- tempfile(fileext = ".paf")
  writeLines(
    paste(
      c(
        "I", 15072434, 2332400, 2332500, "+",
        "I", 15072434, 2332400, 2332500,
        100, 100, 60
      ),
      collapse = "\t"
    ),
    paf
  )
  species <- add_pairwise_alignment(
    species,
    SynPairAlignment(
      name = "XZ1516_vs_ECA2091",
      query_individual = "ECA2091",
      target_individual = "XZ1516",
      file = paf,
      format = "paf"
    )
  )

  built <- ggexon_build(
    ggexon(species) +
      geom_coverage(annotation = "coverage") +
      geom_exon(
        species = strains,
        chr = "I",
        subset = c(2332338L, 2373985L),
        annotation_type = "exon"
      ) +
      geom_nuclink(alignment = "XZ1516_vs_ECA2091") +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  layout <- as.data.frame(built@layout$layout)
  link <- layout[layout$panel_type == "link", , drop = FALSE]
  annotation_panels <- as.integer(layout$PANEL[layout$panel_type == "annotation"])
  coverage_panels <- as.integer(layout$PANEL[layout$panel_type == "coverage"])

  expect_length(link$PANEL, 1L)
  expect_true(all(c(link$t_panel, link$q_panel) %in% annotation_panels))
  expect_false(any(c(link$t_panel, link$q_panel) %in% coverage_panels))
})

test_that("continuous coverage rejects compressed and strip genomic x transforms", {
  species <- coverage_fixture_species("XZ1516")
  base <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_genetag(
      species = "XZ1516",
      chr = "I",
      subset = c(2332338L, 2373985L),
      show_label = FALSE
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")

  expect_error(
    ggexon_build(base + scale_x_ggexon_genomic()),
    "coverage.*continuous.*not supported"
  )
  expect_error(
    ggexon_build(base + strip_scale_x(guide = "none")),
    "coverage.*continuous.*not supported"
  )
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
