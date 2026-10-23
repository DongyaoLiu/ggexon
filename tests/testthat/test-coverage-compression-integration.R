.coverage_compression_integration_species <- function(ids) {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  species <- SynSpecies(name = "PEEL-1 compression integration")
  for (id in ids) {
    individual <- SynIndividual(
      id = id,
      annotation_file = file.path(
        fixture_dir, "WS285.ugt31-zeel1-peel1-nekl1.gtf"
      ),
      annotation_format = "gtf"
    )
    individual <- add_annotation(
      individual,
      SynBigWigAnnotation(
        "coverage", file.path(fixture_dir, paste0(id, ".raw.bw"))
      )
    )
    species <- add_individual(species, individual)
  }
  species
}

test_that("real BigWig, exon and nucleotide-link panels share compressed source maps", {
  skip_on_os("windows") # rtracklayer BigWig I/O is unsupported on Windows.
  ids <- c("XZ1516", "ECA2091")
  window <- c(2332338L, 2373985L)
  species <- .coverage_compression_integration_species(ids)
  paf <- withr::local_tempfile(fileext = ".paf")
  # One ungapped identity block deliberately crosses every exon/intron boundary.
  writeLines(paste(c(
    "I", 15072434L, window[[1L]] - 1L, window[[2L]], "+",
    "I", 15072434L, window[[1L]] - 1L, window[[2L]],
    diff(window) + 1L, diff(window) + 1L, 60L
  ), collapse = "\t"), paf)
  species <- add_pairwise_alignment(species, SynPairAlignment(
    name = "compression_identity",
    query_individual = ids[[2L]], target_individual = ids[[1L]],
    file = paf, format = "paf"
  ))
  # Inclusive coverage intervals draw half-base edges outside their centres.
  # A one-base view margin contains them and keeps BigWig query limits integral.
  plot <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_exon(species = ids, chr = "I", subset = window + c(-1L, 1L),
      annotation_type = "exon") +
    geom_nuclink(alignment = "compression_identity") +
    facet_genomics(ggplot2::vars(track), scales = "free_x",
      xlim = stats::setNames(rep(list(window + c(-1L, 1L)), length(ids)), ids),
      xlim_chr = stats::setNames(rep("I", length(ids)), ids),
      reverse_x = ids[[1L]], reverse_x_match_by = "track") +
    scale_panel_coverage("free_y", auto = TRUE)
  raw <- ggexon_build(plot)
  compressed <- ggexon_build(plot + scale_x_ggexon_genomic(
    intron_factor = 30,
    guide = guide_x_ggexon_piecewise(by = "panel")
  ))
  layout <- as.data.frame(compressed@layout$layout)
  coverage_rows <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation_rows <- layout[layout$panel_type == "annotation", , drop = FALSE]
  expect_equal(nrow(coverage_rows), 2L)
  expect_equal(nrow(annotation_rows), 2L)
  for (id in ids) {
    coverage_row <- coverage_rows[coverage_rows$track == id, , drop = FALSE]
    annotation_row <- annotation_rows[annotation_rows$track == id, , drop = FALSE]
    panel <- as.integer(coverage_row$PANEL)
    source <- as.integer(annotation_row$PANEL)
    expect_identical(as.integer(coverage_row$x_source_panel), source)
    expect_identical(as.integer(coverage_row$SCALE_X),
      as.integer(annotation_row$SCALE_X))
    transform <- compressed@layout$genomic_x_transforms[[as.character(panel)]]
    source_transform <- compressed@layout$genomic_x_transforms[[as.character(source)]]
    expect_true(any(transform$slope == 1 / 30))
    expect_equal(transform[, setdiff(names(transform), "PANEL")],
      source_transform[, setdiff(names(source_transform), "PANEL")])
    expect_identical(compressed@layout$panel_params[[panel]]$reverse,
      if (id == ids[[1L]]) "x" else "none")
    expect_identical(compressed@layout$panel_params[[source]]$reverse,
      if (id == ids[[1L]]) "x" else "none")

    signal <- compressed@data[[1L]][compressed@data[[1L]]$track == id, ]
    original <- raw@data[[1L]][raw@data[[1L]]$track == id, ]
    interval_key <- function(data) paste(data$interval_start, data$interval_end)
    parent <- match(interval_key(signal), interval_key(original))
    expect_false(anyNA(parent))
    expect_equal(signal$coverage, original$coverage[parent])
    expect_equal(signal$genomic_xmin, original$genomic_xmin[parent])
    expect_equal(signal$genomic_xmax, original$genomic_xmax[parent])
    weights <- rowsum(.coverage_span_weights(signal), parent, reorder = FALSE)
    expect_equal(as.numeric(weights[match(seq_len(nrow(original)),
      as.integer(rownames(weights))), 1L]), .coverage_span_weights(original))
    expect_equal(sum(.coverage_span_weights(signal)), diff(window) + 1L)
    expect_equal(compressed@layout$panel_params[[panel]]$y.range,
      raw@layout$panel_params[[panel]]$y.range)
    expect_equal(compressed@layout$panel_params[[panel]]$x.range,
      compressed@layout$panel_params[[source]]$x.range)
  }

  links <- compressed@data[[3L]]
  annotation_panels <- as.integer(annotation_rows$PANEL)
  expect_gt(nrow(links), 4L)
  expect_true(all(as.integer(links$source_panel) %in% annotation_panels))
  for (source in unique(as.integer(links$source_panel))) {
    hit <- as.integer(links$source_panel) == source
    expect_equal(links$x[hit], ggexon_genomic_to_plot_x(links$genomic_x[hit],
      compressed@layout$genomic_x_transforms[[as.character(source)]]))
  }
  axis_data <- compressed@layout$genomic_x_axis_data
  expect_setequal(as.integer(axis_data$PANEL), annotation_panels)
  expect_setequal(axis_data$region_type, c("exon", "intron"))
  expect_silent(ggplot2::ggplot_gtable(compressed))
})

test_that("standalone real BigWig coverage stays genomic without an exon source", {
  skip_on_os("windows") # rtracklayer BigWig I/O is unsupported on Windows.
  species <- .coverage_compression_integration_species("XZ1516")
  window <- c(2332338L, 2373985L)
  # A one-base view margin contains the coverage rectangles' half-base edges.
  plot <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    facet_genomics(ggplot2::vars(track), scales = "free_x",
      xlim = window + c(-1L, 1L), xlim_chr = "I")
  raw <- ggexon_build(plot)
  compressed <- ggexon_build(plot + scale_x_ggexon_genomic(intron_factor = 30))
  expect_true(all(compressed@layout$layout$panel_type == "coverage"))
  expect_true(all(is.na(compressed@layout$layout$x_source_panel)))
  expect_null(compressed@layout$genomic_x_transforms)
  expect_equal(compressed@data[[1L]], raw@data[[1L]])
  expect_equal(compressed@layout$panel_params[[1L]]$x.range,
    raw@layout$panel_params[[1L]]$x.range)
  expect_equal(sum(.coverage_span_weights(compressed@data[[1L]])), 41648)
  expect_silent(ggplot2::ggplot_gtable(compressed))
})
