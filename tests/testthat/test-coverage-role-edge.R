coverage_role_edge_individual <- function() {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  individual <- SynIndividual(
    annotation_file = file.path(
      fixture_dir,
      "WS285.ugt31-zeel1-peel1-nekl1.gtf"
    ),
    annotation_format = "gtf",
    id = "XZ1516"
  )
  add_annotation(
    individual,
    SynBigWigAnnotation(
      "coverage",
      file.path(fixture_dir, "XZ1516.raw.bw")
    )
  )
}

coverage_role_edge_annotation_data <- function(tracks) {
  data.frame(
    track = tracks,
    xmin = seq_along(tracks) * 100,
    xmax = seq_along(tracks) * 100 + 40,
    y = 1,
    strand = "+",
    gene = paste0("gene", seq_along(tracks)),
    label = paste0("gene", seq_along(tracks)),
    stringsAsFactors = FALSE
  )
}

coverage_role_edge_coverage_data <- function(track = "sample") {
  data.frame(
    track = track,
    xmin = c(100, 120),
    xmax = c(119, 139),
    coverage = c(5, 12),
    stringsAsFactors = FALSE
  )
}

coverage_role_edge_empty_individual <- function() {
  gtf <- tempfile(fileext = ".gtf")
  writeLines(
    c(
      "chr1\ttest\tgene\t10\t40\t.\t+\t.\tgene_id \"gene1\"; gene_name \"gene1\";",
      "chr1\ttest\ttranscript\t10\t40\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\";",
      "chr1\ttest\texon\t10\t40\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\"; exon_number \"1\";"
    ),
    gtf
  )
  bigwig <- tempfile(fileext = ".bw")
  signal <- GenomicRanges::GRanges(
    "chr1",
    IRanges::IRanges(80L, 80L),
    score = 5
  )
  GenomeInfoDb::seqinfo(signal) <- GenomeInfoDb::Seqinfo(
    "chr1",
    seqlengths = 100L
  )
  rtracklayer::export.bw(signal, bigwig)

  individual <- SynIndividual(
    annotation_file = gtf,
    annotation_format = "gtf",
    id = "empty"
  )
  add_annotation(
    individual,
    SynBigWigAnnotation("coverage", bigwig)
  )
}

test_that("SynIndividual coverage and annotation use separate role panels", {
  individual <- coverage_role_edge_individual()
  expect_true(is_lazy(get_annotation(individual, "coverage")))

  built <- ggexon_build(
    ggexon(individual) +
      geom_coverage(annotation = "coverage") +
      geom_exon(
        chr = "I",
        subset = c(2332338L, 2373985L),
        annotation_type = "exon"
      ) +
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)
  coverage_panel <- as.integer(layout$PANEL[layout$panel_type == "coverage"])
  annotation_panel <- as.integer(layout$PANEL[layout$panel_type == "annotation"])

  expect_identical(as.character(layout$track), c("XZ1516", "XZ1516"))
  expect_identical(as.character(layout$panel_type), c("coverage", "annotation"))
  expect_length(unique(layout$SCALE_Y), 2L)
  expect_identical(unique(as.integer(built@data[[1L]]$PANEL)), coverage_panel)
  expect_identical(unique(as.integer(built@data[[2L]]$PANEL)), annotation_panel)
  expect_true(all(built@data[[1L]]$ymin == 0))
  expect_true(all(built@data[[1L]]$ymax == built@data[[1L]]$coverage))
  expect_false(any(c(
    ".ggexon_band_ymin", ".ggexon_band_ymax"
  ) %in% names(built@data[[2L]])))
  expect_true(all(built@data[[2L]]$ymin > 0))
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("empty SynIndividual coverage requests keep their first-class panel", {
  individual <- coverage_role_edge_empty_individual()

  built <- ggexon_build(
    ggexon(individual) +
      geom_coverage(annotation = "coverage") +
      geom_exon(
        chr = "chr1",
        subset = c(1L, 50L),
        annotation_type = "exon"
      ) +
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)
  coverage_row <- layout$panel_type == "coverage"
  annotation_row <- layout$panel_type == "annotation"

  expect_identical(as.character(layout$track), c("empty", "empty"))
  expect_identical(as.character(layout$panel_type), c("coverage", "annotation"))
  expect_length(unique(layout$SCALE_Y), 2L)
  expect_identical(nrow(built@data[[1L]]), 0L)
  expect_true(all(
    as.integer(built@data[[2L]]$PANEL) == as.integer(layout$PANEL[annotation_row])
  ))
  expect_false(any(
    as.integer(built@data[[2L]]$PANEL) == as.integer(layout$PANEL[coverage_row])
  ))
  expect_false(any(c(
    ".ggexon_band_ymin", ".ggexon_band_ymax"
  ) %in% names(built@data[[2L]])))
  expect_identical(
    built@layout$panel_scales_y[[layout$SCALE_Y[coverage_row]]]$range$range,
    c(0, 1)
  )
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("SynIndividual coverage-only data does not require an annotation row", {
  coverage <- coverage_role_edge_coverage_data("only")
  built <- ggexon_build(
    ggexon(SynIndividual(id = "only")) +
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
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(as.character(layout$track), "only")
  expect_identical(as.character(layout$panel_type), "coverage")
  expect_true(all(as.integer(built@data[[1L]]$PANEL) == layout$PANEL))
  expect_true(all(built@data[[1L]]$ymin == 0))
  expect_true(all(built@data[[1L]]$ymax == built@data[[1L]]$coverage))
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("SynIndividual lazy coverage-only data accepts facet limits", {
  built <- ggexon_build(
    ggexon(coverage_role_edge_individual()) +
      geom_coverage(annotation = "coverage") +
      facet_genomics(
        ggplot2::vars(track),
        xlim = c(2332338L, 2373985L),
        xlim_chr = "I"
      )
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(as.character(layout$track), "XZ1516")
  expect_identical(as.character(layout$panel_type), "coverage")
  expect_true(is.na(layout$x_source_panel))
  expect_identical(layout$xlim_chr, "I")
  expect_identical(layout$xlim_min, 2332338)
  expect_identical(layout$xlim_max, 2373985)
  expect_identical(
    built@layout$panel_scales_x[[layout$SCALE_X]]$range$range,
    c(2332338, 2373985)
  )
  expect_gt(nrow(built@data[[1L]]), 0L)
})

test_that("SynSpecies coverage-only data does not require an annotation row", {
  coverage <- coverage_role_edge_coverage_data("only")
  built <- ggexon_build(
    ggexon(SynSpecies(name = "coverage only")) +
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
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(as.character(layout$track), "only")
  expect_identical(as.character(layout$panel_type), "coverage")
  expect_true(is.na(layout$x_source_panel))
  expect_identical(unique(as.integer(built@data[[1L]]$PANEL)), 1L)
  expect_identical(built@data[[1L]]$xmin, c(99.5, 119.5))
  expect_identical(built@data[[1L]]$xmax, c(119.5, 139.5))
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("plain SynSpecies coverage-only data accepts facet limits", {
  coverage <- coverage_role_edge_coverage_data("only")
  built <- ggexon_build(
    ggexon(SynSpecies(name = "plain coverage limits")) +
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
      facet_genomics(
        ggplot2::vars(track),
        xlim = list(only = c(90, 150)),
        xlim_chr = c(only = "chr1")
      )
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(layout$panel_type, "coverage")
  expect_identical(layout$xlim_chr, "chr1")
  expect_identical(layout$xlim_min, 90)
  expect_identical(layout$xlim_max, 150)
  expect_identical(
    built@layout$panel_scales_x[[layout$SCALE_X]]$range$range,
    c(90, 150)
  )
})

test_that("SynSpecies lazy coverage-only data accepts named facet limits", {
  species <- add_individual(
    SynSpecies(name = "lazy coverage only"),
    coverage_role_edge_individual()
  )
  built <- ggexon_build(
    ggexon(species) +
      geom_coverage(annotation = "coverage") +
      facet_genomics(
        ggplot2::vars(track),
        xlim = list(XZ1516 = c(2332338L, 2373985L)),
        xlim_chr = c(XZ1516 = "I")
      )
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(as.character(layout$track), "XZ1516")
  expect_identical(as.character(layout$panel_type), "coverage")
  expect_true(is.na(layout$x_source_panel))
  expect_identical(layout$xlim_chr, "I")
  expect_identical(layout$xlim_min, 2332338)
  expect_identical(layout$xlim_max, 2373985)
  expect_identical(
    built@layout$panel_scales_x[[layout$SCALE_X]]$range$range,
    c(2332338, 2373985)
  )
  expect_gt(nrow(built@data[[1L]]), 0L)
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("coverage prepending preserves a stored multi-column annotation grid", {
  ids <- c("ann_a", "ann_b", "ann_c", "ann_d")
  species <- SynSpecies(name = "stored grid")
  for (id in ids) {
    species <- add_individual(species, SynIndividual(id = id))
  }
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:4,
      ROW = c(1L, 1L, 2L, 2L),
      COL = c(1L, 2L, 1L, 2L),
      track = ids,
      panel_type = "annotation",
      individual = ids,
      species = ids,
      stringsAsFactors = FALSE
    ),
    free = list(x = TRUE, y = FALSE)
  )
  annotation <- coverage_role_edge_annotation_data(ids)
  coverage <- coverage_role_edge_coverage_data("ann_a")

  built <- ggexon_build(
    ggexon(species) +
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
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(layout$panel_type, c("coverage", rep("annotation", 4L)))
  expect_identical(layout$ROW, c(1L, 2L, 2L, 3L, 3L))
  expect_identical(layout$COL, c(1L, 1L, 2L, 1L, 2L))
})

test_that("stored coverage rows retain their multi-column role grid", {
  species <- SynSpecies(name = "stored role grid")
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:4,
      ROW = c(1L, 1L, 2L, 2L),
      COL = c(1L, 2L, 1L, 2L),
      track = c("sample_a", "sample_b", "sample_a", "sample_b"),
      panel_type = c("coverage", "coverage", "annotation", "annotation"),
      species = c("sample_a", "sample_b", "sample_a", "sample_b"),
      stringsAsFactors = FALSE
    ),
    free = list(x = TRUE, y = FALSE)
  )
  coverage <- data.frame(
    track = c("sample_a", "sample_b"),
    xmin = c(100, 200),
    xmax = c(119, 219),
    coverage = c(5, 12),
    stringsAsFactors = FALSE
  )
  annotation <- coverage_role_edge_annotation_data(c("sample_a", "sample_b"))

  built <- ggexon_build(
    ggexon(species) +
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
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(
    as.character(layout$panel_type),
    c("coverage", "coverage", "annotation", "annotation")
  )
  expect_identical(
    as.character(layout$track),
    c("sample_a", "sample_b", "sample_a", "sample_b")
  )
  expect_identical(layout$ROW, c(1L, 1L, 2L, 2L))
  expect_identical(layout$COL, c(1L, 2L, 1L, 2L))
  expect_setequal(
    unique(as.integer(built@data[[1L]]$PANEL)),
    as.integer(layout$PANEL[layout$panel_type == "coverage"])
  )
  expect_setequal(
    unique(as.integer(built@data[[2L]]$PANEL)),
    as.integer(layout$PANEL[layout$panel_type == "annotation"])
  )
})

test_that("coverage prepending remaps stored manual link panel ids", {
  species <- SynSpecies(name = "stored manual link ids")
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = c(10L, 20L, 30L),
      ROW = 1:3,
      COL = 1L,
      track = c("ann_top", "manual_link", "ann_bottom"),
      panel_type = c("annotation", "link", "annotation"),
      tspecies = c(NA_character_, "source_top", NA_character_),
      qspecies = c(NA_character_, "source_bottom", NA_character_),
      t_panel = c(NA_integer_, 10L, NA_integer_),
      q_panel = c(NA_integer_, 30L, NA_integer_),
      stringsAsFactors = FALSE
    ),
    free = list(x = TRUE, y = FALSE)
  )
  coverage <- coverage_role_edge_coverage_data("ann_top")
  annotation <- coverage_role_edge_annotation_data(c("ann_top", "ann_bottom"))
  link <- data.frame(
    track = "manual_link",
    tspecies = "source_top",
    tchr = "chr1",
    tstart = 105,
    tend = 115,
    strand = "+",
    qspecies = "source_bottom",
    qchr = "chr1",
    qstart = 205,
    qend = 215,
    group = 1L,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(species) +
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
  link_row <- layout[layout$panel_type == "link", , drop = FALSE]
  top_panel <- as.integer(
    layout$PANEL[layout$panel_type == "annotation" & layout$track == "ann_top"]
  )
  bottom_panel <- as.integer(
    layout$PANEL[
      layout$panel_type == "annotation" & layout$track == "ann_bottom"
    ]
  )

  expect_identical(link_row$t_panel, top_panel)
  expect_identical(link_row$q_panel, bottom_panel)
  expect_false(link_row$t_panel %in% layout$PANEL[layout$panel_type != "annotation"])
  expect_false(link_row$q_panel %in% layout$PANEL[layout$panel_type != "annotation"])
  expect_setequal(
    unique(as.integer(built@data[[3L]]$source_panel)),
    c(top_panel, bottom_panel)
  )
})

test_that("explicit SynSpecies coverage data creates a first-class role panel", {
  coverage <- coverage_role_edge_coverage_data()
  annotation <- coverage_role_edge_annotation_data("sample")
  species <- SynSpecies(name = "explicit role panels")

  built <- ggexon_build(
    ggexon(species) +
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
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track))
  )
  layout <- as.data.frame(built@layout$layout)
  coverage_panel <- as.integer(layout$PANEL[layout$panel_type == "coverage"])
  annotation_panel <- as.integer(layout$PANEL[layout$panel_type == "annotation"])

  expect_identical(as.character(layout$track), c("sample", "sample"))
  expect_identical(as.character(layout$panel_type), c("coverage", "annotation"))
  expect_length(unique(layout$SCALE_Y), 2L)
  expect_identical(unique(as.integer(built@data[[1L]]$PANEL)), coverage_panel)
  expect_identical(unique(as.integer(built@data[[2L]]$PANEL)), annotation_panel)
  expect_identical(built@data[[1L]]$coverage, c(5, 12))
  expect_true(all(built@data[[1L]]$ymin == 0))
  expect_true(all(built@data[[1L]]$ymax == built@data[[1L]]$coverage))
  expect_false(any(c(
    ".ggexon_band_ymin", ".ggexon_band_ymax"
  ) %in% names(built@data[[2L]])))
  expect_null(built@layout$ggexon_composite_coverage_panels)
})

test_that("colliding public track labels map link data only to the link role", {
  collision <- "collision"
  coverage <- coverage_role_edge_coverage_data(collision)
  annotation <- coverage_role_edge_annotation_data(
    c(collision, "top", "bottom")
  )
  link <- data.frame(
    track = collision,
    tspecies = "top",
    tchr = "chr1",
    tstart = 205,
    tend = 215,
    strand = "+",
    qspecies = "bottom",
    qchr = "chr1",
    qstart = 305,
    qend = 315,
    group = 1L,
    stringsAsFactors = FALSE
  )

  built <- ggexon_build(
    ggexon(SynSpecies(name = "role collision")) +
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
  collision_rows <- layout[as.character(layout$track) == collision, , drop = FALSE]
  link_row <- collision_rows[collision_rows$panel_type == "link", , drop = FALSE]
  annotation_panels <- as.integer(layout$PANEL[layout$panel_type == "annotation"])
  link_data <- built@data[[3L]]

  expect_setequal(
    as.character(collision_rows$panel_type),
    c("coverage", "annotation", "link")
  )
  expect_identical(nrow(link_row), 1L)
  expect_true(all(as.integer(link_data$PANEL) == as.integer(link_row$PANEL)))
  expect_identical(
    as.character(collision_rows$track),
    rep(collision, nrow(collision_rows))
  )
  expect_true(all(c(link_row$t_panel, link_row$q_panel) %in% annotation_panels))
  expect_setequal(
    unique(as.integer(link_data$source_panel)),
    c(link_row$t_panel, link_row$q_panel)
  )
  expect_true(all(
    link_data$y[link_data$y_variable == "target_anchor_y"] == 1
  ))
  expect_true(all(
    link_data$y[link_data$y_variable == "query_anchor_y"] == 0
  ))
})

test_that("ordinary link substring labels retain legacy direction metadata", {
  track_levels <- c("top", "linkage", "bottom")
  annotation <- data.frame(
    track = factor(c("top", "bottom"), levels = track_levels),
    x = c(10, 100),
    y = 1
  )
  link <- data.frame(
    track = factor("linkage", levels = track_levels),
    tspecies = "top",
    tchr = "chr1",
    tstart = 12,
    tend = 20,
    strand = "+",
    qspecies = "bottom",
    qchr = "chr1",
    qstart = 110,
    qend = 120,
    group = 1L
  )

  built <- ggexon_build(
    ggexon() +
      ggplot2::geom_blank(
        data = annotation,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
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
  link_layout <- built@layout$layout[
    as.character(built@layout$layout$track) == "linkage",
    ,
    drop = FALSE
  ]
  link_data <- built@data[[2L]]

  expect_identical(nrow(link_layout), 1L)
  expect_false(is.na(link_layout$t_panel))
  expect_false(is.na(link_layout$q_panel))
  expect_setequal(unique(link_data$y), c(0, 1))
  expect_setequal(
    unique(as.integer(link_data$source_panel)),
    c(link_layout$t_panel, link_layout$q_panel)
  )
})
