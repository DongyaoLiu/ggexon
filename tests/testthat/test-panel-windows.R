test_that("coverage x sources resolve by track, recipient, then sole annotation", {
  finalize <- function(coverage_track,
                       coverage_species,
                       annotations) {
    panels <- data.frame(
      PANEL = seq_len(length(annotations) + 1L),
      ROW = seq_len(length(annotations) + 1L),
      COL = 1L,
      track = c(coverage_track, names(annotations)),
      panel_type = c("coverage", rep("annotation", length(annotations))),
      species = c(coverage_species, unname(annotations)),
      xlim_chr = c(NA_character_, rep("chr1", length(annotations))),
      xlim_min = c(NA_real_, seq_along(annotations) * 100),
      xlim_max = c(NA_real_, seq_along(annotations) * 100 + 50),
      stringsAsFactors = FALSE
    )
    syn_layout_panels(.finalize_synspecies_layout_scales(
      panels,
      free = list(x = TRUE, y = FALSE)
    ))
  }

  by_track <- finalize("track_a", "recipient_x", c(track_a = "species_a", track_b = "species_b"))
  expect_identical(
    by_track$x_source_panel[by_track$panel_type == "coverage"],
    by_track$PANEL[by_track$panel_type == "annotation" & by_track$track == "track_a"]
  )

  by_recipient <- finalize("alias_a", "species_a", c(display_a = "species_a", display_b = "species_b"))
  expect_identical(
    by_recipient$x_source_panel[by_recipient$panel_type == "coverage"],
    by_recipient$PANEL[by_recipient$panel_type == "annotation" & by_recipient$species == "species_a"]
  )

  sole <- finalize("alias_a", "unmatched", c(representative = "species_a"))
  expect_identical(
    sole$x_source_panel[sole$panel_type == "coverage"],
    sole$PANEL[sole$panel_type == "annotation"]
  )
  expect_identical(
    sole$SCALE_X[sole$panel_type == "coverage"],
    sole$SCALE_X[sole$panel_type == "annotation"]
  )
  expect_identical(sole$xlim_chr[sole$panel_type == "coverage"], "chr1")
  expect_identical(sole$xlim_min[sole$panel_type == "coverage"], 100)
  expect_identical(sole$xlim_max[sole$panel_type == "coverage"], 150)
})

test_that("equivalent recipient annotation windows resolve deterministically", {
  panels <- data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("sample", "genes_a", "genes_b"),
    panel_type = c("coverage", "annotation", "annotation"),
    individual = c("sample", "sample", "sample"),
    species = c("sample", "sample", "sample"),
    xlim_chr = c(NA_character_, "chr1", "chr1"),
    xlim_min = c(NA_real_, 100, 100),
    xlim_max = c(NA_real_, 200, 200),
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = TRUE, y = FALSE)
  ))
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]

  expect_identical(coverage$x_source_panel, 2L)
  expect_identical(coverage$xlim_chr, "chr1")
  expect_identical(coverage$xlim_min, 100)
  expect_identical(coverage$xlim_max, 200)
  expect_identical(
    coverage$SCALE_X,
    layout$SCALE_X[layout$PANEL == coverage$x_source_panel]
  )
})

test_that("coverage-specific stored windows remain independent", {
  panels <- data.frame(
    PANEL = 1:2,
    ROW = 1:2,
    COL = 1L,
    track = "sample",
    panel_type = c("coverage", "annotation"),
    individual = "sample",
    species = "sample",
    xlim_chr = "chr1",
    xlim_min = c(10, 1),
    xlim_max = c(20, 50),
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = TRUE, y = FALSE)
  ))
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]

  expect_identical(coverage$x_source_panel, annotation$PANEL)
  expect_identical(coverage$xlim_chr, "chr1")
  expect_identical(coverage$xlim_min, 10)
  expect_identical(coverage$xlim_max, 20)
  expect_false(coverage$SCALE_X == annotation$SCALE_X)
})

test_that("numeric-only coverage-specific windows remain independent", {
  panels <- data.frame(
    PANEL = 1:2,
    ROW = 1:2,
    COL = 1L,
    track = "sample",
    panel_type = c("coverage", "annotation"),
    individual = "sample",
    species = "sample",
    xlim_chr = NA_character_,
    xlim_min = c(10, 1),
    xlim_max = c(20, 50),
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = TRUE, y = FALSE)
  ))
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]

  expect_identical(coverage$x_source_panel, annotation$PANEL)
  expect_true(is.na(coverage$xlim_chr))
  expect_identical(coverage$xlim_min, 10)
  expect_identical(coverage$xlim_max, 20)
  expect_false(coverage$SCALE_X == annotation$SCALE_X)
})

test_that("stored coverage windows stay independent from an unstored source", {
  panels <- data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("depth_left", "depth_right", "genes"),
    panel_type = c("coverage", "coverage", "annotation"),
    individual = "sample",
    species = "sample",
    xlim_chr = c("chr1", "chr1", NA_character_),
    xlim_min = c(10, 30, NA_real_),
    xlim_max = c(20, 40, NA_real_),
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = TRUE, y = FALSE)
  ))
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]
  annotation <- layout[layout$panel_type == "annotation", , drop = FALSE]

  expect_identical(
    as.integer(coverage$x_source_panel),
    rep(as.integer(annotation$PANEL), 2L)
  )
  expect_length(unique(coverage$SCALE_X), 2L)
  expect_false(any(coverage$SCALE_X == annotation$SCALE_X))
  expect_identical(coverage$xlim_min, c(10, 30))
  expect_identical(coverage$xlim_max, c(20, 40))
})

test_that("numeric-only equivalent annotation windows resolve deterministically", {
  panels <- data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("sample", "genes_a", "genes_b"),
    panel_type = c("coverage", "annotation", "annotation"),
    individual = "sample",
    species = "sample",
    xlim_chr = NA_character_,
    xlim_min = c(NA, 100, 100),
    xlim_max = c(NA, 200, 200),
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = TRUE, y = FALSE)
  ))
  coverage <- layout[layout$panel_type == "coverage", , drop = FALSE]

  expect_identical(coverage$x_source_panel, 2L)
  expect_true(is.na(coverage$xlim_chr))
  expect_identical(coverage$xlim_min, 100)
  expect_identical(coverage$xlim_max, 200)
})

test_that("standalone coverage rows clear stale annotation sources", {
  panels <- data.frame(
    PANEL = 1L,
    ROW = 1L,
    COL = 1L,
    track = "sample",
    panel_type = "coverage",
    individual = "sample",
    species = "sample",
    x_source_panel = 99L,
    stringsAsFactors = FALSE
  )

  layout <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = FALSE, y = FALSE)
  ))

  expect_true(is.na(layout$x_source_panel))
})

test_that("effective windows keep annotation coordinates for shared aliases", {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  individual <- SynIndividual(
    annotation_file = file.path(
      fixture_dir,
      "WS285.ugt31-zeel1-peel1-nekl1.gtf"
    ),
    annotation_format = "gtf",
    id = "XZ1516"
  )
  individual <- add_annotation(
    individual,
    SynBigWigAnnotation(
      "coverage",
      file.path(fixture_dir, "XZ1516.raw.bw")
    )
  )
  species <- add_individual(SynSpecies(name = "shared alias windows"), individual)
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = "shared alias",
      panel_type = c("coverage", "annotation"),
      individual = "XZ1516",
      species = "XZ1516",
      xlim_chr = "I",
      xlim_min = c(2333000, 2332000),
      xlim_max = c(2334000, 2335000),
      stringsAsFactors = FALSE
    ),
    free = list(x = TRUE, y = FALSE)
  )

  plot <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_genetag(
      data = data.frame(
        track = "shared alias",
        xmin = 2332000,
        xmax = 2335000,
        y = 1,
        strand = "+",
        gene = "shared_gene",
        label = "shared_gene",
        stringsAsFactors = FALSE
      ),
      show_label = FALSE
    ) +
    facet_genomics(ggplot2::vars(track), scales = "free_x")
  windows <- effective_panel_windows(plot)

  expect_identical(nrow(windows), 1L)
  expect_identical(windows$panel_type, "annotation")
  expect_identical(windows$track, "shared alias")
  expect_identical(windows$chr, "I")
  expect_identical(windows$start, 2332000)
  expect_identical(windows$end, 2335000)
})

test_that("different standalone coverage windows receive distinct x scales", {
  layout <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("sample_a", "sample_b"),
      panel_type = "coverage",
      individual = c("sample_a", "sample_b"),
      species = c("sample_a", "sample_b"),
      stringsAsFactors = FALSE
    ),
    free = list(x = FALSE, y = FALSE)
  )
  params <- list(
    panel_xlim = list(sample_a = c(1, 10), sample_b = c(101, 110)),
    panel_xlim_chr = list(sample_a = "chr1", sample_b = "chr1"),
    free = list(x = FALSE, y = FALSE),
    panel_scale_specs = list()
  )

  updated <- .apply_facet_panel_xlim_to_layout(
    layout,
    plot_data = SynSpecies(name = "coverage windows"),
    params = params
  )
  panels <- syn_layout_panels(updated)

  expect_identical(panels$xlim_min, c(1, 101))
  expect_identical(panels$xlim_max, c(10, 110))
  expect_identical(panels$SCALE_X, c(1L, 2L))
  expect_true(updated@free$x)
})

test_that("incompatible recipient annotation windows remain ambiguous", {
  panels <- data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("sample", "genes_a", "genes_b"),
    panel_type = c("coverage", "annotation", "annotation"),
    individual = "sample",
    species = "sample",
    xlim_chr = c(NA_character_, "chr1", "chr1"),
    xlim_min = c(NA, 100, 100),
    xlim_max = c(NA, 200, 201),
    stringsAsFactors = FALSE
  )

  expect_error(
    .finalize_synspecies_layout_scales(
      panels,
      free = list(x = TRUE, y = FALSE)
    ),
    "coverage.*sample.*annotation|annotation.*sample"
  )
})

test_that("coverage layouts reject unresolved or ambiguous annotation x sources", {
  panels <- data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("coverage_x", "annotation_a", "annotation_b"),
    panel_type = c("coverage", "annotation", "annotation"),
    species = c("coverage_x", "species_a", "species_b"),
    stringsAsFactors = FALSE
  )

  expect_error(
    .finalize_synspecies_layout_scales(
      panels,
      free = list(x = TRUE, y = FALSE)
    ),
    "coverage.*coverage_x.*annotation|annotation.*coverage_x"
  )
})

test_that("effective_panel_windows remains annotation-only with coverage rows", {
  layout <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("sample", "sample"),
      panel_type = c("coverage", "annotation"),
      species = c("sample", "sample"),
      xlim_chr = c("chr1", "chr1"),
      xlim_min = c(10, 10),
      xlim_max = c(50, 50),
      stringsAsFactors = FALSE
    )
  )

  windows <- effective_panel_windows(layout)
  expect_identical(nrow(windows), 1L)
  expect_identical(windows$panel_type, "annotation")
  expect_identical(windows$track, "sample")
  row.names(windows) <- NULL
  expect_identical(windows[c("chr", "start", "end")], data.frame(
    chr = "chr1", start = 10, end = 50
  ))
})
