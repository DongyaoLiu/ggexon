coverage_context_edge_species <- function(bigwig_names) {
  stopifnot(is.list(bigwig_names), !is.null(names(bigwig_names)))

  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  gtf <- file.path(fixture_dir, "WS285.ugt31-zeel1-peel1-nekl1.gtf")
  bigwig <- file.path(fixture_dir, "XZ1516.raw.bw")
  species <- SynSpecies(name = "coverage context edges")

  for (individual_id in names(bigwig_names)) {
    individual <- SynIndividual(
      annotation_file = gtf,
      annotation_format = "gtf",
      id = individual_id
    )
    for (annotation_name in bigwig_names[[individual_id]]) {
      individual <- add_annotation(
        individual,
        SynBigWigAnnotation(annotation_name, bigwig)
      )
    }
    species <- add_individual(species, individual)
  }

  species
}

test_that("stored panel aliases do not drop omitted coverage recipients", {
  strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  species <- coverage_context_edge_species(stats::setNames(
    rep(list("coverage"), length(strains)),
    strains
  ))
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("reference genes", "ECA2091 depth"),
      panel_type = c("annotation", "coverage"),
      individual = c(NA_character_, "ECA2091"),
      species = c("XZ1516", NA_character_),
      xlim_chr = "I",
      xlim_min = 2332338,
      xlim_max = 2373985,
      stringsAsFactors = FALSE
    )
  )

  plot <- ggexon(species) + geom_coverage(annotation = "coverage")
  context <- collect_syn_plot_context(plot@layers, plot@data, facet = plot@facet)
  expected_tracks <- c(
    "reference genes", "ECA2091 depth", "ECA701", "ECA2191"
  )

  expect_identical(context$coverage_tracks, expected_tracks)
  expect_identical(
    unname(vapply(
      context$coverage_requests[[1L]]$windows,
      `[[`,
      character(1),
      "individual"
    )),
    strains
  )
})

test_that("coverage recipients win duplicate aliases without reordering tracks", {
  strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  species <- coverage_context_edge_species(stats::setNames(
    rep(list("coverage"), length(strains)),
    strains
  ))
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:4,
      ROW = 1:4,
      COL = 1L,
      track = c(
        "shared alias", "annotation alias", "shared alias", "coverage alias"
      ),
      panel_type = c("annotation", "annotation", "coverage", "coverage"),
      individual = c(NA_character_, NA_character_, "ECA2091", "ECA2191"),
      species = c("XZ1516", "ECA701", NA_character_, NA_character_),
      xlim_chr = "I",
      xlim_min = 2332338,
      xlim_max = 2373985,
      stringsAsFactors = FALSE
    )
  )

  plot <- ggexon(species) + geom_coverage(annotation = "coverage")
  context <- collect_syn_plot_context(plot@layers, plot@data, facet = plot@facet)
  request <- context$coverage_requests[[1L]]

  expect_identical(
    request$tracks,
    c("shared alias", "annotation alias", "coverage alias", "XZ1516")
  )
  expect_identical(
    unname(vapply(request$windows, `[[`, character(1), "individual")),
    c("ECA2091", "ECA701", "ECA2191", "XZ1516")
  )
})

test_that("layout context keeps annotation and coverage aliases with row recipients", {
  species <- coverage_context_edge_species(list(XZ1516 = "coverage"))
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("depth alias", "gene alias"),
      panel_type = c("coverage", "annotation"),
      individual = c("XZ1516", NA_character_),
      species = c(NA_character_, "XZ1516"),
      xlim_chr = "I",
      xlim_min = c(2333000, 2332000),
      xlim_max = c(2334000, 2335000),
      stringsAsFactors = FALSE
    )
  )

  windows <- collect_layout_panel_windows(species)

  expect_setequal(names(windows), c("depth alias", "gene alias"))
  expect_identical(windows[["depth alias"]]$individual, "XZ1516")
  expect_identical(windows[["depth alias"]]$track, "depth alias")
  expect_identical(
    unname(unlist(windows[["depth alias"]][c("start", "end")])),
    c(2333000, 2334000)
  )
  expect_identical(windows[["gene alias"]]$individual, "XZ1516")
  expect_identical(windows[["gene alias"]]$track, "gene alias")
})

test_that("stored coverage windows override same-track annotation context only", {
  species <- coverage_context_edge_species(list(XZ1516 = "coverage"))
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = "shared alias",
      panel_type = c("coverage", "annotation"),
      individual = c("XZ1516", NA_character_),
      species = c(NA_character_, "XZ1516"),
      xlim_chr = "I",
      xlim_min = c(2333000, 2332000),
      xlim_max = c(2334000, 2335000),
      stringsAsFactors = FALSE
    )
  )

  windows <- collect_layout_panel_windows(species)
  public_windows <- effective_panel_windows(species)

  expect_identical(
    unname(unlist(windows[["shared alias"]][c("start", "end")])),
    c(2333000, 2334000)
  )
  expect_identical(nrow(public_windows), 1L)
  expect_identical(public_windows$panel_type, "annotation")
  expect_identical(public_windows$track, "shared alias")
  expect_identical(
    unname(unlist(public_windows[c("start", "end")])),
    c(2332000, 2335000)
  )
})

test_that("stored coverage windows stay out of annotation broadcast candidates", {
  species <- coverage_context_edge_species(list(
    sample_a = "coverage",
    sample_b = "coverage"
  ))
  species_layout(species) <- SynLayout(
    panels = data.frame(
      PANEL = 1:2,
      ROW = 1:2,
      COL = 1L,
      track = c("sample_a depth", "sample_a genes"),
      panel_type = c("coverage", "annotation"),
      individual = c("sample_a", NA_character_),
      species = c(NA_character_, "sample_a"),
      xlim_chr = "I",
      xlim_min = c(2333000, 2332338),
      xlim_max = c(2334000, 2373985),
      stringsAsFactors = FALSE
    )
  )
  plot <- ggexon(species) +
    geom_coverage(annotation = "coverage") +
    geom_exon(
      species = "sample_a",
      chr = "I",
      subset = c(2332338L, 2373985L),
      annotation_type = "exon"
    )

  context <- collect_syn_plot_context(
    plot@layers,
    plot@data,
    facet = plot@facet
  )
  windows <- context$coverage_requests[[1L]]$windows

  expect_identical(names(windows), c("sample_a depth", "sample_b"))
  expect_identical(windows[["sample_a depth"]]$start, 2333000)
  expect_identical(windows[["sample_a depth"]]$end, 2334000)
  expect_identical(windows[["sample_b"]]$start, 2332338)
  expect_identical(windows[["sample_b"]]$end, 2373985)
})

test_that("omitted annotation selection skips zero but reports ambiguous BigWigs", {
  eligible <- coverage_context_edge_species(list(
    zero = character(),
    one = "only"
  ))
  eligible_plot <- ggexon(eligible) + geom_coverage()
  eligible_context <- collect_syn_plot_context(
    eligible_plot@layers,
    eligible_plot@data,
    facet = eligible_plot@facet
  )
  expect_identical(eligible_context$coverage_tracks, "one")

  ambiguous <- coverage_context_edge_species(list(
    zero = character(),
    one = "only",
    ambiguous = c("first", "second")
  ))
  ambiguous_plot <- ggexon(ambiguous) + geom_coverage()

  expect_error(
    collect_syn_plot_context(
      ambiguous_plot@layers,
      ambiguous_plot@data,
      facet = ambiguous_plot@facet
    ),
    "ambiguous.*multiple.*supply.*annotation|multiple.*ambiguous.*supply.*annotation"
  )
})

test_that("broadcast coverage windows normalize chromosome names per recipient", {
  gtf <- tempfile(fileext = ".gtf")
  writeLines(
    c(
      "1\ttest\tgene\t1\t100\t.\t+\t.\tgene_id \"gene1\"; gene_name \"gene1\";",
      "1\ttest\ttranscript\t1\t100\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\";",
      "1\ttest\texon\t1\t100\t.\t+\t.\tgene_id \"gene1\"; transcript_id \"tx1\";"
    ),
    gtf
  )
  species <- add_individual(
    SynSpecies(name = "seqname aliases"),
    SynIndividual(
      annotation_file = gtf,
      annotation_format = "gtf",
      id = "recipient"
    )
  )
  context <- list(windows = list(recipient = list(
    chr = "chr1",
    start = 10,
    end = 20,
    individual = "recipient",
    species = "recipient",
    track = "recipient"
  )))

  window <- normalize_syn_window_request(
    species,
    species = "recipient",
    context = context,
    geom = "geom_coverage"
  )

  expect_identical(window$chr, "1")
  expect_identical(window$start, 10)
  expect_identical(window$end, 20)
})
