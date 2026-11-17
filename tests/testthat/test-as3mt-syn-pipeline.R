as3mt_syn_fixture <- function() {
  dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(dir, "load-as3mt-example.R"), envir = env)
  sys.source(file.path(dir, "as3mt-object-data.R"), envir = env)
  list(model = env$load_as3mt_example(dir), helpers = env, dir = dir)
}

test_that("AS3MT loads one comparative object with interval-native human coverage", {
  fixture <- as3mt_syn_fixture()
  model <- fixture$model
  expect_s4_class(model, "SynSpecies")
  expect_setequal(individual_names(model), c("human", "chimp"))
  human <- individuals(model)[["human"]]
  expect_s4_class(get_annotation(human, "gtex_ba9"), "SynBigWigAnnotation")
  source <- utils::read.delim(file.path(fixture$dir, "human-rnaseq-GTEX-15G19.tsv"))
  signal <- as.data.frame(query_signal(get_annotation(human, "gtex_ba9"),
    "chr10", 102869400, 102902000))
  expect_equal(signal[, c("start", "end", "score")], source[, c("start", "end", "score")])
  expect_equal(nrow(pairwise_alignment_data(model, "human_chimp")), 37L)
})

test_that("prepared AS3MT views retain transcript and DNA evidence without RNA panels", {
  fixture <- as3mt_syn_fixture()
  model <- fixture$helpers$prepare_as3mt_example(fixture$model)
  labels <- fixture$helpers$as3mt_plot_data(model, "exon_labels")
  expect_setequal(labels$exon[labels$id == "human_short"], c(1L, 4:11))
  panels <- species_layout(model)@panels
  expect_equal(as.character(panels$track), c("human", "link_human_chimp", "chimp"))
  expect_equal(sum(panels$panel_type == "coverage"), 0L)
  dna <- fixture$helpers$as3mt_plot_data(model, "dna_markers")
  expect_equal(dna$position[dna$boundary_after], c(102869593, 104324234))
})

test_that("AS3MT tutorial combines transcript models and DNA without RNA panels", {
  skip_if_not_installed("knitr")
  tutorial <- test_path("..", "..", "vignettes", "as3mt-splice-variants-demo.Rmd")
  skip_if_not(file.exists(tutorial), "Tutorial source is not installed")
  code <- tempfile(fileext = ".R")
  on.exit(unlink(code), add = TRUE)
  knitr::purl(tutorial, output = code, documentation = 0, quiet = TRUE)
  env <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(code, envir = env)))

  for (plot in list(env$transcript_plot, env$combined_plot)) {
    built <- ggexon_build(plot)
    panels <- built@layout$layout
    expect_equal(as.character(panels$track), c("human", "link_human_chimp", "chimp"))
    expect_equal(as.character(panels$panel_type), c("annotation", "link", "annotation"))
    expect_false(any(vapply(built@plot@layers, function(layer)
      inherits(layer$geom, "GeomCoverage") || inherits(layer$geom, "GeomJunction"), logical(1))))
    expect_silent(ggplot2::ggplot_gtable(built))
  }
  built <- ggexon_build(env$combined_plot)
  expect_true(any(vapply(built@plot@layers, function(layer)
    inherits(layer$geom, "GeomAlignseq"), logical(1))))
  table <- ggplot2::ggplot_gtable(built)
  panels <- built@layout$layout
  panel_height <- function(row) {
    cell <- table$layout[table$layout$name == paste0("panel-", row$COL, "-", row$ROW), ]
    as.numeric(table$heights[cell$t])
  }
  human <- panels[panels$track == "human", ]
  chimp <- panels[panels$track == "chimp", ]
  expect_equal(panel_height(chimp) / panel_height(human), 0.5)
})
