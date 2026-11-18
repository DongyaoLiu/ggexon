as3mt_syn_fixture <- function() {
  dir <- system.file("extdata", "as3mt", package = "ggexon")
  env <- new.env(parent = globalenv())
  sys.source(file.path(dir, "load-as3mt-example.R"), envir = env)
  sys.source(file.path(dir, "as3mt-object-data.R"), envir = env)
  list(model = env$load_as3mt_example(dir), helpers = env, dir = dir)
}

test_that("AS3MT loads one comparative object with human coverage annotation", {
  fixture <- as3mt_syn_fixture()
  model <- fixture$model
  expect_s4_class(model, "SynSpecies")
  expect_setequal(individual_names(model), c("human", "chimp"))
  human <- individuals(model)[["human"]]
  expect_s4_class(get_annotation(human, "gtex_ba9"), "SynBigWigAnnotation")
  expect_equal(nrow(pairwise_alignment_data(model, "human_chimp")), 37L)
})

test_that("AS3MT human BigWig coverage retains the source intervals", {
  skip_on_os("windows") # rtracklayer BigWig I/O is unsupported on Windows.
  fixture <- as3mt_syn_fixture()
  human <- individuals(fixture$model)[["human"]]
  source <- utils::read.delim(file.path(fixture$dir, "human-rnaseq-GTEX-15G19.tsv"))
  signal <- as.data.frame(query_signal(get_annotation(human, "gtex_ba9"),
    "chr10", 102869400, 102902000))
  expect_equal(signal[, c("start", "end", "score")], source[, c("start", "end", "score")])
})

test_that("AS3MT exon labels follow the loaded transcript rows", {
  fixture <- as3mt_syn_fixture()
  model <- fixture$helpers$prepare_as3mt_example(fixture$model)
  labels <- fixture$helpers$as3mt_plot_data(model, "exon_labels")
  expect_setequal(labels$exon[labels$id == "human_short"], c(1L, 4:11))
  human <- individuals(model)[["human"]]
  features <- as.data.frame(query_features(human, feature_type = NULL, all = TRUE))
  expect_equal(unique(features$isoform_y[features$transcript_id == "human_short"]), 1.8)
  expect_equal(unique(features$isoform_y[features$transcript_id == "human_full"]), .7)
  expect_equal(unique(labels$y[labels$id == "human_short"]), 2.45)
  expect_equal(labels$y[labels$id == "human_full" & labels$exon == 2L], 1.6)
  expect_equal(unique(labels$y[labels$id == "human_full" & labels$exon != 2L]), 1.35)
  expect_equal(unique(labels$y[labels$id == "chimp" & labels$exon != 2L]), 1.35)
  expect_equal(labels$y[labels$id == "chimp" & labels$exon == 2L], 1.6)

  # Labels follow the transcript even when the stored model is moved again.
  moved <- annotation_data(human)
  md <- S4Vectors::mcols(moved)
  md$isoform_y[md$transcript_id == "human_short"] <- 2.1
  S4Vectors::mcols(moved) <- md
  annotation_data(human) <- moved
  model <- fixture$helpers$prepare_as3mt_example(add_individual(model, human))
  labels <- fixture$helpers$as3mt_plot_data(model, "exon_labels")
  expect_equal(unique(labels$y[labels$id == "human_short"]), 2.75)
})

test_that("the AS3MT exon-label helper leaves DNA preparation to the package", {
  fixture <- as3mt_syn_fixture()
  model <- fixture$helpers$prepare_as3mt_example(fixture$model)
  panels <- species_layout(model)@panels
  expect_equal(as.character(panels$track), c("human", "link_human_chimp", "chimp"))
  expect_equal(sum(panels$panel_type == "coverage"), 0L)
  expect_named(fixture$helpers$as3mt_evidence(model)$plot_data, "exon_labels")
})

test_that("AS3MT legacy transcript overlays track the reordered models", {
  fixture <- as3mt_syn_fixture()
  result <- fixture$helpers$plot_as3mt_example(fixture$model)
  labels <- result$plot$layers[[2L]]$data
  ghosts <- result$plot$layers[[3L]]$data
  junction <- result$plot$layers[[4L]]$data
  expect_equal(unique(labels$y[labels$id == "human_short"]), 2.45)
  expect_equal(labels$y[labels$id == "human_full" & labels$exon == 3L], 1.35)
  expect_equal(mean(range(result$build@data[[3L]]$ymin, result$build@data[[3L]]$ymax)), 1.9)
  expect_setequal(ghosts$exon, c(2L, 3L))
  expect_equal(junction$y, 1.45)
  with_junctions <- fixture$helpers$plot_as3mt_example(fixture$model,
    junction_evidence = TRUE)
  labels <- with_junctions$plot$layers[[2L]]$data
  expect_equal(labels$y[labels$id == "human_full" & labels$exon == 2L], .5)
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

  dna <- query_alignseq(env$as3mt, "as3mt_vntr")
  detail <- query_alignseq(env$as3mt, "as3mt_vntr_detail")
  for (rows in list(dna, detail)) {
    expect_setequal(rows$individual, c("human", "chimp"))
    expect_equal(rows$position[rows$boundary_after], c(102869593, 104324234))
    expect_equal(rows$boundary_label[rows$boundary_after], rep("E1 donor | intron 1", 2L))
    substitutions <- rows$event_id[rows$event_type == "substitution"]
    expect_length(substitutions, 4L)
    expect_false(anyNA(substitutions))
  }
  chimp_gaps <- dna[dna$individual == "chimp" & dna$base == "-", ]
  expect_equal(nrow(chimp_gaps), 72L)
  expect_true(all(chimp_gaps$event_length == 72L & is.na(chimp_gaps$position)))
  expect_equal(as.integer(table(detail$individual)), c(36L, 36L))

  plots <- list(transcript = ggexon_build(env$transcript_plot),
    close_up = ggexon_build(env$sequence_plot), combined = ggexon_build(env$combined_plot))
  for (built in plots[c("transcript", "combined")]) {
    panels <- built@layout$layout
    expect_equal(as.character(panels$track), c("human", "link_human_chimp", "chimp"))
    expect_equal(as.character(panels$panel_type), c("annotation", "link", "annotation"))
    expect_false(any(vapply(built@plot@layers, function(layer)
      inherits(layer$geom, "GeomCoverage") || inherits(layer$geom, "GeomJunction"), logical(1))))
    expect_silent(ggplot2::ggplot_gtable(built))
  }
  for (name in c("close_up", "combined")) {
    built <- plots[[name]]
    find_layer <- function(geom) which(vapply(built@plot@layers, function(layer)
      inherits(layer$geom, geom), logical(1)))
    dna_index <- find_layer("GeomAlignseq")
    expect_length(dna_index, 1L)
    layer <- built@plot@layers[[dna_index]]
    expect_s3_class(layer, "LayerSyn")
    dna <- built@data[[dna_index]]
    expect_setequal(as.character(dna$track), c("human", "chimp"))
    exons <- built@data[[find_layer("GeomExon2")]]
    grobs <- layer$draw_geom(dna, built@layout)

    for (id in c("human", "chimp")) {
      rows <- dna[dna$track == id, ]
      panel <- as.integer(unique(rows$PANEL))
      expect_equal(as.character(built@layout$layout$panel_type[panel]), "annotation")
      chunk <- rows[rows$column %in% detail$column[detail$individual == id], ]
      expect_equal(nrow(chunk), 36L)
      anchor_x <- mean(range(chunk$x))
      target <- if (id == "human") "human_short" else "chimp"
      feature <- exons[which(exons$PANEL == rows$PANEL[1L] & exons$transcripts == target &
        exons$xmin <= anchor_x & exons$xmax >= anchor_x), ]
      expect_gt(nrow(feature), 0L)
      start <- built@plot@coordinates$transform(
        data.frame(x = anchor_x, y = max(feature$ymax)),
        built@layout$panel_params[[panel]])
      children <- grobs[[panel]]$children
      connector <- Filter(function(child) inherits(child, "polyline"), children)
      expect_length(connector, 1L)
      expect_equal(as.numeric(connector[[1L]]$x)[1L], start$x)
      expect_equal(as.numeric(connector[[1L]]$y)[1L], start$y)

      if (name == "combined") {
        expect_false(any(vapply(children, inherits, logical(1), "points")))
        labels <- unlist(lapply(children, function(child) as.character(child$label)))
        expect_false("\u221272" %in% labels)
        expect_true(any(vapply(children, inherits, logical(1), "rect")))
      }
    }
  }
  built <- plots$combined
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
