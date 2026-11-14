test_that("projection verbs optionally store fragments and transcript-specific anchors", {
  model <- prepared_genomic_fixture()
  old <- project_mutations_to_genome(model, annotation = "muts")
  expect_s3_class(old, "data.frame")
  projected <- project_mutations_to_genome(model, annotation = "muts", store = TRUE, name = "projected")
  expect_s4_class(projected, "SynIndividual")
  expect_s4_class(get_annotation(projected, "projected"), "SynGenomicProjectionAnnotation")
  fragments <- query_genomic_projection(projected, "projected")
  anchors <- query_genomic_projection(projected, "projected", component = "anchors")
  expect_equal(nrow(fragments), 6L)
  expect_equal(nrow(anchors), 3L)
  expect_equal(sum(anchors$mutation == "K2R"), 2L)
  expect_setequal(anchors$transcripts, c("tx1", "tx2"))
  expect_true(all(anchors$anchor == 103.5))
  expect_equal(nrow(query_protein_mutations(projected, annotation = "muts")), 3L)
  expect_equal(project_mutations_to_genome(projected, annotation = "projected"), fragments)
  selected <- project_mutations_to_genome(model, annotation = "muts", transcripts = "tx1", store = TRUE)
  expect_equal(nrow(query_genomic_projection(selected, "genomic_mutations", component = "anchors")), 2L)
})

test_that("stored domain projections retain fragments and native motif dispatch", {
  model <- project_domains_to_genome(prepared_genomic_fixture(), annotation = "regions",
    transcripts = "tx1", store = TRUE, name = "projected_regions")
  segments <- query_genomic_projection(model, "projected_regions")
  expect_true(nrow(segments) >= 2L)
  expect_true(all(segments$transcripts == "tx1"))
  built <- ggexon_build(ggexon(model) + geom_exon2(compress_introns = FALSE) +
    geom_motif(annotation = "projected_regions", model = "Pfam"))
  expect_equal(nrow(built@data[[2L]]), nrow(segments))
  all_domains <- project_domains_to_genome(prepared_genomic_fixture(), annotation = "regions",
    transcripts = c("tx1", "tx2"), store = TRUE, name = "projected_regions")
  selected <- project_domains_to_genome(all_domains, annotation = "projected_regions", ids = "tx1")
  expect_true(nrow(selected) > 0L)
  expect_true(all(selected$transcripts == "tx1"))
  expect_equal(nrow(project_domains_to_genome(all_domains, annotation = "projected_regions", genes = "missing")), 0L)
})

test_that("stored projections reject source mutation or CDS changes", {
  model <- project_mutations_to_genome(prepared_genomic_fixture(), annotation = "muts", store = TRUE)
  ann <- get_annotation(model, "muts")
  ann@mutation_data$sample_count[1L] <- 20L
  changed <- add_annotation(model, ann)
  expect_error(query_genomic_projection(changed, "genomic_mutations"), "stale|changed")
  features <- annotation_data(model)
  IRanges::start(features)[as.character(S4Vectors::mcols(features)$type) == "CDS"][1L] <- 101L
  annotation_data(model) <- features
  expect_error(query_genomic_projection(model, "genomic_mutations"), "stale|changed")
})

test_that("prepared layout shares bounded head positions and one intron map", {
  model <- project_mutations_to_genome(prepared_genomic_fixture(), annotation = "muts",
    transcripts = "tx1", store = TRUE, name = "projected")
  model <- prepare_genomic_layout(model, annotation = "projected", name = "locus",
    intron_factor = 3, head_spread = 4, mutation_y_by = "sample_count",
    mutation_y_trans = "log10", mutation_y_range = log10(c(2, 11)),
    track_ymin = 2, track_ymax = 2.8)
  heads <- query_genomic_layout(model, "locus", "mutations")
  stems <- query_genomic_layout(model, "locus", "stems")
  exons <- query_genomic_layout(model, "locus", "exons")
  layout <- .get_genomic_layout(model, "locus")
  expect_equal(exons$xmin, c(99.5, 199.5))
  expect_equal(exons$xmax, c(104.5, 208.5))
  expect_equal(heads$lollipop_position, c(103.5, 103.5))
  expect_equal(heads$lollipop_x, c(99.5, 107.5))
  expect_equal(heads$lollipop_y, log10(c(2, 11)))
  expect_equal(stems$lollipop_x[!duplicated(stems$lollipop_curve_id)], heads$lollipop_position)
  expect_equal(stems$lollipop_x[!duplicated(stems$lollipop_curve_id, fromLast = TRUE)], heads$lollipop_x)
  intron <- layout$map[layout$map$region_type == "intron", ]
  expect_equal(intron$plot_end - intron$plot_start, 95 / 3)
  expect_false("PANEL" %in% names(layout$map))
  expect_false("PANEL" %in% names(layout$source_rows))
  plot <- ggexon(model) + geom_exon2(genomic_layout = "locus") +
    geom_protein_lollipop(genomic_layout = "locus", show_backbone = FALSE, show_domains = FALSE) +
    geom_mutation_label(genomic_layout = "locus", label_nudge_y = 0) +
    scale_x_ggexon_genomic(genomic_layout = "locus")
  built <- ggexon_build(plot)
  expect_equal(built@data[[3L]]$x, built@data[[4L]]$x)
  expect_equal(built@data[[3L]]$y, built@data[[4L]]$y)
  expect_equal(built@data[[1L]]$ymin, c(2, 2))
  expect_equal(built@data[[1L]]$ymax, c(2.8, 2.8))
})

test_that("a replaced projection invalidates layouts even with unchanged source data", {
  model <- prepared_genomic_fixture()
  model <- project_mutations_to_genome(model, annotation = "muts", transcripts = "tx1", store = TRUE)
  model <- prepare_genomic_layout(model, annotation = "genomic_mutations", name = "locus")
  model <- project_mutations_to_genome(model, annotation = "muts", transcripts = "tx2", store = TRUE)
  expect_error(query_genomic_layout(model, "locus"), "stale|changed")
})

test_that("prepared exon height and head colours are consumed consistently", {
  model <- project_mutations_to_genome(prepared_genomic_fixture(), annotation = "muts",
    transcripts = "tx1", store = TRUE)
  model <- prepare_genomic_layout(model, annotation = "genomic_mutations", name = "locus",
    head_spread = 4, track_ymin = -0.34, track_ymax = -0.08)
  plot <- ggexon(model) + geom_exon2(genomic_layout = "locus") +
    geom_protein_lollipop(genomic_layout = "locus", show_backbone = FALSE, show_domains = FALSE,
      score = "alt", stem_colour = NULL, stem_alpha = 0.27, point_alpha = 0.9) +
    scale_colour_manual(values = c(R = "red", Q = "blue")) +
    scale_x_ggexon_genomic(genomic_layout = "locus")
  built <- ggexon_build(plot)
  expect_equal(built@data[[1L]]$ymin, rep(-0.34, 2L))
  expect_equal(built@data[[1L]]$ymax, rep(-0.08, 2L))
  expect_equal(built@data[[2L]]$colour[c(1L, 31L)], built@data[[3L]]$colour)
  expect_true(all(built@data[[2L]]$alpha == 0.27))
  expect_true(all(built@data[[3L]]$alpha == 0.9))
  expect_error(geom_exon2(genomic_layout = "locus", compress_introns = TRUE), "prepared|spacing")
  expect_error(geom_protein_lollipop(genomic_layout = "locus", spread_threshold = 2), "prepare_genomic_layout")
  expect_error(geom_exon2(aes(xmin = xmin + 10), genomic_layout = "locus"), "coordinates|geometry")
  expect_error(geom_exon2(genomic_layout = "locus", xmin = 10), "coordinates|geometry")
  expect_error(geom_mutation_label(genomic_layout = "locus", position = "jitter"), "position|geometry")
  expect_error(geom_mutation_label(genomic_layout = "locus", x = 10), "coordinates|geometry")
  expect_error(ggexon_build(ggexon(model) + geom_exon2(genomic_layout = "locus")), "scale|layout")
})

test_that("an empty stored mutation selection still supports the reference layout", {
  model <- project_mutations_to_genome(prepared_genomic_fixture(), annotation = "muts",
    transcripts = "tx1", min_sample_count = 100, store = TRUE)
  expect_equal(nrow(query_genomic_projection(model, "genomic_mutations")), 0L)
  model <- prepare_genomic_layout(model, annotation = "genomic_mutations", name = "empty", transcripts = "tx1")
  plot <- ggexon(model) + geom_exon2(genomic_layout = "empty") +
    geom_protein_lollipop(genomic_layout = "empty", show_domains = FALSE, show_backbone = FALSE) +
    scale_x_ggexon_genomic(genomic_layout = "empty")
  built <- ggexon_build(plot)
  expect_equal(nrow(built@data[[1L]]), 2L)
  expect_equal(nrow(built@data[[3L]]), 0L)
})

test_that("cached domain filters retain source identities and reference annotation", {
  model <- prepared_genomic_fixture()
  ann <- get_annotation(model, "regions")
  other <- ann@domain_data
  other$domain <- "Other"
  other$signature_accession <- "alternate_signature"
  ann@domain_data$signature_accession <- "binding_signature"
  ann@domain_data <- rbind(ann@domain_data, other)
  model <- add_annotation(model, ann)
  model <- project_domains_to_genome(model, annotation = "regions",
    transcripts = c("tx1", "tx2"), store = TRUE)
  expect_equal(nrow(project_domains_to_genome(model, annotation = "genomic_domains",
    domains = "Binding", motif = "Other")), 0L)
  selected <- project_domains_to_genome(model, annotation = "genomic_domains", motif = "binding_signature")
  expect_true(nrow(selected) > 0L)
  expect_true(all(selected$domain_id == "Binding"))
  expected <- project_domains_to_genome(model, annotation = "genomic_domains", ids = "tx1")
  alternate <- get_annotation(model)
  alternate@name <- "alternate"
  S4Vectors::mcols(alternate@annotation)$transcript_id <- "different"
  alternate@feature_index <- NULL
  model <- add_annotation(model, alternate)
  model <- set_active_feature_annotation(model, "alternate")
  expect_equal(project_domains_to_genome(model, annotation = "genomic_domains", ids = "tx1"), expected)
})

test_that("prepared layers reject detached data", {
  expect_error(geom_mutation_label(data = data.frame(position = 1:2, mutation = c("K2R", "K2Q")),
    genomic_layout = "locus"), "Syn object")
  expect_error(geom_exon2(data = data.frame(xmin = 1, xmax = 2, transcripts = "tx1"),
    genomic_layout = "locus"), "Syn object")
})

test_that("score colours are shared by stems for ordinary lollipop inputs", {
  built <- ggplot2::ggplot_build(ggplot2::ggplot() + geom_protein_lollipop(
    mutations = data.frame(position = 1:2, score = c(5, 10)), score = "score",
    stem_colour = NULL, show_backbone = FALSE, show_domains = FALSE))
  expect_equal(built$data[[1L]]$colour[c(1L, 31L)], built$data[[2L]]$colour)
})
