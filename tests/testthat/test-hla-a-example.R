hla_example_fixture <- function() {
  demo_dir <- system.file("extdata", "hla_a_lollipop", package = "ggexon")
  helpers <- new.env(parent = globalenv())
  for (file in c("load-hla-a-example.R", "plot-hla-a-example.R", "project-hla-a-example.R")) {
    sys.source(file.path(demo_dir, file), envir = helpers)
  }
  list(model = helpers$load_hla_a_example(demo_dir), helpers = helpers)
}

test_that("the HLA example loads one reference object with linked protein annotations", {
  fixture <- hla_example_fixture()
  model <- fixture$model
  expect_s4_class(model, "SynIndividual")
  expect_true(methods::validObject(model))
  expect_setequal(annotation_names(model), c("default", "hla_substitutions", "hla_regions"))
  expect_s4_class(get_annotation(model, "hla_substitutions"), "SynProteinMutationAnnotation")
  expect_s4_class(get_annotation(model, "hla_regions"), "SynProteinDomainAnnotation")
  mutations <- query_protein_mutations(model, annotation = "hla_substitutions")
  regions <- as.data.frame(query_domains(get_annotation(model, "hla_regions")))
  expect_equal(nrow(mutations), 1212L)
  expect_equal(mutations$position, mutations$mature_position + 24L)
  expect_equal(unique(mutations$transcript_id), "HLA_A0201_tx")
  expect_equal(regions$start, regions$mature_start + 24L)
  expect_equal(regions$end, regions$mature_end + 24L)
  expect_equal(nchar(sub("\\*$", "", as.character(protein_seq(model)))),
    c(HLA_A0201_tx = 365L))
  expect_identical(names(protein_seq(model)), "HLA_A0201_tx")
  evidence <- syn_metadata(model)$hla_a
  expect_equal(nrow(evidence$alleles), 5998L)
  expect_equal(length(evidence$proteins), 5998L)
  expect_equal(evidence$distinct_mature_proteins, 3188L)
  expect_true(all(tapply(evidence$states$allele_count, evidence$states$position, sum) == 5998L))
})

test_that("HLA protein components dispatch directly from the stored annotations", {
  fixture <- hla_example_fixture()
  plot <- ggexon(fixture$model) + geom_protein_lollipop(
    annotation = "hla_substitutions", domain_annotation = "hla_regions",
    position = "mature_position", domain_start = "mature_start", domain_end = "mature_end",
    protein_length = 341, protein_start = 1, domain = "domain", spread_threshold = 0,
    mutation_y_by = "allele_count", mutation_y_trans = "log10",
    mutation_y_range = log10(c(2, 4668))
  )
  built <- ggexon_build(plot)
  expect_equal(nrow(built@data[[4L]]), 1212L)
  expect_equal(built@data[[2L]]$xmax, 341)
  expect_equal(built@data[[3L]]$xmin, c(1, 91, 183, 275, 285, 309))
  mutations <- query_protein_mutations(fixture$model, annotation = "hla_substitutions")
  expect_equal(built@data[[4L]]$x, as.numeric(mutations$mature_position))
  expect_equal(built@data[[4L]]$y, log10(mutations$allele_count + 1))
})

test_that("HLA figures inherit one Syn object and query their plotting data at build", {
  fixture <- hla_example_fixture()
  plots <- fixture$helpers$plot_hla_a_example(fixture$model)
  for (name in c("overview", "focus")) {
    plot <- plots[[name]]
    expect_s4_class(plot@data, "SynIndividual")
    built <- ggexon_build(plot)
    points <- which(vapply(built@plot@layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
    expect_length(points, 1L)
    expect_true(is.function(built@plot@layers[[points]]$data))
    expected <- if (name == "overview") 1212L else 5L
    expect_equal(nrow(built@data[[points]]), expected)
    heads <- plots[[paste0(name, "_data")]]$mutations
    expect_equal(built@data[[points]]$y, log10(heads$allele_count + 1))
  }
})

test_that("HLA genomic plots resolve exons and splice-aware anchors from the same object", {
  fixture <- hla_example_fixture()
  model <- fixture$helpers$prepare_hla_a_gene_layout(fixture$model, intron_factor = 3)
  structure <- fixture$helpers$hla_a_gene_structure(model)
  projected <- fixture$helpers$hla_a_projected_mutations(model)
  expect_equal(sum(structure$type == "exon"), 8L)
  expect_equal(sum(structure$type == "intron"), 7L)
  expect_equal(nrow(projected$anchors), 1212L)
  expect_true(all(projected$anchors$anchor[projected$anchors$mature_position == 1L] == 504.5))
  native <- fixture$helpers$plot_hla_a_gene_example(
    fixture$helpers$prepare_hla_a_gene_layout(model, intron_factor = 1), intron_factor = 1)
  compressed <- fixture$helpers$plot_hla_a_gene_example(model, intron_factor = 3)
  expect_s4_class(compressed$plot@data, "SynIndividual")
  expect_true(inherits(compressed$plot@layers[[1L]]$data, "waiver"))
  b1 <- ggexon_build(native$plot)
  b3 <- ggexon_build(compressed$plot)
  e1 <- b1@data[[1L]]
  e3 <- b3@data[[1L]]
  expect_equal(e1$xmax - e1$xmin, as.numeric(structure$length[structure$type == "exon"]))
  expect_equal(e3$xmax - e3$xmin, e1$xmax - e1$xmin)
  expect_equal(e3$xmin[-1L] - e3$xmax[-8L], (e1$xmin[-1L] - e1$xmax[-8L]) / 3)
  points <- which(vapply(b3@plot@layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  expect_equal(nrow(b3@data[[points]]), 1212L)
})
