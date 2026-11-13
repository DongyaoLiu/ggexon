protein_track_fixture <- function(id = "cohort", keytype = "gene_id") {
  mutation_file <- tempfile(fileext = ".tsv")
  domain_file <- tempfile(fileext = ".tsv")
  mutations <- data.frame(
    gene_id = c("geneA", "geneA", "geneB"),
    transcript_id = c("txA", "txA", "txB"),
    position = c(18, 42, 250), mutation = c("A18V", "K42R", "G250D"),
    sample_count = c(2, 5, 12)
  )
  domains <- data.frame(
    gene_id = c("geneA", "geneB"), transcript_id = c("txA", "txB"),
    start = c(10, 200), end = c(60, 300),
    domain = c("Binding", "Other"), protein_length = c(160, 400)
  )
  write.table(mutations, mutation_file, sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(domains, domain_file, sep = "\t", quote = FALSE, row.names = FALSE)
  x <- add_annotation(SynIndividual(id = id), SynProteinMutationAnnotation(
    name = "counts", mutation_file = mutation_file, keytype = keytype
  ))
  add_annotation(x, SynProteinDomainAnnotation(
    name = "domains", domain_file = domain_file, keytype = keytype
  ))
}

test_that("protein lollipops resolve lazy annotations and retain true protein length", {
  x <- protein_track_fixture()
  plot <- ggexon(x) +
    geom_protein_lollipop(
      annotation = "counts", domain_annotation = "domains", genes = "geneA",
      mutation_y_by = "sample_count", mutation_y_range = c(2, 5)
    ) +
    geom_mutation_label(
      annotation = "counts", genes = "geneA", mutation_y_by = "sample_count",
      mutation_y_range = c(2, 5), label_nudge_y = 0.8
    )
  built <- ggexon_build(plot)
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(built@data[[3]]$xmin, 10)
  expect_equal(built@data[[3]]$xmax, 60)
  expect_equal(built@data[[4]]$x, c(18, 42))
  expect_equal(built@data[[4]]$y, c(2, 5))
  expect_equal(built@data[[5]]$x, c(18, 42))
  expect_equal(built@data[[5]]$y, c(2.8, 5.8))
  expect_false(get_annotation(x, "counts")@loaded)
  expect_false(get_annotation(x, "domains")@loaded)
  expect_s3_class(ggplot2::ggplot_gtable(built), "gtable")
})

test_that("protein identifier selectors apply to lollipops and labels", {
  x <- protein_track_fixture(keytype = "transcript_id")
  plot <- ggexon(x) +
    geom_protein_lollipop(annotation = "counts", domain_annotation = "domains", ids = "txA") +
    geom_mutation_label(annotation = "counts", ids = "txA")
  built <- ggexon_build(plot)
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(built@data[[4]]$x, c(18, 42))
  expect_equal(built@data[[5]]$label, c("A18V", "K42R"))
})

test_that("SynSpecies lollipops keep individuals in separate facet tracks", {
  sp <- add_individual(SynSpecies("cohorts"), protein_track_fixture("one"))
  sp <- add_individual(sp, protein_track_fixture("two"))
  plot <- ggexon(sp) +
    geom_protein_lollipop(annotation = "counts", domain_annotation = "domains", genes = "geneA") +
    facet_genomics(ggplot2::vars(track), ncol = 1)
  built <- ggexon_build(plot)
  points <- built@data[[4]]
  expect_equal(nrow(points), 4)
  expect_setequal(as.character(points$track), c("one", "two"))
  expect_equal(length(unique(points$PANEL)), 2)
  stems <- built@data[[1]]
  expect_equal(length(unique(stems$group)), 4)
  expect_s3_class(ggplot2::ggplot_gtable(built), "gtable")
  selected <- ggexon_build(ggexon(sp) + geom_protein_lollipop(
    annotation = "counts", domain_annotation = "domains", genes = "geneA", species = "two"
  ))
  expect_equal(nrow(selected@data[[4]]), 2)
  expect_true(all(selected@data[[4]]$track == "two"))
})

test_that("explicit Syn layer data and mutation filters preserve the selected backbone", {
  x <- protein_track_fixture()
  built <- ggexon_build(ggexon() + geom_protein_lollipop(
    data = x, annotation = "counts", domain_annotation = "domains",
    genes = "geneA", min_sample_count = 100
  ))
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(nrow(built@data[[3]]), 1)
  expect_equal(nrow(built@data[[4]]), 0)
})

test_that("protein dispatch rejects ambiguous proteins and incorrect annotation types", {
  x <- protein_track_fixture()
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop()), "select.*protein|one protein")
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(
    annotation = "domains", genes = "geneA"
  )), "SynProteinMutationAnnotation")
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(
    annotation = "counts", domain_annotation = "counts", genes = "geneA"
  )), "SynProteinDomainAnnotation")
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(
    genes = "geneA", protein_length = 30
  )), "protein_length|protein length")
})

test_that("Syn lollipops require a full length when only mutations are stored", {
  x <- protein_track_fixture()
  x <- add_annotation(SynIndividual(id = "counts-only"), get_annotation(x, "counts"))
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA")),
    "protein_length")
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA", protein_length = 160))
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(nrow(built@data[[4]]), 2)
})

test_that("stored protein sequences provide the full length without a terminal stop", {
  gtf <- tempfile(fileext = ".gtf")
  writeLines('chr1\ttest\texon\t1\t483\t.\t+\t.\tgene_id "geneA"; transcript_id "txA";', gtf)
  features <- SynFeatureAnnotation(name = "features", annotation_file = gtf, annotation_format = "gtf")
  protein_seq(features) <- Biostrings::AAStringSet(c(txA = paste0(strrep("A", 160), "*")))
  x <- add_annotation(SynIndividual(id = "sequence-backed"), features, set_active = TRUE)
  original <- protein_track_fixture(keytype = "transcript_id")
  x <- add_annotation(x, get_annotation(original, "counts"))
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(ids = "txA"))
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(built@data[[4]]$x, c(18, 42))
})

test_that("explicit domain annotations are filtered to the selected protein", {
  x <- protein_track_fixture()
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(
    genes = "geneA", domains = get_annotation(x, "domains")
  ))
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(built@data[[3]]$xmax, 60)
})

test_that("domain keys use the corresponding identifier in mutation records", {
  x <- protein_track_fixture()
  domains <- get_annotation(x, "domains")
  domains@keytype <- "transcript_id"
  x <- add_annotation(x, domains)
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA"))
  expect_equal(built@data[[2]]$xmax, 160)
  expect_equal(built@data[[3]]$xmax, 60)
})

test_that("head and label heights use the same per-individual count mapping", {
  one <- protein_track_fixture("one")
  two <- protein_track_fixture("two")
  ann <- get_annotation(two, "counts")
  ann@mutation_data <- query_protein_mutations(ann)
  ann@mutation_data$sample_count <- c(10, 20, 50)
  two <- add_annotation(two, ann)
  sp <- add_individual(add_individual(SynSpecies("cohorts"), one), two)
  built <- ggexon_build(ggexon(sp) +
    geom_protein_lollipop(genes = "geneA", mutation_y_by = "sample_count",
      mutation_y_range = c(0, 1)) +
    geom_mutation_label(genes = "geneA", mutation_y_by = "sample_count",
      mutation_y_range = c(0, 1), label_nudge_y = 0) +
    facet_genomics(ggplot2::vars(track), ncol = 1))
  expect_equal(built@data[[4]]$y, c(0, 1, 0, 1))
  expect_equal(built@data[[5]]$y, c(0, 1, 0, 1))
})

test_that("a coarse gene key cannot silently mix isoforms", {
  x <- protein_track_fixture()
  ann <- get_annotation(x, "counts")
  ann@mutation_data <- query_protein_mutations(ann)
  ann@mutation_data$transcript_id[2] <- "another-isoform"
  x <- add_annotation(x, ann)
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA")),
    "one protein|one transcript")
})

test_that("plain domain overrides match the selected transcript as well as gene", {
  x <- protein_track_fixture()
  domains <- data.frame(transcript_id = c("txA", "txB"),
    start = c(10, 90), end = c(60, 100), domain = c("Right", "Wrong"),
    protein_length = 160)
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA", domains = domains))
  expect_equal(nrow(built@data[[3]]), 1)
  expect_equal(built@data[[3]]$xmax, 60)
})

test_that("default species dispatch skips individuals without mutation annotations", {
  sp <- add_individual(SynSpecies("cohorts"), protein_track_fixture("one"))
  sp <- add_individual(sp, SynIndividual(id = "unrelated"))
  built <- ggexon_build(ggexon(sp) + geom_protein_lollipop(genes = "geneA"))
  expect_equal(nrow(built@data[[4]]), 2)
  expect_true(all(built@data[[4]]$track == "one"))
  expect_error(ggexon_build(ggexon(sp) + geom_protein_lollipop(
    genes = "geneA", species = "unrelated"
  )), "SynProteinMutationAnnotation")
})

test_that("source sample metadata does not separate label layout within one protein", {
  x <- protein_track_fixture()
  ann <- get_annotation(x, "counts")
  ann@mutation_data <- query_protein_mutations(ann)
  ann@mutation_data$individual <- c("source-a", "source-b", "source-c")
  ann@mutation_data$gene_name <- c("alias-a", "alias-b", "geneB")
  ann@mutation_data$transcript_id[1] <- NA_character_
  ann@mutation_data$position[2] <- 19
  ann@mutation_data$mutation[2] <- "K19R"
  x <- add_annotation(x, ann)
  built <- ggexon_build(ggexon(x) + geom_protein_lollipop(genes = "geneA") +
    geom_mutation_label(genes = "geneA"))
  expect_equal(built@data[[4]]$x, c(18, 25))
  expect_equal(built@data[[5]]$x, c(18, 25))
})

test_that("domain-only isoform identifiers cannot be pooled under a gene key", {
  x <- protein_track_fixture()
  ann <- get_annotation(x, "counts")
  ann@mutation_data <- query_protein_mutations(ann)
  ann@mutation_data$transcript_id <- NULL
  x <- add_annotation(x, ann)
  domains <- data.frame(gene_id = "geneA", transcript_id = c("txA", "txB"),
    start = c(10, 90), end = c(60, 100), domain = c("One", "Two"),
    protein_length = 160)
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(
    genes = "geneA", domains = domains
  )), "one protein|one transcript")
  domain_file <- tempfile(fileext = ".tsv")
  write.table(domains, domain_file, sep = "\t", quote = FALSE, row.names = FALSE)
  x <- add_annotation(x, SynProteinDomainAnnotation(
    name = "isoforms", domain_file = domain_file, keytype = "gene_id"
  ))
  expect_error(ggexon_build(ggexon(x) + geom_protein_lollipop(
    genes = "geneA", domain_annotation = "isoforms"
  )), "one protein|one transcript")
})
