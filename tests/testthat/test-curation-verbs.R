curation_homology_fixture <- function() {
  HomologyAnnotation(
    name = "query_to_reference", reference_species = "reference",
    query_species = "query",
    homology_table = data.frame(
      query_gene = c("q1", "q2"), reference_gene = c("r1", "r2"),
      bitscore = c(40, 60), source = c("blast", "blast")
    )
  )
}

curation_feature_fixture <- function() {
  gr <- GenomicRanges::GRanges(
    "chr1", IRanges::IRanges(c(100L, 110L, 300L, 310L),
                            c(180L, 170L, 380L, 370L)), strand = "+"
  )
  S4Vectors::mcols(gr) <- S4Vectors::DataFrame(
    type = c("gene", "exon", "gene", "exon"),
    ID = c("geneA", "exonA", "geneB", "exonB"),
    Parent = c(NA_character_, "geneA", NA_character_, "geneB"),
    gene_id = c("geneA", "geneA", "geneB", "geneB"),
    gene_name = c("geneA", "geneA", "geneB", "geneB")
  )
  ann <- SynFeatureAnnotation("curated", "<in-memory>")
  annotation_data(ann) <- gr
  ann
}

test_that("update_homology preserves omitted columns and unselected relationships", {
  updated <- expect_no_warning(update_homology(
    curation_homology_fixture(), query_gene = "q1",
    reference_gene = "r1_curated", source = "manual"
  ))
  tbl <- homology_table(updated)
  expect_identical(tbl$query_gene, c("q1", "q2"))
  expect_identical(tbl$reference_gene, c("r1_curated", "r2"))
  expect_identical(tbl$source, c("manual", "blast"))
  expect_equal(tbl$bitscore, c(40, 60))
  expect_error(update_homology(updated, query_gene = "absent", source = "manual"),
               "missing")
})

test_that("homology aliases dispatch through selected SynSpecies annotations", {
  sp <- SynSpecies("curation")
  sp <- add_homology_annotation(sp, curation_homology_fixture())
  updated <- update_homology(
    sp, name = "query_to_reference", query_gene = "q1",
    reference_gene = "r1_curated", source = "manual"
  )
  removed <- expect_no_warning(remove_homology(
    updated, query_species = "query", query_gene = "q2", reference_gene = "r2"
  ))
  tbl <- homology_table(get_homology_annotation(removed, name = "query_to_reference"))
  expect_identical(tbl$query_gene, "q1")
  expect_identical(tbl$reference_gene, "r1_curated")
  expect_equal(tbl$bitscore, 40)
})

test_that("remove_homology retains reference guards and missing-id handling", {
  ann <- curation_homology_fixture()
  expect_error(remove_homology(ann, query_gene = "q1", reference_gene = "wrong"),
               "guard")
  expect_error(remove_homology(ann, query_gene = "absent"), "missing")
  retained <- remove_homology(ann, query_gene = "absent", missing = "ignore")
  expect_identical(homology_table(retained)$query_gene, c("q1", "q2"))
  removed <- remove_homology(ann, query_gene = "q1", reference_gene = "r1")
  expect_identical(homology_table(removed)$query_gene, "q2")
  expect_identical(homology_table(removed)$reference_gene, "r2")
})

test_that("map_domain_ids populates the target key used by domain queries", {
  domain_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(domain_file), add = TRUE)
  writeLines(c("protein_id\tdomain\tstart\tend", "p1\tD1\t2\t8",
               "p2\tD2\t4\t10"), domain_file)
  domains <- SynProteinDomainAnnotation("domains", domain_file, keytype = "protein_id")
  mapped <- expect_no_warning(map_domain_ids(domains, c(p1 = "tx1")))
  hits <- query_domains(mapped, ids = "tx1")
  expect_identical(as.character(hits$transcript_id), "tx1")
  expect_identical(as.character(hits$domain), "D1")
  all_domains <- query_domains(mapped)
  expect_equal(nrow(all_domains), 2L)
  expect_true(is.na(all_domains$transcript_id[[2L]]))

  individual <- add_annotation(SynIndividual(id = "sample"), domains)
  mapped_individual <- map_domain_ids(
    individual, c(p2 = "tx2"), annotation = "domains", drop_unmapped = TRUE
  )
  selected <- query_domains(get_annotation(mapped_individual, "domains"), ids = "tx2")
  expect_equal(nrow(selected), 1L)
  expect_identical(as.character(selected$domain), "D2")
})

test_that("reset_patches restores base features while retaining display labels", {
  ann <- set_gene_labels(curation_feature_fixture(), c(geneA = "Gene A"))
  patch_gr <- annotation_data(ann)[1L]
  IRanges::ranges(patch_gr) <- IRanges::IRanges(120L, 160L)
  patched <- patch_annotation(ann, patch_gr, target_ids = "geneA", name = "correction")
  restored <- expect_no_warning(reset_patches(patched))
  expect_equal(IRanges::start(annotation_data(restored)), c(100L, 110L, 300L, 310L))
  expect_equal(IRanges::end(annotation_data(restored)), c(180L, 170L, 380L, 370L))
  expect_identical(as.character(S4Vectors::mcols(annotation_data(restored))$plot_label),
                   c("Gene A", "Gene A", "geneB", "geneB"))
  expect_length(list_patches(restored), 0L)

  individual <- add_annotation(SynIndividual(id = "sample"), patched, set_active = TRUE)
  restored_individual <- reset_patches(individual, annotation = "curated")
  expect_equal(IRanges::start(annotation_data(restored_individual)),
               c(100L, 110L, 300L, 310L))
  expect_length(list_patches(restored_individual, "curated"), 0L)
})

test_that("remove patches retain untargeted genes and recorded mode", {
  removal <- SynAnnotationPatch("remove-A", target_ids = "geneA", mode = "remove")
  expect_true(validObject(removal))
  expect_identical(patch_mode(removal), "remove")
  removed <- patch_annotation(curation_feature_fixture(), removal)
  expect_identical(as.character(S4Vectors::mcols(annotation_data(removed))$gene_id),
                   c("geneB", "geneB"))
  expect_length(list_patches(removed), 1L)
  expect_identical(patch_mode(list_patches(removed)[[1L]]), "remove")
  restored <- reset_patches(removed)
  expect_identical(as.character(S4Vectors::mcols(annotation_data(restored))$ID),
                   c("geneA", "exonA", "geneB", "exonB"))
})

test_that("remove mode edits a named feature annotation on an individual", {
  individual <- add_annotation(
    SynIndividual(id = "sample"), curation_feature_fixture(), set_active = TRUE
  )
  removed <- patch_annotation(
    individual, patch = NULL, annotation = "curated", target_ids = "geneA",
    mode = "remove", name = "remove-A"
  )
  expect_identical(as.character(S4Vectors::mcols(annotation_data(removed))$gene_id),
                   c("geneB", "geneB"))
  expect_length(list_patches(removed, "curated"), 1L)
})

test_that("GFF removal does not require importing a patch payload", {
  absent_file <- tempfile(fileext = ".gff3")
  expect_false(file.exists(absent_file))
  removed <- patch_annotation_from_gff(
    curation_feature_fixture(), patch_file = absent_file,
    target_ids = "geneA", mode = "remove", name = "remove-A"
  )
  expect_identical(as.character(S4Vectors::mcols(annotation_data(removed))$ID),
                   c("geneB", "exonB"))
  expect_identical(patch_mode(list_patches(removed)[[1L]]), "remove")
})

test_that("serialized legacy drop patches remain valid and applicable", {
  legacy <- SynAnnotationPatch("drop-A", target_ids = "geneA", mode = "drop")
  restored_patch <- unserialize(serialize(legacy, NULL))
  expect_true(validObject(restored_patch))
  expect_identical(patch_mode(restored_patch), "drop")
  removed <- patch_annotation(curation_feature_fixture(), restored_patch)
  expect_identical(as.character(S4Vectors::mcols(annotation_data(removed))$ID),
                   c("geneB", "exonB"))
  expect_identical(patch_mode(list_patches(removed)[[1L]]), "drop")
})

test_that("legacy replacement shorthand works beside the exact remove mode", {
  patch_gr <- annotation_data(curation_feature_fixture())[1L]
  IRanges::ranges(patch_gr) <- IRanges::IRanges(120L, 160L)
  patch_file <- tempfile(fileext = ".gff3")
  on.exit(unlink(patch_file), add = TRUE)
  writeLines(c("##gff-version 3",
               "chr1\tcuration\tgene\t120\t160\t.\t+\t.\tID=geneA"), patch_file)

  for (mode in c("r", "re")) {
    patch <- SynAnnotationPatch("replace-A", patch_gr, "geneA", mode = mode)
    expect_identical(patch_mode(patch), "replace")
    edited <- patch_annotation(
      curation_feature_fixture(), patch_gr, target_ids = "geneA",
      mode = mode, name = "replace-A"
    )
    expect_equal(IRanges::start(annotation_data(edited)), c(120L, 300L, 310L))
    imported <- patch_annotation_from_gff(
      curation_feature_fixture(), patch_file, target_ids = "geneA", mode = mode,
      name = "replace-A"
    )
    expect_equal(IRanges::start(annotation_data(imported)), c(120L, 300L, 310L))
  }
  removed <- patch_annotation(
    curation_feature_fixture(), patch = NULL, target_ids = "geneA",
    mode = "remove", name = "remove-A"
  )
  expect_identical(as.character(S4Vectors::mcols(annotation_data(removed))$ID),
                   c("geneB", "exonB"))
})
