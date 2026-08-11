test_that("add verbs are S4 generics with class-specific methods", {
  expect_true(methods::isGeneric("add_annotation"))
  expect_true(methods::isGeneric("add_individual"))
  expect_true(methods::isGeneric("add_individuals_from_folder"))
  expect_true(methods::isGeneric("add_pairwise_alignment"))
  expect_true(methods::isGeneric("add_multiple_alignment"))
  expect_true(methods::isGeneric("add_interproscan_annotation"))
  expect_true(methods::isGeneric("add_protein_mutation_annotation"))
  expect_true(methods::isGeneric("add_tree"))

  expect_true(methods::hasMethod("add_annotation", c("SynIndividual", "SynAnnotation")))
  expect_true(methods::hasMethod("add_individual", c("SynSpecies", "SynIndividual")))
  expect_true(methods::hasMethod("add_pairwise_alignment", c("SynSpecies", "SynPairAlignment")))
  expect_true(methods::hasMethod("add_multiple_alignment", c("SynSpecies", "SynMultiAlignment")))
  expect_true(methods::hasMethod("add_interproscan_annotation", "SynIndividual"))
  expect_true(methods::hasMethod("add_protein_mutation_annotation", "SynIndividual"))
  expect_true(methods::hasMethod("add_protein_mutation_annotation", "SynSpecies"))
  expect_true(methods::hasMethod("add_tree", "SynSpecies"))

  expect_true(methods::hasMethod("add_annotation", c("SynIndividual", "ANY")))
  expect_true(methods::hasMethod("add_individual", c("SynSpecies", "ANY")))
  expect_true(methods::hasMethod("add_pairwise_alignment", c("SynSpecies", "ANY")))
  expect_true(methods::hasMethod("add_multiple_alignment", c("SynSpecies", "ANY")))
  expect_true(methods::hasMethod("add_interproscan_annotation", "ANY"))
  expect_true(methods::hasMethod("add_protein_mutation_annotation", "ANY"))
  expect_true(methods::hasMethod("add_tree", "ANY"))
})

test_that("add verb fallback methods preserve clear validation errors", {
  annotation_path <- system.file("extdata", "compact_synspecies", "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  ann <- SynFeatureAnnotation(name = "features", annotation_file = annotation_path)
  individual <- SynIndividual(id = "XZ1516")
  species <- SynSpecies(name = "worms")
  pair <- SynPairAlignment(
    name = "pair",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = tempfile(fileext = ".paf")
  )
  multi <- SynMultiAlignment(
    name = "multi",
    individuals = c("XZ1516", "N2"),
    file = tempfile(fileext = ".maf")
  )

  expect_error(add_annotation("not-an-individual", ann), "expects a SynIndividual object")
  expect_error(add_annotation(individual, "not-an-annotation"), "`annotation` must be")
  expect_error(add_individual("not-a-species", individual), "expects a SynSpecies object")
  expect_error(add_individual(species, "not-an-individual"), "must be SynIndividual objects")
  expect_error(add_individuals_from_folder("not-a-species", tempdir()), "expects a SynSpecies object")
  expect_error(add_pairwise_alignment("not-a-species", pair), "expects a SynSpecies object")
  expect_error(add_pairwise_alignment(species, "not-an-alignment"), "must be a SynPairAlignment object")
  expect_error(add_multiple_alignment("not-a-species", multi), "expects a SynSpecies object")
  expect_error(add_multiple_alignment(species, "not-an-alignment"), "must be a SynMultiAlignment object")
  expect_error(add_interproscan_annotation("not-an-individual"), "expects a SynIndividual object")
  expect_error(add_tree("not-a-species", tree = pair), "expects a SynSpecies object")
  expect_error(
    add_protein_mutation_annotation("not-a-syn-object", tempfile(fileext = ".tsv")),
    "expects a SynIndividual or SynSpecies object"
  )
})
