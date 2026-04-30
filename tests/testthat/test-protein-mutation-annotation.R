test_that("read_protein_mutation_counts normalizes hash notation and strain index", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tref\talt\tlength\tsample_count\tstrains",
      "zina-1\tsubstitution\tC#316#H\tC\tH\t1\t2\tind1,ind2",
      "zina-1\tsubstitution\tD#219#I\tD\tI\t1\t1\tind2"
    ),
    mutation_file
  )

  tbl <- read_protein_mutation_counts(mutation_file)
  idx <- attr(tbl, "individual_index", exact = TRUE)

  expect_equal(tbl$gene_id, c("zina-1", "zina-1"))
  expect_equal(tbl$position, c(316L, 219L))
  expect_equal(tbl$mutation, c("C316H", "D219I"))
  expect_equal(tbl$sample_count, c(2L, 1L))
  expect_equal(attr(tbl, "individual_col", exact = TRUE), "strains")
  expect_setequal(idx$individual, c("ind1", "ind2"))
  expect_equal(nrow(idx), 3)
})

test_that("add_protein_mutation_annotation filters rows for SynIndividual ids", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tlength\tsample_count\tstrains",
      "sept-1\tsubstitution\tE#39#K\t1\t1\tQX1791",
      "sept-2\tsubstitution\tT#135#I\t1\t2\tCB4852,QX1791"
    ),
    mutation_file
  )

  x <- SynIndividual(id = "CB4852")
  x <- add_protein_mutation_annotation(x, mutation_file)
  hits <- query_protein_mutations(x)

  expect_s4_class(get_annotation(x, "protein_mutations"), "SynProteinMutationAnnotation")
  expect_equal(hits$gene_id, "sept-2")
  expect_equal(hits$mutation, "T135I")
})

test_that("add_protein_mutation_annotation dispatches SynSpecies rows and creates missing individuals", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tlength\tsample_count\tstrains",
      "zina-1\tsubstitution\tC#316#H\t1\t2\tind1,ind2",
      "zina-1\tsubstitution\tD#219#I\t1\t1\tind2"
    ),
    mutation_file
  )

  sp <- SynSpecies("test")
  sp <- add_individual(sp, SynIndividual(id = "ind1"))
  sp <- add_protein_mutation_annotation(sp, mutation_file)

  expect_setequal(names(individuals(sp)), c("ind1", "ind2"))
  expect_equal(nrow(query_protein_mutations(sp, individual = "ind1")), 1)
  expect_equal(nrow(query_protein_mutations(sp, individual = "ind2")), 2)
  expect_equal(
    query_protein_mutations(sp, individual = "ind2", min_sample_count = 2)$mutation,
    "C316H"
  )
})

test_that("add_protein_mutation_annotation can attach unrouted tables to all existing SynSpecies individuals", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tlength\tsample_count",
      "zina-1\tsubstitution\tC#316#H\t1\t2"
    ),
    mutation_file
  )

  sp <- SynSpecies("test")
  sp <- add_individual(sp, SynIndividual(id = "ind1"), SynIndividual(id = "ind2"))
  sp <- add_protein_mutation_annotation(sp, mutation_file, all = TRUE)

  expect_equal(nrow(query_protein_mutations(sp, individual = "ind1")), 1)
  expect_equal(nrow(query_protein_mutations(sp, individual = "ind2")), 1)
})

test_that("query_protein_mutations can read lazy annotation files", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tlength\tsample_count\tstrains",
      "zina-1\tsubstitution\tC#316#H\t1\t2\tind1,ind2"
    ),
    mutation_file
  )

  ann <- SynProteinMutationAnnotation(
    name = "protein_mutations",
    mutation_file = mutation_file
  )
  hits <- query_protein_mutations(ann, strains = "ind2")

  expect_equal(hits$mutation, "C316H")
  expect_equal(hits$position, 316L)
})
