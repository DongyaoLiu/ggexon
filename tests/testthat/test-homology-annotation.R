test_that("HomologyAnnotation normalizes duplicated query genes", {
  expect_warning(
    ha <- HomologyAnnotation(
      name = "q_to_ref",
      reference_species = "ref",
      query_species = "query",
      homology_table = data.frame(
        query_gene = c("g1", "g1", "g1", "g2", "g2"),
        reference_gene = c("r1", "r1b", "r1c", "r2", "r2b"),
        source = c("blast", "manual", "manual", "blast", "manual"),
        stringsAsFactors = FALSE
      )
    ),
    "g1 \\(3 rows\\).*g2 \\(2 rows\\)"
  )

  tbl <- homology_table(ha)
  expect_identical(tbl$query_gene, c("g1", "g2"))
  expect_identical(tbl$reference_gene, c("r1", "r2"))
  expect_identical(tbl$source, c("blast", "blast"))
  expect_true(validObject(ha))
})

test_that("homology_table replacement normalizes and records table edits", {
  ha <- HomologyAnnotation(
    name = "q_to_ref",
    reference_species = "ref",
    query_species = "query",
    homology_table = data.frame(query_gene = "g1", reference_gene = "r1")
  )

  expect_warning(
    homology_table(ha) <- data.frame(
      query_gene = c("g2", "g2", NA, "g3"),
      reference_gene = c("r2", "r2b", "r_missing", ""),
      confidence = c(0.8, 0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    ),
    "g2 \\(2 rows\\)"
  )

  tbl <- homology_table(ha)
  expect_identical(tbl$query_gene, "g2")
  expect_identical(tbl$reference_gene, "r2")
  expect_equal(tbl$confidence, 0.8)
  expect_identical(annotation_metadata(ha)$homology_edits[[1L]]$action, "replace_table")
})

test_that("add_homology adds rows and keeps existing rows unless overwrite is true", {
  ha <- HomologyAnnotation(
    name = "q_to_ref",
    reference_species = "ref",
    query_species = "query",
    homology_table = data.frame(
      query_gene = "g1",
      reference_gene = "r1",
      bitscore = 10,
      stringsAsFactors = FALSE
    )
  )

  expect_warning(
    ha2 <- add_homology(
      ha,
      query_gene = c("g1", "g2"),
      reference_gene = c("new_r1", "r2"),
      source = "manual"
    ),
    "overwrite = FALSE"
  )
  tbl2 <- homology_table(ha2)
  expect_identical(tbl2$query_gene, c("g1", "g2"))
  expect_identical(tbl2$reference_gene, c("r1", "r2"))
  expect_true(is.na(tbl2$source[[1L]]))
  expect_identical(tbl2$source[[2L]], "manual")
  expect_equal(tbl2$bitscore[[1L]], 10)
  expect_true(is.na(tbl2$bitscore[[2L]]))

  ha3 <- add_homology(
    ha,
    query_gene = c("g1", "g2"),
    reference_gene = c("new_r1", "r2"),
    source = "manual",
    overwrite = TRUE
  )
  tbl3 <- homology_table(ha3)
  expect_identical(tbl3$reference_gene[tbl3$query_gene == "g1"], "new_r1")
  expect_equal(tbl3$bitscore[tbl3$query_gene == "g1"], 10)
  expect_identical(tbl3$source[tbl3$query_gene == "g1"], "manual")
  expect_identical(annotation_metadata(ha3)$homology_edits[[1L]]$action, "add")
})

test_that("replace_homology patches supplied columns only", {
  ha <- HomologyAnnotation(
    name = "q_to_ref",
    reference_species = "ref",
    query_species = "query",
    homology_table = data.frame(
      query_gene = c("g1", "g2"),
      reference_gene = c("r1", "r2"),
      bitscore = c(10, 20),
      source = c("blast", "blast"),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    replace_homology(ha, query_gene = "missing", reference_gene = "rX"),
    "Cannot replace missing"
  )

  ha2 <- replace_homology(
    ha,
    data = data.frame(
      query_gene = "g1",
      reference_gene = "new_r1",
      source = "manual",
      stringsAsFactors = FALSE
    )
  )
  tbl2 <- homology_table(ha2)
  expect_identical(tbl2$reference_gene[tbl2$query_gene == "g1"], "new_r1")
  expect_identical(tbl2$source[tbl2$query_gene == "g1"], "manual")
  expect_equal(tbl2$bitscore[tbl2$query_gene == "g1"], 10)

  expect_error(
    replace_homology(ha, query_gene = "g3", source = "manual", add_missing = TRUE),
    "without non-empty `reference_gene`"
  )
  ha3 <- replace_homology(
    ha,
    query_gene = "g3",
    reference_gene = "r3",
    source = "manual",
    add_missing = TRUE
  )
  expect_true("g3" %in% homology_table(ha3)$query_gene)
})

test_that("delete_homology deletes by query gene with optional reference guard", {
  ha <- HomologyAnnotation(
    name = "q_to_ref",
    reference_species = "ref",
    query_species = "query",
    homology_table = data.frame(
      query_gene = c("g1", "g2", "g3"),
      reference_gene = c("r1", "r2", "r3"),
      source = "blast",
      stringsAsFactors = FALSE
    )
  )

  expect_error(delete_homology(ha, query_gene = "missing"), "Cannot delete missing")
  expect_warning(
    ha_warn <- delete_homology(ha, query_gene = "missing", missing = "warn"),
    "Cannot delete missing"
  )
  expect_identical(homology_table(ha_warn), homology_table(ha))
  expect_error(
    delete_homology(ha, query_gene = "g1", reference_gene = "wrong"),
    "guard does not match"
  )

  ha2 <- delete_homology(ha, query_gene = "g1", reference_gene = "r1")
  expect_identical(homology_table(ha2)$query_gene, c("g2", "g3"))
  ha3 <- delete_homology(
    ha,
    data = data.frame(query_gene = c("g1", "g3"), stringsAsFactors = FALSE)
  )
  expect_identical(homology_table(ha3)$query_gene, "g2")
})

test_that("SynSpecies homology CRUD methods select attached homology annotations", {
  ha1 <- HomologyAnnotation(
    name = "q1_to_ref",
    reference_species = "ref",
    query_species = "q1",
    homology_table = data.frame(query_gene = "g1", reference_gene = "r1")
  )
  ha2 <- HomologyAnnotation(
    name = "q2_to_ref",
    reference_species = "ref",
    query_species = "q2",
    homology_table = data.frame(query_gene = "g2", reference_gene = "r2")
  )
  sp <- SynSpecies(name = "worms")
  sp <- add_homology_annotation(sp, ha1)
  sp <- add_homology_annotation(sp, ha2)

  expect_error(
    add_homology(
      sp,
      name = "q1_to_ref",
      query_species = "q1",
      query_gene = "g3",
      reference_gene = "r3"
    ),
    "either `name` or `query_species`"
  )

  sp <- replace_homology(
    sp,
    query_species = "q1",
    query_gene = "g1",
    reference_gene = "new_r1"
  )
  expect_identical(
    homology_table(get_homology_annotation(sp, query_species = "q1"))$reference_gene,
    "new_r1"
  )

  sp <- add_homology(
    sp,
    name = "q2_to_ref",
    query_gene = "g3",
    reference_gene = "r3"
  )
  expect_identical(
    homology_table(get_homology_annotation(sp, name = "q2_to_ref"))$query_gene,
    c("g2", "g3")
  )

  sp <- delete_homology(sp, name = "q2_to_ref", query_gene = "g2")
  expect_identical(
    homology_table(get_homology_annotation(sp, name = "q2_to_ref"))$query_gene,
    "g3"
  )
})
