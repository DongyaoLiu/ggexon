test_that("pairwise_alignment_data(cigar = TRUE) expands PAF cg matches into long rows", {
  paf_path <- tempfile(fileext = ".paf")
  writeLines(
    c(
      paste(
        c(
          "V_RagTag", 1000, 100, 114, "+",
          "V", 2000, 500, 514, 12, 16, 255,
          "cg:Z:5M2I3M2D4M"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "V_RagTag", 1000, 100, 110, "-",
          "V", 2000, 500, 509, 8, 10, 255,
          "cg:Z:4M2I3M1D1M"
        ),
        collapse = "\t"
      )
    ),
    paf_path
  )

  pair <- SynPairAlignment(
    name = "XZ1516_vs_N2_detail",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path,
    format = "paf"
  )

  paf <- pairwise_alignment_data(pair, cigar = TRUE)

  expect_identical(nrow(paf), 6L)
  expect_true(all(c("paf_row", "block_index", "block_size", "cigar_op", "qstrand", "tstrand") %in% names(paf)))
  expect_identical(paf$qstart, c(100L, 107L, 110L, 106L, 101L, 100L))
  expect_identical(paf$qend, c(105L, 110L, 114L, 110L, 104L, 101L))
  expect_identical(paf$tstart, c(500L, 505L, 510L, 500L, 504L, 508L))
  expect_identical(paf$tend, c(505L, 508L, 514L, 504L, 507L, 509L))
  expect_identical(paf$qstrand, c("+", "+", "+", "-", "-", "-"))
  expect_identical(paf$tstrand, rep("+", 6L))
  expect_identical(paf$block_size, c(5L, 3L, 4L, 4L, 3L, 1L))
  expect_identical(unique(paf$track), "link_XZ1516_vs_N2_detail")
})

test_that("load_alignment(cigar = TRUE) caches detailed PAF match rows", {
  paf_path <- tempfile(fileext = ".paf")
  writeLines(
    paste(
      c(
        "V_RagTag", 1000, 100, 114, "+",
        "V", 2000, 500, 514, 12, 16, 255,
        "cg:Z:5M2I3M2D4M"
      ),
      collapse = "\t"
    ),
    paf_path
  )

  pair <- SynPairAlignment(
    name = "XZ1516_vs_N2_detail",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path,
    format = "paf"
  )

  loaded <- load_alignment(pair, cigar = TRUE)

  expect_true(isTRUE(loaded@metadata$paf_detail))
  expect_identical(nrow(pairwise_alignment_data(loaded)), 3L)
  expect_identical(pairwise_alignment_data(loaded)$block_index, c(1L, 2L, 3L))
})

test_that("load_alignment on SynSpecies preserves cached detailed PAF alignments by default", {
  paf_path <- tempfile(fileext = ".paf")
  writeLines(
    paste(
      c(
        "V_RagTag", 1000, 100, 114, "+",
        "V", 2000, 500, 514, 12, 16, 255,
        "cg:Z:5M2I3M2D4M"
      ),
      collapse = "\t"
    ),
    paf_path
  )

  pair <- SynPairAlignment(
    name = "XZ1516_vs_N2_detail",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path,
    format = "paf"
  ) |>
    load_alignment(cigar = TRUE)

  sp <- SynSpecies(name = "worms")
  sp <- suppressWarnings(add_pairwise_alignment(sp, pair))
  reloaded <- load_alignment(sp)

  stored_pair <- pairwise_alignments(reloaded)[["XZ1516_vs_N2_detail"]]
  expect_true(isTRUE(stored_pair@metadata$paf_detail))
  expect_identical(nrow(stored_pair@data), 3L)
  expect_identical(stored_pair@data$block_index, c(1L, 2L, 3L))
})
