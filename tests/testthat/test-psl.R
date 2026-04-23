test_that("read_pairwise_psl parses PSL rows into the internal pairwise table", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    c(
      paste(
        c(
          10, 2, 1, 0, 1, 5, 0, 0, "++",
          "N2_V_100_200", 1000, 100, 120,
          "chrV", 2000, 500, 520,
          2, "10,3,", "100,117,", "500,517,"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          4, 1, 0, 0, 0, 0, 0, 0, "+-",
          "N2_V_100_200", 1000, 130, 135,
          "chrV", 2000, 700, 705,
          1, "5,", "130,", "700,"
        ),
        collapse = "\t"
      )
    ),
    psl_path
  )

  psl <- read_pairwise_psl(
    psl_path,
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra"
  )

  expect_s3_class(psl, "data.frame")
  expect_identical(names(psl), c(
    "qchr", "qlen", "qstart", "qend", "strand",
    "tchr", "tlen", "tstart", "tend", "nmatch", "alen", "mapq"
  ))
  expect_identical(psl$qchr, c("V", "V"))
  expect_identical(psl$tchr, c("chrV", "chrV"))
  expect_identical(psl$strand, c("+", "-"))
  expect_identical(psl$nmatch, c(11L, 4L))
  expect_identical(psl$alen, c(13L, 5L))
})

test_that("read_pairwise_psl expands ungapped blocks when more = TRUE", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        10, 0, 0, 0, 0, 0, 0, 0, "+-",
        "N2_V_100_200", 1000, 100, 120,
        "chrV", 2000, 1490, 1500,
        2, "10,3,", "100,117,", "500,517,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  psl <- read_pairwise_psl(
    psl_path,
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra",
    more = TRUE
  )

  expect_identical(nrow(psl), 2L)
  expect_true(all(c("psl_row", "block_index", "block_size", "qstrand", "tstrand") %in% names(psl)))
  expect_identical(psl$qstart, c(100L, 117L))
  expect_identical(psl$qend, c(110L, 120L))
  expect_identical(psl$tstart, c(1490L, 1480L))
  expect_identical(psl$tend, c(1500L, 1483L))
  expect_identical(psl$block_size, c(10L, 3L))
  expect_identical(psl$qstrand, c("+", "+"))
  expect_identical(psl$tstrand, c("-", "-"))
})

test_that("SynPairAlignment with format = 'psl' is parsed through pairwise_alignment_data", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        10, 2, 1, 0, 1, 5, 0, 0, "++",
        "N2_V_100_200", 1000, 100, 120,
        "chrV", 2000, 500, 520,
        2, "10,3,", "100,117,", "500,517,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  pair <- SynPairAlignment(
    name = "N2_vs_afra_psl",
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra",
    file = psl_path,
    format = "psl"
  )

  paf_like <- pairwise_alignment_data(pair)

  expect_identical(nrow(paf_like), 1L)
  expect_identical(paf_like$qspecies, "N2")
  expect_identical(paf_like$tspecies, "Caenorhabditis_afra")
  expect_identical(paf_like$track, "link_N2_vs_afra_psl")
  expect_identical(paf_like$qchr, "V")
  expect_identical(paf_like$tchr, "chrV")
})

test_that("load_alignment(more = TRUE) caches PSL ungapped blocks", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        10, 0, 0, 0, 0, 0, 0, 0, "+-",
        "N2_V_100_200", 1000, 100, 120,
        "chrV", 2000, 1490, 1500,
        2, "10,3,", "100,117,", "500,517,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  pair <- SynPairAlignment(
    name = "N2_vs_afra_psl",
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra",
    file = psl_path,
    format = "psl"
  )

  loaded <- load_alignment(pair, more = TRUE)

  expect_true(isTRUE(loaded@metadata$psl_more))
  expect_identical(nrow(pairwise_alignment_data(loaded)), 2L)
  expect_identical(pairwise_alignment_data(loaded)$block_index, c(1L, 2L))
})

test_that("load_alignment on SynSpecies preserves cached detailed PSL alignments by default", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        10, 0, 0, 0, 0, 0, 0, 0, "+-",
        "N2_V_100_200", 1000, 100, 120,
        "chrV", 2000, 1490, 1500,
        2, "10,3,", "100,117,", "500,517,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  pair <- SynPairAlignment(
    name = "N2_vs_afra_psl",
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra",
    file = psl_path,
    format = "psl"
  ) |>
    load_alignment(more = TRUE)

  sp <- SynSpecies(name = "worms")
  sp <- suppressWarnings(add_pairwise_alignment(sp, pair))
  reloaded <- load_alignment(sp)

  stored_pair <- pairwise_alignments(reloaded)[["N2_vs_afra_psl"]]
  expect_true(isTRUE(stored_pair@metadata$psl_more))
  expect_identical(nrow(stored_pair@data), 2L)
  expect_identical(stored_pair@data$block_index, c(1L, 2L))
})

test_that("read_pairwise_psl parses the 22-column liftover PSL shape", {
  psl_path <- tempfile(fileext = ".psl")
  writeLines(
    paste(
      c(
        "N2_V_20450000_20490000",
        100, 5, 0, 0, 0, 0, 0, 0,
        "+-",
        "V", 20924180, 20467551, 20467656,
        "V", 12207686, 10256132, 10256237,
        1, "105,", "20467551,", "10256132,"
      ),
      collapse = "\t"
    ),
    psl_path
  )

  psl <- read_pairwise_psl(
    psl_path,
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra"
  )

  expect_identical(nrow(psl), 1L)
  expect_identical(psl$qchr, "V")
  expect_identical(psl$tchr, "V")
  expect_identical(psl$strand, "-")
  expect_identical(psl$nmatch, 100L)
  expect_identical(psl$alen, 105L)
})

test_that("read_pairwise_psl handles the external liftover PSL fixture shape", {
  psl_path <- file.path(
    "/Users/liudongyao/Downloads/workprogress/C35Project/V3/04/liftover_N2_V_20450000_20490000/psl",
    "Caenorhabditis_afra.psl"
  )
  skip_if_not(file.exists(psl_path))

  psl <- read_pairwise_psl(
    psl_path,
    query_individual = "N2",
    target_individual = "Caenorhabditis_afra"
  )

  expect_true(nrow(psl) > 0L)
  expect_true(all(as.character(psl$qchr) == "V"))
  expect_true(all(psl$qlen > 0L))
  expect_true(all(psl$tlen > 0L))
  expect_true(all(psl$alen >= psl$nmatch))
})
